# ..............................................................................
# uf.parse.xmls.with.prefix()
#
# Функция парсит xml-файлы с одним и тем же префиксом по шаблонам и записывает 
#  результаты в таблицу с метаданными
#
# Аргументы:
#  * file.names             -- вектор имён xml-файлов (с путями) для разбора
#  * xml.patterns           -- фрейм с названиями столбцов итоговой таблицы 
#                              данных и xpath шаблонами для их заполнения
#  * rows.temp.tbl          -- количество столбцов во временной таблице
#                              (чтобы не забить оперативку полностью)
#  * out.file               -- полное имя файла для записи временных и итоговой
#                              таблиц
#  * log.filename           -- имя файла для записи лога
#
# Возвращаемое значение: TRUE, если выполнено без ошибок,
#                        NULL иначе
# 
# Создаёт / модифицирует файлы: 
#  * сохраняет временные таблицы по rows.temp.tbl строк, затем объединяет их
#     в итоговую таблицу и удаляет временные
#  * сохраняет csv-файл с данными 
#  * сохраняет csv-файл с метаданными
# ..............................................................................

uf.parse.xmls.with.prefix <- function(file.names, xml.patterns, rows.temp.tbl,
                                      out.file, log.filename, prefix = pref) {
    
    # # отладка
    # file.names <- flnms
    # xml.patterns <- patterns[[pref]]
    # rows.temp.tbl <- 5000
    # out.file <- paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/DT_',
    #                    pref, '.csv')
    # log.filename <- log.parse.XML.filename
    # prefix = pref
    
    
    # счётчик для временных таблиц
    tmp.files.count <- 0
    # путь для временных таблиц
    out.path <- gsub(paste0('(.*', lProcedureToScrap$procedureCode, '/)(.*)'), 
                     '\\1', out.file)
    
    # счётчики для статуса
    n <- length(file.names)
    i.files.сount <- 0
    # имена тегов
    tagnames <- xml.patterns[, 2]
    # промежуточная таблица
    df.lines.notices <- NULL
    # счётчик для очистки консоли
    console.clean.count <- 0
    
    # КОСТЫЛЬ С ПРЕФИКСОМ fcsContractSign ......................................
    #  с префиксом fcsContractSign есть проблемы: 
    #  в новой версии изменился namespace
    if (prefix == 'fcsContractSign') {
        ptt.new <- gsub('oos:', '', xml.patterns[, 1])
        ptt.old <- xml.patterns[, 1]
    }
    # ..........................................................................
    
    for (f in file.names) {
        
        # # отладка
        # f <- file.names[1]
        # f <- "./data/raw/06_from201901to201912_loaded2020-03-06/xmls/fcsProtocolEF1_0108000000119000004_22249704.xml"
        
        rootNode <- xmlTreeParse(f, encoding = 'UTF-8', useInternalNodes = T)
        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootNode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
        names(ns)[names(ns) == ''] <- 'xmlns'
        
        # КОСТЫЛЬ С ПРЕФИКСОМ fcsContractSign ..................................
        if (prefix == 'fcsContractSign') {
            sch.ver <- xpathSApply(rootNode, paste0('//', prefix), xmlGetAttr, 
                                   'schemeVersion', namespaces = ns)
            if (length(sch.ver) == 0) {
                sch.ver <- xpathSApply(rootNode, paste0('//ns2:', prefix), 
                                       xmlGetAttr, 'schemeVersion', namespaces = ns)
            }
            
            if (as.numeric(sch.ver) >= 9) {
                xml.patterns[, 1] <- ptt.new
            } else {
                xml.patterns[, 1] <- ptt.old
            }
        }
        # КОНЕЦ КОСТЫЛЯ С ПРЕФИКСОМ fcsContractSign ............................
        
        # # отладка
        # tmp.xpth <- xml.patterns[, 1][5]
        # tmp.xpth <- '//xmlns:application/xmlns:admitted[../xmlns:admissionResults]|//xmlns:application/xmlns:admitted[not(../following-sibling::xmlns:admitted)]|//xmlns:application/xmlns:appRejectedReason[1]/xmlns:explanation'
        # xpathSApply(rootNode, tmp.xpth, xmlValue, namespaces = ns)
        # # в шаблонах fcsProtocolEF1 ругается на запрос 
        # #  //xmlns:application/xmlns:admitted[not(../following-sibling::xmlns:admitted)]
        # #  причём ему не нравится именно not
        
        # парсим строку целиком
        new.line <- tryCatch({
            res <- unlist(unname(sapply(xml.patterns[, 1],
                                        function(path) {
                                            val.tmp <- xpathSApply(rootNode, path,
                                                                   xmlValue,
                                                                   namespaces = ns)
                                            # если тег не найден, ставим NA
                                            if (length(val.tmp) == 0) val.tmp <- NA
                                            # если найдено несколько тегов,
                                            #  сжимаем в одно значение
                                            if (length(val.tmp) > 1) {
                                                val.tmp <- paste(val.tmp,
                                                                 collapse = '#')
                                                }
                                            val.tmp
                                            })))
            names(res) <- tagnames
            res
        },
        error = function(cond) {
            msg <- paste0('Не удалось запарсить файл, ', f, 
                          '\nСообщение об ошибке:\n', cond, '\n')
            uf.write.to.log(msg, log.parse.XML.filename)
            return(NULL)
        },
        warning = function(cond) {
            msg <- paste0('Парсинг файла вызвал предупреждение, ', f, 
                          '\nТекст сообщения:\n', cond, '\n')
            uf.write.to.log(msg, log.parse.XML.filename)
            res
        })
        
        # # отладка
        # new.line
        
        if (is.null(new.line)) {
            red('Нештатное завершение работы парсера')
            return(NULL)
        }
        
        i.files.сount <- i.files.сount + 1
        if (is.null(df.lines.notices)) {
            df.lines.notices <- data.frame(t(new.line), stringsAsFactors = F)
        } else {
            df.lines.notices <- rbind(df.lines.notices, new.line)
        }
        
        # # отладка
        # df.lines.notices
        
        # когда набирается заданное количество строк, скидываем таблицу на диск
        if (nrow(df.lines.notices) == rows.temp.tbl) {
            tmp.files.count <- tmp.files.count + 1
            tbl.name <-
                paste0(out.path, 'DF_lines_', formatC(tmp.files.count, width = 3, 
                                            format = 'd', flag = '0'), '.csv')
            write.csv2(df.lines.notices, tbl.name, row.names = F)
            
            msg <- paste0('Временный файл ', tbl.name, ' сохранён.')
            uf.write.to.log(msg, log.filename)
            
            df.lines.notices <- NULL
        }
        
        message(paste0(i.files.сount, ' ...', sub(f, pattern = '^.*/', 
                                                  replacement = ''),
                       '... ', round(i.files.сount / n * 100, 1), '% done'))
        
        console.clean.count <- console.clean.count + 1
        if (console.clean.count == iMaxConsoleStatusLines) {
            cat("\014")
            console.clean.count <- 0
        }
    }
    
    # сохраняем на диск последний кусок таблицы
    if (!is.null(df.lines.notices)) {
        tmp.files.count <- tmp.files.count + 1
        tbl.name <-
            paste0(out.path, 'DF_lines_', formatC(tmp.files.count, width = 3, 
                                        format = 'd', flag = '0'), '.csv')
        write.csv2(df.lines.notices, tbl.name, row.names = F)
        
        df.lines.notices <- NULL
    }
    
    # сообщения в консоль и в лог
    msg <- paste0('Закончили разбор файлов ', pref, ': ', Sys.time())
    uf.write.to.log(msg, log.parse.XML.filename)
    
    # имена временных файлов
    tmp.files <- paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/',
                        'DF_lines_', formatC(1:tmp.files.count, width = 3, 
                                             format = 'd', flag = '0'), '.csv')
    
    # читаем первую из временных таблиц
    DT.all <- data.table(read.csv2(tmp.files[1], colClasses = rep('character', n.cols),
                                   stringsAsFactors = F))
    
    # читаем остальные временные таблицы
    for (flnm in tmp.files[-1]) {
        DT.all <- rbind(DT.all, 
                        data.table(read.csv2(flnm, colClasses = rep('character', 
                                                                    n.cols),
                                             stringsAsFactors = F)))
    }
    
    # записываем общую таблицу
    uf.write.table.with.metadata(DT.all, out.file, F)
    
    # убираем временные таблицы из памяти и с диска
    DT.all <- NULL
    df.lines.notices <- NULL
    rm(DT.all, df.lines.notices)
    file.remove(tmp.files)
    
    # сообщения в консоль и в лог
    msg <- paste0('Файл ', out.file, ' сохранён.')
    uf.write.to.log(msg, log.parse.XML.filename)
    
    msg <- paste0('Временные файлы удалены.')
    uf.write.to.log(msg, log.parse.XML.filename)
    
    return(TRUE)
}
