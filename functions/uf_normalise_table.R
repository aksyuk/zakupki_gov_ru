# ..............................................................................
# uf.normalise.table()
#
# Функция для номализации таблицы: расклеивает ячейки с несколькими значениями,
#  соединёнными символом #.
#
# Аргументы:
#  * DT            -- таблица с данными (в теле функции первым делом 
#                     преобразуется во фрейм)
#  * key.col.names -- названия ключевых столбцов таблицы
#  * group.cols    -- номера столбцов таблицы с символами # для ускорения работы
#  * plot.filename -- файл, в который рисуем график изменение размера 
#  * plot.main     -- заголовок для графика
#
# Возвращаемое значение: список из таблицы ($data) и её размера ($size.Mb)
# 
# Создаёт / модифицирует файлы: нет
# ..............................................................................

uf.normalise.table <- function(DT, max.console.lines = iMaxConsoleStatusLines,
                               log.errors.flnm = paste0(sDataSamplePath,
                                                    'log_normalise_err.log')) {
    
    # # отладка
    # DT <- protocols.EF3
    # DT <- df
    
    # таблица с результатами
    DT.result <- NULL
    
    msg <- paste0('Начали нормализацию: ', Sys.time())
    message(msg)
    
    # найти столбцы, в которых есть #
    message(paste0('Индексация столбцов...', Sys.time()))
    cl.hashes <- grepl(unlist(apply(DT, 2, function(x){
        x <- paste0(unlist(x), collapse = '@')
    })), pattern = '#')
    cl.hashes <- colnames(DT)[cl.hashes]
    
    # найти строки, в которых есть #
    message(paste0('Индексация строк...', Sys.time()))
    rw.hashes <- grepl(unlist(apply(DT, 1, function(x){
        x <- paste0(unlist(x), collapse = '@')
    })), pattern = '#')
    rw.hashes <- rownames(DT)[rw.hashes]
    
    # на случай если попался кусок таблицы без склеек
    if (length(rw.hashes) == 0) {
        message(paste0('Таблица не содержит склеек'))
        return(list(data = DT, size.Mb = object.size(DT)))
    }
    
    message(paste0('Начинаю цикл по строкам.', Sys.time()))
    
    # счётчики
    n <- length(rw.hashes)
    console.clean.count <- 0
    i <- 0
    
    # копируем строки без склеянных ячеек
    DT.result <- DT[!(rownames(DT) %in% rw.hashes), ]
    
    # цикл по строкам с #
    for (i.row in rw.hashes) {
        
        # # отладка
        # i.row <- 9681
        # i.row <- rw.hashes[1]
        
        res <- tryCatch({
            # выбираем строку, расклеиваем в список
            new.line <- unlist(DT[rownames(DT) == i.row, ])
            new.line <- gsub(new.line, pattern = '(http://|https://)[^;]*', 
                             replacement = '')
            lst <- lapply(new.line, function(x) {unlist(strsplit(x, '#'))})
            lst.length <- sapply(lst, length)
            
            # сколько строк добавить
            rw.hashes.add <- unique(lst.length[lst.length > 1])
            
            # повторяем значения остальных ячеек в строке столько раз,
            #  сколько повторов в ячейке с #
            lst <- lapply(lst, function(x) {
                x <- rep(x, rw.hashes.add / length(x))
            })
            
            # превращаем список во фрейм, меняем номера строк
            dt <- data.frame(lst, stringsAsFactors = F)
            rownames(dt) <- paste0(i.row, '.', 1:rw.hashes.add)
            
            # сбиваем с таблицей для этого столбца
            DT.result <- rbind(DT.result, dt)
            dt <- NULL
            rm(dt)
            
            list(no.err = T, data = DT.result)
        },
        error = function(cond) {
            message("Запись файла не удалась")
            message("Сообщение об ошибке:")
            message(paste0(cond, '\n'))
            uf.write.to.log(paste0('Таблица: ', paste0(colnames(DT), 
                                                       collapse = ';'), 
                                   '\n i.row = ', i.row, '\n', cond), 
                            log.errors.flnm, T)
            list(no.err = F, data = NULL)
        },
        warning = function(cond) {
            message("Запись файла вызвала предупреждение")
            message("Текст сообщения:")
            message(paste0(cond, '\n'))
            uf.write.to.log(paste0('Таблица: ', paste0(colnames(DT), 
                                                       collapse = ';'), 
                                   '\n i.row = ', i.row, '\n', cond), 
                            log.errors.flnm, T)
            list(no.err = F, data = DT.result)
        },
        finally = {
            # статус в консоль
            i <- i + 1
            message(paste0('Строка ', i.row, ': ', 
                           round(i / n * 100, 1), '%... '))
            console.clean.count <- console.clean.count + 1
            if (console.clean.count == max.console.lines) {
                cat("\014")
                console.clean.count <- 0
            }
        })
        
        if (res$no.err == F) {
            cat(red(paste0('Аварийная остановка: произошла ошибка.',
                           '\nСм. файл лога: ', log.errors.flnm)))
            return(res$no.err)
        }
    }
    
    message(paste0('Закончили нормализацию: ', Sys.time()))
    
    DT <- NULL
    rm(DT)
    
    return(list(data = res$data, size.Mb = object.size(res$data)))
}
