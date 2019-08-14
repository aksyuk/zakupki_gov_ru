# ..............................................................................
# parser_ftp.R
# ..............................................................................
# Парсинг содержимого необъятного ftp сервера госзакупок, 
#  который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
# ..............................................................................
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# ..............................................................................
# Версия 1.0 (18.07.2018)
# ..............................................................................



# НАСТРОЙКИ --------------------------------------------------------------------


# Загрузка библиотек ===========================================================
library('RCurl')
library('XML')
library('dplyr')
library('reshape')
library('data.table')
library('stringi')
library('ggplot2')
library('httr')
library('readr')
library('jsonlite')
library('RJSONIO')


# Настройки системы ============================================================
# отображение больших цифр в обычном формате, а не в экспоненциальном
options("scipen" = 100, "digits" = 4)


# Функции ======================================================================

# uf.write.to.log() ............................................................
#
# Функция записи сообщения в консоль и в лог.
#
# Source: https://tomizonor.wordpress.com/2013/04/17/file-utf8-windows/
#
# Аргументы:
#  * msg           -- текст сообщения
#  * out.file.name -- файл, в который пишем результаты
#  * bom           -- писать ли спецсимволы в начало такстового файла 
#                     (T, если Windows)
#  * silent        -- если T, то не выводить сообщение в консоль
#
# Возвращаемое значение: нет.
# 
# Создаёт / модифицирует файлы:
#  * out.file.name (путь + имя) -- текстовый лог. Файл надо создать заранее.
#                                  строки добавляются в конец.
#
uf.write.to.log <- function(msg, out.file.name, bom = F, silent = F) {
    # соединение с файлом
    con <- file(out.file.name, 'ab')
    # спецсимволы начала файла в Windows 
    BOM <- charToRaw('\xEF\xBB\xBF')
    if (bom) writeBin(BOM, con, endian = 'little')
    # пишем в файл
    writeBin(charToRaw(paste0(msg, '\r\n')), con, endian = 'little')
    # закрываем соединение
    close(con)
    # пишем сообщение в консоль
    if (!silent) message(msg)
}


# uf.normalise.table() ............................................................
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
# Возвращаемое значение: нет.
# 
# Создаёт / модифицирует файлы:
#  * plot.filename (путь + имя) -- графический файл .png (статус)
#
uf.normalise.table <- function(DT, 
                               plot.filename, plot.main,
                               max.console.lines = iMaxConsoleStatusLines) {
    
    # # отладка
    # DT <- protocols.EF3[8600:8700, c('purchaseNumber', 'fcsProtocolEF3.id',
    #                              'protocolDate', 'applications.journalNumber',
    #                              'applications.inn', 'application.appRating',
    #                              'applications.countryFullName',
    #                              'applications.postAddress'), with = F]
    # str(DT)
    # 
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
    
    message(paste0('Начинаю цикл по строкам.', Sys.time()))
    
    # размер таблицы для графика
    o.s.01 <- NULL
        
    # счётчики
    n <- length(rw.hashes)
    console.clean.count <- 0
    i <- 0
    
    # цикл по строкам с #
    for (i.row in rw.hashes) {
        
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
        if (i.row == 1) {
            DT.result <- dt
        } else {
            DT.result <- rbind(DT.result, dt)
        }
        
        # статус в консоль
        i <- i + 1
        message(paste0('Строка ', i.row, ': ', 
                       round(i / n * 100, 1), '%... '))
        console.clean.count <- console.clean.count + 1
        if (console.clean.count == max.console.lines) {
            cat("\014")
            console.clean.count <- 0
        }
    }

    # # отладка
    # DT[DT$purchaseNumber %in% c('0111100000217000017', '0111100000217000020'), ]
    # DT.result[DT.result$purchaseNumber %in% c('0111100000217000017', '0111100000217000020'), ]
    # dim(DT)
    # dim(DT.result)
    
    o.s.01 <- c(o.s.01, object.size(DT.result))
    
    message(paste0('Закончили нормализацию: ', Sys.time()))
    
    if (!is.null(plot.filename)) {
        png(plot.filename)
        plot(o.s.01 / 1024 / 1024, ylab = 'Mb', xlab = 'Num. of columns with #',
             main = paste0('DT size:', plot.main))
        dev.off()
    }

    plot(o.s.01 / 1024 / 1024, ylab = 'Mb', xlab = 'Num. of columns with #',
         main = paste0('DT size:', plot.main))
    
    rm(DT, dt)
    
    return(DT.result)
}


# uf.nominatim.OSM() ............................................................
#
# Функция для определения координат по адресу.
#
# Source: https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 
#
# Аргументы:
#  * address       -- адрес, ЕДИНИЧНЫЙ ВЕКТОР
#
# Возвращаемое значение: фрейм с широтой и долготой адреса.
# 
# Создаёт / модифицирует файлы: нет. 
#
uf.nominatim.OSM <- function(address = NULL) {
    if (suppressWarnings(is.null(address))) return(data.frame())
    
    d <- tryCatch(
         jsonlite::fromJSON(
             gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
                  'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
        ), error = function(err) {
            msg <- paste0('Address caused an error: ', address, '\n',
                          'The original error message:\n', 
                          conditionMessage(err))
            message(msg)
            
            return(data.frame(long = -200, lat = -200))
        }
    )
    
    if (length(d) == 0) {
        return(data.frame(long = -200, lat = -200))
    }
    
    return(data.frame(long = as.numeric(d$lon), lat = as.numeric(d$lat)))
}


# uf.get.coords.Yandex() ............................................................
#
# Функция для определения координат по адресу через https API Яндекс карт.
#
# Аргументы:
#  * address       -- адрес, ЕДИНИЧНЫЙ ВЕКТОР
#
# Возвращаемое значение: фрейм с широтой и долготой адреса.
# 
# Создаёт / модифицирует файлы: нет. 
#
uf.get.coords.Yandex <- function (address) {
    
    # отладка
    address <- all.participants.addr[responsibleOrg.long == -200, 
                                     address.OSM][1]
    
    my.https <- paste0('https://geocode-maps.yandex.ru/1.x/?geocode=', address)
    my.https <- gsub('\\s+', '\\%20', my.https)
    
    latlong <- tryCatch({
        xmlData <- getURL(my.https)
        rootNode <- xmlTreeParse(xmlData, encoding = 'UTF-8', useInternalNodes = T)
        
        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootNode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
        names(ns)[names(ns) == ''] <- 'xmlns'
        ns['xmlns'] <- 'http://www.opengis.net/gml'
        
        xpathSApply(rootNode, path = '//xmlns:Point/xmlns:pos', xmlValue,
                    namespaces = ns)[1]
        }, error = function(err) {
            msg <- paste0('Address caused an error: ', address, '\n',
                          'The original error message:\n', 
                          conditionMessage(err))
            message(msg)
            
            return(data.frame(long = -200, lat = -200))
        }
    )
    
    if (length(latlong) == 0) {
        return(data.frame(long = -200, lat = -200))
    }
    
    return(data.frame(long = gsub(latlong, pattern = '\\s.*$', replacement = ''),
                      lat = gsub(latlong, pattern = '[^\\s]*\\s', replacement = ''),
                      stringsAsFactors = F))
}


# uf.format.address.for.OSM() ..................................................
#
# Функция для преобразования вектора исходных адресов из базы госзакупок
#  к виду, более-менее пригодному для поиска координат по адресу в базе 
#  OpenStreetMap.
#
# Аргументы:
#  * address       -- адрес
#
# Возвращаемое значение: вектор отформатированных адресов.
# 
# Создаёт / модифицирует файлы: нет. 
#
uf.format.address.for.OSM <- function(address) {
    # переносим город в конец адреса
    # шаблоны для того, чтобы удалить названия городов из адреса
    ptt.city.remove <- 
        list(
            "пос[.]\\s?ж[.]\\s?д[.]\\sст.([^,]*),", # пос. ж. д. ст. Название,
            "п[.]г[.]т[.]([^,]*),",                 # п.г.т. Название
            "г[.]([^,]*),",                         # г. Название
            ",([^,]*)\\sг,",                        # Название г,
            ",([^,]*)\\sГород,",                    # Название Город,
            ",([^,]*)\\sпгт,",                      # Название пгт,
            ",([^,]*)\\sс,",                        # Название с,
            "с[.]([^,]*),",                         # с. Название
            "пос\\s([^,]*),",                       # пос Название
            "пос.([^,]*),",                         # пос. Название
            ",([^,]*)\\sпос([.]|\\s),"              # Название пос. ,
        )
    # шаблоны для того, чтобы вытащить названия городов
    ptt.city.pull <- lapply(ptt.city.remove, function(x) {
        x <- paste0('.*', x, '.*')
    })
    
    # вытаскиваем из адресов города и сохраняем отдельно
    cities <- address
    sapply(ptt.city.pull, function(x) {
        cities[cities == address] <<- 
            gsub(cities[cities == address], pattern = x, replacement = "\\1")
    })
    cities <- trimws(cities, 'both')
    
    # убираем города из адресов
    address.OSM <- address
    sapply(ptt.city.remove, function(x) {
        address.OSM[address.OSM == address] <<- 
            gsub(address.OSM[address.OSM == address], pattern = x, replacement = "")
    })
    
    # убираем район
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*\\sр-н(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*\\sрайон(,?.*)', 
                        replacement = '\\1\\2')
    # убираем сельское поселение
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*сельское поселение(,?.*)', 
                        replacement = '\\1\\2')
    
    # убираем индексы
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)\\d{6}(,?.*)', 
                        replacement = '\\1\\2')
    # убираем РФ
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Российская Федерация(,?.*)', 
                        replacement = '\\1\\2')
    # убираем Татарстан Респ
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Республика(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Респ(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Республика Татарстан(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Респ(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Респ Татарстан(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)РТ(,?.*)', 
                        replacement = '\\1\\2')
    # убираем пустые запятые
    address.OSM <- gsub(address.OSM, pattern = ',\\s?,', replacement = '')
    # убираем ведущие пробелы
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    # убираем ведущие запятые
    address.OSM <- gsub(address.OSM, pattern = '^,\\s?', replacement = '')
    # и ещё раз ведущие пробелы
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    
    # всё в верхний регистр
    address.OSM <- toupper(address.OSM)
    # убираем кавычки
    address.OSM <- gsub(address.OSM, pattern = '\\"', replacement = '')
    
    # стандартизируем названия улиц 
    address.OSM <- gsub(address.OSM, pattern = 'УЛИЦА', replacement = 'УЛ.')
    address.OSM <- gsub(address.OSM, pattern = 'УЛ(\\s|[.])([^,]*),', 
                        replacement = '\\2 УЛИЦА,')
    address.OSM <- gsub(address.OSM, pattern = '(^|,)([^,]*)УЛ,', 
                        replacement = ', \\2 УЛИЦА,')
    
    address.OSM <- gsub(address.OSM, pattern = 'ПР-К?Т', replacement = 'ПРОСПЕКТ')
    address.OSM <- gsub(address.OSM, pattern = 'ПР.\\s?МИРА', 
                        replacement = 'ПРОСПЕКТ МИРА')
    address.OSM <- gsub(address.OSM, pattern = '(\\s|,)ПР.', 
                        replacement = 'ПРОСПЕКТ')
    address.OSM <- gsub(address.OSM, pattern = '([^,]*)\\sПРОСПЕКТ', 
                        replacement = 'ПРОСПЕКТ \\1,')
    
    address.OSM <- gsub(address.OSM, pattern = '(\\s|,)ПР-Д\\s', 
                        replacement = 'ПРОЕЗД')
    address.OSM <- gsub(address.OSM, pattern = '^ПР-Д\\s', 
                        replacement = 'ПРОЕЗД')
    
    address.OSM <- gsub(address.OSM, pattern = '(,|\\s)Ш(,|\\s)', 
                        replacement = ' ШОССЕ')
    address.OSM <- gsub(address.OSM, pattern = '^Ш(,|\\s)', 
                        replacement = ' ШОССЕ ')
    
    address.OSM <- gsub(address.OSM, pattern = '(,|\\s)НАБ(,|\\s)', 
                        replacement = ' НАБЕРЕЖНАЯ')
    address.OSM <- gsub(address.OSM, pattern = '^НАБ(,|\\s)', 
                        replacement = ' НАБЕРЕЖНАЯ ')
    
    address.OSM <- gsub(address.OSM, pattern = '(^Б-Р\\s)|(\\sБ-Р\\s)|(\\sБ-Р,)', 
                        replacement = ' БУЛЬВАР ')
    address.OSM <- gsub(address.OSM, pattern = '(^ПЛ\\s)|(,\\s?ПЛ\\s)', 
                        replacement = ' ПЛОЩАДЬ ')
    address.OSM <- gsub(address.OSM, pattern = 'ПЛОЩАДЬ\\s([^,]*),', 
                        replacement = '\\1 ПЛОЩАДЬ,')
    
    
    # и ещё раз пробелы
    address.OSM <- gsub(address.OSM, pattern = '\\s+', replacement = ' ')
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    
    # неполные названия улиц
    address.OSM <- gsub(address.OSM, pattern = '\\sЯХИНА,', 
                        replacement = 'РУСТЕМА ЯХИНА')
    address.OSM <- gsub(address.OSM, pattern = 'К.\\s?МАРКСА(\\s|,)', 
                        replacement = 'КАРЛА МАРКСА ')
    address.OSM <- gsub(address.OSM, pattern = 'Л.\\s?ТОЛСТОГО(\\s|,)', 
                        replacement = 'ЛЬВА ТОЛСТОГО ')
    address.OSM <- gsub(address.OSM, pattern = 'М.\\s?ДЖАЛИЛЯ(\\s|,)', 
                        replacement = 'МУСЫ ДЖАЛИЛЯ ')
    address.OSM <- gsub(address.OSM, pattern = 'Х.\\s?ТАКТАША(\\s|,)', 
                        replacement = 'ХАДИ ТАКТАША ')
    address.OSM <- gsub(address.OSM, pattern = 'АК.\\s?ПАРИНА(\\s|,)', 
                        replacement = 'АКАДЕМИКА ПАРИНА ')
    
    
    # разбираемся с домами и квартирами
    address.OSM <- gsub(address.OSM, pattern = 'ДОМ', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = 'АДМИНИСТРАТИВНОЕ ЗДАНИЕ', 
                        replacement = '')
    # убираем тире, помещения и квартиры в самом конце адреса
    address.OSM <- gsub(address.OSM, pattern = ',\\s?-\\s?$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ПОМ[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ПОМЕЩЕНИЕ[^,]*$', 
                        replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ОФИС[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?КВ.[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?КВАРТИРА[^,]*$', 
                        replacement = '')
    # если в конце два номера без метки помещения
    address.OSM <- gsub(address.OSM, pattern = '(,\\s?\\d*),\\s?\\d*$', 
                        replacement = '\\1')
    
    # вытаскиваем дома
    houses <- gsub(address.OSM, pattern = '.*,([^,]*)$', replacement = '\\1')
    address.OSM <- gsub(address.OSM, pattern = ',([^,]*)$', replacement = '')
    
    houses <- gsub(houses, pattern = 'Д[.]\\s?', replacement = '')
    # убираем ведущие пробелы
    houses <- gsub(houses, pattern = '^\\s*', replacement = '')
    
    # добавляем дома обратно
    address.OSM <- paste0(address.OSM, ', Д. ', toupper(houses))
    
    
    # добавляем город
    address.OSM <- paste0(address.OSM, ', ', toupper(cities))
    # добавляем страну
    address.OSM <- paste0(address.OSM, ', РОССИЯ')
    
    # доводим напильником: некоторые номера домов не находятся ни в гугле,
    #  ни в яндексе
    address.OSM <- gsub(address.OSM, pattern = 'ЛЕНИНА, Д. 26 Г/2', 
                        replacement = 'ЛЕНИНА, Д. 26')
    address.OSM <- gsub(address.OSM, pattern = ',\\s31A,', 
                        replacement = ', 31 А,')
    address.OSM <- 
        gsub(address.OSM, 
             pattern = '(ИОВЛЕВА|КОСМОНАВТОВ|НАРИМАНОВА|БОЛЬШАЯ КРАСНАЯ|КАРЛА МАРКСА|ЛЕНИНА|ВАХИТОВА|ЦЕНТРАЛЬНАЯ),', 
             replacement = '\\1 УЛИЦА,')
    
    return(address.OSM)
}


# uf.process.large.table.normalising() ..................................................
#
# Функция для нормализации большой таблицы по кускам. Разбиваем на много таблиц 
#  поменьше, нормализуем по отдельности, пишем в директорию для csv-файлов.
#  Затем проходим по записанным файлам и сбиваем всё вместе.
#
# Аргументы:
#  * DT                     -- таблица для нормализации
#  * out.total.table.name   -- имя для итоговой нормализованной таблицы
#  * dt.step                -- сколько строк будет в каждом кусочке таблицы
#  * csvs.path              -- путь в папку для сохранения csv
#  * max.console.lines      -- максимум строк в консоли
#
# Возвращаемое значение: нормализованная таблица данных.
# 
# Создаёт / модифицирует файлы: 
#  * пишет нормализованные кусочки таблицы в папку csvs.path, 
#    а после сборки итоговой таблицы стирает их
#
uf.process.large.table.normalising <- 
    function(DT, out.total.table.name,
             key.col.names, group.cols,
             dt.step = 100, csvs.path = sRawCSVPath,
             max.console.lines = iMaxConsoleStatusLines) {
        
    # счётчик строк в консоли    
    console.clean.count <- 0    
        
    # общее количество табличек по dt.step строк
    dt.count <- nrow(DT) %/% dt.step
    if (nrow(DT) / dt.step != nrow(DT) %/% dt.step) {
        dt.count <- dt.count + 1
    }
    # первая и последняя строки первого кусочка большой таблицы
    fst.row <- 1
    lst.row <- fst.row + dt.step - 1

    # цикл по номеру таблички
    for (file.count in 1:dt.count) {
        
        # берём кусочек исходной таблицы
        df <- data.frame(DT[fst.row:lst.row, ])
        rownames(df) <- fst.row:lst.row
        
        # запускаем нормализацию
        df <- uf.normalise.table(df, plot.filename = NULL,
                                 paste0(sRawCSVPath, 'DF_curr_part_',
                                        formatC(file.count, width = 3,
                                                format = 'd', flag = '0')))
        
        # пишем результат на диск
        out.file.name <- paste0(csvs.path, 'DF_curr_part_',
                                formatC(file.count, width = 3,
                                        format = 'd', flag = '0'), '.csv')
        # сообщение в консоль
        message(paste0('Записываю файл ', out.file.name, '... (', file.count,
                       ' из ', dt.count, ').'))
        console.clean.count <- console.clean.count + 1
        # непосредственно запись в файл
        write.csv2(df, out.file.name, row.names = F)
        # write_delim(df, out.file.name, delim = ";")
        
        # обновляем счётчики строк
        fst.row <- lst.row + 1
        lst.row <- fst.row + dt.step - 1
        
        # очищаем консоль
        if (console.clean.count == max.console.lines * 2) {
            cat("\014")
            console.clean.count <- 0
        }
    }
    
    # все имена табличек на диске
    flnms <- paste0(csvs.path,
                    grep(dir(csvs.path), pattern = 'DF_curr_part_.*[.]csv', 
                         value = T))
    
    # сбить всё в один файл
    f.count <- 1
    n <- length(flnms)
    
    # цикл по именам файлов
    for (f in flnms) {
        message(paste0('Reading file ', f, '... (', f.count, ' из ', n,')'))
        console.clean.count <- console.clean.count + 1
        
        DT.part <- read.csv2(f, colClasses = rep('character', 3))
        
        if (f.count == 1) {
            DT.all <- copy(DT.part)
        } else {
            DT.all <- data.table(rbind(DT.all, DT.part))
        }
        
        message(paste0('Table size: ', 
                       round(object.size(DT.all) / 1024 / 1024, 1), 
                       ' Mb. nrow(DT.all) = ', nrow(DT.all), '\n'))
        console.clean.count <- console.clean.count + 1
        
        # обновляем счётчики
        f.count <- f.count + 1
        
        # очищаем консоль
        if (console.clean.count == max.console.lines * 2) {
            cat("\014")
            console.clean.count <- 0
        }
    }
    
    # записываем общую таблицу
    out.file.name <- paste0(csvs.path, out.total.table.name)
    write.csv2(DT.all, out.file.name, row.names = F)
    
    # удаляем временные файлы
    file.remove(flnms)
    rm(DT, flnms)
    
    return(DT.all)
}


# Константы ====================================================================

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10

# координаты FTP-сервера госзакупок и логин-пароль для бесплатного доступа
sFTPURL <- 'ftp://ftp.zakupki.gov.ru/'
sUserPwd <- 'free:free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
sYEAR <- c('201709', '201710', '201711', '201712', '201801', '201802', '201803',
          '201804', '201805')

# список директорий с регионами
sRegionFoldersNames <- 
    unlist(strsplit(getURL(paste0(sFTPURL, 'fcs_regions/'), 
                           ftp.use.epsv = FALSE, dirlistonly = TRUE, 
                           userpwd = sUserPwd), '\r\n'))
sRegionFoldersNames <- sRegionFoldersNames[grep('_Resp$|_kraj$|_.?obl$', 
                                                  sRegionFoldersNames)]
message(paste0('Regions: ', length(sRegionFoldersNames), ' folders.'))

# преобразуем папки регионов в URL-адреса
sRegionFolderURLs <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            sRegionFoldersNames, '/')

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')

# * Структура директорий рабочей папки #########################################

# все равки исходных данных ....................................................
sRawDataPath <- './data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# исходные данные в архивах, выгрузка за период ................................
# sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], '_',
#              format(Sys.time(), '%d-%m-%Y'), '/')
sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], 
               '_15-06-2018', '/')
if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- paste0(sRawArchPath, 'xmls/')
if (!dir.exists(sRawXMLPath)) dir.create(sRawXMLPath)

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- paste0(sRawArchPath, 'csv/')
if (!dir.exists(sRawCSVPath)) dir.create(sRawCSVPath)

# таблицы-справочники ..........................................................
sRefPath <- paste0('./data/reference/')
if (!dir.exists(sRefPath)) dir.create(sRefPath)

# графики ......................................................................
sPlotPath <- paste0('./plots/')
if (!dir.exists(sPlotPath)) dir.create(sPlotPath)


# Переменные ===================================================================

#  регион (регионы): Республика Татарстан
my.region <- grep(sRegionFolderURLs, pattern = 'Tatar', value = T)
message(paste0('Работаем с регионом: ', my.region))



# 1. ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК -------------------------------------

# # ВНИМАНИЕ: загрузка архивов занимает время !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # выгрузка xml в архивах в папку './data/raw/' в рабочей директории
# for (sRegion in my.region) {
# 
#     for (sSubf in sSubfolders) {
#         # адреса архивов на сервере
#         zips <- 
#             unlist(strsplit(getURL(paste0(sRegion, sSubf),
#                                    ftp.use.epsv = FALSE, dirlistonly = TRUE, 
#                                    userpwd = sUserPwd), '\r\n'))
#         
#         # берём заявки, открытые в периодах, начинающихся с sYEAR
#         for (y in sYEAR) {
#             # адреса архивов за период
#             zips.y <- grep(zips, 
#                            pattern = paste0('_', y, '.*_.*_.*'), value = T)
#             
#             # отладка
#             # print(paste0(y, ': ', zips.y))
# 
#             # загружаем архивы в папку с равками по одному
#             for (sZip in zips.y) {
#                 download.file(paste0('ftp://', sUserPwd, '@',
#                                      gsub('^.*://', '', sRegion), sSubf, sZip),
#                               destfile = paste0(sRawArchPath, sZip))
#             }
#         }
#     }
# }



# 2. РАСПАКОВКА АРХИВОВ --------------------------------------------------------
# # имена всех загруженных архивов
# xmlzips <- dir(sRawArchPath)
# 
# # ВНИМАНИЕ: оооочень долго !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # распаковываем архивы в папку для XML-ей
# length(xmlzips)
# # счётчики файлов для сообщений статуса
# i.files.сount <- 0
# n <- length(xmlzips)
# # счётчик строк вывода в консоли
# console.clean.count <- 0
# for (sXMLzip in xmlzips) {
#     # распаковать архив в папку для XML-ей; в каждом архиве до кучи xml-файлов
#     unzip(paste0(sRawArchPath, sXMLzip), exdir = sRawXMLPath)
#     
#     # обновить счётчики и выдать сообщение в консоль
#     i.files.сount <- i.files.сount + 1
#     message(paste0('Распаковка...', round(i.files.сount / n * 100, 1), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }



# 3. ИНДЕКСАЦИЯ XML-ФАЙЛОВ -----------------------------------------------------

# # все имена извлечёных xml-файлов ..............................................
# all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
# 
# message(paste0('Всего файлов формата XML: ', length(all.xmls)))
# 
# # вектор большой, поэтому записываем его в таблицу
# write.csv2(data.frame(files = all.xmls), paste0(sRawArchPath, 'all_xmls.csv'),
#            row.names = F)
# 
# all.xmls <- read.csv2(paste0(sRawCSVPath, 'all_xmls.csv'),
#                       stringsAsFactors = F)[, 1]
# 
# message(paste0('Всего файлов формата XML: ', length(all.xmls)))
# message('Первые 10: ')
# message(paste0(all.xmls[1:10], collapse = '\n'))


# # все уникальные номера извещений ..............................................
# #  сначала убираем 'fcs_'
# all.ids <- sub('^fcs_', '^fcs', all.xmls)
# 
# # затем всё, что идёт до первого '_', включая его
# all.ids <- sub('^(.*?)_', '', all.ids)
# 
# # теперь всё, что идёт от '_' и до конца
# all.ids <- sub('_.*$', '', all.ids)
# all.ids <- unique(all.ids)
# 
# message(paste0('Всего уникальных номеров извещений: ', length(all.ids)))
# message('Количество символов в номерах извещений:')
# table(nchar(all.ids))

# # все префиксы файлов ..........................................................
# prefixes <- table(gsub('_$', '', gsub(all.xmls, pattern = '(\\d{19}|\\d{18}).*$',
#                                       replacement = '')))
# 
# message('Всего префиксов файлов: ', length(prefixes))
# sort(prefixes, decreasing = T)


# # Индексируем имена всех файлов ================================================
# # таблица с именами всех файлов: имена столбцов по префиксам, первый столбец -- 
# #  номера измещений, значения в остальных столбцах -- кол-во файлов 
# #  по связке (ID + префикс)
# #
# DT.xml.files.index <- data.table(noticeID = all.ids)
# 
# # счётчик для очистки консоли
# console.clean.count <- 0
# # счётчики файлов для сообщений статуса
# i.files.сount <- 0
# n <- length(prefixes)
# 
# df.diff.IDs <- data.frame(fileID = NULL, tagID = NULL)
# 
# # цикл по уникальным префиксам файлов
# for (pr_i in names(prefixes)) {
#     
#     # вытаскиваем все имена файлов с заданным префиксам
#     tmp.v <- grep(all.xmls, pattern = paste0('^', pr_i), value = T)
# 
#     # вытаскиваем noticeID из имени файла
#     noticeID <- sub('^fcs_', '^fcs', tmp.v)
#     noticeID <- sub('^(.*?)_', '', noticeID)
#     noticeID <- sub('_.*$', '', noticeID)
#     
#     DT <- data.table(table(noticeID))
# 
#     DT.xml.files.index <-
#         merge(DT.xml.files.index, DT, by = 'noticeID', all.x = T)
#     DT.xml.files.index[is.na(N), N := 0]
#     colnames(DT.xml.files.index)[colnames(DT.xml.files.index) == 'N'] <- pr_i
# 
#     i.files.сount <- i.files.сount + 1
#     message(paste0(pr_i, ', ', round(i.files.сount / n * 100, 1), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }
# 
# # проверка
# sapply(DT.xml.files.index, function(x){sum(is.na(x))})
# summary(DT.xml.files.index)
# table(nchar(DT.xml.files.index$noticeID ))
# 
# # записываем таблицу-индекс
# write.csv2(DT.xml.files.index, paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
#            row.names = F)
#
# # читаем весь индекс одним файлом
# DT.xml.files.index <- read.csv2(paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
#                            stringsAsFactors = F,
#                            colClasses = c('character', rep('numeric', 55)),
#                            encoding = 'CP-1251')
# DT.xml.files.index <- data.table(DT.xml.files.index)
# dim(DT.xml.files.index)
# str(DT.xml.files.index)
# 
# message(paste0('Уникальных номеров извещений в файле-индексе: ', 
#                length(unique(DT.xml.files.index$noticeID))))
# 
# # проверка
# table(nchar(DT.xml.files.index$noticeID))
# DT.xml.files.index$noticeID[nchar(DT.xml.files.index$noticeID) == 18][1:10]


# # Анализ файла-индекса =========================================================
# # ПРОИНДЕКСИРОВАН ТАТАРСТАН С СЕНТЯБРЯ ПО МАЙ
# n <- nrow(DT.xml.files.index)
# message(paste0('Всего уникальных id закупок за период: ', n))

# # аукционы
# loop.ids <- DT.xml.files.index[DT.xml.files.index$fcsNotificationEA44 > 0, ]$noticeID
# message(paste0('Извещений об электронных аукционах: ', length(loop.ids),
#                ' (', round(length(loop.ids) / n * 100, 1), '%)'))

# # как среди них распределяются протоколы
# sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max)[
#     sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max) > 0]
# 
# # почему же контракты не обнаруживаются??
# gp <- ggplot(data = DT.xml.files.index[noticeID %in% loop.ids, -1], aes(x = contract)) +
#     geom_histogram()
# gp

# СЮРПРИЗ
#  файлы контрактов идут С ДРУГИМИ ID
#  см. 0111100000217000017
#  и контракт contract_1165802029417000020_37417208.xml
#  таким образом, найти контракт, которым завершилась процедура, 
#  похоже, можно только по тегу notificationNumber в теле файла с префиксом
#  contract_
#  при этом номер в имени файла -- это содержимое тега regNum

# grep(dir(sRawXMLPath), pattern = '0111100000217000017.*xml', value = T)

# # больше всего каких-то fcsContractSign. Похоже, это просто электронные подписи
# #  со стороны заказчиков + все реквизиты заказа
# nrow(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# head(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# tail(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# 
# # fcsProtocolEF3 -- это финальная стадия аукциона
# nrow(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
# head(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
# tail(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])


# # первые 11 цифр purchaseNumber -- это id заказчика
# message(paste0('Уникальных заказчиков: ', length(unique(substr(loop.ids, 1, 11)))))
   


# 3. РАЗБОР XML-ФАЙЛОВ ---------------------------------------------------------


# # Делаем индекс файлов с аукционами ============================================
# # выясняем все префиксы файлов с интересующими id
# dt.loop.filenames <- gsub(all.xmls, pattern = 'fcs_', replacement = 'fcs')
# dt.loop.filenames <- gsub(dt.loop.filenames, pattern = '[.]xml',
#                           replacement = '')
# dt.loop.filenames <- data.table(do.call(rbind, strsplit(dt.loop.filenames, '_')))
# dt.loop.filenames <- dt.loop.filenames[V2 %in% loop.ids, ]
# colnames(dt.loop.filenames) <- c('prefix', 'purchaseNum', 'id')
# loop.refs <- unique(dt.loop.filenames$prefix)
# # обязательно ставим аукционы на первое место
# dt.loop.filenames[, .N, by = prefix]
# dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}), by = prefix]
# loop.refs <- dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}),
#                                by = prefix][order(-purchaseNum), ]$prefix
# loop.refs


# # Идём по префиксам, делаем индекс =============================================
# # счётчик для очистки консоли
# console.clean.count <- 0
# # счётчики
# i.files.сount <- 0
# n <- length(loop.refs)
# # цикл по префиксам файлов, которые относятся к электронным аукционам
# for (pref in loop.refs) {
#     # имена xml-файлов с текущим префиксом
#     xmls.by.prefix <- 
#         grep(all.xmls, pattern = paste0(pref, '.*[.]xml'), value = T)
#     # убираем расширения
#     xmls.by.prefix <-
#         gsub(xmls.by.prefix, pattern = '[.]xml$', replacement = '')
#     # убираем префиксы
#     xmls.by.prefix <- 
#         gsub(xmls.by.prefix, pattern = paste0(pref, '_'), replacement = '')
#     
#     # делаем таблицу-индекс для текущего префикса
#     #  столбец 1: id извещения
#     #  столбец 2: id документа
#     DT.xmls.by.prefix.index <- 
#         data.table(purchaseNumber = gsub(xmls.by.prefix, pattern = '_.*$',
#                                          replacement = ''),
#                    id = gsub(xmls.by.prefix, pattern = '^.*_', replacement = ''))
#     colnames(DT.xmls.by.prefix.index)[colnames(DT.xmls.by.prefix.index) == 'id'] <- paste0('id.', pref)
# 
#     # совмещаем с общим индексом (все префиксы вместе)
#     i.files.сount <- i.files.сount + 1
#     if (i.files.сount == 1) {
#         DT.EA44.index <- DT.xmls.by.prefix.index
#         setkey(DT.EA44.index, purchaseNumber)
#     } else {
#         setkey(DT.xmls.by.prefix.index, purchaseNumber)
#         DT.EA44.index <- merge(DT.EA44.index, DT.xmls.by.prefix.index, 
#                                all.x = TRUE, allow.cartesian = T)
#     }
# 
#     # убираем мусор
#     rm(xmls.by.prefix, DT.xmls.by.prefix.index)
#     
#     message(paste0(pref, ': индекс готов на ', round(i.files.сount / n * 100, 0), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }
# 
# # проверка
# dim(DT.EA44.index)
# str(DT.EA44.index)
# DT.EA44.index
# 
# # сохраняем
# write.csv2(DT.EA44.index, paste0(sRawCSVPath, 'DT_index_EA44.csv'),
#            row.names = F)

# DT.EA44.index <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_index_EA44.csv'),
#                                  stringsAsFactors = F,
#                                  colClasses = rep('character', 12)))
# DT.EA44.index
# 
# # процент пропусков
# round(DT.EA44.index[, lapply(.SD, function(x){sum(is.na(x))})] / nrow(DT.EA44.index), 2)
# # всего xml для обработки
# n <- sum(DT.EA44.index[, sapply(.SD, function(x){sum(!is.na(unique(x)))})][-1])
# message(paste0('Всего xml для обработки: ', n,  ' (',
#                round(n / length(all.xmls) * 100, 1),
#                '% от общего числа файлов).'))


# # Парсим файлы с электронными аукционами =======================================
# # список префиксов файлов
# colnames(DT.EA44.index)
# 
# # шаблоны для разбора хранятся в отдельном csv
# df.patterns <- read.csv2(paste0(sRefPath, 'df_xml_patterns.csv'),
#                          stringsAsFactors = F)
# 
# # преобразуем фрейм с шаблонами в список
# patterns <- as.list(df.patterns)
# patterns <- c(list(ns = c('xmlns:', 'xmlns:', 'oos:', 'xmlns:',
#                           'xmlns:', 'xmlns:')), patterns)
# 
# # делаем из шаблонов имена столбцов итоговой таблицы
# patterns[-1] <- lapply(patterns[-1], function(x) {
#     x <- x[x != '']
#     tmp <- gsub(x, pattern = 'xmlns[:]|oos[:]', replacement = '')
#     tmp <- gsub(tmp, pattern = '[//]|[/]', replacement = '.')
#     tmp <- gsub(tmp, pattern = '^[.]*', replacement = '')
#     tmp <- gsub(tmp, pattern = '[.]+', replacement = '.')
#     tmp <- gsub(tmp, pattern = '^.*:', replacement = '')
#     tmp <- gsub(tmp, pattern = '/.*:', replacement = '/')
#     tmp <- gsub(tmp, pattern = '[.]$', replacement = '')
#     tmp <- gsub(tmp, pattern = '[][]', replacement = '')
#     tmp <- gsub(tmp, pattern = '\\)|\\(', replacement = '')
#     tmp <- gsub(tmp, pattern = '([^\\|]*)\\|.*', replacement = '\\1')
# 
#     x <- data.frame(xml.pattern = x)
#     x$xpath.names <- as.character(tmp)
# 
#     x <- x
# })
# 
# # отладка
# # patterns
# patterns$fcsProtocolEF3
# # length(patterns)
# 
# # лог
# log.parse.XML.filename <- paste0(sRawArchPath, 'log_xml_parse.txt')
# if (!file.exists(log.parse.XML.filename)) file.create(log.parse.XML.filename)
# 
# # максимальное число строк таблицы в оперативной памяти
# step.n <- 10000
# 
# # цикл по типам файлов (столбцам таблицы DT.EA44.index со 2 по 15)
# for (j in 7:7) {
# 
#     # отладка
#     # j <- 2
# 
#     DT <- na.omit(unique(DT.EA44.index[, c(1, j), with = F]))
#     if (nrow(DT) == 0) {
#         next
#     }
# 
#     filePrefix <- gsub(colnames(DT)[2], pattern = '^.*[.]', replacement = '')
#     n.cols <- length(patterns[[filePrefix]][, 1])
# 
#     msg <- paste0('Начинаю разбор файлов ', filePrefix, ': ', Sys.time())
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     # это все имена файлов заданного типа
#     flnms <-
#         unique(paste0(sRawXMLPath, '/', filePrefix, '_',
#                       unname(unlist(DT[, 1, with = F])), '_',
#                       unname(unlist(DT[, 2, with = F])), '.xml'))
#     tmp.files.count <- 0
# 
#     # отладка
#     # tmp.files.count <- 4
#     # flnms <- flnms[(((tmp.files.count - 1) * step.n) + 1):length(flnms)]
#     # flnms <- flnms[1:10]
# 
#     # счётчики для статуса
#     n <- length(flnms)
#     i.files.сount <- 0
#     # имена тегов
#     tagnames <- patterns[[filePrefix]][, 2]
#     # промежуточная таблица
#     df.lines.notices <- NULL
#     # счётчик для очистки консоли
#     console.clean.count <- 0
# 
#     for (f in flnms) {
# 
#         # отладка
#         # f <- flnms[1]
# 
#         rootNode <- xmlTreeParse(f, encoding = 'UTF-8', useInternalNodes = T)
#         # танцы с namespace
#         nsDefs <- xmlNamespaceDefinitions(rootNode)
#         ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
#         names(ns)[names(ns) == ''] <- 'xmlns'
# 
#         # парсим строку целиком
#         new.line <-
#             unlist(unname(sapply(patterns[[filePrefix]][, 1],
#                                  function(path) {
#                                      val.tmp <- xpathSApply(rootNode, path,
#                                                             xmlValue,
#                                                             namespaces = ns)
#                                      # если тег не найден, ставим NA
#                                      if (length(val.tmp) == 0) val.tmp <- NA
#                                      # если найдено несколько тегов,
#                                      #  сжимаем в одно значение
#                                      if (length(val.tmp) > 1) {
#                                          val.tmp <- paste(val.tmp,
#                                                           collapse = '#')
#                                          }
#                                      val.tmp
#                                      })))
#         names(new.line) <- tagnames
# 
#         # отладка
#         # new.line
# 
#         i.files.сount <- i.files.сount + 1
#         if (is.null(df.lines.notices)) {
#             df.lines.notices <- data.frame(t(new.line), stringsAsFactors = F)
#         } else {
#             df.lines.notices <- rbind(df.lines.notices, new.line)
#         }
# 
#         # когда набирается заданное количество строк, скидываем таблицу на диск
#         if (nrow(df.lines.notices) == step.n) {
#             tmp.files.count <- tmp.files.count + 1
#             tbl.name <-
#                 paste0(sRawCSVPath, 'DF_lines_',
#                        formatC(tmp.files.count, width = 3, format = 'd',
#                                flag = '0'), '.csv')
#             write.csv2(df.lines.notices, tbl.name, row.names = F)
# 
#             msg <- paste0('Временный файл ', tbl.name, ' сохранён.')
#             uf.write.to.log(msg, log.parse.XML.filename)
# 
#             df.lines.notices <- NULL
#         }
# 
#         message(paste0(j, ' ...', sub(f, pattern = '^.*/', replacement = ''),
#                        '... ', round(i.files.сount / n * 100, 1), '% done'))
# 
#         console.clean.count <- console.clean.count + 1
#         if (console.clean.count == iMaxConsoleStatusLines) {
#             cat("\014")
#             console.clean.count <- 0
#         }
#     }
# 
#     # сохраняем на диск последний кусок таблицы
#     if (!is.null(df.lines.notices)) {
#         tmp.files.count <- tmp.files.count + 1
#         tbl.name <-
#             paste0(sRawCSVPath, 'DF_lines_',
#                    formatC(tmp.files.count, width = 3, format = 'd', flag = '0'),
#                    '.csv')
#         write.csv2(df.lines.notices, tbl.name, row.names = F)
# 
#         df.lines.notices <- NULL
#     }
# 
#     # сообщения в консоль и в лог
#     msg <- paste0('Закончили разбор файлов ', filePrefix, ': ', Sys.time())
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     # имена временных файлов
#     tmp.files <- paste0(sRawCSVPath, 'DF_lines_',
#                         formatC(1:tmp.files.count, width = 3, format = 'd',
#                                 flag = '0'), '.csv')
# 
#     # читаем первую из временных таблиц
#     DT.all <- data.table(read.csv2(tmp.files[1], colClasses = rep('character', n.cols),
#                                    stringsAsFactors = F))
# 
#     # читаем остальные временные таблицы
#     for (flnm in tmp.files[-1]) {
#         DT.all <- rbind(DT.all, data.table(read.csv2(flnm, colClasses = rep('character',
#                                                                             n.cols),
#                                                      stringsAsFactors = F)))
#     }
# 
#     # записываем общую таблицу
#     write.csv2(DT.all, paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
#                row.names = F)
# 
#     # убираем временные таблицы из памяти и с диска
#     rm(DT, DT.all)
#     file.remove(tmp.files)
# 
#     # сообщения в консоль и в лог
#     msg <- paste0('Файл ', paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
#                   ' сохранён.')
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     msg <- paste0('Временные файлы удалены.')
#     uf.write.to.log(msg, log.parse.XML.filename)
# }



# 4. РАБОТА С RAW-ТАБЛИЦАМИ ----------------------------------------------------


# Извлекаем данные для географии заказчиков и поставщиков ======================

#  * Работаем с таблицей объявлений ############################################
# имя файла с результатами парсинга XML (не нормализованная таблица)
flnm <- paste0(sRawCSVPath, 'DT_fcsNotificationEA44.csv')

# грузим
DT.notif <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                na.strings = c('NA', '<NA>'),
                                # encoding = 'UTF-8',
                                colClasses = rep('character', 34)))
# проверка
dim(DT.notif)
DT.notif
colnames(DT.notif)

DT.notif.with.orgs <- unique(DT.notif[, c('fcsNotificationEF.purchaseNumber',
                                          'responsibleOrg.regNum'), with = F])

# # вытащить всех уникальных заказчиков ..........................................
# columns.to.search <- c('responsibleOrg.regNum', 'responsibleOrg.fullName',
#                        'responsibleOrg.postAddress')
# all.orgs <-
#     unique(data.table(DT.notif[, columns.to.search, with = F]))[order(responsibleOrg.regNum), ]
# # есть записи с расхождениями в почтовом индексе; оставляем только уникальные id
# n <- nrow(all.orgs)
# all.orgs <-
#     all.orgs[!c(F, all.orgs$responsibleOrg.regNum[2:n] == all.orgs$responsibleOrg.regNum[1:n-1]), ]
# write.csv2(all.orgs, paste0(sRefPath, 'df_all_orgs.csv'), row.names = F)
# 
all.orgs <- data.table(read.csv2(paste0(sRefPath, 'df_all_orgs_long_lat.csv'), 
                                 stringsAsFactors = F))
# 
# # вытащить все товары из заказов ...............................................
# columns.to.search <- c('purchaseObject.OKPD2.code', 'purchaseObject.OKPD2.name')
# all.OKPD2 <- unique(data.table(DT.notif[, columns.to.search, with = F]))
# dim(all.OKPD2)
# all.OKPD2 <-
#     uf.normalise.table(all.OKPD2, paste0(sPlotPath, 'plot_all_okpd2.png'),
#                        'all.OKPD2')
# all.OKPD2 <- unique(data.table(all.OKPD2))
# all.OKPD2[, purchaseObject.OKPD2.code.2dig :=
#               gsub(all.OKPD2$purchaseObject.OKPD2.code, pattern = '[.].*$',
#                    replacement = '')]
# write.csv2(all.OKPD2, paste0(sRefPath, 'df_all_OKPD2.csv'), row.names = F)
# 
# OKPD.industries <- sort(unique(all.OKPD2$purchaseObject.OKPD2.code.2dig))
# write.csv2(data.frame(OKPD2.2dig = OKPD.industries),
#            paste0(sRefPath, 'df_all_OKPD2_industries.csv'), row.names = F)

# ..............................................................................
# # вытаскиваем заказчиков и участников аукционов
# # ЗАКАЗЧИКИ
# all.orgs$responsibleOrg.address.OSM <- 
#     uf.format.address.for.OSM(all.orgs$responsibleOrg.postAddress)
# 
# all.orgs$responsibleOrg.address.OSM[!grepl(all.orgs$responsibleOrg.address.OSM, 
#                                            pattern = '(УЛИЦА|ПРОСПЕКТ|ПЛОЩАДЬ|БУЛЬВАР|КРЕМЛЬ|ПЕР|ШОССЕ|НАБЕРЕЖНАЯ)')]
# 
# n <- nrow(all.orgs)
# console.clean.count <- 0
# for (i in 1:n) {
# 
#     if (all.orgs[i, 'responsibleOrg.long'] == -200) {
#         # отладка
#         message(i)
#         message(all.orgs$responsibleOrg.address.OSM[i])
#         
#         i.address <- uf.nominatim.OSM(all.orgs$responsibleOrg.address.OSM[i])
#         
#         all.orgs[i, 'responsibleOrg.long'] <- i.address$long
#         all.orgs[i, 'responsibleOrg.lat'] <- i.address$lat
#         
#         # статус в консоль
#         msg <- paste0('Обработка адресов: ', round(i / n * 100, 1), '%... ')
#         message(msg)
#         console.clean.count <- console.clean.count + 1
#         if (console.clean.count == iMaxConsoleStatusLines) {
#             cat("\014")
#             console.clean.count <- 0
#         }
#     }
# }
# 
# message(paste0('Всего адресов: ', nrow(all.orgs)))
# message(paste0('Адреса не найдены: ',
#                sum(na.omit(all.orgs$responsibleOrg.long == -200))))
# 
# write.csv2(all.orgs, paste0(sRefPath, 'df_all_orgs_long_lat.csv'), row.names = F)


# # пробуем переставить улицу после названия
# all.orgs <- data.table(all.orgs)
# all.orgs[responsibleOrg.long == -200 & grepl(responsibleOrg.address.OSM, pattern = 'УЛИЦА'), 
#          responsibleOrg.address.OSM := gsub(responsibleOrg.address.OSM, 
#                                             pattern = '([^.]*)\\sУЛИЦА',
#                                             replacement = 'УЛИЦА \\1')]

# и запускаем цикл ещё раз

# # устраняем ошибку с переменой местами координат
# all.orgs[responsibleOrg.long == -200, ]$responsibleOrg.postAddress
# 
# dt <- read.csv2('./data/reference/part_long_lat_change.csv', 
#                 stringsAsFactors = F)
# colnames(dt)[colnames(dt) == 'responsibleOrgaddress.OSM'] <- 
#     'responsibleOrg.address.OSM'
# all.orgs <- all.orgs[!(responsibleOrg.regNum %in% dt$responsibleOrg.regNum), ]
# all.orgs <- rbind(all.orgs, dt)


# API Google Maps стал платным с середины июля 2018, поэтому адреса, 
#  не найденые на OSM, искали в Яндексе вручную
# цены на Geocoding API:
#  https://developers.google.com/maps/documentation/geocoding/usage-and-billing

# таблица заказчиков с заполненными вручную координатами:
# all.orgs <- read.csv2(paste0(sRefPath, 'df_all_orgs_long_lat.csv'), 
#                       stringsAsFactors = F)

# ..............................................................................
# УЧАСТНИКИ
#
# # первый этап аукциона /////////////////////////////////////////////////////////
# protocols.EF1 <-
#     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF1.csv'),
#                          stringsAsFactors = F,
#                          colClasses = rep('character', 5)))
# protocols.EF1 <- unique(protocols.EF1)
# colnames(protocols.EF1)[colnames(protocols.EF1) == 'fcsProtocolEF1.purchaseNumber'] <-
#     'purchaseNumber'
# protocols.EF1
# 
# # проверка:
# #  строк (уникальных id) в файле должно быть столько же...
# nrow(protocols.EF1)
# #  ...сколько уникальных id протоколов 01
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$id.fcsProtocolEF1))

# # расклеиваим ячейки (файл записывается в csv автоматом)
# DT.protocols01 <-
#     uf.process.large.table.normalising(protocols.EF1[, c('purchaseNumber',
#                                                          'fcsProtocolEF1.id',
#                                                          'protocolDate',
#                                                          'application.journalNumber',
#                                                          'admitted'), with = F],
#                                        out.total.table.name = 'DT_fcsProtocolEF1_clean.csv')

# DT.protocols01 <- data.table(read.csv2(paste0(sRawCSVPath, 
#                                               'DT_fcsProtocolEF1_clean.csv'),
#                                        stringsAsFactors = F,
#                                        colClasses = rep('character', 5)))
# 
# # проверка
# length(unique(DT.protocols01$purchaseNumber))
# length(na.omit(unique(DT.protocols01$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber))
# 
# # во всех нижеследующих заявках процедура аукциона обрывается из-за различных 
# #  ошибок: не подано ни одной заявки, например
# all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber)
# missed.ids.01 <- all.ids.in.index[!(all.ids.in.index %in% 
#                                         na.omit(unique(DT.protocols01$purchaseNumber)))]
# 
# 
# # делаем номера заявок числами
# DT.protocols01[, application.journalNumber 
#                := as.numeric(application.journalNumber)]
# 
# # делаем логический столбец логическим
# DT.protocols01[admitted != 'true', admitted := 'false']
# DT.protocols01[, admitted := as.logical(toupper(admitted))]
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols01[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols01$protocolDate, pattern = 'T', 
#                                    replacement = ' '))]
# DT.protocols01[, protocolDate := NULL]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols01)[colnames(DT.protocols01) == 'protocolDate_POSIXct'] <- 
#     'protocol01Date_POSIXct'
# 
# # результаты
# str(DT.protocols01)
# dim(DT.protocols01)
# DT.protocols01
# 
# 
# # второй этап аукциона /////////////////////////////////////////////////////////
# # protocols.EF2 <-
# #     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF2.csv'),
# #                          stringsAsFactors = F,
# #                          colClasses = rep('character', 9)))
# # protocols.EF2 <- unique(protocols.EF2)
# # colnames(protocols.EF2)[colnames(protocols.EF2) == 'application.journalNumber..priceOffers.'] <-
# #     'application.journalNumber'
# # protocols.EF2
# 
# # # расклеиваим ячейки
# # DT.protocols02 <-
# #     uf.process.large.table.normalising(protocols.EF2[, c('purchaseNumber',
# #                                                          'fcsProtocolEF2.id',
# #                                                          'protocolDate',
# #                                                          'application.journalNumber',
# #                                                          'application.firstOffer.price',
# #                                                          'application.lastOffer.price',
# #                                                          'application.priceOffers.offersQuantity'), with = F],
# #                                        out.total.table.name = 'DT_fcsProtocolEF2_clean.csv')
# 
# DT.protocols02 <- data.table(read.csv2(paste0(sRawCSVPath, 
#                                               'DT_fcsProtocolEF2_clean.csv'),
#                                        stringsAsFactors = F,
#                                        colClasses = rep('character', 5)))
# 
# # проверка
# length(unique(DT.protocols02$purchaseNumber))
# length(na.omit(unique(DT.protocols02$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber))
# 
# # во всех нижеследующих заявках процедура аукциона обрывается из-за различных 
# #  ошибок: не подано ни одной заявки, например
# all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber)
# missed.ids.02 <- all.ids.in.index[!(all.ids.in.index %in% 
#                                         na.omit(unique(DT.protocols02$purchaseNumber)))]
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols02[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols02$protocolDate, 
#                                    pattern = 'T', replacement = ' '))]
# DT.protocols02[, protocolDate := NULL]
# 
# # преобразовываем цены и кол-ва в числа
# DT.protocols02[, application.firstOffer.price := 
#                    as.numeric(application.firstOffer.price)]
# DT.protocols02[, application.lastOffer.price := 
#                    as.numeric(application.lastOffer.price)]
# DT.protocols02[, application.priceOffers.offersQuantity := 
#                    as.numeric(application.priceOffers.offersQuantity)]
# 
# # делаем номера заявок числами
# DT.protocols02[, application.journalNumber 
#                := as.numeric(application.journalNumber)]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols02)[colnames(DT.protocols02) == 'protocolDate_POSIXct'] <- 
#     'protocol02Date_POSIXct'
# 
# # результаты
# str(DT.protocols02)
# dim(DT.protocols02)
# DT.protocols02
# 
# 
# # ПРОБЛЕМА
# #  протоколы оказываются друг с другом не связаны по ключам
# #  одному notice ID может соответствовать до кучи протоколов
# #  Решение -- связывать их по ближайшим датам.
# 
# 
# # # третий этап аукциона /////////////////////////////////////////////////////////
# # protocols.EF3 <-
# #     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF3.csv'),
# #                          stringsAsFactors = F,
# #                          colClasses = rep('character', 9)))
# # protocols.EF3 <- unique(protocols.EF3)
# # protocols.EF3
# 
# # # расклеиваим ячейки
# # DT.protocols03 <-
# #     uf.process.large.table.normalising(protocols.EF3[, c('purchaseNumber',
# #                                                          'fcsProtocolEF3.id',
# #                                                          'protocolDate',
# #                                                          'applications.journalNumber',
# #                                                          'applications.inn',
# #                                                          'application.appRating',
# #                                                          'applications.countryFullName',
# #                                                          'applications.postAddress'), with = F],
# #                                        out.total.table.name = 'DT_fcsProtocolEF3_clean.csv')
# 
# DT.protocols03 <- data.table(read.csv2(paste0(sRawCSVPath, 
#                                               'DT_fcsProtocolEF3_clean.csv'),
#                                        stringsAsFactors = F, 
#                                        colClasses = rep('character', 8)))
# 
# # проверка
# length(unique(DT.protocols03$purchaseNumber))
# length(na.omit(unique(DT.protocols03$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF3), ]$purchaseNumber))
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols03[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols03$protocolDate, pattern = 'T', 
#                                    replacement = ' '))]
# DT.protocols03[, protocolDate := NULL]
# 
# # смотрим сведения о стране
# table(DT.protocols03$applications.countryFullName)
# ### Российская Федерация 
# ###                33395
# DT.protocols03[, applications.countryFullName := NULL]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols03)[colnames(DT.protocols03) == 'applications.journalNumber'] <- 
#     'application.journalNumber'
# colnames(DT.protocols03)[colnames(DT.protocols03) == 'protocolDate_POSIXct'] <- 
#     'protocol03Date_POSIXct'
# 
# # делаем номера заявок числами
# DT.protocols03[, application.journalNumber := as.numeric(application.journalNumber)]
# 
# # разбираемся с application.appRating
# table(DT.protocols03$application.appRating)
# DT.protocols03[, application.appRating := as.numeric(application.appRating)]
# DT.protocols03 <- na.omit(DT.protocols03)
# 
# # результаты
# str(DT.protocols03)
# dim(DT.protocols03)
# DT.protocols03
# 
# 
# # совмещаем информацию об участниках аукционов /////////////////////////////////
# dim(DT.protocols01)
# dim(DT.protocols02)
# dim(DT.protocols03)
# length(unique(DT.protocols01$purchaseNumber))
# length(unique(DT.protocols02$purchaseNumber))
# length(unique(DT.protocols03$purchaseNumber))
# 
# # совмещаем таблицы
# DT.all.EA <- merge(DT.protocols01, DT.protocols02, 
#                    by = c('purchaseNumber', 'application.journalNumber'))
# DT.all.EA <- merge(DT.all.EA, DT.protocols03, 
#                    by = c('purchaseNumber', 'application.journalNumber'))
# DT.all.EA
# 
# write.csv2(DT.all.EA, paste0(sRawCSVPath, 'DT_all_auctions_clean.csv'),
#            row.names = F)
# 
# # всего заявок
# length(unique(DT.all.EA$purchaseNumber))

# # все уникальные участники
# length(unique(DT.all.EA$applications.postAddress))
# all.participants.addr <- unique(DT.all.EA[, applications.inn, applications.postAddress])
# write.csv2(all.participants.addr,
#            paste0(sRefPath, 'dt_all_post_addr_participants.csv'),
#            row.names = F)
# 
# all.participants.addr$responsibleOrg.long <- -200
# all.participants.addr$responsibleOrg.lat <- -200

# отладка
# i <- 10
# tmp <- all.participants.addr$applications.postAddress[i]
# tmp
# tmp <- paste0('дом ', gsub(tmp[1], pattern = '.*Дом:\\s([^;]*);.*', replacement = '\\1'), ',',
#                  gsub(tmp, pattern = '.*Улица:\\s([^;]*);.*', replacement = '\\1'), ' улица,',
#                  gsub(tmp, pattern = '.*Город:\\s([^;]*);.*', replacement = '\\1'), ',',
#                  'Россия')
# tmp
# uf.nominatim.OSM(tmp)

# all.participants.addr[, address.OSM := applications.postAddress]
# # преобразовать структурированные адреса к виду, пригодному для OSM
# all.participants.addr[grepl(applications.postAddress, pattern = ';'), 
#                       address.OSM := 
#                           sapply(applications.postAddress, function(x){
#                               paste0('дом ', 
#                                      gsub(gsub(x, pattern = '.*Дом:\\s([^;]*);.*', replacement = '\\1'),
#                                           pattern = 'Дом|ДОМ|д.', replacement = ''), 
#                                      ',',
#                                      gsub(x, pattern = '.*Улица:\\s([^;]*);.*', replacement = '\\1'), 
#                                      ' улица,',
#                                      ifelse(!grepl(x, pattern = 'Город :'),
#                                             gsub(x, pattern = '.*Город:\\s([^;]*);.*', replacement = '\\1'),
#                                             gsub(x, pattern = '.*Субъект РФ:\\s([^;]*);.*', replacement = '\\1')), 
#                                      ',Россия')
#                           })]
# # отформатировать значения без ";", но с запятыми
# all.participants.addr[!grepl(applications.postAddress, pattern = ';') &
#                           grepl(applications.postAddress, pattern = ','), 
#                       address.OSM := 
#                           sapply(applications.postAddress, uf.format.address.for.OSM)]
# # убрать индекс из значений без запятых
# all.participants.addr[!grepl(address.OSM, pattern = ','),
#                       address.OSM :=
#                           sapply(applications.postAddress, function(x){
#                               gsub(x, pattern = '\\d{6}', replacement = '')
#                           })]
# all.participants.addr[, address.OSM := 
#                           sapply(address.OSM, function(x){
#                               gsub(x, pattern = '\\s+', replacement = ' ')
#                           })]
# 
# # отладка
# # all.participants.addr[1:12, applications.postAddress, address.OSM]


# # узнаём координаты организаций через Яндекс .................................
# DT <- all.orgs
# 
# n <- nrow(DT)
# console.clean.count <- 0
# for (i in 1:n) {
# 
#     if (DT[i, 'responsibleOrg.long'] == -200) {
#         
#         addr <- DT$address.OSM[i]
#         
#         # отладка
#         message(paste0(i, ': ', addr))
# 
#         i.address <- uf.get.coords.Yandex(addr)
#         i.address$long <- as.numeric(i.address$long)
#         i.address$lat <- as.numeric(i.address$lat)
# 
#         # отладка
#         message(paste0(i.address[1, ], collapse = ' '))
#         
#         DT[i, 'responsibleOrg.long'] <- i.address$long
#         DT[i, 'responsibleOrg.lat'] <- i.address$lat
# 
#         # статус в консоль
#         msg <- paste0('Обработка адресов: ', round(i / n * 100, 1), '%... ')
#         message(msg)
#         console.clean.count <- console.clean.count + 1
#         if (console.clean.count == iMaxConsoleStatusLines) {
#             cat("\014")
#             console.clean.count <- 0
#         }
#     }
# }
# 
# message(paste0('Всего адресов: ', nrow(DT)))
# message(paste0('Адреса не найдены: ',
#                sum(na.omit(DT$responsibleOrg.long == -200))))
# 
# DT[responsibleOrg.long == -200, ]
# 
# all.orgs <- DT
# 
# write.csv2(all.orgs,
#            paste0(sRefPath, 'df_all_orgs_long_lat.csv'),
#            row.names = F)
# 
# write.csv2(all.participants.addr,
#            paste0(sRefPath, 'dt_all_post_addr_participants.csv'),
#            row.names = F)
# 
# # координаты центра Казани (т.е. улица не распознана верно):
# nrow(all.participants.addr[responsibleOrg.long == all.participants.addr$responsibleOrg.long[6495] & 
#                                responsibleOrg.lat == all.participants.addr$responsibleOrg.lat[6495], ])
# nrow(all.participants.addr[grepl(all.participants.addr$applications.postAddress, pattern = 'Казань'), ])
# nrow(all.participants.addr)
# str(all.participants.addr)



# биндим координаты ............................................................

DT.all.EA <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_all_auctions_clean.csv'),
                       stringsAsFactors = F,
                       colClasses = c('character', 'numeric', 'character',
                                      'logical', rep('character', 2),
                                      rep('numeric', 3), rep('character', 3),
                                      'numeric', rep('character', 2))))
str(DT.all.EA)
dim(DT.all.EA)

all.participants.addr <- 
    data.table(read.csv2(paste0(sRefPath, 'dt_all_post_addr_participants.csv'),
                         colClasses = c(rep('character', 2), rep('numeric', 2), 
                                        'character')))

all.orgs <- 
    data.table(read.csv2(paste0(sRefPath, 'df_all_orgs_long_lat.csv'),
                         colClasses = c(rep('character', 4), rep('numeric', 2))))
colnames(all.participants.addr) <- gsub(colnames(all.participants.addr),
                                        pattern = 'responsibleOrg[.]',
                                        replacement = 'participant.')
    

DT.all.EA <- merge(DT.all.EA, all.participants.addr[, c('applications.inn',
                                                        'participant.long',
                                                        'participant.lat'), 
                                                    with = F],
                   by = 'applications.inn')
dim(DT.all.EA)

DT.all.EA <- merge(DT.all.EA, DT.notif.with.orgs, 
                   by.x = 'purchaseNumber', 
                   by.y = 'fcsNotificationEF.purchaseNumber')
dim(DT.all.EA)

DT.all.EA <- merge(DT.all.EA, all.orgs[, c('responsibleOrg.regNum',
                                           'responsibleOrg.long',
                                           'responsibleOrg.lat'), with = F],
                   by = 'responsibleOrg.regNum', all.x = T)
dim(DT.all.EA)
