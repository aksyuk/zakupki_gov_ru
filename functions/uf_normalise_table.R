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
