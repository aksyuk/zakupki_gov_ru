



uf.convert.char.to.date <- function(dates.char.vector) {
    
    # # отладка
    # dates.char.vector <- DT.protocols02$protocol02Date
    
    # костыль, т.к. я не могу нормально составить рег. выражение для +- перед
    #  часовым поясом
    if (min(nchar(dates.char.vector)) < 25) {
        msg <- paste0('Даты не конвертированы, т.к. в данных нет часового пояса', 
                      ' в формате +HH:MM\nПример данных: ',
                      dates.char.vector[nchar(dates.char.vector) < 25][1])
        cat(red(msg))
        return(dates.char.vector)    
    }
    
    # вытаскиваем знак смещения часового пояса и инвертируем его
    tz.sign.char <- gsub('.*([+]|-)\\d{2}:\\d{2}$', '\\1', 
                         dates.char.vector)
    tz.sign.invert <- rep('', length(tz.sign.char))
    tz.sign.invert[tz.sign.char == '+'] <- '-'
    tz.sign.invert[tz.sign.char == '-'] <- '+'

    # # отладка
    # table(tz.sign.invert)

    # вытаскиваем timezone
    tz <- as.POSIXct(gsub('(.*)[+|-](.*)$', '\\2', dates.char.vector), 
                     format = '%H:%M')
    tz <- paste0('Etc/GMT', tz.sign.invert, hour(tz) + minute(tz) / 60)
    
    # удаляем ненужное
    rm(tz.sign.char, tz.sign.invert)
    
    # # отладка
    # table(tz)    
    
    # отрезаем зону от исходной строки
    dates.wo.tz <- gsub('(.*)[+|-](.*)$', '\\1', dates.char.vector)
    
    # заготовка под вектор с датами в часовом поясе Москвы
    date.MSK <- rep(as.POSIXct('1900-01-01 00:00:01', tz = 'Europe/Moscow'), 
                    length(dates.char.vector))
    tz.loop <- unique(tz)
    
    for (tz.curr in tz.loop) {
        
        # # отладка
        # tz.curr <- tz.loop[3]
        
        date.orig.tz <- as.POSIXct(dates.wo.tz[tz == tz.curr], 
                                   format = '%Y-%m-%dT%H:%M:%S', tz = tz.curr)
        date.MSK[tz == tz.curr] <- with_tz(date.orig.tz, 'Europe/Moscow')
    }
    
    # # отладка
    # dates.char.vector[1:10]
    # dates.wo.tz[1:10]
    # date.MSK[1:10]
    
    return(date.MSK)
}


