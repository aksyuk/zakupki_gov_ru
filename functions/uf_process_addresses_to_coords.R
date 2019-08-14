# uf.process.addresses.to.coords() .............................................
#
# Функция для добавления в таблицу столбцаотформатированного адреса и столбцов
#  с координатами по адресу
#
# Аргументы:
#  * DТ                -- таблица
#  * col.address       -- имя столбца с адресами
#  * col.coords.prefix -- префикс для столбцов с кординатами
#  * out.file.name     -- имя файла для записи преобразованной таблицы
#
# Возвращаемое значение: преобраованная таблица.
# 
# Создаёт / модифицирует файлы: сохраняет результаты в таблицу 
#                               с именем out.file.name
#
uf.process.addresses.to.coords <- function(DT, col.address, col.coords.prefix,
                                           out.file.name,
                                           max.console.lines = iMaxConsoleStatusLines * 3) {
    
    # # отладка
    # DT <- all.orgs
    # col.address <- 'responsibleOrg.postAddress'
    # col.coords.prefix <- 'responsibleOrg.'
    # out.file.name <- paste0(sRawCSVPath, 'df_all_orgs_long_lat.csv')
    
    DT[, address.OSM := unname(unlist(DT[, col.address, with = F]))]
    
    
    # преобразовать структурированные адреса к виду, пригодному для OSM
    DT[grepl(unname(unlist(DT[, col.address, with = F])), pattern = ';'),
       address.OSM := sapply(address.OSM, function(x){
           paste0('дом ',
                  gsub(gsub(x, pattern = '.*Дом:\\s([^;]*);.*', replacement = '\\1'),
                       pattern = 'Дом|ДОМ|д.', replacement = ''),
                  ',',
                  gsub(x, pattern = '.*Улица:\\s([^;]*);.*', replacement = '\\1'),
                  ' улица,',
                  ifelse(!grepl(x, pattern = 'Город :'),
                         gsub(x, pattern = '.*Город:\\s([^;]*);.*', replacement = '\\1'),
                         gsub(x, pattern = '.*Субъект РФ:\\s([^;]*);.*', replacement = '\\1')),
                  ',Россия')
       })]
    # отформатировать значения без ";", но с запятыми
    DT[!grepl(unname(unlist(DT[, col.address, with = F])), pattern = ';') &
           grepl(unname(unlist(DT[, col.address, with = F])), pattern = ','),
       address.OSM := sapply(address.OSM, uf.format.address.for.OSM)]
    
    # убрать индекс из значений без запятых
    DT[!grepl(address.OSM, pattern = ','),
       address.OSM := sapply(address.OSM, function(x){
           gsub(x, pattern = '\\d{6}', replacement = '')
       })]
    DT[, address.OSM := sapply(address.OSM, function(x){
        gsub(x, pattern = '\\s+', replacement = ' ')
    })]
    
    # # отладка
    # DT[1:12, c(col.address, 'address.OSM'), with = F]
    
    # узнаём координаты организаций через Яндекс
    n <- nrow(DT)
    console.clean.count <- 0
    
    if (length(colnames(DT)[colnames(DT) == paste0(col.coords.prefix, 'long')]) == 0) {
        DT[, lat := -200]
        DT[, long := -200]
    } else {
        colnames(DT)[colnames(DT) == paste0(col.coords.prefix, 'lat')] <- 'lat'
        colnames(DT)[colnames(DT) == paste0(col.coords.prefix, 'long')] <- 'long'
    }
    
    for (i in 1:n) {
        
        if (DT[i, 'long'] == -200) {
            
            addr <- DT$address.OSM[i]
            
            # отладка
            message(paste0(i, ': ', addr))
            console.clean.count <- console.clean.count + 1
            if (console.clean.count == max.console.lines) {
                cat("\014")
                console.clean.count <- 0
            }
            
            i.address <- uf.get.coords.Yandex(addr)
            i.address$long <- as.numeric(i.address$long)
            i.address$lat <- as.numeric(i.address$lat)
            
            # отладка
            message(paste0(i.address[1, ], collapse = ' '))
            console.clean.count <- console.clean.count + 1
            if (console.clean.count == max.console.lines) {
                cat("\014")
                console.clean.count <- 0
            }
            
            DT[i, 'lat'] <- i.address$lat
            DT[i, 'long'] <- i.address$long
            
            # статус в консоль
            msg <- paste0('Обработка адресов: ', round(i / n * 100, 1), '%... ')
            message(msg)
            console.clean.count <- console.clean.count + 1
            if (console.clean.count == max.console.lines) {
                cat("\014")
                console.clean.count <- 0
            }
        }
    }
    
    message(paste0('Всего адресов: ', nrow(DT)))
    message(paste0('Адреса не найдены: ',
                   sum(na.omit(DT$responsibleOrg.long == -200))))
    
    colnames(DT)[colnames(DT) == 'address.OSM'] <-  paste0(col.address, '.OSM')
    colnames(DT)[colnames(DT) == 'lat'] <-  paste0(col.coords.prefix, 'lat')
    colnames(DT)[colnames(DT) == 'long'] <-  paste0(col.coords.prefix, 'long')
    
    write.csv2(DT, out.file.name, row.names = F)
    msg <- paste0('Записан файл ', out.file.name)
    message(msg)
    
    return(DT)
}
