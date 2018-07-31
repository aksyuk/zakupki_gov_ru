# ..............................................................................
# ПОПЫТКА РАСКЛЕИТЬ ВЕСЬ ФАЙЛ ЦЕЛИКОМ: ПОЛУЧИЛОСЬ ОЧЕНЬ ДОЛГО И НЕЭФФЕКТИВНО

dim(DT)
dt.step <- 100
dt.count <- nrow(DT) %/% dt.step
if (nrow(DT) / dt.step != nrow(DT) %/% dt.step) {
    dt.count <- dt.count + 1
}
fst.row <- 1
lst.row <- fst.row + dt.step - 1
file.count <- 1
for (i in file.count:dt.count) {
    df <- data.frame(DT[fst.row:lst.row, ])
    rownames(df) <- fst.row:lst.row
    df <- uf.normalise.table(df, paste0(sPlotPath, 'plot_',
                                    formatC(file.count, width = 3,
                                            format = 'd', flag = '0'), '.png'),
                         paste0('DF_curr_part_',
                                formatC(file.count, width = 3,
                                        format = 'd', flag = '0')))
    out.file.name <- paste0(sRawCSVPath, 'DF_curr_part_',
                            formatC(file.count, width = 3,
                                    format = 'd', flag = '0'), '.csv')

    message(paste0('Записываю файл ', out.file.name, '... (', file.count,
                   ' из ', dt.count, ').'))

    write.csv2(df, out.file.name, row.names = F)
    # write_delim(df, out.file.name, delim = ";")
    file.count <- file.count + 1

    fst.row <- lst.row + 1
    lst.row <- fst.row + dt.step - 1
}

flnms <- paste0(sRawCSVPath,
                grep(dir(sRawCSVPath), pattern = 'DF_curr_part_.*[.]csv', 
                     value = T))
length(flnms)
flnms[1:10]


# сбить всё в один файл
f.count <- 1
n <- length(flnms)
console.clean.count <- 0

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
    if (console.clean.count == iMaxConsoleStatusLines * 2) {
        cat("\014")
        console.clean.count <- 0
    }
    
    
    f.count <- f.count + 1
}

dim(DT.all)
DT.all


out.file.name <- paste0(sRawCSVPath, 'DT_fcsProtocolEF1_clean.csv')
write.csv2(DT.all, out.file.name, row.names = F)

file.remove(flnms)



f.count <- 1
n <- length(flnms)
console.clean.count <- 0

# вытащить всех уникальных заказчиков
columns.to.search <- c('responsibleOrg.regNum', 'responsibleOrg.fullName')
all.orgs <- NULL

for (f in flnms) {

    message(paste0('Reading file ', f, '... (', f.count, ' из ', n,')'))
    console.clean.count <- console.clean.count + 1

    DT <- read.csv2(f, colClasses = rep('character', 33))

    message(paste0('DT size: ', round(object.size(DT) / 1024 / 1024,
                                      1), ' Mb. nrow = ', nrow(DT)))

    # DT <- read_delim(f, delim = ';', col_types = rep('c', 33))
    DT <- data.table(DT[, columns.to.search])
    DT <- unique(DT)

    if (f.count == 1) {
        all.orgs <- copy(DT)
    } else {
        all.orgs <- data.table(rbind(all.orgs, DT))
        all.orgs <- unique(all.orgs)
    }

    rm(DT)

    if (console.clean.count == 10) {
        cat("\014")
        console.clean.count <- 0
    }

    message(paste0('Table size: ', round(object.size(all.orgs) / 1024 / 1024,
                                         1), ' Mb. nrow = ', nrow(all.orgs),
                   '\n'))

    f.count <- f.count + 1
}


# ..............................................................................
# # вытаскиваем заказчиков и участников аукционов
# # ЗАКАЗЧИКИ

# база Open Street Map

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


# API Google Maps стал платным с середины июля 2018, поэтому адреса, 
#  не найденые на OSM, искали в Яндексе вручную
# цены на Geocoding API:
#  https://developers.google.com/maps/documentation/geocoding/usage-and-billing