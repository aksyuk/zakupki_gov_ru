
# ............................................................................
#
# ЗАКАЗЧИКИ
# ............................................................................
#
columns.to.search <- c('responsibleOrg.regNum', 'responsibleOrg.fullName',
                       'responsibleOrg.postAddress')
all.orgs <-
    unique(data.table(DT.notif[, columns.to.search, with = F]))

# есть записи с расхождениями в почтовом индексе; оставляем только уникальные id
all.orgs <- all.orgs[order(responsibleOrg.regNum), ]
n <- nrow(all.orgs)
flags <- c(T, rep(NA, n - 1))
for (i in 2:n) {
    flags[i] <- all.orgs$responsibleOrg.regNum[i] !=
        all.orgs$responsibleOrg.regNum[i - 1]
}
all.orgs <- all.orgs[flags, ]

# делаем адреса и узнаём координаты через Яндекс, потому что это единственный
#  рабочий вариант
all.orgs <-
    uf.process.addresses.to.coords(all.orgs,
                                   col.address = 'responsibleOrg.postAddress',
                                   col.coords.prefix = 'responsibleOrg.',
                                   out.file.name = paste0(sRawCSVPath,
                                                          'df_all_orgs_long_lat.csv'))
# обрабатываем пропуски
DT <- all.orgs[is.na(responsibleOrg.long), ]
n <- nrow(DT)
for (i in 1:n) {
    addr <- DT[i, ]$responsibleOrg.postAddress
    longlat <- uf.get.coords.Yandex(addr)
    
    DT[i, ]$responsibleOrg.lat <- as.numeric(longlat$lat)
    DT[i, ]$responsibleOrg.long <- as.numeric(longlat$long)
}
all.orgs <- all.orgs[!is.na(responsibleOrg.long), ]
all.orgs <- rbind(all.orgs, DT)

# ВЫВОД: неструктурированные адреса вообще не надо форматировать,
#  Яндекс скушает их и так

all.orgs[is.na(responsibleOrg.lat), ]
longlat <- data.frame(long = 49.306991, lat = 55.933026)
all.orgs[responsibleOrg.regNum == '01113000013',
         responsibleOrg.lat := as.numeric(longlat$lat)]
all.orgs[responsibleOrg.regNum == '01113000013',
         responsibleOrg.long := as.numeric(longlat$long)]

# сохраняем таблицу с координатами
write.csv2(all.orgs, paste0(sRefPath, 'df_all_orgs_long_lat.csv'), row.names = F)


# ............................................................................
#
# ПРОТОКОЛЫ
# ............................................................................
#
# преобразовываем в дату-время столбец protocolDate
DT.protocols02[, protocolDate_POSIXct :=
                   as.POSIXct(gsub(DT.protocols02$protocolDate,
                                   pattern = 'T', replacement = ' '))]
DT.protocols02[, protocolDate := NULL]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols02)[colnames(DT.protocols02) == 'protocolDate_POSIXct'] <-
    'protocol02Date_POSIXct'





# все уникальные участники
length(unique(DT.all.EA$applications.postAddress))
all.participants.addr <- unique(DT.all.EA[, applications.inn, applications.postAddress])
all.participants.addr <- uf.process.addresses.to.coords(all.participants.addr,
                                                        col.address = 'applications.postAddress',
                                                        col.coords.prefix = 'participant.',
                                                        out.file.name = paste0(sRefPath, 'dt_all_participants_long_lat.csv'))


# ..............................................................................
#
# СОВМЕЩЕНИЕ ТАБЛИЦ (+РАССТОЯНИЯ)
# ..............................................................................
#
# # 5. СОВМЕЩАЕМ ТАБЛИЦЫ И ДОБАВЛЯЕМ КООРДИНАТЫ ----------------------------------
#
#
# DT.all.EA <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_all_auctions_clean.csv'),
#                        stringsAsFactors = F,
#                        colClasses = c('character', 'numeric', 'character',
#                                       'logical', rep('character', 2),
#                                       rep('numeric', 3), rep('character', 3),
#                                       'numeric', rep('character', 2))))
# str(DT.all.EA)
# dim(DT.all.EA)
# 
# all.participants.addr <-
#     data.table(read.csv2(paste0(sRefPath, 'dt_all_participants_long_lat.csv'),
#                          colClasses = c(rep('character', 2), rep('numeric', 2))))
# 
# summary(all.participants.addr)
# all.participants.addr[participant.long < 5, ]
# DT.protocols03[applications.inn == '6318002353',]
# all.participants.addr[participant.long < 5, applications.postAddress :=
#                           'г.Самара, пер.Карякина, д.2, оф.51']
# addr <- all.participants.addr[participant.long < 5, ]$applications.postAddress
# longlat <- uf.get.coords.Yandex(addr)
# all.participants.addr[participant.long < 5, participant.long :=
#                           as.numeric(longlat$long)]
# all.participants.addr[participant.long < 5, participant.lat :=
#                           as.numeric(longlat$lat)]
# 
# # правим адреса с косяками
# loop.inns <- all.participants.addr[nchar(applications.postAddress) < 25, 
#                                    ]$applications.inn
# 
# i <- 61
# loop.inns[i]
# 
# addr <- 'Санкт-Петербург г, пр-кт.Науки, д.18, лит.А'
# all.participants.addr[applications.inn == loop.inns[i], 
#                       applications.postAddress := addr]
# all.participants.addr[applications.inn %in% loop.inns[i], ]
# 
# 
# all.participants.addr[applications.inn %in% loop.inns, ]
# 
# 
# n <- length(loop.inns)
# for (i in 1:n) {
#     message(paste0(i, ' из ', n, ' (', round(i / n * 100, 1), '%)'))
#     
#     addr <- all.participants.addr[applications.inn == loop.inns[i], 
#                                   ]$applications.postAddress
#     
#     message(paste0(addr, collapse = ' '))
#     
#     longlat <- uf.get.coords.Yandex(addr)
#     
#     message(longlat)
#     
#     all.participants.addr[applications.inn == loop.inns[i], 
#                           participant.long := as.numeric(longlat$long)]
#     all.participants.addr[applications.inn == loop.inns[i], 
#                           participant.lat := as.numeric(longlat$lat)]
# }
#
#
# # из-за повторяющихся адресов выходит фигня полная
# all.participants.addr[, .N, by = applications.inn][N > 1, ]
# all.participants.addr[applications.inn == 1655372820, ]
# # поэтому мы их МАКСИМАЛЬНО ОСТОРОЖНО убираем
# all.participants.addr <- all.participants.addr[order(applications.inn), ]
# n <- nrow(all.participants.addr)
# flags <- c(T, rep(NA, n - 1))
# for (i in 2:n) {
# 
#     flags[i] <- all.participants.addr$applications.inn[i] !=
#         all.participants.addr$applications.inn[i - 1]
# }
# all.participants.addr <- all.participants.addr[flags, ]
# 
# 
# # там были NA у неизвестных адресов, ниже вбиты вручную
# all.participants.addr[is.na(participant.long), ]
#
# # некоторые координаты можно забить вручную
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = '141100 г. Щелково Московской области, ул. Талсинская, дом 3'), 
#                       ]$participant.long <- 37.993167
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = '141100 г. Щелково Московской области, ул. Талсинская, дом 3'), 
#                       ]$participant.lat <- 55.925077
# 
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = 'Страна: Российская Федерация; ОКАТО: 45290582000; Почтовый индекс: 109316; Субъект РФ: Москва; Улица: Волгоградский пр-т; Дом: д. 42, корп. 42А; Офис: офис 25;'), 
#                       ]$participant.long <- 37.725343
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = 'Страна: Российская Федерация; ОКАТО: 45290582000; Почтовый индекс: 109316; Субъект РФ: Москва; Улица: Волгоградский пр-т; Дом: д. 42, корп. 42А; Офис: офис 25;'), 
#                       ]$participant.lat <- 55.710246
# 
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = 'Казань,Волочаевская ул.,д. 6'), 
#                       ]$participant.long <- 49.173482
# all.participants.addr[grepl(applications.postAddress, 
#                             pattern = 'Казань,Волочаевская ул.,д. 6'), 
#                       ]$participant.lat <- 55.710246
#   
# # а эти адреса и координаты узнаём из интернета
# inn <- 1661042234
# addr <- 'г.Казань, ул.Карагандинская, д.6А, кв.47'
# all.participants.addr[applications.inn == inn, ]$applications.postAddress <- addr
# all.participants.addr[applications.inn == inn, address.OSM :=
#                           uf.format.address.for.OSM(applications.postAddress)]
# longlat <- 
#     uf.get.coords.Yandex(all.participants.addr[applications.inn == inn, ]$address.OSM)
# all.participants.addr[applications.inn == inn, participant.long := as.numeric(longlat$long)]
# all.participants.addr[applications.inn == inn, participant.lat := as.numeric(longlat$lat)]
# all.participants.addr[applications.inn == inn, ]
#
# # вот здесь реально адрес "79"
# #  и это проблема с парсингом XML, ПОТОМУ ЧТО НАДО ЭКРАНИРОВАТЬ ХЕШИ 
# #  В ЗНАЧЕНИЯХ ТЕГОВ !!!!!
# all.participants.addr[applications.inn == '165804134156', ]$applications.postAddress <- 
#     'республика Татарстан, город Казань'
# all.participants.addr[applications.inn == '165804134156', address.OSM := 
#                           uf.format.address.for.OSM(applications.postAddress)]
# longlat <- uf.get.coords.Yandex(all.participants.addr[applications.inn == '165804134156', ]$address.OSM)
# all.participants.addr[applications.inn == '165804134156', participant.long := 
#                           as.numeric(longlat$long)]
# all.participants.addr[applications.inn == '165804134156', participant.lat := 
#                           as.numeric(longlat$lat)]
#                      
# write.csv2(all.participants.addr,
#            paste0(sRefPath, 'dt_all_participants_long_lat.csv'),
#            row.names = F)
#
# # проверка
# sum(unique(all.participants.addr$applications.inn) %in% 
#         unique(DT.all.EA$applications.inn))
# length(unique(all.participants.addr$applications.inn))
# sort(unique(all.participants.addr$applications.inn)[!(unique(all.participants.addr$applications.inn) %in% 
#                                                           unique(DT.all.EA$applications.inn))])
# 
# sum(unique(DT.all.EA$applications.inn) %in% 
#         unique(all.participants.addr$applications.inn))
# length(unique(DT.all.EA$applications.inn))
# sort(unique(DT.all.EA$applications.inn)[!(unique(DT.all.EA$applications.inn) %in% 
#                                               unique(all.participants.addr$applications.inn))])
# 
# 
# # Добавляем координаты участников аукционов ....................................
# DT <- unique(all.participants.addr[, c('applications.inn', 'participant.long',
#                                        'participant.lat'), with = F])
# DT.all.EA <- merge(DT.all.EA, DT, by = 'applications.inn', all.x = T)
# 
# 
# # Добавляем координаты заказчиков ..............................................
# 
# all.orgs <-
#     data.table(read.csv2(paste0(sRefPath, 'df_all_orgs_long_lat.csv'),
#                          colClasses = c(rep('character', 4), rep('numeric', 2))))
# 
# # внешний ключ -- id заказчика -- это первые 11 символов notice ID
# DT.all.EA$responsibleOrg.regNum <- substr(DT.all.EA$purchaseNumber, 1, 11)
# 
# 
# # проверка
# sum(unique(all.orgs$responsibleOrg.regNum) %in% 
#         unique(DT.all.EA$responsibleOrg.regNum))
# length(unique(all.orgs$responsibleOrg.regNum))
# sort(unique(all.orgs$responsibleOrg.regNum)[!(unique(all.orgs$responsibleOrg.regNum) %in% 
#                                                   unique(DT.all.EA$responsibleOrg.regNum))])
# 
# sum(unique(DT.all.EA$applications.inn) %in% 
#         unique(all.participants.addr$applications.inn))
# length(unique(DT.all.EA$applications.inn))
# sort(unique(DT.all.EA$applications.inn)[!(unique(DT.all.EA$applications.inn) %in% 
#                                               unique(all.participants.addr$applications.inn))])
# 
# 
# DT <- all.orgs[, c('responsibleOrg.regNum', 'responsibleOrg.long',
#                    'responsibleOrg.lat'), with = F]
# # DT <- all.orgs[, c('responsibleOrg.regNum', 'responsibleOrg.postAddress'), with = F]
# DT.all.EA <- merge(DT.all.EA, DT,  by = 'responsibleOrg.regNum', all.x = T)
# dim(DT.all.EA)
# summary(DT.all.EA)
# 
# # записываем файл с цепочкой протоколов с с координатами
# write.csv2(DT.all.EA, paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat.csv'),
#            row.names = F)
# cls <- sapply(DT.all.EA, class)
# nms <- names(cls)
# df <- data.frame(col.names = nms, col.classes = cls)
# rownames(df) <- 1:nrow(df)
# write.csv2(df, paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat_META.csv'),
#            row.names = F)
#
#
#
# 6. СЧИТАЕМ РАССТОЯНИЯ И ДЕЛАЕМ ОБЗОР -----------------------------------------

flnm <- paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat_META.csv')
meta <- read.csv2(flnm, stringsAsFactors = F)
DT.all.EA <- 
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat.csv'),
                         colClasses = meta$col.classes))
str(DT.all.EA)
dim(DT.all.EA)
summary(DT.all.EA)

# # матрица расстояний от Яндекса ПЛАТНАЯ
# #  info: https://tech.yandex.ru/routing/distance_matrix/?from=mapsapi
# #  поэтому здесь будут расстояния по прямой
# # расстояния по прямой заказчик - поставщик
# DT.all.EA[, dist.crow := 6371 * acos(sin(participant.lat * pi / 180) * sin(responsibleOrg.lat * pi / 180) + 
#                                          cos(participant.lat * pi / 180) * cos(responsibleOrg.lat * pi / 180) * 
#                                          cos(participant.long * (pi / 180) - responsibleOrg.long * (pi / 180)))]
# 
# summary(DT.all.EA$dist.crow)
# DT.all.EA[is.na(dist.crow), ]
# DT.all.EA[is.nan(dist.crow), ]

# write.csv2(DT.all.EA, paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat.csv'),
#            row.names = F)
# cls <- sapply(DT.all.EA, class)
# nms <- names(cls)
# df <- data.frame(col.names = nms, col.classes = cls)
# rownames(df) <- 1:nrow(df)
# write.csv2(df, paste0(sRawCSVPath, 'DT_all_auctions_clean_long_lat_META.csv'),
#            row.names = F)


DT.all.EA[, c('responsibleOrg.postAddress', 'applications.postAddress', 'dist.crow'), 
          with = F]

gp <- ggplot(data = DT.all.EA, aes(x = dist.crow))
gp <- gp + geom_histogram(fill = grey(0.8), col = 'black')
gp <- gp + xlab('Расстояния заказчик - участники аукциона, км')
gp <- gp + ylab('Частота')
gp
png(paste0(sPlotPath, 'plot-for-report-01.png'), width = 500, height = 500)
gp
dev.off()

gp <- ggplot(data = DT.all.EA[application.appRating == 1, ], aes(x = dist.crow))
gp <- gp + geom_histogram(fill = grey(0.8), col = 'black')
gp <- gp + xlab('Расстояния заказчик - победители аукциона, км')
gp <- gp + ylab('Частота')
gp
png(paste0(sPlotPath, 'plot-for-report-02.png'), width = 500, height = 500)
gp
dev.off()

colnames(DT.all.EA)
DT <- DT.notif[, c('fcsNotificationEF.purchaseNumber', 
                   'purchaseObject.OKPD2.code'), with = F]
DT.all.EA <- merge(DT.all.EA, DT, by.x = 'purchaseNumber', 
                   by.y = 'fcsNotificationEF.purchaseNumber',
                   all.x = T, all.y = F)


# пробуем посмотреть по отраслям
flnm <- paste0(sRawCSVPath, 'DT_all_OKPD2_clean_META.csv')
meta <- read.csv2(flnm, stringsAsFactors = F)
DT <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_all_OKPD2_clean.csv'),
                           colClasses = meta$col.classes))
# ОТКУДА NA???
DT.all.EA <-
    merge(DT.all.EA, unique(DT[, c('fcsNotificationEF.purchaseNumber', 
                                   'purchaseObject.OKPD2.code.2dig'), with = F]),
          by.x = 'purchaseNumber',
          by.y = 'fcsNotificationEF.purchaseNumber', 
          all.x = T, all.y = F)