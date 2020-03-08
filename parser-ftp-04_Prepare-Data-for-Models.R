# ..............................................................................
# parser-ftp-04_Prepare-Data-for-Models.R
# 
# Построение моделей на данных по 44ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.3.1 (05 Mar 2020)
# ******************************************************************************
# Объединение таблиц и предобработка данных.
# На основе таблиц с разобранными XML по электронным аукционам 
#  делаем таблицу с переменными:
#
#  * purchaseNumber                 character   номер заявки 
#  * docPublishDate_notice	        POSIXct     дата публикации заявки
#  * responsibleRole	            character   роль ответственной организации
#  * lot.maxPrice	                numeric     начальная максимальная цена 
#                                               закупки, рублей
#  * restriction.shortName	        character   кодовое название ограничения 
#                                               процедуры закупки
#  * purchaseObject.OKPD2.code.2dig	character   первые 2 цифры кода товара 
#                                               закупки по ОКПД2
#  * application.count.p01	        integer     количество заявок, поданных на 
#                                               первый этап электронного 
#                                               аукциона
#  * protocol01Date	                POSIXct     дата протокола первого этапа 
#                                               аукциона
#  * max.price.offers.quantity	    numeric     максимальное количество ценовых
#                                               предложений от одного участника
#                                               аукциона 
#  * application.count.p02	        integer     количество заявок, допущенных ко 
#                                               второму этапу аукциона
#  * time.stage02.hours	            numeric     продолжительность второго этапа
#                                               аукциона, в часах
#  * protocol02Date	                POSIXct     дата протокола первого этапа 
#                                               аукциона
#  * winner.price	                numeric     цена победителя аукциона, рублей
#  * protocol03Date	                POSIXct     дата протокола третьего этапа 
#                                               аукциона
#  * total.time.days	            numeric     продолжительность процедуры от 
#                                               даты публикации заявки до 
#                                               публикации протокола третьего 
#                                               этапа аукциона, дней
#  * time.stage.01.days	            numeric     продолжительность первого 
#                                               этапа аукциона, дней
#  * time.stage.02.days	            numeric     продолжительность второго 
#                                               этапа аукциона, дней
#  * time.stage.03.hours	        numeric     продолжительность второго 
#                                               этапа аукциона, часов
# ..............................................................................

# читаем сохранённый список имён xml-файлов из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
file.name <- paste0(sDataSamplePath, 'xlms_names_list.csv')
all.xmls <- read.csv2(file.name, stringsAsFactors = F)[, 1]
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# читаем индекс из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sRawCSVPath, 'DT_index_melt_', lProcedureToScrap$procedureCode, 
               '.csv')
DT.proc.index <- uf.read.table.with.metadata(flnm)
dim(DT.proc.index)
str(DT.proc.index)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# Загрузка и подготовка данных -------------------------------------------------


# ЗАКАЗЧИК И МАКСИМАЛЬНАЯ ЦЕНА =================================================


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_responsibleOrgs_clean.csv')
DT.responsibleOrgs <- uf.read.table.with.metadata(flnm)
summary(DT.responsibleOrgs)
dim(DT.responsibleOrgs)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# * проверяем валюту начальной цены
if (length(table(DT.responsibleOrgs$lot.currency.code)) == 1) {
    # если всё в рублях, убираем валюту
    DT.responsibleOrgs[, lot.currency.code := NULL]
}

# table(DT.responsibleOrgs$responsibleRole)
# * сведения о закупщиках нужны для географических координат, пока отбрасываем
DT.responsibleOrgs[, responsibleOrg.regNum := NULL]
DT.responsibleOrgs[, responsibleOrg.fullName := NULL]
DT.responsibleOrgs[, responsibleOrg.postAddress := NULL]
DT.responsibleOrgs[, responsibleOrg.INN := NULL]
DT.responsibleOrgs[, responsibleOrg.KPP := NULL]
colnames(DT.responsibleOrgs)[colnames(DT.responsibleOrgs) == 
                            'docPublishDate'] <- 'docPublishDate_notice'
DT.responsibleOrgs[, docPublishDate_notice := 
                       uf.convert.char.to.date(docPublishDate_notice)]


# ..............................................................................
#
# ОГРАНИЧЕНИЯ НА ЗАКУПКУ
# ..............................................................................


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_restrictions_clean.csv')
DT.restrictions <- uf.read.table.with.metadata(flnm)
summary(DT.restrictions)
dim(DT.restrictions)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# пропуски есть только в столбцах ограничений
nas <- uf.count.nas.in.table(DT.restrictions)
nas$columns.na
table(DT.restrictions$restriction.shortName)
# похоже, пропуски -- это закупки без ограничений. меняем на 'no'
DT.restrictions[is.na(DT.restrictions$restriction.shortName), 
                restriction.shortName := 'no']

# * убираем дату
DT.restrictions[, docPublishDate := NULL]
# * убираем название ограничения
DT.restrictions[, restriction.name := NULL]


# набор данных для модели: создание ////////////////////////////////////////////
DT.model <- merge(DT.responsibleOrgs, DT.restrictions, 
                  by = c('purchaseNumber', 'fcsNotificationEF.id'))
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# ТОВАРЫ =======================================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_TPY_codes_clean.csv')
DT.TPY.codes <- uf.read.table.with.metadata(flnm)
summary(DT.TPY.codes)
dim(DT.TPY.codes)
# names(table(DT.TPY.codes$purchaseObject.KTRU.code.2dig))
# names(table(DT.TPY.codes$purchaseObject.OKPD2.code.2dig))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


nas <- uf.count.nas.in.table(DT.TPY.codes)
nas$columns.na

id.wo.okpd.ktru <- unique(select(filter(DT.TPY.codes, 
                                        is.na(purchaseObject.OKPD2.code) & 
                                            is.na(purchaseObject.KTRU.code) &
                                            is.na(drugPurchaseObjectInfo.MNNName)),
                            purchaseNumber))[, 1]

# окончательно разбираемся с кодами товаров
#  * оставляем только двузначные ОКПД и КТРУ
DT.TPY.codes[, purchaseObject.KTRU.code := NULL]
DT.TPY.codes[, purchaseObject.OKPD2.code := NULL]
#  * ставим код ОКПД 21 для лекарственных средств
DT.TPY.codes[!is.na(drugPurchaseObjectInfo.MNNName), 
             purchaseObject.OKPD2.code.2dig := '21']

# проверка .....................................................................
unique(select(filter(DT.TPY.codes, !is.na(drugPurchaseObjectInfo.MNNName)),
              purchaseObject.OKPD2.code.2dig))
# ..............................................................................

#  * убираем столбец с медицинскими кодами
DT.TPY.codes[, drugPurchaseObjectInfo.MNNName := NULL]
#  * у тех объявлений, где есть КТРУ, но нет ОКПД2, берём первые две цифры ОКПД2
#    из КТРУ (они совпадают)
DT.TPY.codes[is.na(purchaseObject.OKPD2.code.2dig) & 
                 !is.na(purchaseObject.KTRU.code.2dig), 
             purchaseObject.OKPD2.code.2dig := purchaseObject.KTRU.code.2dig]
#  * проблема в тех объявлениях, где есть и то, и другое, и они не совпадают
DT.tmp <- DT.TPY.codes[!is.na(purchaseObject.OKPD2.code.2dig) & 
                           !is.na(purchaseObject.KTRU.code.2dig), 
            ][purchaseObject.OKPD2.code.2dig != purchaseObject.KTRU.code.2dig, ]
#  * эти записи дублируем как новые строки таблицы (присоединяем снизу)
DT.tmp[, purchaseObject.OKPD2.code.2dig := purchaseObject.KTRU.code.2dig]
DT.tmp[, purchaseObject.OKPD2.name := purchaseObject.KTRU.name]
#  * теперь совмещаем и оставляем только ОКПД
DT.tmp <- rbind(DT.TPY.codes, DT.tmp)
DT.tmp[, purchaseObject.KTRU.code.2dig := NULL]
DT.tmp[, purchaseObject.KTRU.name := NULL]
DT.tmp <- unique(DT.tmp)
dim(DT.tmp)
DT.TPY.codes <- DT.tmp
names(table(DT.TPY.codes$purchaseObject.OKPD2.code.2dig))
#  * убираем описания товаров, оставляем только коды
DT.TPY.codes[, purchaseObject.OKPD2.name := NULL]
DT.TPY.codes <- unique(DT.TPY.codes)
dim(DT.TPY.codes)


# набор данных для модели: добавление столбцов /////////////////////////////////
DT.model <- merge(DT.model, DT.TPY.codes, 
                  by = c('purchaseNumber', 'fcsNotificationEF.id'))
# дальше работаем с таблицами по протоколам, 
#  fcsNotificationEF.id больше не нужен (он с самого начала был перестраховкой)
DT.model <- DT.model[, fcsNotificationEF.id := NULL]
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# УЧАСТНИКИ АУКЦИОНОВ ==========================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF1_clean.csv')
DT.protocols01 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols01)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocols01[, protocol01Date := uf.convert.char.to.date(protocol01Date)]

# оставляем тех участников аукционов, заявления которых были приняты
DT.protocols01 <- DT.protocols01[admitted == T, ]
tmp <- select(DT.protocols01, purchaseNumber, application.journalNumber)[, .N,
                                                          by = 'purchaseNumber']
DT.protocols01 <- merge(DT.protocols01, tmp, by = 'purchaseNumber')
colnames(DT.protocols01)[colnames(DT.protocols01) == 'N'] <- 
    'application.count.p01'
# * оставляем только столбцы для модели: количество принятых заявок + дата
DT.protocols01 <- select(DT.protocols01, purchaseNumber, fcsProtocolEF1.id,
                         application.journalNumber, application.count.p01, 
                         protocol01Date)

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF2_clean.csv')
DT.protocols02 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols02)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocols02[, protocol02Date := uf.convert.char.to.date(protocol02Date)]
DT.protocols02[, application.firstOffer.date := 
                   uf.convert.char.to.date(application.firstOffer.date)]
DT.protocols02[, application.lastOffer.date := 
                   uf.convert.char.to.date(application.lastOffer.date)]

# считаем max количество предложений цен
tmp.1 <- select(DT.protocols02, purchaseNumber, fcsProtocolEF2.id, 
                application.priceOffers.offersQuantity)[, lapply(.SD, max),
                                by = c('purchaseNumber', 'fcsProtocolEF2.id')]
colnames(tmp.1)[colnames(tmp.1) == 'application.priceOffers.offersQuantity'] <- 
    'max.price.offers.quantity'
# считаем количество участников второго этапа аукциона
tmp.2 <- select(DT.protocols02, purchaseNumber, fcsProtocolEF2.id,
                application.journalNumber)[, .N, 
                                by = c('purchaseNumber', 'fcsProtocolEF2.id')]
colnames(tmp.2)[colnames(tmp.2) == 'N'] <- 'application.count.p02'

# добавляем рассчитанные столбцы к исходной таблице
DT.protocols02 <- merge(DT.protocols02, tmp.1, 
                        by = c('purchaseNumber', 'fcsProtocolEF2.id'))
DT.protocols02 <- merge(DT.protocols02, tmp.2, 
                        by = c('purchaseNumber', 'fcsProtocolEF2.id'))

# считаем длительность аукциона по разнице времени ценовых предложений
DT.protocols02[!is.na(application.lastOffer.date), 
               max.lastOffer.date := max(application.lastOffer.date), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[!is.na(application.firstOffer.date), 
               min.firstOffer.date := max(application.firstOffer.date), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[!is.na(max.lastOffer.date) & !is.na(min.firstOffer.date), 
               time.stage02.hours := max(as.numeric(difftime(max.lastOffer.date,
                             min.firstOffer.date, 'hours')), round(5/60/60, 3)), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[, time.stage02.hours := unclass(time.stage02.hours)]
summary(DT.protocols02$time.stage02.hours)

# * оставляем только столбцы для модели: максимальное количество предложений
#    цен на аукционе; число участников аукциона
DT.protocols02 <- select(DT.protocols02, purchaseNumber, fcsProtocolEF2.id,
                         application.journalNumber, application.lastOffer.price,
                         max.price.offers.quantity, application.count.p02,
                         time.stage02.hours, protocol02Date)
DT.protocols02 <- unique(DT.protocols02)

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF3_clean.csv')
DT.protocols03 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols03)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocols03[, protocol03Date := uf.convert.char.to.date(protocol03Date)]


# сколько извещений с одинаковыми номерами #####################################
dt.dbl.notice <- data.table(filter(unique(select(DT.responsibleOrgs,
                                                 purchaseNumber,
    docPublishDate_notice))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1))

if (nrow(dt.dbl.notice) > 0) {
    # вычисляем дату самого последнего для каждого извещения
    last.dates <- sapply(dt.dbl.notice$purchaseNumber, function(x) {
        max(DT.responsibleOrgs[purchaseNumber == x, ]$docPublishDate_notice)
    })
    last.dates <- data.table(purchaseNumber = names(last.dates),
                             last.dates = last.dates)
    last.dates$last.dates <- as.POSIXct(last.dates$last.dates,
                                        origin = lubridate::origin,
                                        tz = 'Europe/Moscow')
    dt.dbl.notice <- merge(dt.dbl.notice, last.dates)
    # результат
    print(dt.dbl.notice[order(-N), ])
    length(dt.dbl.notice$purchaseNumber)
}


# сколько протоколов 1 этапа с одинаковыми номерами ############################
dt.dbl.p01 <- data.table(filter(unique(select(DT.protocols01, purchaseNumber, 
            protocol01Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1))

if (nrow(dt.dbl.p01) > 0) {
    # вычисляем дату самого последнего для каждого протокола
    last.dates <- sapply(dt.dbl.p01$purchaseNumber, function(x) {
        max(DT.protocols01[purchaseNumber == x, ]$protocol01Date)
    })
    last.dates <- data.table(purchaseNumber = names(last.dates),
                             last.dates = last.dates)
    last.dates$last.dates <- as.POSIXct(last.dates$last.dates, 
                                        origin = lubridate::origin, 
                                        tz = 'Europe/Moscow')
    dt.dbl.p01 <- merge(dt.dbl.p01, last.dates)
    # результат
    print(dt.dbl.p01[order(-N), ])
    length(dt.dbl.p01$purchaseNumber)
}


# сколько протоколов 2 этапа с одинаковыми номерами ############################
dt.dbl.p02 <- data.table(filter(unique(select(DT.protocols02, purchaseNumber, 
            protocol02Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1))

if (nrow(dt.dbl.p02) > 0) {
    # вычисляем дату самого последнего для каждого извещения
    last.dates <- sapply(dt.dbl.p02$purchaseNumber, function(x) {
        max(DT.protocols02[purchaseNumber == x, ]$protocol02Date)
    })
    last.dates <- data.table(purchaseNumber = names(last.dates),
                             last.dates = last.dates)
    last.dates$last.dates <- as.POSIXct(last.dates$last.dates, 
                                        origin = lubridate::origin, 
                                        tz = 'Europe/Moscow')
    dt.dbl.p02 <- merge(dt.dbl.p02, last.dates)
    # результат
    print(dt.dbl.p02[order(-N), ])
    length(dt.dbl.p02$purchaseNumber)
}


# сколько протоколов 3 этапа с одинаковыми номерами ############################
dt.dbl.p03 <- data.table(filter(unique(select(DT.protocols03, purchaseNumber, 
            protocol03Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1))

if (nrow(dt.dbl.p03) > 0) {
    # вычисляем дату самого последнего для каждого извещения
    last.dates <- sapply(dt.dbl.p03$purchaseNumber, function(x) {
        max(DT.protocols03[purchaseNumber == x, ]$protocol03Date)
    })
    last.dates <- data.table(purchaseNumber = names(last.dates),
                             last.dates = last.dates)
    last.dates$last.dates <- as.POSIXct(last.dates$last.dates, 
                                        origin = lubridate::origin, 
                                        tz = 'Europe/Moscow')
    dt.dbl.p03 <- merge(dt.dbl.p03, last.dates)
    # результат
    print(dt.dbl.p03[order(-N), ])
    length(dt.dbl.p03$purchaseNumber)
}

# по протоколам выпадает вот такая доля извещений
pID.doubles <- unique(c(dt.dbl.p01$purchaseNumber, dt.dbl.p02$purchaseNumber, 
                        dt.dbl.p03$purchaseNumber))
length(pID.doubles) / length(unique(DT.responsibleOrgs$purchaseNumber))


# вариант 1: оставляем только самые свежие #####################################
if (nrow(dt.dbl.notice) > 0) {
    notice.wo.doubles.01 <- unique(merge(DT.responsibleOrgs, 
                                         select(dt.dbl.notice, -N), 
                                         by.x = c('purchaseNumber', 
                                                  'docPublishDate_notice'),
                                         by.y = c('purchaseNumber', 
                                                  'last.dates')))
    notice.wo.doubles.02 <- unique(data.table(filter(DT.responsibleOrgs, 
                          !(purchaseNumber %in% dt.dbl.notice$purchaseNumber))))
    DT.responsibleOrgs <- rbind(notice.wo.doubles.01, notice.wo.doubles.02)    
}

if (nrow(dt.dbl.p01) > 0) {
    p.01.wo.doubles.01 <- unique(merge(DT.protocols01, select(dt.dbl.p01, -N), 
                                by.x = c('purchaseNumber', 'protocol01Date'),
                                by.y = c('purchaseNumber', 'last.dates')))
    p.01.wo.doubles.02 <- unique(data.table(filter(DT.protocols01, 
                            !(purchaseNumber %in% dt.dbl.p01$purchaseNumber))))
    DT.protocols01 <- rbind(p.01.wo.doubles.01, p.01.wo.doubles.02)    
}

if (nrow(dt.dbl.p02) > 0) {
    p.02.wo.doubles.01 <- unique(merge(DT.protocols02, select(dt.dbl.p02, -N),
                                by.x = c('purchaseNumber', 'protocol02Date'),
                                by.y = c('purchaseNumber', 'last.dates')))
    p.02.wo.doubles.02 <- unique(data.table(filter(DT.protocols02, 
                            !(purchaseNumber %in% dt.dbl.p02$purchaseNumber))))
    DT.protocols02 <- rbind(p.02.wo.doubles.01, p.02.wo.doubles.02)
}

if (nrow(dt.dbl.p03) > 0) {
    p.03.wo.doubles.01 <- unique(merge(DT.protocols03, select(dt.dbl.p03, -N), 
                                by.x = c('purchaseNumber', 'protocol03Date'),
                                by.y = c('purchaseNumber', 'last.dates')))
    p.03.wo.doubles.02 <- unique(data.table(filter(DT.protocols03, 
                            !(purchaseNumber %in% dt.dbl.p03$purchaseNumber))))
    DT.protocols03 <- rbind(p.03.wo.doubles.01, p.03.wo.doubles.02)
}


# # вариант 2: просто удаляем все повторы ########################################
# DT.protocols01 <- 
#     DT.protocols01[!(purchaseNumber %in% dt.dbl.p01$purchaseNumber), ]
# DT.protocols02 <- 
#     DT.protocols02[!(purchaseNumber %in% dt.dbl.p02$purchaseNumber), ]
# DT.protocols03 <- 
#     DT.protocols03[!(purchaseNumber %in% dt.dbl.p03$purchaseNumber), ]


# определяем цену победителя аукциона ##########################################
tmp.1 <- select(DT.protocols01, purchaseNumber, application.journalNumber,
                protocol01Date)
tmp.2 <- select(DT.protocols02, purchaseNumber, application.journalNumber,
                protocol02Date, application.lastOffer.price)
tmp.3 <- select(DT.protocols03, purchaseNumber, application.journalNumber,
                protocol03Date, application.appRating)

tmp <- select(filter(merge(merge(tmp.1, tmp.2, 
                        by = c('purchaseNumber', 'application.journalNumber')),
                        tmp.3, 
                        by = c('purchaseNumber', 'application.journalNumber')),
                     application.appRating == 1), purchaseNumber, 
              application.lastOffer.price)
tmp <- data.table(unique(tmp))
# добавляем цену победителя к данным по протоколам
DT.protocols03 <- merge(DT.protocols03, tmp, by = 'purchaseNumber')
colnames(DT.protocols03)[colnames(DT.protocols03) == 
                             'application.lastOffer.price'] <- 'winner.price'

# протоколы 1: оставляем только столбцы для модели
DT.protocols01 <- select(DT.protocols01, fcsProtocolEF1.id, purchaseNumber, 
                         application.count.p01, protocol01Date)
DT.protocols01 <- unique(DT.protocols01)
# протоколы 2: оставляем только столбцы для модели
DT.protocols02 <- select(DT.protocols02, fcsProtocolEF2.id, purchaseNumber, 
                         max.price.offers.quantity, application.count.p02,
                         time.stage02.hours, protocol02Date)
DT.protocols02 <- unique(DT.protocols02)
# протоколы 3: оставляем только столбцы для модели
DT.protocols03 <- select(DT.protocols03, fcsProtocolEF3.id,
                         purchaseNumber, winner.price, protocol03Date)
DT.protocols03 <- unique(DT.protocols03)

# объединяем данные по протоколам этапов аукциона
DT.protocols <- merge(DT.protocols01, DT.protocols02, 'purchaseNumber')
DT.protocols <- merge(DT.protocols, DT.protocols03, by = 'purchaseNumber')
DT.protocols <- unique(DT.protocols)
dim(DT.protocols)
DT.protocols[, fcsProtocolEF1.id := NULL]
DT.protocols[, fcsProtocolEF2.id := NULL]
DT.protocols[, fcsProtocolEF3.id := NULL]
DT.protocols <- unique(DT.protocols)
dim(DT.protocols)


# набор данных для модели: добавление столбцов /////////////////////////////////
# убираем ID протоколов
DT.model <- merge(DT.model, DT.protocols, by = 'purchaseNumber')
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na

# считаем переменные на основе дат
DT.model[, total.time.days := as.numeric(difftime(protocol03Date, 
                                                  docPublishDate_notice,
                                                  units = 'days'))]
DT.model[, time.stage.01.days := as.numeric(difftime(protocol01Date, 
                                                     docPublishDate_notice,
                                                     units = 'days'))]
DT.model[, time.stage.02.days := as.numeric(difftime(protocol02Date, 
                                                     protocol01Date,
                                                     units = 'days'))]
DT.model[, time.stage.03.hours := as.numeric(difftime(protocol03Date, 
                                                      protocol02Date, 
                                                      units = 'hours'))]
str(DT.model)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# записываем таблицу с данными для модели >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRawCSVPath, 'DT_model_', lProcedureToScrap$procedureCode, 
               '_', my.region$name, '.csv')
uf.write.table.with.metadata(DT.model, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
