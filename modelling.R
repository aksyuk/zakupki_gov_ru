# ..............................................................................
# modelling.R
# 
# Построение моделей на данных по 44ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.0 (28.02.2020)
# 
# ..............................................................................



# Загрузка и подготовка данных -------------------------------------------------


# ЗАКАЗЧИК И МАКСИМАЛЬНАЯ ЦЕНА =================================================


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_responsibleOrgs_clean.csv')
DT.responsibleOrgs <- uf.read.table.with.metadata(flnm)
summary(DT.responsibleOrgs)
dim(DT.responsibleOrgs)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# * проверяем валюту начальной цены
table(DT.responsibleOrgs$lot.currency.code)
# всё в рублях, убираем валюту
DT.responsibleOrgs[, lot.currency.code := NULL]
table(DT.responsibleOrgs$responsibleRole)
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
names(table(DT.TPY.codes$purchaseObject.KTRU.code.2dig))
names(table(DT.TPY.codes$purchaseObject.OKPD2.code.2dig))
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


# предварительный анализ данных ................................................
# количество разных кодов товаров в некоторых объявлениях очень велико
tmp <- DT.TPY.codes[, .N, by = purchaseNumber][order(-N), ]
length(unique(DT.TPY.codes[purchaseNumber == tmp[1, ]$purchaseNumber, 
                           ]$purchaseObject.OKPD2.name))
length(unique(DT.TPY.codes[purchaseNumber == tmp[1, ]$purchaseNumber, 
                           ]$purchaseObject.OKPD2.code.2dig))
# ..............................................................................


#  * убираем описания товаров, оставляем только коды
DT.TPY.codes[, purchaseObject.OKPD2.name := NULL]
DT.TPY.codes <- unique(DT.TPY.codes)
dim(DT.TPY.codes)


# предварительный анализ данных ................................................
#  распределение заявок по кодам ОКПД2
flnm <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/KeyBooks/okpd2/okpd2.csv'
dt.okpd2 <- data.table(read.csv(flnm, stringsAsFactors = F, 
                                colClasses = 'character'))
dt.okpd2 <- dt.okpd2[nchar(code) == 2, ]
dt.okpd2[, comment := NULL]

purchise.count.by.okpd <- data.table(table(DT.TPY.codes$purchaseObject.OKPD2.code.2dig))
purchise.count.by.okpd <- merge(purchise.count.by.okpd, dt.okpd2, 
                                by.x = 'V1', by.y = 'code')
purchise.count.by.okpd <- purchise.count.by.okpd[order(-N), ]
purchise.count.by.okpd[, prcnt := round(N / nrow(DT.TPY.codes) * 100, 1)]
colnames(purchise.count.by.okpd)[colnames(purchise.count.by.okpd) == 'V1'] <- 
    'okpd.2dig'
head(purchise.count.by.okpd)

tmp <- filter(DT.TPY.codes[, .N, by = purchaseNumber], N == 4)
msg <- paste0('Заявок с кодами товаров > 1: ',
              round(nrow(tmp) / 
                        length(unique(DT.TPY.codes$purchaseNumber)) * 100, 1),
              '% (', nrow(tmp), ')')
message(msg)
# ..............................................................................


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


# оставляем тех участников аукционов, заявления которых были приняты
DT.p01.model <- DT.protocols01[admitted == T, ]
tmp <- select(DT.p01.model, purchaseNumber, application.journalNumber)[, .N,
                                                          by = 'purchaseNumber']
DT.p01.model <- merge(DT.p01.model, tmp, by = 'purchaseNumber')
colnames(DT.p01.model)[colnames(DT.p01.model) == 'N'] <- 
    'application.count.p01'
# * оставляем только столбцы для модели: количество принятых заявок + дата
DT.p01.model <- select(DT.p01.model, purchaseNumber, fcsProtocolEF1.id,
                       application.count.p01, protocol01Date)
# преобразование дат
DT.p01.model[, protocol01Date := uf.convert.char.to.date(protocol01Date)]


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF2_clean.csv')
DT.protocols02 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols02)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


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
DT.p02.model <- merge(DT.protocols02, tmp.1, 
                      by = c('purchaseNumber', 'fcsProtocolEF2.id'))
DT.p02.model <- merge(DT.p02.model, tmp.2, 
                      by = c('purchaseNumber', 'fcsProtocolEF2.id'))

# преобразование дат
DT.protocols02[, protocol02Date := uf.convert.char.to.date(protocol02Date)]
DT.protocols02[, application.firstOffer.date := 
                   uf.convert.char.to.date(application.firstOffer.date)]
DT.protocols02[, application.lastOffer.date := 
                   uf.convert.char.to.date(application.lastOffer.date)]

# считаем длительность аукциона по разнице времени ценовых предложений
DT.protocols02[, max.lastOffer.date := max(application.lastOffer.date), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[, min.firstOffer.date := max(application.firstOffer.date), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[, time.stage02.hours := max(as.numeric(difftime(max.lastOffer.date,
                                                               min.firstOffer.date, 'hours')),
                                           round(5/60/60, 3)), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[, time.stage02.hours := unclass(time.stage02.hours)]
summary(DT.protocols02$time.stage02.hours)

# добавляем рассчитанные столбцы к исходной таблице
DT.p02.model <- merge(DT.p02.model, unique(select(DT.protocols02, 
                                                  purchaseNumber,
                                                  fcsProtocolEF2.id,
                                                  time.stage02.hours)), 
                      by = c('purchaseNumber', 'fcsProtocolEF2.id'))

# * оставляем только столбцы для модели: максимальное количество предложений
#    цен на аукционе; число участников аукциона
DT.p02.model <- select(DT.p02.model, purchaseNumber, fcsProtocolEF2.id,
                       max.price.offers.quantity, application.count.p02,
                       time.stage02.hours, protocol02Date)
DT.p02.model <- unique(DT.p02.model)

DT.p02.model[, .N, by = purchaseNumber][order(-N), ]
nrow(filter(DT.p02.model[, .N, by = purchaseNumber], N > 1))
DT.p02.model[purchaseNumber == '0101200009518003293', ]
grep('0101200009518003293', all.xmls, value = T)


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF3_clean.csv')
DT.protocols03 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols03)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# предварительный анализ данных ................................................
select(filter(DT.protocols03, purchaseNumber == '0101000000118000052'), 
       application.appRating, application.journalNumber)
filter(DT.protocols01, application.journalNumber == 110)
filter(DT.protocols02, application.journalNumber == 110)
filter(DT.protocols03, application.journalNumber == 110)

grep('0101100000918000299', all.xmls, value = T)
grep('0101000000118000049', all.xmls, value = T)

filter(DT.protocols01, purchaseNumber == '0101100000918000299')
filter(DT.protocols02, purchaseNumber == '0101100000918000299')
filter(DT.protocols03, purchaseNumber == '0101100000918000299')
# ..............................................................................


# определяем цену победителя аукциона
tmp.1 <- select(DT.protocols01, purchaseNumber, 
                application.journalNumber,
                protocol01Date)
tmp.2 <- select(DT.protocols02, purchaseNumber, application.journalNumber,
                protocol02Date,
                application.lastOffer.price)
tmp.3 <- select(DT.protocols03, purchaseNumber, application.journalNumber,
                protocol03Date,
                application.appRating)

# # совмещаем протоколы в последовательности "объявление" - "этап 1" по датам
# tmp.1[purchaseNumber == '0301100046118000160', ]

# сколько извещений с одинаковыми номерами
dt.dbl.notice <- filter(unique(select(DT.responsibleOrgs, purchaseNumber, 
    docPublishDate_notice))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1)
nrow(dt.dbl.notice)
length(dt.dbl.notice$purchaseNumber)
# сколько протоколов 1 этапа с одинаковыми номерами
dt.dbl.p01 <- filter(unique(select(DT.protocols01, purchaseNumber, 
            protocol01Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1)
nrow(dt.dbl.p01)
length(dt.dbl.p01$purchaseNumber)
# сколько протоколов 2 этапа с одинаковыми номерами
dt.dbl.p02 <- filter(unique(select(DT.protocols02, purchaseNumber, 
            protocol02Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1)
nrow(dt.dbl.p02)
length(dt.dbl.p02$purchaseNumber)
# сколько протоколов 3 этапа с одинаковыми номерами
dt.dbl.p03 <- filter(unique(select(DT.protocols03, purchaseNumber, 
            protocol03Date))[, .N, by = 'purchaseNumber'][order(-N), ], N > 1)
nrow(dt.dbl.p03)
length(dt.dbl.p03$purchaseNumber)
# по протоколам выпадает вот такая доля извещений
pID.doubles <- unique(c(dt.dbl.notice$purchaseNumber, dt.dbl.p01$purchaseNumber, 
                        dt.dbl.p02$purchaseNumber, dt.dbl.p03$purchaseNumber))
length(pID.doubles) / length(unique(DT.responsibleOrgs$purchaseNumber))



# убираем их из таблиц данных во имя Луны
tmp.1 <- tmp.1[!(purchaseNumber %in% pID.doubles), ]
tmp.2 <- tmp.2[!(purchaseNumber %in% pID.doubles), ]
tmp.3 <- tmp.3[!(purchaseNumber %in% pID.doubles), ]
tmp.1[, protocol01Date := NULL]
tmp.2[, protocol02Date := NULL]
tmp.3[, protocol03Date := NULL]



tmp <- select(filter(merge(merge(tmp.1, tmp.2, 
                        by = c('purchaseNumber', 'application.journalNumber')),
                        tmp.3, 
                        by = c('purchaseNumber', 'application.journalNumber')),
                     application.appRating == 1), purchaseNumber, 
              application.lastOffer.price)
tmp <- data.table(unique(tmp))
# добавляем цену победителя к данным по протоколам
DT.p03.model <- merge(DT.protocols03, tmp, by = 'purchaseNumber')
colnames(DT.p03.model)[colnames(DT.p03.model) == 'application.lastOffer.price'] <- 
    'winner.price'
# оставляем только столбцы для модели
DT.p03.model <- select(DT.p03.model, fcsProtocolEF3.id,
                       purchaseNumber, winner.price, protocol03Date)
DT.p03.model <- unique(DT.p03.model)

# объединяем данные по протоколам этапов аукциона
DT.protocols <- merge(DT.p01.model, DT.p02.model, 'purchaseNumber')
DT.protocols <- merge(DT.protocols, DT.p03.model, by = 'purchaseNumber')
DT.protocols <- unique(DT.protocols)
dim(DT.protocols)
DT.protocols[, fcsProtocolEF1.id := NULL]
DT.protocols[, fcsProtocolEF2.id := NULL]
DT.protocols[, fcsProtocolEF3.id := NULL]
DT.protocols <- unique(DT.protocols)
dim(DT.protocols)

# предварительный анализ данных ................................................
length(unique(DT.protocols$purchaseNumber))

length(unique(DT.proc.index$purchaseNum))

tmp <- unique(DT.proc.index[prefix == 'fcsNotificationEA44', ]$purchaseNum)
tmp <- tmp[tmp %in% DT.proc.index[prefix == 'fcsProtocolEF1', ]$purchaseNum]
tmp <- tmp[tmp %in% DT.proc.index[prefix == 'fcsProtocolEF2', ]$purchaseNum]
tmp <- tmp[tmp %in% DT.proc.index[prefix == 'fcsProtocolEF3', ]$purchaseNum]
length(tmp)

msg <- paste0('Всего ', round(length(tmp) / 
                           length(unique(DT.proc.index$purchaseNum)) * 100, 1),
              '% (', length(tmp), 
              ') объявлений относятся к электронным аукционам')
message(msg)

tmp.1 <- unique(DT.proc.index$purchaseNum)[!(unique(DT.proc.index$purchaseNum) 
                                             %in% tmp)]
length(tmp.1)

DT.proc.index[purchaseNum == tmp.1[1], ]
DT.proc.index[purchaseNum == tmp.1[2], ]

grep(tmp.1[1], all.xmls, value = T)
# ..............................................................................


# набор данных для модели: добавление столбцов /////////////////////////////////
# убираем ID протоколов
DT.model <- merge(DT.model, DT.protocols, by = 'purchaseNumber')
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na

# считаем переменные на основе дат
DT.model[, total.time.days := as.numeric(difftime(protocol03Date, docPublishDate_notice,
                                                  units = 'days'))]
DT.model[, time.stage.01.days := as.numeric(difftime(protocol01Date, docPublishDate_notice,
                                                     units = 'days'))]
DT.model[, time.stage.02.days := as.numeric(difftime(protocol02Date, protocol01Date,
                                                     units = 'days'))]
DT.model[, time.stage.03.hours := as.numeric(difftime(protocol03Date, protocol02Date,
                                                      units = 'hours'))]
str(DT.model)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# записываем таблицу с данными для модели >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRawCSVPath, 'DT_model_', lProcedureToScrap$procedureCode, '.csv')
uf.write.table.with.metadata(DT.model, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# предварительный анализ данных ................................................
length(DT.protocols$purchaseNumber)
length(unique(DT.protocols$purchaseNumber))

length(unique(DT.p03.model$purchaseNumber))
length(unique(DT.p02.model$purchaseNumber))
length(unique(DT.p01.model$purchaseNumber))
grep('fcsProtocolEF', unique(DT.proc.index$prefix), value = T)

tmp.1 <- unique(filter(DT.proc.index, prefix == 'fcsProtocolEF1')$purchaseNum)
length(tmp.1)
length(tmp.1[!(tmp.1 %in% DT.p01.model$purchaseNumber)])

grep('0101200002319000312', all.xmls, value = T)

tmp.2 <- unique(filter(DT.proc.index, prefix == 'fcsProtocolEF2')$purchaseNum)
length(tmp.2)
tmp.3 <- unique(filter(DT.proc.index, prefix == 'fcsProtocolEF3')$purchaseNum)
length(tmp.3)
tmp <- tmp.1[tmp.1 %in% tmp.2]
length(tmp[tmp %in% tmp.3])
# ..............................................................................



# Построение модели ------------------------------------------------------------


# читаем таблицу с данными для модели <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sRawCSVPath, 'DT_model_', lProcedureToScrap$procedureCode, '.csv')
DT.model <- uf.read.table.with.metadata(flnm)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


colnames(DT.model)

summary(select(DT.model, total.time.days, time.stage.01.days, 
               time.stage.02.days, time.stage.03.hours))
sapply(select(DT.model, total.time.days, time.stage.01.days, 
              time.stage.02.days, time.stage.03.hours), function(x) {
                  sum(x < 0)
              })
DT.model[time.stage.02.days < 0, ]
dim(DT.model)

# начальные цены (кол-во, цены и дисперсия) по кодам товаров
table(DT.model$purchaseObject.OKPD2.code.2dig)
tmp.1 <- select(DT.model, purchaseObject.OKPD2.code.2dig, lot.maxPrice)[, 
                                   .N, by = 'purchaseObject.OKPD2.code.2dig']
tmp.2 <- select(DT.model, purchaseObject.OKPD2.code.2dig, lot.maxPrice)[, 
                    lapply(.SD, mean), by = 'purchaseObject.OKPD2.code.2dig']
tmp.3 <- select(DT.model, purchaseObject.OKPD2.code.2dig, lot.maxPrice)[, 
                    lapply(.SD, sd), by = 'purchaseObject.OKPD2.code.2dig']
tbl <- merge(merge(tmp.1, tmp.2), tmp.3)
colnames(tbl)[colnames(tbl) == 'lot.maxPrice.x'] <- 'mean.lot.maxPrice'
colnames(tbl)[colnames(tbl) == 'lot.maxPrice.y'] <- 'sd.lot.maxPrice'
tbl[order(-mean.lot.maxPrice), ]

# отрезаем первые 4 ОКПД, у них аномально высокие цены закупки
plot(tbl$mean.lot.maxPrice)
okpd.drop <- tbl[order(-mean.lot.maxPrice), ]$purchaseObject.OKPD2.code.2dig[1:4]

DT.model <- filter(DT.model, !(purchaseObject.OKPD2.code.2dig %in% okpd.drop))

# экономия
DT.model$y <- DT.model$lot.maxPrice - DT.model$winner.price
summary(DT.model$y)
sum(DT.model$y < 0) / length(DT.model$y)
boxplot(DT.model$y, horizontal = T)

# убираем наблюдения с отрицательной экономией
DT.model <- filter(DT.model, y > 0)
summary(DT.model$y)

# делаем фактор из ограничения на СМП
DT.model <- as.data.table(DT.model)
DT.model[, SMP.only := 
             as.factor(ifelse(restriction.shortName == 'MB44330', 1, 0))]
# вообще убираем закупки с другими ограничениями
DT.model <- filter(DT.model, restriction.shortName %in% c('MB44330', 'no'))


dt <- select(DT.model, y, SMP.only, application.count.p01,
             max.price.offers.quantity, application.count.p02,
             time.stage.01.days, time.stage02.hours, total.time.days)
dt <- unique(dt)
dim(dt)

ggpairs(dt, ggplot2::aes(color = SMP.only))  
dt.log <- as.data.table(dt)
dt.log[, y := log(y)]
ggpairs(dt.log, ggplot2::aes(color = SMP.only))  

# SMP.only1 незначим
fit.1 <- lm(y ~ ., data = dt)
summary(fit.1)
fit.1 <- lm(y ~ SMP.only + max.price.offers.quantity +
                application.count.p02 + time.stage.01.days + 
                total.time.days, 
            data = dt)
summary(fit.1)

# SMP.only1 незначим
fit.2 <- lm(y ~ ., data = dt.log)
summary(fit.2)


dt.okpd.21 <- select(filter(DT.model, purchaseObject.OKPD2.code.2dig == '21'), 
                     y, SMP.only, application.count.p01,
                     max.price.offers.quantity, application.count.p02,
                     time.stage.01.days, time.stage02.hours, total.time.days)
dt.okpd.21 <- unique(dt.okpd.21)
dim(dt.okpd.21)

# SMP.only1 незначим
fit.3 <- lm(y ~ ., data = dt.okpd.21)
summary(fit.3)
fit.3 <- lm(y ~ SMP.only + application.count.p01 + max.price.offers.quantity +
                application.count.p02 + time.stage.01.days +
                total.time.days,
            data = dt.okpd.21)
summary(fit.3)
fit.3 <- lm(y ~ SMP.only + application.count.p01 + max.price.offers.quantity +
                application.count.p02 + time.stage.01.days,
            data = dt.okpd.21)
summary(fit.3)
    

dt.okpd.32 <- select(filter(DT.model, purchaseObject.OKPD2.code.2dig == '32'), 
                     y, SMP.only, application.count.p01,
                     max.price.offers.quantity, application.count.p02,
                     time.stage.01.days, time.stage02.hours,  
                     total.time.days)
dt.okpd.32 <- unique(dt.okpd.32)
dim(dt.okpd.32)

# значима на уровне 0.10, R-квадрат 0,630
fit.4 <- lm(y ~ ., data = dt.okpd.32)
summary(fit.4)
fit.4 <- lm(y ~ SMP.only + application.count.p01 + max.price.offers.quantity +
                time.stage.01.days, 
            data = dt.okpd.32)
summary(fit.4)
