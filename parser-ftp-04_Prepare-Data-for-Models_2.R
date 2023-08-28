# ..............................................................................
# parser-ftp-04_Prepare-Data-for-Models_2.R
# 
# Построение моделей на данных по 44ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.4.2 (28 Aug 2023)
#
# ******************************************************************************
# Объединение таблиц и предобработка данных.
# На основе таблиц с разобранными XML по электронным аукционам 
#  делаем таблицу с переменными:
#
# purchaseNumber                    character   номер заявки 
#
# lot.maxPrice	                    numeric     начальная максимальная цена 
#                                               закупки, рублей
#
# last.notice.date	                POSIXct     дата публикации заявки (если 
#                                               было несколько заходов, берём 
#                                               последнюю дату)
#
# SMP.only                          numeric     флаг: 1 закупка у СМП, 0 иначе  
#
# Nat.Regime                        numeric     флаг: 1 если есть ограничение 
#                                               на нацрежим, 0 если нет
#
# purchaseObject.OKPD2.code.2dig    character   двузначный код ОКПД2 товара 
#                                               закупки
#
# application.count.p01             integer     количество поданных на первом 
#                                               этапе заявок
#
# application.reject.count.p01      integer     количество отклонённых на первом 
#                                               этапе заявок    
#
# max.price.offers.quantity         numeric     количество предложений цены 
#                                               от участников аукциона
#
# time.stage.01.days                numeric     длительность первого этапа 
#                                               процедуры, дней
#
# application.count.p02             integer     количество заявок на втором 
#                                               этапе аукциона
#
# time.stage02.hours                numeric     длительность второго этапа 
#                                               (предложения цен), в часах
#
# winner.price                      numeric     цена победителя аукциона, рубли    
#
# auc.type                          character   флажок на отслеживание прогресса 
#                                               процедуры
#
# application.countryFullName       character   страна организации, подавшей 
#                                               заявку, согласно протоколу
#
# single.app.proc.success           logical     флаг на аукционы с единственной 
#                                               заявкой, завершившиеся успешно
#
# single.part.proc.success          logical     флаг на аукционы с единственным 
#                                               участником, завершившиеся 
#                                               успешно   
#
# winner.evades                     character   'yes' если победитель аукциона 
#                                               уклонился от заключения 
#                                               контракта, 'no' если нет    
#
# single.app.rejected.ANR           numeric     единственная заявка отклонена 
#                                               по причине несоответствия 
#                                               требованиям извещения 
#                                               или документации
#
# single.app.rejected.DNFV          numeric     единственная заявка отклонена 
#                                               по причине неполного объема 
#                                               документов или недостоверной 
#                                               информации в заявке    
#
# single.app.rejected.PNRD          numeric     единственная заявка отклонена 
#                                               по причине несоответствия 
#                                               участника требованиям извещения 
#                                               или документации
#
# rejected.ANR                      numeric     заявки отклонены по причине 
#                                               несоответствия требованиям 
#                                               извещения или документации
#
# procedurelFailed                  logical     флажок на процедуры, 
#                                               не завершившиеся заключением 
#                                               контракта (F).   
#
# ..............................................................................

log.errors.flnm = paste0(sDataSamplePath, 'log_make_model_table_err.log')

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
DT.responsibleOrgs[, responsibleRole := NULL]
colnames(DT.responsibleOrgs)[colnames(DT.responsibleOrgs) == 
                            'docPublishDate'] <- 'docPublishDate_notice'
DT.responsibleOrgs[, docPublishDate_notice := 
                       uf.convert.char.to.date(docPublishDate_notice)]


# боремся с дублированием извещений с одинаковыми номерами #####################

# вычисляем дату самой последней публикации для каждого извещения
#  если начальная цена одна и та же, это одна и та же закупка
DT.responsibleOrgs[, last.notice.date := max(docPublishDate_notice), 
                   by = list(purchaseNumber, lot.maxPrice)]
# дата самой первой публикации извещения
DT.responsibleOrgs[, frst.notice.date := min(docPublishDate_notice), 
                   by = list(purchaseNumber, lot.maxPrice)]
# заменяем столбец с последними датами на дату публикации заявки
# DT.responsibleOrgs[, docPublishDate_notice := NULL]
# оставляем только уникальные
DT.responsibleOrgs <- unique(DT.responsibleOrgs)


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

# # виды ограничений
# table(DT.restrictions$restriction.name)
# table(DT.restrictions$restriction.shortName)

# # * убираем дату
# DT.restrictions[, docPublishDate := NULL]
# * убираем название ограничения
DT.restrictions[, restriction.name := NULL]

# # есть ли закупки с двумя ограничениями сразу?
# dt.tst <- DT.restrictions[, length(unique(.SD$restriction.shortName)), 
#                           by = purchaseNumber]
# dt.tst[V1 > 1, ][, length(unique(.SD$purchaseNumber)), by = V1]
# dt.tst[V1 == 3, ]
# 
# DT.restrictions[purchaseNumber == '0101300031320000205', ]
# 
# # закупка с несколькими нотисами
# DT.restrictions[purchaseNumber == '0101300031320000205', ]
# paste0(sRawXMLPath, grep('0101300031320000205', all.xmls, value = T))

# механизм расклеивания таблицы с ограничениями таков, что если у закупки 
#  два ограничения появляется две строки в таблице
#  делаем дамми по связке номер закупки + номер нотиса
dt.restr.tmp <- unique(select(DT.restrictions, -restriction.shortName))
# DT.restrictions[, SMP.only := ifelse(restriction.shortName == 'MB44330', 1, 0)]
# DT.restrictions[, Nat.Regime := ifelse(restriction.shortName == 'JB2149', 1, 0)]
dt.restr.tmp <- merge(dt.restr.tmp, filter(select(DT.restrictions, 
                                                  purchaseNumber, 
                                                  fcsNotificationEF.id, 
                                                  restriction.shortName),
                                           restriction.shortName == 'MB44330'), 
                      by = c('purchaseNumber', 'fcsNotificationEF.id'), 
                      all.x = T)
dt.restr.tmp[, SMP.only := ifelse(restriction.shortName == 'MB44330', 1, 0)]
dt.restr.tmp[is.na(SMP.only), SMP.only := 0]
dt.restr.tmp[, restriction.shortName := NULL]
dt.restr.tmp <- merge(dt.restr.tmp, filter(select(DT.restrictions, 
                                                  purchaseNumber, 
                                                  fcsNotificationEF.id, 
                                                  restriction.shortName),
                                           restriction.shortName == 'JB2149'), 
                      by = c('purchaseNumber', 'fcsNotificationEF.id'), 
                      all.x = T)
dt.restr.tmp[, Nat.Regime := ifelse(restriction.shortName == 'JB2149', 1, 0)]
dt.restr.tmp[is.na(Nat.Regime), Nat.Regime := 0]
dt.restr.tmp[, restriction.shortName := NULL]


# набор данных для модели: создание ////////////////////////////////////////////
cat(blue('Создаём набор данных из таблиц организаторов закупки и ограничений\n'))
DT.model <- merge(DT.responsibleOrgs, dt.restr.tmp, 
                  by = c('purchaseNumber', 'fcsNotificationEF.id'))
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# ID извещений, по которым несколько строк с одинаковыми ценами
#  из них оставляем самые свежие строки
dt.check.doubles <- DT.model %>% group_by(purchaseNumber) %>% 
    summarise(N = n(), maxPrice.var = var(lot.maxPrice),
              last.notice.date = last.notice.date) %>% 
    filter(N > 1, maxPrice.var == 0) %>% arrange(-N)
nrow(dt.check.doubles)
dt.check.doubles


# набор данных для модели: удаление строк //////////////////////////////////////
# оставляем последнее по дате извещение
DT.model <- rbind(filter(filter(DT.model, purchaseNumber %in% dt.check.doubles$purchaseNumber),
                         docPublishDate_notice == last.notice.date),
                  filter(DT.model, !(purchaseNumber %in% dt.check.doubles$purchaseNumber)))
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# # смотрим ID извещений, по которым несколько строк с разными ценами
# dt.check.doubles <- DT.model %>% group_by(purchaseNumber) %>% 
#     summarise(N = n(), maxPrice.var = var(lot.maxPrice)) %>% 
#     filter(N > 1, maxPrice.var > 0) %>% arrange(-N)
# nrow(dt.check.doubles)
# dt.check.doubles
# 
# DT.model[purchaseNumber == dt.check.doubles$purchaseNumber[1], ]
# DT.model[purchaseNumber == dt.check.doubles$purchaseNumber[2], ]
# DT.model[purchaseNumber == dt.check.doubles$purchaseNumber[3], ]


# набор данных для модели: удаление строк //////////////////////////////////////
# убираем их все, т.к. пока хз что с ними делать
DT.model <- DT.model[!(purchaseNumber %in% dt.check.doubles$purchaseNumber), ]
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# проверка: 1 строка = 1 извещение
cat(yellow(paste0('ПРОВЕРКА DT.model: 1 строка = 1 извещение: ', 
                  length(unique(DT.model$purchaseNumber)) == nrow(DT.model),
                  '\n')))


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

dt.count.rows <- DT.TPY.codes %>% select(purchaseNumber, 
                                         purchaseObject.OKPD2.code.2dig) %>%
    unique() %>% group_by(purchaseNumber) %>% summarise(N = n())
dt.count.rows <- data.table(filter(dt.count.rows, N > 1))
dt.count.rows[order(-N), ]
n.msg <- nrow(dt.count.rows)
cat(yellow(paste0('закупок с более чем одним товарным кодом: ', 
                  round(n.msg / length(unique(DT.model$purchaseNumber)) * 100, 1),
                  '% (', n.msg, ')\n')))
ids.to.drop <- unique(dt.count.rows$purchaseNumber)

length(unique(dt.count.rows$purchaseNumber))
length(unique(DT.model$purchaseNumber))


# набор данных для модели: удаление строк //////////////////////////////////////
# убираем закупки с несколькими кодами
DT.model <- DT.model[!(DT.model$purchaseNumber %in% ids.to.drop), ]
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# набор данных для модели: добавление столбцов /////////////////////////////////
cat(blue('Добавляем сведения о товарах закупки\n'))
DT.model <- merge(DT.model, DT.TPY.codes, 
                  by = c('purchaseNumber', 'fcsNotificationEF.id'))
# дальше работаем с таблицами по протоколам, 
#  fcsNotificationEF.id больше не нужен (он с самого начала был перестраховкой)
DT.model <- unique(DT.model[, fcsNotificationEF.id := NULL])
dim(DT.model)
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# проверка: 1 строка = 1 извещение
cat(yellow(paste0('ПРОВЕРКА DT.model: 1 строка = 1 извещение: ', 
                  length(unique(DT.model$purchaseNumber)) == nrow(DT.model),
                  '\n')))


# УЧАСТНИКИ АУКЦИОНОВ ==========================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF1_clean.csv')
DT.protocols01 <- uf.read.table.with.metadata(flnm)
summary(DT.protocols01)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocols01[, protocol01Date := uf.convert.char.to.date(protocol01Date)]

# если несколько протоколов на одну закупку, оставляем последний
DT.protocols01[, max.p01.date := max(protocol01Date), by = 'purchaseNumber']
DT.protocols01[, difftime.days := difftime(max.p01.date, protocol01Date, 
                                           units = 'days')]
DT.protocols01 <- DT.protocols01[protocol01Date == max.p01.date, ]
DT.protocols01[, difftime.days := NULL]

# считаем количество допущенных участников
if ('admitted' %in% colnames(DT.protocols01)) {
    dt.adm.count <- select(DT.protocols01[admitted == T, ], purchaseNumber, 
                           application.journalNumber)[, .N, by = 'purchaseNumber']    
} else {
    dt.adm.count <- select(DT.protocols01[application.admitted.explanation == 'true', ], purchaseNumber, 
                           application.journalNumber)[, .N, by = 'purchaseNumber']
}
colnames(dt.adm.count)[colnames(dt.adm.count) == 'N'] <- 
    'application.count.p01'
# считаем количество не допущенных участников
if ('admitted' %in% colnames(DT.protocols01)) {
    dt.non.adm.count <- select(DT.protocols01[admitted == F, ], purchaseNumber, 
                               application.journalNumber)[, .N, by = 'purchaseNumber']
} else {
    dt.non.adm.count <- select(DT.protocols01[application.admitted.explanation != 'true', ], purchaseNumber, 
                               application.journalNumber)[, .N, by = 'purchaseNumber']
}

colnames(dt.non.adm.count)[colnames(dt.non.adm.count) == 'N'] <- 
    'application.reject.count.p01'

DT.protocols01 <- merge(DT.protocols01, dt.adm.count, by = 'purchaseNumber')
DT.protocols01 <- merge(DT.protocols01, dt.non.adm.count, by = 'purchaseNumber',
                        all.x = T)
DT.protocols01[is.na(application.reject.count.p01), 
               application.reject.count.p01 := 0]

# * оставляем только столбцы для модели: количество принятых заявок + дата
DT.protocols01 <- select(DT.protocols01, purchaseNumber, fcsProtocolEF1.id,
                         application.journalNumber, application.count.p01, 
                         application.reject.count.p01, protocol01Date,
                         abandonedReason.code, abandonedReason.name,
                         application.reject.count.p01)


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

# если несколько протоколов на одну закупку, оставляем последний
DT.protocols02[, max.p02.date := max(protocol02Date), by = 'purchaseNumber']
DT.protocols02[, difftime.days := difftime(max.p02.date, protocol02Date, 
                                           units = 'days')]
summary(as.numeric(DT.protocols02$difftime.days))
DT.protocols02 <- DT.protocols02[protocol02Date == max.p02.date, ]
DT.protocols02[, difftime.days := NULL]

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

# если несколько протоколов на одну закупку, оставляем последний
DT.protocols03[, max.p03.date := max(protocol03Date), by = 'purchaseNumber']
DT.protocols03[, difftime.days := difftime(max.p03.date, protocol03Date, 
                                           units = 'days')]
summary(as.numeric(DT.protocols03$difftime.days))
DT.protocols03 <- DT.protocols03[protocol03Date == max.p03.date, ]
DT.protocols03[, difftime.days := NULL]


# АУКЦИОНЫ С ОДНОЙ ЗАЯВКОЙ =====================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEFSingleApp_clean.csv')
DT.protocolsSingleApp <- uf.read.table.with.metadata(flnm)
summary(DT.protocolsSingleApp)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocolsSingleApp[, protocolSingleAppDate := 
                          uf.convert.char.to.date(protocolSingleAppDate)]

# если несколько протоколов на одну закупку, оставляем последний
DT.protocolsSingleApp[, max.single.app.date := max(protocolSingleAppDate), 
                      by = 'purchaseNumber']
DT.protocolsSingleApp[, difftime.days := difftime(max.single.app.date, 
                                                  protocolSingleAppDate, 
                                                  units = 'days')]
summary(as.numeric(DT.protocolsSingleApp$difftime.days))
DT.protocolsSingleApp <- 
    DT.protocolsSingleApp[protocolSingleAppDate == max.single.app.date, ]

DT.protocolsSingleApp[, difftime.days := NULL]

# убираем ИНН 
DT.protocolsSingleApp[, application.inn := NULL]


# АУКЦИОНЫ С ОДНИМ УЧАСТНИКОМ ==================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEFSinglePart_clean.csv')
DT.protocolsSinglePart <- uf.read.table.with.metadata(flnm)
summary(DT.protocolsSinglePart)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocolsSinglePart[, protocolSinglePartDate := 
                          uf.convert.char.to.date(protocolSinglePartDate)]

# если несколько протоколов на одну закупку, оставляем последний
DT.protocolsSinglePart[, max.single.part.date := max(protocolSinglePartDate),
                       by = 'purchaseNumber']
DT.protocolsSinglePart[, difftime.days := difftime(max.single.part.date, 
                                                   protocolSinglePartDate, 
                                                   units = 'days')]
summary(as.numeric(DT.protocolsSinglePart$difftime.days))
DT.protocolsSinglePart <- 
    DT.protocolsSinglePart[protocolSinglePartDate == max.single.part.date, ]
DT.protocolsSinglePart[, difftime.days := NULL]

# убираем ИНН, КПП и прочее
DT.protocolsSinglePart[, application.inn := NULL]
DT.protocolsSinglePart[, application.kpp := NULL]
с[, application.organizationName := NULL]
DT.protocolsSinglePart[, publisherOrg.regNum := NULL]
DT.protocolsSinglePart[, publisherOrg.fullName := NULL]
DT.protocolsSinglePart[, fcsProtocolEFSinglePart.id := NULL]
DT.protocolsSinglePart[, externalId := NULL]
DT.protocolsSinglePart[, protocolNumber := NULL]
DT.protocolsSinglePart[, foundationProtocolNumber := NULL]


# РЕЗУЛЬТАТЫ РАЗМЕЩЕНИЯ ========================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsPlacementResult_clean.csv')
DT.PlacementResult <- uf.read.table.with.metadata(flnm)
summary(DT.PlacementResult)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# типы столбцов
DT.PlacementResult$procedurelFailed <- 
    as.logical(DT.PlacementResult$procedurelFailed)
DT.PlacementResult$application.journalNumber <- 
    as.numeric(DT.PlacementResult$application.journalNumber)
DT.PlacementResult$application.appRating <- 
    as.numeric(DT.PlacementResult$application.appRating)
DT.PlacementResult$application.price <- 
    as.numeric(DT.PlacementResult$application.price)


# ОТМЕНА ПРОТОКОЛА =============================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolCancel_clean.csv')
DT.protocolCancel <- uf.read.table.with.metadata(flnm)
summary(DT.protocolCancel)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.protocolCancel$fcsProtocolCancel.docDate <- 
    uf.convert.char.to.date(DT.protocolCancel$fcsProtocolCancel.docDate)
DT.protocolCancel$cancelReason.docDate <- 
    uf.convert.char.to.date(DT.protocolCancel$cancelReason.docDate)

# если несколько протоколов на одну закупку, оставляем последний
DT.protocolCancel[, max.proc.cancel.date := max(fcsProtocolCancel.docDate),
                  by = 'purchaseNumber']
DT.protocolCancel[, difftime.days := difftime(max.proc.cancel.date, 
                                              fcsProtocolCancel.docDate,
                                              units = 'days')]
summary(as.numeric(DT.protocolCancel$difftime.days))
DT.protocolCancel <- 
    DT.protocolCancel[fcsProtocolCancel.docDate == max.proc.cancel.date, ]
DT.protocolCancel[, difftime.days := NULL]


# ОТМЕНА ИЗВЕЩЕНИЯ =============================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsNotificationCancel_clean.csv')
DT.notificationCancel <- uf.read.table.with.metadata(flnm)
summary(DT.notificationCancel)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# преобразование дат
DT.notificationCancel$fcsNotificationCancel.docDate <- 
    uf.convert.char.to.date(DT.notificationCancel$fcsNotificationCancel.docDate)

# если несколько протоколов на одну закупку, оставляем последний
DT.notificationCancel[, max.notice.cancel.date := max(fcsNotificationCancel.docDate),
                      by = 'purchaseNumber']
DT.notificationCancel[, difftime.days := difftime(max.notice.cancel.date, 
                                                  fcsNotificationCancel.docDate,
                                                  units = 'days')]
summary(as.numeric(DT.notificationCancel$difftime.days))
DT.notificationCancel <- 
    DT.notificationCancel[fcsNotificationCancel.docDate == max.notice.cancel.date, ]
DT.notificationCancel[, difftime.days := NULL]


# ПРИЗНАНИЕ ПРОТОКОЛА НЕДЕЙСТВИТЕЛЬНЫМ =========================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEFInvalidation.csv')
DT.ProtocolInval <- read.csv2(flnm, stringsAsFactors = F, 
                              colClasses = rep('character', 5))
summary(DT.ProtocolInval)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# # КОНТРАКТ =====================================================================
# 
# # читаем таблицу из напрямую, без очистки <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# flnm <- paste0(out.path, 'DT_fcsContractSign.csv')
# if (file.exists(flnm)) {
#     DT.ContractSign <- read.csv2(flnm, stringsAsFactors = F, 
#                                  colClasses = rep('character', 5))
#     summary(DT.ContractSign)
# } else {
#     cat(red(paste0('Файл ', flnm, ' не найден')))
#     uf.write.to.log(paste0('Ошибка при выполнении скрипта ', 
#                            'parser-ftp-04_Prepare-Data-for-Models_2.R : файл ', 
#                            flnm, ' не найден.\n'), 
#                     log.errors.flnm, T)
# }
# # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# 
# # проверка на наличие склеенных значений
# grep('#', DT.ContractSign$supplier.inn)
# 
# # числовые столбцы
# DT.ContractSign$priceRUR <- as.numeric(DT.ContractSign$priceRUR)
# 
# # имена столбцов
# colnames(DT.ContractSign)[colnames(DT.ContractSign) == 'foundation.order.purchaseNumber'] <- 'purchaseNumber'


# определяем цену победителя аукциона ##########################################
# обнаружены чудесные извещения, по которым нет протокола 1 стадии, но есть
#  2 и 3, и заказчик определён
nrow(DT.flnms.index[fcsProtocolEF1 == 0 & fcsProtocolEF2 == 1 & 
                        fcsProtocolEF3 == 1 & fcsPlacementResult == 1, ])
DT.protocols[purchaseNumber == '0101100000220000006', ]
# поэтому химичим
tmp.1 <- select(DT.protocols01, purchaseNumber, application.journalNumber,
                protocol01Date)
tmp.2 <- select(DT.protocols02, purchaseNumber, application.journalNumber,
                protocol02Date, application.lastOffer.price)
tmp.3 <- select(DT.protocols03, purchaseNumber, application.journalNumber,
                protocol03Date, application.appRating)

tmp <- select(filter(merge(merge(tmp.1, tmp.2, 
                        by = c('purchaseNumber', 'application.journalNumber'),
                        all.y = T),
                        tmp.3, 
                        by = c('purchaseNumber', 'application.journalNumber')),
                     application.appRating == 1), purchaseNumber, 
              application.lastOffer.price)
tmp <- data.table(unique(tmp))
# tmp[purchaseNumber == '0101100000220000006', ]

# ищем дубликаты ID извещений
dt.check.doubles <- tmp %>% group_by(purchaseNumber) %>% 
    summarise(N = n()) %>% filter(N > 1) %>% arrange(-N)
nrow(dt.check.doubles)
dt.check.doubles

# tmp[purchaseNumber == '0301100035120000036', ]
# tmp.1[purchaseNumber == '0301100035120000036', ]
# tmp.2[purchaseNumber == '0301100035120000036', ]
# tmp.3[purchaseNumber == '0301100035120000036', ]

# так и не придумала как отличить друг от друга 2 одинаковых по purchaseNumber
#  и дате протокола с разными победителями, поэтому выкидываем
tmp <- tmp[!(purchaseNumber %in% dt.check.doubles$purchaseNumber), ]

# добавляем цену победителя к данным по протоколам
DT.protocols03 <- merge(DT.protocols03, tmp, by = 'purchaseNumber')
colnames(DT.protocols03)[colnames(DT.protocols03) == 
                             'application.lastOffer.price'] <- 'winner.price'

# протоколы 1: оставляем только столбцы для модели
DT.protocols01 <- select(DT.protocols01, purchaseNumber, application.count.p01, 
                         protocol01Date, application.reject.count.p01)
DT.protocols01 <- unique(DT.protocols01)
DT.protocols01[, .N, by = 'purchaseNumber'][N > 1, ]

# протоколы 2: оставляем только столбцы для модели
DT.protocols02 <- select(DT.protocols02, purchaseNumber, 
                         max.price.offers.quantity, application.count.p02,
                         time.stage02.hours, protocol02Date)
DT.protocols02 <- unique(DT.protocols02)
DT.protocols02[, .N, by = 'purchaseNumber'][N > 1, ]

# протоколы 3: оставляем только столбцы для модели
DT.protocols03 <- select(DT.protocols03, purchaseNumber, winner.price, 
                         protocol03Date)
DT.protocols03 <- unique(DT.protocols03)
DT.protocols03[, .N, by = 'purchaseNumber'][N > 1, ]

# объединяем данные по протоколам этапов аукциона
DT.protocols <- merge(DT.protocols01, DT.protocols02, by = 'purchaseNumber',
                      all.y = T)
DT.protocols <- merge(DT.protocols, DT.protocols03, by = 'purchaseNumber')
DT.protocols <- unique(DT.protocols)
dim(DT.protocols)

cat(yellow(paste0('ПРОВЕРКА DT.protocols: 1 строка = 1 извещение: ', 
                  length(unique(DT.protocols$purchaseNumber)) == nrow(DT.protocols),
                  '\n')))


# набор данных для модели: добавление столбцов /////////////////////////////////
# добавляем протоколы
DT.model <- merge(DT.model, DT.protocols, by = 'purchaseNumber',
                  all.x = T)
DT.model[, .N, by = 'purchaseNumber'][N > 1, ]

# добавляем флажок с отслеживанием прогресса процедуры 'auc.type'
# все 3 стадии завершены
DT.model$auc.type <- rep('', nrow(DT.model))
DT.model[is.na(winner.price), auc.type := '3 stages fail']

# окончен после стадии 2 (не состоялся)
sel.pID <- unique(DT.protocols02$purchaseNumber)
sel.pID <- sel.pID[!(sel.pID %in% unique(DT.protocols03$purchaseNumber))]
DT.model[purchaseNumber %in% sel.pID, auc.type := '2 stages fail']

# окончен после стадии 1 (не состоялся)
sel.pID <- unique(DT.protocols01$purchaseNumber)
sel.pID <- sel.pID[!(sel.pID %in% unique(DT.protocols02$purchaseNumber))]
DT.model[purchaseNumber %in% sel.pID, auc.type := '1 stage fail']

# добавляем процедуры с единственной заявкой
DT.model <- merge(DT.model, DT.protocolsSingleApp, by = 'purchaseNumber',
                  all.x = T)
sel.pID <- unique(DT.protocolsSingleApp$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID & is.na(winner.price), 
         winner.price := as.numeric(winnerPrice)]
DT.model[, winnerPrice := NULL]
colnames(DT.model)[grepl('rejected', colnames(DT.model))] <- 
    paste0('single.app.', colnames(DT.model)[grepl('rejected', colnames(DT.model))])
colnames(DT.model)[colnames(DT.model) == 'proc.success'] <- 
    'single.app.proc.success'

# проставляем статус: аукцион с единственной заявкой
sel.pID <- unique(DT.protocolsSingleApp$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, auc.type := 'single app']

# добавляем аукционы с единственным участником
DT.model <- merge(DT.model, unique(DT.protocolsSinglePart), 
                  by = 'purchaseNumber', all.x = T)
sel.pID <- unique(DT.protocolsSinglePart$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID & is.na(winner.price), 
         winner.price := winnerPrice]
DT.model[, winnerPrice := NULL]
colnames(DT.model)[colnames(DT.model) == 'proc.success'] <- 
    'single.part.proc.success'
DT.model[is.na(application.countryFullName.x) & !is.na(application.countryFullName.y), 
         application.countryFullName.x := application.countryFullName.y]
DT.model[, application.countryFullName.y := NULL]
colnames(DT.model)[colnames(DT.model) == 'application.countryFullName.x'] <- 
    'application.countryFullName'

# проставляем статус: аукцион с единственным участником
sel.pID <- unique(DT.protocolsSinglePart$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, auc.type := 'single part']

# добавляем placementResults
DT.model <- merge(DT.model, DT.PlacementResult, by = 'purchaseNumber',
                  all.x = T)
DT.model[is.na(abandonedReason.code.x) & !is.na(abandonedReason.code.y), 
         abandonedReason.code.x := abandonedReason.code.y]
DT.model[, abandonedReason.code.y := NULL]
colnames(DT.model)[colnames(DT.model) == 'abandonedReason.code.x'] <- 
    'abandonedReason.code'
DT.model[is.na(abandonedReason.name.x) & !is.na(abandonedReason.name.y), 
         abandonedReason.name.x := abandonedReason.name.y]
DT.model[, abandonedReason.name.y := NULL]
colnames(DT.model)[colnames(DT.model) == 'abandonedReason.name.x'] <- 
    'abandonedReason.name'

# проставляем статус: победитель уклонился от заключения договора
DT.model$winner.evades <- rep('', nrow(DT.model))
DT.model[application.result == 'RC', winner.evades := 'yes']
DT.model[application.result != 'RC', winner.evades := 'no']
DT.model[winner.evades == '', winner.evades := NA]

# правим цену победителя по placement results
DT.model[!is.na(winner.price) & application.result == 'CC', 
         winner.price := application.price]
# удаляем столбцы, которые больше не понадобятся
DT.model[, application.journalNumber := NULL]
DT.model[, application.appRating := NULL]
DT.model[, application.result := NULL]
DT.model[, application.price := NULL]
DT.model <- unique(DT.model)

# столбец с результатами определения поставщика (пересекается с auc.type,
#  поэтому показываем отдельно)
DT.model[, placement.result := '']
sel.pID <- unique(DT.PlacementResult[procedurelFailed == F, ]$purchaseNumber)
DT.model[(purchaseNumber %in% sel.pID), placement.result := 'success']
sel.pID <- unique(DT.PlacementResult[procedurelFailed == T, ]$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, placement.result := 'failed']

# столбцы с флажками на отмены (пересекается с auc.type,
#  поэтому показываем отдельно)
DT.model[, stage.cancel := '']
sel.pID <- unique(DT.protocolCancel$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, stage.cancel := 'protocol']
sel.pID <- unique(DT.notificationCancel$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, stage.cancel := 'notification']
sel.pID <- unique(DT.ProtocolInval$purchaseNumber)
DT.model[purchaseNumber %in% sel.pID, stage.cancel := 'protocol invalidation']

DT.model[(auc.type == '') & (placement.result == 'success'), 
         auc.type := 'success']

table(DT.model$auc.type)
table(DT.model$placement.result)
table(DT.model$stage.cancel)

table(DT.model$auc.type, DT.model$placement.result)
table(DT.model$auc.type, DT.model$stage.cancel)

cat(yellow(paste0('Изменяем статус ', 
                  nrow(DT.model[placement.result == 'success' & auc.type != 'success', ]),
                       ' аукционов на "success" \n')))

DT.model[placement.result == 'success' & auc.type != 'success', 
         auc.type := 'success']

# все остальные тоже считаем отменами
DT.model[auc.type == '', auc.type := 'cancel']
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# считаем переменные на основе дат
DT.model[, total.time.days := as.numeric(difftime(protocol03Date, 
                                                  last.notice.date,
                                                  units = 'days'))]
DT.model[, time.stage.01.days := as.numeric(difftime(protocol01Date, 
                                                     last.notice.date,
                                                     units = 'days'))]

# сколько объявлений с расхождением между первой и последней датой 
#  размещения более, чем сутки
DT.model[, difftime.days := as.numeric(difftime(last.notice.date, frst.notice.date, units = 'days'))]
DT.model[, difftime.days.int := cut(difftime.days, 
                                    breaks = c(min(difftime.days) - 0.01, 7, 14, 21, 30, 60, 90, max(difftime.days)))]
DT.model %>% group_by(difftime.days.int) %>% summarise(N = n())

ID.count <- DT.model %>% group_by(purchaseNumber) %>% summarise(N = n(),
                                                                mean.difftime.days = mean(difftime.days))
filter(ID.count[order(-ID.count$N), ], N > 1)

n.msg <- nrow(DT.model[difftime.days > 14, ])
summary(DT.model$difftime.days)

nrow(DT.model[difftime.days == max(difftime.days), ])
unique(DT.model[difftime.days == max(difftime.days), ]$purchaseNumber)

unique(select(DT.model[difftime.days < max(difftime.days), ][order(difftime.days), ],
       purchaseNumber))
# от ID с максимальной разницей до минимальной, только уникальные
unique(select(DT.model[order(-difftime.days), ], purchaseNumber))
DT.model[purchaseNumber == '0301100012720000026', ]

cat(yellow(paste0('извещений с расхождением между первой и последней датой ',
                  ' размещения более, чем две недели: ', 
                  round(n.msg / nrow(DT.model) * 100, 1), '% (', n.msg, ')\n')))
# выкидываем их
DT.model <- DT.model[as.numeric(difftime(DT.model$last.notice.date, DT.model$frst.notice.date, units = 'days')) <= 14, ]
DT.model[, frst.notice.date := NULL]

colnames(DT.model)

# оставляем только столбцы для сравнения средних
col.select <- grep('rejected', colnames(DT.model), value = T)
DT.model <- select(DT.model, 
                   purchaseNumber, lot.maxPrice, last.notice.date,
                   SMP.only, Nat.Regime, 
                   purchaseObject.OKPD2.code.2dig,
                   application.count.p01, application.reject.count.p01,
                   max.price.offers.quantity, 
                   time.stage.01.days, application.count.p02, 
                   time.stage02.hours, winner.price, auc.type, 
                   application.countryFullName, 
                   single.app.proc.success, single.part.proc.success,
                   winner.evades, all_of(col.select), procedurelFailed)

# записываем таблицу с данными для модели >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRawCSVPath, 'DT_model_2_', lProcedureToScrap$procedureCode, 
               '_', my.region$name, '.csv')
uf.write.table.with.metadata(DT.model, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
