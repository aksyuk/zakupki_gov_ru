# ..............................................................................
# parser-ftp-04_Prepare-Data-for-Models.R
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
#  * purchaseNumber                 character   номер заявки 
#
#  * docPublishDate_notice	        POSIXct     дата публикации заявки
#
#  * responsibleRole	            character   роль ответственной организации
#
#  * lot.maxPrice	                numeric     начальная максимальная цена 
#                                               закупки, рублей
#
#  * restriction.shortName	        character   кодовое название ограничения 
#                                               процедуры закупки
#
#  * purchaseObject.OKPD2.code.2dig	character   первые 2 цифры кода товара 
#                                               закупки по ОКПД2
#
#  * application.count.p01	        integer     количество заявок, поданных на 
#                                               первый этап электронного 
#                                               аукциона
#
#  * protocol01Date	                POSIXct     дата протокола первого этапа 
#                                               аукциона
#
#  * max.price.offers.quantity	    numeric     максимальное количество ценовых
#                                               предложений от одного участника
#                                               аукциона 
#
#  * application.count.p02	        integer     количество заявок, допущенных ко 
#                                               второму этапу аукциона
#
#  * time.stage.02.hours	        numeric     продолжительность второго этапа
#                                               аукциона, в часах
#
#  * protocol02Date	                POSIXct     дата протокола первого этапа 
#                                               аукциона
#
#  * winner.price	                numeric     цена победителя аукциона, рублей
#
#  * protocol03Date	                POSIXct     дата протокола третьего этапа 
#                                               аукциона
#
#  * total.time.days	            numeric     продолжительность процедуры от 
#                                               даты публикации заявки до 
#                                               публикации протокола третьего 
#                                               этапа аукциона, дней
#
#  * time.stage.01.days	            numeric     продолжительность первого 
#                                               этапа аукциона, дней
#
#  * time.stage.02.days	            numeric     продолжительность второго 
#                                               этапа аукциона, дней
#
#  * region             	        character   регион
#
#  * period                         character   период 
#
# ..............................................................................

log.errors.flnm = paste0(sDataSamplePath, 'log_make_model_table_err.log')



# ПОДСЧЁТ ЦЕПОЧЕК ФАЙЛОВ ПО ИНДЕКСУ --------------------------------------------

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

# читаем индекс типов файлов из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sRawCSVPath, 'DT_all_xml_files_index.csv')
DT.flnms.index <- uf.read.table.with.metadata(flnm)
dim(DT.flnms.index)
head(DT.flnms.index)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# # уникальные последовательности файлов в виде цепочек 0 и 1
# index.file.names.binary <- unique(apply(DT.flnms.index[, -1], 1, function(x) {
#     x[x > 0] <- 1
#     paste0(x, collapse = '')
# }))


# Таблица со сводкой по цепочкам успешных аукционов ============================

# строки таблицы с подсчётом наличия файлов в цепочках
notices.to.check <- c('fksNotificationEA44', 'fcsNotificationEA44',
                      'fksNotificationEA615', 
                      'fksNotificationINM111', 'fksNotificationOK504',
                      'fksNotificationOKU504', 'fksNotificationPO615', 
                      'fksNotificationZK504', 'fksNotificationZP504',
                      'contract', 'contractProcedure')

# столбцы таблицы с подсчётом наличия файлов в цепочках
stage.to.find <- c('fcsPlacementResult', 'epNotificationCancel', 
                   'fcsProtocolEF1', 'fcsProtocolEF2', 'fcsProtocolEF3', 
                   'fcsProtocolP615', 'fcsProtocolEvasion',
                   'fcsProtocolEFSingleApp', 'fcsProtocolEFSinglePart',
                   'contractProcedureCancel', 'epNotificationCancel',
                   'fcsNotificationCancel', 'fcsProtocolCancel',
                   'fcsProtocolEOKOUSingleApp', 'fcsProtocolEOKOUSinglePart',
                   'fcsProtocolOKSingleApp')

# считаем файлы
df.check <- as.data.frame(lapply(stage.to.find, function(stage){
    
    if (length(grep(stage, colnames(DT.flnms.index))) > 0) {
        df.tmp.01 <- DT.flnms.index[as.data.frame(DT.flnms.index[, stage, with = F])[, 1] == 1, ]
    
        stage.check <- unlist(lapply(notices.to.check, function(notice) {
            
            if (length(grep(notice, colnames(DT.flnms.index))) > 0) {
                df.tmp.02 <- df.tmp.01[as.data.frame(df.tmp.01[, notice, with = F])[, 1] == 1, ]
                nrow(df.tmp.02)
            } else {
                0
            }
        }))
    } else {
        rep(0, length(notices.to.check))
    }
}))

# дополняем таблицу-сводку заголовками и метаданными
colnames(df.check) <- stage.to.find
df.check <- cbind(notice.type = notices.to.check, 
                  Регион = my.region$name,
                  Период = paste0(s.YEAR.MON[1], '-', s.YEAR.MON[length(s.YEAR.MON)]),
                  df.check)
rownames(df.check) <- 1:nrow(df.check)

# записываем сводку по цепочкам файлов >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
out.file.name <- paste0(sDataSamplePath, 'df_file_chains_check.csv')
uf.write.table.with.metadata(df.check, out.file.name)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# Таблица с подсчётом извещений и результатов ==================================

proc.cat <- c('Запрос котировок в электронной форме',
              'Запрос предложений в электронной форме',
              'Предварительный отбор квалифицированных подрядных организаций',
              'Конкурс с ограниченным участием в электронной форме',
              'Открытый конкурс в электронной форме',
              'Конкурс или запрос котировок с учетом положений ст.111 44-ФЗ',
              'Электронный аукцион на оказание услуг или выполнение работ по капитальному ремонту общего имущества в многоквартирном доме',
              'Электронный аукцион')
proc.flnms.01 <- c('fksNotificationZK504', 'fksNotificationZP504',
                   'fksNotificationPO615', 'fksNotificationOKU504',
                   'fksNotificationOK504', 'fksNotificationINM111',
                   'fksNotificationEA615', 'fksNotificationEA44')
proc.flnms.02 <- c('fcsNotificationZK504', 'fcsNotificationZP504',
                   'fcsNotificationPO615', 'fcsNotificationOKU504',
                   'fcsNotificationOK504', 'fcsNotificationINM111',
                   'fcsNotificationEA615', 'fcsNotificationEA44')
dt.summary <- data.table(Регион = my.region$name, 
                         Период = paste0(s.YEAR.MON[1], '-', s.YEAR.MON[length(s.YEAR.MON)]),
                         Категория = proc.cat, 
                         Файлы = paste0(proc.flnms.01, ' | ', proc.flnms.02),
                         Число.записей = rep(0, length(proc.cat)),
                         Число.уникальных.записей = rep(0, length(proc.cat)))

count.01 <- sapply(proc.flnms.01, function(flnm) {
    if (length(grep(flnm, colnames(DT.flnms.index))) > 0) {
        dt <- DT.flnms.index[, colnames(DT.flnms.index) %in% c('noticeID', flnm), 
                             with = F]
        c(sum(dt[, 2] > 0), sum(unique(dt)[, 2] > 0))
    } else {
        c(0, 0)
    }
})

count.02 <- sapply(proc.flnms.02, function(flnm) {
    if (length(grep(flnm, colnames(DT.flnms.index))) > 0) {
        dt <- DT.flnms.index[, colnames(DT.flnms.index) %in% c('noticeID', flnm), 
                             with = F]
        c(sum(dt[, 2] > 0), sum(unique(dt)[, 2] > 0))
    } else {
        c(0, 0)
    }
})

dt.tmp <- t(count.01) + t(count.02)

dt.summary[, 5] <- dt.tmp[, 1]
dt.summary[, 6] <- dt.tmp[, 2]

# записываем сводку по цепочкам файлов >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
out.file.name <- paste0(sDataSamplePath, 'df_notices_count.csv')
uf.write.table.with.metadata(dt.summary, out.file.name)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



# Загрузка и подготовка данных -------------------------------------------------


# ЗАКАЗЧИК И МАКСИМАЛЬНАЯ ЦЕНА =================================================


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sOutPath, 'DT_responsibleOrgs_clean.csv')
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
# DT.responsibleOrgs[, responsibleRole := NULL]
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
flnm <- paste0(sOutPath, 'DT_restrictions_clean.csv')
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
DT.model <- merge(DT.responsibleOrgs, DT.restrictions, 
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
flnm <- paste0(sOutPath, 'DT_TPY_codes_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolEF1_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolEF2_clean.csv')
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
               min.firstOffer.date := min(application.firstOffer.date), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[!is.na(max.lastOffer.date) & !is.na(min.firstOffer.date), 
               time.stage.02.hours := max(as.numeric(difftime(max.lastOffer.date,
                             min.firstOffer.date, 'hours')), round(5/60/60, 3)), 
               by = c('purchaseNumber', 'fcsProtocolEF2.id')]
DT.protocols02[, time.stage.02.hours := unclass(time.stage.02.hours)]
summary(DT.protocols02$time.stage.02.hours)

# * оставляем только столбцы для модели: максимальное количество предложений
#    цен на аукционе; число участников аукциона
DT.protocols02 <- select(DT.protocols02, purchaseNumber, fcsProtocolEF2.id,
                         application.journalNumber, application.lastOffer.price,
                         max.price.offers.quantity, application.count.p02,
                         time.stage.02.hours, protocol02Date)
DT.protocols02 <- unique(DT.protocols02)

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sOutPath, 'DT_fcsProtocolEF3_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolEFSingleApp_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolEFSinglePart_clean.csv')
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
# DT.protocolsSinglePart[, application.organizationName := NULL]
# DT.protocolsSinglePart[, publisherOrg.regNum := NULL]
# DT.protocolsSinglePart[, publisherOrg.fullName := NULL]
# DT.protocolsSinglePart[, fcsProtocolEFSinglePart.id := NULL]
# DT.protocolsSinglePart[, externalId := NULL]
# DT.protocolsSinglePart[, protocolNumber := NULL]
# DT.protocolsSinglePart[, foundationProtocolNumber := NULL]


# РЕЗУЛЬТАТЫ РАЗМЕЩЕНИЯ ========================================================

# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sOutPath, 'DT_fcsPlacementResult_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolCancel_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsNotificationCancel_clean.csv')
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
flnm <- paste0(sOutPath, 'DT_fcsProtocolEFInvalidation.csv')
DT.ProtocolInval <- read.csv2(flnm, stringsAsFactors = F, 
                              colClasses = rep('character', 5))
summary(DT.ProtocolInval)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# # КОНТРАКТ =====================================================================
# 
# # читаем таблицу из напрямую, без очистки <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# flnm <- paste0(sOutPath, 'DT_fcsContractSign.csv')
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
DT.protocols01[purchaseNumber == '0101100000220000006', ]
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
                         time.stage.02.hours, protocol02Date)
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

if (length(grep('abandonedReason.code.x', colnames(DT.model)))) {
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
}

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


# набор данных для модели: добавление столбцов /////////////////////////////////
# добавляем регион
DT.model[, region := lst_REGION$name]

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
DT.model[, time.stage.03.days := as.numeric(difftime(protocol03Date, 
                                                      protocol02Date, 
                                                      units = 'days'))]
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# набор данных для модели: оставляем только закупки с протоколами трёх стадий //
sel.pID <- unique(DT.protocols$purchaseNumber)
DT.model <- filter(DT.model, purchaseNumber %in% sel.pID)
DT.model <- select(DT.model, purchaseNumber, docPublishDate_notice, 
                   responsibleRole, lot.maxPrice, restriction.shortName, 
                   purchaseObject.OKPD2.code.2dig, application.count.p01, 
                   protocol01Date, max.price.offers.quantity, 
                   application.count.p02, time.stage.02.hours,
                   protocol02Date, winner.price, protocol03Date, 
                   total.time.days, time.stage.01.days, time.stage.03.days,
                   region, period)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# набор данных для модели: убираем пропуски ////////////////////////////////////
nas <- uf.count.nas.in.table(DT.model)
nas$columns.na
DT.model <- na.omit(DT.model)
dim(DT.model)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# записываем таблицу с данными для модели >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRawCSVPath, 'DT_model_', lProcedureToScrap$procedureCode, 
               '_', my.region$name, '.csv')
uf.write.table.with.metadata(DT.model, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
