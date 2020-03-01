# ..............................................................................
# parser-ftp-04_Parse-Loaded_XML-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  http://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.3 (28.02.2020)
# 
# Эта часть кода парсит xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ
# ..............................................................................


# папка с csv-файлами по текущему типу процедур
out.path <- paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/')


# 3. РАЗБОР XML-ФАЙЛОВ ---------------------------------------------------------


# Делаем индекс файлов с аукционами, включая номера документов =================

DT.proc.index <- all.xmls
DT.proc.index <- gsub('[.]xml', '', DT.proc.index)
DT.proc.index <- data.table(do.call(rbind, strsplit(gsub('fcs_', 'fcs', 
                                                         DT.proc.index), '_')))
colnames(DT.proc.index) <- c('prefix', 'purchaseNum', 'id')
# противный префикс fcs_notificationEFDateChange портит всю обедню,
#  но переименовывать исходники плохая идея
DT.proc.index$prefix[DT.proc.index$prefix == 'fcsnotificationEFDateChange'] <- 
    'fcs_notificationEFDateChange'
head(DT.proc.index)

# проверка количества файлов
tmp <- cbind(DT.proc.index[, -2][, .N, by = prefix][order(-N), ], 
             sort(sapply(DT.xml.files.index[, -1], sum), decreasing = T))
sapply(tmp[, -1], sum)

# оставялем только id для электронных аукционов
DT.proc.index <- DT.proc.index[purchaseNum %in% loop.ids, ]
dim(DT.proc.index)


# записываем таблицу-индекс с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRawCSVPath, 'DT_index_melt_', lProcedureToScrap$procedureCode, 
               '.csv')
uf.write.table.with.metadata(DT.proc.index, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем индекс из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sRawCSVPath, 'DT_index_melt_', lProcedureToScrap$procedureCode, 
               '.csv')
DT.proc.index <- uf.read.table.with.metadata(flnm)
dim(DT.proc.index)
str(DT.proc.index)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# всего xml для обработки
n <- nrow(DT.proc.index)
message(paste0('Всего xml для обработки: ', n,  ' (', 
               round(n / length(all.xmls) * 100, 1),
               '% от общего числа файлов).'))


# Парсим файлы с электронными аукционами =======================================

# шаблоны для разбора хранятся в отдельном csv
df.patterns <- read.csv2(paste0(sRefPath, 'df_xml_patterns.csv'),
                         stringsAsFactors = F)
# список префиксов файлов из индекса, для которых нет шаблона
unique(DT.proc.index$prefix)[!(unique(DT.proc.index$prefix) %in% names(df.patterns))]

# преобразуем фрейм с шаблонами в список
patterns <- as.list(df.patterns)
patterns <- c(list(ns = c('xmlns:', 'xmlns:', 'oos:', 'xmlns:',
                          'xmlns:', 'xmlns:')), patterns)
patterns <- lapply(patterns, function(x){x[x != ""]})

# делаем из шаблонов имена столбцов итоговой таблицы
patterns[-1] <- lapply(patterns[-1], function(x) {
    x <- x[x != '']
    tmp <- gsub(x, pattern = 'xmlns[:]|oos[:]', replacement = '')
    tmp <- gsub(tmp, pattern = '[//]|[/]', replacement = '.')
    tmp <- gsub(tmp, pattern = '^[.]*', replacement = '')
    tmp <- gsub(tmp, pattern = '[.]+', replacement = '.')
    tmp <- gsub(tmp, pattern = '^.*:', replacement = '')
    tmp <- gsub(tmp, pattern = '/.*:', replacement = '/')
    tmp <- gsub(tmp, pattern = '[.]$', replacement = '')
    tmp <- gsub(tmp, pattern = '[][]', replacement = '')
    tmp <- gsub(tmp, pattern = '\\)|\\(', replacement = '')
    tmp <- gsub(tmp, pattern = '([^\\|]*)\\|.*', replacement = '\\1')

    x <- data.frame(xml.pattern = x, stringsAsFactors = F)
    x$xpath.names <- as.character(tmp)

    x <- x
})

# лог
log.parse.XML.filename <- paste0(sRawArchPath, 'log_xml_parse.txt')
if (!file.exists(log.parse.XML.filename)) file.create(log.parse.XML.filename)

# # для настройки частичного парсинга ............................................
# loop.prefs <- 'fcsNotificationEA44'
# patterns[['fcsNotificationEA44']] <- 
#     patterns[['fcsNotificationEA44']][c(1, 2, 28, 29), ]
# # ..............................................................................

# цикл по типам файлов
# loop.prefs <- unique(DT.proc.index$prefix)
filename.to.save.data <- paste0(out.path, 'DT_', pref, '.csv')
for (pref in loop.prefs) {

    # отладка
    # pref <- loop.prefs[6]

    # выборка с файлами текущего префикса
    DT <- filter(DT.proc.index, prefix == pref)
    
    # проверяем, есть ли шаблоны тегов для этого префикса
    if (is.null(patterns[[pref]])) {
        message(paste0('Не найдены теги для префикса файла: ', pref,
                       '\nПропускаю ', nrow(DT), ' файлов.\n'))
        rm(DT)
        next
    }
    
    # количество столбцов в разобранном файле
    n.cols <- nrow(patterns[[pref]])

    msg <- paste0('Начинаю разбор файлов ', pref, ': ', Sys.time())
    uf.write.to.log(msg, log.parse.XML.filename)

    # это все имена файлов заданного типа
    flnms <- paste0(sRawXMLPath,
                    apply(DT, 1, function(x) {paste(x, collapse = '_')}), 
                    '.xml')
    
    # парсим файлы с текущим префиксом
    res <- uf.parse.xmls.with.prefix(flnms, patterns[[pref]], 10000, 
                                     filename.to.save.data,
                                     log.parse.XML.filename)
    if (is.null(res)) {
        break
    }
}



# 4. ЧИСТИМ RAW-ТАБЛИЦЫ --------------------------------------------------------


#  * Работаем с таблицей объявлений ============================================
# грузим результаты парсинга XML (не нормализованную таблицу)
DT.notif <- data.table(read.csv2(paste0(out.path, 'DT_fcsNotificationEA44.csv'), 
                                 stringsAsFactors = F,
                                na.strings = c('NA', '<NA>'),
                                # encoding = 'UTF-8',
                                colClasses = 'character'))
# проверка
dim(DT.notif)
str(DT.notif)
colnames(DT.notif)


# # проверка .....................................................................
# #  список 'id заявки -- id организации-заказчика
# DT.notif.with.orgs <- unique(DT.notif[, c('fcsNotificationEF.purchaseNumber',
#                                           'responsibleOrg.regNum'), with = F])
# # произведение д.б. == 1
# prod(substr(DT.notif.with.orgs$fcsNotificationEF.purchaseNumber, 1, 11) ==
#          DT.notif.with.orgs$responsibleOrg.regNum)
# # ..............................................................................

# переименовываем первый столбец
colnames(DT.notif)[colnames(DT.notif) == 'fcsNotificationEF.purchaseNumber'] <- 
    'purchaseNumber'
# ставим столбец с номером заявки на первое место
DT.notif <- select(DT.notif, purchaseNumber, everything())

# с этой таблицей в лоб не справиться, много склеянных ячеек
#  поэтому вытаскиваем куски по частям


# ..............................................................................
#
# ЗАКАЗЧИК И МАКСИМАЛЬНАЯ ЦЕНА 
# ..............................................................................

columns.to.search <- c(c('purchaseNumber', 'fcsNotificationEF.id', 
                         'docPublishDate', 'responsibleOrg.regNum',
                         'responsibleOrg.fullName', 'responsibleOrg.postAddress',
                         'responsibleOrg.INN', 'responsibleOrg.KPP',
                         'responsibleRole', 'lot.maxPrice', 'lot.currency.code'))
all.responsibleOrgs <- unique(data.table(DT.notif[, columns.to.search, 
                                                  with = F]))
dim(all.responsibleOrgs)

# здесь ничего расклеивать обычно не нужно
#  если эта сумма == 0, то столбцов, в которых есть #, в таблице нет
sum(grepl(unlist(apply(all.responsibleOrgs, 2, function(x){
    x <- paste0(unlist(x), collapse = '@')
})), pattern = '#'))

# преобразовываем максимальную цену в число
all.responsibleOrgs[, lot.maxPrice := as.numeric(lot.maxPrice)]
# число пропусков
sum(is.na(all.responsibleOrgs$lot.maxPrice))


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_responsibleOrgs_clean.csv')
uf.write.table.with.metadata(all.responsibleOrgs, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# удаляем таблицы из рабочего пространства
rm(all.responsibleOrgs)


# ..............................................................................
#
# ОГРАНИЧЕНИЯ НА ЗАКУПКУ
# ..............................................................................

columns.to.search <- c(c('purchaseNumber', 'fcsNotificationEF.id', 
                         'docPublishDate', 'restriction.shortName',
                         'restriction.name'))
all.restrictions <- unique(data.table(DT.notif[, columns.to.search, with = F]))
dim(all.restrictions)

# расклеиваим ячейки (файл записывается в csv автоматом)
DT.restrictions <- uf.process.large.table.normalising(all.restrictions,
                                                      'DT_restrictions_clean.csv', 
                                                      out.path)


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_restrictions_clean.csv')
uf.write.table.with.metadata(DT.restrictions, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# удаляем таблицы из рабочего пространства
rm(DT.restrictions)


# ..............................................................................
#
# ТОВАРЫ
# ..............................................................................

# ПЕРВЫЙ КЛАССИФИКАТОР ТОВАРОВ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
columns.to.search <- c('purchaseNumber', 'fcsNotificationEF.id',
                       'purchaseObject.OKPD2.code', 'purchaseObject.OKPD2.name')
DT.all.OKPD2 <- unique(data.table(DT.notif[, columns.to.search, with = F]))
dim(DT.all.OKPD2)

DT.all.OKPD2 <- uf.process.large.table.normalising(DT.all.OKPD2,
                                                'DT_all_OKPD2_clean.csv',
                                                out.path)

# эта таблица вся из символьных столбцов, и всё что надо сделать -- это удалить
#  дублирующиеся строки
DT.all.OKPD2 <- unique(data.table(DT.all.OKPD2))
DT.all.OKPD2[, purchaseObject.OKPD2.code.2dig :=
                 gsub('[.].*$', '', DT.all.OKPD2$purchaseObject.OKPD2.code)]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.all.OKPD2)[colnames(DT.all.OKPD2) == 'fcsNotificationEF.purchaseNumber'] <-
    'purchaseNumber'

# ВТОРОЙ КЛАССИФИКАТОР ТОВАРОВ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
columns.to.search <- c('purchaseNumber', 'fcsNotificationEF.id',
                       'purchaseObject.KTRU.code', 'purchaseObject.KTRU.name')
DT.all.KTRU <- unique(data.table(DT.notif[, columns.to.search, with = F]))
dim(DT.all.KTRU)

DT.all.KTRU <- uf.process.large.table.normalising(DT.all.KTRU,
                                                  'DT_all_KTRU_clean.csv',
                                                  out.path)

# эта таблица вся из символьных столбцов, и всё что надо сделать -- это удалить
#  дублирующиеся строки
DT.all.KTRU <- unique(data.table(DT.all.KTRU))
DT.all.KTRU[, purchaseObject.KTRU.code.2dig :=
                gsub('[.].*$', '', DT.all.KTRU$purchaseObject.KTRU.code)]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.all.KTRU)[colnames(DT.all.KTRU) == 'fcsNotificationEF.purchaseNumber'] <-
    'purchaseNumber'

# ТРЕТИЙ КЛАССИФИКАТОР ТОВАРОВ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
columns.to.search <- c('purchaseNumber', 'fcsNotificationEF.id',
                       'drugPurchaseObjectInfo.MNNName')
DT.all.MNNName <- unique(data.table(DT.notif[, columns.to.search, with = F]))
dim(DT.all.MNNName)

DT.all.MNNName <- uf.process.large.table.normalising(DT.all.MNNName,
                                                     'DT_all_MNNName_clean.csv',
                                                     out.path)

# эта таблица вся из символьных столбцов, и всё что надо сделать -- это удалить
#  дублирующиеся строки
DT.all.MNNName <- unique(data.table(DT.all.MNNName))

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.all.MNNName)[colnames(DT.all.MNNName) == 'fcsNotificationEF.purchaseNumber'] <-
    'purchaseNumber'

# Объединяем классификаторы товаров \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

dim(DT.all.KTRU)
dim(DT.all.OKPD2)
dim(DT.all.MNNName)

DT <- merge(DT.all.KTRU, DT.all.OKPD2, all = T, by = c('purchaseNumber',
                                                       'fcsNotificationEF.id'))
DT <- merge(DT, DT.all.MNNName, all = T, by = c('purchaseNumber',
                                                'fcsNotificationEF.id'))
dim(DT)
colnames(DT)
all.pid <- unique(DT$purchaseNumber)
all.pid[!(all.pid %in% c(unique(DT.all.KTRU$purchaseNumber), 
                         unique(DT.all.OKPD2$purchaseNumber),
                         unique(DT.all.MNNName$purchaseNumber)))]
filter(DT, purchaseNumber == all.pid[1])
pid.n <- select(DT, purchaseNumber, fcsNotificationEF.id)[, .N, 
                                            by = purchaseNumber][order(-N),]
pid.n
filter(DT, purchaseNumber == pid.n[2, ]$purchaseNumber)


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_TPY_codes_clean.csv')
uf.write.table.with.metadata(DT, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# удаляем таблицы из рабочего пространства
rm(DT, DT.all.KTRU, DT.all.OKPD2, DT.all.MNNName)


# ..............................................................................
#
# УЧАСТНИКИ АУКЦИОНОВ
# ..............................................................................

#  * Работаем с таблицей протоколов этапа 1 ====================================
flnm <- paste0(out.path, 'DT_fcsProtocolEF1.csv')
protocols.EF1 <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                      colClasses = 'character'))
colnames(protocols.EF1)[colnames(protocols.EF1) == 'fcsProtocolEF1.purchaseNumber'] <-
    'purchaseNumber'
protocols.EF1

# проверка .....................................................................
#  строк (уникальных id) в файле должно быть столько же...
nrow(protocols.EF1)
#  ...сколько уникальных id протоколов 01
nrow(filter(DT.proc.index, prefix == 'fcsProtocolEF1'))
# ..............................................................................

# ставим столбец с номером заявки на первое место
protocols.EF1 <- select(protocols.EF1, purchaseNumber, everything())
# расклеиваим ячейки (файл записывается в csv автоматом)
DT.protocols01 <- uf.process.large.table.normalising(protocols.EF1,
                                                     'DT_fcsProtocolEF1_clean.csv',
                                                     out.path)

# логический столбец admitted содержит для непринятых заявок причины
# перекидываем причины в новый столбец, делаем admitted логическим
DT.protocols01[is.na(as.logical(admitted)), admitted.F.reason := admitted]
DT.protocols01[is.na(as.logical(admitted)), admitted := F]
DT.protocols01[, admitted := as.logical(admitted)]
table(DT.protocols01$admitted)
# число пропусков
sum(is.na(DT.protocols01$admitted))

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols01[is.na(as.numeric(application.journalNumber)), 
               application.journalNumber.comment := application.journalNumber]
DT.protocols01[, application.journalNumber := as.numeric(application.journalNumber)]
table(DT.protocols01$application.journalNumber)
# число пропусков
sum(is.na(DT.protocols01$application.journalNumber))

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols01)[colnames(DT.protocols01) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols01)[colnames(DT.protocols01) == 'protocolDate'] <-
    'protocol01Date'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEF1_clean.csv')
uf.write.table.with.metadata(DT.protocols01, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# проверка .....................................................................
length(unique(DT.protocols01$purchaseNumber))
length(na.omit(unique(DT.protocols01$purchaseNumber)))
nrow(filter(DT.proc.index, prefix == 'fcsProtocolEF1'))

# во всех нижеследующих заявках процедура аукциона обрывается из-за различных
#  ошибок: не подано ни одной заявки, например
all.ids.in.index <- 
    unique(filter(DT.proc.index, prefix == 'fcsProtocolEF1')$purchaseNum)
missed.ids.01 <- all.ids.in.index[!(all.ids.in.index %in%
                                        na.omit(unique(DT.protocols01$purchaseNumber)))]
length(missed.ids.01)


#  * Работаем с таблицей протоколов этапа 2 ====================================
flnm <- paste0(out.path, 'DT_fcsProtocolEF2.csv')
protocols.EF2 <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                      colClasses = 'character'))
colnames(protocols.EF2)[colnames(protocols.EF2) == 'application.journalNumber.priceOffers'] <-
    'application.journalNumber'
protocols.EF2

# ставим столбец с номером заявки на первое место
protocols.EF2 <- select(protocols.EF2, purchaseNumber, everything())
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols02 <- uf.process.large.table.normalising(protocols.EF2,
                                                     'DT_fcsProtocolEF2_clean.csv',
                                                     out.path)

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols02[is.na(as.numeric(application.journalNumber)), 
               application.journalNumber.comment := application.journalNumber]
DT.protocols02[, application.journalNumber := as.numeric(application.journalNumber)]
table(DT.protocols02$application.journalNumber)
# число пропусков
sum(is.na(DT.protocols02$application.journalNumber))

# первая цена
DT.protocols02[, application.firstOffer.price := as.numeric(application.firstOffer.price)]
# число пропусков
sum(is.na(DT.protocols02$application.firstOffer.price))

# последняя цена
DT.protocols02[, application.lastOffer.price := as.numeric(application.lastOffer.price)]
# число пропусков
sum(is.na(DT.protocols02$application.lastOffer.price))

# количество предложений цен
DT.protocols02[, application.priceOffers.offersQuantity := as.numeric(application.priceOffers.offersQuantity)]
# число пропусков
sum(is.na(DT.protocols02$application.priceOffers.offersQuantity))

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols02)[colnames(DT.protocols02) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols02)[colnames(DT.protocols02) == 'protocolDate'] <-
    'protocol02Date'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEF2_clean.csv')
uf.write.table.with.metadata(DT.protocols02, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# проверка .....................................................................
length(unique(DT.protocols02$purchaseNumber))
length(na.omit(unique(DT.protocols02$purchaseNumber)))
nrow(filter(DT.proc.index, prefix == 'fcsProtocolEF2'))

# во всех нижеследующих заявках процедура аукциона обрывается из-за различных
#  ошибок: не подано ни одной заявки, например
all.ids.in.index <- 
    filter(DT.proc.index, prefix == 'fcsProtocolEF2')$purchaseNum
missed.ids.02 <- all.ids.in.index[!(all.ids.in.index %in%
                                        unique(DT.protocols02$purchaseNumber))]
length(missed.ids.02)


#  * Работаем с таблицей протоколов этапа 3 ====================================
flnm <- paste0(out.path, 'DT_fcsProtocolEF3.csv')
protocols.EF3 <- data.table(read.csv2(flnm, stringsAsFactors = F, 
                                      colClasses = 'character'))
protocols.EF3

# ставим столбец с номером заявки на первое место
protocols.EF3 <- select(protocols.EF3, purchaseNumber, everything())
# плюс по некоторым участникам нет КПП, выкидываем этот столбец
protocols.EF3[, applications.kpp := NULL]
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols03 <- uf.process.large.table.normalising(protocols.EF3,
                                                     'DT_fcsProtocolEF3_clean.csv',
                                                     out.path)

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols03[is.na(as.numeric(applications.journalNumber)), 
               applications.journalNumber.comment := applications.journalNumber]
DT.protocols03[, applications.journalNumber := as.numeric(applications.journalNumber)]
table(DT.protocols03$applications.journalNumber)
# число пропусков
sum(is.na(DT.protocols03$applications.journalNumber))

# числовой столбец application.appRating содержит текстовые причины отказа
DT.protocols03[is.na(as.numeric(application.appRating)), 
               application.appRating.drop.reason := application.appRating]
DT.protocols03[, application.appRating := as.numeric(application.appRating)]
table(DT.protocols03$application.appRating)
# число пропусков
sum(is.na(DT.protocols03$application.appRating))

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols03)[colnames(DT.protocols03) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols03)[colnames(DT.protocols03) == 'protocolDate'] <-
    'protocol03Date'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEF3_clean.csv')
uf.write.table.with.metadata(DT.protocols03, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# проверка .....................................................................
length(unique(DT.protocols03$purchaseNumber))
length(na.omit(unique(DT.protocols03$purchaseNumber)))
nrow(filter(DT.proc.index, prefix == 'fcsProtocolEF3'))
# здесь надо смотреть ещё протоколы с единственной заявкой и с единственным
#  участником аукциона
