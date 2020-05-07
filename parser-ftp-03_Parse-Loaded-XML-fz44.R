# ..............................................................................
# parser-ftp-03_Parse-Loaded_XML-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  http://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.3.2 (06 May 2020)
# 
# Эта часть кода парсит xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ
# ..............................................................................



# 3. РАЗБОР XML-ФАЙЛОВ ---------------------------------------------------------

log.parse.XML.filename <- paste0(gsub('csv/$', '', sRawCSVPath), 
                                 'log_xml_parse.txt')


# Делаем индекс файлов с аукционами, включая номера документов =================

# DT.proc.index <- uf.make.file.index.for.proc(all.xmls)


# Читаем таблицу с шаблонами для разбора xml ===================================

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

# читаем шаблоны для разбора xml из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.patterns <- read.csv2(paste0(sRefPath, 'df_xml_patterns.csv'),
                         stringsAsFactors = F)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# делаем список с шаблонами для парсинга
patterns <- uf.make.colnames.from.xpath(df.patterns)

# список префиксов файлов из индекса, для которых нет шаблона
pref.no.pattern <- unique(DT.proc.index$prefix)[!(unique(DT.proc.index$prefix) 
                                                  %in% names(df.patterns))]
if (length(pref.no.pattern) > 0) {
    cat(red((paste0('На эти префиксы нет шаблона: ', pref.no.pattern, 
                   collapse = '\n'))))
    # полное имя файла, чтобы просмотреть в браузере
    message(paste0(getwd(), gsub('^[.]', '', sRawXMLPath), 
                   grep(pref.no.pattern, all.xmls, value = T)[1]))
}


# Парсим файлы с электронными аукционами =======================================

# # для настройки частичного парсинга ............................................
# # только те префиксы, файлы для которых не сохранены
# loop.prefs <- loop.prefs[!(loop.prefs %in% gsub('[.]csv$', '',
#                                             gsub('^[^_]*_', '', dir(out.path))))]
# только файлы заданного типа
loop.prefs <- 'fcsProtocolEFSingleApp'
# loop.prefs <- 'fcsProtocolEF1'
# только выборочные шаблоны
# patterns[['fcsNotificationEA44']] <- 
#     patterns[['fcsNotificationEA44']][c(1, 2, 28, 29), ]
# # ..............................................................................

# # цикл по типам файлов
# loop.prefs <- unique(DT.proc.index$prefix)

for (pref in loop.prefs) {

    # файл для записи результатов
    filename.to.save.data <- paste0(out.path, 'DT_', pref, '.csv')
    
    # отладка
    # pref <- loop.prefs[1]

    # выборка с файлами текущего префикса
    DT <- filter(DT.proc.index, prefix == pref)
    
    # проверяем, есть ли шаблоны тегов для этого префикса
    if (is.null(patterns[[pref]])) {
        message(paste0('Не найдены теги для префикса файла: ', pref,
                       '\nПропускаю ', nrow(DT), ' файлов.\n'))
        DT <- NULL
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
    #  запись в файл происходит внутри функции uf.parse.xmls.with.prefix
    res <- uf.parse.xmls.with.prefix(flnms, patterns[[pref]], 5000, 
                                     filename.to.save.data,
                                     log.parse.XML.filename)
    if (is.null(res)) {
        break
    }
    
    # чистим память
    res <- NULL
    rm(res)
}



# 4. ЧИСТИМ RAW-ТАБЛИЦЫ --------------------------------------------------------


#  * Работаем с таблицей объявлений ============================================

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
DT.notif <- data.table(read.csv2(paste0(out.path, 'DT_fcsNotificationEA44.csv'), 
                                 stringsAsFactors = F,
                                na.strings = c('NA', '<NA>'),
                                # encoding = 'UTF-8',
                                colClasses = 'character'))
# проверка
dim(DT.notif)
str(DT.notif)
colnames(DT.notif)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# повторы номеров извещений с разными датами и/или ценами
select(DT.notif, fcsNotificationEF.purchaseNumber, docPublishDate, 
       lot.maxPrice)[, lapply(.SD, function(x) {
           length(unique(x))
       }), by = fcsNotificationEF.purchaseNumber][docPublishDate > 1 &
                                                      lot.maxPrice > 1, ][order(-docPublishDate), ]
# повторы номеров извещений с разными датами и кодами ОКПД
select(DT.notif, fcsNotificationEF.purchaseNumber, docPublishDate, 
       purchaseObject.OKPD2.code, lot.maxPrice)[, lapply(.SD, function(x) {
           length(unique(x))
       }), by = fcsNotificationEF.purchaseNumber][docPublishDate > 1 &
                                                      purchaseObject.OKPD2.code > 1 & lot.maxPrice > 1 ][order(-docPublishDate), ]

table(DT.notif[fcsNotificationEF.purchaseNumber == '0301300208018000066', ]$purchaseObject.OKPD2.code)


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
all.responsibleOrgs <- NULL
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
DT.restrictions <- NULL
rm(DT.restrictions)


# ..............................................................................
#
# ТОВАРЫ
# ..............................................................................

# ПЕРВЫЙ КЛАССИФИКАТОР ТОВАРОВ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
columns.to.search <- c('purchaseNumber', 'fcsNotificationEF.id',
                       'purchaseObject.OKPD2.code')
# 'purchaseObject.OKPD2.name' выдаёт ошибку в некоторых случаях (падает сессия)
#   поэтому разбиваем на два шага
DT.all.OKPD2 <- DT.notif %>% select(one_of(columns.to.search))
DT.all.OKPD2$purchaseObject.OKPD2.name <- DT.notif$purchaseObject.OKPD2.name
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
DT.all.KTRU <- DT.notif %>% select(one_of(columns.to.search))
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
DT.all.MNNName <- DT.notif %>% select(one_of(columns.to.search))
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
DT <- NULL
DT.all.KTRU <- NULL
DT.all.OKPD2 <- NULL
DT.all.MNNName <- NULL
rm(DT, DT.all.KTRU, DT.all.OKPD2, DT.all.MNNName)


# ..............................................................................
#
# УЧАСТНИКИ АУКЦИОНОВ
# ..............................................................................

#  * Работаем с таблицей протоколов этапа 1 ====================================

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF1.csv')
protocols.EF1 <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                      colClasses = 'character'))
colnames(protocols.EF1)[colnames(protocols.EF1) == 'fcsProtocolEF1.purchaseNumber'] <-
    'purchaseNumber'
protocols.EF1
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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
colnames(DT.protocols01)[colnames(DT.protocols01) == 
                             'application.admitted.explanation'] <- 'admitted'
# логический столбец admitted содержит для непринятых заявок причины
# перекидываем причины в новый столбец, делаем admitted логическим
DT.protocols01[is.na(as.logical(admitted)), admitted.F.reason := admitted]
DT.protocols01[is.na(as.logical(admitted)), admitted := F]
DT.protocols01[, admitted := as.logical(admitted)]
table(DT.protocols01$admitted)
# число пропусков
sum(is.na(DT.protocols01$admitted))

# в столбца application.journalNumber есть текстовые идентификаторы
table(DT.protocols01$application.journalNumber)

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

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF2.csv')
protocols.EF2 <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                      colClasses = 'character'))
colnames(protocols.EF2)[colnames(protocols.EF2) == 'application.journalNumber.priceOffers'] <-
    'application.journalNumber'
protocols.EF2
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# ставим столбец с номером заявки на первое место
protocols.EF2 <- select(protocols.EF2, purchaseNumber, everything())
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols02 <- uf.process.large.table.normalising(protocols.EF2,
                                                     'DT_fcsProtocolEF2_clean.csv',
                                                     out.path)

# в столбце application.journalNumber есть текстовые идентификаторы
table(DT.protocols02$application.journalNumber)

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

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_fcsProtocolEF3.csv')
protocols.EF3 <- data.table(read.csv2(flnm, stringsAsFactors = F, 
                                      colClasses = 'character'))
protocols.EF3
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# ставим столбец с номером заявки на первое место
protocols.EF3 <- select(protocols.EF3, purchaseNumber, everything())
# плюс по некоторым участникам нет КПП, выкидываем этот столбец
protocols.EF3[, applications.kpp := NULL]
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols03 <- uf.process.large.table.normalising(protocols.EF3,
                                                     'DT_fcsProtocolEF3_clean.csv',
                                                     out.path)

# Делаем имя application.appRating.reason.application.notConsidered короче
colnames(DT.protocols03)[grepl('application.appRating', colnames(DT.protocols03))] <- 
    'application.appRating'

# числовой столбец application.journalNumber содержит текстовые идентификаторы
table(DT.protocols03$applications.journalNumber)

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols03)[colnames(DT.protocols03) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols03)[colnames(DT.protocols03) == 'protocolDate'] <-
    'protocol03Date'

# числовой столбец application.appRating содержит текстовые причины отказа
#  выносим их в отдельный столбец
DT.protocols03[is.na(as.numeric(application.appRating)), 
               application.appRating.drop.reason := application.appRating]
DT.protocols03[, application.appRating := as.numeric(application.appRating)]


# # отладка
# tmp[!is.na(application.appRating) & !is.na(application.appRating.drop.reason), ]
# tmp[purchaseNumber == '0340200003319009253', ]
# DT.protocols03[purchaseNumber == '0340200003319009253', ]

table(DT.protocols03$application.appRating)
# число пропусков
sum(is.na(DT.protocols03$application.appRating))

# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEF3_clean.csv')
uf.write.table.with.metadata(DT.protocols03, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# проверка .....................................................................
length(unique(DT.protocols03$purchaseNumber))
length(na.omit(unique(DT.protocols03$purchaseNumber)))
nrow(filter(DT.proc.index, prefix == 'fcsProtocolEF3'))


#  * Работаем с таблицей протоколов аукционов с единственной заявкой ===========

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
singleApp <- data.table(read.csv2(paste0(out.path, 
                                         'DT_fcsProtocolEFSingleApp.csv'), 
                                  stringsAsFactors = F,
                                  na.strings = c('NA', '<NA>'),
                                  # encoding = 'UTF-8',
                                  colClasses = 'character'))
# проверка
dim(singleApp)
str(singleApp)
colnames(singleApp)

# ставим столбец с номером заявки на первое место
singleApp <- select(singleApp, purchaseNumber, everything())
# переименовываем столбцы для дальнейшего связывания таблиц
colnames(singleApp)[colnames(singleApp) == 'protocolDate'] <- 'protocolSingleAppDate'

# нормализуем таблицу (расклеиваем значения в столбцах)
DT.singleApp <- uf.process.large.table.normalising(singleApp,
                                                   'DT_fcsProtocolEFSingleApp_clean.csv',
                                                   out.path)

# столбец с ценой числовой
DT.singleApp$winnerPrice <- as.numeric(DT.singleApp$winnerPrice)

# если по любой причине единственный участник отклонён, 
#  то контракт не будет заключён
DT.singleApp[!is.na(application.appRejectedReason.code), proc.success := F]
DT.singleApp[is.na(application.appRejectedReason.code), proc.success := T]

# выносим коды причин отклонения заявок в дамми
rej.codes <- names(table(DT.singleApp$application.appRejectedReason.code))
for (i in 1:length(rej.codes)) {
    
    DT.singleApp$VAR <- 0
    
    # закупки, у которых стоит код отклонения заявки
    p.loop <- DT.singleApp[DT.singleApp$application.appRejectedReason.code == rej.codes[i], ]$purchaseNumber
    DT.singleApp[purchaseNumber %in% p.loop, VAR := 1]
    
    # закупки, у которых код отклонения заявки NA
    p.loop <- DT.singleApp[is.na(application.appRejectedReason.code), ]$purchaseNumber
    DT.singleApp[purchaseNumber %in% p.loop, VAR := 0]
    
    # переименовываем столбец в соответствии с кодом отказа
    colnames(DT.singleApp)[colnames(DT.singleApp) == 'VAR'] <- 
        paste0('rejected.', rej.codes[i])
}

# оставляем только столбцы для модели
col.keep <- grep('^rejected[.]', colnames(DT.singleApp), value = T)
DT.singleApp <- select(DT.singleApp, purchaseNumber, protocolSingleAppDate,
                       application.inn, application.countryFullName,
                       proc.success, winnerPrice, proc.success,
                       all_of(col.keep))
DT.singleApp <- unique(DT.singleApp)

# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEFSingleApp_clean.csv')
uf.write.table.with.metadata(DT.singleApp, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#  * Работаем с таблицей протоколов аукционов с единственным участником ========

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
singlePart <- data.table(read.csv2(paste0(out.path, 
                                         'DT_fcsProtocolEFSinglePart.csv'), 
                                  stringsAsFactors = F,
                                  na.strings = c('NA', '<NA>'),
                                  # encoding = 'UTF-8',
                                  colClasses = 'character'))
# проверка
dim(singlePart)
str(singlePart)
colnames(singlePart)

# ставим столбец с номером заявки на первое место
singlePart <- select(singlePart, purchaseNumber, everything())
# переименовываем столбцы для дальнейшего связывания таблиц
colnames(singlePart)[colnames(singlePart) == 'protocolDate'] <- 'protocolSinglePartDate'

# нормализуем таблицу (расклеиваем значения в столбцах)
DT.singlePart <- uf.process.large.table.normalising(singlePart,
                                                   'DT_fcsProtocolEFSinglePart_clean.csv',
                                                   out.path)

# столбец с ценой числовой
DT.singlePart$winnerPrice <- as.numeric(DT.singlePart$winnerPrice)

# преобразуем явно в таблицу
DT.singlePart <- data.table(DT.singlePart)

# если по любой причине единственный участник отклонён, 
#  то контракт не будет заключён
DT.singlePart[!is.na(application.appRejectedReason.code), proc.success := F]
DT.singlePart[is.na(application.appRejectedReason.code), proc.success := T]

# выносим коды причин отклонения заявок в дамми
rej.codes <- names(table(DT.singlePart$application.appRejectedReason.code))
if (length(rej.codes) > 0) {
    for (i in 1:length(rej.codes)) {
        
        DT.singlePart$VAR <- 0
        
        # закупки, у которых стоит код отклонения заявки
        p.loop <- DT.singlePart[DT.singlePart$application.appRejectedReason.code == rej.codes[i], ]$purchaseNumber
        DT.singlePart[purchaseNumber %in% p.loop, VAR := 1]
        
        # закупки, у которых код отклонения заявки NA
        p.loop <- DT.singlePart[is.na(application.appRejectedReason.code), ]$purchaseNumber
        DT.singlePart[purchaseNumber %in% p.loop, VAR := 0]
        
        # переименовываем столбец в соответствии с кодом отказа
        colnames(DT.singlePart)[colnames(DT.singlePart) == 'VAR'] <- 
            paste0('rejected.', rej.codes[i])
    } 
    
    col.keep <- grep('^rejected[.]', colnames(DT.singlePart), value = T)
} else {
    col.keep <- NULL
}


# TODO перепарсить нормально (теги ИНН, КПП поехали, в df.patterns исправлено)
# TODO добавить ИНН в очищенную таблицу

# оставляем только столбцы для модели
DT.singlePart <- select(DT.singlePart, purchaseNumber, protocolSinglePartDate,
                        application.countryFullName, proc.success, winnerPrice, 
                        proc.success,
                        all_of(col.keep))
DT.singlePart <- unique(DT.singlePart)

# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(out.path, 'DT_fcsProtocolEFSinglePart_clean.csv')
uf.write.table.with.metadata(DT.singlePart, flnm)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#  * Работаем с таблицей результатов ===========================================

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
placementResult <- data.table(read.csv2(paste0(out.path, 
                                          'DT_fcsPlacementResult.csv'), 
                                   stringsAsFactors = F,
                                   na.strings = c('NA', '<NA>'),
                                   # encoding = 'UTF-8',
                                   colClasses = 'character'))
# проверка
dim(placementResult)
str(placementResult)
colnames(placementResult)

# типы столбцов
placementResult$application.journalNumber <- 
    as.numeric(placementResult$application.journalNumber)
placementResult$application.appRating <- 
    as.numeric(placementResult$application.appRating)
placementResult$application.price <- 
    as.numeric(placementResult$application.price)
placementResult$procedurelFailed <- 
    as.logical(toupper(placementResult$procedurelFailed))

# нормализуем таблицу (расклеиваем значения в столбцах)
DT.placementResult <- 
    uf.process.large.table.normalising(placementResult,
                                       'DT_fcsPlacementResult_clean.csv',
                                       out.path)



#  * Работаем с отменами =======================================================

# отмена протокола #############################################################

# грузим результаты парсинга XML (не нормализованную таблицу) <<<<<<<<<<<<<<<<<<
protocolCancel <- data.table(read.csv2(paste0(out.path, 
                                               'DT_fcsProtocolCancel.csv'), 
                                        stringsAsFactors = F,
                                        na.strings = c('NA', '<NA>'),
                                        # encoding = 'UTF-8',
                                        colClasses = 'character'))
# проверка
dim(protocolCancel)
str(protocolCancel)
colnames(protocolCancel)

# проверка на склейки
sapply(protocolCancel, function(x) {sum(grepl('#', x))})

grep('#', protocolCancel$fcsProtocolCancel.docDate, value = T)

# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocolCancel <- 
    uf.process.large.table.normalising(protocolCancel,
                                       'DT_fcsProtocolCancel_clean.csv',
                                       out.path)


# отмена объявления ############################################################

notificationCancel <- data.table(read.csv2(paste0(out.path, 
                                              'DT_fcsNotificationCancel.csv'), 
                                       stringsAsFactors = F,
                                       na.strings = c('NA', '<NA>'),
                                       # encoding = 'UTF-8',
                                       colClasses = 'character'))
# проверка
dim(notificationCancel)
str(notificationCancel)
colnames(notificationCancel)

# проверка на склейки
sapply(notificationCancel, function(x) {sum(grepl('#', x))})

grep('#', notificationCancel$fcsNotificationCancel.docDate, value = T)

# нормализуем таблицу (расклеиваем значения в столбцах)
DT.notificationCancel <- 
    uf.process.large.table.normalising(notificationCancel[, 1:3],
                                       'DT_fcsNotificationCancel_clean.csv',
                                       out.path)

