# ..............................................................................
# parser_ftp.R
# ..............................................................................
# Парсинг содержимого необъятного ftp сервера госзакупок, 
#  который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
# ..............................................................................
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# ..............................................................................
# Версия 1.1 (16.07.2019)
# ..............................................................................



# НАСТРОЙКИ --------------------------------------------------------------------


# Загрузка библиотек ===========================================================
library('RCurl')
library('XML')
library('dplyr')
library('reshape')
library('data.table')
library('stringi')
library('ggplot2')
library('httr')
library('readr')
library('jsonlite')
library('RJSONIO')


# Настройки системы ============================================================
# отображение больших цифр в обычном формате, а не в экспоненциальном
options("scipen" = 100, "digits" = 4)


# Функции ======================================================================

files.sources <- dir('./functions/')
sapply(paste0('./functions/', files.sources), source)


# Константы ====================================================================

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10

# координаты FTP-сервера госзакупок и логин-пароль для бесплатного доступа
sFTPURL <- 'ftp://ftp.zakupki.gov.ru/'
sUserPwd <- 'free:free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
# sYEAR <- c('201709', '201710', '201711', '201712', '201801', '201802', '201803',
#           '201804', '201805')
sYEAR <- c(paste0(rep(2018, ), formatC(6:12, width = 2, flag = '0')),
           paste0(rep(2019, ), formatC(1:6, width = 2, flag = '0')))

# список директорий с регионами
doc <- getURL(paste0(sFTPURL, 'fcs_regions/'), 
              ftp.use.epsv = FALSE, dirlistonly = TRUE, 
              userpwd = sUserPwd)
# отладка
# write(doc, 'tmp.txt')
# гении с ftp госзакупок разместили под списком регионов гиганский список логов
#  поэтому его надо отрезать
doc <- gsub(pattern = '_logs.*$', replacement = '', doc)

sRegionFoldersNames <- unlist(strsplit(doc, '\n'))
# убираем папки, не относящиеся к регионам
sRegionFoldersNames <- sRegionFoldersNames[grep('_Resp$|_kraj$|_.?obl$', 
                                                  sRegionFoldersNames)]
message(paste0('Regions: ', length(sRegionFoldersNames), ' folders.'))

# преобразуем папки регионов в URL-адреса
sRegionFolderURLs <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            sRegionFoldersNames, '/')

#  регион: Татарстан
sRegionFolderURLs[66]

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')


# * Структура директорий рабочей папки #########################################

# все равки исходных данных ....................................................
sRawDataPath <- './data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# исходные данные в архивах, выгрузка за период ................................
sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], 
               '_16-07-2019', '/')
if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- paste0(sRawArchPath, 'xmls/')
if (!dir.exists(sRawXMLPath)) dir.create(sRawXMLPath)

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- paste0(sRawArchPath, 'csv/')
if (!dir.exists(sRawCSVPath)) dir.create(sRawCSVPath)

# таблицы-справочники ..........................................................
sRefPath <- paste0('./data/reference/')
if (!dir.exists(sRefPath)) dir.create(sRefPath)

# графики ......................................................................
sPlotPath <- paste0('./plots/')
if (!dir.exists(sPlotPath)) dir.create(sPlotPath)


# Переменные ===================================================================

#  регион (регионы): Республика Татарстан
my.region <- grep(sRegionFolderURLs, pattern = 'Tatar', value = T)
message(paste0('Работаем с регионом: ', my.region))



# 1. ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК -------------------------------------

# # ВНИМАНИЕ: загрузка архивов занимает время !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # выгрузка xml в архивах в папку './data/raw/' в рабочей директории
# for (sRegion in my.region) {
# 
#     for (sSubf in sSubfolders) {
#         # адреса архивов на сервере
#         zips <- 
#             unlist(strsplit(getURL(paste0(sRegion, sSubf),
#                                    ftp.use.epsv = FALSE, dirlistonly = TRUE, 
#                                    userpwd = sUserPwd), '\r\n'))
#         
#         # берём заявки, открытые в периодах, начинающихся с sYEAR
#         for (y in sYEAR) {
#             # адреса архивов за период
#             zips.y <- grep(zips, 
#                            pattern = paste0('_', y, '.*_.*_.*'), value = T)
#             
#             # отладка
#             # print(paste0(y, ': ', zips.y))
# 
#             # загружаем архивы в папку с равками по одному

# папка для загрузки в /data/raw
# path <- paste0('./data/raw/', YEAR[1], '-', YEAR[length(YEAR)], '_',
#              format(Sys.time(), '%d-%m-%Y'), '/')
path <- paste0('./data/raw/', YEAR[1], '-', YEAR[length(YEAR)], 
               '_15-06-2018', '/')
if (!dir.exists(path)) {
    dir.create(path)
}

# выгрузка xml в архивах в папку './data/raw/' в рабочей директории
my.region <- grep(region.folder.url, pattern = 'Tatar', value = T)
# загрузка архивов занимает время!
# for (sRegion in my.region) {
# 
#     for (sSubf in sSubfolders) {
#         zips <- unlist(strsplit(getURL(paste0(sRegion, sSubf),
#                                        ftp.use.epsv = FALSE,
#                                        dirlistonly = TRUE, userpwd = userpwd),
#                                 '\r\n'))
#         # берём заявки, открытые в периодах, начинающихся с YEAR
#         for (y in YEAR) {
#             zips.y <- grep(zips, pattern = paste0('_', y, '.*_.*_.*'), value = T)
#             # print(paste0(y, ': ', zips.y))
# 
#             for (sZip in zips.y) {
#                 download.file(paste0('ftp://', sUserPwd, '@',
#                                      gsub('^.*://', '', sRegion), sSubf, sZip),
#                               destfile = paste0(sRawArchPath, sZip))
#             }
#         }
#     }
# }


# 2. РАСПАКОВКА АРХИВОВ --------------------------------------------------------
# # имена всех загруженных архивов
# xmlzips <- dir(sRawArchPath)
# 
# # ВНИМАНИЕ: оооочень долго !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # распаковываем архивы в папку для XML-ей
# распаковываем архивы в tmp
# оооочень долго!
# length(xmlzips)
# # счётчики файлов для сообщений статуса
# i.files.сount <- 0
# n <- length(xmlzips)
# # счётчик строк вывода в консоли
# console.clean.count <- 0
# for (sXMLzip in xmlzips) {
#     # распаковать архив в папку для XML-ей; в каждом архиве до кучи xml-файлов
#     unzip(paste0(sRawArchPath, sXMLzip), exdir = sRawXMLPath)
#     
#     # обновить счётчики и выдать сообщение в консоль
#     i.files.сount <- i.files.сount + 1
#     message(paste0('Распаковка...', round(i.files.сount / n * 100, 1), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }


#     # шаг 1: распаковать архив в tmp. там до кучи xml-файлов
#     unzip(paste0(path, sXMLzip), exdir = tmppath)
#     iCount <- iCount + 1
#     message(paste0('Распаковка...', round(iCount / length(xmlzips) * 100,
#                                           1), '%'))
# }


# индексация: считаем файлы, которые относятся к уникальным ID извещений
all.xmls <- grep(dir(tmppath), pattern = 'xml$', value = T)
length(all.xmls)

# все уникальные номера извещений
#  сначала убираем 'fcs_'
all.ids <- sub('^fcs_', '^fcs', all.xmls)
# затем всё, что идёт до первого '_', включая его
all.ids <- sub('^(.*?)_', '', all.ids)
# теперь всё, что идёт от '_' и до конца
all.ids <- sub('_.*$', '', all.ids)
all.ids <- unique(all.ids)
table(nchar(all.ids))
length(all.ids)

# все префиксы файлов
prefixes <- table(gsub('_$', '', gsub(all.xmls, pattern = '(\\d{19}|\\d{18}).*$', 
                                      replacement = '')))
prefixes
length(prefixes)

# индексируем
dt.file.index <- data.table(noticeID = all.ids)

n <- length(prefixes)
prc <- 0
for (pr_i in names(prefixes)) {
    tmp.v <- grep(all.xmls, pattern = paste0('^', pr_i), value = T)

    noticeID <- sub('^fcs_', '^fcs', tmp.v)
    noticeID <- sub('^(.*?)_', '', noticeID)
    noticeID <- sub('_.*$', '', noticeID)
        
    DT <- data.table(table(noticeID))

    dt.file.index <- merge(dt.file.index, DT, by = 'noticeID', all.x = T)
    dt.file.index[is.na(N), N := 0]
    colnames(dt.file.index)[colnames(dt.file.index) == 'N'] <- pr_i

    prc <- prc + 1
    message(paste0(pr_i, ', ', round(prc / n * 100, 1), '%'))
}

sapply(dt.file.index, function(x){sum(is.na(x))})
summary(dt.file.index)
table(nchar(dt.file.index$noticeID ))

# записываем таблицу-индекс
write.csv2(dt.file.index, paste0(path, 'index_ALL.csv'), row.names = F)

# весь индекс одним файлом
dt.file.index <- read.csv2(paste0(path, 'index_ALL.csv'),
                           stringsAsFactors = F, 
                           colClasses = c('character', rep('numeric', 55)))
dt.file.index <- data.table(dt.file.index)

# dt.file.index$noticeID <- as.character(format(as.numeric(dt.file.index$noticeID),
#                                               scientific = F))
length(unique(dt.file.index$noticeID))
str(dt.file.index)

table(nchar(dt.file.index$noticeID))
dt.file.index$noticeID[nchar(dt.file.index$noticeID) == 18][1:10]

# ПРОИНДЕКСИРОВАН ТАТАРСТАН С СЕНТЯБРЯ ПО МАЙ
n <- nrow(dt.file.index)
message(paste0('Всего уникальных id закупок за период: ', n))

# аукционы
loop.ids <- dt.file.index[dt.file.index$fcsNotificationEA44 > 0, ]$noticeID
message(paste0('Извещений об электронных аукционах: ', length(loop.ids),
               ' (', round(length(loop.ids) / n * 100, 1), '%)'))

# как среди них распределяются протоколы
sapply(dt.file.index[noticeID %in% loop.ids, -1], max)[
    sapply(dt.file.index[noticeID %in% loop.ids, -1], max) > 0]

# почему же контракты не обнаруживаются??
gp <- ggplot(data = dt.file.index[noticeID %in% loop.ids, -1], aes(x = contract)) +
    geom_histogram()
gp

# СЮРПРИЗ
#  файлы контрактов идут С ДРУГИМИ ID
#  см. 0111100000217000017
#  и контракт contract_1165802029417000020_37417208.xml
#  таким образом, найти контракт, которым завершилась процедура, 
#  похоже, можно только по тегу notificationNumber в теле файла с префиксом
#  contract_

# больше всего каких-то fcsContractSign. Похоже, это просто электронные подписи
#  со стороны заказчиков + все реквизиты заказа
nrow(dt.file.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
head(dt.file.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
tail(dt.file.index[noticeID %in% loop.ids & fcsContractSign > 0, ])

# fcsProtocolEF3 -- это финальная стадия аукциона
nrow(dt.file.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
head(dt.file.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
tail(dt.file.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])


length(loop.ids) / nrow(dt.file.index)

sapply(dt.file.index[, -1], min)



dt.file.index <- dt.file.index[, 1:4]
head(df.file.index)

# почему-то количество уникальных id меньше, чем число строк в индексе
nrow(df.file.index)
length(unique(df.file.index$noticeID))
# делаем таблицу с уникальными id
df.file.index.unq <- data.table(df.file.index)
df.file.index.unq[, contract := sum(contract), by = 'noticeID']
df.file.index.unq[, contractProcedure := sum(contractProcedure), by = 'noticeID']
df.file.index.unq[, contractProcedureCancel := sum(contractProcedureCancel), by = 'noticeID']
df.file.index.unq <- unique(df.file.index.unq)

head(df.file.index.unq)
dim(df.file.index.unq)

sapply(df.file.index.unq[, 2:4], sum)
sapply(df.file.index[, 2:4], sum)


    
# парсим файлы с аукционами (цикл по номерам извещений)
# выкидываем id без файлов contract -- это документы для заключенных ранее
loop.ids <- df.file.index.unq[contract != 0, ]$contract
table(loop.ids)

# теперь разбираемся: что за id с 428 контрактами?
ids <- df.file.index.unq[contract == 406, ]$noticeID

files <- NULL
for (id in ids) {
    tmp <- grep(all.xmls, pattern = id, value = T)
    message(paste0(id, ': ', paste0(tmp, collapse = "; ")))
    
    if (length(tmp) > 0) {
        files <- c(files, tmp)
    }
}
# ну феноменально, чо. индекс ли кривой, или ещё что... такие файлы не найдены!

doc <- xmlTreeParse(paste0(tmppath, '/', files[1]))
class(doc)

getNodeSet(doc, namespaces = 'ns2')





# Разбираем все подряд заявки --------------------------------------------------
#  запаситесь попкорном

# набор xpath запросов для парсинга xml: архивы contracts
xpath.patterns <-
    c('//xmlns:id', '//xmlns:publishDate', '//xmlns:notificationNumber',
      '//xmlns:currentContractStage', '//xmlns:customer//xmlns:fullName',
      '//xmlns:customer//xmlns:inn', '//xmlns:product/xmlns:OKPD2/xmlns:code',
      '//xmlns:product/xmlns:OKPD2/xmlns:name',
      '//xmlns:product/xmlns:OKEI/xmlns:code',
      '//xmlns:product/xmlns:OKEI/xmlns:fullName',
      '//xmlns:product/xmlns:quantity', '//xmlns:product/xmlns:sumRUR',
      '//xmlns:suppliers//xmlns:fullName', '//xmlns:suppliers//xmlns:INN',
      '//xmlns:suppliers//xmlns:OKTMO/xmlns:code',
      '//xmlns:suppliers//xmlns:OKTMO/xmlns:name')
names(xpath.patterns) <- c('id', 'publishDate', 'notificationNumber',
                           'currentContractStage', 'customer.name',
                           'customer.inn', 'OKPD2.code', 'OKPD2.name',
                           'OKEI.code', 'OKEI.fullName', 'quantity', 'sumRUR',
                           'supplier.fullName', 'supplier.INN',
                           'supplier.OKTMO.code', 'supplier.OKTMO.name')


# таблица со всеми контрагентами
df.kon <- data.frame(inn = '', kpp = '', organizationName = '', 
                     stringsAsFactors = F)
# таблица с закупками
df.zak <- data.frame(purchase_id = '', customer_inn = '', okpd2_1 = '',
                     okpd2_2 = '', stage = '', one_participant = '',
                     for_small_and_medium = '', start_max_price = '',
                     purchase_sum = '', eval_date = '', contracet_date = '',
                     fed_distr = '',
                     stringsAsFactors = F)
# таблица с заявками поставщиков-участников торгов
# df.zay <- 


# 3. ИНДЕКСАЦИЯ XML-ФАЙЛОВ -----------------------------------------------------

# # все имена извлечёных xml-файлов ..............................................
# all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
# 
# message(paste0('Всего файлов формата XML: ', length(all.xmls)))
# 
# # вектор большой, поэтому записываем его в таблицу
# write.csv2(data.frame(files = all.xmls), paste0(sRawArchPath, 'all_xmls.csv'),
#            row.names = F)
# 
# all.xmls <- read.csv2(paste0(sRawCSVPath, 'all_xmls.csv'),
#                       stringsAsFactors = F)[, 1]
# 
# message(paste0('Всего файлов формата XML: ', length(all.xmls)))
# message('Первые 10: ')
# message(paste0(all.xmls[1:10], collapse = '\n'))


# # все уникальные номера извещений ..............................................
# #  сначала убираем 'fcs_'
# all.ids <- sub('^fcs_', '^fcs', all.xmls)
# 
# # затем всё, что идёт до первого '_', включая его
# all.ids <- sub('^(.*?)_', '', all.ids)
# 
# # теперь всё, что идёт от '_' и до конца
# all.ids <- sub('_.*$', '', all.ids)
# all.ids <- unique(all.ids)
# 
# message(paste0('Всего уникальных номеров извещений: ', length(all.ids)))
# message('Количество символов в номерах извещений:')
# table(nchar(all.ids))

# # все префиксы файлов ..........................................................
# prefixes <- table(gsub('_$', '', gsub(all.xmls, pattern = '(\\d{19}|\\d{18}).*$',
#                                       replacement = '')))
# 
# message('Всего префиксов файлов: ', length(prefixes))
# sort(prefixes, decreasing = T)


# # Индексируем имена всех файлов ================================================
# # таблица с именами всех файлов: имена столбцов по префиксам, первый столбец -- 
# #  номера измещений, значения в остальных столбцах -- кол-во файлов 
# #  по связке (ID + префикс)
# #
# DT.xml.files.index <- data.table(noticeID = all.ids)
# 
# # счётчик для очистки консоли
# console.clean.count <- 0
# # счётчики файлов для сообщений статуса
# i.files.сount <- 0
# n <- length(prefixes)
# 
# df.diff.IDs <- data.frame(fileID = NULL, tagID = NULL)
# 
# # цикл по уникальным префиксам файлов
# for (pr_i in names(prefixes)) {
#     
#     # вытаскиваем все имена файлов с заданным префиксам
#     tmp.v <- grep(all.xmls, pattern = paste0('^', pr_i), value = T)
# 
#     # вытаскиваем noticeID из имени файла
#     noticeID <- sub('^fcs_', '^fcs', tmp.v)
#     noticeID <- sub('^(.*?)_', '', noticeID)
#     noticeID <- sub('_.*$', '', noticeID)
#     
#     DT <- data.table(table(noticeID))
# 
#     DT.xml.files.index <-
#         merge(DT.xml.files.index, DT, by = 'noticeID', all.x = T)
#     DT.xml.files.index[is.na(N), N := 0]
#     colnames(DT.xml.files.index)[colnames(DT.xml.files.index) == 'N'] <- pr_i
# 
#     i.files.сount <- i.files.сount + 1
#     message(paste0(pr_i, ', ', round(i.files.сount / n * 100, 1), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }
# 
# # проверка
# sapply(DT.xml.files.index, function(x){sum(is.na(x))})
# summary(DT.xml.files.index)
# table(nchar(DT.xml.files.index$noticeID ))
# 
# # записываем таблицу-индекс
# write.csv2(DT.xml.files.index, paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
#            row.names = F)
#
# # читаем весь индекс одним файлом
# DT.xml.files.index <- read.csv2(paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
#                            stringsAsFactors = F,
#                            colClasses = c('character', rep('numeric', 55)),
#                            encoding = 'CP-1251')
# DT.xml.files.index <- data.table(DT.xml.files.index)
# dim(DT.xml.files.index)
# str(DT.xml.files.index)
# 
# message(paste0('Уникальных номеров извещений в файле-индексе: ', 
#                length(unique(DT.xml.files.index$noticeID))))
# 
# # проверка
# table(nchar(DT.xml.files.index$noticeID))
# DT.xml.files.index$noticeID[nchar(DT.xml.files.index$noticeID) == 18][1:10]


# # Анализ файла-индекса =========================================================
# # ПРОИНДЕКСИРОВАН ТАТАРСТАН С СЕНТЯБРЯ ПО МАЙ
# n <- nrow(DT.xml.files.index)
# message(paste0('Всего уникальных id закупок за период: ', n))

# # аукционы
# loop.ids <- DT.xml.files.index[DT.xml.files.index$fcsNotificationEA44 > 0, ]$noticeID
# message(paste0('Извещений об электронных аукционах: ', length(loop.ids),
#                ' (', round(length(loop.ids) / n * 100, 1), '%)'))

# # как среди них распределяются протоколы
# sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max)[
#     sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max) > 0]
# 
# # почему же контракты не обнаруживаются??
# gp <- ggplot(data = DT.xml.files.index[noticeID %in% loop.ids, -1], aes(x = contract)) +
#     geom_histogram()
# gp

# СЮРПРИЗ
#  файлы контрактов идут С ДРУГИМИ ID
#  см. 0111100000217000017
#  и контракт contract_1165802029417000020_37417208.xml
#  таким образом, найти контракт, которым завершилась процедура, 
#  похоже, можно только по тегу notificationNumber в теле файла с префиксом
#  contract_
#  при этом номер в имени файла -- это содержимое тега regNum

# grep(dir(sRawXMLPath), pattern = '0111100000217000017.*xml', value = T)

# # больше всего каких-то fcsContractSign. Похоже, это просто электронные подписи
# #  со стороны заказчиков + все реквизиты заказа
# nrow(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# head(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# tail(DT.xml.files.index[noticeID %in% loop.ids & fcsContractSign > 0, ])
# 
# # fcsProtocolEF3 -- это финальная стадия аукциона
# nrow(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
# head(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])
# tail(DT.xml.files.index[noticeID %in% loop.ids & fcsProtocolEF3 > 0, ])


# # первые 11 цифр purchaseNumber -- это id заказчика
# message(paste0('Уникальных заказчиков: ', length(unique(substr(loop.ids, 1, 11)))))
   


# 3. РАЗБОР XML-ФАЙЛОВ ---------------------------------------------------------


# # Делаем индекс файлов с аукционами ============================================
# # выясняем все префиксы файлов с интересующими id
# dt.loop.filenames <- gsub(all.xmls, pattern = 'fcs_', replacement = 'fcs')
# dt.loop.filenames <- gsub(dt.loop.filenames, pattern = '[.]xml',
#                           replacement = '')
# dt.loop.filenames <- data.table(do.call(rbind, strsplit(dt.loop.filenames, '_')))
# dt.loop.filenames <- dt.loop.filenames[V2 %in% loop.ids, ]
# colnames(dt.loop.filenames) <- c('prefix', 'purchaseNum', 'id')
# loop.refs <- unique(dt.loop.filenames$prefix)
# # обязательно ставим аукционы на первое место
# dt.loop.filenames[, .N, by = prefix]
# dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}), by = prefix]
# loop.refs <- dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}),
#                                by = prefix][order(-purchaseNum), ]$prefix
# loop.refs


# # Идём по префиксам, делаем индекс =============================================
# # счётчик для очистки консоли
# console.clean.count <- 0
# # счётчики
# i.files.сount <- 0
# n <- length(loop.refs)
# # цикл по префиксам файлов, которые относятся к электронным аукционам
# for (pref in loop.refs) {
#     # имена xml-файлов с текущим префиксом
#     xmls.by.prefix <- 
#         grep(all.xmls, pattern = paste0(pref, '.*[.]xml'), value = T)
#     # убираем расширения
#     xmls.by.prefix <-
#         gsub(xmls.by.prefix, pattern = '[.]xml$', replacement = '')
#     # убираем префиксы
#     xmls.by.prefix <- 
#         gsub(xmls.by.prefix, pattern = paste0(pref, '_'), replacement = '')
#     
#     # делаем таблицу-индекс для текущего префикса
#     #  столбец 1: id извещения
#     #  столбец 2: id документа
#     DT.xmls.by.prefix.index <- 
#         data.table(purchaseNumber = gsub(xmls.by.prefix, pattern = '_.*$',
#                                          replacement = ''),
#                    id = gsub(xmls.by.prefix, pattern = '^.*_', replacement = ''))
#     colnames(DT.xmls.by.prefix.index)[colnames(DT.xmls.by.prefix.index) == 'id'] <- paste0('id.', pref)
# 
#     # совмещаем с общим индексом (все префиксы вместе)
#     i.files.сount <- i.files.сount + 1
#     if (i.files.сount == 1) {
#         DT.EA44.index <- DT.xmls.by.prefix.index
#         setkey(DT.EA44.index, purchaseNumber)
#     } else {
#         setkey(DT.xmls.by.prefix.index, purchaseNumber)
#         DT.EA44.index <- merge(DT.EA44.index, DT.xmls.by.prefix.index, 
#                                all.x = TRUE, allow.cartesian = T)
#     }
# 
#     # убираем мусор
#     rm(xmls.by.prefix, DT.xmls.by.prefix.index)
#     
#     message(paste0(pref, ': индекс готов на ', round(i.files.сount / n * 100, 0), '%'))
#     console.clean.count <- console.clean.count + 1
# 
#     # очистка консоли
#     if (console.clean.count == iMaxConsoleStatusLines) {
#         cat("\014")
#         console.clean.count <- 0
#     }
# }
# 
# # проверка
# dim(DT.EA44.index)
# str(DT.EA44.index)
# DT.EA44.index
# 
# # сохраняем
# write.csv2(DT.EA44.index, paste0(sRawCSVPath, 'DT_index_EA44.csv'),
#            row.names = F)

# DT.EA44.index <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_index_EA44.csv'),
#                                  stringsAsFactors = F,
#                                  colClasses = rep('character', 12)))
# DT.EA44.index
# 
# # процент пропусков
# round(DT.EA44.index[, lapply(.SD, function(x){sum(is.na(x))})] / nrow(DT.EA44.index), 2)
# # всего xml для обработки
# n <- sum(DT.EA44.index[, sapply(.SD, function(x){sum(!is.na(unique(x)))})][-1])
# message(paste0('Всего xml для обработки: ', n,  ' (',
#                round(n / length(all.xmls) * 100, 1),
#                '% от общего числа файлов).'))


# # Парсим файлы с электронными аукционами =======================================
# # список префиксов файлов
# colnames(DT.EA44.index)
# 
# # шаблоны для разбора хранятся в отдельном csv
# df.patterns <- read.csv2(paste0(sRefPath, 'df_xml_patterns.csv'),
#                          stringsAsFactors = F)
# 
# # преобразуем фрейм с шаблонами в список
# patterns <- as.list(df.patterns)
# patterns <- c(list(ns = c('xmlns:', 'xmlns:', 'oos:', 'xmlns:',
#                           'xmlns:', 'xmlns:')), patterns)
# 
# # делаем из шаблонов имена столбцов итоговой таблицы
# patterns[-1] <- lapply(patterns[-1], function(x) {
#     x <- x[x != '']
#     tmp <- gsub(x, pattern = 'xmlns[:]|oos[:]', replacement = '')
#     tmp <- gsub(tmp, pattern = '[//]|[/]', replacement = '.')
#     tmp <- gsub(tmp, pattern = '^[.]*', replacement = '')
#     tmp <- gsub(tmp, pattern = '[.]+', replacement = '.')
#     tmp <- gsub(tmp, pattern = '^.*:', replacement = '')
#     tmp <- gsub(tmp, pattern = '/.*:', replacement = '/')
#     tmp <- gsub(tmp, pattern = '[.]$', replacement = '')
#     tmp <- gsub(tmp, pattern = '[][]', replacement = '')
#     tmp <- gsub(tmp, pattern = '\\)|\\(', replacement = '')
#     tmp <- gsub(tmp, pattern = '([^\\|]*)\\|.*', replacement = '\\1')
# 
#     x <- data.frame(xml.pattern = x)
#     x$xpath.names <- as.character(tmp)
# 
#     x <- x
# })
# 
# # отладка
# # patterns
# patterns$fcsProtocolEF3
# # length(patterns)
# 
# # лог
# log.parse.XML.filename <- paste0(sRawArchPath, 'log_xml_parse.txt')
# if (!file.exists(log.parse.XML.filename)) file.create(log.parse.XML.filename)
# 
# # максимальное число строк таблицы в оперативной памяти
# step.n <- 10000
# 
# # цикл по типам файлов (столбцам таблицы DT.EA44.index со 2 по 15)
# for (j in 2:2) {
# 
#     # отладка
#     # j <- 2
# 
#     DT <- na.omit(unique(DT.EA44.index[, c(1, j), with = F]))
#     if (nrow(DT) == 0) {
#         next
#     }
# 
#     filePrefix <- gsub(colnames(DT)[2], pattern = '^.*[.]', replacement = '')
#     n.cols <- length(patterns[[filePrefix]][, 1])
# 
#     msg <- paste0('Начинаю разбор файлов ', filePrefix, ': ', Sys.time())
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     # это все имена файлов заданного типа
#     flnms <-
#         unique(paste0(sRawXMLPath, filePrefix, '_',
#                       unname(unlist(DT[, 1, with = F])), '_',
#                       unname(unlist(DT[, 2, with = F])), '.xml'))
#     tmp.files.count <- 0
# 
#     # отладка
#     # tmp.files.count <- 4
#     # flnms <- flnms[(((tmp.files.count - 1) * step.n) + 1):length(flnms)]
#     # flnms <- flnms[1:10]
# 
#     # счётчики для статуса
#     n <- length(flnms)
#     i.files.сount <- 0
#     # имена тегов
#     tagnames <- patterns[[filePrefix]][, 2]
#     # промежуточная таблица
#     df.lines.notices <- NULL
#     # счётчик для очистки консоли
#     console.clean.count <- 0
# 
#     for (f in flnms) {
# 
#         # отладка
#         # f <- flnms[1]
# 
#         rootNode <- xmlTreeParse(f, encoding = 'UTF-8', useInternalNodes = T)
#         # танцы с namespace
#         nsDefs <- xmlNamespaceDefinitions(rootNode)
#         ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
#         names(ns)[names(ns) == ''] <- 'xmlns'
# 
#         # парсим строку целиком
#         new.line <-
#             unlist(unname(sapply(patterns[[filePrefix]][, 1],
#                                  function(path) {
#                                      val.tmp <- xpathSApply(rootNode, path,
#                                                             xmlValue,
#                                                             namespaces = ns)
#                                      # если тег не найден, ставим NA
#                                      if (length(val.tmp) == 0) val.tmp <- NA
#                                      # если найдено несколько тегов,
#                                      #  сжимаем в одно значение
#                                      if (length(val.tmp) > 1) {
#                                          val.tmp <- paste(val.tmp,
#                                                           collapse = '#')
#                                          }
#                                      val.tmp
#                                      })))
#         names(new.line) <- tagnames
# 
#         # отладка
#         # new.line
# 
#         i.files.сount <- i.files.сount + 1
#         if (is.null(df.lines.notices)) {
#             df.lines.notices <- data.frame(t(new.line), stringsAsFactors = F)
#         } else {
#             df.lines.notices <- rbind(df.lines.notices, new.line)
#         }
# 
#         # когда набирается заданное количество строк, скидываем таблицу на диск
#         if (nrow(df.lines.notices) == step.n) {
#             tmp.files.count <- tmp.files.count + 1
#             tbl.name <-
#                 paste0(sRawCSVPath, 'DF_lines_',
#                        formatC(tmp.files.count, width = 3, format = 'd',
#                                flag = '0'), '.csv')
#             write.csv2(df.lines.notices, tbl.name, row.names = F)
# 
#             msg <- paste0('Временный файл ', tbl.name, ' сохранён.')
#             uf.write.to.log(msg, log.parse.XML.filename)
# 
#             df.lines.notices <- NULL
#         }
# 
#         message(paste0(j, ' ...', sub(f, pattern = '^.*/', replacement = ''),
#                        '... ', round(i.files.сount / n * 100, 1), '% done'))
# 
#         console.clean.count <- console.clean.count + 1
#         if (console.clean.count == iMaxConsoleStatusLines) {
#             cat("\014")
#             console.clean.count <- 0
#         }
#     }
# 
#     # сохраняем на диск последний кусок таблицы
#     if (!is.null(df.lines.notices)) {
#         tmp.files.count <- tmp.files.count + 1
#         tbl.name <-
#             paste0(sRawCSVPath, 'DF_lines_',
#                    formatC(tmp.files.count, width = 3, format = 'd', flag = '0'),
#                    '.csv')
#         write.csv2(df.lines.notices, tbl.name, row.names = F)
# 
#         df.lines.notices <- NULL
#     }
# 
#     # сообщения в консоль и в лог
#     msg <- paste0('Закончили разбор файлов ', filePrefix, ': ', Sys.time())
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     # имена временных файлов
#     tmp.files <- paste0(sRawCSVPath, 'DF_lines_',
#                         formatC(1:tmp.files.count, width = 3, format = 'd',
#                                 flag = '0'), '.csv')
# 
#     # читаем первую из временных таблиц
#     DT.all <- data.table(read.csv2(tmp.files[1], colClasses = rep('character', n.cols),
#                                    stringsAsFactors = F))
# 
#     # читаем остальные временные таблицы
#     for (flnm in tmp.files[-1]) {
#         DT.all <- rbind(DT.all, data.table(read.csv2(flnm, colClasses = rep('character',
#                                                                             n.cols),
#                                                      stringsAsFactors = F)))
#     }
# 
#     # записываем общую таблицу
#     write.csv2(DT.all, paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
#                row.names = F)
# 
#     # убираем временные таблицы из памяти и с диска
#     rm(DT, DT.all)
#     file.remove(tmp.files)
# 
#     # сообщения в консоль и в лог
#     msg <- paste0('Файл ', paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
#                   ' сохранён.')
#     uf.write.to.log(msg, log.parse.XML.filename)
# 
#     msg <- paste0('Временные файлы удалены.')
#     uf.write.to.log(msg, log.parse.XML.filename)
# }



# 4. РАБОТА С RAW-ТАБЛИЦАМИ ----------------------------------------------------


# Извлекаем данные для географии заказчиков и поставщиков ======================

# #  * Работаем с таблицей объявлений ############################################
# # имя файла с результатами парсинга XML (не нормализованная таблица)
# flnm <- paste0(sRawCSVPath, 'DT_fcsNotificationEA44.csv')
# 
# # грузим
# DT.notif <- data.table(read.csv2(flnm, stringsAsFactors = F,
#                                 na.strings = c('NA', '<NA>'),
#                                 # encoding = 'UTF-8',
#                                 colClasses = rep('character', 34)))
# # проверка
# dim(DT.notif)
# DT.notif
# colnames(DT.notif)

# DT.notif.with.orgs <- unique(DT.notif[, c('fcsNotificationEF.purchaseNumber',
#                                           'responsibleOrg.regNum'), with = F])
# prod(substr(DT.notif.with.orgs$fcsNotificationEF.purchaseNumber, 1, 11) == 
#          DT.notif.with.orgs$responsibleOrg.regNum)


# # ............................................................................
# #
# # ТОВАРЫ
# # ............................................................................
# #
# columns.to.search <- c('fcsNotificationEF.purchaseNumber',
#                        'purchaseObject.OKPD2.code', 'purchaseObject.OKPD2.name')
# all.OKPD2 <- unique(data.table(DT.notif[, columns.to.search, with = F]))
# dim(all.OKPD2)
# 
# all.OKPD2 <- 
#     uf.process.large.table.normalising(all.OKPD2, 
#                                        out.total.table.name = 'DT_all_OKPD2_clean.csv')
# 
# all.OKPD2 <- unique(data.table(all.OKPD2))
# all.OKPD2[, purchaseObject.OKPD2.code.2dig :=
#               gsub(all.OKPD2$purchaseObject.OKPD2.code, pattern = '[.].*$',
#                    replacement = '')]
# 
# write.csv2(all.OKPD2, paste0(sRawCSVPath, 'DT_all_OKPD2_clean.csv'), row.names = F)
# cls <- sapply(all.OKPD2, class)
# nms <- names(cls)
# df <- data.frame(col.names = nms, col.classes = cls)
# rownames(df) <- 1:nrow(df)
# write.csv2(df, paste0(sRawCSVPath, 'DT_all_OKPD2_clean_META.csv'),
#            row.names = F)
# 
#
# # ............................................................................
# #
# # ОКПД (ОТРАСЛИ)
# # ............................................................................
# OKPD.industries <- sort(unique(all.OKPD2$purchaseObject.OKPD2.code.2dig))
# write.csv2(data.frame(OKPD2.2dig = OKPD.industries),
#            paste0(sRefPath, 'df_all_OKPD2_industries.csv'), row.names = F)


# # ............................................................................
# #
# # ЗАКАЗЧИКИ
# # ............................................................................
# #
# columns.to.search <- c('responsibleOrg.regNum', 'responsibleOrg.fullName',
#                        'responsibleOrg.postAddress')
# all.orgs <-
#     unique(data.table(DT.notif[, columns.to.search, with = F]))
# 
# # есть записи с расхождениями в почтовом индексе; оставляем только уникальные id
# all.orgs <- all.orgs[order(responsibleOrg.regNum), ]
# n <- nrow(all.orgs)
# flags <- c(T, rep(NA, n - 1))
# for (i in 2:n) {
#     flags[i] <- all.orgs$responsibleOrg.regNum[i] != 
#         all.orgs$responsibleOrg.regNum[i - 1]
# }
# all.orgs <- all.orgs[flags, ]
# 
# # делаем адреса и узнаём координаты через Яндекс, потому что это единственный 
# #  рабочий вариант
# all.orgs <- 
#     uf.process.addresses.to.coords(all.orgs, 
#                                    col.address = 'responsibleOrg.postAddress',
#                                    col.coords.prefix = 'responsibleOrg.',
#                                    out.file.name = paste0(sRawCSVPath, 
#                                                           'df_all_orgs_long_lat.csv'))
# # обрабатываем пропуски
# DT <- all.orgs[is.na(responsibleOrg.long), ]
# n <- nrow(DT)
# for (i in 1:n) {
#     addr <- DT[i, ]$responsibleOrg.postAddress
#     longlat <- uf.get.coords.Yandex(addr)
#     
#     DT[i, ]$responsibleOrg.lat <- as.numeric(longlat$lat)
#     DT[i, ]$responsibleOrg.long <- as.numeric(longlat$long)
# }
# all.orgs <- all.orgs[!is.na(responsibleOrg.long), ]
# all.orgs <- rbind(all.orgs, DT)
# 
# # ВЫВОД: неструктурированные адреса вообще не надо форматировать, 
# #  Яндекс скушает их и так
# 
# all.orgs[is.na(responsibleOrg.lat), ]
# longlat <- data.frame(long = 49.306991, lat = 55.933026)
# all.orgs[responsibleOrg.regNum == '01113000013', 
#          responsibleOrg.lat := as.numeric(longlat$lat)]
# all.orgs[responsibleOrg.regNum == '01113000013', 
#          responsibleOrg.long := as.numeric(longlat$long)]
# 
# # сохраняем таблицу с координатами
# write.csv2(all.orgs, paste0(sRefPath, 'df_all_orgs_long_lat.csv'), row.names = F)


# # ............................................................................
# #
# # УЧАСТНИКИ АУКЦИОНОВ
# # ............................................................................
# #
# #  * Работаем с таблицей протоколов этапа 1 ####################################
# protocols.EF1 <-
#     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF1.csv'),
#                          stringsAsFactors = F,
#                          colClasses = rep('character', 5)))
# protocols.EF1 <- unique(protocols.EF1)
# colnames(protocols.EF1)[colnames(protocols.EF1) == 'fcsProtocolEF1.purchaseNumber'] <-
#     'purchaseNumber'
# protocols.EF1
# 
# # проверка:
# #  строк (уникальных id) в файле должно быть столько же...
# nrow(protocols.EF1)
# #  ...сколько уникальных id протоколов 01
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$id.fcsProtocolEF1))

# # расклеиваим ячейки (файл записывается в csv автоматом)
# DT.protocols01 <-
#     uf.process.large.table.normalising(protocols.EF1[, c('purchaseNumber',
#                                                          'fcsProtocolEF1.id',
#                                                          'protocolDate',
#                                                          'application.journalNumber',
#                                                          'admitted'), with = F],
#                                        out.total.table.name = 'DT_fcsProtocolEF1_clean.csv')

# DT.protocols01 <- data.table(read.csv2(paste0(sRawCSVPath, 
#                                               'DT_fcsProtocolEF1_clean.csv'),
#                                        stringsAsFactors = F,
#                                        colClasses = rep('character', 5)))
# 
# # проверка
# length(unique(DT.protocols01$purchaseNumber))
# length(na.omit(unique(DT.protocols01$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber))
# 
# # во всех нижеследующих заявках процедура аукциона обрывается из-за различных 
# #  ошибок: не подано ни одной заявки, например
# all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber)
# missed.ids.01 <- all.ids.in.index[!(all.ids.in.index %in% 
#                                         na.omit(unique(DT.protocols01$purchaseNumber)))]
# 
# 
# # делаем номера заявок числами
# DT.protocols01[, application.journalNumber 
#                := as.numeric(application.journalNumber)]
# 
# # делаем логический столбец логическим
# DT.protocols01[admitted != 'true', admitted := 'false']
# DT.protocols01[, admitted := as.logical(toupper(admitted))]
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols01[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols01$protocolDate, pattern = 'T', 
#                                    replacement = ' '))]
# DT.protocols01[, protocolDate := NULL]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols01)[colnames(DT.protocols01) == 'protocolDate_POSIXct'] <- 
#     'protocol01Date_POSIXct'
# 
# # результаты
# str(DT.protocols01)
# dim(DT.protocols01)
# DT.protocols01
# 
#
# #  * Работаем с таблицей протоколов этапа 2 ####################################
# # protocols.EF2 <-
# #     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF2.csv'),
# #                          stringsAsFactors = F,
# #                          colClasses = rep('character', 9)))
# # protocols.EF2 <- unique(protocols.EF2)
# # colnames(protocols.EF2)[colnames(protocols.EF2) == 'application.journalNumber..priceOffers.'] <-
# #     'application.journalNumber'
# # protocols.EF2
# 
# # # расклеиваим ячейки
# # DT.protocols02 <-
# #     uf.process.large.table.normalising(protocols.EF2[, c('purchaseNumber',
# #                                                          'fcsProtocolEF2.id',
# #                                                          'protocolDate',
# #                                                          'application.journalNumber',
# #                                                          'application.firstOffer.price',
# #                                                          'application.lastOffer.price',
# #                                                          'application.priceOffers.offersQuantity'), with = F],
# #                                        out.total.table.name = 'DT_fcsProtocolEF2_clean.csv')
# 
# DT.protocols02 <- data.table(read.csv2(paste0(sRawCSVPath, 
#                                               'DT_fcsProtocolEF2_clean.csv'),
#                                        stringsAsFactors = F,
#                                        colClasses = rep('character', 5)))
# 
# # проверка
# length(unique(DT.protocols02$purchaseNumber))
# length(na.omit(unique(DT.protocols02$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber))
# 
# # во всех нижеследующих заявках процедура аукциона обрывается из-за различных 
# #  ошибок: не подано ни одной заявки, например
# all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber)
# missed.ids.02 <- all.ids.in.index[!(all.ids.in.index %in% 
#                                         na.omit(unique(DT.protocols02$purchaseNumber)))]
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols02[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols02$protocolDate, 
#                                    pattern = 'T', replacement = ' '))]
# DT.protocols02[, protocolDate := NULL]
# 
# # преобразовываем цены и кол-ва в числа
# DT.protocols02[, application.firstOffer.price := 
#                    as.numeric(application.firstOffer.price)]
# DT.protocols02[, application.lastOffer.price := 
#                    as.numeric(application.lastOffer.price)]
# DT.protocols02[, application.priceOffers.offersQuantity := 
#                    as.numeric(application.priceOffers.offersQuantity)]
# 
# # делаем номера заявок числами
# DT.protocols02[, application.journalNumber 
#                := as.numeric(application.journalNumber)]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols02)[colnames(DT.protocols02) == 'protocolDate_POSIXct'] <- 
#     'protocol02Date_POSIXct'
# 
# # результаты
# str(DT.protocols02)
# dim(DT.protocols02)
# DT.protocols02
# 
# 
# # ПРОБЛЕМА
# #  протоколы оказываются друг с другом не связаны по ключам
# #  одному notice ID может соответствовать до кучи протоколов
# #  Решение -- связывать их по ближайшим датам.
# 
# 
# #  * Работаем с таблицей протоколов этапа 3 ####################################
# # protocols.EF3 <-
# #     data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF3.csv'),
# #                          stringsAsFactors = F,
# #                          colClasses = rep('character', 9)))
# # protocols.EF3 <- unique(protocols.EF3)
# # protocols.EF3
# 
# # # расклеиваим ячейки
# # DT.protocols03 <-
# #     uf.process.large.table.normalising(protocols.EF3[, c('purchaseNumber',
# #                                                          'fcsProtocolEF3.id',
# #                                                          'protocolDate',
# #                                                          'applications.journalNumber',
# #                                                          'applications.inn',
# #                                                          'application.appRating',
# #                                                          'applications.countryFullName',
# #                                                          'applications.postAddress'), with = F],
# #                                        out.total.table.name = 'DT_fcsProtocolEF3_clean.csv')
# 
# DT.protocols03 <- data.table(read.csv2(paste0(sRawCSVPath,
#                                               'DT_fcsProtocolEF3_clean.csv'),
#                                        stringsAsFactors = F,
#                                        colClasses = rep('character', 8)))
# 
# # проверка
# length(unique(DT.protocols03$purchaseNumber))
# length(na.omit(unique(DT.protocols03$purchaseNumber)))
# length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF3), ]$purchaseNumber))
# 
# # преобразовываем в дату-время столбец protocolDate
# DT.protocols03[, protocolDate_POSIXct :=
#                    as.POSIXct(gsub(DT.protocols03$protocolDate, pattern = 'T', 
#                                    replacement = ' '))]
# DT.protocols03[, protocolDate := NULL]
# 
# # смотрим сведения о стране
# table(DT.protocols03$applications.countryFullName)
# ### Российская Федерация 
# ###                33395
# DT.protocols03[, applications.countryFullName := NULL]
# 
# # переименовываем столбцы для дальнейшего связывания таблиц
# colnames(DT.protocols03)[colnames(DT.protocols03) == 'applications.journalNumber'] <- 
#     'application.journalNumber'
# colnames(DT.protocols03)[colnames(DT.protocols03) == 'protocolDate_POSIXct'] <- 
#     'protocol03Date_POSIXct'
# 
# # делаем номера заявок числами
# DT.protocols03[, application.journalNumber := as.numeric(application.journalNumber)]
# 
# # разбираемся с application.appRating
# table(DT.protocols03$application.appRating)
# DT.protocols03[, application.appRating := as.numeric(application.appRating)]
# DT.protocols03 <- na.omit(DT.protocols03)
# 
# # результаты
# str(DT.protocols03)
# dim(DT.protocols03)
# DT.protocols03
# 
# 
# # совмещаем информацию об участниках аукционов /////////////////////////////////
# dim(DT.protocols01)
# dim(DT.protocols02)
# dim(DT.protocols03)
# length(unique(DT.protocols01$purchaseNumber))
# length(unique(DT.protocols02$purchaseNumber))
# length(unique(DT.protocols03$purchaseNumber))
# 
# # совмещаем таблицы
# DT.all.EA <- merge(DT.protocols01, DT.protocols02, 
#                    by = c('purchaseNumber', 'application.journalNumber'))
# DT.all.EA <- merge(DT.all.EA, DT.protocols03, 
#                    by = c('purchaseNumber', 'application.journalNumber'))
# DT.all.EA
# 
# write.csv2(DT.all.EA, paste0(sRawCSVPath, 'DT_all_auctions_clean.csv'),
#            row.names = F)
# 
# # всего заявок
# length(unique(DT.all.EA$purchaseNumber))

# # все уникальные участники
# length(unique(DT.all.EA$applications.postAddress))
# all.participants.addr <- unique(DT.all.EA[, applications.inn, applications.postAddress])
# all.participants.addr <- uf.process.addresses.to.coords(all.participants.addr,
#                                                         col.address = 'applications.postAddress',
#                                                         col.coords.prefix = 'participant.',
#                                                         out.file.name = paste0(sRefPath, 'dt_all_participants_long_lat.csv'))



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


