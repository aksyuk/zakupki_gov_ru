# Парсинг содержимого необъятного ftp сервера, который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
#

library('RCurl')
library('XML')
library('dplyr')
library('reshape')
library('data.table')
library('stringi')
library('ggplot2')

# options("scipen" = 100, "digits" = 4)


# ## имя браузера для маскировки запроса по getURL()
# userAgent <- 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'
# 
# ## имя файла для экспорта
# exportCsvFileName <- paste0('zakupki_gov_ru_', Sys.time(), '.csv')

url <- 'ftp://ftp.zakupki.gov.ru/'
userpwd <- 'free:free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
YEAR <- c('201709', '201710', '201711', '201712', '201801', '201802', '201803',
          '201804', '201805')

# список директорий с регионами
region.folders.names <- unlist(strsplit(getURL(paste0(url, 'fcs_regions/'), 
                                               ftp.use.epsv = FALSE, 
                                               dirlistonly = TRUE, 
                                               userpwd = userpwd),
                                        '\r\n'))
region.folders.names <- region.folders.names[grep('_Resp$|_kraj$|_.?obl$', 
                                                  region.folders.names)]
length(region.folders.names)

# список файлов в папке
region.folder.url <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            region.folders.names, '/')

#  регион: Татарстан
region.folder.url[66]

if (!file.exists('./data/raw')) {
    dir.create('./data/raw')
}

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')

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
#                 download.file(paste0('ftp://', userpwd, '@',
#                                      gsub('^.*://', '', sRegion), sSubf, sZip),
#                               destfile = paste0(path, sZip))
#             }
#         }
#     }
# }

# залезаем в дебри архивов
xmlzips <- dir(path)
tmppath <- paste0(path, 'tmp')
if (!dir.exists(tmppath)) {
    dir.create(tmppath)
}

# распаковываем архивы в tmp
# оооочень долго!
# length(xmlzips)
# iCount <- 0
# for (sXMLzip in xmlzips) {
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



df.file.index <- df.file.index[, 1:4]
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
df.zay <- 










