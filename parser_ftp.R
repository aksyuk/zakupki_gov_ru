# Парсинг содержимого необъятного ftp сервера, который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
#

library('RCurl')
library('XML')
library('dplyr')

# ## имя браузера для маскировки запроса по getURL()
# userAgent <- 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'
# 
# ## имя файла для экспорта
# exportCsvFileName <- paste0('zakupki_gov_ru_', Sys.time(), '.csv')

url <- 'ftp://ftp.zakupki.gov.ru/'
userpwd <- 'free:free'

# год, за который грузим документацию
YEAR <- 2018

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
#  регион: Московская область
region.folder.url <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            region.folders.names, '/')

df.summary.contracts <- data.frame(region = region.folders.names,
                                   number_of_contracts_2017 = 0)

if (!file.exists('./data/raw')) {
    dir.create('./data/raw')
}

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')

# папка для загрузки в /data/raw
# path <- paste0('./data/raw/', YEAR, '_',
#                 format(Sys.time(), '%d-%m-%Y'), '/')
path <- paste0('./data/raw/', YEAR, '_26-03-2018', '/')
if (!dir.exists(path)) {
    dir.create(path)
}

# выгрузка xml в архивах в папку './data/raw/' в рабочей директории
for (sRegion in region.folder.url[1]) {
    
    for (sSubf in sSubfolders) {
        zips <- unlist(strsplit(getURL(paste0(sRegion, sSubf),  
                                       ftp.use.epsv = FALSE, 
                                       dirlistonly = TRUE, userpwd = userpwd),
                                '\r\n'))
        # берём заявки, открытые в YEAR году
        zips <- grep(zips, pattern = paste0('_', YEAR, '.*_.*_.*'), value = T)
        
        for (sZip in zips) {
            download.file(paste0('ftp://', userpwd, '@',
                                 gsub('^.*://', '', sRegion), sSubf, sZip),
                          destfile = paste0(path, sZip))        
        }
    }
}

# залезаем в дебри архивов
xmlzips <- dir(path)
tmppath <- paste0(path, 'tmp')
if (!dir.exists(tmppath)) {
    dir.create(tmppath)
}

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

# распаковываем архивы в tmp
for (sXMLzip in xmlzips) {
    # шаг 1: распаковать архив в tmp. там до кучи xml-файлов
    unzip(paste0(path, sXMLzip), exdir = tmppath)
}

# индексация: считаем файлы, которые относятся к уникальным ID извещений
all.xmls <- grep(dir(tmppath), pattern = 'xml$', value = T)
# уникальные номера измещений
all.ids <- gsub(all.xmls, pattern = '^.*?_', replacement = '')
all.ids <- gsub(all.ids, pattern = '_.*', replacement = '')
all.ids <- unique(all.ids)
length(all.ids)
# все префиксы файлов
prefixes <- unique(gsub(all.xmls, pattern = '_.*', replacement = ''))
length(prefixes)
df.file.index <- 
    as.data.frame(matrix(rep(0, length(prefixes) * length(all.ids)), 
                         nrow = length(all.ids), ncol = length(prefixes)))
colnames(df.file.index) <- prefixes
df.file.index <- cbind(data.frame(noticeID = all.ids,
                                  stringsAsFactors = F), df.file.index)
n <- length(all.ids)
i <- 0
for (id in all.ids) {
    flnms <- grep(all.xmls, pattern = id, value = T)
    flnms <- table(gsub(flnms, pattern = '_.*', replacement = ''))
    df.file.index[df.file.index$noticeID == id, names(flnms)] <- flnms
    i <- i + 1
    message(paste0(id, ' ... ', round(i / n * 100, 1), '%'))
}

nrow(df.file.index)
sapply(df.file.index[, -1], function(x){sum(x > 0)})
nrow(df.file.index)
round(sapply(df.file.index[, -1], 
             function(x){sum(x > 0)}) / nrow(df.file.index), 3)


# YOU ARE HERE -----------------------------------------------------------------
# !!! если грузить так, то таблица нулевая, потому что между извещениями
#     и конкурсами проходит много времени. Надо расширить интервал сбора данных
# !!!
# берём все электронные аукционы, полностью попавшие в интервал рассмотрения 
#   и завершившиеся заключением контракта
loop.ids <- df.file.index[df.file.index$fcsNotificationEA44 > 0 &
                              df.file.index$fcsNotificationEP44 > 0 & 
                              df.file.index$fcsProtocolEF1 > 0 & 
                              df.file.index$fcsProtocolEF2 > 0 & 
                              df.file.index$fcsProtocolEF3 > 0, ]$noticeID
length(loop.ids)
# добавляем к ним все завершённые открытые конкурсы
loop.ids <- c(loop.ids, 
              df.file.index[df.file.index$fcsProtocolOK1 > 0 & df.file.index$fcsProtocolOK2 > 0, ]$noticeID)
# добавляем процедуры с единственной заявкой и с единственным участником
loop.ids <- c(loop.ids,
              df.file.index[df.file.index$fcsProtocolEFSingleApp > 0, ])
loop.ids <- c(loop.ids,
              df.file.index[df.file.index$fcsProtocolEFSinglePart > 0, ])
loop.ids <- unique(loop.ids)
length(loop.ids)

# таблица со всеми контрагентами
df.kon <- data.frame(inn = '', kpp = '', organizationName = '', 
                     stringsAsFactors = F)
# таблица с закупками
df.zak <- data.frame(purchaseNumber = '', customer_inn = '',
                     stringsAsFactors = F)
# таблица с заявками поставщиков-участников торгов
df.zay <- 

# парсим файлы с аукционами (цикл по номерам извещений)
for (id in loop.ids) {
    
}





