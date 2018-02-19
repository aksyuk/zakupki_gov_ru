#
# Парсинг содержимого необъятного ftp сервера, который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
#

library('RCurl')
library('XML')

# ## имя браузера для маскировки запроса по getURL()
# userAgent <- 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'
# 
# ## имя файла для экспорта
# exportCsvFileName <- paste0('zakupki_gov_ru_', Sys.time(), '.csv')

url <- 'ftp://ftp.zakupki.gov.ru/'
userpwd <- 'free:free'

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
                            region.folders.names, '/contracts/')

df.summary.contracts <- data.frame(region = region.folders.names,
                                   number_of_contracts_2017 = 0)

if (!file.exists('./data/raw')) {
    dir.create('./data/raw')
}

# выгрузка xml в архивах в папку './data/raw/' в рабочей директории
for (iRegion in 1:length(region.folder.url)) {
    zips <- unlist(strsplit(getURL(region.folder.url[iRegion],  
                                   ftp.use.epsv = FALSE, 
                                   dirlistonly = TRUE, userpwd = userpwd),
                            '\r\n'))
    # берём заявки, открытые в 2018 году
    zips <- grep(zips, pattern = '_2018.*_.*_.*', value = T)

    for (iContract in 1:length(zips)) {
        download.file(paste0('ftp://', userpwd, '@',
                             gsub('^.*://', '', region.folder.url[iRegion]),
                             zips[iContract]),
                      destfile = paste0('./data/raw/', zips[iContract]))        
    }
}

# залезаем в дебри архивов
#  индексируем всё это богатство
path <- './data/raw/2018_19-02-2018/'
xmlzips <- dir(path)
n <- length(xmlzips)

for (iXMLzipCount in 1:length(xmlzips)) {
    
    xml <- unzip(paste0(path, xmlzips[iXMLzipCount]), 
                 exdir = paste0(path, '/tmp'))
    rootnode <- xmlTreeParse()
    
    
    message(paste0('Архив ', iXMLzipCount, ' из ', n, 
                   ' (', round(iXMLzipCount / n * 100, 1), '%)'))
}



