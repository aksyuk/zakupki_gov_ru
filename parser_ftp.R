#
# Парсинг содержимого необъятного ftp сервера, который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
#

library('RCurl')

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

for (iRegion in 1:length(region.folder.url)){
    zips <- unlist(strsplit(getURL(region.folder.url,  ftp.use.epsv = FALSE, 
                                   dirlistonly = TRUE, userpwd = userpwd),
                            '\r\n'))
    for (iContract in 1:length(zips)) {
        # считаем количество контрактов, заключённых в 2017 году
        
    }
}




