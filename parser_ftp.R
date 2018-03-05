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
tmppath <- paste0(path, 'tmp')
n <- length(xmlzips)

# ВНИМАНИЕ: это очень длинный цикл!
for (iXMLzipCount in 1:length(xmlzips)) {
    
    # шаг 1: распаковать архив в tmp. там до кучи xml-файлов
    unzip(paste0(path, xmlzips[iXMLzipCount]), exdir = tmppath)
    xmls <- grep(dir(tmppath), pattern = 'xml$', value = T)

    # шаг 2: сделать рыбу таблицы
    df.xmls <- NULL
    df.xmls.line <- 
        data.frame(id = '', publishDate = '', notificationNumber = '',
                   currentContractStage = '',
                   customer.name = '', customer.inn = '',
                   OKPD2.code = '', OKPD2.name = '',
                   OKEI.code = '', OKEI.fullName = '', 
                   quantity = 0, sumRUR = 0,
                   supplier.name = '', supplier.inn = '', 
                   supplier.OKTMO.code = '', supplier.OKTMO.name = '',
                   stringsAsFactors = F)
    
    for (j in 1:length(xmls)) {
        # шаг 3: запарсить распакованный xml
        rootnode <- xmlTreeParse(paste0(tmppath, '/', xmls[j]), 
                                 useInternalNodes = T)

        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootnode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
        names(ns)[1] <- "xmlns"
                
        # отсев XML по типу записей
        ls <- getNodeSet(rootnode, "//xmlns:singleCustomer|//xmlns:executions|//xmlns:cancelledProcedureId", 
                         ns)
        if (length(ls) != 0) break
        
        # считываем теги
        tag <- getNodeSet(rootnode, "//xmlns:id", ns)
        df.xmls.line[1, 'id'] <- ifelse(length(tag) == 0, NA, 
                                        xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:publishDate", ns)
        df.xmls.line[1, 'publishDate'] <- ifelse(length(tag) == 0, NA, 
                                                 xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:notificationNumber", ns)
        df.xmls.line[1, 'notificationNumber'] <- ifelse(length(tag) == 0, NA, 
                                                        xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:currentContractStage", ns)
        df.xmls.line[1, 'currentContractStage'] <- ifelse(length(tag) == 0, NA,
                                                          xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:customer//xmlns:fullName", ns)
        df.xmls.line[1, 'customer.name'] <- ifelse(length(tag) == 0, NA,
                                                   xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:customer//xmlns:inn", ns)
        df.xmls.line[1, 'customer.inn'] <- ifelse(length(tag) == 0, NA,
                                                  xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:OKPD2/xmlns:code", ns)
        df.xmls.line[1, 'OKPD2.code'] <- ifelse(length(tag) == 0, NA,
                                                xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:OKPD2/xmlns:name", ns)
        df.xmls.line[1, 'OKPD2.name'] <- ifelse(length(tag) == 0, NA,
                                                xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:OKEI/xmlns:code", ns)
        df.xmls.line[1, 'OKEI.code'] <- ifelse(length(tag) == 0, NA,
                                               xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:OKEI/xmlns:fullName", ns)
        df.xmls.line[1, 'OKEI.name'] <- ifelse(length(tag) == 0, NA,
                                               xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:quantity", ns)
        df.xmls.line[1, 'quantity'] <- ifelse(length(tag) == 0, NA,
                                              xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:product/xmlns:sumRUR", ns)
        df.xmls.line[1, 'sumRUR'] <- ifelse(length(tag) == 0, NA,
                                            xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:suppliers//xmlns:fullName", ns)
        df.xmls.line[1, 'supplier.name'] <- ifelse(length(tag) == 0, NA,
                                                   xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:suppliers//xmlns:INN", ns)
        df.xmls.line[1, 'supplier.inn'] <- ifelse(length(tag) == 0, NA,
                                                  xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:suppliers//xmlns:OKTMO/xmlns:code", ns)
        df.xmls.line[1, 'supplier.OKTMO.code'] <- ifelse(length(tag) == 0, NA,
                                                         xmlValue(tag[[1]]))
        tag <- getNodeSet(rootnode, "//xmlns:suppliers//xmlns:OKTMO/xmlns:name", ns)
        df.xmls.line[1, 'supplier.OKTMO.name'] <- ifelse(length(tag) == 0, NA,
                                                         xmlValue(tag[[1]]))
        
        # записываем значения во фрейм
        df.xmls <- rbind(df.xmls, df.xmls.line)
    }
    
    # шаг 4: экспортировать в csv
    message(paste0('Строк в ', xmlzips[iXMLzipCount], ': ', nrow(df.xmls), ''))
    if (nrow(df.xmls) > 0) {
        write.csv(df.xmls, 
                  file = paste0('./data/', 
                                gsub(xmlzips[iXMLzipCount], pattern = '.xml.zip', 
                                     replacement = ''), 
                                '.csv'), row.names = F)
    }

    # шаг 5: удалить содержимое папки tmp
    rem.status <- file.remove(paste0(tmppath, '/', dir(tmppath)))
    
    # шаг 6: сообщение в консоль
    message(paste0('Архив ', iXMLzipCount, ' из ', n, 
                   ' (', round(iXMLzipCount / n * 100, 1), '%)'))
}

###  Архив 211 из 1345 (15.7%)
### Строк в contract_Evrejskaja_Aobl_2018010100_2018020100_001.xml.zip: 



# поудалять пустые csv их папки ./data
flnms <- grep(dir('./data'), pattern = 'csv', value = T)
flnms <- paste0('./data/', flnms)
df.all <- NULL
for (f in flnms) {
    tmp <- tryCatch({
        if (file.size(f) > 0) {
            read.csv(f, stringsAsFactors = F,na.strings = c('NA', '<NA>'))
        }
    }, error = function(err) {
        # error handler picks up where error was generated
        print(paste("Read.table didn't work!:  ", err))
        file.remove(f)
    })
    df.all <- rbind(df.all, tmp)
}
df.all
unique(df.all$customer.name)
unique(df.all$OKPD2.name)

# организации без казённых, муниципальных и государственных
org <- unique(df.all$customer.name)
org[!grepl('КАЗЕНН|ГОСУДАРСТВЕН|МИНИСТЕРСТВО|АДМИНИСТРАЦ|МУНИЦИПАЛ|БЮДЖЕТН|УПРАВЛЕНИЕ|ИНСПЕКЦИЯ', 
           org)]


