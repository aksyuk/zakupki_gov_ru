#
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
path <- paste0('./data/raw/', YEAR, '_',
                format(Sys.time(), '%d-%m-%Y'), '/')
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
#  индексируем всё это богатство
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

# отбираем только xml с контрактами
xmls <- grep(dir(tmppath), pattern = '^contract.*xml$', value = T)

# шаг 2: сделать рыбу таблицы
ls.xmls <- list(0)
i.ls.xmls <- 1
excluded.single <- NULL
excluded.cancel <- 0

# парсим xml-файлы
# ВНИМАНИЕ: это очень длинный цикл!
for (xml.file in xmls[1:10]) {
    # шаг 3: запарсить распакованный xml
    rootnode <- xmlTreeParse(paste0(tmppath, '/', xml.file), 
                             useInternalNodes = T)

    # танцы с namespace
    nsDefs <- xmlNamespaceDefinitions(rootnode)
    ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
    names(ns)[1] <- "xmlns"
                
    # отсев XML по типу записей
    #  выкидываем закупки у единственного поставщика
    lst <- getNodeSet(rootnode, '//xmlns:singleCustomer', ns)
    if (length(lst) != 0) {
        excluded.single <- c(excluded.single, sapply(lst, xmlValue))
        next
    }
    # выкидываем отменённые закупки тоже, считаем их
    if (length(getNodeSet(rootnode, 
                          '//xmlns:executions//xmlns:cancelledProcedureId', 
                          ns)) != 0) {
        excluded.cancel <- excluded.cancel + 1
        next
    }
        
    # считываем теги
    tags <- getNodeSet(rootnode, xpath.patterns, ns)
        
    # имена родительских тегов для правильного формирования имён столбцов
    parent.names <- xpathSApply(rootnode, xpath.patterns, function(x){
        xmlName(xmlParent(x))
        }, namespaces = ns)
        
    df.xmls.line <- as.data.frame(lapply(tags, xmlValue), 
                                  stringsAsFactors = F)
    colnames(df.xmls.line) <- 
        paste(parent.names, sapply(tags, xmlName), sep = '.')
    
    # шаг 4: найти связанные xml с протоколами
    notice.id <- substr(xml.file, gregexpr('_', xml.file)[[1]][1] + 1, 
                        gregexpr('_', xml.file)[[1]][2] - 1)
    
    xml.related <- grep(dir(tmppath), pattern = paste0(notice.id, '.*xml'), 
                        value = T)
    xml.related <- xml.related[!xml.file %in% xml.related]
    
    if (length(xml.related) > 0) {
        df.xmls.line$xmls.related <- paste(xml.related, collapse = '#')
    } else {
        df.xmls.line$xmls.related <- NA
    }
    
    # записываем значения во фрейм
    ls.xmls[[i.ls.xmls]] <- df.xmls.line
    names(ls.xmls)[i.ls.xmls] <- paste0('notice.id:', notice.id)
    i.ls.xmls <- i.ls.xmls + 1
    
    message(paste0(notice.id, ' ... ', 
                   round(which(xmls == xml.file) / length(xmls) * 100, 1),
                   '% done'))
}
    




# шаг 6: экспортировать в csv
message(paste0('Строк в ', sXMLzip, ': ', nrow(df.xmls), ''))
if (!is.null(df.xmls)) {
    write.csv(df.xmls, 
              file = paste0('./data/', 
                            gsub(sXMLzip, pattern = '.xml.zip', 
                            replacement = ''), '.csv'), row.names = F)
}

# шаг 5: удалить содержимое папки tmp
rem.status <- file.remove(paste0(tmppath, '/', dir(tmppath)))
    
# шаг 6: сообщение в консоль
message(paste0('Архив ', which(xmlzips == sXMLzip), ' из ', length(xmlzips), 
               ' (',  
               round(which(xmlzips == sXMLzip) / length(xmlzips) * 100, 1),
               '%)'))

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


