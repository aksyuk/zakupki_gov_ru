# отбираем только xml с контрактами
xmls <- grep(dir(tmppath), pattern = '^contract.*xml$', value = T)

# шаг 2: сделать рыбу таблицы
#   выгрузку по каждой заявке бросаем в список, т.к. столбцы не всегда совпадают
ls.xmls <- list(0)
#   номер элемента списка
i.ls.xmls <- 1
#   счётчик: сколько исключили аукционов с одним поставщиков
excluded.single <- NULL
#   причины отмены контрактов
excluded.cancel <- NULL
# флаги для того, чтобы выбрасывать связанные документы

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
    xml.related <- xml.related[xml.related != xml.file]
    
    if (length(xml.related) > 0) {
        df.xmls.line$xmls.related <- paste(xml.related, collapse = '#')
    } else {
        df.xmls.line$xmls.related <- NA
    }
    
    # шаг 5: записываем значения в список
    ls.xmls[[i.ls.xmls]] <- df.xmls.line
    names(ls.xmls)[i.ls.xmls] <- paste0('notice.id:', notice.id)
    i.ls.xmls <- i.ls.xmls + 1
    
    message(paste0(notice.id, ' ... ', 
                   round(which(xmls == xml.file) / length(xmls) * 100, 1),
                   '% done'))
}


# шаг 6: экспортировать таблицу в csv
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

prot.xmls <- grep(dir('./data/raw/2018_26-03-2018/tmp/'), 
                  pattern = 'fcsProtocol.*', value = T)
table(substr(prot.xmls, 14, 14))
prot.xmls[substr(prot.xmls, 14, 14) == 'A']