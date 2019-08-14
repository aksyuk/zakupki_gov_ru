#
# Мне нужно понять, почему получается такая фигня с данными:
#  * протоколы не складываются в цепочки
#  * не могу найти файлов по notificationID
#  * не могу найти id протокола из файла в таблице с результатами парсинга.
#

DT.protocols01[purchaseNumber == '311300039417000192' & 
                   protocol01Date_POSIXct == '2017-10-17 09:17:58', ]
DT.protocols02[purchaseNumber == '311300039417000192' & 
                   protocol02Date_POSIXct == '2017-10-20 10:32:29', ]
DT.protocols03[purchaseNumber == '311300039417000192' & 
                   protocol03Date_POSIXct == '2017-10-23 09:09:58', ]

# grep(dir(sRawXMLPath), pattern = '311300039417000192.*[.]xml', value = T)
### [1] "fcsContractSign_0311300039417000192_10987573.xml"     "fcsNotificationEA44_0311300039417000192_13986682.xml"
### [3] "fcsPlacementResult_0311300039417000192_7811314.xml"   "fcsProtocolEF1_0311300039417000192_15836607.xml"     
### [5] "fcsProtocolEF2_0311300039417000192_15871273.xml"      "fcsProtocolEF3_0311300039417000192_15892648.xml"    

DT <- protocols.EF1[fcsProtocolEF1.purchaseNumber == '311300039417000192', ]
DT[fcsProtocolEF1.id == '15755958', ]

protocols.EF1[fcsProtocolEF1.id == '15836607', ]

DT <- DT.protocols01[purchaseNumber == '311300039417000192', ]
DT[fcsProtocolEF1.id == '15836607', ]

DT.protocols01[fcsProtocolEF1.id == '15836607', ]


DT <- protocols.EF2[purchaseNumber == '311300039417000192', ]
DT[fcsProtocolEF2.id == '15594655', ]
DT <- DT.protocols02[purchaseNumber == '311300039417000192', ]
DT[fcsProtocolEF2.id == '15594655', ]

DT.xml.files.index <- read.csv2(paste0(sRawCSVPath, 'DT_all_xml_files_index.csv'),
                           stringsAsFactors = F,
                           colClasses = c('character', rep('numeric', 55)),
                           encoding = 'CP-1251')
DT.xml.files.index <- data.table(DT.xml.files.index)
dim(DT.xml.files.index)
str(DT.xml.files.index)


# Может, есть несовпадения в тегах purchaseID и в тех же ID в именах файлов?
#  5% первого префикса просмотрели в цикле ниже (ооочень долго),
#  расхождений не нашли.

# # Индексируем имена всех файлов ================================================
# # таблица с именами всех файлов: имена столбцов по префиксам, первый столбец -- 
# #  номера измещений, значения в остальных столбцах -- id xml-файла из его имени
DT.xml.files.index <- data.table(noticeID.filename = all.ids)

# счётчик для очистки консоли
console.clean.count <- 0
# счётчики файлов для сообщений статуса
i.files.сount <- 0
n <- length(prefixes)

df.diff.IDs <- data.frame(fileID = NULL, tagID = NULL)

# цикл по уникальным префиксам файлов
for (pr_i in names(prefixes)) {
    
    # вытаскиваем все имена файлов с заданным префиксам
    tmp.v <- grep(all.xmls, pattern = paste0('^', pr_i), value = T)
    
    # вытаскиваем noticeID из имени файла
    noticeID.fn <- sub('^fcs_', '^fcs', tmp.v)
    noticeID.fn <- sub('^(.*?)_', '', noticeID.fn)
    noticeID.fn <- sub('_.*$', '', noticeID.fn)
    
    for (i in 1:length(tmp.v)) {
        rootNode <- xmlTreeParse(paste0(sRawXMLPath, tmp.v[i]), 
                                 encoding = 'UTF-8', useInternalNodes = T)
        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootNode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), 
                        names = names(nsDefs))
        names(ns)[names(ns) == ''] <- 'xmlns'
        # парсим noticeID
        val.tmp <- unique(xpathSApply(rootNode, '//purchaseNumber', xmlValue, 
                                      namespaces = ns))
        
        if (length(val.tmp) > 1) {
            message(paste0(tmp.v[i], ' # ', paste0(val.tmp, collapse = ' # ')))
        }
        
        if (sum(val.tmp != noticeID.fn[i])) {
            df.diff.IDs <- rbind(df.diff.IDs,
                                 data.frame(fileID = rep(noticeID.fn[i],
                                                         length(val.tmp)), 
                                            tagID = val.tmp))
        }
        
        message(round(i / length(tmp.v) * 100, 1))
        console.clean.count <- console.clean.count + 1
        # очистка консоли
        if (console.clean.count == iMaxConsoleStatusLines) {
            cat("\014")
            console.clean.count <- 0
        }
    }
    
    DT <- data.table(table(noticeID.fn))
    
    DT.xml.files.index <-
        merge(DT.xml.files.index, DT, by = 'noticeID.filename', all.x = T)
    DT.xml.files.index[is.na(N), N := 0]
    colnames(DT.xml.files.index)[colnames(DT.xml.files.index) == 'N'] <- pr_i
    
    i.files.сount <- i.files.сount + 1
    message(paste0(pr_i, ', ', round(i.files.сount / n * 100, 1), '%'))
    console.clean.count <- console.clean.count + 1
    
    # очистка консоли
    if (console.clean.count == iMaxConsoleStatusLines) {
        cat("\014")
        console.clean.count <- 0
    }
}

