

DT.EA44.index <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_index_EA44.csv'),
                                      stringsAsFactors = F,
                                      colClasses = rep('character', 12)))
DT.EA44.index

nrow(DT.EA44.index)
nrow(unique(DT.EA44.index))
length(unique(DT.EA44.index$purchaseNumber))

DT.EA44.index[, .N, by = purchaseNumber][N == 5, ]

DT.EA44.index[purchaseNumber == '0111100002218000007']


# выкидываем уточнения
DT <- unique(DT.EA44.index[, !(colnames(DT.EA44.index) %in% 'id.fcsClarification'), 
                           with = F])
nrow(DT)

DT[, .N, by = purchaseNumber][N == 5, ]
DT[purchaseNumber == '0111100008517000242']


# все протоколы 01 из индекса: purchaseNumber + protocolID
DT <- na.omit(unique(DT.EA44.index[, c('purchaseNumber', 'id.fcsProtocolEF1'), 
                                   with = F]))
DT[, id.file.found := F]

# ищем такие же файлы
sYEAR <- c('201709', '201710', '201711', '201712', '201801', '201802', '201803',
           '201804', '201805')
sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], 
                       '_15-06-2018', '/')
sRawCSVPath <- paste0(sRawArchPath, 'csv/')
all.xmls <- read.csv2(paste0(sRawCSVPath, 'all_xmls.csv'),
                      stringsAsFactors = F)[, 1]

i.count <- 0 #3248
n <- nrow(DT)

tm <- Sys.time()
for (i in 1:100) {

    DT[i, id.file.found := 
           sum(grepl(all.xmls, pattern = paste0(DT$purchaseNumber[i], 
                                                '_', DT$id.fcsProtocolEF1[i])))]
    
    i.count <- i.count + 1
    if (i.count %/% 10 == i.count / 10) {
        message(paste0(round(i.count / n * 100, 1), '% готово'))
    }
}
message(paste0(round(difftime(Sys.time(), tm, units = 'mins'), 1), ' mins'))
### 0.5 mins

prod(as.logical(DT$id.file.found[1:3247]))

tm <- Sys.time()
tmp <- sapply(as.list(paste0(DT$purchaseNumber[1:100], '_', DT$id.fcsProtocolEF1[1:100])), 
              function(x) {as.logical(sum(grepl(all.xmls, pattern = x)))})
DT[1:100, id.file.found := tmp]
message(paste0(round(difftime(Sys.time(), tm, units = 'mins'), 1), ' mins'))
### 0.5 mins


# до конца не досчитали, но в индексе с именами файлов, похоже, всё в порядке

# сверяем с нерасклеянными протоколами первого этапа
protocols.EF1 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF1.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 7)))
protocols.EF1

DT.p <- unique(protocols.EF1[, c('fcsProtocolEF1.purchaseNumber', 
                                 'fcsProtocolEF1.id'), with = F])

tmp.v <- paste0(DT$purchaseNumber, '_', DT$id.fcsProtocolEF1) == 
    paste0(DT.p$fcsProtocolEF1.purchaseNumber, '_', DT.p$fcsProtocolEF1.id)
tmp.v[tmp.v == F]
prod(tmp.v)

# сверяем с нерасклеянными протоколами второго этапа
protocols.EF2 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF2.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 9)))
protocols.EF2

DT.p <- unique(protocols.EF2[, c('purchaseNumber', 
                                 'fcsProtocolEF2.id'), with = F])

DT <- na.omit(unique(DT.EA44.index[, c('purchaseNumber', 'id.fcsProtocolEF2'), 
                                   with = F]))
nrow(DT)

tmp.v <- paste0(DT$purchaseNumber, '_', DT$id.fcsProtocolEF2) == 
    paste0(DT.p$purchaseNumber, '_', DT.p$fcsProtocolEF2.id)
tmp.v[tmp.v == F]
prod(tmp.v)

# сверяем с нерасклеянными протоколами третьего этапа
protocols.EF3 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF3.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 9)))
protocols.EF3

DT.p <- unique(protocols.EF3[, c('purchaseNumber', 
                                 'fcsProtocolEF3.id'), with = F])

DT <- na.omit(unique(DT.EA44.index[, c('purchaseNumber', 'id.fcsProtocolEF3'), 
                                   with = F]))
nrow(DT)

tmp.v <- paste0(DT$purchaseNumber, '_', DT$id.fcsProtocolEF3) == 
    paste0(DT.p$purchaseNumber, '_', DT.p$fcsProtocolEF3.id)
tmp.v[tmp.v == F]
prod(tmp.v)

# ТЕПЕРЬ СОШЛОСЬ