for (k in 1:1) {
    # порядковый номер первой записи в таблице на странице
    num.start <- k
    
    
    
    # порядковый номер последней записи в таблице на странице
    num.end <- k + n.at.one.page - 1
    
    ## парсим страницу с выдачей во фрейм df.part ..............................
    ### 1 столбец -- порядковый номер записи в общей таблице
    df.part <- data.frame(Num = seq(frst.record.number, 
                                    frst.record.number + n.at.one.page - 1, 1))
    
    ### 2 столбец -- статус заказа
    xpath.string <- "//td[@class='tenderTd']/dl/dt/span"
    df.part$Status <- xpathApplyAndClean(rootNode, xpath.string)
    df.part$Status <- gsub('/.*$', '', df.part$Status)
    
    ### 3 столбец -- основание
    xpath.string <- "//td[@class='tenderTd']/dl/dt/span/span"
    df.part$Law <- xpathApplyAndClean(rootNode, xpath.string)
    
    ### 4 столбец -- начальная цена
    xpath.string <- "//td[@class = 'tenderTd']/dl/dd/strong"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('\\s+', '', xpath.result)
    df.part$StartPrice <- xpath.result
    
    ### 5 столбец -- номер заявки (контракта)
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dt/a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('№ ', '', xpath.result)
    df.part$ContractID <- xpath.result
    
    ### 6 столбец -- ссылка на контракт
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dt/a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string, 'attr', 'href')
    xpath.result <- paste0('http://zakupki.gov.ru', xpath.result)
    df.part$ContractHyperlink <- xpath.result
    
    ### 7 столбец -- наименование заказчика
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']//a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('(?<=[\\s])\\s*|^\\s+|\\s+$', '', xpath.result, 
                         perl = T)
    xpath.result <- gsub('^Заказчик: ', '', xpath.result)
    xpath.result <- gsub('^Организация, осуществляющая размещение: ', '', 
                         xpath.result)
    df.part$NameCustomer <- xpath.result
    
    ### 8 столбец -- ссылка на заказчика
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']//a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string, 'attr', 'href')
    xpath.result <- paste0('http://zakupki.gov.ru', xpath.result)
    df.part$customerHyperlink <- xpath.result
    
    
    
    
    
    ### 8 столбец (заготовка) -- ОКТМО заказчика
    df.part$CustomerOKTMO <- rep('', n.at.one.page)
    
    ### с датами обновления и размещения приходится потанцевать
    xpath.string <- "//label[@class='width80']/parent::li"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    tmp <- xpath.result
    xpath.result <- c(gsub(gsub(tmp[grepl(tmp, pattern = 'Размещено:')],
                                pattern = 'Размещено:', replacement = ''),
                           pattern = '^\\s+|\\s+$', replacement = ''),
                      gsub(gsub(tmp[grepl(tmp, pattern = 'Обновлено:')],
                                pattern = 'Обновлено:', replacement = ''),
                           pattern = '^\\s+|\\s+$', replacement = ''))
    
    ### 9 столбец -- дата размещения заказа
    df.part$PostedDate <- xpath.result[1]
    
    ### 10 столбец -- дата обновления заказа
    df.part$UpdatedDate <- xpath.result[2]
    
    ## находим переходим по ссылке к описанию электронного аукциона
    xpath.string <- "//dd[@class = 'additionalDescriptionList']/ul/li/span/a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string, 'attr', 'href')
    xpath.result <- paste0('http://zakupki.gov.ru', xpath.result)
    details.URLs <- xpath.result
    
    ### 11 столбец (заготовка) -- наименование электронной площадки
    df.part$PlatformName <- rep('', n.at.one.page)
    
    ### 12 столбец (заготовка) -- объект закупки: наименование
    df.part$ObjectName <- rep('', n.at.one.page)
    
    ### 13 столбец (заготовка) -- объект закупки: код по ОКПД2
    df.part$ObjectCode <- rep('', n.at.one.page)
    
    ### 14 столбец (заготовка) -- объект закупки: единица измерения
    df.part$ObjectUnits <- rep('', n.at.one.page)
    
    ### 15 столбец (заготовка) -- объект закупки: количество единиц
    df.part$ObjectAmount <- rep('', n.at.one.page)
    
    ### 16 столбец (заготовка) -- объект закупки: цена за единицу
    df.part$ObjectUnitPrice <- rep('', n.at.one.page)
    
    ### 17 столбец (заготовка) -- начальная (максимальная) цена контракта
    df.part$ObjectNMZ <- rep('', n.at.one.page)
    
    ### 18 столбец (заготовка) -- есть ли ограничения допуска
    df.part$IsRestrictions <- rep(F, n.at.one.page)
    
    ### 19 столбец (заготовка) -- минимальное ценовое предложение
    df.part$MinPriceOffer <- rep('', n.at.one.page)
    
    ### 20 столбец (заготовка) -- максимальное ценовое предложение
    df.part$MinPriceOffer <- rep('', n.at.one.page)
    
    ### 21 столбец (заготовка) -- количество участников
    df.part$ParticipantsNumber <- rep('', n.at.one.page)
    
    
    # ссылки на страницы с общей информацией
    destFileURL <- details.URLs
    destFileURL <- gsub(destFileURL, pattern = '/notice/view/',
                        replacement = '/notice/ea44/view/')
    
    # сведения о поставщиках: гиперссылки
    destFileURL2 <- paste0('http://zakupki.gov.ru/epz/order/notice/ea44/view/supplier-results.html?regNumber=',
                           gsub('.*=', '', destFileURL))
    
    ## парсим детали
    for (i in 1:length(details.URLs)) {
        
        print(paste0('i = ', i))
        print(destFileURL[i])
        
        destHtml <- getURL(destFileURL[i], .encoding = 'UTF-8',
                           ssl.verifypeer = FALSE,
                           .opts = list(useragent = userAgent,
                                        followlocation = TRUE))
        
        ## разбираем как html
        descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
        ## корневой элемент
        descRootNode <- xmlRoot(descDoc)
        
        # всё содержимое табличек, потому что я не могу нормально
        #  запарсить по условию
        xpath.string <- "//div[@class = 'noticeTabBoxWrapper']/table//td"
        raw.notice <- xpathApplyAndClean(descRootNode, xpath.string)
        
        ### 11 столбец -- наименование электронной площадки
        search.tag <- 'Наименование электронной площадки'
        df.part$PlatformName[i] <- raw.notice[which(grepl(raw.notice,
                                                          pattern = search.tag)) + 1]
        
        ### 12 столбец -- объект закупки: наименование
        search.tag <- 'Наименование товара, работы, услуги'
        df.part$ObjectName[i] <- raw.notice[which(grepl(raw.notice,
                                                        pattern = search.tag)) + 8][1]
        
        ### 13 столбец -- объект закупки: код по ОКПД2
        search.tag <- 'Наименование товара, работы, услуги'
        df.part$ObjectCode <- raw.notice[which(grepl(raw.notice,
                                                     pattern = search.tag)) + 9][1]
        
        ### 14 столбец -- объект закупки: единица измерения
        search.tag <- 'Наименование товара, работы, услуги'
        df.part$ObjectUnits[i] <- raw.notice[which(grepl(raw.notice,
                                                         pattern = search.tag)) + 10][1]
        
        ### 15 столбец -- объект закупки: количество единиц
        search.tag <- 'Наименование товара, работы, услуги'
        df.part$ObjectAmount[i] <- raw.notice[which(grepl(raw.notice,
                                                          pattern = search.tag)) + 11][1]
        
        ### 16 столбец -- объект закупки: цена за единицу
        search.tag <- 'Наименование товара, работы, услуги'
        df.part$ObjectUnitPrice[i] <- raw.notice[which(grepl(raw.notice,
                                                             pattern = search.tag)) + 12][1]
        
        ### 17 столбец -- начальная (максимальная) цена контракта
        search.tag <- 'цена контракта'
        df.part$ObjectNMZ[i] <- raw.notice[which(grepl(raw.notice,
                                                       pattern = search.tag)) + 1]
        
        ### 18 столбец -- есть ли ограничения допуска
        search.tag <- 'Условия, запреты и ограничения допуска'
        txt.restr <- raw.notice[which(grepl(raw.notice,
                                            pattern = search.tag))]
        if (length(txt.restr) != 0) {
            df.part$IsRestrictions[i] <- T
        }
        
        # загружаем текст html-страницы с координатами заказчика
        destFileURL3 <- df.part$customerHyperlink[i]
        print(paste0('заказчик: ', destFileURL3))
        destHtml <- getURL(destFileURL3, .encoding = 'UTF-8',
                           ssl.verifypeer = FALSE,
                           .opts = list(useragent = userAgent,
                                        followlocation = TRUE))
        ## разбираем как html
        descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
        ## корневой элемент
        descRootNode <- xmlRoot(descDoc)
        
        ### 8 столбец -- ОКТМО заказчика
        txt.oktmo <- xpathApplyAndClean(descRootNode,
                                        "//*[@id = 'tab-info']//td")
        df.part$CustomerOKTMO <- tryXpath(txt.oktmo[which(txt.oktmo == 'ОКТМО') + 1])
        
        
        # загружаем текст html-страницы с результатами определения поставщика
        print(paste0('Аукцион: ', destFileURL2[i]))
        destHtml <- getURL(destFileURL2[i], .encoding = 'UTF-8',
                           ssl.verifypeer = FALSE,
                           .opts = list(useragent = userAgent,
                                        followlocation = TRUE))
        
        ## разбираем как html
        descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
        ## корневой элемент
        descRootNode <- xmlRoot(descDoc)
        
        raw.partisipants <- xpathApplyAndClean(descRootNode, "//td")
        search.tag <- c('Протокол определения поставщика', 'Информация о контракте')
        raw.partisipants <- raw.partisipants[(which(grepl(raw.partisipants,
                                                          pattern = search.tag[1])) + 6):(which(grepl(raw.partisipants, 
                                                                                                      pattern = search.tag[2])) - 1)]
        prices <- raw.partisipants[seq(4,
                                       floor(length(raw.partisipants) / 4) * 4,
                                       by = 4)]
        prices <- gsub(pattern = 'Российский рубль', replacement = '', prices)
        prices <- gsub(pattern = '[[:space:]]', replacement = '', prices)
        prices <- as.numeric(gsub(',', '.', prices))
        
        ### 19 столбец -- минимальное ценовое предложение
        df.part$MinPriceOffer <- min(prices)
        
        ### 20 столбец -- максимальное ценовое предложение
        df.part$MinPriceOffer <- max(prices)
        
        ### 21 столбец -- количество участников
        df.part$ParticipantsNumber <- length(xpathApplyAndClean(descRootNode,
                                                                "//div[@class = 'noticeTabBoxWrapper']//tbody/tr"))
    }
    
    
    # ЗАПИСЬ РЕЗУЛЬТАТОВ -------------------------------------------------------
    
    filePath <- gsub(paste0('./', exportCsvFileName),
                     pattern = ':',
                     replacement = '-')
    filePath <- gsub(filePath,
                     pattern = '\\s',
                     replacement = '_')
    if (!file.exists(filePath)) {
        file.create(filePath)
        write.table(df.part, filePath, row.names = F, append = T, dec = ',',
                    sep = ';')
    } else {
        write.table(df.part, filePath, row.names = F, append = T, dec = ',',
                    sep = ';', col.names = F)
    }
}
