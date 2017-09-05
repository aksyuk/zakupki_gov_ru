#
# В этом скрипте содержится код разбора выдачи сайта госзакупок zakupki.gov.ru
# Начало работы: 29.08.2017
# Автор: Светлана Аксюк (s.a.aksuk@gmail.com)
#
# Данные загружаются по запросу через URL.
# Пример URL для разбора:
# http://zakupki.gov.ru/epz/contract/quicksearch/search.html?searchString=&morphology=on&pageNumber=1&sortDirection=false&recordsPerPage=_50&sortBy=PO_DATE_OBNOVLENIJA&priceFrom=0&priceTo=1000000&budgetLevels=&budgetName=&nonBudgetCodesList=&placingWayForContractList_4=on&placingWayForContractList=4&contractStageList_1=on&contractStageList_2=on&contractStageList=1%2C2&regions=&contractDateFrom=&contractDateTo=&contractInputNameDefenseOrderNumber=&contractInputNameContractNumber=&publishDateFrom=01.01.2016&publishDateTo=29.08.2017&updateDateFrom=&updateDateTo=
#
# Результаты -- характеристики госзакупок из базы данных -- записыввются 
#  в .csv файл.
# Ограничение API: 500 записей при выгрузке вручную с сайта.
# Мы запрашиваем все записи с максимальным количеством на странице: 50, 
#  потом листаем в цикле. Внимание: заказы быстро добавляются, в конце 
#  стоит проверить таблицу на наличие дубликатов. Уникальный идентификатор -- 
#  номер заказа (ContractID).
#


# ПАКЕТЫ -----------------------------------------------------------------------
#
library('RCurl')
library('XML')
library('RSelenium')
library('wdman')


# КОНСТАНТЫ --------------------------------------------------------------------
#
## постоянная часть запроса данных (URL)
const.html <- c('http://zakupki.gov.ru/epz/contract/quicksearch/search.html?searchString',
                'morphology',
                'pageNumber',
                'sortDirection',
                'recordsPerPage',
                'sortBy',
                'fz44',
                'priceFrom',
                'priceTo',
                'budgetLevels',
                'budgetName',
                'nonBudgetCodesList',
                'placingWayForContractList_4',
                'placingWayForContractList',
                'contractStageList_1',
                'contractStageList_2',
                'contractStageList',
                'regions',
                'contractDateFrom',
                'contractDateTo',
                'contractInputNameDefenseOrderNumber',
                'contractInputNameContractNumber',
                'publishDateFrom',
                'publishDateTo',
                'updateDateFrom',
                'updateDateTo')

## имя браузера для маскировки запроса по getURL()
userAgent <- 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'

## имя файла для экспорта
exportCsvFileName <- paste0('zakupki_gov_ru_', Sys.time(), '.csv')


# ФУНКЦИИ ----------------------------------------------------------------------
#
xpathApplyAndClean <- function(root.node, xpath.text, return.mode = 'value',
                               attr.name = NULL) {
    # сами теги
    if (return.mode == 'value') {
        result <- xpathSApply(root.node, xpath.text, xmlValue)
    }
    if (return.mode == 'attr') {
        result <- xpathSApply(root.node, xpath.text, xmlGetAttr, attr.name)
    }
    # убираем перевод каретки
    result <- gsub(result, pattern = '\r\n', replacement = '')
    # убираем пробелы по краям
    result <- gsub(result, pattern = "^\\s+|\\s+$", replacement = '')
    # возвращаем значения
    return(result)
}


# ПЕРЕМЕННЫЕ -------------------------------------------------------------------
#
## переменная часть запроса данных (URL)
var.html <- c('',                                       # searchString
              'on',                                     # morphology
              '1',                                      # pageNumber
              'false',                                  # sortDirection
              '_50',                                    # recordsPerPage
              'PO_DATE_OBNOVLENIJA',                    # sortBy
              'on',                                     # fz44
              '0',                                      # priceFrom
              '1000000',                                # priceTo
              '',                                       # budgetLevels
              '',                                       # budgetName
              '',                                       # nonBudgetCodesList
              'on',                                     # placingWayForContractList_4
              '4',                                      # placingWayForContractList
              'on',                                     # contractStageList_1
              'on',                                     # contractStageList_2
              '1%2C2',                                  # contractStageList
              '',                                       # regions
              '',                                       # contractDateFrom
              '',                                       # contractDateTo
              '',                                       # contractInputNameDefenseOrderNumber
              '',                                       # contractInputNameContractNumber
              '01.06.2017',                             # publishDateFrom
              '31.08.2017',                             # publishDateTo
              '',                                       # updateDateFrom
              ''                                        # updateDateTo
              ) 

fileURL <- paste0(paste0(const.html, '=', var.html), collapse = '&')


# ЗАГРУЗКА ---------------------------------------------------------------------
#
<<<<<<< Updated upstream
=======
## создаём эмулятор браузера, т.к. сайт госзакупок хитровывернутый
selServ <- wdman::selenium(verbose = FALSE)
rD <- RSelenium::rsDriver(browser = "chrome")
remDr <- rD[["client"]]
remDr$open(silent = T)
remDr$navigate(fileURL)

#!!!!!!
# НУЖНО ЭТО ДЕЛАТЬ ЧЕРЕЗ Docker
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused
#!!!!!!

>>>>>>> Stashed changes
## загружаем текст html-страницы
html <- getURL(fileURL)
## разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
## корневой элемент
rootNode <- xmlRoot(doc)


# ПАРСИНГ ----------------------------------------------------------------------
#
# РЕЕСТР КОНТРАКТОВ ============================================================
#
## настроечные переменные
#...............................................................................

### сколько страниц в выдаче
n.pages <- as.numeric(xpathApplyAndClean(rootNode, 
                                         "//div[@class = 'paginator greyBox']/ul/li"))
n.pages <- n.pages[!is.na(n.pages)]
n.pages <- max(n.pages)

# количество записей на одной странице
n.at.one.page <- as.numeric(substr(var.html[5], 2, 
                                   nchar(var.html[5])))


### цикл по номерам страниц выдачи
for (k in 1:1) {
    # порядковый номер первой записи в таблице на странице
    num.start <- k
    
    # порядковый номер последней записи в таблице на странице
    num.end <- k + n.at.one.page - 1
    
    ## парсим страницу с выдачей во фрейм df.part
    #...............................................................................
    ### 1 столбец -- порядковый номер записи в общей таблице
    df.part <- data.frame(Num = seq(num.start, num.end, 1))
    
    ### 2 столбец -- статус заказа
    df.part$Status <- 
        xpathApplyAndClean(rootNode, "//dd[@class='status-field']")
    
    ### 3 столбец -- итоговая сумма заказа (предложение победителя)
    df.part$FinalStoim <- 
        xpathApplyAndClean(rootNode, "//td[@class = 'tenderTd']/dl/dd/strong")
    
    ### 4 столбец -- номер контракта
    df.part$ContractID <- 
        xpathApplyAndClean(rootNode, "//td[@class='descriptTenderTd']/dl/dt")
    
    ### 5 столбец -- ссылка на контракт
    df.part$ContractHyperlink <- 
        xpathApplyAndClean(rootNode, "//td[@class='descriptTenderTd']/dl/dt/a",
                           'attr', 'href')
    
    ### 6 столбец -- ссылка на заказчика
    df.part$customerHyperlink <- 
        xpathApplyAndClean(rootNode, "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']/a",
                           'attr', 'href')
    
    ### 7 столбец -- наименование заказчика
    df.part$NameCustomer <- 
        xpathApplyAndClean(rootNode, 
                           "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']")
    
    ### 8 столбец (заготовка) -- ОКТМО заказчика
    df.part$CustomerOKTMO <- rep('', n.at.one.page)
        
    
    ### с датами обновления и размещения приходится потанцевать
    tmp <- xpathApplyAndClean(rootNode, 
                              "//label[@class='width80']/parent::li")
    tmp.1 <- gsub(gsub(tmp[grepl(tmp, pattern = 'Размещено:')],
                       pattern = 'Размещено:', replacement = ''),
                  pattern = '^\\s+|\\s+$', replacement = '')
    tmp.2 <- gsub(gsub(tmp[grepl(tmp, pattern = 'Обновлено:')],
                       pattern = 'Обновлено:', replacement = ''),
                  pattern = '^\\s+|\\s+$', replacement = '')
    
    ### 9 столбец -- дата размещения заказа
    df.part$PostedDate <- tmp.1
    
    ### 10 столбец -- дата обновления заказа
    df.part$UpdatedDate <- tmp.2
    
    ## находим переходим по ссылке к описанию электронного аукциона
    details.URLs <- xpathApplyAndClean(rootNode, 
                                       "//dd[@class = 'additionalDescriptionList']/ul/li/span/a",
                                       'attr', 'href')
    
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
    destFileURL <- paste0('http://zakupki.gov.ru', details.URLs)
    destFileURL <- gsub(destFileURL, pattern = '/notice/view/',
                        replacement = '/notice/ea44/view/')
    
<<<<<<< Updated upstream
    # сведения о поставщиках: гиперссылки
    destFileURL2 <- paste0('http://zakupki.gov.ru/epz/order/notice/ea44/view/supplier-results.html?regNumber=', 
                           gsub('.*=', '', destFileURL))
=======
    getURL(destFileURL[i], .encoding = 'UTF-8')
>>>>>>> Stashed changes
    
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
        raw.notice <- xpathApplyAndClean(descRootNode, 
                                         "//div[@class = 'noticeTabBoxWrapper']/table//td")
        
        ### 11 столбец -- наименование электронной площадки
        df.part$PlatformName[i] <- raw.notice[which(grepl(raw.notice, 
                                                          pattern = 'Наименование электронной площадки')) + 1]
        
        ### 12 столбец -- объект закупки: наименование
        df.part$ObjectName[i] <- raw.notice[which(grepl(raw.notice, 
                                                        pattern = 'Наименование товара, работы, услуги')) + 8][1]
        
        ### 13 столбец -- объект закупки: код по ОКПД2
        df.part$ObjectCode <- raw.notice[which(grepl(raw.notice, 
                                                     pattern = 'Наименование товара, работы, услуги')) + 9][1]
        
        ### 14 столбец -- объект закупки: единица измерения
        df.part$ObjectUnits[i] <- raw.notice[which(grepl(raw.notice, 
                                                         pattern = 'Наименование товара, работы, услуги')) + 10][1]
        
        ### 15 столбец -- объект закупки: количество единиц
        df.part$ObjectAmount[i] <- raw.notice[which(grepl(raw.notice, 
                                                          pattern = 'Наименование товара, работы, услуги')) + 11][1]
        
        ### 16 столбец -- объект закупки: цена за единицу
        df.part$ObjectUnitPrice[i] <- raw.notice[which(grepl(raw.notice, 
                                                             pattern = 'Наименование товара, работы, услуги')) + 12][1]
        
        ### 17 столбец -- начальная (максимальная) цена контракта
        df.part$ObjectNMZ[i] <- raw.notice[which(grepl(raw.notice, 
                                                       pattern = 'цена контракта')) + 1]
        
        ### 18 столбец -- есть ли ограничения допуска
        txt.restr <- raw.notice[which(grepl(raw.notice, 
                                            pattern = 'Условия, запреты и ограничения допуска'))]
        if (length(txt.restr) != 0) {
            df.part$IsRestrictions[i] <- T
        }
        
        # загружаем текст html-страницы с координатами заказчика
        destFileURL3 <- paste0('http://zakupki.gov.ru', df.part$customerHyperlink[i])
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
        df.part$CustomerOKTMO <- txt.oktmo[which(txt.oktmo == 'ОКТМО') + 1]
        
        
        # загружаем текст html-страницы с результатами определения поставщика
        destHtml <- getURL(destFileURL2[i], .encoding = 'UTF-8',
                           ssl.verifypeer = FALSE, 
                           .opts = list(useragent = userAgent,
                                        followlocation = TRUE))
        
        ## разбираем как html
        descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
        ## корневой элемент
        descRootNode <- xmlRoot(descDoc)

        raw.partisipants <- xpathApplyAndClean(descRootNode, "//td")
        raw.partisipants <- raw.partisipants[(which(grepl(raw.partisipants,
                                                          pattern = 'Протокол определения поставщика')) + 6):(which(grepl(raw.partisipants,
                                                                                                                          pattern = 'Информация о контракте')) - 1)]
        prices <- raw.partisipants[seq(4, 
                                       floor(length(raw.partisipants) / 4) * 4, 
                                       by = 4)]
        prices <- gsub(pattern = 'Российский рубль', replacement = '', prices)
        prices <- gsub('\\s', '', prices)
        prices <- as.numeric(gsub(',', '.', prices))
        
        ### 19 столбец -- минимальное ценовое предложение
        df.part$MinPriceOffer <- min(prices)
        
        ### 20 столбец -- максимальное ценовое предложение
        df.part$MinPriceOffer <- max(prices)
        
        ### 21 столбец -- количество участников
        df.part$ParticipantsNumber <- length(xpathApplyAndClean(descRootNode, 
                                                                "//div[@class = 'noticeTabBoxWrapper']//tbody/tr"))
    }
    
    
    # ЗАПИСЬ РЕЗУЛЬТАТОВ -----------------------------------------------------------
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


