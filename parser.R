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


# КОНСТАНТЫ --------------------------------------------------------------------
#
## постоянная часть запроса данных (URL)
const.html <- c('http://zakupki.gov.ru/epz/contract/quicksearch/search.html?searchString',
                'morphology',
                'pageNumber',
                'sortDirection',
                'recordsPerPage',
                'sortBy',
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
              '01.01.2016',                             # publishDateFrom
              '29.08.2017',                             # publishDateTo
              '',                                       # updateDateFrom
              ''                                        # updateDateTo
              ) 

fileURL <- paste0(paste0(const.html, '=', var.html), collapse = '&')


# ЗАГРУЗКА ---------------------------------------------------------------------
#
## создаём эмулятор браузера, т.к. сайт госзакупок хитровывернутый
RSelenium::rsDriver()
remDr <- remoteDriver(port = 4444, browserName = "chrome")
remDr$open(silent = T)
remDr$navigate(fileURL)

#!!!!!!
# НУЖНО ЭТО ДЕЛАТЬ ЧЕРЕЗ Docker
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused
#!!!!!!

## загружаем текст html-страницы
html <- getURL(fileURL)
## разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
## корневой элемент
rootNode <- xmlRoot(doc)


# ПАРСИНГ ----------------------------------------------------------------------
#
## настроечные переменные
#...............................................................................
### номер страницы выдачи
k <- 1
# порядковый номер первой записи в таблице на странице
num.start <- k
# количество записей на одной странице
n.at.one.page <- as.numeric(substr(var.html[5], 2, 
                                   nchar(var.html[5])))
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

### 6 столбец -- наименование заказчика
df.part$NameCustomer <- 
    xpathApplyAndClean(rootNode, 
                       "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']")

### с датами обновления и размещения приходится потанцевать
tmp <- xpathApplyAndClean(rootNode, 
                          "//label[@class='width80']/parent::li")
tmp.1 <- gsub(gsub(tmp[grepl(tmp, pattern = 'Размещено:')],
                   pattern = 'Размещено:', replacement = ''),
              pattern = '^\\s+|\\s+$', replacement = '')
tmp.2 <- gsub(gsub(tmp[grepl(tmp, pattern = 'Обновлено:')],
                   pattern = 'Обновлено:', replacement = ''),
              pattern = '^\\s+|\\s+$', replacement = '')

### 7 столбец -- дата размещения заказа
df.part$PostedDate <- tmp.1

### 8 столбец -- дата обновления заказа
df.part$UpdatedDate <- tmp.2

## находим переходим по ссылке к описанию электронного аукциона
details.URLs <- xpathApplyAndClean(rootNode, 
                                   "//dd[@class = 'additionalDescriptionList']//a",
                                   'attr', 'href')

### 9 столбец (заготовка) -- наименование электронной площадки
df.part$PlatformName <- rep('', n.at.one.page)

### 10 столбец (заготовка) -- объект закупки: наименование
df.part$ObjectName <- rep('', n.at.one.page)

### 11 столбец (заготовка) -- объект закупки: единица измерения
df.part$ObjectUnits <- rep('', n.at.one.page)

### 12 столбец (заготовка) -- объект закупки: количество единиц
df.part$ObjectAmount <- rep('', n.at.one.page)

### 13 столбец (заготовка) -- объект закупки: цена за единицу
df.part$ObjectUnitPrice <- rep('', n.at.one.page)

### 14 столбец (заготовка) -- начальная (максимальная) цена контракта
df.part$ObjectNMZ <- rep('', n.at.one.page)

### 15 столбец (заготовка) -- ценовое предложение участника
df.part$ParticipantPriceOffer <- rep('', n.at.one.page)

### 16 столбец (заготовка) -- наименование участника
df.part$ParticipantName <- rep('', n.at.one.page)

### 17 столбец (заготовка) -- флаг: победитель
df.part$ParticipantIsWinner <- rep(F, n.at.one.page)

## парсим детали
for (i in 1:length(details.URLs)) {
    ## загружаем текст html-страницы
    destFileURL <- paste0('http://zakupki.gov.ru', details.URLs)
    destFileURL <- gsub(destFileURL, pattern = '/notice/view/',
                        replacement = '/notice/ea44/view/')
    
    getURL('http://zakupki.gov.ru/epz/order/notice/ea44/view/common-info.html?regNumber=0163200000316006223',
           .encoding = 'UTF-8')
    
    destHtml <- getURL(destFileURL[i], .encoding = 'UTF-8')
    
    ## разбираем как html
    descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
    ## корневой элемент
    descRootNode <- xmlRoot(descDoc)
    
    ### 9 столбец (запись) -- наименование электронной площадки
    df.part$PlatformName <- xpathApplyAndClean(descRootNode,
                                               "//td")
    
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

