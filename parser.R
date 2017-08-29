
library('RCurl')
library('XML')


# выгрузка по URL

# http://zakupki.gov.ru/epz/contract/quicksearch/search.html?searchString=&morphology=on&pageNumber=1&sortDirection=false&recordsPerPage=_50&sortBy=PO_DATE_OBNOVLENIJA&priceFrom=0&priceTo=1000000&budgetLevels=&budgetName=&nonBudgetCodesList=&placingWayForContractList_4=on&placingWayForContractList=4&contractStageList_1=on&contractStageList_2=on&contractStageList=1%2C2&regions=&contractDateFrom=&contractDateTo=&contractInputNameDefenseOrderNumber=&contractInputNameContractNumber=&publishDateFrom=01.01.2016&publishDateTo=29.08.2017&updateDateFrom=&updateDateTo=

const.html <- c('http://zakupki.gov.ru/epz/contract/quicksearch/search.html?searchString',
                '&morphology',
                '&pageNumber',
                '&sortDirection',
                '&recordsPerPage',
                '&sortBy',
                '&priceFrom',
                '&priceTo',
                '&budgetLevels',
                '&budgetName',
                '&nonBudgetCodesList',
                '&placingWayForContractList_4',
                '&placingWayForContractList',
                '&contractStageList_1',
                '&contractStageList_2',
                '&contractStageList',
                '&regions',
                '&contractDateFrom',
                '&contractDateTo',
                '&contractInputNameDefenseOrderNumber',
                '&contractInputNameContractNumber',
                '&publishDateFrom',
                '&publishDateTo',
                '&updateDateFrom',
                '&updateDateTo')
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

fileURL <- paste0(const.html, var.html, collapse = '=')

# загружаем текст html-страницы
html <- getURL(fileURL)
# разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
# корневой элемент
rootNode <- xmlRoot(doc)

k <- 1

df.part <- data.frame(Num = seq(k, 
                                k + as.numeric(substr(var.html[5], 2, 
                                                      nchar(var.html[5]))) - 1, 
                                1))

df.part$Status <- gsub(gsub(xpathSApply(rootNode, "//dd[@class='status-field']", xmlValue),
                            pattern = '\r\n', replacement = ''),
                       pattern = "^\\s+|\\s+$", replacement = '')
        
df.part$Stoim <- as.numeric(gsub(gsub(xpathSApply(rootNode, "//td[@class = 'tenderTd']/dl/dd/strong", xmlValue),
                                      pattern = '\u00A0', replacement = ''),
                                 pattern = ',', replacement = '.'))

df.part$ContractID <- gsub(gsub(xpathSApply(rootNode, 
                                            "//td[@class='descriptTenderTd']/dl/dt", 
                                            xmlValue),
                                pattern = '\r\n', replacement = ''),
                           pattern = "^\\s+|\\s+$", replacement = '')
df.part$ContractHyperlink <- paste0('http://zakupki.gov.ru', 
                                    xpathSApply(rootNode, 
                                         "//td[@class='descriptTenderTd']/dl/dt/a", 
                                         xmlGetAttr, 'href'))

df.part$NameCustomer <- gsub(gsub(xpathSApply(rootNode, "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']", 
                                              xmlValue),
                                  pattern = '\r\n', replacement = ''),
                             pattern = "^\\s+|\\s+$", replacement = '')



df.part

# КАК ЗАПАРСИТЬ РЕКВИЗИТЫ ЗАКАЗА, ЕСЛИ ОНИ ВСТРЕЧАЮТСЯ НЕ ВЕЗДЕ???
