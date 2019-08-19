# ..............................................................................
# parser-html-02_Load-Data.R
# 
# Парсинг сайта госзакупок, 
#  который находится по адресу
#  http://zakupki.gov.ru/
# Результаты -- таблица по госзакупкам -- записывается в .csv файл.
# Параметры поиска в файле parser_html_URL_params.csv.
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
#
# Версия 0.9 (16.08.2019)
#
# Эта часть кода содержит непосрадственно загрузку и сохранение данных

# ..............................................................................
# ВНИМАНИЕ!!!!!!
# ПРИ БОЛЬШОМ КОЛИЧЕСТВЕ ОБРАЩЕНИЙ НА САЙТЕ ГОСЗАКУПОК МОГУТ ЗАБАНИТЬ ПО IP
#
# Ограничение API: 500 записей при выгрузке вручную с сайта.
# Мы запрашиваем все записи с максимальным количеством на странице: 50,
#  потом листаем в цикле. Внимание: заказы быстро добавляются, в конце
#  стоит проверить таблицу на наличие дубликатов. Уникальный идентификатор --
#  номер заказа (ContractID).



# ПЕРЕМЕННЫЕ -------------------------------------------------------------------

## составляющие запроса данных (URL)
df.params <- read.csv2('./parser_html_URL_params.csv', stringsAsFactors = F)
const.html <- df.params$ParamID
var.html <- df.params$Value

file.URL <- paste0(paste0(const.html, '=', var.html), collapse = '&')

curr.file.URL <- gsub('_MY_PAGE_NUMBER_', 1, file.URL)



# ЗАГРУЗКА ---------------------------------------------------------------------

## загружаем текст html-страницы
html <- getURL(curr.file.URL, .encoding = 'UTF-8', ssl.verifypeer = FALSE,
               .opts = list(useragent = USER.AGENT, followlocation = TRUE))
## разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
## корневой элемент
rootNode <- xmlRoot(doc)
# # сохраняем в файл
# saveXML(rootNode, file = 'test_XML.xml')



# ПАРСИНГ ----------------------------------------------------------------------


# РЕЕСТР КОНТРАКТОВ ============================================================

## настроечные переменные ......................................................

### сколько страниц в выдаче
N <- xpathApplyAndClean(rootNode, "//p[@class = 'allRecords']/strong")[1]
N <- as.numeric(gsub('\\s', '', N))
N <- N[!is.na(N)]
N <- max(N)

# количество записей на одной странице
n.at.one.page <- df.params[df.params$ParamID == 'recordsPerPage', 2]
n.at.one.page <- as.numeric(gsub('[^0-9]', '', n.at.one.page))

n.pages <- ceiling(N / n.at.one.page)

### цикл по номерам страниц выдачи
for (curr.page.number in 1:n.pages) {
    
    
    
    # ПЕРЕЛИСТНУТЬ СТРАНИЦУ ----------------------------------------------------
    curr.file.URL <- gsub('_MY_PAGE_NUMBER_', curr.page.number, file.URL)
    
    cat(yellow(paste0('page = ', curr.page.number, ' of ', n.pages,
                      ': ', curr.file.URL, '\n')))
    
    ## загружаем текст html-страницы
    html <- getURL(curr.file.URL, .encoding = 'UTF-8', ssl.verifypeer = FALSE,
                   .opts = list(useragent = USER.AGENT, followlocation = TRUE))
    ## разбираем как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    ## корневой элемент
    rootNode <- xmlRoot(doc)
    
    # порядковый номер первой записи в таблице на странице
    frst.record.number <- 1 + (curr.page.number - 1) * n.at.one.page

    
    xpath.string <- "//div[@class = 'noticeTabBoxWrapper']/table//td"
    raw.page <- xpathApplyAndClean(rootNode, xpath.string)
    
    
    ## парсим страницу с выдачей во фрейм df.part ..............................
    # ### 1 столбец -- порядковый номер записи в общей таблице
    # df.part <- data.frame(Num = seq(frst.record.number, 
    #                                 frst.record.number + n.at.one.page - 1, 1))
    
    ### 2 столбец -- номер заявки (контракта)
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dt/a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('№ ', '', xpath.result)
    df.part <- data.frame(ContractID = 
                              paste0("'", as.character(xpath.result), "'"))
    
    ### 3 столбец -- тип заявки
    xpath.string <- "//td[@class='tenderTd']/dl/dt/strong"
    df.part$Type <- xpathApplyAndClean(rootNode, xpath.string)
    
    ### 4 столбец -- статус заказа
    xpath.string <- "//td[@class='tenderTd']/dl/dt/span"
    df.part$Status <- xpathApplyAndClean(rootNode, xpath.string)
    df.part$Status <- gsub('/.*$', '', df.part$Status)
    
    ### 5 столбец -- основание
    xpath.string <- "//td[@class='tenderTd']/dl/dt/span/span"
    df.part$Law <- xpathApplyAndClean(rootNode, xpath.string)
    
    ### 6 столбец -- начальная цена
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dt/a/ancestor::td/preceding-sibling::td/dl/dd/strong"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('\\s+', '', xpath.result)
    
    xpath.string <- "//td/dl/dd/strong/ancestor::td/following-sibling::td[@class='descriptTenderTd']/dl/dt/a"
    key.result <- xpathApplyAndClean(rootNode, xpath.string)
    key.result <- gsub('№ ', '', key.result)
    key.result <- paste0("'", as.character(key.result), "'")
    
    df <- data.frame(ContractID = key.result, StartPrice = xpath.result)
    df.part <- merge(df.part, df, by = 'ContractID', all.x = T)

    ### 7 столбец -- ссылка на контракт
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dt/a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string, 'attr', 'href')
    xpath.result <- paste0('http://zakupki.gov.ru', xpath.result)
    xpath.result <- gsub('http://zakupki.gov.ruhttp', 'http', xpath.result)
    df.part$ContractHyperlink <- xpath.result

    ### 8 столбец -- наименование заказчика
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']//a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    xpath.result <- gsub('(?<=[\\s])\\s*|^\\s+|\\s+$', '', xpath.result, 
                         perl = T)
    xpath.result <- gsub('^Заказчик: ', '', xpath.result)
    xpath.result <- gsub('^Организация, осуществляющая размещение: ', '', 
                         xpath.result)
    df.part$NameCustomer <- xpath.result
    
    ### 9 столбец -- ссылка на заказчика
    xpath.string <- "//td[@class='descriptTenderTd']/dl/dd[@class='nameOrganization']//a"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string, 'attr', 'href')
    xpath.result <- paste0('http://zakupki.gov.ru', xpath.result)
    df.part$customerHyperlink <- xpath.result
        
    ### с датами обновления и размещения приходится потанцевать
    xpath.string <- "//td[@class='amountTenderTd']/ul/li"
    xpath.result <- xpathApplyAndClean(rootNode, xpath.string)
    tmp <- data.frame(PostedDate = grep('Размещено:', xpath.result, value = T),
                      UpdatedDate = grep('Обновлено:', xpath.result, value = T))
    tmp$PostedDate <- gsub('Размещено: ', '', tmp$PostedDate)
    tmp$UpdatedDate <- gsub('Обновлено: ', '', tmp$UpdatedDate)

    ### 10 столбец -- дата размещения заказа
    df.part$PostedDate <- tmp[, 1]

    ### 11 столбец -- дата обновления заказа
    df.part$UpdatedDate <- tmp[, 2]

    ### 12 столбец (заготовка) -- наименование электронной площадки
    df.part$PlatformName <- rep('', length(df.part$ContractID))

    ### 13 столбец (заготовка) -- объект закупки: наименование
    df.part$ObjectName <- rep('', length(df.part$ContractID))

    ## переходим по ссылке к описанию электронного аукциона
    details.URLs <- df.part$ContractHyperlink
    
    ## парсим детали
    for (i in 1:length(details.URLs)) {

        cat(green(paste0('i = ', frst.record.number + i - 1, ' of ', N,
                          ': ', details.URLs[i], '\n')))

        destHtml <- getURL(details.URLs[i], .encoding = 'UTF-8', ssl.verifypeer = FALSE,
                           .opts = list(useragent = USER.AGENT, followlocation = TRUE))

        ## разбираем как html
        descDoc <- htmlTreeParse(destHtml, useInternalNodes = T)
        ## корневой элемент
        descRootNode <- xmlRoot(descDoc)

        # всё содержимое табличек, потому что я не могу нормально
        #  запарсить по условию
        xpath.string <- "//div[@class = 'noticeTabBoxWrapper']/table//td"
        raw.notice <- xpathApplyAndClean(descRootNode, xpath.string)

        ### 12 столбец -- наименование электронной площадки
        search.tag <- 'Наименование электронной площадки'
        tag.val <- raw.notice[which(grepl(raw.notice, pattern = search.tag)) + 1]
        df.part$PlatformName[i] <- ifelse(length(tag.val) == 0, NA, tag.val)

        ### 13 столбец -- объект закупки: наименование
        search.tag <- 'Наименование закупки'
        tag.val <- raw.notice[which(grepl(raw.notice, pattern = search.tag)) + 1]
        if (length(tag.val) == 0) {
            search.tag <- 'Наименование объекта закупки'
            tag.val <- raw.notice[which(grepl(raw.notice, pattern = search.tag)) + 1]
        }
        df.part$ObjectName[i] <- ifelse(length(tag.val) == 0, NA, tag.val)

        ### 14 - 17 столбцы -- есть ли ограничения допуска
        xpath.string <- "//input"
        xpath.result <- data.frame(id = xpathApplyAndClean(descRootNode, xpath.string, 'attr', 'id'),
                                   value = xpathApplyAndClean(descRootNode, xpath.string, 'attr', 'disabled'))
        xpath.result <- xpath.result[!is.null(xpath.result$id), ]
        if (nrow(xpath.result) > 0) {
            xpath.result$id <- paste0('restr_', xpath.result$id)
            xpath.result$value <- ifelse(xpath.result$value == 'disabled', F, T)
            
            for (j in 1:nrow(xpath.result)) {
                col.num <- which(colnames(df.part) == xpath.result[j, 'id'])
                if (length(col.num) == 0) {
                    df.part$temp_name <- rep('', length(df.part$ContractID))
                    colnames(df.part)[colnames(df.part) == 'temp_name'] <- 
                        xpath.result[j, 'id']
                    
                    col.num <- which(colnames(df.part) == xpath.result[j, 'id'])
                    df.part[i, col.num] <- xpath.result[j, 'value']
                } else {
                    df.part[i, col.num] <- xpath.result[j, 'value']
                }
            }
        }
    }

    

    # ЗАПИСЬ РЕЗУЛЬТАТОВ -------------------------------------------------------
    
    file.name <- 
        gsub('___', paste0('_region_', 
                           df.params[df.params$ParamID == 'regions', 2],
                           '_', gsub('[.]', '-', df.params[df.params$ParamID == 'applSubmissionCloseDateFrom', 2]),
                           '_', gsub('[.]', '-', df.params[df.params$ParamID == 'applSubmissionCloseDateTo', 2]),
                           '_part_', curr.page.number), 
             CSV.FILE.PREFIX)
    
    region.csv.dir.path <- 
        paste0(CSV.DIR.PATH, 'code-', 
               df.params[df.params$ParamID == 'okpd2IdsCodes', 2],
               '_', gsub('\\s', '-', DF.REGIONS[DF.REGIONS$ID == df.params[df.params$ParamID == 'regions', 2], 1]), '/')
    if (!dir.exists(region.csv.dir.path)) {
        dir.create(region.csv.dir.path)
    }
    
    file.path <- paste0(region.csv.dir.path, file.name)
    write.csv2(df.part, file.path, row.names = F)
}



# ЗАПИСЬ ПАРАМЕТРОВ ПОИСКА -----------------------------------------------------

file.path <- paste0(region.csv.dir.path, 
                    paste0('parser_html_URL_params_', gsub('\\s|:', '_', Sys.time()),
                           '.csv'))
write.csv2(df.params, file.path, row.names = F)
