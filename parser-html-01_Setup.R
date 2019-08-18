
# ..............................................................................
# parser-html-01_Setup.R
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
# Эта часть кода содержит все предварительные настройки

# ..............................................................................
# ВНИМАНИЕ!!!!!!
# ПРИ БОЛЬШОМ КОЛИЧЕСТВЕ ОБРАЩЕНИЙ НА САЙТЕ ГОСЗАКУПОК МОГУТ ЗАБАНИТЬ ПО IP
#
# Ограничение API: 500 записей при выгрузке вручную с сайта.
# Мы запрашиваем все записи с максимальным количеством на странице: 50,
#  потом листаем в цикле. Внимание: заказы быстро добавляются, в конце
#  стоит проверить таблицу на наличие дубликатов. Уникальный идентификатор --
#  номер заказа (ContractID).



# ПАКЕТЫ -----------------------------------------------------------------------

library('RCurl')
library('XML')
library('crayon')
library('data.table')



# КОНСТАНТЫ --------------------------------------------------------------------

## коды регионов из URL
DF.REGIONS <- read.csv2('./data/reference/df_regions_codes.csv', stringsAsFactors = F)

## имя браузера для маскировки запроса по getURL()
USER.AGENT <- 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko'

## имя файла для экспорта
CSV.FILE.PREFIX <- 'zakupki_gov_ru_html___.csv'

## папка для записи 
CSV.DIR.PATH <- gsub(':', '_', paste0('./data/raw/', Sys.Date(), '/'))
if (!dir.exists(CSV.DIR.PATH)) {
    dir.create(CSV.DIR.PATH)
}



# ФУНКЦИИ ----------------------------------------------------------------------

# Применить xpath запрос и почистить возвращаемый результат
#  от хвостовых пробелов и спецсимволов
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
    result <- gsub(result, pattern = '\n', replacement = '')
    # убираем пробелы по краям
    result <- gsub(result, pattern = "^\\s+|\\s+$", replacement = '')
    # возвращаем значения
    return(result)
}

# Вместо try-catch для результатов xpath запроса:
#  если результат возвращается нулевой, меняем его на NA
tryXpath <- function(xpath.result) {
    if (length(xpath.result) == 0) {
        xpath.result <- NA
    }
    return(xpath.result)
}

