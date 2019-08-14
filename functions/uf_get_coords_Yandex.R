# uf.get.coords.Yandex() ............................................................
#
# Функция для определения координат по адресу через https API Яндекс карт.
#
# Аргументы:
#  * address       -- адрес, ЕДИНИЧНЫЙ ВЕКТОР
#
# Возвращаемое значение: фрейм с широтой и долготой адреса.
# 
# Создаёт / модифицирует файлы: нет. 
#
uf.get.coords.Yandex <- function(address) {
    
    # # отладка
    # address <- all.participants.addr[responsibleOrg.long == -200, 
    #                                  address.OSM][1]
    
    my.https <- paste0('https://geocode-maps.yandex.ru/1.x/?geocode=', address)
    my.https <- gsub('\\s+', '\\%20', my.https)
    
    latlong <- tryCatch({
        xmlData <- getURL(my.https)
        rootNode <- xmlTreeParse(xmlData, encoding = 'UTF-8', useInternalNodes = T)
        
        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootNode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
        names(ns)[names(ns) == ''] <- 'xmlns'
        ns['xmlns'] <- 'http://www.opengis.net/gml'
        
        xpathSApply(rootNode, path = '//xmlns:Point/xmlns:pos', xmlValue,
                    namespaces = ns)[1]
    }, error = function(err) {
        msg <- paste0('Address caused an error: ', address, '\n',
                      'The original error message:\n', 
                      conditionMessage(err))
        message(msg)
        
        return(data.frame(long = -200, lat = -200))
    }
    )
    
    if (length(latlong) == 0) {
        return(data.frame(long = -200, lat = -200))
    }
    
    return(data.frame(long = gsub(latlong, pattern = '\\s.*$', replacement = ''),
                      lat = gsub(latlong, pattern = '[^\\s]*\\s', replacement = ''),
                      stringsAsFactors = F))
}
