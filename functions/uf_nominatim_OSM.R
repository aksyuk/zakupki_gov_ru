# ..............................................................................
# uf.nominatim.OSM()
#
# Функция для определения координат по адресу.
#
# Source: https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 
#
# Аргументы:
#  * address       -- адрес, ЕДИНИЧНЫЙ ВЕКТОР
#
# Возвращаемое значение: фрейм с широтой и долготой адреса.
# 
# Создаёт / модифицирует файлы: нет. 
# ..............................................................................

uf.nominatim.OSM <- function(address = NULL) {
    if (suppressWarnings(is.null(address))) return(data.frame())
    
    d <- tryCatch(
        jsonlite::fromJSON(
            gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
                 'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
        ), error = function(err) {
            msg <- paste0('Address caused an error: ', address, '\n',
                          'The original error message:\n', 
                          conditionMessage(err))
            message(msg)
            
            return(data.frame(long = -200, lat = -200))
        }
    )
    
    if (length(d) == 0) {
        return(data.frame(long = -200, lat = -200))
    }
    
    return(data.frame(long = as.numeric(d$lon), lat = as.numeric(d$lat)))
}
