# ..............................................................................
# uf_make_colnames_from_xpath.R
#
# Функция для создания имён столбцов из xpath-запросов для парсинга XML.
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия: 1.0 (08 Mar 2020)
# ******************************************************************************
# Аргументы:
#  * df.patt      -- фрейм с запросами по типам XML-файлов 
#                    (импортируется) из директории sRefPath
#
# Возвращаемое значение: список; первый элемент -- пространства имён XML-файлов;
#                        следующие элементы -- фрейма со столбцами запрос,
#                        имя колонки с данными; имена элементов -- по префиксам 
#                        XML-файлов
# 
# Создаёт / модифицирует файлы: 
#  нет
# ******************************************************************************
# Зависимости:
#  нет
# ..............................................................................

uf.make.colnames.from.xpath <- function(df.patt) {
    # преобразуем фрейм с шаблонами в список
    patterns <- as.list(df.patt)
    patterns <- c(list(ns = c('xmlns:', 'xmlns:', 'oos:', 'xmlns:',
                              'xmlns:', 'xmlns:')), patterns)
    patterns <- lapply(patterns, function(x){x[x != ""]})
    
    # делаем из шаблонов имена столбцов итоговой таблицы
    patterns[-1] <- lapply(patterns[-1], function(x) {
        x <- x[x != '']
        tmp <- gsub(x, pattern = 'xmlns[:]|oos[:]', replacement = '')
        tmp <- gsub(tmp, pattern = '[//]|[/]', replacement = '.')
        tmp <- gsub(tmp, pattern = '[|]', replacement = '.')
        tmp <- gsub(tmp, pattern = '^[.]*', replacement = '')
        tmp <- gsub(tmp, pattern = '[.]+', replacement = '.')
        tmp <- gsub(tmp, pattern = '[[].*[]]', replacement = '')
        tmp <- gsub(tmp, pattern = '^.*:', replacement = '')
        tmp <- gsub(tmp, pattern = '/.*:', replacement = '/')
        tmp <- gsub(tmp, pattern = '[.]$', replacement = '')
        tmp <- gsub(tmp, pattern = '[][]', replacement = '')
        tmp <- gsub(tmp, pattern = '\\)|\\(', replacement = '')
        tmp <- gsub(tmp, pattern = '([^\\|]*)\\|.*', replacement = '\\1')
        
        x <- data.frame(xml.pattern = x, stringsAsFactors = F)
        x$xpath.names <- as.character(tmp)
        
        x <- x
    })   
    
    return(patterns)
}
