# uf.find.file.names() .........................................................
#
# Функция генерирует названия файлов по строкам файла-индекса.
#
# Аргументы:
#  * DT.index    -- кусок таблицы с индексом файлов
#  * search.dir  -- вектор с именами файлов в папке с xml, по умолчанию all.xmls
#
# Возвращаемое значение: выдаёт в консоль сообщение с названиями файлов.
# 
#
uf.find.file.names <- function(DT.index, all.xmls.filenames = all.xmls){
    
    n <- nrow(DT.index)
    prompt.continue <- 'y'
    
    if (n > 10) {
        message(paste0('В таблице ', n, ' строк.'))
        prompt.continue <- readline('Продолжить? (y/n)')
    }
    
    if (prompt.continue == 'y') {
        apply(DT.index, 1, function(x){
            cat(red((paste0('notification ID: ', x[1], '\n'))))
            flnms <- paste0(paste0(sRawXMLPath, grep(x[1], all.xmls.filenames, 
                                                     value = T)),
                            collapse = '\n')
            message(flnms)
        })
    }
}
