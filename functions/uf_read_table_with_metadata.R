# ..............................................................................
# uf.read.table.with.metadata()
#
# Функция получает на вход имя файла и читает его, используя для определения 
#  классов столбцов одноимённый файл с суффиксом '_META'
#
# Аргументы:
#  * file.name              -- имя файла с данными
#
# Возвращаемое значение: таблица, если выполнено без ошибок; 
#                        NULL если были ошибки
# 
# Создаёт / модифицирует файлы: 
#  нет
# ..............................................................................


uf.read.table.with.metadata <- function(file.name) {
    
    res <- tryCatch({
        
        ext <- gsub('(.*)([.]csv)$', '\\2', file.name)
        meta.file.name <- paste0(gsub(paste0(ext, '$'), '', file.name),
                                 '_META', ext)
        df.meta <- read.csv2(meta.file.name, stringsAsFactors = F)
        DT <- data.table(read.csv2(file.name, stringsAsFactors = F,
                                   colClasses = df.meta$col.classes))
        DT
    },
    
    error = function(cond) {
        message("Не удалось прочесть файл")
        message("Сообщение об ошибке:")
        message(cond)
        return(NULL)
    },
    warning = function(cond) {
        message("Чтение файла вызвало предупреждение")
        message("Текст сообщения:")
        message(cond)
        return(DT)
    },
    finally = {
        rm(file.name)
    })
    
    return(res)
}
