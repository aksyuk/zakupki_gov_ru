# ..............................................................................
# uf_read_table_with_metadata.R
#
# Функция получает на вход имя файла и читает его, используя для определения 
#  классов столбцов одноимённый файл с суффиксом '_META'
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия: 1.0 (08 Mar 2020)
# ******************************************************************************
# Аргументы:
#  * file.name -- имя файла с данными
#
# Возвращаемое значение: таблица, если выполнено без ошибок; 
#                        NULL если были ошибки
# 
# Создаёт / модифицирует файлы: 
#  нет
# ******************************************************************************
# # Зависимости:
#  data.table
# ..............................................................................

uf.read.table.with.metadata <- function(file.name) {
    
    # # отладка
    # file.name <- file.full.name
    
    res <- tryCatch({
        
        ext <- gsub('(^.*)([.]csv)$', '\\2', file.name)
        meta.file.name <- paste0(gsub(paste0(ext, '$'), '', file.name),
                                 '_META', ext)
        df.meta <- read.csv2(meta.file.name, stringsAsFactors = F)
        
        if (uf.get.os() == 'windows') {
            DT <- data.table(read.csv2(file.name, stringsAsFactors = F,
                                       colClasses = df.meta$col.classes,
                                       fileEncoding = 'UTF-8'))
        } else {
            DT <- data.table(read.csv2(file.name, stringsAsFactors = F,
                                       colClasses = df.meta$col.classes))
        }
        DT
    },
    
    error = function(cond) {
        message("Не удалось прочесть файл")
        message("Сообщение об ошибке:")
        message(cond)
        NULL
    },
    warning = function(cond) {
        message("Чтение файла вызвало предупреждение")
        message("Текст сообщения:")
        message(cond)
        DT
    },
    finally = {
        rm(file.name)
    })
    
    return(res)
}
