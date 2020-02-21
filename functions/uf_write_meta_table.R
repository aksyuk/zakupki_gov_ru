# uf.write.table.with.metadata() ...............................................
#
# Функция получает на вход таблицу, определяет классы столбцов и записывает
#  в csv-файлы две таблцы, исходную и таблицу с метаданными: имя столбца, 
#  тип столбца
#
# Аргументы:
#  * DT                     -- таблица, для которой создаём метаданные
#  * out.table.filename    -- имя сохранения таблицы
#
# Возвращаемое значение: TRUE, если выполнено без ошибок
# 
# Создаёт / модифицирует файлы: 
#  * пишет нормализованные кусочки таблицы в папку csvs.path, 
#    а после сборки итоговой таблицы стирает их
#
uf.write.table.with.metadata <- function(DT, out.table.filename) {
    
    res <- tryCatch({
        write.csv2(DT, out.table.filename, row.names = F)
        cls <- sapply(DT, class) 
        nms <- names(cls)
        df.meta <- data.frame(col.names = nms, col.classes = cls)
        rownames(df.meta) <- 1:nrow(df.meta)
        write.csv2(df.meta, paste0(gsub('[.]csv$', '', out.table.filename), 
                                   '_META.csv'),
                   row.names = F)
    },
    error = function(cond) {
        message("Запись файла не удалась")
        message("Сообщение об ошибке:")
        message(cond)
        return(FALSE)
    },
    warning = function(cond) {
        message("Запись файла вызвала предупреждение")
        message("Сообщение об ошибке:")
        message(cond)
        return(TRUE)
    },
    finally = {
        rm(DT)
    })
    
    return(res)
}
