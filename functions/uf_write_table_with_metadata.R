# ..............................................................................
# uf.write.table.with.metadata()
#
# Функция получает на вход таблицу, определяет классы столбцов и записывает
#  в csv-файлы две таблцы, исходную и таблицу с метаданными: имя столбца, 
#  тип столбца
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия: 1.0 (08 Mar 2020)
# ******************************************************************************
# Аргументы:
#  * DT                     -- таблица, для которой создаём метаданные
#  * out.table.filename     -- имя сохранения таблицы
#  * write.metadata         -- сохранять ли в файл метаданные (по умолчанию T)
#
# Возвращаемое значение: TRUE, если выполнено без ошибок
# 
# Создаёт / модифицирует файлы: 
#  * сохраняет csv-файл с данными из первого аргумента фукнции; имя файла: 
#     второй аргумент функции
#  * создаёт файл с метаданными; столбцы: имя столбца, тип данных; имя файла: 
#     второй аргумент функции + '_META' перед расширением файла
# ******************************************************************************
# # Зависимости:
# 
# ..............................................................................

uf.write.table.with.metadata <- function(DT, out.table.filename,
                                         write.metadata = T) {
    
    res <- tryCatch({
        # таблица данных
        write.csv2(DT, out.table.filename, row.names = F)
        cat(green(paste0('Записан файл: ', out.table.filename, '\n')))
        
        # таблица метаданных
        if (write.metadata) {
            cls <- unlist(lapply(DT, function(x) {class(x)[1]}))
            nms <- names(cls)
            df.meta <- data.frame(col.names = nms, col.classes = cls)
            rownames(df.meta) <- 1:nrow(df.meta)
            out.metadata.filename <- paste0(gsub('[.]csv$', '', 
                                                 out.table.filename), '_META.csv')
            write.csv2(df.meta, out.metadata.filename, row.names = F)            
        }
        
        cat(green(paste0('Записан файл метаданных: ', out.metadata.filename, '\n')))
        
        TRUE
    },
    error = function(cond) {
        message("Запись файла не удалась")
        message("Сообщение об ошибке:")
        message(paste0(cond, '\n'))
        return(FALSE)
    },
    warning = function(cond) {
        message("Запись файла вызвала предупреждение")
        message("Текст сообщения:")
        message(paste0(cond, '\n'))
        return(TRUE)
    },
    finally = {
        rm(DT)
    })
    
    return(res)
}
