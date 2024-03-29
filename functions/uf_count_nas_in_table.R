# ..............................................................................
# uf_count_nas_in_table.R   
#
# Функция подсчёта пропусков в столбцах и строках таблицы.
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия: 1.0 (08 Mar 2020)
# ******************************************************************************
# Аргументы:
#  * DT            -- таблица с данными 
#
# Возвращаемое значение: список из двух векторов: 1) columns.na, количество 
#  пропусков с названиями столбцов таблицы; 2) rows.na, количество пропусков
#  с названиями строк таблицы. Возвращаются только те столбцы и строки, 
#  в которых есть пропуски.
# 
# Создаёт / модифицирует файлы: 
#  нет
# ******************************************************************************
# Зависимости:
#  нет
# ..............................................................................

uf.count.nas.in.table <- function(DT){
    
    columns.na <- sapply(DT, function(x){sum(is.na(x))})
    columns.na <- columns.na[columns.na > 0]
    
    rows.na <- unlist(apply(DT, 1, function(x){sum(is.na(x))}))
    rows.na <- rows.na[rows.na > 0]
    
    return(list(columns.na = columns.na, rows.na = rows.na))
}
