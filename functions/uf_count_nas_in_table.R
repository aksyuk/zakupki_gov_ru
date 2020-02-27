# uf.count.nas.in.table() ............................................................
#
# Функция подсчёта пропусков в столбцах и строках таблицы.
#
# Аргументы:
#  * DT            -- таблица с данными 
#
# Возвращаемое значение: список из двух векторов: 1) columns.na, количество 
#  пропусков с названиями столбцов таблицы; 2) rows.na, количество пропусков
#  с названиями строк таблицы. Возвращаются только те столбцы и строки, 
#  в которых есть пропуски.
# 
#
uf.count.nas.in.table <- function(DT){
    
    columns.na <- sapply(DT, function(x){sum(is.na(x))})
    columns.na <- columns.na[columns.na > 0]
    
    rows.na <- unlist(apply(DT, 1, function(x){sum(is.na(x))}))
    rows.na <- rows.na[rows.na > 0]
    
    return(list(columns.na = columns.na, rows.na = rows.na))
}
