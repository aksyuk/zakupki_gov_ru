# ..............................................................................
# uf.get.os()
#
# Функция определения ОС. Нужна при записи строк в текстовый файл.
#
# Source: source: https://www.r-bloggers.com/identifying-the-os-from-r/
#
# Аргументы: нет
#
# Возвращаемое значение: именованный вектор единичной длины в возможными 
#  значениями osx, linux или windows и именем элемента sysname
# 
# Создаёт / модифицирует файлы: нет
# ..............................................................................

uf.get.os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
            os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os))
            os <- "linux"
    }
    tolower(os)
}
