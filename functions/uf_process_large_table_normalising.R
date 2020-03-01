# ..............................................................................
# uf.process.large.table.normalising()
#
# Функция для нормализации большой таблицы по кускам. Разбиваем на много таблиц 
#  поменьше, нормализуем по отдельности, пишем в директорию для csv-файлов.
#  Затем проходим по записанным файлам и сбиваем всё вместе.
#
# Аргументы:
#  * DT                     -- таблица для нормализации
#  * out.total.table.name   -- имя для итоговой нормализованной таблицы
#  * dt.step                -- сколько строк будет в каждом кусочке таблицы
#  * csvs.path              -- путь в папку для сохранения csv
#  * max.console.lines      -- максимум строк в консоли
#
# Возвращаемое значение: нормализованная таблица данных.
# 
# Создаёт / модифицирует файлы: 
#  * пишет нормализованные кусочки таблицы в папку csvs.path, 
#    а после сборки итоговой таблицы стирает их
# ..............................................................................

uf.process.large.table.normalising <- function(DT, out.total.table.name,
                                               csvs.path,
                                               dt.step = 100, 
                                               max.console.lines = iMaxConsoleStatusLines) {
    # счётчик строк в консоли    
    console.clean.count <- 0    
    
    # папка для временных файлов
    csvs.path.tmp <- paste0(csvs.path, 'tmp/')
    if (!dir.exists(csvs.path.tmp)) {
        dir.create(csvs.path.tmp)
    }
        
    # общее количество табличек по dt.step строк
    dt.count <- nrow(DT) %/% dt.step
    if (nrow(DT) / dt.step != nrow(DT) %/% dt.step) {
        dt.count <- dt.count + 1
    }
    # первая и последняя строки первого кусочка большой таблицы
    fst.row <- 1
    lst.row <- fst.row + dt.step - 1
        
    # цикл по номеру таблички
    for (file.count in 1:dt.count) {
            
        # берём кусочек исходной таблицы
        df <- data.frame(DT[fst.row:lst.row, ])
        rownames(df) <- fst.row:lst.row
            
        # запускаем нормализацию
        df <- uf.normalise.table(df)
        df <- df$data
            
        if (!is.null(df)) {
            # пишем результат на диск
            out.file.name <- paste0(csvs.path.tmp, 'DF_curr_part_', 
                                    formatC(file.count, width = 3, 
                                            format = 'd', flag = '0'), '.csv')
            # сообщение в консоль
            message(paste0('Записываю файл ', out.file.name, '... (', file.count,
                           ' из ', dt.count, ').'))
            console.clean.count <- console.clean.count + 1
            # непосредственно запись в файл
            write.csv2(df, out.file.name, row.names = F)
                
            # обновляем счётчики строк
            fst.row <- lst.row + 1
            lst.row <- fst.row + dt.step - 1
                
            # очищаем консоль
            if (console.clean.count == max.console.lines * 2) {
                cat("\014")
                console.clean.count <- 0
            }            
        }
    }
        
    # все имена табличек на диске
    flnms <- paste0(csvs.path.tmp, grep('DF_curr_part_.*[.]csv', 
                                        dir(csvs.path.tmp), value = T))
        
    # сбить всё в один файл
    f.count <- 1
    n <- length(flnms)
        
    # цикл по именам файлов
    for (f in flnms) {
        message(paste0('Reading file ', f, '... (', f.count, ' из ', n,')'))
        console.clean.count <- console.clean.count + 1
            
        DT.part <- read.csv2(f, colClasses = 'character')
            
        if (f.count == 1) {
            DT.all <- copy(DT.part)
        } else {
            DT.all <- data.table(rbind(DT.all, DT.part))
        }
            
        message(paste0('Table size: ', 
                       round(object.size(DT.all) / 1024 / 1024, 1), 
                       ' Mb. nrow(DT.all) = ', nrow(DT.all), '\n'))
        console.clean.count <- console.clean.count + 1
            
        # обновляем счётчики
        f.count <- f.count + 1
            
        # очищаем консоль
        if (console.clean.count == max.console.lines * 2) {
            cat("\014")
            console.clean.count <- 0
        }
    }
        
    # убираем полностью пустые строки
    DT.all <- DT.all[!apply(DT.all, 1, 
                            function(x){sum(is.na(x))}) == ncol(DT.all), ]
    
    # записываем общую таблицу
    out.file.name <- paste0(csvs.path, out.total.table.name)
    uf.write.table.with.metadata(DT.all, out.file.name)
        
    # удаляем временные файлы
    file.remove(flnms)
    rm(DT)
        
    return(DT.all)
}
