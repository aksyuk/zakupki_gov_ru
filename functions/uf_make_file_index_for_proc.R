# ..............................................................................
# uf_make_file_index_for_proc.R
#
# Функция для создания файла-индекса по процедуре с типами файлов, 
#  номерами заявок и номерами документов.
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия: 1.0 (08 Mar 2020)
# ******************************************************************************
# Аргументы:
#  * all.xmls      -- символьный вектор с именами всех файлов в sRawXMLPath
#  * key.col.names -- названия ключевых столбцов таблицы
#  * group.cols    -- номера столбцов таблицы с символами # для ускорения работы
#  * plot.filename -- файл, в который рисуем график изменение размера 
#  * plot.main     -- заголовок для графика
#
# Возвращаемое значение: таблица с индексом файлов
# 
# Создаёт / модифицирует файлы: 
#  нет
# ******************************************************************************
# Зависимости:
#  data.table
# ..............................................................................

uf.make.file.index.for.proc <- function(all.xmls.names, 
                                        path.to.raw.csvs = sRawCSVPath,
                                        proc.descr = lstProcedureType) {
    
    # # отладка
    # all.xmls.names = vAllXML
    # path.to.raw.csvs = sRawCSVPath
    # proc.descr = lstProcedureType
    
    DT <- all.xmls.names
    DT <- gsub('[.]xml', '', DT)
    DT <- data.table(do.call(rbind, strsplit(gsub('fcs_', 'fcs', DT), '_')))
    colnames(DT) <- c('prefix', 'purchaseNum', 'id')
    
    # противный префикс fcs_notificationEFDateChange портит всю обедню,
    #  но переименовывать исходники плохая идея
    DT$prefix[DT$prefix == 'fcsnotificationEFDateChange'] <- 
        'fcs_notificationEFDateChange'
    
    # # отладка
    # head(DT)
    # # проверка количества файлов
    # tmp <- cbind(DT[, -2][, .N, by = prefix][order(-N), ],
    #              sort(sapply(DT_XML_INDEX[, -1], sum), decreasing = T))
    # sapply(tmp[, -1], sum)
    
    # оставялем только id для электронных аукционов
    loop.ids <- c(unname(unlist(sapply(proc.descr$prefix, function(x) {
        grep(x, all.xmls.names, value = T)
    }))))
    loop.ids <- sub('^fcs_', '^fcs', loop.ids)
    # затем всё, что идёт до первого '_', включая его
    loop.ids <- sub('^(.*?)_', '', loop.ids)
    # теперь всё, что идёт от '_' и до конца
    loop.ids <- sub('_.*$', '', loop.ids)
    loop.ids <- unique(loop.ids)
    DT <- DT[purchaseNum %in% loop.ids, ]
    
    # # отладка
    # dim(DT)
    
    # записываем таблицу-индекс с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    flnm <- paste0(path.to.raw.csvs, 'DT_index_melt_', 
                   proc.descr$name, '.csv')
    uf.write.table.with.metadata(DT, flnm)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    return(DT)
}

