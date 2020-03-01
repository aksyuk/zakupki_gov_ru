# ..............................................................................
# запускать после прогона указанного скрипта
# ..............................................................................



# Проверки для файла parser-ftp-03_Unzip-and-Index-fz44.R ----------------------

# проверка: количество знаков в noticeID (не потерялись ли ведущие нули?)
table(nchar(DT.xml.files.index$noticeID))
DT.xml.files.index$noticeID[nchar(DT.xml.files.index$noticeID) == 18][1:10]


# Анализ файла-индекса =========================================================
n <- nrow(DT.xml.files.index)
message(paste0('Всего уникальных id закупок за период: ', n))

# как среди них распределяются протоколы
sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], sum)[
    sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], sum) > 0]

# контракты не пересекаются с электронными аукционами
all.contracts.ids <- grep('^contract_', all.xmls, value = T)
all.contracts.ids <- gsub('contract_', all.contracts.ids, replacement = '')
all.contracts.ids <- gsub('_.*$', all.contracts.ids, replacement = '')
all.contracts.ids <- unique(all.contracts.ids)
length(all.contracts.ids)
loop.ids[loop.ids %in% all.contracts.ids]
