
# 
# parser-ftp-03_Unzip-and-Index.R
# 
# Парсинг содержимого необъятного ftp сервера госзакупок, 
#  который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.1 (16.07.2019)
# 
# Эта часть кода распаковывает и индексирует xml-файлы, скачанные с FTP    
# 



# 1. РАСПАКОВКА ----------------------------------------------------------------

# распаковывать надо средствами ОС в папку ./data/raw/xml
#  распаковываем содержимое архивов из contracts, notifications, protocols



# 2. ИНДЕКСАЦИЯ ----------------------------------------------------------------

# индексация: считаем файлы, которые относятся к уникальным ID извещений
all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
length(all.xmls)

write.csv2(data.frame(flnms = all.xmls),
           file = paste0(sRawArchPath, 'xlms_names_list.csv'), row.names = F)
 
# all.xmls <- read.csv2(paste0(sRawArchPath, 'xlms_names_list.csv'),
#                       stringsAsFactors = F)[, 1]
# length(all.xmls)

# все уникальные номера извещений
#  сначала убираем 'fcs_'
all.ids <- sub('^fcs_', '^fcs', all.xmls)
# затем всё, что идёт до первого '_', включая его
all.ids <- sub('^(.*?)_', '', all.ids)
# теперь всё, что идёт от '_' и до конца
all.ids <- sub('_.*$', '', all.ids)
all.ids <- unique(all.ids)
table(nchar(all.ids))
length(all.ids)

# все префиксы файлов
prefixes <- table(gsub('_$', '', gsub(all.xmls, pattern = '(\\d{19}|\\d{18}).*$', 
                                      replacement = '')))
prefixes
length(prefixes)


# Индексируем имена всех файлов ================================================
# таблица с именами всех файлов: имена столбцов по префиксам, первый столбец --
#  номера измещений, значения в остальных столбцах -- кол-во файлов
#  по связке (ID + префикс)
#
DT.xml.files.index <- data.table(noticeID = all.ids)

# счётчик для очистки консоли
console.clean.count <- 0
# счётчики файлов для сообщений статуса
i.files.сount <- 0
n <- length(prefixes)

df.diff.IDs <- data.frame(fileID = NULL, tagID = NULL)

# цикл по уникальным префиксам файлов
for (pr_i in names(prefixes)) {

    # вытаскиваем все имена файлов с заданным префиксам
    tmp.v <- grep(all.xmls, pattern = paste0('^', pr_i), value = T)

    # вытаскиваем noticeID из имени файла
    noticeID <- sub('^fcs_', '^fcs', tmp.v)
    noticeID <- sub('^(.*?)_', '', noticeID)
    noticeID <- sub('_.*$', '', noticeID)

    DT <- data.table(table(noticeID))

    DT.xml.files.index <-
        merge(DT.xml.files.index, DT, by = 'noticeID', all.x = T)
    DT.xml.files.index[is.na(N), N := 0]
    colnames(DT.xml.files.index)[colnames(DT.xml.files.index) == 'N'] <- pr_i

    i.files.сount <- i.files.сount + 1
    message(paste0(pr_i, ', ', round(i.files.сount / n * 100, 1), '%'))
    console.clean.count <- console.clean.count + 1

    # очистка консоли
    if (console.clean.count == iMaxConsoleStatusLines) {
        cat("\014")
        console.clean.count <- 0
    }
}

# проверка
sapply(DT.xml.files.index, function(x){sum(is.na(x))})
summary(DT.xml.files.index)
table(nchar(DT.xml.files.index$noticeID ))

# записываем таблицу-индекс
write.csv2(DT.xml.files.index, paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
           row.names = F)

# читаем весь индекс одним файлом
DT.xml.files.index <- read.csv2(paste0(sRawCSVPath, 'df_all_xml_files_index.csv'),
                           stringsAsFactors = F,
                           colClasses = c('character', rep('numeric', 55)),
                           encoding = 'CP-1251')
DT.xml.files.index <- data.table(DT.xml.files.index)
dim(DT.xml.files.index)
str(DT.xml.files.index)

message(paste0('Уникальных номеров извещений в файле-индексе: ',
               length(unique(DT.xml.files.index$noticeID))))

# проверка: количество знаков в noticeID (не потерялись ли ведущие нули?)
table(nchar(DT.xml.files.index$noticeID))
DT.xml.files.index$noticeID[nchar(DT.xml.files.index$noticeID) == 18][1:10]


# Анализ файла-индекса =========================================================
n <- nrow(DT.xml.files.index)
message(paste0('Всего уникальных id закупок за период: ', n))

# аукционы
loop.ids <- DT.xml.files.index[DT.xml.files.index$fcsNotificationEA44 > 0, ]$noticeID
message(paste0('Извещений об электронных аукционах: ', length(loop.ids),
               ' (', round(length(loop.ids) / n * 100, 1), '%)'))

# как среди них распределяются протоколы
sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max)[
    sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], max) > 0]

all.contracts.ids <- grep(all.xmls, pattern = '^contract_', value = T)
all.contracts.ids <- gsub(all.contracts.ids, pattern = 'contract_', 
                          replacement = '')
all.contracts.ids <- gsub(all.contracts.ids, pattern = '_.*$', 
                          replacement = '')
all.contracts.ids <- unique(all.contracts.ids)
length(all.contracts.ids)

# парсим файлы с аукционами (цикл по номерам извещений)
# выкидываем id без файлов contract -- это документы для заключенных ранее
loop.ids <- DT.xml.files.index[contract != 0, ]$contract
table(loop.ids)

# теперь разбираемся: что за id со 169 контрактами?
ids <- DT.xml.files.index[contract == 169, ]$noticeID

files <- NULL
for (id in ids) {
    tmp <- grep(all.xmls, pattern = id, value = T)
    tmp <- gsub(tmp, pattern = 'contract_', replacement = '')
    tmp <- grep(all.xmls, pattern = '^contract_', value = T)
    files <- c(files, )
    message(paste0(id, ': ', paste0(tmp, collapse = "; ")))
    
    if (length(tmp) > 0) {
        files <- c(files, tmp)
    }
}
# ожидаемо отличаются второй группой цифр в названии файлов

