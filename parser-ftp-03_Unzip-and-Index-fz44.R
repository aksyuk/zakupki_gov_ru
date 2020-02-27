
# 
# parser-ftp-03_Unzip-and-Index-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  http://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.2 (20.02.2020)
# 
# Эта часть кода распаковывает и индексирует xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ



# 1. РАСПАКОВКА ----------------------------------------------------------------

# распаковывать надо средствами ОС в папку ./data/raw/xml
#  распаковываем содержимое архивов из contracts, notifications, protocols

# bash (cd в папку с архивами): 
#  unzip \*.zip -d ../xmls


# 2. ИНДЕКСАЦИЯ ----------------------------------------------------------------

# # индексация: считаем файлы, которые относятся к уникальным ID извещений
# all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
# length(all.xmls)


# # записываем имена всех xml-файлов в csv >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# write.csv2(data.frame(flnms = all.xmls),
#            file = paste0(sRawArchPath, 'xlms_names_list.csv'), row.names = F)
# # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
all.xmls <- read.csv2(paste0(sRawArchPath, 'xlms_names_list.csv'),
                      stringsAsFactors = F)[, 1]
length(all.xmls)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# все уникальные номера извещений
#  сначала меняем 'fcs_' на 'fcs'
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
# проверка 
sum(prefixes) == length(all.xmls)


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
    tmp.v <- grep(paste0('^', pr_i, '_'), all.xmls, value = T)

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

# проверка .....................................................................
#  число пропусков по столбцам и строкам
uf.count.nas.in.table(DT.xml.files.index)

# число файлов в таблице-индексе должно сходиться с количеством файлов в папке
sum(DT.xml.files.index[, -1]) == length(all.xmls)

#  сравниваем суммы по столбцам с количеством префиксов
test.compare <- data.frame(num.files = sapply(DT.xml.files.index[, -1], sum), 
                           num.prefixes = as.vector(prefixes))
# префиксы, у которых количество файлов на noticeID, видимо, больше 1
tmp <- test.compare$num.files - test.compare$num.prefixes
names(tmp) <- rownames(test.compare)
tmp <- tmp[tmp > 0]
tmp


# записываем таблицу-индекс с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.xml.files.index, 
                             paste0(sRawCSVPath, 'DT_all_xml_files_index.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем индекс из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_all_xml_files_index_META.csv'),
                     stringsAsFactors = F)
DT.xml.files.index <- data.table(read.csv2(paste0(sRawCSVPath,
                                                  'DT_all_xml_files_index.csv'),
                                           stringsAsFactors = F,
                                           colClasses = df.meta$col.classes))
dim(DT.xml.files.index)
str(DT.xml.files.index)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


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
sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], sum)[
    sapply(DT.xml.files.index[noticeID %in% loop.ids, -1], sum) > 0]

# контракты не пересекаются с электронными аукционами
all.contracts.ids <- grep('^contract_', all.xmls, value = T)
all.contracts.ids <- gsub('contract_', all.contracts.ids, replacement = '')
all.contracts.ids <- gsub('_.*$', all.contracts.ids, replacement = '')
all.contracts.ids <- unique(all.contracts.ids)
length(all.contracts.ids)
loop.ids[loop.ids %in% all.contracts.ids]
