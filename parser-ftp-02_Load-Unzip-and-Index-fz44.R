# ..............................................................................
# parser-ftp-02_Load-Unzip-and-Index-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  ftp://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.3.1 (05 Mar 2020)
# 
# Эта часть кода распаковывает и индексирует xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ
# 
# КЛЮЧЕВЫЕ ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ
#  * all.xmls           -- вектор с именами всех файлов из директории 
#                          с распакованными xml
#  * DT.xml.files.index -- data.table с индексом всех файлов xml: 
#                          строки по noticeID, столбцы по префиксам файлов,
#                          значение 1 показывает наличие файла, 0 отсутствие
#  * loop.ids           -- вектор со всеми noticeIDs, которые соответствует
#                          типу процедуры из lProcedureToScrap
# ..............................................................................



# ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК ----------------------------------------

# ..............................................................................
# ВНИМАНИЕ: загрузка архивов занимает ВРЕМЯ
# ЕЩЁ ВНИМАНИЕ:
#  Закачку надо делать средствами FTP-менеджера, который поддерживает 
#   докачку файлов, например, FileZilla. Массовая загрузка с ftp с помощью R 
#   это долго и чревато ошибками, особенно при плохом интернет-соединении.
# С адреса:
my.region$url
# -- грузим содержимое подпапок contracts, notifications, protocols
#     в папку archives
# ..............................................................................



# РАСПАКОВКА -------------------------------------------------------------------

# ВНИМАНИЕ: иногда разархивирование средствами R выдаёт ошибку
#
# распаковывать надо средствами ОС в папку ./data/raw/xmls
#  распаковываем содержимое архивов из archives


# Генерируем файл с командами для ОС ===========================================
if (uf.get.os() != 'windows') {
    # создаём файл
    fn.bash.unzip <- paste0(sDataSamplePath, 'bash_unzip')
    file.create(fn.bash.unzip)
    
    # открыть соединение
    con <- con <- file(fn.bash.unzip, 'ab')
    
    # пишем в файл
    #  первая строка файла
    txt.line <- '#!/bin/bash'
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # пишем в файл
    #  cd в папку с архивами
    txt.line <- paste0('cd ', getwd(), gsub('^[.]', '', sRawArchPath))
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # пишем в файл
    #  распаковать архивы в папку xmls
    txt.line <- 'unzip -o "*.zip" -d ../xmls'
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # закрываем соединение
    close(con)
    
} else {
    # TODO написать кусок под Windows
    
    # создаём файл
    
    # # открыть соединение
    # con <- con <- file(fn.bash.unzip, 'ab')
    
    # # спецсимволы начала файла в Windows 
    # BOM <- charToRaw('\xEF\xBB\xBF')
    # writeBin(BOM, con, endian = 'little')
}


# Непосредственно распаковка ===================================================

# теперь сгенерированный файл надо запустить
cat(yellow('Для распаковки запустите файл ', fn.bash.unzip))



# ИНДЕКСАЦИЯ -------------------------------------------------------------------

# индексация: считаем файлы, которые относятся к уникальным ID извещений
file.name <- paste0(sDataSamplePath, 'xlms_names_list.csv')
# проверяем, нет ли уже такого файла, и предлагаем пользователю прочитать его
#  либо сделать новый 
if (file.exists(file.name)) {
    
    # ///////////////////////ВВОД ДАННЫХ В КОНСОЛЬ//////////////////////////////
    df.msg <- data.frame(n = 1:2, opt = c('Проиндексировать заново и обновить',
                                          'Прочесть сохранённый'))
    message('Есть список xml-файлов, сохранённый ранее. Что сделать?\n', 
            paste(apply(df.msg, 1, function(x) {paste(x, collapse = '. ')}),
                  collapse = '\n'))
    # prompt.proc.type <- readline('Введите номер опции:')
    # быстрая опция
    prompt.proc.type <- 2
    # //////////////////////////////////////////////////////////////////////////
    
    cat(yellow(paste0('Выбрано: ', prompt.proc.type, '. ', 
                      df.msg[prompt.proc.type, 2], '\n')))
    
    if (prompt.proc.type == 1) {
        # получаем имена всех файлов
        message('Получаю имена xml-файлов...')
        all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
        
        # записываем имена всех xml-файлов в csv >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        write.csv2(data.frame(flnms = all.xmls), file = file.name, row.names = F)
        # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
        message('Готово')
        
    } else {
        # читаем сохранённый список имён xml-файлов из csv <<<<<<<<<<<<<<<<<<<<<
        all.xmls <- read.csv2(file.name, stringsAsFactors = F)[, 1]
        # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    }
    
    message(paste0('Файл ', file.name, ' прочитан'))
    
} else {
    # получаем имена всех файлов
    message('Получаю имена xml-файлов...')
    all.xmls <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
    
    # записываем имена всех xml-файлов в csv >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    write.csv2(data.frame(flnms = all.xmls), file = file.name, row.names = F)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    message('Готово')
}

n <- length(all.xmls)
cat(yellow(paste0('В директории ', n, ' xml-файлов')))


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
# проверка (д.б. TRUE)
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

    # # очистка консоли
    # if (console.clean.count == iMaxConsoleStatusLines) {
    #     cat("\014")
    #     console.clean.count <- 0
    # }
}

# проверка .....................................................................
#  число пропусков по столбцам 
check.na <- uf.count.nas.in.table(DT.xml.files.index)
check.na

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
DT.xml.files.index <- uf.read.table.with.metadata(paste0(sRawCSVPath, 
                                                'DT_all_xml_files_index.csv'))
dim(DT.xml.files.index)
str(DT.xml.files.index)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

message(paste0('Уникальных номеров извещений в файле-индексе: ',
               length(unique(DT.xml.files.index$noticeID))))

# noticeID по типу процедуры 
slct <- as.vector(select(DT.xml.files.index, lProcedureToScrap$noticeFileName) > 0)
loop.ids <- DT.xml.files.index[slct, ]$noticeID
n <- length(unique(DT.xml.files.index$noticeID))
message(paste0('Извещений по типу процедур: ', lProcedureToScrap$procedureType, 
               ' ', length(loop.ids),
               ' (', round(length(loop.ids) / n * 100, 1), '%)'))
