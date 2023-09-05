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
# Версия 1.8.0 (02 Sep 2023)
# 
# Эта часть кода распаковывает и индексирует xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ
# 
# КЛЮЧЕВЫЕ ПЕРЕМЕННЫЕ
#  * vAllXML            -- вектор с именами всех файлов из директории 
#                          с распакованными xml
#  * DT_XML_INDEX       -- data.table с индексом всех файлов xml: 
#                          строки по noticeID, столбцы по префиксам файлов,
#                          значение 1 показывает наличие файла, 0 отсутствие
#
# ..............................................................................



# ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК ----------------------------------------

# ..............................................................................
# ВНИМАНИЕ: загрузка архивов занимает ВРЕМЯ
#  и происходит с помощью запуска bash скрипта
#  Пока версия только для Ubuntu Linux
#
# Генерим bash-файл с командами для поиска и скачивания файлов с ftp-сервера


# Генерируем файл с командами для ОС ===========================================

sSubFolders <- c('contracts', 'protocols', 'notifications')
flnm.bash.download <- paste0(sDataSamplePath, 'load_from_ftp.sh')

if (uf.get.os() != 'windows') {
    
    txt.line <- paste0('#!/bin/bash \n# рабочая директория -- папка с выгрузкой по региону за период \n',
                       'cd ', getwd(), gsub('^[.]', '', sDataSamplePath))
    write_lines(txt.line, flnm.bash.download)
    
    txt.line <- paste0('# создаём текстовые файлы со списками содержимого ftp-сервера \n',
                       'touch ls.txt ls_by_period.txt load_archives.sh \n', 
                       ' > ls.txt > ls_by_period.txt > load_archives.sh')
    write_lines(txt.line, flnm.bash.download, append = T)
    
    txt.line <- paste0('# стучимся в ftp, перемещаемся в директорию региона \n', 
                       'lftp -u ', gsub(':', ',', s.USER.PWD.44), ' ', s.FTP.URL, ' << EOF \n',
                       'cd ./fcs_regions/', lst.REGION$name, '/')
    write_lines(txt.line, flnm.bash.download, append = T)
    
    for (sbf in sSubFolders) {
        txt.line <- paste0('# cкачиваем имена архивов из директории ', sbf, 
                           ' в файл ls.txt \n',
                           'cd ./', sbf, '\n',
                           'ls -lh >> ls.txt \n',
                           'cd ../')
        write_lines(txt.line, flnm.bash.download, append = T)
    }
    
    txt.line <- paste0('# закрываем соединение с ftp сервером \nexit \nEOF')
    write_lines(txt.line, flnm.bash.download, append = T)
    
    txt.line <- paste0('# фильтруем все имена архивов по s_YEAR_MON \n',
                       'grep "', uf.make.regex.from.year.mon(), '"',
                       ' ./ls.txt > ls_by_period.txt')
    write_lines(txt.line, flnm.bash.download, append = T)
    
    txt.line <- paste0("# пишем скрипт для поимённого скачивания только нужных файлов \n",
                       "echo -e '#!/bin/bash' > load_archives.sh\n",
                       "echo -e 'lftp -u", gsub(':', ',', s.USER.PWD.44), " ", 
                       s.FTP.URL, "<< EOF \ncd ./fcs_regions/", lst.REGION$name)
    for (sbf in sSubFolders) {
        txt.line <- paste0(txt.line, "\ncd ./", sbf, 
                           " \nset xfer:clobber on' >> load_archives.sh \n",
                           "awk -F'.*", gsub('s$', '', sbf), 
                           "' '{print $2}' ls_by_period.txt | awk '!/^$/' | sed -e 's/^/get ", 
                           gsub('s$', '', sbf), "/' >> load_archives.sh \n",
                           "echo -e 'cd ../")
    }
    txt.line <- paste0(txt.line, "' >> load_archives.sh \n",
                       "echo -e 'exit \nEOF' >> load_archives.sh \n")
    write_lines(txt.line, flnm.bash.download, append = T)
    
    txt.line <- paste0('# качаем \n', '#cd ./archives \n', 
                       '#../load_archives.sh \n')
    write_lines(txt.line, flnm.bash.download, append = T)
    
} else {
    # ToDo дописать под винду
}

# считаем суммарный объём архивов
count.size <- read_lines(paste0(sDataSamplePath, 'ls_by_period.txt'))
count.size <- round(sum(as.numeric(unname(sapply(count.size, function(x) {
    tmp <- strsplit(x, ' ')[[1]]
    tmp <- tmp[tmp != '']
    tmp[5]
})))) / 10^6, 1)

# дальше надо запустить скрипт
cat(yellow(paste0('Чтобы скачать архивы, запустите файл ',
                  flnm.bash.download, '\n',
                  'ВНИМАНИЕ: суммарный размер архивов ', count.size, ' Мбайт')))

# ..............................................................................



# РАСПАКОВКА -------------------------------------------------------------------

# ВНИМАНИЕ: иногда разархивирование средствами R выдаёт ошибку
#
# распаковывать надо средствами ОС в папку ./data/raw/xmls
#  распаковываем содержимое архивов из archives


# Генерируем файл с командами для ОС ===========================================

# создаём файл
flnm.bash.unzip <- paste0(sDataSamplePath, 'unzip_archives.sh')

if (uf.get.os() != 'windows') {
    
    # пишем в файл
    #  первая строка файла
    txt.line <- '#!/bin/bash'
    write_lines(txt.line, flnm.bash.unzip)
    
    # пишем в файл
    #  cd в папку с архивами
    txt.line <- paste0('cd ', getwd(), gsub('^[.]', '', sRawArchPath))
    write_lines(txt.line, flnm.bash.unzip, append = T)
    
    # пишем в файл
    #  распаковать архивы в папку xmls
    txt.line <- 'unzip -o "*.zip" -d ../xmls'
    write_lines(txt.line, flnm.bash.unzip, append = T)
    
    # удалить файлы с подписями (здорово экономит место)
    txt.line <- 'find ../xmls -name "*.sig" -print0 | xargs -0 rm'
    write_lines(txt.line, flnm.bash.unzip, append = T)
    
} else {
    #  должен быть установлен 7zip и добавлен в path
    #  чтобы добавить его в path, запустить в cmd:
    #     set PATH=%PATH%;C:\Program Files\7-Zip\
    #     echo %PATH%
    #     7z
    
    # открыть соединение
    con <- con <- file(flnm.bash.unzip, 'ab')
    
    # пишем в файл
    # спецсимволы начала файла в Windows 
    BOM <- charToRaw('\xEF\xBB\xBF')
    writeBin(BOM, con, endian = 'little')
    
    # пишем в файл
    #  первая строка файла
    txt.line <- '@ECHO OFF'
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # пишем в файл
    #  cd в папку с архивами
    txt.line <- paste0('cd /d "', getwd(), gsub('^[.]', '', sRawArchPath), '"')
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # пишем в файл
    #  распаковать архивы в папку xmls 
    txt.line <- paste0('7z x "./*.zip" -o"', getwd(), 
                       gsub('^[.]', '', sRawXMLPath), '" -y')
    writeBin(charToRaw(paste0(txt.line, '\r\n')), con, endian = 'little')
    
    # закрываем соединение
    close(con)
}


# Собственно распаковка ========================================================

# теперь сгенерированный файл надо запустить
cat(yellow('Для распаковки запустите файл ', flnm.bash.unzip))



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
        vAllXML <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
        
        # записываем имена всех xml-файлов в csv >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        write.csv2(data.frame(flnms = vAllXML), file = file.name, row.names = F)
        # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
        message('Готово')
        
    } else {
        # читаем сохранённый список имён xml-файлов из csv <<<<<<<<<<<<<<<<<<<<<
        vAllXML <- read.csv2(file.name, stringsAsFactors = F)[, 1]
        # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    }
    
    message(paste0('Файл ', file.name, ' прочитан'))
    
} else {
    # получаем имена всех файлов
    message('Получаю имена xml-файлов...')
    vAllXML <- grep(dir(sRawXMLPath), pattern = 'xml$', value = T)
    
    # записываем имена всех xml-файлов в csv >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    write.csv2(data.frame(flnms = vAllXML), file = file.name, row.names = F)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    message('Готово')
}

n <- length(vAllXML)
cat(yellow(paste0('В директории ', n, ' xml-файлов')))


# все уникальные номера извещений
#  сначала меняем 'fcs_' на 'fcs'
all.ids <- sub('^fcs_', '^fcs', vAllXML)
# затем всё, что идёт до первого '_', включая его
all.ids <- sub('^(.*?)_', '', all.ids)
# теперь всё, что идёт от '_' и до конца
all.ids <- sub('_.*$', '', all.ids)
all.ids <- unique(all.ids)
table(nchar(all.ids))
length(all.ids)

# все префиксы файлов
prefixes <- table(gsub('_$', '', gsub(vAllXML, pattern = '(\\d{19}|\\d{18}).*$', 
                                      replacement = '')))
# проверка (д.б. TRUE)
sum(prefixes) == length(vAllXML)


# Индексируем имена всех файлов ================================================
# таблица с именами всех файлов: имена столбцов по префиксам, первый столбец --
#  номера измещений, значения в остальных столбцах -- кол-во файлов
#  по связке (ID + префикс)
#
DT_XML_INDEX <- data.table(noticeID = all.ids)

# счётчик для очистки консоли
console.clean.count <- 0
# счётчики файлов для сообщений статуса
i.files.сount <- 0
n <- length(prefixes)

df.diff.IDs <- data.frame(fileID = NULL, tagID = NULL)

# цикл по уникальным префиксам файлов
for (pr_i in names(prefixes)) {

    # вытаскиваем все имена файлов с заданным префиксам
    tmp.v <- grep(paste0('^', pr_i, '_'), vAllXML, value = T)

    # вытаскиваем noticeID из имени файла
    noticeID <- sub('^fcs_', '^fcs', tmp.v)
    noticeID <- sub('^(.*?)_', '', noticeID)
    noticeID <- sub('_.*$', '', noticeID)

    # считаем частоты каждого noticeID по префиксу
    DT <- data.table(table(noticeID))

    # совмещаем по ключу noticeID, т.е. каждый раз добавляем по столбцу
    DT_XML_INDEX <-
        merge(DT_XML_INDEX, DT, by = 'noticeID', all.x = T)
    # там где получились пропуски, должны быть нули (этих noticeID 
    #  у предыдущих префиксов не было)
    DT_XML_INDEX[is.na(N), N := 0]
    # название столбца -- это текущий префикс файла
    colnames(DT_XML_INDEX)[colnames(DT_XML_INDEX) == 'N'] <- pr_i

    # счётчик файлов и статус в консоль
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
#  число пропусков по столбцам файла-индекса
check.na <- uf.count.nas.in.table(DT_XML_INDEX)
check.na

# число файлов в таблице-индексе должно сходиться с количеством файлов в папке
sum(DT_XML_INDEX[, -1]) == length(vAllXML)

#  сравниваем суммы по столбцам с количеством префиксов
test.compare <- data.frame(num.files = sapply(DT_XML_INDEX[, -1], sum), 
                           num.prefixes = as.vector(prefixes))
# префиксы, у которых количество файлов на noticeID, видимо, больше 1
tmp <- test.compare$num.files - test.compare$num.prefixes
names(tmp) <- rownames(test.compare)
tmp <- tmp[tmp > 0]
tmp

# записываем таблицу-индекс с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT_XML_INDEX, 
                             paste0(sRawCSVPath, 'DT_all_xml_files_index.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# читаем таблицу-индекс из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(sRawCSVPath, 'DT_all_xml_files_index.csv')
DT_XML_INDEX <- uf.read.table.with.metadata(flnm)
dim(DT_XML_INDEX)
str(DT_XML_INDEX)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

message(paste0('Уникальных номеров извещений в файле-индексе: ',
               length(unique(DT_XML_INDEX$noticeID))))

# вектор со всеми noticeIDs, которые соответствует типу процедуры
#  из lProcedureToScrap
slct <- DT_XML_INDEX %>% select(any_of(lstProcedureType$prefix))
slct <- as.vector(slct > 0)
loop.ids <- DT_XML_INDEX[slct, ]$noticeID
n <- length(unique(DT_XML_INDEX$noticeID))
message(paste0('Извещений по типу процедур "', lstProcedureType$label, 
               '": \n', length(loop.ids),
               ' (', round(length(loop.ids) / n * 100, 1), '%)'))
