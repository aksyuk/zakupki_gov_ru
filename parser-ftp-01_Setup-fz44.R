# ..............................................................................
# parser_ftp_01-Setup-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  http://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.3.2 (06 May 2020)
# 
# Эта часть скрипта содержит все предварительные настройки
#  и работает с сервером по 44 ФЗ
# ..............................................................................



# Загрузка библиотек -----------------------------------------------------------
library('RCurl')
library('XML')
library('dplyr')
library('reshape')
library('data.table')
library('stringi')
library('ggplot2')
library('httr')
library('readr')
library('jsonlite')
library('RJSONIO')
library('crayon')
library('lubridate')



# Настройки системы ------------------------------------------------------------
# отображение больших цифр в обычном формате, а не в экспоненциальном
options("scipen" = 100, "digits" = 9)



# Функции ----------------------------------------------------------------------

# функция преобразования строковых дат в формат даты, с часовыми поясами
eval(parse('./functions/uf_convert_char_to_date.R', encoding = 'UTF-8'))

# функция подсчёта пропусков в столбцах таблицы
eval(parse('./functions/uf_count_nas_in_table.R', encoding = 'UTF-8'))

# # функция формата адреса поставщика, сейчас не используется
# eval(parse('./functions/uf_format_address_for_OSM.R', encoding = 'UTF-8'))

# # функция для работы с адресами, сейчас не используется
# eval(parse('./functions/uf_nominatim_OSM.R', encoding = 'UTF-8'))

# # функция загрузки координат по адресу черех Яндекс карты, 
# #  сейчас не используется
# eval(parse('./functions/uf_get_coords_Yandex.R', encoding = 'UTF-8'))

# # функция преобразования адреса в координаты, сейчас не используется 
# eval(parse('./functions/uf_process_addresses_to_coords.R', encoding = 'UTF-8'))

# функция определения операционной системы
eval(parse('./functions/uf_get_os.R', encoding = 'UTF-8'))

# функция, которая преобразовывает xpath-запросы для отбора xml-тегов
#  в заголовки таблицы с данными
eval(parse('./functions/uf_make_colnames_from_xpath.R', encoding = 'UTF-8'))

# функция создания индекса xml-файлов для типа процедуры 
#  (электронные аукционы)
eval(parse('./functions/uf_make_file_index_for_proc.R', encoding = 'UTF-8'))

# функция нормализация таблицы: расклеивает несколько значений в одной 
#  "ячейке", соединённые символом решётки (#)
eval(parse('./functions/uf_normalise_table.R', encoding = 'UTF-8'))

# функция, которая разбирает xml-файлы, опираясь на префикс в названии
#  файла и на список xpath-запросов для разбора файлов с таким префиксом
eval(parse('./functions/uf_parse_xmls_with_prefix.R', encoding = 'UTF-8'))

# функция нормализации большой таблицы (режет на куски, разбирает по частям)
eval(parse('./functions/uf_process_large_table_normalising.R', encoding = 'UTF-8'))

# функция чтения таблицы данных и таблицы метаданных к ней
eval(parse('./functions/uf_read_table_with_metadata.R', encoding = 'UTF-8'))

# функция записи таблицы данных и таблицы метаданных к ней
eval(parse('./functions/uf_write_table_with_metadata.R', encoding = 'UTF-8'))

# функция записи в текстовый лог
eval(parse('./functions/uf_write_to_log.R', encoding = 'UTF-8'))



# Константы --------------------------------------------------------------------

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10

# координаты FTP-сервера госзакупок и логин-пароль для бесплатного доступа
sFTPURL <- 'ftp://ftp.zakupki.gov.ru/'
sUserPwd44 <- 'free:free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
sYEAR <- paste0(rep(2019, 12), formatC(1:12, width = 2, flag = '0'))
# sYEAR <- paste0(rep(2020, 2), formatC(1:2, width = 2, flag = '0'))

# часть названия региона для поиска
# 01
srch.reg <- 'Bashk'
# 03 /это не ошибка/
# srch.reg <- 'Udmu'
# 04
# srch.reg <- 'Permsk'
# 05
# srch.reg <- 'Kirov'
# 06
# srch.reg <- 'Marij'
# 07
# srch.reg <- 'Mordo'
# 08
# srch.reg <- 'Nizhegorod'
# 09
# srch.reg <- 'Orenb'
# 10
# srch.reg <- 'Penz'
# 11
# srch.reg <- 'Samars'
# 12
# srch.reg <- 'Saratov'
# 13
# srch.reg <- 'Tatar'
# 14
# srch.reg <- 'Uljan'
# 15
# srch.reg <- 'Chuvash'


# Список директорий с регионами ================================================

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
# создаём список вариантов для выбора
vars <- data.frame(n = 1:2, txt = c('Перезагрузить с ftp', 
                                    'Прочитать сохранённый'))
# показываем варианты пользователю
message(paste0('Загрузка списка регионов:\n',
               paste0(apply(vars, 1, function(x){paste0(x, collapse = '. ')}),
                      collapse = '\n')))

# в этой строке читаем выбор пользователя
# prompt.load.reg.list <- readline('Введите номер опции:')

# быстрая опция 
prompt.load.reg.list <- 2

# показываем результат выбора в консоли
cat(yellow(paste0('Выбрано: ', vars[prompt.load.reg.list, 2], '\n')))
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

if (prompt.load.reg.list == 1) {
    doc <- getURL(paste0(sFTPURL, 'fcs_regions/'), 
                  ftp.use.epsv = FALSE, dirlistonly = TRUE, 
                  userpwd = sUserPwd44)
    # отладка
    # write(doc, 'tmp.txt')
    # на ftp госзакупок разместили под списком регионов гиганский список логов
    #  поэтому его надо отрезать
    doc <- gsub(pattern = '_logs.*$', replacement = '', doc)
    
    sRegionFoldersNames <- unlist(strsplit(doc, '\r\n'))
    # убираем папки, не относящиеся к регионам
    sRegionFoldersNames <- sRegionFoldersNames[grep('_Resp$|_kraj$|_.?obl$', 
                                                    sRegionFoldersNames)]
    write(sRegionFoldersNames, './data/reference/regions_list.txt')
}

if (file.exists('./data/reference/regions_list.txt')) {
    sRegionFoldersNames <- scan('./data/reference/regions_list.txt',
                                character())
    } else {
        message(paste0('Файл', '"regions_list.txt"', 'не найден в ', 
                       './data/reference/', '. Повторите загрузку с сервера.'))
}

message(paste0('Regions: ', length(sRegionFoldersNames), ' folders.'))

# преобразуем папки регионов в URL-адреса
sRegionFolderURLs <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            sRegionFoldersNames, '/')

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')


# Структура директорий папки с данными =========================================

# все равки исходных данных ####################################################
sRawDataPath <- './data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# исходные данные в архивах, выгрузка за период ################################
dirs.raw <- dir('./data/raw')
n.dirs <- length(dirs.raw)
regs.by.readme <- rep('', n.dirs)
if (n.dirs > 0) {
    # если папка './data/raw' не пуста, считаем папки внутри
    msg <- paste0(1:n.dirs, '. ', dirs.raw)
    
    # Найти директорию по заданной в srch.reg части названия региона ###############
    #  используем информацию из README.txt в директориях загрузки
    
    # ищем в README наш регион
    for (d in dirs.raw) {
        rdm <- read_lines(paste0(sRawDataPath, '/', d, '/README.txt'))
        regs.by.readme[dirs.raw == d] <- gsub('Регион: ', '', rdm[1])
    } 
    
    # добавляем в список опций для выбора
    msg <- paste(msg, 'регион:', regs.by.readme, '\n')
    
} else {
    msg <- NULL
}

msg <- c(msg, paste0(n.dirs + 1, '. Создать новую выгрузку\n'))
if (n.dirs > 0) {
    # ToDo: сделать нормальную проверку, если ли такая папка в выгрузках
    msg <- c(msg, paste0(n.dirs + 2, '. Выбрать автоматически по названию региона и периоду: ', 
                         srch.reg, ' ', sYEAR[1], '-', sYEAR[length(sYEAR)], '\n'))
}
    
# Выбрать директорию вручную или создать новую #################################

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
message('Выберите выгрузку:\n', msg)
prompt.load.sample <- readline('Введите номер опции:')
# быстрая опция: новая выгрузка
# prompt.load.sample <- n.dirs + 1
# быстрая опция: выбрать по названию региона
# prompt.load.sample <- n.dirs + 2
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

if (prompt.load.sample == n.dirs + 1) {
    # определяем порядковый номер для папки новой выгрузки
    if (n.dirs == 0) {
        new.count <- formatC(1, width = 2, flag = '0')
    } else {
        new.count <- as.numeric(gsub('^([[:digit:]]{2})(_.*)', '\\1', dirs.raw))
        new.count <- formatC(new.count[length(new.count)] + 1, width = 2, flag = '0')
    }
    
    # формат пути к папке с новой выгрузкой: <директория с равками>/ 
    #  <порядковый номер выгрузки>_from<начало периода выгрузки в формате 
    #  YYYYMM>to<конец периода выгрузки в формате YYYYMM>_loaded<дата загрузки 
    #  данных в формате YYYY-MM-DD>/
    sDataSamplePath <- paste0('./data/raw/', new.count, '_from',
                           sYEAR[1], 'to', sYEAR[length(sYEAR)], '_loaded', 
                           format(Sys.Date(), format = "%Y-%m-%d"), '/')
    if (!dir.exists(sDataSamplePath)) dir.create(sDataSamplePath) 
    
    # пишем параметры данных в README.txt
    flnm <- paste0(sDataSamplePath, 'README.txt')
    msg <- paste0('Регион: ', my.region$name, '\n',
                  'Период: с ', sYEAR[1], ' по ', sYEAR[length(sYEAR)], '\n',
                  'Тип процедуры: ', all.proc.types[prompt.proc.type, 2], '\n',
                  'Дата загрузки: ', format(Sys.Date(), format = "%Y-%m-%d"))
    # Encoding(msg) <- 'windows-1251'
    uf.write.to.log(msg, out.file.name = flnm, silent = T)
    
} else if (prompt.load.sample == n.dirs + 2) {
    # ищем в README наш регион
    for (d in dirs.raw) {
        rdm <- read_lines(paste0(sRawDataPath, '/', d, '/README.txt'))
        
        if (length(grep(srch.reg, rdm[1])) > 0 & 
                length(grep(sYEAR[1], rdm[2])) & 
                length(grep(sYEAR[length(sYEAR)], rdm[2]))) {
            
            sDataSamplePath <- paste0('./data/raw/', d, '/')
            break
        }
    } 
    
} else {
    # выбираем выгрузку, сделанную ранее
    sDataSamplePath <- paste0('./data/raw/', 
                           dirs.raw[as.numeric(prompt.load.sample)], '/')
}

cat(yellow(paste0('Выбрано: ', prompt.load.sample, ';\n',
                  'sDataSamplePath = ', sDataSamplePath, '\n')))

# исходные данные в архивах ....................................................
sRawArchPath <- paste0(sDataSamplePath, 'archives/')
if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- paste0(sDataSamplePath, 'xmls/')
if (!dir.exists(sRawXMLPath)) dir.create(sRawXMLPath)

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- paste0(sDataSamplePath, 'csv/')
if (!dir.exists(sRawCSVPath)) dir.create(sRawCSVPath)

# таблицы-справочники ..........................................................
sRefPath <- './data/reference/'
if (!dir.exists(sRefPath)) dir.create(sRefPath)

# графики ......................................................................
sPlotPath <- './plots/'
if (!dir.exists(sPlotPath)) dir.create(sPlotPath)

# логи
sLogPath <- './logs/'
if (!dir.exists(sLogPath)) dir.create(sLogPath)



# Переменные -------------------------------------------------------------------

#  регион (регионы)
my.region <- list(url = grep(sRegionFolderURLs, pattern = srch.reg, value = T))
my.region$name <- gsub('.*[/]', '', gsub('[/]$', '', my.region$url))
cat(yellow(paste0('Работаем с регионом: ', my.region$name, '\n')))

# все типы процедур
#  на самом деле там пока только электронные аукционы
all.proc.types <- read.csv2(paste0(sRefPath, 'dt_procedure_types.csv'),
                            stringsAsFactors = F, fileEncoding = 'cp1251')

msg <- paste0(1:nrow(all.proc.types), '. ', all.proc.types$procedureType)

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
message('Выберите процедуры:\n', msg)
# prompt.proc.type <- readline('Введите номер опции:')
# быстрая опция
prompt.proc.type <- 1
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

cat(yellow(paste0('Выбрано: ', prompt.proc.type, '. ', 
                  all.proc.types[prompt.proc.type, 2], '\n')))

# процедура, с которой будем работать
lProcedureToScrap <- as.list(all.proc.types[prompt.proc.type, ])
# директория внутри csv
drnm <- paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/')
if (!dir.exists(drnm)) {
    dir.create(drnm)
}

# папка с csv-файлами по текущему типу процедур
out.path <- paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/')

message('ПОДГОТОВКА РАБОЧЕГО ПРОСТРАНСТВА ЗАВЕРШЕНА')

# URL для загрузки архивов:
my.region$url
