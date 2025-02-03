# ..............................................................................
# parser_ftp_01-Setup-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.4.2 (28 Aug 2023)
# 
# Эта часть скрипта содержит все предварительные настройки
#  и работает с сервером по 44 ФЗ
# ..............................................................................
#
# ПРО ИМЕНА ПЕРЕМЕННЫХ
#
# константы путей к директориям, а также параметры выборки, 
#  заданные пользователем, записаны капсом через точку, 
#  в начале указание типа (s - string, v - вектор длиной > 1, 
#  lst - список и т.д.):
#    * s.REF.PATH
#    * s.YEAR.MON
#    * lst.REGION
#
# прочие переменные, используемые глобально, записаны верблюжьим регистром,
#  также с указанием типа в начале:
#    * lstRegionFoldersNames
#    * sRawArchPath
#
# локальные переменные записаны строчными через точку:
#    * prompt.value
#    * dirs.raw
#
# имена таблиц начинаются с DT для объектов data.table и df для data.frame:
#    * DT.XML.INDEX
#    * DT.proc.index
#
# имена пользовательских функций начинаются с uf и записаны через точку:
#    * uf.read.table.with.metadata()
#    * uf.count.nas.in.table()
# ..............................................................................



# Загрузка библиотек -----------------------------------------------------------
library('crayon')
library('RCurl')
library('readr')
library('lubridate')
library('data.table')
library('dplyr')
library('XML')

# library('reshape')
# library('stringi')
# library('ggplot2')
# library('httr')
# library('jsonlite')
# library('RJSONIO')




# Настройки системы ------------------------------------------------------------
# отображение больших цифр в обычном формате, а не в экспоненциальном
options('scipen' = 100, 'digits' = 9)



# Функции ----------------------------------------------------------------------

# функция преобразования строковых дат в формат даты, с часовыми поясами
eval(parse('./functions/uf_convert_char_to_date.R', encoding = 'UTF-8'))

# функция подсчёта пропусков в столбцах таблицы
eval(parse('./functions/uf_count_nas_in_table.R', encoding = 'UTF-8'))

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

# функция преобразования вектора периодов в regex-запрос
eval(parse('./functions/uf_make_regex_from_year_mon.R', encoding = 'UTF-8'))

# Переменные -------------------------------------------------------------------


# Директории, которые не зависят от выгрузки ===================================

# таблицы-справочники ..........................................................
s.REF.PATH <- './data/reference/'
if (!dir.exists(s.REF.PATH)) dir.create(s.REF.PATH)

# все сырые исходные данные ....................................................
s.RAW.DATA.PATH <- './data/raw'
if (!file.exists(s.RAW.DATA.PATH)) dir.create(s.RAW.DATA.PATH)

# графики ......................................................................
s.PLOT.PATH <- './plots/'
if (!dir.exists(s.PLOT.PATH)) dir.create(s.PLOT.PATH)

# логи .........................................................................
s.LOG.PATH <- './logs/'
if (!dir.exists(s.LOG.PATH)) dir.create(s.LOG.PATH)


# Настройки выгрузки данных ====================================================

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10

# координаты FTP-сервера госзакупок и логин-пароль для бесплатного доступа
s.FTP.URL <- 'ftp://ftp.zakupki.gov.ru/'
s.USER.PWD.44 <- 'free:free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
s.YEAR.MON <- paste0(rep(2021, 12), formatC(1:12, width = 2, flag = '0'))

# часть названия региона для поиска
# номер папки загрузки: 01
srch.reg <- 'Bashk'
# 02
# srch.reg <- 'Udmu'
# 03
# srch.reg <- 'Permsk'
# 04
# srch.reg <- 'Kirov'
# 05
# srch.reg <- 'Marij'
# 06
# srch.reg <- 'Mordo'
# 07
# srch.reg <- 'Nizhegorod'
# 08
# srch.reg <- 'Orenb'
# 09
# srch.reg <- 'Penz'
# 10
# srch.reg <- 'Samars'
# 11
# srch.reg <- 'Saratov'
# 12
# srch.reg <- 'Tatar'
# 13
# srch.reg <- 'Uljan'
# 14
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
    
    # грузим список регионов с ftp
    doc <- getURL(paste0(s.FTP.URL, 'fcs_regions/'), 
                  ftp.use.epsv = FALSE, dirlistonly = TRUE, 
                  userpwd = s.USER.PWD.44)
    
    # расклеиваем позиции списка по символу перевода строки
    tmp <- unlist(strsplit(doc, '\n'))
    
    # оставляем только адреса папок, относящиеся к регионам
    patt <- '_Resp$|_kraj$|_.?obl$|_AO$|Peterburg$|Moskva$|_NR$|_Aobl$|_g$'
    tmp <- tmp[grep(patt, tmp)]
    
    # записываем список регионов в файл >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    write(tmp, './data/reference/regions_list.txt')
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
}

if (file.exists('./data/reference/regions_list.txt')) {
    
    # читаем список папок с регионами из файла <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    lstRegionFoldersNames <- list(regions = scan('./data/reference/regions_list.txt',
                                                 character()),
                                  url.prefix = 'ftp://ftp.zakupki.gov.ru/fcs_regions/')
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    } else {
        message(paste0('Файл', '"regions_list.txt"', 'не найден в ', 
                       s.REF.PATH, '. Повторите загрузку с сервера.'))
}
# выводим количество папок с регионами на сервере
message(paste0('Всего папок по регионам: ', 
               length(lstRegionFoldersNames$regions)))

#  регион, по которому требуются данные: URL и наименование
lst.REGION <- list(name = grep(lstRegionFoldersNames$regions, 
                               pattern = srch.reg, value = T))
lst.REGION$url <- paste0(lstRegionFoldersNames$url.prefix,
                         lst.REGION$name)
lst.REGION$name <- gsub('.*[/]', '', gsub('[/]$', '', lst.REGION$url))
cat(yellow(paste0('Работаем с регионом: ', lst.REGION$name, '\n')))


# Типы процедур ================================================================
#  на самом деле там пока только электронные аукционы,
#   но у файла с объявлениями встречаются уже два префикса
all.proc.types <- read.csv2(paste0(s.REF.PATH, 'dt_procedure_types.csv'),
                            stringsAsFactors = F, fileEncoding = 'UTF-8')

msg <- paste0(1:length(unique(all.proc.types$procedureType)), 
              '. ', unique(all.proc.types$procedureType))

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
message('Выберите процедуры:\n', msg)
# prompt.proc.type <- readline('Введите номер опции:')
# быстрая опция
prompt.proc.type <- 1
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

# процедура, с которой будем работать
tmp <- unique(all.proc.types$procedureCode)[as.numeric(prompt.proc.type)]
lstProcedureType <- list(name = tmp, label = '', prefix = '')
lstProcedureType$label <- all.proc.types %>% 
    filter(procedureCode == tmp) %>% select('procedureType') %>%
    unname %>% unlist %>% unique
lstProcedureType$prefix <- all.proc.types %>% 
    filter(procedureCode == tmp) %>% select('noticeFileName') %>%
    unname %>% unlist

cat(yellow(paste0('Выбрано: ', prompt.proc.type, '. ', 
                  lstProcedureType$label, '\n')))


# Структура директорий папки с данными =========================================

# Выбрать директорию вручную или создать новую #################################

# исходные данные в архивах, выгрузка за период ################################
dirs <- dir('./data/raw')[!grepl('.*[.].{3}', dir('./data/raw'))]
lst.dirs.raw <- list(path = dirs, reg = '', period = '')
n.all.dirs <- length(lst.dirs.raw[[1]])

if (n.all.dirs > 0) {
    
    # Найти директорию по заданной в srch.reg части названия региона ###############
    #  используем информацию из README.txt в директориях загрузки
    lst.dirs.raw$reg <- unname(sapply(lst.dirs.raw$path, function(x){
        gsub('Регион: ', '', 
             read_lines(paste0(s.RAW.DATA.PATH, '/', x, '/README.txt'))[1])
    }))
    # читаем периоды
    lst.dirs.raw$period <- unname(sapply(lst.dirs.raw$path, function(x){
        gsub('Период: ', '', 
             read_lines(paste0(s.RAW.DATA.PATH, '/', x, '/README.txt'))[2])
    }))
    
    # номера папок с нужным регионом
    indx.region <- which(grepl(lst.REGION$name, lst.dirs.raw$reg))
    
    # добавляем в список опций для выбора
    msg <- paste0(1:length(indx.region), '. ', lst.dirs.raw$path[indx.region], 
                  ' регион: ', lst.dirs.raw$reg[indx.region], '\n')
    
} else {
    msg <- NULL
}

msg <- c(msg, paste0(length(indx.region) + 1, '. Создать новую выгрузку\n'))

n.dirs.reg <- length(indx.region)

if (n.dirs.reg > 0) {
    
    patt <- paste0('с ', s.YEAR.MON[1], ' по ', s.YEAR.MON[length(s.YEAR.MON)])
    
    if (length(grep(patt, lst.dirs.raw$period[indx.region], value = T)) > 0) {
        msg <- c(msg, paste0(n.dirs.reg + 2, 
                             '. Определить по названию региона и периоду: ', 
                             lst.REGION$name, ' ', s.YEAR.MON[1], '-', 
                             s.YEAR.MON[length(s.YEAR.MON)], '\n'))
    }
}

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
message('Выберите выгрузку:\n', msg)
# prompt.load.sample <- readline('Введите номер опции:')
# быстрая опция: новая выгрузка
# prompt.load.sample <- n.dirs.reg + 1
# быстрая опция: выбрать по названию региона и периоду
prompt.load.sample <- n.dirs.reg + 2
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

if (prompt.load.sample == n.dirs.reg + 1) {
    # определяем порядковый номер для папки новой выгрузки
    if (n.all.dirs == 0) {
        new.count <- formatC(1, width = 2, flag = '0')
    } else {
        new.count <- formatC(n.all.dirs + 1, width = 2, flag = '0')
    }
    
    # формат пути к папке с новой выгрузкой: <директория с равками>/ 
    #  <порядковый номер выгрузки>_from<начало периода выгрузки в формате 
    #  YYYYMM>to<конец периода выгрузки в формате YYYYMM>_loaded<дата загрузки 
    #  данных в формате YYYY-MM-DD>/
    sDataSamplePath <- paste0('./data/raw/', new.count, '_from',
                           s.YEAR.MON[1], 'to', s.YEAR.MON[length(s.YEAR.MON)], '_loaded', 
                           format(Sys.Date(), format = "%Y-%m-%d"), '/')
    if (!dir.exists(sDataSamplePath)) dir.create(sDataSamplePath) 
    
    # пишем параметры данных в README.txt
    flnm <- paste0(sDataSamplePath, 'README.txt')
    msg <- paste0('Регион: ', lst.REGION$name, '\n',
                  'Период: с ', s.YEAR.MON[1], ' по ', s.YEAR.MON[length(s.YEAR.MON)], '\n',
                  'Тип процедуры: ', sProcedureType$label, '\n',
                  'Дата загрузки: ', format(Sys.Date(), format = "%Y-%m-%d"))
    # Encoding(msg) <- 'windows-1251'
    uf.write.to.log(msg, out.file.name = flnm, silent = T)
    
} else if (prompt.load.sample == n.dirs.reg + 2) {
    
    indx.selected <- which(grepl(lst.REGION$name, lst.dirs.raw$reg) & 
              grepl(paste0('с ', s.YEAR.MON[1], ' по ', 
                           s.YEAR.MON[length(s.YEAR.MON)]), 
                    lst.dirs.raw$period))
    
    sDataSamplePath <- paste0('./data/raw/', 
                              lst.dirs.raw$path[indx.selected], '/')
    
} else {
    # выбираем выгрузку, сделанную ранее
    sDataSamplePath <- paste0('./data/raw/', 
                           lst.dirs.raw$path[as.numeric(prompt.load.sample)], '/')
}

cat(yellow(paste0('Выбрано: ', prompt.load.sample, '\n',
                  'Путь:    ', sDataSamplePath, '\n',
                  'Регион:  ', lst.dirs.raw$reg[indx.selected], '\n',
                  'Период:  ', lst.dirs.raw$period[indx.selected], '\n')))


# Директории, которые зависят от выгрузки ======================================

# исходные данные в архивах ....................................................
sRawArchPath <- paste0(sDataSamplePath, 'archives/')
if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- paste0(sDataSamplePath, 'xmls/')
if (!dir.exists(sRawXMLPath)) dir.create(sRawXMLPath)

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- paste0(sDataSamplePath, 'csv/')
if (!dir.exists(sRawCSVPath)) dir.create(sRawCSVPath)

# директория внутри csv
drnm <- paste0(sRawCSVPath, lstProcedureType$name, '/')
if (!dir.exists(drnm)) {
    dir.create(drnm)
}

# папка с csv-файлами по текущему типу процедур
sOutPath <- paste0(sRawCSVPath, lstProcedureType$name, '/')

rm(tmp, srch.reg, msg, dirs, drnm, all.proc.types, indx.region,
   lst.dirs.raw, n.all.dirs, n.dirs.reg, patt, vars, indx.selected,
   prompt.load.reg.list, prompt.load.sample, prompt.proc.type)

message('ПОДГОТОВКА РАБОЧЕГО ПРОСТРАНСТВА ЗАВЕРШЕНА')
