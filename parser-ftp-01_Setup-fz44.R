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
# Версия 1.3 (28.02.2020)
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
options("scipen" = 100, "digits" = 4)



# Функции ----------------------------------------------------------------------

files.sources <- dir('./functions/')
sapply(paste0('./functions/', files.sources), source)



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


# Список директорий с регионами ================================================

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
vars <- data.frame(n = 1:2, txt = c('Перезагрузить с ftp', 
                                    'Прочитать сохранённый'))
message(paste0('Загрузка списка регионов:\n',
               paste0(apply(vars, 1, function(x){paste0(x, collapse = '. ')}),
                      collapse = '\n')))
# prompt.load.reg.list <- readline('Введите номер опции:')
# быстрая опция 
prompt.load.reg.list <- 2
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
    
    sRegionFoldersNames <- unlist(strsplit(doc, '\n'))
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

#  регион: Башкирия
sRegionFolderURLs[7]

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')


# * Структура директорий рабочей папки #########################################

# все равки исходных данных ....................................................
sRawDataPath <- './data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# исходные данные в архивах, выгрузка за период ................................
dirs.raw <- dir('./data/raw')
n.dirs <- length(dirs.raw)
msg <- paste0(1:n.dirs, '. ', dirs.raw, '\n')

# /////////////////////////ВВОД ДАННЫХ В КОНСОЛЬ////////////////////////////////
message('Выберите выгрузку:\n', msg, n.dirs + 1, '. Создать новую выгрузку')
# prompt.load.sample <- readline('Введите номер опции:')
# быстрая опция
prompt.load.sample <- 1
# /////////////////////КОНЕЦ ВВОДА ДАННЫХ В КОНСОЛЬ/////////////////////////////

if (prompt.load.sample == n.dirs + 1) {
    sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], 
                           '_', Sys.Date(), '/')
    if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)    
} else {
    sRawArchPath <- paste0('./data/raw/', 
                           dirs.raw[as.numeric(prompt.load.sample)], '/')
}

cat(yellow(paste0('Выбрано: ', prompt.load.sample, ';\nsRawArchPath = ', 
                  sRawArchPath, '\n')))

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- paste0(sRawArchPath, 'xmls/')
if (!dir.exists(sRawXMLPath)) dir.create(sRawXMLPath)

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- paste0(sRawArchPath, 'csv/')
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
my.region <- grep(sRegionFolderURLs, pattern = 'Bashk', value = T)
message(paste0('Работаем с регионом: ', my.region))

# все типы процедур
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

message('ПОДГОТОВКА РАБОЧЕГО ПРОСТРАНСТВА ЗАВЕРШЕНА')

