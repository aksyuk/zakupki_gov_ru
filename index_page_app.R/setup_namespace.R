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
# library('ggplot2')
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

# функция чтения таблицы данных и таблицы метаданных к ней
eval(parse('../functions/uf_read_table_with_metadata.R', encoding = 'UTF-8'))

# функция записи таблицы данных и таблицы метаданных к ней
eval(parse('../functions/uf_write_table_with_metadata.R', encoding = 'UTF-8'))

# функция записи в текстовый лог
eval(parse('../functions/uf_write_to_log.R', encoding = 'UTF-8'))



# Переменные -------------------------------------------------------------------


# Директории, которые не зависят от выгрузки ===================================

# таблицы-справочники ..........................................................
sRefPath <- '../data/reference/'
if (!dir.exists(sRefPath)) dir.create(sRefPath)

# все сырые исходные данные ....................................................
sRawDataPath <- '../data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# графики ......................................................................
sPlotPath <- '../plots/'
if (!dir.exists(sPlotPath)) dir.create(sPlotPath)

# логи .........................................................................
sLogPath <- '../logs/'
if (!dir.exists(sLogPath)) dir.create(sLogPath)


# Настройки выгрузки данных ====================================================

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10


# Список директорий с регионами ================================================

if (file.exists('../data/reference/regions_list.txt')) {
    sRegionFoldersNames <- scan('../data/reference/regions_list.txt',
                                character())
} else {
    message(paste0('Файл', '"regions_list.txt"', 'не найден в ', 
                   sRefPath, '. Повторите загрузку с сервера.'))
}


# Типы процедур ================================================================
#  на самом деле там пока только электронные аукционы
all.proc.types <- read.csv2(paste0(sRefPath, 'dt_procedure_types.csv'),
                            stringsAsFactors = F, fileEncoding = 'cp1251')


# Директории, которые зависят от выгрузки ======================================

# папка с выгрузкой
sDataSamplePath <- reactive({paste0('../data/raw/', readme.path(), '/')})
message(paste0('sDataSamplePath = ', sDataSamplePath()))

# исходные данные в архивах ....................................................
sRawArchPath <- reactive({paste0(sDataSamplePath, 'archives/')})
message(paste0('sRawArchPath = ', sRawArchPath()))

# исходные данные в xml (распакованные архивы) .................................
sRawXMLPath <- reactive({paste0(sDataSamplePath, 'xmls/')})
message(paste0('sRawXMLPath = ', sRawXMLPath()))

# исходные данные в csv (разобранные xml) ......................................
sRawCSVPath <- reactive({paste0(sDataSamplePath, 'csv/')})
message(paste0('sRawCSVPath = ', sRawCSVPath()))

# процедура, с которой будем работать
prompt.proc.type <- 1
lProcedureToScrap <- as.list(all.proc.types[prompt.proc.type, ])
# директория внутри csv
message(paste0('lProcedureToScrap = ', lProcedureToScrap))

# папка с csv-файлами по текущему типу процедур
out.path <- reactive({paste0(sRawCSVPath, lProcedureToScrap$procedureCode, '/')})

message('ПОДГОТОВКА РАБОЧЕГО ПРОСТРАНСТВА ЗАВЕРШЕНА')
