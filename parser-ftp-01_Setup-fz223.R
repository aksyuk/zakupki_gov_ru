
# ..............................................................................
# parser_ftp_01-Setup.R
# 
# Парсинг содержимого ftp-серверов госзакупок, 
#  которые находятся по адресам:
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free              - 44 ФЗ
#  логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.1 (16.07.2019)
# 
# Эта часть кода содержит все предварительные настройки
# 



# НАСТРОЙКИ --------------------------------------------------------------------


# Загрузка библиотек ===========================================================
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


# Настройки системы ============================================================
# отображение больших цифр в обычном формате, а не в экспоненциальном
options("scipen" = 100, "digits" = 4)


# Функции ======================================================================

files.sources <- dir('./functions/')
sapply(paste0('./functions/', files.sources), source)


# Константы ====================================================================

# максимальное количество строк в консоли для сообщений статуса 
#  (для автоматической очистки)
iMaxConsoleStatusLines <- 10

# координаты FTP-сервера госзакупок и логин-пароль для бесплатного доступа
sFTPURL <- 'ftp://ftp.zakupki.gov.ru/'
sUserPwd44 <- 'fz223free:fz223free'

# периоды, за которые грузим документацию: архивы лежат по месяцам, 
#  начало периода -- первое число месяца. Указываем начала интересующих нас 
#  периодов
# sYEAR <- c('201709', '201710', '201711', '201712', '201801', '201802', '201803',
#           '201804', '201805')
sYEAR <- c(paste0(rep(2018, ), formatC(6:12, width = 2, flag = '0')),
           paste0(rep(2019, ), formatC(1:6, width = 2, flag = '0')))

# список директорий с регионами
message('Загрузка списка регионов:\n1. Перезагрузить с ftp\n2. Прочитать сохранённый')
# prompt.load.reg.list <- readline('Введите номер опции:')
prompt.load.reg.list <- 2

if (prompt.load.reg.list == 1) {
    doc <- getURL(paste0(sFTPURL, 'fcs_regions/'), 
                  ftp.use.epsv = FALSE, dirlistonly = TRUE, 
                  userpwd = sUserPwd)
    # отладка
    # write(doc, 'tmp.txt')
    # гении с ftp госзакупок разместили под списком регионов гиганский список логов
    #  поэтому его надо отрезать
    doc <- gsub(pattern = '_logs.*$', replacement = '', doc)
    
    sRegionFoldersNames <- unlist(strsplit(doc, '\n'))
    # убираем папки, не относящиеся к регионам
    sRegionFoldersNames <- sRegionFoldersNames[grep('_Resp$|_kraj$|_.?obl$', 
                                                    sRegionFoldersNames)]
    write(sRegionFoldersNames, './data/reference/regions_list.txt')
}

if (prompt.load.reg.list == 2) {
    if (file.exists('./data/reference/regions_list.txt')) {
        sRegionFoldersNames <- scan('./data/reference/regions_list.txt',
                                    character())
    } else {
        message(paste0('Файл', '"regions_list.txt"', 'не найден в ', 
                       './data/reference/', '. Повторите загрузку с сервера.'))
    }
}

message(paste0('Regions: ', length(sRegionFoldersNames), ' folders.'))

# преобразуем папки регионов в URL-адреса
sRegionFolderURLs <- paste0('ftp://ftp.zakupki.gov.ru/fcs_regions/',
                            sRegionFoldersNames, '/')

#  регион: Татарстан
sRegionFolderURLs[66]

# имена папок с архивами
sSubfolders <- c('notifications/', 'protocols/', 'contracts/')


# * Структура директорий рабочей папки #########################################

# все равки исходных данных ....................................................
sRawDataPath <- './data/raw'
if (!file.exists(sRawDataPath)) dir.create(sRawDataPath)

# исходные данные в архивах, выгрузка за период ................................
sRawArchPath <- paste0('./data/raw/', sYEAR[1], '-', sYEAR[length(sYEAR)], 
                       '_16-07-2019', '/')
if (!dir.exists(sRawArchPath)) dir.create(sRawArchPath)

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


# Переменные ===================================================================

#  регион (регионы): Республика Татарстан
my.region <- grep(sRegionFolderURLs, pattern = 'Tatar', value = T)
message(paste0('Работаем с регионом: ', my.region))

message('ПОДГОТОВКА РАБОЧЕГО ПРОСТРАНСТВА ЗАВЕРШЕНА')

