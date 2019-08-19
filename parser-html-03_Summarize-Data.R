# ..............................................................................
# parser-html-03_Summarize-Data.R
# 
# Парсинг сайта госзакупок, 
#  который находится по адресу
#  http://zakupki.gov.ru/
# Результаты -- таблица по госзакупкам -- записывается в .csv файл.
# Параметры поиска в файле parser_html_URL_params.csv.
#
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
#
# Версия 0.9 (16.08.2019)
#
# Эта часть кода содержит код объединения данных из одной выгрузки и их анализ

# ..............................................................................
# ВНИМАНИЕ!!!!!!
# ПРИ БОЛЬШОМ КОЛИЧЕСТВЕ ОБРАЩЕНИЙ НА САЙТЕ ГОСЗАКУПОК МОГУТ ЗАБАНИТЬ ПО IP
#
# Ограничение API: 500 записей при выгрузке вручную с сайта.
# Мы запрашиваем все записи с максимальным количеством на странице: 50,
#  потом листаем в цикле. Внимание: заказы быстро добавляются, в конце
#  стоит проверить таблицу на наличие дубликатов. Уникальный идентификатор --
#  номер заказа (ContractID).



# ПЕРЕМЕННЫЕ -------------------------------------------------------------------

# папки для загрузки
dir.load <- dir(RAW.DATA.PATH)[2:3]
all.folders <- paste0(RAW.DATA.PATH, paste0(dir.load[1], '/',
                                            dir(paste0(RAW.DATA.PATH, dir.load[1]))), '/')
all.folders <- c(all.folders, 
                 paste0(RAW.DATA.PATH, paste0(dir.load[2], '/',
                                              dir(paste0(RAW.DATA.PATH, dir.load[2]))), '/'))

dt.all <- NULL

# теперь загружаем остальные таблицы
for (fldr in all.folders) {
    
    reg.name <- gsub('-', ' ', gsub('(^.*_)|/', '', fldr))
    cat(yellow(paste0('Регион: ', reg.name, '\n')))
    
    file.names <- dir(fldr)
    file.names <- file.names[!grepl('^parser_', file.names)]
    
    file.count <- 0
    dt.reg <- NULL
    for (flnm in file.names) {
    
        file.count <- file.count + 1    
        cat(green(paste0('Файл ', file.count, ' из ', length(file.names),
                         ': ', flnm, '\n')))
        
        dt.curr <- data.table(read.csv2(paste0(fldr, flnm), 
                                        stringsAsFactors = F,
                                        fileEncoding = 'cp1251'))
        dt.reg <- data.table(rbind(dt.reg, dt.curr, fill = T))
    }
    # убрать столбцы нулевых ограничений
    if (sum(colnames(dt.reg) == 'restr_NA')) dt.reg[, restr_NA := NULL]
    if (sum(colnames(dt.reg) == 'restr_NULL')) dt.reg[, restr_NULL := NULL]
    
    # добавить столбец с названием региона
    dt.reg[, region := reg.name]
    
    dt.all <- rbind(dt.all, dt.reg, fill = T)
    
    # почистить мусор из кодов лотов
    dt.all$ObjectName <- gsub('[$(]document.*$', '', dt.all$ObjectName)
}

dim(dt.all)
colnames(dt.all)

table(dt.all$region)
sort(table(dt.all$PlatformName), decreasing = T)

# проверка на дублирование ContractID
dim(dt.all)
dim(unique(dt.all))
length(unique(dt.all$ContractID))
table(dt.all$ContractID)[table(dt.all$ContractID) > 1][1]

dt.all[ContractID == "'0101100007319000051'", ]

write.csv2(dt.all, './data/code_26_all_regions_16-17-08-2019.csv',
           row.names = F)

dt.all <- data.table(read.csv2('./data/code_26_all_regions_16-17-08-2019.csv',
                               stringsAsFactors = F))


dt.all[PlatformName == 'АО «ЕЭТП»', c(1:5, 17)]

dt.count <- dt.all[PlatformName == 'АО «ЕЭТП»', .N, by = region]
dt.count.all <- dt.all[, .N, by = region]

dt.count <- merge(dt.count, dt.count.all, by = 'region')
dt.count[, N.prc := round(N.x / N.y * 100, 1)]
dt.count[order(-N.prc), ][1:10, ]

dt.sum <- na.omit(dt.all[PlatformName == 'АО «ЕЭТП»', c(17, 5)])[, lapply(.SD, sum), by = region]
dt.sum.all <- na.omit(dt.all[, c(17, 5)])[, lapply(.SD, sum), by = region]

dt.sum <- merge(dt.sum, dt.sum.all, by = 'region')
dt.sum[, S.prc := round(StartPrice.x / StartPrice.y * 100, 1)]
dt.sum[order(-S.prc), ][1:10, ]

sample.1 <- dt.all[region == 'Архангельская область' & PlatformName == 'АО «ЕЭТП»', ]
write.csv2(sample.1, './data/sample_code-26_region-5277339.csv', row.names = F)

