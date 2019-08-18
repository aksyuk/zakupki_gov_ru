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

patt <- df.params[df.params$ParamID == 'regions', 2]
csv.names <- grep(patt, dir(dir.path), value = T)

df.all <- read.csv2(paste0(dir.path, csv.names[1]), stringsAsFactors = F)

for (flnum in 2:length(csv.names)) {
    df <- read.csv2(paste0(dir.path, csv.names[flnum]), stringsAsFactors = F)

    if (dim(df)[2] < dim(df.all)[2]) {
        clnms <- colnames(df.all)[!colnames(df.all) %in% colnames(df)]

        m <- dim(df.all)[2] - dim(df)[2]
        df <- cbind(df, matrix(rep(NA, m * dim(df)[1]), dim(df)[1], m))
        colnames(df)[(dim(df)[2] - m + 1):dim(df)[2]] <- clnms
    }

    if (dim(df)[2] > dim(df.all)[2]) {
        clnms <- colnames(df)[!colnames(df) %in% colnames(df.all)]

        m <- dim(df)[2] - dim(df.all)[2]
        df.all <- cbind(df.all, matrix(rep(NA, m * dim(df.all)[1]),
                                       dim(df.all)[1], m))
        colnames(df.all)[(dim(df.all)[2] - m + 1):dim(df.all)[2]] <- clnms
    }


    df.all <- rbind(df.all, df)
}

df.all <- unique(data.table(df.all))

dim(df.all)
length(unique(df.all$ContractID))
sort(round(table(df.all$PlatformName) / nrow(df.all) * 100, 1),
     decreasing = T)
