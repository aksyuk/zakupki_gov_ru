
# 
# parser-ftp-04_Parse-Loaded_XML-fz44.R
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
# Эта часть кода парсит xml-файлы, скачанные с FTP    
#  и работает с сервером по 44 ФЗ



# 3. РАЗБОР XML-ФАЙЛОВ ---------------------------------------------------------


# Делаем индекс файлов с аукционами ============================================
# выясняем все префиксы файлов с интересующими id
dt.loop.filenames <- gsub(all.xmls, pattern = 'fcs_', replacement = 'fcs')
dt.loop.filenames <- gsub(dt.loop.filenames, pattern = '[.]xml',
                          replacement = '')
dt.loop.filenames <- data.table(do.call(rbind, strsplit(dt.loop.filenames, '_')))
colnames(dt.loop.filenames) <- c('prefix', 'purchaseNum', 'id')
head(dt.loop.filenames)

dt.loop.filenames <- dt.loop.filenames[purchaseNum %in% loop.ids, ]
dim(dt.loop.filenames)

loop.refs <- unique(dt.loop.filenames$prefix)
# считаем количество уникальных значений по префиксам файлов
dt.loop.filenames[, .N, by = prefix]
dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}), by = prefix]
loop.refs <- dt.loop.filenames[, lapply(.SD, function(x) {length(unique(x))}),
                               by = prefix][order(-purchaseNum), ]$prefix


# Идём по префиксам, делаем индекс =============================================
# счётчик для очистки консоли
console.clean.count <- 0
# счётчики
i.files.сount <- 0
n <- length(loop.refs)
# цикл по префиксам файлов, которые относятся к электронным аукционам
for (pref in loop.refs) {
    # имена xml-файлов с текущим префиксом
    xmls.by.prefix <-
        grep(all.xmls, pattern = paste0(pref, '_.*[.]xml'), value = T)
    # убираем расширения
    xmls.by.prefix <-
        gsub(xmls.by.prefix, pattern = '[.]xml$', replacement = '')
    # убираем префиксы
    xmls.by.prefix <-
        gsub(xmls.by.prefix, pattern = paste0(pref, '_'), replacement = '')

    # делаем таблицу-индекс для текущего префикса
    #  столбец 1: id извещения
    #  столбец 2: id документа
    DT.xmls.by.prefix.index <-
        data.table(purchaseNumber = gsub(xmls.by.prefix, pattern = '_.*$',
                                         replacement = ''),
                   id = gsub(xmls.by.prefix, pattern = '^.*_', replacement = ''))
    colnames(DT.xmls.by.prefix.index)[colnames(DT.xmls.by.prefix.index) == 'id'] <- paste0('id.', pref)

    # совмещаем с общим индексом (все префиксы вместе)
    i.files.сount <- i.files.сount + 1
    if (i.files.сount == 1) {
        DT.EA44.index <- DT.xmls.by.prefix.index
        setkey(DT.EA44.index, purchaseNumber)
    } else {
        setkey(DT.xmls.by.prefix.index, purchaseNumber)
        DT.EA44.index <- merge(DT.EA44.index, DT.xmls.by.prefix.index,
                               all.x = TRUE, allow.cartesian = T)
    }

    # убираем мусор
    rm(xmls.by.prefix, DT.xmls.by.prefix.index)

    message(paste0(pref, ': индекс готов на ', round(i.files.сount / n * 100, 0), '%'))
    console.clean.count <- console.clean.count + 1

    # очистка консоли
    if (console.clean.count == iMaxConsoleStatusLines) {
        cat("\014")
        console.clean.count <- 0
    }
}

# проверка
dim(DT.EA44.index)
str(DT.EA44.index)
DT.EA44.index

# сохраняем
write.csv2(DT.EA44.index, paste0(sRawCSVPath, 'DT_index_EA44.csv'),
           row.names = F)

# DT.EA44.index <- data.table(read.csv2(paste0(sRawCSVPath, 'DT_index_EA44.csv'),
#                                  stringsAsFactors = F,
#                                  colClasses = rep('character', 12)))
DT.EA44.index

# процент пропусков
round(DT.EA44.index[, lapply(.SD, function(x){sum(is.na(x))})] / nrow(DT.EA44.index), 2)
# всего xml для обработки
n <- sum(DT.EA44.index[, sapply(.SD, function(x){sum(!is.na(unique(x)))})][-1])
message(paste0('Всего xml для обработки: ', n,  ' (',
               round(n / length(all.xmls) * 100, 1),
               '% от общего числа файлов).'))


# Парсим файлы с электронными аукционами =======================================
# список префиксов файлов
colnames(DT.EA44.index)

# шаблоны для разбора хранятся в отдельном csv
df.patterns <- read.csv2(paste0(sRefPath, 'df_xml_patterns.csv'),
                         stringsAsFactors = F)

# преобразуем фрейм с шаблонами в список
patterns <- as.list(df.patterns)
patterns <- c(list(ns = c('xmlns:', 'xmlns:', 'oos:', 'xmlns:',
                          'xmlns:', 'xmlns:')), patterns)

# делаем из шаблонов имена столбцов итоговой таблицы
patterns[-1] <- lapply(patterns[-1], function(x) {
    x <- x[x != '']
    tmp <- gsub(x, pattern = 'xmlns[:]|oos[:]', replacement = '')
    tmp <- gsub(tmp, pattern = '[//]|[/]', replacement = '.')
    tmp <- gsub(tmp, pattern = '^[.]*', replacement = '')
    tmp <- gsub(tmp, pattern = '[.]+', replacement = '.')
    tmp <- gsub(tmp, pattern = '^.*:', replacement = '')
    tmp <- gsub(tmp, pattern = '/.*:', replacement = '/')
    tmp <- gsub(tmp, pattern = '[.]$', replacement = '')
    tmp <- gsub(tmp, pattern = '[][]', replacement = '')
    tmp <- gsub(tmp, pattern = '\\)|\\(', replacement = '')
    tmp <- gsub(tmp, pattern = '([^\\|]*)\\|.*', replacement = '\\1')

    x <- data.frame(xml.pattern = x)
    x$xpath.names <- as.character(tmp)

    x <- x
})

# отладка
# patterns
patterns$fcsProtocolEF3
# length(patterns)

# лог
log.parse.XML.filename <- paste0(sRawArchPath, 'log_xml_parse.txt')
if (!file.exists(log.parse.XML.filename)) file.create(log.parse.XML.filename)

# максимальное число строк таблицы в оперативной памяти
step.n <- 10000

# цикл по типам файлов (столбцам таблицы DT.EA44.index со 2 по n)
for (j in 2:dim(DT.EA44.index)[2]) {

    # отладка
    # j <- 2

    DT <- na.omit(unique(DT.EA44.index[, c(1, j), with = F]))
    if (nrow(DT) == 0) {
        next
    }

    filePrefix <- gsub(colnames(DT)[2], pattern = '^.*[.]', replacement = '')
    n.cols <- length(patterns[[filePrefix]][, 1])

    msg <- paste0('Начинаю разбор файлов ', filePrefix, ': ', Sys.time())
    uf.write.to.log(msg, log.parse.XML.filename)

    # это все имена файлов заданного типа
    flnms <-
        unique(paste0(sRawXMLPath, filePrefix, '_',
                      unname(unlist(DT[, 1, with = F])), '_',
                      unname(unlist(DT[, 2, with = F])), '.xml'))
    tmp.files.count <- 0

    # отладка
    # tmp.files.count <- 4
    # flnms <- flnms[(((tmp.files.count - 1) * step.n) + 1):length(flnms)]
    # flnms <- flnms[1:10]

    # счётчики для статуса
    n <- length(flnms)
    i.files.сount <- 0
    # имена тегов
    tagnames <- patterns[[filePrefix]][, 2]
    # промежуточная таблица
    df.lines.notices <- NULL
    # счётчик для очистки консоли
    console.clean.count <- 0

    for (f in flnms) {

        # отладка
        # f <- flnms[1]

        rootNode <- xmlTreeParse(f, encoding = 'UTF-8', useInternalNodes = T)
        # танцы с namespace
        nsDefs <- xmlNamespaceDefinitions(rootNode)
        ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
        names(ns)[names(ns) == ''] <- 'xmlns'

        # парсим строку целиком
        new.line <-
            unlist(unname(sapply(patterns[[filePrefix]][, 1],
                                 function(path) {
                                     val.tmp <- xpathSApply(rootNode, path,
                                                            xmlValue,
                                                            namespaces = ns)
                                     # если тег не найден, ставим NA
                                     if (length(val.tmp) == 0) val.tmp <- NA
                                     # если найдено несколько тегов,
                                     #  сжимаем в одно значение
                                     if (length(val.tmp) > 1) {
                                         val.tmp <- paste(val.tmp,
                                                          collapse = '#')
                                         }
                                     val.tmp
                                     })))
        names(new.line) <- tagnames

        # отладка
        # new.line

        i.files.сount <- i.files.сount + 1
        if (is.null(df.lines.notices)) {
            df.lines.notices <- data.frame(t(new.line), stringsAsFactors = F)
        } else {
            df.lines.notices <- rbind(df.lines.notices, new.line)
        }

        # когда набирается заданное количество строк, скидываем таблицу на диск
        if (nrow(df.lines.notices) == step.n) {
            tmp.files.count <- tmp.files.count + 1
            tbl.name <-
                paste0(sRawCSVPath, 'DF_lines_',
                       formatC(tmp.files.count, width = 3, format = 'd',
                               flag = '0'), '.csv')
            write.csv2(df.lines.notices, tbl.name, row.names = F)

            msg <- paste0('Временный файл ', tbl.name, ' сохранён.')
            uf.write.to.log(msg, log.parse.XML.filename)

            df.lines.notices <- NULL
        }

        message(paste0(j, ' ...', sub(f, pattern = '^.*/', replacement = ''),
                       '... ', round(i.files.сount / n * 100, 1), '% done'))

        console.clean.count <- console.clean.count + 1
        if (console.clean.count == iMaxConsoleStatusLines) {
            cat("\014")
            console.clean.count <- 0
        }
    }

    # сохраняем на диск последний кусок таблицы
    if (!is.null(df.lines.notices)) {
        tmp.files.count <- tmp.files.count + 1
        tbl.name <-
            paste0(sRawCSVPath, 'DF_lines_',
                   formatC(tmp.files.count, width = 3, format = 'd', flag = '0'),
                   '.csv')
        write.csv2(df.lines.notices, tbl.name, row.names = F)

        df.lines.notices <- NULL
    }

    # сообщения в консоль и в лог
    msg <- paste0('Закончили разбор файлов ', filePrefix, ': ', Sys.time())
    uf.write.to.log(msg, log.parse.XML.filename)

    # имена временных файлов
    tmp.files <- paste0(sRawCSVPath, 'DF_lines_',
                        formatC(1:tmp.files.count, width = 3, format = 'd',
                                flag = '0'), '.csv')

    # читаем первую из временных таблиц
    DT.all <- data.table(read.csv2(tmp.files[1], colClasses = rep('character', n.cols),
                                   stringsAsFactors = F))

    # читаем остальные временные таблицы
    for (flnm in tmp.files[-1]) {
        DT.all <- rbind(DT.all, data.table(read.csv2(flnm, colClasses = rep('character',
                                                                            n.cols),
                                                     stringsAsFactors = F)))
    }

    # записываем общую таблицу
    write.csv2(DT.all, paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
               row.names = F)

    # убираем временные таблицы из памяти и с диска
    rm(DT, DT.all)
    file.remove(tmp.files)

    # сообщения в консоль и в лог
    msg <- paste0('Файл ', paste0(sRawCSVPath, 'DT_', filePrefix, '.csv'),
                  ' сохранён.')
    uf.write.to.log(msg, log.parse.XML.filename)

    msg <- paste0('Временные файлы удалены.')
    uf.write.to.log(msg, log.parse.XML.filename)
}



# 4. ЧИСТИМ RAW-ТАБЛИЦЫ --------------------------------------------------------


#  * Работаем с таблицей объявлений ============================================
# имя файла с результатами парсинга XML (не нормализованная таблица)
flnm <- paste0(sRawCSVPath, 'DT_fcsNotificationEA44.csv')

# грузим
DT.notif <- data.table(read.csv2(flnm, stringsAsFactors = F,
                                na.strings = c('NA', '<NA>'),
                                # encoding = 'UTF-8',
                                colClasses = 'character'))
# проверка
dim(DT.notif)
str(DT.notif)
colnames(DT.notif)

# # проверка .....................................................................
# #  список 'id заявки -- id организации-заказчика
# DT.notif.with.orgs <- unique(DT.notif[, c('fcsNotificationEF.purchaseNumber',
#                                           'responsibleOrg.regNum'), with = F])
# prod(substr(DT.notif.with.orgs$fcsNotificationEF.purchaseNumber, 1, 11) ==
#          DT.notif.with.orgs$responsibleOrg.regNum)
# # ..............................................................................

# переименовываем первый столбец
colnames(DT.notif)[colnames(DT.notif) == 'fcsNotificationEF.purchaseNumber'] <- 
    'purchaseNumber'
# ставим столбец с номером заявки на первое место
DT.notif <- select(DT.notif, purchaseNumber, everything())

# с этой таблицей в лоб не справиться, много склеянных ячеек
#  поэтому вытаскиваем куски по частям


# ..............................................................................
#
# ЗАКАЗЧИК И МАКСИМАЛЬНАЯ ЦЕНА
# ..............................................................................

columns.to.search <- c(c('purchaseNumber', 'fcsNotificationEF.id', 
                         'docPublishDate', 'responsibleOrg.regNum',
                         'responsibleOrg.fullName', 'responsibleOrg.postAddress',
                         'responsibleOrg.INN', 'responsibleOrg.KPP',
                         'responsibleRole', 'lot.maxPrice', 'lot.currency.code'))
all.responsibleOrgs <- unique(data.table(DT.notif[, columns.to.search, 
                                                  with = F]))
dim(all.responsibleOrgs)

# здесь ничего расклеивать обычно не нужно
#  если эта сумма == 0, то столбцов, в которых есть #, в таблице нет
sum(grepl(unlist(apply(all.responsibleOrgs, 2, function(x){
    x <- paste0(unlist(x), collapse = '@')
})), pattern = '#'))

# преобразовываем в дату-время столбец docPublishDate
all.responsibleOrgs[, docPublishDate_POSIXct :=
                   as.POSIXct(gsub('T', ' ', docPublishDate))]
all.responsibleOrgs[, docPublishDate := NULL]

# преобразовываем максимальную цену в число
all.responsibleOrgs[, lot.maxPrice := as.numeric(lot.maxPrice)]
sum(is.na(all.responsibleOrgs$lot.maxPrice))


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(all.responsibleOrgs, 
                             paste0(sRawCSVPath, 'DT_responsibleOrgs_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_responsibleOrgs_clean_META.csv'),
                     stringsAsFactors = F)
DT.responsibleOrgs <- data.table(read.csv2(paste0(sRawCSVPath,
                                       'DT_responsibleOrgs_clean.csv'),
                                       stringsAsFactors = F,
                                       colClasses = df.meta$col.classes))
summary(DT.responsibleOrgs)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# ..............................................................................
#
# ОГРАНИЧЕНИЯ НА ЗАКУПКУ
# ..............................................................................
#
columns.to.search <- c(c('purchaseNumber', 'fcsNotificationEF.id', 
                         'docPublishDate', 'restriction.shortName',
                         'restriction.name'))
all.restrictions <- unique(data.table(DT.notif[, columns.to.search, 
                                               with = F]))
dim(all.restrictions)

# расклеиваим ячейки (файл записывается в csv автоматом)
DT.restrictions <-
    uf.process.large.table.normalising(all.restrictions,
                             out.total.table.name = 'DT_restrictions_clean.csv')

# преобразовываем в дату-время столбец docPublishDate
DT.restrictions[, docPublishDate_POSIXct :=
                    as.POSIXct(gsub('T', ' ', docPublishDate))]
DT.restrictions[, docPublishDate := NULL]


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.restrictions, 
                             paste0(sRawCSVPath, 'DT_restrictions_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_restrictions_clean_META.csv'),
                     stringsAsFactors = F)
DT.restrictions <- data.table(read.csv2(paste0(sRawCSVPath,
                                               'DT_restrictions_clean.csv'),
                                        stringsAsFactors = F,
                                        colClasses = df.meta$col.classes))
summary(DT.restrictions)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# ..............................................................................
#
# ТОВАРЫ
# ..............................................................................
#
columns.to.search <- c('purchaseNumber',
                       'purchaseObject.OKPD2.code', 'purchaseObject.OKPD2.name')
all.OKPD2 <- unique(data.table(DT.notif[, columns.to.search, with = F]))
dim(all.OKPD2)

all.OKPD2 <- uf.process.large.table.normalising(all.OKPD2,
                                out.total.table.name = 'DT_all_OKPD2_clean.csv')

# эта таблица вся из символьных столбцов, и всё что надо сделать -- это удалить
#  дублирующиеся строки
all.OKPD2 <- unique(data.table(all.OKPD2))
all.OKPD2[, purchaseObject.OKPD2.code.2dig :=
              gsub(all.OKPD2$purchaseObject.OKPD2.code, pattern = '[.].*$',
                   replacement = '')]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(all.OKPD2)[colnames(all.OKPD2) == 'fcsNotificationEF.purchaseNumber'] <-
    'purchaseNumber'

uf.write.table.with.metadata(all.OKPD2, paste0(sRawCSVPath, 
                                               'DT_all_OKPD2_clean.csv'))


dim(DT.responsibleOrgs)
length(unique(DT.responsibleOrgs$purchaseNumber))
length(unique(all.OKPD2$purchaseNumber))
DT.responsibleOrgs <- merge(DT.responsibleOrgs, 
                            unique(all.OKPD2[, c('purchaseNumber', 
                                                 'purchaseObject.OKPD2.code.2dig')]), 
                            by = 'purchaseNumber', all.x = T)


# ..............................................................................
#
# ОКПД (ОТРАСЛИ)
# ..............................................................................
OKPD.industries <- sort(unique(all.OKPD2$purchaseObject.OKPD2.code.2dig))
write.csv2(data.frame(OKPD2.2dig = OKPD.industries),
           paste0(sRefPath, 'df_all_OKPD2_industries_', sYEAR[1], '-', 
                         sYEAR[length(sYEAR)], '_', Sys.Date(), '.csv'), 
           row.names = F)



# ..............................................................................
#
# УЧАСТНИКИ АУКЦИОНОВ
# ..............................................................................
#
#  * Работаем с таблицей протоколов этапа 1 ====================================
protocols.EF1 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF1.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 5)))
protocols.EF1 <- unique(protocols.EF1)
colnames(protocols.EF1)[colnames(protocols.EF1) == 'fcsProtocolEF1.purchaseNumber'] <-
    'purchaseNumber'
protocols.EF1

# проверка .....................................................................
#  строк (уникальных id) в файле должно быть столько же...
nrow(protocols.EF1)
#  ...сколько уникальных id протоколов 01
length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$id.fcsProtocolEF1))
# ..............................................................................

# ставим столбец с номером заявки на первое место
protocols.EF1 <- select(protocols.EF1, purchaseNumber, everything())
# расклеиваим ячейки (файл записывается в csv автоматом)
DT.protocols01 <- uf.process.large.table.normalising(protocols.EF1,
                           out.total.table.name = 'DT_fcsProtocolEF1_clean.csv')

# логический столбец admitted содержит для непринятых заявок причины
# перекидываем причины в новый столбец, делаем admitted логическим
DT.protocols01[is.na(as.logical(admitted)), admitted.F.reason := admitted]
DT.protocols01[is.na(as.logical(admitted)), admitted := F]
DT.protocols01[, admitted := as.logical(admitted)]
table(DT.protocols01$admitted)
sum(is.na(DT.protocols01$admitted))

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols01[is.na(as.numeric(application.journalNumber)), 
               application.journalNumber.comment := application.journalNumber]
DT.protocols01[, application.journalNumber := as.numeric(application.journalNumber)]
table(DT.protocols01$application.journalNumber)
sum(is.na(DT.protocols01$application.journalNumber))

# преобразовываем в дату-время столбец protocolDate
DT.protocols01[, protocolDate_POSIXct :=
                   as.POSIXct(gsub('T', ' ', protocolDate))]
DT.protocols01[, protocolDate := NULL]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols01)[colnames(DT.protocols01) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols01)[colnames(DT.protocols01) == 'protocolDate_POSIXct'] <-
    'protocol01Date_POSIXct'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.protocols01, 
                             paste0(sRawCSVPath, 'DT_fcsProtocolEF1_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF1_clean_META.csv'),
                     stringsAsFactors = F)
DT.protocols01 <- data.table(read.csv2(paste0(sRawCSVPath,
                                              'DT_fcsProtocolEF1_clean.csv'),
                                       stringsAsFactors = F,
                                       colClasses = df.meta$col.classes))
summary(DT.protocols01)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# проверка .....................................................................
length(unique(DT.protocols01$purchaseNumber))
length(na.omit(unique(DT.protocols01$purchaseNumber)))
length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber))

# во всех нижеследующих заявках процедура аукциона обрывается из-за различных
#  ошибок: не подано ни одной заявки, например
all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF1), ]$purchaseNumber)
missed.ids.01 <- all.ids.in.index[!(all.ids.in.index %in%
                                        na.omit(unique(DT.protocols01$purchaseNumber)))]



#  * Работаем с таблицей протоколов этапа 2 ====================================
protocols.EF2 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF2.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 9)))
protocols.EF2 <- unique(protocols.EF2)
colnames(protocols.EF2)[colnames(protocols.EF2) == 'application.journalNumber.priceOffers'] <-
    'application.journalNumber'
protocols.EF2

# ставим столбец с номером заявки на первое место
protocols.EF2 <- select(protocols.EF2, purchaseNumber, everything())
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols02 <- uf.process.large.table.normalising(protocols.EF2,
                           out.total.table.name = 'DT_fcsProtocolEF2_clean.csv')

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols02[is.na(as.numeric(application.journalNumber)), 
               application.journalNumber.comment := application.journalNumber]
DT.protocols02[, application.journalNumber := as.numeric(application.journalNumber)]
table(DT.protocols02$application.journalNumber)
sum(is.na(DT.protocols02$application.journalNumber))

# первая цена
DT.protocols02[, application.firstOffer.price := as.numeric(application.firstOffer.price)]
sum(is.na(DT.protocols02$application.firstOffer.price))

# последняя цена
DT.protocols02[, application.lastOffer.price := as.numeric(application.lastOffer.price)]
sum(is.na(DT.protocols02$application.lastOffer.price))

# количество предложений цен
DT.protocols02[, application.priceOffers.offersQuantity := as.numeric(application.priceOffers.offersQuantity)]
sum(is.na(DT.protocols02$application.priceOffers.offersQuantity))

# преобразовываем в дату-время столбец protocolDate
DT.protocols02[, protocolDate_POSIXct :=
                   as.POSIXct(gsub('T', ' ', protocolDate))]
DT.protocols02[, protocolDate := NULL]

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols02)[colnames(DT.protocols02) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols02)[colnames(DT.protocols02) == 'protocolDate_POSIXct'] <-
    'protocol02Date_POSIXct'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.protocols02, 
                             paste0(sRawCSVPath, 'DT_fcsProtocolEF2_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF2_clean_META.csv'),
                     stringsAsFactors = F)
DT.protocols02 <- data.table(read.csv2(paste0(sRawCSVPath,
                                              'DT_fcsProtocolEF2_clean.csv'),
                                       stringsAsFactors = F,
                                       colClasses = df.meta$col.classes))
summary(DT.protocols02)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# проверка .....................................................................
length(unique(DT.protocols02$purchaseNumber))
length(na.omit(unique(DT.protocols02$purchaseNumber)))
length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber))

# во всех нижеследующих заявках процедура аукциона обрывается из-за различных
#  ошибок: не подано ни одной заявки, например
all.ids.in.index <- unique(DT.EA44.index[!is.na(id.fcsProtocolEF2), ]$purchaseNumber)
missed.ids.02 <- all.ids.in.index[!(all.ids.in.index %in%
                                        na.omit(unique(DT.protocols02$purchaseNumber)))]


#  * Работаем с таблицей протоколов этапа 3 ====================================
protocols.EF3 <-
    data.table(read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF3.csv'),
                         stringsAsFactors = F,
                         colClasses = rep('character', 9)))
protocols.EF3 <- unique(protocols.EF3)
protocols.EF3

# ставим столбец с номером заявки на первое место
protocols.EF3 <- select(protocols.EF3, purchaseNumber, everything())
# нормализуем таблицу (расклеиваем значения в столбцах)
DT.protocols03 <-
    uf.process.large.table.normalising(protocols.EF3,
                           out.total.table.name = 'DT_fcsProtocolEF3_clean.csv')

# преобразовываем в дату-время столбец protocolDate
DT.protocols03[, protocolDate_POSIXct :=
                   as.POSIXct(gsub(DT.protocols03$protocolDate, pattern = 'T',
                                   replacement = ' '))]
DT.protocols03[, protocolDate := NULL]

# числовой столбец application.journalNumber содержит какой-то мусор
#  сохраняем на всякий случай
DT.protocols03[is.na(as.numeric(applications.journalNumber)), 
               applications.journalNumber.comment := applications.journalNumber]
DT.protocols03[, applications.journalNumber := as.numeric(applications.journalNumber)]
table(DT.protocols03$applications.journalNumber)
sum(is.na(DT.protocols03$applications.journalNumber))

# числовой столбец application.appRating содержит текстовые причины отказа
DT.protocols03[is.na(as.numeric(application.appRating)), 
               application.appRating.drop.reason := application.appRating]
DT.protocols03[, application.appRating := as.numeric(application.appRating)]
table(DT.protocols03$application.appRating)
sum(is.na(DT.protocols03$application.appRating))

# переименовываем столбцы для дальнейшего связывания таблиц
colnames(DT.protocols03)[colnames(DT.protocols03) == 'applications.journalNumber'] <-
    'application.journalNumber'
colnames(DT.protocols03)[colnames(DT.protocols03) == 'protocolDate_POSIXct'] <-
    'protocol03Date_POSIXct'


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.protocols03, 
                             paste0(sRawCSVPath, 'DT_fcsProtocolEF3_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df.meta <- read.csv2(paste0(sRawCSVPath, 'DT_fcsProtocolEF3_clean_META.csv'),
                     stringsAsFactors = F)
DT.protocols03 <- data.table(read.csv2(paste0(sRawCSVPath,
                                              'DT_fcsProtocolEF3_clean.csv'),
                                       stringsAsFactors = F,
                                       colClasses = df.meta$col.classes))
summary(DT.protocols03)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# проверка .....................................................................
length(unique(DT.protocols03$purchaseNumber))
length(na.omit(unique(DT.protocols03$purchaseNumber)))
length(unique(DT.EA44.index[!is.na(id.fcsProtocolEF3), ]$purchaseNumber))


# смотрим сведения о стране
table(DT.protocols03$applications.countryFullName)
### Российская Федерация
###                33395
DT.protocols03[, applications.countryFullName := NULL]



# совмещаем информацию об участниках аукционов /////////////////////////////////
dim(DT.protocols01)
dim(DT.protocols02)
dim(DT.protocols03)
length(unique(DT.protocols01$purchaseNumber))
length(unique(DT.protocols02$purchaseNumber))
length(unique(DT.protocols03$purchaseNumber))

# совмещаем таблицы
DT.all.EA <- merge(DT.protocols01, DT.protocols02,
                   by = c('purchaseNumber', 'application.journalNumber'), 
                   all = T)
DT.all.EA <- merge(DT.all.EA, DT.protocols03,
                   by = c('purchaseNumber', 'application.journalNumber'),
                   all = T)
DT.all.EA


# записываем очищенную таблицу с метаданными >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
uf.write.table.with.metadata(DT.all.EA, 
                             paste0(sRawCSVPath, 'DT_all_auctions_clean.csv'))
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# всего заявок
length(unique(DT.all.EA$purchaseNumber))
