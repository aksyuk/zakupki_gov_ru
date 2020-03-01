# ..............................................................................
# modelling_data_glimpse.R
# ..............................................................................


# ..............................................................................
#
# ОГРАНИЧЕНИЯ НА ЗАКУПКУ
# ..............................................................................


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_restrictions_clean.csv')
DT.restrictions <- uf.read.table.with.metadata(flnm)
summary(DT.restrictions)
dim(DT.restrictions)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# делаем таблицу-справочник
dt.restr.ref <- unique(select(DT.restrictions, restriction.shortName, 
                              restriction.name))
dt.restr.ref[restriction.shortName == 'no', 
             restriction.name := 'нет ограничений']


# записываем таблицу-справочник >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
flnm <- paste0(sRefPath, 'dt_restrictions.csv')
if (!file.exists(flnm)) {
    write.csv2(dt.restr.ref, flnm, row.names = F)
} else {
    dt <- read.csv2(flnm, stringsAsFactors = F, colClasses = 'character')
    dt <- data.table(rbind(dt, dt.restr.ref))
    dt <- unique(dt)
    write.csv2(dt, flnm, row.names = F)
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# ..............................................................................
#
# ТОВАРЫ
# ..............................................................................


# читаем таблицу из csv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
flnm <- paste0(out.path, 'DT_TPY_codes_clean.csv')
DT.TPY.codes <- uf.read.table.with.metadata(flnm)
summary(DT.TPY.codes)
dim(DT.TPY.codes)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


nas <- uf.count.nas.in.table(DT.TPY.codes)
nas$columns.na

# сравним первые 2 цифры кодов ОКПД2 и КТРУ по записям, в которых известны
#  оба кода. они должны совпадать по определению КТРУ
compare.2.dig <- filter(DT.TPY.codes, 
                        !is.na(purchaseObject.OKPD2.code.2dig) & 
                            !is.na(purchaseObject.KTRU.code.2dig))
nrow(compare.2.dig)
# но первые 2 цифры совпадают не всегда
nrow(compare.2.dig[compare.2.dig$purchaseObject.OKPD2.code.2dig != 
                       compare.2.dig$purchaseObject.KTRU.code.2dig, ])
# и внутри хрень какая-то
compare.2.dig[compare.2.dig$purchaseObject.OKPD2.code.2dig != 
                  compare.2.dig$purchaseObject.KTRU.code.2dig, ][1:10, 3:6]
# 
tmp <- unique(compare.2.dig[compare.2.dig$purchaseObject.OKPD2.code.2dig != 
                                compare.2.dig$purchaseObject.KTRU.code.2dig, 
                            ]$purchaseNumber)
length(tmp)
# так получается когда заказчик указал в объявлении много позиций, часть из
#  которых закодирована по КТРУ, а часть по ОКПД2, как, например, в этом файле
grep(paste0(tmp[1], '_', 
            DT.model[purchaseNumber == tmp[1], ]$fcsNotificationEF.id),
     all.xmls, value = T)
