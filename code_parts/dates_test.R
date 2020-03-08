
tmp.1 <- as.POSIXct('2018-12-07 09:16:47', tz = 'Europe/Moscow')
tmp.2 <- as.POSIXct('2018-12-07 09:16:47', tz = 'Asia/Dushanbe')
difftime(tmp.1, tmp.2, 'hours')

tmp.1 <- as.POSIXct('2018-12-07 09:16:47', tz = 'Etc/GMT+5')
tmp.2 <- as.POSIXct('2018-12-07 09:16:47', tz = 'Etc/GMT-5')
difftime(tmp.1, tmp.2, 'hours')


tmp <- c(DT.protocols02$application.firstOffer.date, 
         DT.protocols02$application.lastOffer.date)

tz <- as.POSIXct(gsub('(.*)[+](.*)$', '\\2', tmp), format = '%H:%M')
tz <- hour(tz) + minute(tz) / 60
table(tz)


tmp <- c("2018-12-07T09:16:47+01:00", "2018-12-14T12:15:48-05:00",
         "2018-12-14T12:31:54+05:00", "2018-12-14T12:31:54+01:00")
# вытаскиваем знак и инвертируем его
tz.sign.char <- gsub('.*([+]|-).*$', '\\1', tmp)
tz.sign.invert <- rep('', length(tz.sign.char))
tz.sign.invert[tz.sign.char == '+'] <- '-'
tz.sign.invert[tz.sign.char == '-'] <- '+'

# считаем смещение в часах
tz <- as.POSIXct(gsub('(.*)[+|-](.*)$', '\\2', tmp), format = '%H:%M')
tz <- paste0('Etc/GMT', tz.sign.invert, hour(tz) + minute(tz) / 60)
table(tz)

# отрезаем зону от исходной строки
tmp.wo.tz <- gsub('(.*)[+|-](.*)$', '\\1', tmp)
# tmp

tmp.date <- rep(as.POSIXct('1900-01-01 00:00:01', tz = 'Europe/Moscow'), 
                length(tmp))
tz.loop <- unique(tz)
for (tz.curr in tz.loop) {
    
    # # отладка
    # tz.curr <- tz.loop[3]
    
    date.orig.tz <- as.POSIXct(tmp.wo.tz[tz == tz.curr], 
                               format = '%Y-%m-%dT%H:%M:%S', tz = tz.curr)
    tmp.date[tz == tz.curr] <- with_tz(date.orig.tz, 'Europe/Moscow')
}
tmp[1:10]
tmp.wo.tz[1:10]
tmp.date[1:10]
summary(tmp.date)
summary(hour(tmp.date))
sum(is.na(tmp))
tmp[is.na(tmp.date)][1:10]



tmp.date <- as.POSIXct(tmp, tz = "Etc/GMT-5")
as.POSIXct(paste(tmp, tz))
