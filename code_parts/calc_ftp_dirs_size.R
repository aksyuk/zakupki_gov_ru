
# считаем суммарные размеры архивов в папках contracts, protocols 
#  и notifications по регионам
flnm <- './scanner_app/du.txt'
tmp <- read_lines(flnm)
sizes <- tmp
sizes[grepl('^[:0-9:]', sizes)] <- gsub('\\t.*', '', 
                                        sizes[grepl('^[:0-9:]', sizes)])
ind.regs <- which(grepl('^[:a-z,A-Z:]', tmp))
sizes.Gb <- round(sapply(ind.regs, function(x) {
    sum(as.numeric(sizes[(x + 1):(x + 3)]))
}) / 1000, 1)
df.sizes <- data.frame(regions = tmp[ind.regs],
                       sizes.Gb = sizes.Gb)
# результат -- фрейм с размерами в гигабайтах
df.sizes
sum(df.sizes$sizes.Gb)
