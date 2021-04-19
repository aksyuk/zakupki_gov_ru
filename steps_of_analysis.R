
srch.reg <- 'Chuvash'
sYEAR <- paste0(rep(2020, 12), formatC(1:12, width = 2, flag = '0'))

eval(parse('./parser-ftp-01_Setup-fz44.R', encoding = 'UTF-8'))

eval(parse('./parser-ftp-04_Prepare-Data-for-Models.R', encoding = 'UTF-8'))


