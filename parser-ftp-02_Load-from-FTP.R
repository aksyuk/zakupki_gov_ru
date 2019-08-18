
# ..............................................................................
# parser-ftp-02_Load-from-FTP.R
# 
# Парсинг содержимого необъятного ftp сервера госзакупок, 
#  который находится по адресу
#  http://ftp.zakupki.gov.ru/
#  логин: free; пароль: free
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия 1.1 (16.07.2019)
# 
# Эта часть кода содержит загрузку архивов по региону за период с FTP
# 



# 1. ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК -------------------------------------

# лог
sLogFileName <- paste0(sLogPath, 'load_from_FTP.log')

# счётчики
n.folders <- length(my.region) * length(sSubfolders) * length(sYEAR)
i.folder <- 0

# ВНИМАНИЕ: загрузка архивов занимает ВРЕМЯ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ЕЩЁ ВНИМАНИЕ: 
#  ЗАКАЧКУ ЛУЧШЕ ДЕЛАТЬ СРЕДСТВАМИ FTP-МЕНЕДЖЕРА, НАПРИМЕР, FILEZILLA

# выгрузка xml в архивах в папку './data/raw/' в рабочей директории
for (sRegion in my.region) {

    # запись в лог
    uf.write.to.log(paste0(Sys.time(), ': starting ', sRegion), sLogFileName)
    
    for (sSubf in sSubfolders) {
        
        # запись в лог
        uf.write.to.log(paste0(Sys.time(), ': folder ', sSubf), sLogFileName)
        
        # папка для сохранения
        if (!dir.exists(paste0(sRawArchPath, sSubf))) {
            dir.create(paste0(sRawArchPath, sSubf))
        }
        
        # адреса архивов на сервере
        doc <- getURL(paste0(sRegion, sSubf),
                      ftp.use.epsv = FALSE, dirlistonly = TRUE,
                      userpwd = sUserPwd)
        zips <- unlist(strsplit(doc, '\n'))

        # берём заявки, открытые в периодах, начинающихся с sYEAR
        for (y in sYEAR) {
            
            i.folder <- i.folder + 1
            cat(red(paste0(i.folder, ' папка из ', n.folders, 
                           ' (', round(i.folder / n.folders * 100, 1), '%)\n')))
            # запись в лог
            uf.write.to.log(paste0(Sys.time(), ': year ', y), sLogFileName)
            
            zips.y <- grep(zips, pattern = paste0('_', y, '.*_.*_.*'), value = T)
            
            for (sZip in zips.y) {
                
                dest.filename <- paste0(sRawArchPath, sSubf, sZip)
                if (!file.exists(dest.filename)) {
                    
                    # запись в лог
                    uf.write.to.log(paste0(Sys.time(), ': download file ', sZip), 
                                    sLogFileName)
                    
                    download.file(paste0('ftp://', sUserPwd, '@',
                                         gsub('^.*://', '', sRegion), sSubf, sZip),
                                  destfile = dest.filename)   
                }
            }
        }
    }
}

