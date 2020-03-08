# ..............................................................................
# parser-ftp-02_Load-from-FTP-fz44.R
# 
# Парсинг содержимого ftp-сервера госзакупок, 
#  который находится по адресу:
#  ftp://ftp.zakupki.gov.ru/
#    X  логин: free; пароль: free              - 44 ФЗ
#       логин: fz223free; пароль: fz223free    - 223 ФЗ
# 
# Автор: Суязова (Аксюк) Светлана s.a.aksuk@gmail.com
# 
# Версия --
# 
# Эта часть кода больше не используется
# ..............................................................................



# 1. ЗАГРУЗКА АРХИВОВ С СЕРВЕРА ГОСЗАКУПОК -------------------------------------

# ..............................................................................
# ВНИМАНИЕ: загрузка архивов занимает ВРЕМЯ
# ЕЩЁ ВНИМАНИЕ:
#  Закачку надо делать средствами FTP-менеджера, который поддерживает 
#   докачку файлов, например, FileZilla. Массовая загрузка с ftp с помощью R 
#   это долго и чревато ошибками, особенно при плохом интернет-соединении.
# С адреса:
my.region$url
# -- грузим содержимое подпапок contracts, notifications, protocols
#     в папку archives
# ..............................................................................

# # лог
# sLogFileName <- paste0(sLogPath, 'load_from_FTP.log')
# 
# # счётчики
# n.folders <- length(my.region) * length(sSubfolders) * length(sYEAR)
# i.folder <- 0

# # выгрузка xml в архивах в папку './data/raw/' в рабочей директории
# for (sRegion in my.region) {
# 
#     # запись в лог
#     uf.write.to.log(paste0(Sys.time(), ': starting ', sRegion), sLogFileName)
# 
#     for (sSubf in sSubfolders) {
# 
#         # запись в лог
#         uf.write.to.log(paste0(Sys.time(), ': folder ', sSubf), sLogFileName)
# 
#         # папка для сохранения
#         if (!dir.exists(paste0(sDataSamplePath, sSubf))) {
#             dir.create(paste0(sDataSamplePath, sSubf))
#         }
# 
#         # адреса архивов на сервере
#         doc <- getURL(paste0(sRegion, sSubf),
#                       ftp.use.epsv = FALSE, dirlistonly = TRUE,
#                       userpwd = sUserPwd44)
#         zips <- unlist(strsplit(doc, '\n'))
# 
#         # берём заявки, открытые в периодах, начинающихся с sYEAR
#         for (y in sYEAR) {
# 
#             i.folder <- i.folder + 1
#             cat(red(paste0(i.folder, ' папка из ', n.folders,
#                            ' (', round(i.folder / n.folders * 100, 1), '%)\n')))
#             # запись в лог
#             uf.write.to.log(paste0(Sys.time(), ': year ', y), sLogFileName)
# 
#             zips.y <- grep(zips, pattern = paste0('_', y, '.*_.*_.*'), value = T)
# 
#             for (sZip in zips.y) {
# 
#                 dest.filename <- paste0(sDataSamplePath, sSubf, sZip)
#                 if (!file.exists(dest.filename)) {
# 
#                     # запись в лог
#                     uf.write.to.log(paste0(Sys.time(), ': download file ', sZip),
#                                     sLogFileName)
# 
#                     download.file(paste0('ftp://', sUserPwd44, '@',
#                                          gsub('^.*://', '', sRegion), sSubf, sZip),
#                                   destfile = dest.filename)
#                 }
#             }
#         }
#     }
# }
