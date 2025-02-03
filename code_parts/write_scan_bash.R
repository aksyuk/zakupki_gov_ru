
flnm.bash.scan <- paste0('./scanner_app/', 'bash_scan_ftp.sh')

txt.line <- paste0('#!/bin/bash \n ', 
                   'cd ./scanner_app/\n',
                   '# создаём текстовые файлы со списками содержимого ftp-сервера\n',
                   'touch du.txt \n', ' > du.txt\n',
                   '# стучимся в ftp, перемещаемся в директорию региона \n',
                   'lftp -u free,free ftp://ftp.zakupki.gov.ru/ << EOF \n',
                   'cd ./fcs_regions/')
write_lines(txt.line, flnm.bash.scan)

for (reg in lstRegionFoldersNames$regions) {
    txt.line <- paste0("echo '", reg, "' >> du.txt \n",
                       "echo '", reg, "'\n",
                       'cd ./', reg, '/ \n',
                       'du -bms ./contracts >> du.txt \n',
                       'du -bms ./protocols >> du.txt \n',
                       'du -bms ./notifications >> du.txt \n',
                       'cd ../')
    write_lines(txt.line, flnm.bash.scan, append = T)
}

txt.line <- paste0('exit \n', 'EOF')
write_lines(txt.line, flnm.bash.scan, append = T)