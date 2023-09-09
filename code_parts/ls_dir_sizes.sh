#!/bin/bash 
cd /home/light/git-repos/zakupki_gov_ru/scanner_app/
# создаём текстовые файлы со списками содержимого ftp-сервера 
touch du.txt
 > du.txt
# стучимся в ftp, перемещаемся в директорию региона 
lftp -u free,free ftp://ftp.zakupki.gov.ru/ << EOF 
echo 'Altaj_Resp' >> du.txt
cd ./fcs_regions/Altaj_Resp/
du -bms ./contracts >> du.txt
du -bms ./protocols >> du.txt
du -bms ./notifications >> du.txt 
exit 
EOF

