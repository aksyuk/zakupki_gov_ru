#!/bin/bash 
 cd ./scanner_app/
# создаём текстовые файлы со списками содержимого ftp-сервера
touch du.txt 
 > du.txt
# стучимся в ftp, перемещаемся в директорию региона 
lftp -u free,free ftp://ftp.zakupki.gov.ru/ << EOF 
cd ./fcs_regions/
echo 'Adygeja_Resp' >> du.txt 
cd ./Adygeja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Altaj_Resp' >> du.txt 
cd ./Altaj_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
exit 
EOF