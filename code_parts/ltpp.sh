#!/bin/bash

cat << EOF > load_from_ftp_test.sh
#!/bin/bash

# рабочая директория -- папка с выгрузкой по региону за период
cd ~/git-repos/zakupki_gov_ru/data/raw/30_from202101to202112_loaded2023-09-01/
# создаём текстовые файлы со списками содержимого ftp-сервера
touch ls.txt ls_by_period.txt load_archives.sh
 > ls.txt > ls_by_period.txt > load_archives.sh

# стучимся в ftp, перемещаемся в директорию региона
lftp -u free,free ftp://ftp.zakupki.gov.ru
cd ./fcs_regions/Bashkortostan_Resp/

# cкачиваем имена архивов из директории contracts в файл ls.txt
cd ./contracts
ls -lh >> ls.txt
cd ../

# cкачиваем имена архивов из директории notifications в файл ls.txt
cd ./notifications
ls -lh >> ls.txt
cd ../

# cкачиваем имена архивов из директории protocols в файл ls.txt
cd ./protocols
ls -lh >> ls.txt
cd ../

# закрываем соединение с ftp сервером
exit

# фильтруем все имена архивов по s_YEAR_MON
grep "_202101[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202102[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202103[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202104[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202105[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202106[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202107[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202108[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202109[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202110[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202111[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}\\|_202112[[:digit:]]\\{4\\}_202[[:digit:]]\\{7\\}" ./ls.txt > ls_by_period.txt

# размер всех файлов по региону и периоду в мегабайтах
awk -F' ' '{print $5}' ls_by_period.txt | awk '{ sum += $0 / 10^6} END { if (sum) print sum }' 

# пишем скрипт для поимённого скачивания только нужных файлов
echo -e '#!/bin/bash' > load_archives.sh
echo -e 'lftp -u free,free ftp://ftp.zakupki.gov.ru << EOF \ncd ./fcs_regions/Bashkortostan_Resp/contracts \nset xfer:clobber on' >> load_archives.sh
awk -F'.*contract' '{print $2}' ls_by_period.txt | awk '!/^$/' | sed -e 's/^/get contract/' >> load_archives.sh
echo -e 'cd ../protocols \nset xfer:clobber on' >> load_archives.sh
awk -F'.*protocol' '{print $2}' ls_by_period.txt | awk '!/^$/' | sed -e 's/^/get protocol/' >> load_archives.sh
echo -e 'cd ../notifications \nset xfer:clobber on' >> load_archives.sh
awk -F'.*notification' '{print $2}' ls_by_period.txt | awk '!/^$/' | sed -e 's/^/get notification/' >> load_archives.sh
echo -e 'exit \nEOF' >> load_archives.sh

# качаем
#cd ./archives
#../load_archives.sh

EOF