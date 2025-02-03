#!/bin/bash 
 cd ./scanner_app/
# создаём текстовые файлы со списками содержимого ftp-сервера
touch du.txt 
 > du.txt
# стучимся в ftp, перемещаемся в директорию региона 
lftp -u free,free ftp://ftp.zakupki.gov.ru/ << EOF 
cd ./fcs_regions/
echo 'Adygeja_Resp' >> du.txt 
echo 'Adygeja_Resp'
cd ./Adygeja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Altaj_Resp' >> du.txt 
echo 'Altaj_Resp'
cd ./Altaj_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Altajskij_kraj' >> du.txt 
echo 'Altajskij_kraj'
cd ./Altajskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Amurskaja_obl' >> du.txt 
echo 'Amurskaja_obl'
cd ./Amurskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Arkhangelskaja_obl' >> du.txt 
echo 'Arkhangelskaja_obl'
cd ./Arkhangelskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Astrakhanskaja_obl' >> du.txt 
echo 'Astrakhanskaja_obl'
cd ./Astrakhanskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Bajkonur_g' >> du.txt 
echo 'Bajkonur_g'
cd ./Bajkonur_g/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Bashkortostan_Resp' >> du.txt 
echo 'Bashkortostan_Resp'
cd ./Bashkortostan_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Belgorodskaja_obl' >> du.txt 
echo 'Belgorodskaja_obl'
cd ./Belgorodskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Brjanskaja_obl' >> du.txt 
echo 'Brjanskaja_obl'
cd ./Brjanskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Burjatija_Resp' >> du.txt 
echo 'Burjatija_Resp'
cd ./Burjatija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Chechenskaja_Resp' >> du.txt 
echo 'Chechenskaja_Resp'
cd ./Chechenskaja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Cheljabinskaja_obl' >> du.txt 
echo 'Cheljabinskaja_obl'
cd ./Cheljabinskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Chukotskij_AO' >> du.txt 
echo 'Chukotskij_AO'
cd ./Chukotskij_AO/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Chuvashskaja_Resp' >> du.txt 
echo 'Chuvashskaja_Resp'
cd ./Chuvashskaja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Dagestan_Resp' >> du.txt 
echo 'Dagestan_Resp'
cd ./Dagestan_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Donetskaya_NR' >> du.txt 
echo 'Donetskaya_NR'
cd ./Donetskaya_NR/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Evrejskaja_Aobl' >> du.txt 
echo 'Evrejskaja_Aobl'
cd ./Evrejskaja_Aobl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Hersonskaya_obl' >> du.txt 
echo 'Hersonskaya_obl'
cd ./Hersonskaya_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Ingushetija_Resp' >> du.txt 
echo 'Ingushetija_Resp'
cd ./Ingushetija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Irkutskaja_obl' >> du.txt 
echo 'Irkutskaja_obl'
cd ./Irkutskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Ivanovskaja_obl' >> du.txt 
echo 'Ivanovskaja_obl'
cd ./Ivanovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Jamalo-Neneckij_AO' >> du.txt 
echo 'Jamalo-Neneckij_AO'
cd ./Jamalo-Neneckij_AO/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Jaroslavskaja_obl' >> du.txt 
echo 'Jaroslavskaja_obl'
cd ./Jaroslavskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kabardino-Balkarskaja_Resp' >> du.txt 
echo 'Kabardino-Balkarskaja_Resp'
cd ./Kabardino-Balkarskaja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kaliningradskaja_obl' >> du.txt 
echo 'Kaliningradskaja_obl'
cd ./Kaliningradskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kalmykija_Resp' >> du.txt 
echo 'Kalmykija_Resp'
cd ./Kalmykija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kaluzhskaja_obl' >> du.txt 
echo 'Kaluzhskaja_obl'
cd ./Kaluzhskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kamchatskij_kraj' >> du.txt 
echo 'Kamchatskij_kraj'
cd ./Kamchatskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Karachaevo-Cherkesskaja_Resp' >> du.txt 
echo 'Karachaevo-Cherkesskaja_Resp'
cd ./Karachaevo-Cherkesskaja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Karelija_Resp' >> du.txt 
echo 'Karelija_Resp'
cd ./Karelija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kemerovskaja_obl' >> du.txt 
echo 'Kemerovskaja_obl'
cd ./Kemerovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Khabarovskij_kraj' >> du.txt 
echo 'Khabarovskij_kraj'
cd ./Khabarovskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Khakasija_Resp' >> du.txt 
echo 'Khakasija_Resp'
cd ./Khakasija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Khanty-Mansijskij_AO-Jugra_AO' >> du.txt 
echo 'Khanty-Mansijskij_AO-Jugra_AO'
cd ./Khanty-Mansijskij_AO-Jugra_AO/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kirovskaja_obl' >> du.txt 
echo 'Kirovskaja_obl'
cd ./Kirovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Komi_Resp' >> du.txt 
echo 'Komi_Resp'
cd ./Komi_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kostromskaja_obl' >> du.txt 
echo 'Kostromskaja_obl'
cd ./Kostromskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Krasnodarskij_kraj' >> du.txt 
echo 'Krasnodarskij_kraj'
cd ./Krasnodarskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Krasnojarskij_kraj' >> du.txt 
echo 'Krasnojarskij_kraj'
cd ./Krasnojarskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Krim_Resp' >> du.txt 
echo 'Krim_Resp'
cd ./Krim_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kurganskaja_obl' >> du.txt 
echo 'Kurganskaja_obl'
cd ./Kurganskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Kurskaja_obl' >> du.txt 
echo 'Kurskaja_obl'
cd ./Kurskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Leningradskaja_obl' >> du.txt 
echo 'Leningradskaja_obl'
cd ./Leningradskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Lipeckaja_obl' >> du.txt 
echo 'Lipeckaja_obl'
cd ./Lipeckaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Luganskaya_NR' >> du.txt 
echo 'Luganskaya_NR'
cd ./Luganskaya_NR/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Magadanskaja_obl' >> du.txt 
echo 'Magadanskaja_obl'
cd ./Magadanskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Marij_El_Resp' >> du.txt 
echo 'Marij_El_Resp'
cd ./Marij_El_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Mordovija_Resp' >> du.txt 
echo 'Mordovija_Resp'
cd ./Mordovija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Moskovskaja_obl' >> du.txt 
echo 'Moskovskaja_obl'
cd ./Moskovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Moskva' >> du.txt 
echo 'Moskva'
cd ./Moskva/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Murmanskaja_obl' >> du.txt 
echo 'Murmanskaja_obl'
cd ./Murmanskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Neneckij_AO' >> du.txt 
echo 'Neneckij_AO'
cd ./Neneckij_AO/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Nizhegorodskaja_obl' >> du.txt 
echo 'Nizhegorodskaja_obl'
cd ./Nizhegorodskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Novgorodskaja_obl' >> du.txt 
echo 'Novgorodskaja_obl'
cd ./Novgorodskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Novosibirskaja_obl' >> du.txt 
echo 'Novosibirskaja_obl'
cd ./Novosibirskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Omskaja_obl' >> du.txt 
echo 'Omskaja_obl'
cd ./Omskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Orenburgskaja_obl' >> du.txt 
echo 'Orenburgskaja_obl'
cd ./Orenburgskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Orlovskaja_obl' >> du.txt 
echo 'Orlovskaja_obl'
cd ./Orlovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Penzenskaja_obl' >> du.txt 
echo 'Penzenskaja_obl'
cd ./Penzenskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Permskij_kraj' >> du.txt 
echo 'Permskij_kraj'
cd ./Permskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Primorskij_kraj' >> du.txt 
echo 'Primorskij_kraj'
cd ./Primorskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Pskovskaja_obl' >> du.txt 
echo 'Pskovskaja_obl'
cd ./Pskovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Rjazanskaja_obl' >> du.txt 
echo 'Rjazanskaja_obl'
cd ./Rjazanskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Rostovskaja_obl' >> du.txt 
echo 'Rostovskaja_obl'
cd ./Rostovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Sakha_Jakutija_Resp' >> du.txt 
echo 'Sakha_Jakutija_Resp'
cd ./Sakha_Jakutija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Sakhalinskaja_obl' >> du.txt 
echo 'Sakhalinskaja_obl'
cd ./Sakhalinskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Samarskaja_obl' >> du.txt 
echo 'Samarskaja_obl'
cd ./Samarskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Sankt-Peterburg' >> du.txt 
echo 'Sankt-Peterburg'
cd ./Sankt-Peterburg/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Saratovskaja_obl' >> du.txt 
echo 'Saratovskaja_obl'
cd ./Saratovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Sevastopol_g' >> du.txt 
echo 'Sevastopol_g'
cd ./Sevastopol_g/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Severnaja_Osetija-Alanija_Resp' >> du.txt 
echo 'Severnaja_Osetija-Alanija_Resp'
cd ./Severnaja_Osetija-Alanija_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Smolenskaja_obl' >> du.txt 
echo 'Smolenskaja_obl'
cd ./Smolenskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Stavropolskij_kraj' >> du.txt 
echo 'Stavropolskij_kraj'
cd ./Stavropolskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Sverdlovskaja_obl' >> du.txt 
echo 'Sverdlovskaja_obl'
cd ./Sverdlovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tambovskaja_obl' >> du.txt 
echo 'Tambovskaja_obl'
cd ./Tambovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tatarstan_Resp' >> du.txt 
echo 'Tatarstan_Resp'
cd ./Tatarstan_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tjumenskaja_obl' >> du.txt 
echo 'Tjumenskaja_obl'
cd ./Tjumenskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tomskaja_obl' >> du.txt 
echo 'Tomskaja_obl'
cd ./Tomskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tulskaja_obl' >> du.txt 
echo 'Tulskaja_obl'
cd ./Tulskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tverskaja_obl' >> du.txt 
echo 'Tverskaja_obl'
cd ./Tverskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Tyva_Resp' >> du.txt 
echo 'Tyva_Resp'
cd ./Tyva_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Udmurtskaja_Resp' >> du.txt 
echo 'Udmurtskaja_Resp'
cd ./Udmurtskaja_Resp/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Uljanovskaja_obl' >> du.txt 
echo 'Uljanovskaja_obl'
cd ./Uljanovskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Vladimirskaja_obl' >> du.txt 
echo 'Vladimirskaja_obl'
cd ./Vladimirskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Volgogradskaja_obl' >> du.txt 
echo 'Volgogradskaja_obl'
cd ./Volgogradskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Vologodskaja_obl' >> du.txt 
echo 'Vologodskaja_obl'
cd ./Vologodskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Voronezhskaja_obl' >> du.txt 
echo 'Voronezhskaja_obl'
cd ./Voronezhskaja_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Zabajkalskij_kraj' >> du.txt 
echo 'Zabajkalskij_kraj'
cd ./Zabajkalskij_kraj/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
echo 'Zaporozhskaya_obl' >> du.txt 
echo 'Zaporozhskaya_obl'
cd ./Zaporozhskaya_obl/ 
du -bms ./contracts >> du.txt 
du -bms ./protocols >> du.txt 
du -bms ./notifications >> du.txt 
cd ../
exit 
EOF
