# ..............................................................................
# uf.format.address.for.OSM()
#
# Функция для преобразования вектора исходных адресов из базы госзакупок
#  к виду, более-менее пригодному для поиска координат по адресу в базе 
#  OpenStreetMap.
#
# Аргументы:
#  * address       -- адрес
#
# Возвращаемое значение: вектор отформатированных адресов.
# 
# Создаёт / модифицирует файлы: нет. 
# ..............................................................................

uf.format.address.for.OSM <- function(address) {
    # переносим город в конец адреса
    # шаблоны для того, чтобы удалить названия городов из адреса
    ptt.city.remove <- 
        list(
            "пос[.]\\s?ж[.]\\s?д[.]\\sст.([^,]*),", # пос. ж. д. ст. Название,
            "п[.]г[.]т[.]([^,]*),",                 # п.г.т. Название
            "г[.]([^,]*),",                         # г. Название
            ",([^,]*)\\sг,",                        # Название г,
            ",([^,]*)\\sГород,",                    # Название Город,
            ",([^,]*)\\sпгт,",                      # Название пгт,
            ",([^,]*)\\sс,",                        # Название с,
            "с[.]([^,]*),",                         # с. Название
            "пос\\s([^,]*),",                       # пос Название
            "пос.([^,]*),",                         # пос. Название
            ",([^,]*)\\sпос([.]|\\s),"              # Название пос. ,
        )
    # шаблоны для того, чтобы вытащить названия городов
    ptt.city.pull <- lapply(ptt.city.remove, function(x) {
        x <- paste0('.*', x, '.*')
    })
    
    # вытаскиваем из адресов города и сохраняем отдельно
    cities <- address
    sapply(ptt.city.pull, function(x) {
        cities[cities == address] <<- 
            gsub(cities[cities == address], pattern = x, replacement = "\\1")
    })
    cities <- trimws(cities, 'both')
    
    # убираем города из адресов
    address.OSM <- address
    sapply(ptt.city.remove, function(x) {
        address.OSM[address.OSM == address] <<- 
            gsub(address.OSM[address.OSM == address], pattern = x, replacement = "")
    })
    
    # убираем район
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*\\sр-н(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*\\sрайон(,?.*)', 
                        replacement = '\\1\\2')
    # убираем сельское поселение
    address.OSM <- gsub(address.OSM, pattern = '(.*,)[^\\D]*сельское поселение(,?.*)', 
                        replacement = '\\1\\2')
    
    # убираем индексы
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)\\d{6}(,?.*)', 
                        replacement = '\\1\\2')
    # убираем РФ
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Российская Федерация(,?.*)', 
                        replacement = '\\1\\2')
    # убираем Татарстан Респ
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Республика(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Респ(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Республика Татарстан(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Татарстан Респ(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)Респ Татарстан(,?.*)', 
                        replacement = '\\1\\2')
    address.OSM <- gsub(address.OSM, pattern = '(.*,?)РТ(,?.*)', 
                        replacement = '\\1\\2')
    # убираем пустые запятые
    address.OSM <- gsub(address.OSM, pattern = ',\\s?,', replacement = '')
    # убираем ведущие пробелы
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    # убираем ведущие запятые
    address.OSM <- gsub(address.OSM, pattern = '^,\\s?', replacement = '')
    # и ещё раз ведущие пробелы
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    
    # всё в верхний регистр
    address.OSM <- toupper(address.OSM)
    # убираем кавычки
    address.OSM <- gsub(address.OSM, pattern = '\\"', replacement = '')
    
    # стандартизируем названия улиц 
    address.OSM <- gsub(address.OSM, pattern = 'УЛИЦА', replacement = 'УЛ.')
    address.OSM <- gsub(address.OSM, pattern = 'УЛ(\\s|[.])([^,]*),', 
                        replacement = '\\2 УЛИЦА,')
    address.OSM <- gsub(address.OSM, pattern = '(^|,)([^,]*)УЛ,', 
                        replacement = ', \\2 УЛИЦА,')
    
    address.OSM <- gsub(address.OSM, pattern = 'ПР-К?Т', replacement = 'ПРОСПЕКТ')
    address.OSM <- gsub(address.OSM, pattern = 'ПР.\\s?МИРА', 
                        replacement = 'ПРОСПЕКТ МИРА')
    address.OSM <- gsub(address.OSM, pattern = '(\\s|,)ПР.', 
                        replacement = 'ПРОСПЕКТ')
    address.OSM <- gsub(address.OSM, pattern = '([^,]*)\\sПРОСПЕКТ', 
                        replacement = 'ПРОСПЕКТ \\1,')
    
    address.OSM <- gsub(address.OSM, pattern = '(\\s|,)ПР-Д\\s', 
                        replacement = 'ПРОЕЗД')
    address.OSM <- gsub(address.OSM, pattern = '^ПР-Д\\s', 
                        replacement = 'ПРОЕЗД')
    
    address.OSM <- gsub(address.OSM, pattern = '(,|\\s)Ш(,|\\s)', 
                        replacement = ' ШОССЕ')
    address.OSM <- gsub(address.OSM, pattern = '^Ш(,|\\s)', 
                        replacement = ' ШОССЕ ')
    
    address.OSM <- gsub(address.OSM, pattern = '(,|\\s)НАБ(,|\\s)', 
                        replacement = ' НАБЕРЕЖНАЯ')
    address.OSM <- gsub(address.OSM, pattern = '^НАБ(,|\\s)', 
                        replacement = ' НАБЕРЕЖНАЯ ')
    
    address.OSM <- gsub(address.OSM, pattern = '(^Б-Р\\s)|(\\sБ-Р\\s)|(\\sБ-Р,)', 
                        replacement = ' БУЛЬВАР ')
    address.OSM <- gsub(address.OSM, pattern = '(^ПЛ\\s)|(,\\s?ПЛ\\s)', 
                        replacement = ' ПЛОЩАДЬ ')
    address.OSM <- gsub(address.OSM, pattern = 'ПЛОЩАДЬ\\s([^,]*),', 
                        replacement = '\\1 ПЛОЩАДЬ,')
    
    
    # и ещё раз пробелы
    address.OSM <- gsub(address.OSM, pattern = '\\s+', replacement = ' ')
    address.OSM <- gsub(address.OSM, pattern = '^\\s*', replacement = '')
    
    # неполные названия улиц
    address.OSM <- gsub(address.OSM, pattern = '\\sЯХИНА,', 
                        replacement = 'РУСТЕМА ЯХИНА')
    address.OSM <- gsub(address.OSM, pattern = 'К.\\s?МАРКСА(\\s|,)', 
                        replacement = 'КАРЛА МАРКСА ')
    address.OSM <- gsub(address.OSM, pattern = 'Л.\\s?ТОЛСТОГО(\\s|,)', 
                        replacement = 'ЛЬВА ТОЛСТОГО ')
    address.OSM <- gsub(address.OSM, pattern = 'М.\\s?ДЖАЛИЛЯ(\\s|,)', 
                        replacement = 'МУСЫ ДЖАЛИЛЯ ')
    address.OSM <- gsub(address.OSM, pattern = 'Х.\\s?ТАКТАША(\\s|,)', 
                        replacement = 'ХАДИ ТАКТАША ')
    address.OSM <- gsub(address.OSM, pattern = 'АК.\\s?ПАРИНА(\\s|,)', 
                        replacement = 'АКАДЕМИКА ПАРИНА ')
    
    
    # разбираемся с домами и квартирами
    address.OSM <- gsub(address.OSM, pattern = 'ДОМ', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = 'АДМИНИСТРАТИВНОЕ ЗДАНИЕ', 
                        replacement = '')
    # убираем тире, помещения и квартиры в самом конце адреса
    address.OSM <- gsub(address.OSM, pattern = ',\\s?-\\s?$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ПОМ[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ПОМЕЩЕНИЕ[^,]*$', 
                        replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?ОФИС[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?КВ.[^,]*$', replacement = '')
    address.OSM <- gsub(address.OSM, pattern = ',\\s?КВАРТИРА[^,]*$', 
                        replacement = '')
    # если в конце два номера без метки помещения
    address.OSM <- gsub(address.OSM, pattern = '(,\\s?\\d*),\\s?\\d*$', 
                        replacement = '\\1')
    
    # вытаскиваем дома
    houses <- gsub(address.OSM, pattern = '.*,([^,]*)$', replacement = '\\1')
    address.OSM <- gsub(address.OSM, pattern = ',([^,]*)$', replacement = '')
    
    houses <- gsub(houses, pattern = 'Д[.]\\s?', replacement = '')
    # убираем ведущие пробелы
    houses <- gsub(houses, pattern = '^\\s*', replacement = '')
    
    # добавляем дома обратно
    address.OSM <- paste0(address.OSM, ', Д. ', toupper(houses))
    
    
    # добавляем город
    address.OSM <- paste0(address.OSM, ', ', toupper(cities))
    # добавляем страну
    address.OSM <- paste0(address.OSM, ', РОССИЯ')
    
    # доводим напильником: некоторые номера домов не находятся ни в гугле,
    #  ни в яндексе
    address.OSM <- gsub(address.OSM, pattern = 'ЛЕНИНА, Д. 26 Г/2', 
                        replacement = 'ЛЕНИНА, Д. 26')
    address.OSM <- gsub(address.OSM, pattern = ',\\s31A,', 
                        replacement = ', 31 А,')
    address.OSM <- 
        gsub(address.OSM, 
             pattern = '(ИОВЛЕВА|КОСМОНАВТОВ|НАРИМАНОВА|БОЛЬШАЯ КРАСНАЯ|КАРЛА МАРКСА|ЛЕНИНА|ВАХИТОВА|ЦЕНТРАЛЬНАЯ),', 
             replacement = '\\1 УЛИЦА,')
    
    return(address.OSM)
}
