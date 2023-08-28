
# Скрипты для загрузки данных с ftp госзакупок   

Автор: Светлана Суязова (Аксюк) [s.a.aksuk@gmail.com](mailto:s.a.aksuk@gmail.com)     

Последнее большое обновление: 08 Mar 2020 :bouquet: :ice_cream: :coffee:     

*Последнее обновление*: 28 Aug 2023 :book: :sunflower: :baby_bottle:     
Обновлено руководство по загрузке данных: добавлены списки переменных в итоговых таблицах данных.      

# Справочник по скриптам   

Порядок выполнения скриптов для загрузки данных по электронным аукционам с [ftp.zakupki.gov.ru](http://ftp.zakupki.gov.ru/) по 44ФЗ:   
  
1. `parser-ftp-01_Setup-fz44.R` -- подготовка рабочего пространства, создание директорий под данные.
2. `parser-ftp-02_Load-Unzip-and-Index-fz44.R` -- загрузка и распаковка (сторонними средствами), индексация скачанных файлов. Самый длительный этап. :clock730:       
3. `parser-ftp-03_Parse-Loaded-XML-fz44.R` -- парсинг распакованных XML-файлов по закупкам. Второй по длительности этап :clock330:. В файле ./data/reference/df_xml_patterns.csv находятся xpath-запросы для разбора разных типов xml-файлов.   
4. `parser-ftp-04_Prepare-Data-for-Models.R` -- вычисление переменных под модели зависимости процента экономии от переменных, характеризующих конкуренцию в электронных аукционах. Для экономии памяти вычисления и совмещение таблиц делаются отдельно по каждому региону, и результат сохраняется в папку `csv` внутри папки с выгрузкой данных по именем `DT_model_EA44_<НАЗВАНИЕ РЕГИОНА>.csv`. Здесь мы выбираем данные только по успешным электронным аукционам, чем сильно ограничиваем выборку. Ситуация осложняется тем, что идентификатор заявки `purchiseNumber` может дублироваться в нескольких аукционах, и непонятно, что с этим делать. Сейчас скрипт оставляет только самые свежие из протоколов и заявок с одним и тем же `purchiseNumber`. В идеале надо смотреть глубже и связывать такие повторы в отдельные цепочки. Пока не знаю как. :woman_shrugging:   
5. `parser-ftp-04_Prepare-Data-for-Models_2.R` -- данные для модели для сравнения количества неудачных аукционов в товарных группах, связанных с ИТ, vs все остальные. Добавлены результаты процедур с одной заявкой, с одним участником, отменённые объявления и протоколы, а также загадочные файлы `fcs_PlacementResults.xml`. Там обнаружены, в частности, результаты электронных аукционов, для которых нет xml-файлов с протоколами :question: Мне ещё предстоит разобраться в причинах этого, но пока что из `fcs_PlacementResults.xml` взяты финальная цена и факт того, что победитель уклонился от заключения контракта (там есть сведения по второму номеру).         


Немного подробнее о содержимом ftp здесь: [`filenames_description.md`](https://github.com/aksyuk/zakupki_gov_ru/blob/master/filenames_description.md).   

Ещё вам понадобится:    

* много свободного места на жёстком диске    
* много времени     
* много оперативной памяти    
* FileZilla или другой ftp-менеджер для загрузки архивов (в R это дольше и без возможности докачки)    
* архиватор для распаковки архивов (в R получается дольше и чревато ошибками)    
 
 
# Актуальное руководство по загрузке данных    

[см. здесь](https://github.com/aksyuk/zakupki_gov_ru/blob/master/doc/README.md)   

# План работы   

Что планируется улучшить:   

1. Добавить интерфейс (приложение shiny app?) для настройки параметров загрузки данных :date: до 24.09.2023     

1. Сделать схемы взаимодействия файлов и переменных, чтобы улучшить понимание работы кода :date: до 10.09.2023   

1. Переписать код через dplyr, дабы минимизировать кровь из глаз, и почистить косяки в подготовке модельных данных :date: до 10.09.2023.    
