
# загрузка библиотек
library('data.table')

# функция определения операционной системы
eval(parse('./functions/uf_get_os.R', encoding = 'UTF-8'))

# функция чтения таблицы данных и таблицы метаданных к ней
eval(parse('./functions/uf_read_table_with_metadata.R', encoding = 'UTF-8'))

fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_responsibleOrgs_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_restrictions_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_all_OKPD2_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_all_MNNName_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_all_KTRU_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_TPY_codes_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolEF1_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolEF2_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolEF3_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsPlacementResult_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolCancel_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolEFSingleApp_clean.csv'
fileName <- './data/raw/01_from201901to201912_loaded2020-02-20/csv/EA44/DT_fcsProtocolEFSinglePart_clean.csv'
DT <- uf.read.table.with.metadata(fileName)

DT
colnames(DT)


