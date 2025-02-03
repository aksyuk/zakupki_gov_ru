
library('readr')

# читаем скрипт
cmd.text <- read_lines('./code_parts/bash_scan_ftp.sh')
# удаляем пустые строки
cmd.text <- cmd.text[cmd.text != '']
# удаляем комментарии
cmd.text <- cmd.text[!grepl('^#', cmd.text)]
# склеиваем команды в одну длинную строку
cmd.text <- paste0(cmd.text, collapse = ';')

system(cmd.text)
