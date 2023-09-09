
library('curl')
library('dplyr')

list.files <- curl::new_handle()
curl::handle_setopt(list.files, ftp_use_epsv = T, dirlistonly = F)
url <- paste0(gsub('(.*)://(.*)', '\\1://', s.FTP.URL), 
              s.USER.PWD.44, '@',
              gsub('(.*)://(.*)', '\\2', s.FTP.URL), 'fcs_regions/')

for (reg in sRegionFoldersNames[1:2]) {
    reg.url <- paste0(url, reg, '/')
    # message(reg.url)
    
    tbl.ls <- curl_fetch_memory(url = reg.url,
                                handle = list.files)[["content"]] |>
        rawToChar() |>
        read.table(text = _)
    
    tbl.ls <- tbl.ls %>% mutate(size.Mb = V5 / 10^6, dir = V9) %>%
        select(dir, size.Mb)
    
    print(tbl.ls)
    
    # dirs <- unname(sapply(files, function(x) {
    #     gsub('.* ', '', files)
    # }))
    # sizes <- unname(sapply(files, function(x) {
    #     gsub('.* ', '', files)
    # }))
    # 
    # print(dirs)
}

# system(file = './code_parts/ls_dir_sizes.sh')  # не работает
    