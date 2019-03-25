library(rvest)
library(httr)
Sys.setenv('R_MAX_VSIZE'=32000000000)
# Install package if not exists
pkgs <- c('data.table', "magrittr", 'reshape2', 'lubridate', 'stringr', 'dplyr', 'tidyr',
          'tidyverse','googlesheets','openxlsx','psych','clipr','formattable',
          'ROracle','dbplyr','DBI')

new.pkgs <- pkgs[!(pkgs %in% installed.packages())]
if (length(new.pkgs)) {
  install.packages(new.pkgs, repos = 'http://cran.csie.ntu.edu.tw/')
}
# Require packages
##lapply : 以迴圈方式require()啟用packages
lapply(pkgs, require, character.only = TRUE)

#------------------------------------#
#useful function

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_data_frame(x = .)
  return(t_df)
}

get_colnames <- function(df, .collapse = ", ") {
  final <- df %>% colnames() %>% str_c(collapse = .collapse)
  return(final)
}

#Mac本機端與Oracle內的中文問題
#Sys.setenv(NLS_LANG="TRADITIONAL CHINESE_TAIWAN.UTF8")
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")

#---------------------------------------------------------#
#開始爬蟲#

###把之前爬的link讀進來
text_all <- read_rds("/Users/dtseng02/Documents/Dennis/text_all.rds")
text_all_link <- text_all %>% mutate(link = str_c("https://pttweb.tw",link))

###這邊其實只是想創一個空的tibble之後去bind_rows()
push_df <- list()
main_df <- tibble(meta_author="", meta_board="", meta_time="", meta_comments="", meta_push="", main_text="", id="") %>% filter(row_number()<1)

###確認一下頁面index的number從第一個link裡面開始爬
number_complete = 1

###因為有些連結會死掉所以用purrr::possible，爬不到設定為null
p_read_html <- possibly(read_html, otherwise = NULL)

for(i in 1:300) {#number_latest
  #number_complete = 281
  j = number_complete
  k = j + 9
  
  ###用p_read_html()把每頁文章的html抓下來，接下來按照原始文章id幫list貼名字
  ###compact()可以把null弄掉排毒成功！
  ###原本是抓下來每個小元素底下合併成dataframe再加上id當作一個column
  ###現在用set_names()的好處是省時先命名，再來是就算有連結死掉也可以踢掉它
  ###不用把底下的id_raw寫死一定要十個row
  html_raw = text_all_link[j:k,] %>% pull(link) %>% 
    map(function(x){x %>% p_read_html()}) %>% 
    set_names(pull(text_all_link[j:k,"id"])) %>% compact()
  # id_raw = text_all_link[j:k,] %>% select(id) ###原本寫死的
  
  ###爬文章的meta資料
  ### meta #css_meta <- c(".author a","span.board","span.post-time","link+ span",".head-item .push-tag")
  meta_author   <- html_raw %>% map(function(x){x %>% html_nodes(".author a") %>% html_text() %>% '['(2)})
  meta_board    <- html_raw %>% map(function(x){x %>% html_nodes("span.board") %>% html_text() %>% '['(2)})
  meta_time     <- html_raw %>% map(function(x){x %>% html_nodes("span.post-time") %>% html_text()})
  meta_comments <- html_raw %>% map(function(x){x %>% html_nodes("link+ span") %>% html_text()})
  meta_push     <- html_raw %>% map(function(x){x %>% html_nodes(".head-item .push-tag") %>% html_text() %>% str_c(collapse = "_")})
  
  main_meta_df <- tibble(meta_author=unlist(meta_author),meta_board=unlist(meta_board),meta_time=unlist(meta_time),meta_comments=unlist(meta_comments),meta_push=unlist(meta_push),id=names(meta_author))
  
  ###爬文章的text資料
  ### main #".f3.hl",".f3.push-content",".push-ipdatetime"
  main_text_tmp <- html_raw %>% map(function(x){x %>% html_nodes(xpath = '//*[@id="main-content"]/node()[not(self::div)]') %>% html_text() %>% bind_cols(main_all=.) %>% mutate(ID=row_number())})
  main_IP  <- main_text_tmp %>% map(function(x){x %>% filter(str_detect(main_all,"發信站"))})
  ###IP欄位以下刪掉但有些例外沒有IP所以用if(length(x)==0)處理，等於0代表沒IP，只好給定一個超大值
  main_stop_row_id <- main_IP %>% map(function(x){x %>% pull(ID)}) %>% map(function(x){if(length(x)==0) x = 10000 else(x)})
  main_text_clean <- map2(main_text_tmp, main_stop_row_id, function(x,y){x %>% filter(ID < y) %>%
      mutate(main_all = str_replace_all(main_all, "\\n","")) %>%
      pull(main_all) %>% str_c(collapse = " ") %>% bind_cols(main_text=.)})
  main_text_df <- tibble(main_text=unlist(main_text_clean))#id=names(main_text_clean),
  
  ###合併meta與text
  main_df_tmp <- bind_cols(main_meta_df, main_text_df)
  
  ###爬文章的push資料
  ### push # css_push <- c(".f3.hl",".f3.push-content",".push-ipdatetime")
  push_author  <- html_raw %>% map(function(x){x %>% html_nodes(".push") %>% html_nodes(".f3.hl") %>% html_text()})
  push_content <- html_raw %>% map(function(x){x %>% html_nodes(".f3.push-content") %>% html_text()})
  push_time    <- html_raw %>% map(function(x){x %>% html_nodes(".push-ipdatetime") %>% html_text()})
  push_df_tmp <- pmap(list(push_author,push_content,push_time),function(x,y,z){bind_cols(push_author=unlist(x),push_content=unlist(y),push_time=unlist(z))})
  
  ###避免原本寫死的命名，現在已經踢掉死掉的連結了
  names(push_df_tmp) <- names(push_df_tmp) %>% str_c("ID#", .)
  
  ###用完整的tibble/list去bind這次迴圈抓下來的tibble
  push_df <- c(push_df, push_df_tmp)
  main_df <- main_df %>% bind_rows(main_df_tmp)
  
  ###印出數字告訴自己進度然後更新要爬的頁數
  print(number_complete)
  number_complete = number_complete + 10
  
  ###休息一下
  tmsleep<-sample(11:15,1)
  Sys.sleep(tmsleep)
}

###寫檔案出去
main_df %>% write_rds("/Users/dtseng02/Documents/Dennis/main_df.rds")
push_df %>% write_rds("/Users/dtseng02/Documents/Dennis/push_df.rds")

###如果爬到一半關掉記得先讀檔案進來
main_df <- read_rds("/Users/dtseng02/Documents/Dennis/main_df.rds")
push_df <- read_rds("/Users/dtseng02/Documents/Dennis/push_df.rds")

###關掉用不到的連線
closeAllConnections()
gc()
