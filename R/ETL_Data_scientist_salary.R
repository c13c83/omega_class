#=======================================================
# author: César Carvalho
# professor: Wagner Bonat
# class: EDA
# task: create insights for EDA and import the dataset 
#       for next class.
# 
#=======================================================

#extract_data -----

ETL_data_salary <- read.csv('data/Data_Science_Fields_Salary_Categorization.csv') 


url_path <- paste0('https://www.exchangerates.org.uk/',
                   'INR-USD-spot-exchange-rates-history-',
                   '2020',
                   '.html')


#rough way ----

#to get the exchange rates series from INR to USD

get_exchg <- function(url_path) {
  require(dplyr)
  
  ETL_url <- readLines(url(url_path), warn = FALSE,skipNul = TRUE)
  unlink(ETL_url)
  
  select_rows <- grep('            <tr class=\\\"col', ETL_url)
  
  df <- ETL_url[select_rows]
 
  sub_df <- unlist(strsplit(df, '</tr>'))
  
  select_rows_sub_df <- grep('<tr class=\\\"col', sub_df)
  
  data_raw <- sub_df[select_rows_sub_df]
  
  data_raw[2]
  
  lubridate::dmy(regmatches(sub_df,regexpr('\\d{2}/\\d{2}/\\d{4}', sub_df)))
  
  regmatches(sub_df, regexpr('\\d{1}\\.\\d{3,4}</td>', sub_df))

  dplyr::tibble(ref_date = lubridate::dmy(regmatches(sub_df,regexpr('\\d{2}/\\d{2}/\\d{4}', sub_df))),
                usd =  regmatches(sub_df, regexpr('\\d{1}\\.\\d{3,4}</td>', sub_df)) %>% 
                  stringr::str_remove('(</td>)')
  )
  
}
  
  

  teste <- get_exchg(url_path)
  
  
  
  
  
  test <- tibble(ref_date = lapply(sub_df, function(x) lubridate::dmy(regmatches(x,regexpr('\\d{2}/\\d{2}/\\d{4}', x)))) %>%  unlist(),
          usd = lapply(sub_df, function(x) regmatches(x, regexpr('\\d{1}\\.\\d{3,4}', x))) %>% unlist())
  
  df[1]
  txt <- sub_df[1]
  
  split_rows <- function(row_id) {
    sub_df <- strsplit(df[1], '</tr>')[[1]]
    
    res <- tibble(ref_date = str_extract(sub_df, '\\d{2}/\\d{2}/\\d{4}'),
           usd = lubridate::dmy(regmatches(txt,regexpr('\\d{2}/\\d{2}/\\d{4}',txt)))
    )
    
    
    read_values <- function(txt) {
      return( lubridate::dmy(regmatches(txt,regexpr('\\d{2}/\\d{2}/\\d{4}',txt))),
      regmatches(txt, regexpr('\\d{1}\\.\\d{3,4}', txt)) )
    }
    
    res <- map(sub_df %>% unlist(), read_values) 
     
   
  }
  
}

lubridate::dmy('01/01/2020')

# ETL_exchg_rate_html <- read_file(url("https://www.exchangerates.org.uk/INR-USD-spot-exchange-rates-history-2020.html"))

exchg_rate_split <- ETL_exchg_rate_html %>% 
  str_split('\n')

cat(ETL_exchg_rate_html)

init_split_table <- grep("<table", exchg_rate_split[[1]])
end_split_table <- grep("</table>", exchg_rate_split[[1]])

exchg_rate_split_tr <- exchg_rate_split[[1]][init_split_table[2]:end_split_table[2]]

init_split_tr <- grep('<tr', exchg_rate_split_tr)
end_split_tr <- grep('</tr', exchg_rate_split_tr)

#getsecond tr ake function to get the sae value in init and end

exchg_rate_split_month <- exchg_rate_split_tr[7] %>% str_split('</tr>')

exchg_rate_rows <- grep('<tr class=\\\"col', exchg_rate_split_month[[1]])

# make a loop to get the two values
#sample

txt_raw <- exchg_rate_split_month[[1]][exchg_rate_rows[2]]

ref_date <- str_extract(txt_raw, '\\d{2}/\\d{2}/\\d{4}')
usd <- str_extract(txt_raw, '\\d{1}\\.\\d{3}')







match(exchg_rate_split[[1]], exchg_rate_split[[1]] = str_detect(exchg_rate_split[[1]], '<a name="INR-USD-history-table"></a>'))




x <- c(1:40)

match(x, x = 5)

row_number(init_line[m])

df_exchg[[1]][init_line:length(ETL_exchg_rate_html[[1]])]

ETL_exchg_rate <- ETL_exchg_rate_html[[1]][5]

#check & clean data ----

years <- unique(ETL_data_salary$Working_Year)

designations <- unique(ETL_data_salary$Designation)

experience <- unique(ETL_data_salary$Employment_Status)

exchange_rate <- 


#better way
                       
library(tidyverse)
library(xml2)

u <- url("https://www.exchangerates.org.uk/INR-USD-spot-exchange-rates-history-2020.html")
html <- read_html(u)

tibble(dt_ref = html %>% 
         xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[3])") %>% 
         xml_text() %>% 
         str_extract(r"(\d\d/\d\d/\d{4})") %>% 
         lubridate::dmy(),
       usd = html %>% 
         xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[2])") %>% 
         xml_text() %>% 
         str_extract(r"(\d\.\d+$)") %>% 
         as.numeric())                        
