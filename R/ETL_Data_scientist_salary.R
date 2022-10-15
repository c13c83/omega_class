#=======================================================
# author: César Carvalho
# professor: Wagner Bonat
# class: EDA
# task: create insights for EDA and import the dataset 
#       for next class.
# oackages: dplyr, stringr, lubridate,
#=======================================================

#extract_data -----

ETL_data_salary <- read.csv('data/Data_Science_Fields_Salary_Categorization.csv') 


# helpers ----
#rough way to exchange rates ----

#to get the exchange rates series from INR to USD

get_exchg <- function(year) {
  require(dplyr)
  
  url_path <- paste0('https://www.exchangerates.org.uk/',
                     'INR-USD-spot-exchange-rates-history-',
                     year,
                     '.html')
  
  ETL_url <- readLines(url(url_path), warn = FALSE,skipNul = TRUE)
  unlink(ETL_url)
  
  select_rows <- grep('            <tr class=\\\"col', ETL_url)
  
  df <- ETL_url[select_rows]
 
  sub_df <- unlist(strsplit(df, '</tr>'))
  
  select_rows_sub_df <- grep('<tr class=\\\"col', sub_df)
  
  data_raw <- sub_df[select_rows_sub_df]
  
  dplyr::tibble(ref_date = lubridate::dmy(regmatches(sub_df,regexpr('\\d{2}/\\d{2}/\\d{4}', sub_df))),
                usd =  regmatches(sub_df, regexpr('\\d{1}\\.\\d{3,4}</td>', sub_df)) %>% 
                  stringr::str_remove('(</td>)')
  )
  
}

# elegant way to get the exchange rates ---- 

elegant_get_exchg <- function(year) {
  
  require(tidyverse)
  require(xml2)

  url_path <- url(paste0('https://www.exchangerates.org.uk/',
                         'INR-USD-spot-exchange-rates-history-',
                         year,
                         '.html')
                  )
  
  html <- read_html(url_path)
  
  tibble(ref_date = html %>% 
           xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[3])") %>% 
           xml_text() %>% 
           str_extract(r"(\d\d/\d\d/\d{4})") %>% 
           lubridate::dmy(),
         usd = html %>% 
           xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[2])") %>% 
           xml_text() %>% 
           str_extract(r"(\d\.\d+$)") %>% 
           as.numeric())        
}


exchange_rupees_dolars_rough <- purrr::map_df(years, get_exchg)
exchange_rupees_dolars_elegant <- purrr::map_df(years, elegant_get_exchg)
  
check_results_exchg <- exchange_rupees_dolars_rough %>% 
  left_join(exchange_rupees_dolars_elegant,
            by = c('ref_date')) %>% 
  mutate(check = usd.y == usd.x)
  


#check & clean data ----

years <- unique(ETL_data_salary$Working_Year)

designations <- unique(ETL_data_salary$Designation)

experience <- unique(ETL_data_salary$Employment_Status)

exchange_rate <- 

                       
                
