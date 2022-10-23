#=======================================================
# author: César Carvalho
# professor: Wagner Bonat
# class: EDA
# task: create insights for EDA and import the dataset 
#       for next class.
# oackages: dplyr, stringr, lubridate, xml2, ggplot2,
# purrr, tidyr, readr
#=======================================================


# loading some packages the other packages will called only the specific functions ----
pckgs <- c('dplyr', 
           'stringr', 
           'ggplot2')
invisible(lapply(pckgs, library, character.only = TRUE))

#issues ----

# ????

#extract_data -----

ETL_data_salary <- read.csv('data/Data_Science_Fields_Salary_Categorization.csv') 

#glimpse -----
#check & clean data ----

columns_names <- names(ETL_data_salary)

 # X -> (int) id 
 # Working_Year -> (int) present date 
 # Designation -> (chr) job title
 # Experience
 # Employment_Status -> chr
 #   - "FT" -> full time
 #   - "PT" -> part time
 #   - "CT" -> contract ???
 #   - "FL" -> freelance
 # Salary_In_Rupees -> will be transformed do USD with year mean
 # Employee_Location ->  get country names to associate
 # Company_Location
 # Company_Size
 # Remote_Working_Ratio


years <- unique(ETL_data_salary$Working_Year)

designations <- sort(unique(ETL_data_salary$Designation))

experience <- unique(ETL_data_salary$Employment_Status)

ee_location <- unique(ETL_data_salary$Employee_Location)

co._location <- unique(ETL_data_salary$Company_Location)

country_without_co. <-  ee_location[ee_location %in% co._location]
country_without_co. <-  setdiff(ee_location, co._location)

# match(ee_location, ee_location == co._location)

# making helpers ----
#rough way to exchange rates ----

#to get the exchange rates series from INR to USD

get_exchg <- function(year) {
  # require(dplyr)
  
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

  url_path <- url(paste0('https://www.exchangerates.org.uk/',
                         'INR-USD-spot-exchange-rates-history-',
                         year,
                         '.html')
                  )
  
  html <- xml2::read_html(url_path)
  
  dplyr::tibble(ref_date = html %>%
                  xml2::xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[3])") %>%
                  xml2::xml_text() %>% 
                  stringr::str_extract(r"(\d\d/\d\d/\d{4})") %>% 
                  lubridate::dmy(),
                usd = html %>% 
                  xml2::xml_find_all(r"(//tr[@class='colone' or @class='coltwo']/td[2])") %>% 
                  xml2::xml_text() %>% 
                  stringr::str_extract(r"(\d\.\d+$)") %>% 
                  as.numeric())        
}


exchange_rupees_dolars_rough <- purrr::map_df(years, get_exchg)
exchange_rupees_dolars_elegant <- purrr::map_df(years, elegant_get_exchg)
  
check_results_exchg <- exchange_rupees_dolars_rough %>% 
  left_join(exchange_rupees_dolars_elegant,
            by = c('ref_date')) %>% 
  mutate(check = usd.y == usd.x)

n_exchg_elegant <- length(unique(exchange_rupees_dolars_elegant$ref_date))
n_exchg_rough <- length(unique(exchange_rupees_dolars_rough$ref_date))
n_res_exchg_check <- length(check_results_exchg$check)

if(n_exchg_elegant == n_exchg_rough) n_exchg_elegant == n_res_exchg_check

year_mean <- exchange_rupees_dolars_elegant %>% 
  mutate(year = lubridate::year(ref_date)) %>% 
  group_by(year) %>% 
    summarise(mean = mean(usd))


#sanitize data

#exchange rupees to dollar by yearly mean


df <- ETL_data_salary %>% 
  mutate(Rate_USD = purrr::map(Working_Year, function(x) year_mean$mean[x == year_mean$year])) %>% 
  tidyr::unnest(cols = c(Rate_USD)) %>% 
  mutate(Salary_In_Rupees = readr::parse_number(Salary_In_Rupees, locale = readr::locale(decimal_mark = "."))) %>% 
  mutate(Salary_In_USD_Dollars = Salary_In_Rupees * Rate_USD) %>% 
  relocate(Salary_In_USD_Dollars, .before = Salary_In_Rupees)
 


#classifying columns ----


table(ETL_USD$Working_Year)
prop.table(ETL_USD$Working_Year)


df$Working_Year <- factor(ETL_USD$Working_Year, levels = c('2020', '2021', '2022'))
df$Company_Size <- factor(ETL_USD$Company_Size, levels = c('S', 'M', 'L'))
df$Experience <- factor(ETL_USD$Experience, levels = c('EN', 'MI', 'SE', 'EX'))
df$Employment_Status <- factor

my_factor <- function()

df <- df %>% 
mutate(across(.cols = c(Designation,
                        Employee_Location,
                        # Employment_Status,
                        Company_Location),
              as.factor))




table(df$Employment_Status)

head(ETL_USD)

summary(df)

ggplot(df) +
  geom_bar(aes(x = Designation)) +
  coord_flip()
