---
title: "EDA in R"
author: "Cesar Carvalho"
date: '2022-10-24'
class: 'Omega DataScience'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r }
pckgs <- c('dplyr', 
           'stringr', 
           'ggplot2')
invisible(lapply(pckgs, library, character.only = TRUE))
```

# 1. Extracting data

```{r}
ETL_data_salary <- read.csv('data/Data_Science_Fields_Salary_Categorization.csv') 
```


### Getting columns
```{r}
columns_names <- names(ETL_data_salary)
```


## check & clean data


 X -> (int) id 
 Working_Year -> (int) present date 
 Designation -> (chr) job title
 Experience
 Employment_Status -> chr
   - "FT" -> full time
   - "PT" -> part time
   - "CT" -> contract ???
   - "FL" -> freelance
 Salary_In_Rupees -> will be transformed do USD with year mean
 Employee_Location ->  get country names to associate
 Company_Location
 Company_Size
 Remote_Working_Ratio


```{r}
years <- unique(ETL_data_salary$Working_Year)

designations <- sort(unique(ETL_data_salary$Designation))

experience <- unique(ETL_data_salary$Employment_Status)

ee_location <- unique(ETL_data_salary$Employee_Location)

co._location <- unique(ETL_data_salary$Company_Location)

country_with_co. <-  ee_location[ee_location %in% co._location]
country_without_co. <-  setdiff(ee_location, co._location)

```

# 2. making helpers

rough way to exchange rates 

<!-- #to get the exchange rates series from INR to USD -->
```{r}
url_is_ok <- function(url) if(attr(curlGetHeaders(url), 'status') == 200) TRUE
```


```{r}
get_exchg <- function(year) {
  # require(dplyr)
  
  url_path <- paste0('https://www.exchangerates.org.uk/',
                     'INR-USD-spot-exchange-rates-history-',
                     year,
                     '.html')

  if(!url_is_ok(url_path)) stop('problem with server, use the backup in /data')
  
  ETL_url <- readLines(url(url_path), warn = FALSE, skipNul = TRUE)
  
  cons <- getAllConnections()
  
  con <- getConnection(length(cons) - 1L)
  close(con)
  
  # stdin()
  select_rows <- grep('            <tr class=\\\"col', ETL_url)
  
  df <- ETL_url[select_rows]
 
  sub_df <- unlist(strsplit(df, '</tr>'))
  
  select_rows_sub_df <- grep('<tr class=\\\"col', sub_df)
  
  data_raw <- sub_df[select_rows_sub_df]
  
  dplyr::tibble(ref_date = lubridate::dmy(regmatches(sub_df,regexpr('\\d{2}/\\d{2}/\\d{4}', sub_df))),
                usd =  regmatches(sub_df, regexpr('\\d{1}\\.\\d{3,4}</td>', sub_df)) %>% 
                  stringr::str_remove('(</td>)')
  )
  # on.exit(close(con)) 
  # on.exit(closeAllConnections())

}
```

## elegant way to get the exchange rates 

```{r}


elegant_get_exchg <- function(year) {

  url_path <- url(paste0('https://www.exchangerates.org.uk/',
                         'INR-USD-spot-exchange-rates-history-',
                         year,
                         '.html')
                  )
  
  # if(!url_is_ok(url_path)) stop('problem with server, could use from local data')
  
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

```

## just checking the two methods to get exchange

```{r}


exchange_rupees_dolars_rough <- purrr::map_df(years, get_exchg)
exchange_rupees_dolars_elegant <- purrr::map_df(years, elegant_get_exchg)
  
check_results_exchg <- exchange_rupees_dolars_rough %>% 
  left_join(exchange_rupees_dolars_elegant,
            by = c('ref_date')) %>% 
  mutate(check = usd.y == usd.x)

n_exchg_rough <- length(unique(exchange_rupees_dolars_rough$ref_date))
n_exchg_elegant <- length(unique(exchange_rupees_dolars_elegant$ref_date))
n_res_exchg_check <- length(check_results_exchg$check)
```

## after run this, if the results are the same, it will remove 'n_exchg_rough',
## and when run again you will get an error, so run the code from init again
```{r}


if(n_exchg_elegant == n_exchg_rough) {
  if (n_exchg_elegant == n_res_exchg_check) cat('the result is equal \n')
  cat('removing unnecessary vectors ;)')
  if (!is.null(get0('n_exchg_rough'))) rm(n_exchg_rough)
  if (!is.null(get0('n_res_exchg_check'))) rm(n_res_exchg_check)
  #just for safety if website is down - have a local file
  saveRDS(n_exchg_elegant, 'data/exchange_rates.rds') 
  }
```

## getting the mean by year
```{r}


year_mean <- exchange_rupees_dolars_elegant %>% 
  mutate(year = lubridate::year(ref_date)) %>% 
  group_by(year) %>% 
    summarise(mean = mean(usd))
```

# 3. Sanitizing data

## exchange rupees to dollar mean exchg by year
```{r}
df <- ETL_data_salary %>% 
  mutate(Rate_USD = purrr::map(Working_Year, 
                               function(x) year_mean$mean[x == year_mean$year])) %>% 
  tidyr::unnest(cols = c(Rate_USD)) %>% 
  mutate(Salary_In_Rupees = readr::parse_number(Salary_In_Rupees,
                                                locale = readr::locale(decimal_mark = "."))) %>% 
  mutate(Salary_In_USD_Dollars = Salary_In_Rupees * Rate_USD) %>% 
  relocate(Salary_In_USD_Dollars, .before = Salary_In_Rupees)
```
 
# 4.Modeling Data 

t <- table(c(df$Designation, ))
```{r}

df_Designation_n_yr <- df %>% 
  group_by(Designation) %>% 
  summarise(qtd_yr = length(unique(Working_Year))) %>% 
  ungroup() %>% 
  left_join(df, by = 'Designation') 

df_Designation_ee <- df %>% 
  group_by(Designation, Working_Year) %>% 
  summarise(ee_yr = n(),
            mean_wage_USD = mean(Salary_In_USD_Dollars),
            mean_wage_INR = mean(Salary_In_Rupees),
            .groups = 'drop_last') %>% 
  ungroup() %>% 
  left_join(df, by = c('Designation', 'Working_Year'))


  # filter(qtd == 1L) %>% 
```





  
  distinct(qtd, .keep_all = TRUE)

  group_by(Designation) %>% 
  filter(qtd == max(qtd))

vars <- df %||% tidyselect::peek_vars(fn = "everything")

names <- names(df)

seq_along(names)

lapply(names, function(x) cat(paste(df$x)))

df[ , (cols) := lapply(.SD, "*", -1), .SDcols = cols]

df1 <- data.frame(x = 1)
df2 <- data.frame(x = 2)
rbind(df1, df2)

dfs <- list(df1, df2)

rlang::inject(cbind(!!!dfs))

dfs <- list(mtcars, mtcars)
inject(rbind(!!!mtcars))
dfs <- list(df1, df2)

prop.table(ETL_USD$Working_Year)


df$Working_Year <- factor(ETL_USD$Working_Year, 
                          levels = c('2020', '2021', '2022'))
df$Company_Size <- factor(ETL_USD$Company_Size, 
                          levels = c('S', 'M', 'L'))
df$Experience <- factor(ETL_USD$Experience,
                        levels = c('EN', 'MI', 'SE', 'EX'))
df$Employment_Status <- factor

my_factor <- function(df_input, to_factor, order_by){
  df_input$to_factor <<- factor(df_input$to_factor,
                                levels = order_by)
}



## converting to factor ----
```{r}
df <- df %>% 
mutate(across(.cols = c(Designation,
                        Employee_Location,
                        Company_Location),
              as.factor))

```




table(df$Employment_Status)

head(ETL_USD)

#summarizing 
```{r}


summary(df)
```

# 5. Analising

## plots
```{r}


ggplot(df) +
  geom_bar(aes(x = Designation)) +
  coord_flip()
```


