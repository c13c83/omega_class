

# ----
# Author: César Carvalho
# WebScraping: CEASA - RS (price of natural goods)
Packages <- names(sessionInfo()[["otherPkgs"]])
# ----
  
#loading packages ----
pckgs <- c('dplyr', 
           'stringr', 
           'ggplot2',
           'lubridate')
invisible(lapply(pckgs, library, character.only = TRUE))

#helpers -----

dmy_ <- function(x) format(as.Date(x), "%d-%m-%Y")

url <- function(x) {
  if (x < as.Date('2020-01-08')) return(paste0('https://ceasa.rs.gov.br/tabcotacao/cotacao-', dmy_(x), '/'))
  if (x == as.Date('2020-01-08')) return('https://ceasa.rs.gov.br/tabcotacao/2339/')
  if (x > as.Date('2020-01-08')) return(paste0('https://ceasa.rs.gov.br/tabcotacao/', dmy_(x), '/'))
}

# settings ----

start <- as.Date('2019-11-12')
diff_days <- as.numeric(today() - start)
days <- start - 1 + 1:diff_days %>% sort(decreasing = FALSE)

day <- days[59]
url(day)

curlGetHeaders(url(day)) %>%
    attr(which = 'status')

read_ceasa <- function(day) {
  status_url <- curlGetHeaders(url(day)) %>%
    attr(which = 'status')
  
  if (status_url == 404L) return()
  
  html <- xml2::read_html(url(day))
  
  tbl <- html %>%
    xml_find_all(xpath = '//*//table[starts-with(@id, "tablepress-")]')

  
  rows <- tbl %>% xml_find_all(xpath = '//tr'  )
  
  n_col <- tbl %>% 
    xml_find_all(xpath = '//tr//td') %>% 
    xml_attrs() %>% 
    unique() %>% 
    length()
  
  col_names <- c('product',
                 'unit',
                 'max',
                 'moda',
                 'min')
  
  col_6names <- c('product',
                  'unit',
                  'type_unit',
                  'max',
                  'moda',
                  'min')
  
  # getting the values from tr looking the td, after 28-11-2019 (d-m-Y) we have
  # 5 cols, the columns weight was removed. In fact it really was unnecessary
  map_dfc(1:n_col, function(x) {
    as_tibble(rows %>% 
             xml_find_all(xpath = paste0('//td[', x, ']')) %>%
             xml_text(),
             quiet = TRUE
           # .names_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
           )}
    ) %>% 
    set_names(nm = if (n_col == 5L){col_names} else {col_6names}) %>%
    select(-any_of('type_unit')) %>% 
    mutate(ref_date = day)
}


df <- map_df(days, read_ceasa)

df <- tibble('product' = NULL,
             'type_unit' = NULL,
             'qty_unit' = NULL,
             'max' = NULL,
             'moda' = NULL,
             'min' = NULL,
             )

for (i in 50:length(days)) {
  
  df <- df %>% bind_rows(read_ceasa(days[i]))
  
  cat(paste('getting day:', days[i], 'from loop ->', i, '\n'))
  
}

saveRDS(df, 'data/preview_ceasa.rds')
