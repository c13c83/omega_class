

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

url <- function(x) paste0('https://ceasa.rs.gov.br/tabcotacao/cotacao-', dmy_(x), '/')


# settings ----
# curretn_date <- format(Sys.Date(), "%d-%m-%Y")
start <- as.Date('2019-11-12')
end <- today()        
diff_days <- as.numeric(end-start)
                  
days <- start + 1:diff_days

z = structure(list(a = 1), foo = 2)
base::attr(diff_days, 'numeric') # 2

dates %>% sort(decreasing = FALSE)

date1 <- ymd_hms("2009-03-08 01:59:59")
date2 <- ymd_hms("2000-02-29 12:00:00")
interval(date2, date1)
interval(date1, date2)
span <- interval(ymd(20090101), ymd(20090201))

lubridate::int_end()

html <- xml2::read_html(url(start))

html %>% xml2::xml_structure()

tbl <- html %>%
  xml_find_all(xpath = '//*//table[@id="tablepress-8"]')



url(start) 
