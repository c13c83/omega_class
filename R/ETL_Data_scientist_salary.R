library(tidyverse)


ETL_data1 <- read.csv('data/data_cleaned_2021.csv')

#extract_data -----

ETL_data_salary <- read.csv('data/Data_Science_Fields_Salary_Categorization.csv') 

ETL_exchg_rate_html <- read_file("https://www.exchangerates.org.uk/INR-USD-spot-exchange-rates-history-2020.html") 

exchg_rate_split <- ETL_exchg_rate_html %>% 
  str_split('\n')

cat(ETL_exchg_rate_html)

init_split_table <- grep("<table", exchg_rate_split[[1]])
end_split_table <- grep("</table>", exchg_rate_split[[1]])

exchg_rate_split_tr <- exchg_rate_split[[1]][init_split_table[2]:end_split_table[2]]

cat(exchg_rate_split_tbody)
init_split_tr <- grep('<tr', exchg_rate_split_tr)
end_split_tr <- grep('</tr', exchg_rate_split_tr)

#getsecond tr ake function to get the sae value in init and end

exchg_rate_split_month <- exchg_rate_split_tr[7] %>% str_split('</tr>')

exchg_rate_rows <- grep('<tr class=\\\"col', exchg_rate_split_month[[1]])

# make a loop to get the two values
#sample
ref_date <- exchg_rate_split_month[[1]][exchg_rate_rows[1]]


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


