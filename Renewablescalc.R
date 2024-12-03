#load packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#load data
files <- list.files(pattern='^202.*?\\.csv')
df_list <- lapply(files,read_csv)
generation <- bind_rows(df_list)

#move TPs from columns to rows
generation_trans = gather(generation,"TP1","TP2","TP3","TP4","TP5","TP6","TP7","TP8","TP9","TP10","TP11","TP12","TP13","TP14","TP15","TP16","TP17","TP18","TP19","TP20","TP21","TP22","TP23","TP24","TP25","TP26","TP27","TP28","TP29","TP30","TP31","TP32","TP33","TP34","TP35","TP36","TP37","TP38","TP39","TP40","TP41","TP42","TP43","TP44","TP45","TP46","TP47","TP48","TP49","TP50",key='Trading_Period',value='gen')

unique(generation_trans$Tech_Code)

gen_date <- generation_trans %>%
  rename(TP = 'Trading_Period') %>%
  group_by(Trading_Date,TP,Tech_Code,gen) %>%
  summarise() %>%
  aggregate(gen ~ Trading_Date + TP + Tech_Code, sum) %>%
  filter(Tech_Code == "Thrml" & gen == 0) %>%
  mutate(time = 0.5) %>%
  aggregate(time ~ Trading_Date, sum)





