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
  group_by(Trading_Date,Trading_Period,Fuel_Code) %>%
  summarise()



#assign intensity factors to each instance of generation and calculate emissions from each instance
gen_final = mutate(generation_trans,
                   factor=
                     ifelse(POC_Code=='HLY2201',
                            ifelse(Gen_Code=='huntly_1_4',
                                   ifelse(Fuel_Code=='Coal',1.01,.589),
                                   ifelse(Gen_Code=='huntly_e3p',.4,.568)),
                            ifelse((POC_Code=='HWA1101' & Fuel_Code=='Gas') | POC_Code=='HWA1102',.683,
                                   ifelse(POC_Code=='JRD1101',.548,
                                          ifelse(POC_Code=='KAW0111' | POC_Code=='KAW0112',.06,
                                                 ifelse(POC_Code=='KAW1101',.123,
                                                        ifelse(POC_Code=='KIN0112',.364,
                                                               ifelse((POC_Code=='KOE1101' & Trading_Date<='2023-09-30'),.307,
                                                                      ifelse(POC_Code=='KPA1101',.683,
                                                                             ifelse(POC_Code=='MKE1101',.569,
                                                                                    ifelse(POC_Code=='NAP2201',.063,
                                                                                           ifelse(POC_Code=='NAP2202',.064,
                                                                                                  ifelse(POC_Code=='OKI2201',.266,
                                                                                                         ifelse(POC_Code=='PPI2201',.038,
                                                                                                                ifelse(POC_Code=='SFD2201',.513,
                                                                                                                       ifelse(POC_Code=='THI2201',.04,
                                                                                                                              ifelse(POC_Code=='TWH0331' & Fuel_Code=='Gas',.632,
                                                                                                                                     ifelse(POC_Code=='WHI2201',.76,
                                                                                                                                            ifelse(POC_Code=='WKM2201' & Fuel_Code=='Geo',.052,
                                                                                                                                                   ifelse(POC_Code=='WRK0331',
                                                                                                                                                          ifelse(Gen_Code=='rotokawa',.084,.053),
                                                                                                                                                          ifelse(POC_Code=='WRK2201',.018,
                                                                                                                                                                 ifelse(POC_Code=='TAB2201',.08,0))))))))))))))))))))),
                   real_factor = factor * gen)
#tidy up df
gen_final = select(gen_final,Trading_Date,'Trading Period',gen,real_factor)


#condense generation instances into single entries for each TP on each date
gen_final = gen_final %>% 
  rename(TP = 'Trading Period') %>%
  aggregate(cbind(gen, real_factor) ~ Trading_Date + TP, sum) %>%
  mutate(intensity = real_factor/gen) %>%
  mutate(TP_sort = as.integer(sub('..',"",TP))) %>%
  mutate(time = ((TP_sort*30)-30)/60)


