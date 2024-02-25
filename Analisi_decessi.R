library(data.table)
library(tidyverse)
library(janitor)
library(downloader)

#Ricostruici la popolazione


## Dati sulla popolazione da Istat


pop_2011_18 = as.data.frame(fread("Input/regioni.csv"))%>% ##Elaborato da me, non c'è su Istat già pronto
  filter(anno != 2019 & genere != "totale" & anno >= 2011)%>%
  gather(anni, popolazione, "0":"100")%>%
  rename(codice_regione = codice)%>%
  mutate(anni = as.numeric(anni))

pop_2019 = as.data.frame(fread("Input/POSAS_2019_it_Regioni.csv"))%>%
  clean_names()%>%
  select(codice_regione, regione, et_a , totale_maschi, totale_femmine)%>%
  mutate(anno = 2019)


pop_2020 = as.data.frame(fread("Input/POSAS_2020_it_Regioni.csv"))%>%
  clean_names()%>%
  select(codice_regione, regione, et_a , totale_maschi, totale_femmine)%>%
  mutate(anno = 2020)


pop_2021 = as.data.frame(fread("Input/POSAS_2021_it_Regioni.csv"))%>%
  clean_names()%>%
  select(codice_regione, regione, et_a , totale_maschi, totale_femmine)%>%
  mutate(anno = 2021)


pop_2022 = as.data.frame(fread("Input/POSAS_2022_it_Regioni.csv"))%>%
  clean_names()%>%
  select(codice_regione, regione, et_a , totale_maschi, totale_femmine)%>%
  mutate(anno = 2022)


pop_2023 = as.data.frame(fread("Input/POSAS_2023_it_Regioni.csv"))%>%
  clean_names()%>%
  select(codice_regione, regione, et_a , totale_maschi, totale_femmine)%>%
  mutate(anno = 2023)


pop_2019_23 = bind_rows(pop_2019, pop_2020, pop_2021, pop_2022, pop_2023)%>%
  gather(genere, popolazione,  totale_maschi:totale_femmine)%>%
  rename(anni = et_a)%>%
  mutate(genere = ifelse(genere == "totale_femmine", "femmine", "maschi"))

popolazione = bind_rows(pop_2019_23, pop_2011_18)%>%
  filter(anni != 999 & genere != "Totale")%>%
  mutate(fascia_anagrafica = case_when(anni >=0 & anni <=0 ~ '0',
                                       anni >=1 & anni <=4 ~ '1-4',
                                       anni >=5 & anni <=9 ~ '5-9',
                                       anni >=10 & anni <=14 ~ '10-14',
                                       anni >=15 & anni <=19 ~ '15-19',
                                       anni >=20 & anni <=24 ~ '20-24',
                                       anni >=25 & anni <=29 ~ '25-29',
                                       anni >=30 & anni <=34 ~ '30-34',
                                       anni >=35 & anni <=39 ~ '35-39',
                                       anni >=40 & anni <=44 ~ '40-44',
                                       anni >=45 & anni <=49 ~ '45-49',
                                       anni >=50 & anni <=54 ~ '50-54',
                                       anni >=55 & anni <=59 ~ '55-59',
                                       anni >=60 & anni <=64 ~ '60-64',
                                       anni >=65 & anni <=69 ~ '65-69',
                                       anni >=70 & anni <=74 ~ '70-74',
                                       anni >=75 & anni <=79 ~ '75-79',
                                       anni >=80 & anni <=84 ~ '80-84',
                                       anni >=85 & anni <=89 ~ '85-89',
                                       anni >=90 & anni <=94 ~ '90-94',
                                       anni >=95 & anni <=99 ~ '95-99',
                                       anni >=100 ~ '100+'))%>%
  mutate(genere = toupper(genere))%>%
  group_by(codice_regione , anno, genere, fascia_anagrafica)%>%
  summarise(pop = sum(popolazione))


## Leggi file Istat decessi


download(link, dest="istat.zip", mode="wb") 
unzip ("istat.zip", exdir = "Input")

mortalita_0 = as.data.frame(fread("Input/comuni_giornaliero_31dicembre23.csv"))

mortalita = mortalita_0%>%
  group_by(REG, NOME_REGIONE, CL_ETA)%>%
  summarise(across(where(is.numeric), sum, na.rm=T))%>%
  gather(genere, decessi, M_11:F_23)%>%
  mutate(anno = as.numeric(paste0("20", substr(genere, 3,4))),
         genere = ifelse(substr(genere, 1, 1)=="F", "femmine", "maschi"))%>%
  select(REG, NOME_REGIONE, CL_ETA, anno, genere, decessi)%>%
  mutate(fascia_anagrafica = case_when(CL_ETA==0 ~ '0',
                                       CL_ETA==1 ~ '1-4',
                                       CL_ETA==2 ~ '5-9',
                                       CL_ETA==3 ~ '10-14',
                                       CL_ETA==4 ~ '15-19',
                                       CL_ETA==5 ~ '20-24',
                                       CL_ETA==6 ~ '25-29',
                                       CL_ETA==7 ~ '30-34',
                                       CL_ETA==8 ~ '35-39',
                                       CL_ETA==9 ~ '40-44',
                                       CL_ETA==10 ~ '45-49',
                                       CL_ETA==11 ~ '50-54',
                                       CL_ETA==12 ~ '55-59',
                                       CL_ETA==13 ~ '60-64',
                                       CL_ETA==14 ~ '65-69',
                                       CL_ETA==15 ~ '70-74',
                                       CL_ETA==16 ~ '75-79',
                                       CL_ETA==17 ~ '80-84',
                                       CL_ETA==18 ~ '85-89',
                                       CL_ETA==19 ~ '90-94',
                                       CL_ETA==20 ~ '95-99',
                                       CL_ETA==21 ~ '100+'))%>%
  rename(codice_regione = REG)%>%
  mutate(genere = toupper(genere))


## Unidsci mortalità e struttura demografica

data_00 = inner_join(mortalita, popolazione, by=c("codice_regione", "fascia_anagrafica", "genere", "anno"))

## Dati per standardizzazione

data_0 = inner_join(data_00, popolazione%>%
                      filter(anno == 2023)%>%
                      rename(pop_std = pop)%>%
                      rename(anno_std = anno),
                      by=c("codice_regione", "fascia_anagrafica", "genere"))



## Decessi totali

data = data_0%>%
  group_by(anno)%>%
  summarise(decessi = sum(decessi),
            pop = sum(pop))


write.csv(data, file="Output/decessi_nazionali.csv")


## Decessi standardizzati per l'Italia

data = data_0%>%
  group_by(anno, genere, fascia_anagrafica)%>%
    summarise(decessi = sum(decessi),
            pop = sum(pop),
            pop_std = sum(pop_std))%>%
  group_by(anno)%>%
  mutate(pop_std_totale = sum(pop_std))%>%
  ungroup()%>%
  mutate(pop_perc = pop_std / pop_std_totale ,
         tassi = decessi / pop,
         tasso_std = pop_perc * tassi)%>%
  group_by(anno)%>%
  summarise(tasso_std = sum(tasso_std))%>%
  mutate(tasso_standard = tasso_std * 100000)
  

write.csv(data, file="Output/decessi_nazionali_standard.csv")


## Tassi di mortalità per fascia anagrafica

data = data_0%>%
  mutate(fascia_anagrafica = case_when(CL_ETA==13 ~ '60-69',
                                       CL_ETA==14 ~ '60-69',
                                       CL_ETA==15 ~ '70-79',
                                       CL_ETA==16 ~ '70-79',
                                       CL_ETA==17 ~ '80-89',
                                       CL_ETA==18 ~ '80-89',
                                       CL_ETA==19 ~ '90+',
                                       CL_ETA==20 ~ '90+',
                                       CL_ETA==21 ~ '90+',
                                       TRUE ~ "<60"))%>%
  filter(anno >= 2015)%>%
  mutate(anno_raggr = case_when(anno >= 2015 & anno <= 2019 ~ '2015-19',
                                TRUE ~ as.character(anno)))%>%
  group_by(anno_raggr, fascia_anagrafica)%>%
  summarise(decessi = sum(decessi),
            pop = sum(pop))%>%
  mutate(tasso_mort = decessi / pop *100)


write.csv(data, file="Output/tassi_mortalita.csv")


## Decessi standardizzati per regioni

data = data_0%>%
  group_by(NOME_REGIONE, anno, fascia_anagrafica)%>%
  summarise(decessi = sum(decessi),
            pop = sum(pop))%>%
  mutate(tasso = decessi / pop)%>%
  group_by(NOME_REGIONE, anno)%>%
  mutate(pop_perc = pop / sum(pop) ,
         tassi = decessi / pop,
         tasso = pop_perc * tassi)%>%
  summarise(tasso = sum(tasso))%>%
  mutate(tasso_standard = tasso * 100000)%>%
  mutate(anno_raggr = case_when(anno >= 2015 & anno <= 2019 ~ '2015-19',
                                TRUE ~ as.character(anno)))%>%
  filter(anno >= 2015)%>%
  group_by(NOME_REGIONE, anno_raggr)%>%
  summarise(tasso_standard = mean(tasso_standard))


write.csv(data, file="Output/decessi_regioni_standard.csv")


## Decessi standardizzati per genere

data = data_0%>%
  group_by(anno, genere, fascia_anagrafica)%>%
  summarise(decessi = sum(decessi),
            pop = sum(pop),
            pop_std = sum(pop_std))%>%
  group_by(anno, genere)%>%
  mutate(pop_std_totale = sum(pop_std))%>%
  ungroup()%>%
  mutate(pop_perc = pop_std / pop_std_totale ,
         tassi = decessi / pop,
         tasso_std = pop_perc * tassi)%>%
  group_by(anno, genere)%>%
  summarise(tasso_std = sum(tasso_std))%>%
  mutate(tasso_standard = tasso_std * 100000)%>%
  mutate(anno_raggr = case_when(anno >= 2015 & anno <= 2019 ~ '2015-19',
                                TRUE ~ as.character(anno)))%>%
  filter(anno >= 2015)%>%
  group_by(genere, anno_raggr)%>%
  summarise(tasso_standard = mean(tasso_standard))


data%>%
  filter(anno_raggr %in% c('2022', '2023'))%>%
  spread( anno_raggr, tasso_standard)%>%
  clean_names()%>%
  mutate(var = (x2023 - x2022 ) / x2022 * 100)%>%
  arrange(var)


write.csv(data, file="Output/decessi_genere_standard.csv")


