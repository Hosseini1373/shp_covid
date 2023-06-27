library(tidyverse)
library(haven)
library(GGally)

# function to read data:
sav_names <- list.files(pattern = "*.sav") # Get all file names
sav_names 

read_data <- function(x){
  return (read_sav(x)) 
}

#Daten einlesen Lesen Sie alle benötigten Dateien der Rohdaten in R ein. Wir empfehlen, mit den SPSS

#read_data()

covid <- read_sav("raw_data/SHP_Covid_SPSS_SAS_STATA/SHP_Covid_USER.sav")
p2020 <- read_sav("raw_data/SHP20_P_USER.sav")
p2019 <- read_sav("raw_data/SHP19_P_USER.sav")
p2018 <- read_sav("raw_data/SHP18_P_USER.sav")
p2017 <- read_sav("raw_data/SHP17_P_USER.sav")
p2016 <- read_sav("raw_data/SHP16_P_USER.sav")
p2015 <- read_sav("raw_data/SHP15_P_USER.sav")
h2019 <- read_sav("raw_data/SHP19_H_USER.sav")
so <- read_sav("raw_data/SHP_SO.sav")


#Select Reduzieren Sie die eingelesenen Daten auf die benötigten Variablen. Dazu gehören neben den erforderlichen Variablen auch ID-Variablen, die für das spätere Mergen der Tabellen notwendig sind.----


# labels sapply(covid, attr, "label")

covid |>
  select(IDPERS, C20PC44, C20SEX, C20D29, C20W01, C20W02a, C20W02b, C20W03a, C20W03b, C20W03c, C20C01) ->covid

p2020 |>
  select(IDPERS, IDHOUS20, P20C44, AGE20, P20D29, I20UNEY, EDUCAT20, P20L01, P20F54, P20P04) -> p2020

p2019 |>
  select(IDPERS, IDHOUS19, P19C44, AGE19, P19D29, I19UNEY, EDUCAT19, P19L01, P19F54) -> p2019

h2019 |>
  select(IDHOUS19, NBPERS19, CANTON19, COM2_19, HLDCEN19, I19HTYN) -> h2019

p2018 |>
  select(IDPERS, I18UNEY) -> p2018

p2017 |>
  select(IDPERS, I17UNEY, P17P04) -> p2017

p2016 |>
  select(IDPERS, I16UNEY) -> p2016

p2015 |>
  select(IDPERS, I15UNEY) -> p2015

names(so)[74] <- 'PO58' # $$ To-DO Problem kurzfristig gelöst
so |>
  select(IDPERS, PO58)

# Merge Covid with p2019 left-JOIN----

merge<-merge(x = covid, y = p2019, by.x = "IDPERS",by.y="IDPERS", all.x = TRUE) # maybe rename due to FUnction?

# Merge "merge" with h2019 left-Join so we just have the households that participate at the covid questionaire:

merge <- merge(x = merge, y = h2019, by.x = "IDHOUS19", by.y = "IDHOUS19", all.x = TRUE)

# Merge "merge" with SO left Join

merge <- merge(x = merge, y = so, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# merge for Var 14 Unemployed

merge <- merge(x = merge, y = p2018, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)
merge <- merge(x = merge, y = p2017, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)
merge <- merge(x = merge, y = p2016, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)
merge <- merge(x = merge, y = p2015, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)
merge <- merge(x = merge, y = p2020, by.x = "IDPERS", by.y = "IDPERS", all.x = TRUE)

# Filtering Berücksichtigen Sie ausschliesslich Befragte, die im Jahr 2019 und 2020 erwachsen waren und bei der regulären SHP Befragung im Jahr 2019 als auch bei der COVID-19 Befragung im Jahr 2020 teilgenommen haben. Dokumentieren Sie, wieviele Befragte durch diese Einschränkung verloren gehen, und aus welchem Grund.----

merge |>
  filter(AGE19 >= 18) -> merge

# Beim filtern der Daten sind genau 2212 Leute aus der Tabelle entfernt worden, weil sie zur Zeit der Befragung noch nicht erwachsen waren oder die Daten NA's oder nicht mögliche Werte enthalten wie Beispielsweise 0. Nach SHP_User_Guide_w22 Seite 6 sind alle Befragten über 14! Somit sind unsere Daten inkonsistent.


# Variable 1 Change in Satisfaction
merge|>
  mutate(C20PC44=as.integer(C20PC44)
         ,P19C44=as.integer(P19C44)
         ,Change_satisfaction=C20PC44-P19C44
         ,Change_satisfaction=case_when(
           Change_satisfaction<0~"worse"
           ,Change_satisfaction==0~"same"
           ,Change_satisfaction>0~"better"))->merge


# Variable 2 Sex

merge|>
  mutate(Sex = C20SEX
         ,Sex = case_when(
           Sex == 1 ~ "Man"
           ,Sex == 2 ~ "Woman")) -> merge

# Variable 3 Age

merge|>
  mutate(Age = AGE19)  -> merge

# Variable 4 Canton

merge |>
  mutate(Canton = CANTON19,
         Canton = case_when(
           Canton == 1 ~ "AG"
           ,Canton == 2 ~ "AI"
           ,Canton == 3 ~ "AR"
           ,Canton == 4 ~ "BE"
           ,Canton == 5 ~ "BS"
           ,Canton == 6 ~ "BL"
           ,Canton == 7 ~ "FR"
           ,Canton == 8 ~ "GE"
           ,Canton == 9 ~ "GL"
           ,Canton == 10 ~ "GR"
           ,Canton == 11 ~ "JU"
           ,Canton == 12 ~ "LU"
           ,Canton == 13 ~ "NE"
           ,Canton == 14 ~ "NW"
           ,Canton == 15 ~ "OW"
           ,Canton == 16 ~ "SG"
           ,Canton == 17 ~ "SH"
           ,Canton == 18 ~ "SO"
           ,Canton == 19 ~ "SZ"
           ,Canton == 20 ~ "TG"
           ,Canton == 21 ~ "TI"
           ,Canton == 22 ~ "UR"
           ,Canton == 23 ~ "VD"
           ,Canton == 24 ~ "VS"
           ,Canton == 25 ~ "ZG"
           ,Canton == 26 ~ "ZH"
         )) -> merge

# Variable 5 Commun type

merge |>
  mutate(Commun_type = COM2_19,
         Commun_type = case_when(
           Commun_type == 1 ~ "Centre"
           ,Commun_type == 2 ~ "Suburban"
           ,Commun_type == 3 ~ "Wealthy"
           ,Commun_type == 4 ~ "Peripheral urban"
           ,Commun_type == 5 ~ "Tourist"
           ,Commun_type == 6 ~ "Industrial and tertiary sector"
           ,Commun_type == 7 ~ "Rural commuter"
           ,Commun_type == 8 ~ "Mixed agricultural"
           ,Commun_type == 9 ~ "Peripheral agricultural"
         )) -> merge

# Variable 6 highest Education level

merge |>
  mutate(Edu = EDUCAT19,
         Edu = case_when(
           Edu == 0 ~ "basic education"
           ,Edu == 1 ~ "basic education"
           ,Edu == 2 ~ "basic education"
           ,Edu == 3 ~ "basic education"
           ,Edu == 4 ~ "apprenticeship"
           ,Edu == 5 ~ "basic education"
           ,Edu == 6 ~ "higher education"
           ,Edu == 7 ~ "higher education"
           ,Edu == 8 ~ "basic education"
           ,Edu == 9 ~ "basic education"
           ,Edu == 10 ~ "higher education"
         )) -> merge

# Variable 7 Relation Status

merge |>
  mutate(Relation_status = C20D29
         ,Relation_status = case_when(
           Relation_status == 1 ~ "yes, living together"
           ,Relation_status == 2 ~ "yes, but not living together"
           ,Relation_status == 3 ~ "no"
         )) -> merge

# Variable 8 Relation Change
merge |>
  mutate(Change_in_relation = case_when(
    P19D29 == C20D29 ~ "No change"
    ,(P19D29 == 1 | P19D29 == 2) & C20D29 == 3 ~ "Seperation"
    ,P19D29 == 3 & (C20D29 == 1 | C20D29 == 2) ~ "New in Relation"
  )) -> merge

# Variable 9 Change in work situation
#Kurzfristige Änderung der Arbeitssituation nach Pandemiebeginn (ja = wurde Arbeitslos, Kurzarbeit, arbeitet weniger z.B. wegen Kinderbetreuung, eigenes  Geschäft ist direkt von Pandemie betroffen, sonst nein (Annahme das keine Angabe 'NA' auch = -> Nein ist!)

#merge|>
#  mutate(Workingsituation = case_when(
#    I19UNEY <= 0 & C20W01 == 4 ~ "Ja",
#    C20W01 == 2 & (C20W03a == 1 | C20W03b == 1 | C20W03c == 1)~ "Ja",
#    (C20W01 == 1 | C20W01 == 3) & (C20W02a == 1 |C20W02b == 1)~ "Ja")) -> merge

# Try to have both answers!!!
merge|>
  mutate(Workingsituation = ifelse(
    (I19UNEY <= 0 & C20W01 == 4) |
      (C20W01 == 2 & (C20W03a == 1 | C20W03b == 1 | C20W03c == 1)) |
      ((C20W01 == 1 | C20W01 == 3) & (C20W02a == 1 |C20W02b == 1)), 'Yes','No')) -> merge


# Variable 10 Covid Infection levels(Yes)

merge |>
  mutate(Corona_infection = case_when(
    C20C01 == 2 | C20C01 == 3 | C20C01 == 4 |C20C01 == 5 | C20C01 == 6 ~ "Yes"
    ,C20C01 == 1 ~ "No")) -> merge

# Variable 11 Houshold type

merge |> 
  mutate(Household_type = case_when(
    HLDCEN19 == 1000 ~ "One-person households"
    ,HLDCEN19 == 2111 ~ "Married couple without children"
    ,HLDCEN19 == 2112 ~ "Consensual couple without children"
    ,HLDCEN19 == 2121 ~ "Married couple without children and another person"
    ,HLDCEN19 == 2122 ~ "Consensual couple without children and another person"
    ,HLDCEN19 == 2211 ~ "Married couple with children"
    ,HLDCEN19 == 2212 ~ "Consensual couple with children"
    ,HLDCEN19 == 2221 ~ "Married couple with children and another person"
    ,HLDCEN19 == 2222 ~ "Consensual couple with children and another person"
    ,HLDCEN19 == 2310 ~ "One parent with children"
    ,HLDCEN19 == 2320 ~ "One parent with children and another person"
    ,HLDCEN19 == 3110 ~ "Other types of households with only related family"
    ,HLDCEN19 == 3120 ~ "Other types of households with and without related family"
    ,HLDCEN19 == 3200 ~ "Other types of households without related family")) -> merge

# Variable 12 Netto-Income per person NBPERS19

merge |>
  mutate(Income = (I19HTYN/NBPERS19)) -> merge

# Variable 13 Financial Problems

merge |>
  mutate(Financial_problem = PO58
         ,Financial_problem = case_when(
           PO58 == 1 ~ "Yes"
           ,PO58 == 2 ~ "No")) -> merge

# Variable 14 unemployed Annahme, dass NA's Nein ist!

merge |>
  mutate(Unemployed = case_when(
    I19UNEY > 0 | I18UNEY > 0 | I17UNEY > 0 | I16UNEY > 0 | I15UNEY > 0 ~ "Yes"
    ,TRUE ~ 'No')) ->merge


# Variable 15 Accident or Illness between 2019 and 2020, ADOPTION of the NA's are turned into the status 'unknown'!

merge |>
  mutate(Illness_accident = case_when(
    P20L01 == P19L01 ~"No change",
    P20L01 == 1 & P19L01 == 2 ~"Accident happend",
    P20L01 == 2 & P19L01 == 1 ~"Accident resolved",
    TRUE ~ 'unknown'
  )) -> merge

# Variable 16 Happy with Partner, all NA's have been transformed to 'unknown'!

merge|>
  mutate(P20F54=as.integer(P20F54)
         ,P19F54=as.integer(P19F54)
         ,Happy_with_partner=P20F54-P19F54
         ,Happy_with_partner=case_when(
           Happy_with_partner<0~"unhappier"
           ,Happy_with_partner==0~"same"
           ,Happy_with_partner>0~"happier"
           ,TRUE ~'unknown'
         ))->merge



# Variable 17 Confidence in Federal-Government, all NA's have been transformed into 'unknown'?

merge|>
  mutate(P17P04=as.integer(P17P04)
         ,P20P04=as.integer(P20P04)
         ,Trust_government=P20P04-P17P04
         ,Trust_government=case_when(
           Trust_government<0~"worse"
           ,Trust_government==0~"same"
           ,Trust_government>0~"better"
           ,TRUE ~ 'unknown'
         ))->merge


# Filter new table

merge |>
  select(Change_satisfaction, Sex, Age, Canton, Commun_type, Edu, Relation_status, Change_in_relation,Workingsituation,Corona_infection, Household_type, Income, Financial_problem, Unemployed, Illness_accident, Happy_with_partner, Trust_government) -> tibble

# Save cleaned data tibble
#save(tibble, file= "Cleaned_data.RData")

63+72+170+151+99+3+383+309+368+1585+819
# NA - Check, First Check : 
# Change_satisfaction: 63
# Relation_status : 72
# Change_in_Relation: 170
# Workingsituation: 151
# Corona Infection: 99
# Household_type: 3
# Income: 383
# Financel_problem: 309
# Illness_accident: 368
# Happy_with_partner: 1585
# Trust_government: 819
# Total:4022
# Annahmen treffen um NA's in den drei selbst definierten Variablen zu reduzieren!
# Es wurden alle NA's zum Zustand 'unknown' geändert, dass keine unnötigen Verluste der Beobachtungen in Kauf genommen werden müssen.
colSums(is.na(tibble))
# Check for NA's
# NA - Check, Second Check : 
# Change_satisfaction: 63
# Relation_status : 72
# Change_in_Relation: 170
# Workingsituation: 151
# Corona Infection: 99
# Household_type: 3
# Income: 383
# Financel_problem: 309
# Total : 1250

tibble_na<-tibble[complete.cases(tibble),]
#Save tibble_na
#save(tibble_na, file= "Cleaned_data_na.RData")

column_clear<-function(tib){
  drop.cols <- c(1,2,4,5,6,7)
  tib|>select(-(drop.cols))->tib
  return(tib)
}

?load
library(readxl)
bfs<-suppressMessages(read_xlsx("BFS/px-x-0102010000_101_20220415-140732.xlsx",skip=2,n_max=53))|>
  column_clear()|>#Remove some columns
  fill(1,.direction = c("down"))|>#for each kanton add that it to the next line
  rename(Canton=...3,Sex=...8)|>
  rename_with(.cols=-c(1,2),.fn = \(x){str_extract(x, "[:digit:]*")})|>#remove Jahre from the column names
  pivot_longer(cols = -c(1,2), names_to ="Age", values_to = "People")|>mutate(Sex=case_when(Sex=="Mann"~"Man",
                                                                                            Sex=="Frau"~"Woman"))->tmp
#just one thing: sum(tibble$Age>99) equal 0 and in the BFS table we have 100 years and above summarized in one column

total<-nrow(tibble)
tibble|>group_by(Age)|>summarise(Proportion=n())|>
  mutate(Age=as.numeric(Age),Proportion=Proportion/total)->Data1  

total<-nrow(tibble_na)
tibble_na|>group_by(Age)|>summarise(Proportion=n())|>
  mutate(Age=as.numeric(Age),Proportion=Proportion/total)->Data2 

total<-sum(tmp$People)#all the people in BFS
tmp|>group_by(Age)|>summarise(Proportion=sum(People))|>
  mutate(Age=as.numeric(Age),Proportion=Proportion/total)->Data3

colors <- c("red","green","blue")
ggplot()+
  geom_line(data=Data3, aes(x=Age, y=Proportion,  color='BFSPOP',group=1)) +
  geom_line(data=Data1, aes(x=Age, y=Proportion, color='SHPPOP.ALL',  group=1))+
  geom_line(data=Data2, aes(x=Age, y=Proportion, color='SHPPOP.COMPLETE', group=1))+
  ylim(0, 0.0405)+
  scale_y_continuous(labels = scales::percent)+
  labs(color = "Source") +
  scale_color_manual(values = colors)+
  theme_bw()

ggsave(path ="output", filename= "Age.jpeg" ,width = 12,
       height = 7)

total<-nrow(tibble)
tibble|>group_by(Sex)|>summarise(Proportion=n())|>
  mutate(Sex=as.factor(Sex),Proportion=Proportion/total)->Data1  

total<-nrow(tibble_na)
tibble_na|>group_by(Sex)|>summarise(Proportion=n())|>
  mutate(Sex=as.factor(Sex),Proportion=Proportion/total)->Data2 


total<-sum(tmp$People)#all the people in BFS
tmp|>group_by(Sex)|>summarise(Proportion=sum(People))|>
  mutate(Sex=as.factor(Sex),Proportion=Proportion/total)->Data3

#Comparison of Sexes
colors <- c("red","green","blue")
ggplot()+
  geom_line(data=Data3, aes(x=Sex, y=Proportion,  color='BFSPOP',group=1)) +
  geom_line(data=Data1, aes(x=Sex, y=Proportion, color='SHPPOP.ALL',  group=1))+
  geom_line(data=Data2, aes(x=Sex, y=Proportion, color='SHPPOP.COMPLETE', group=1))+
  ylim(0, 0.6)+
  scale_y_continuous(labels = scales::percent)+
  labs(color = "Source") +
  scale_color_manual(values = colors)+
  theme_bw()

ggsave(path ="output", filename = "Sex.jpeg",width = 12,
       height = 7)

# Check for Plausibility, Comparison of Cantons
total<-nrow(tibble)
tibble|>
  group_by(Canton)|>
  summarise(Proportion=n())|>
  mutate(Canton=as.factor(Canton),Proportion=Proportion/total)->Data1  

total<-nrow(tibble_na)
tibble_na|>
  group_by(Canton)|>
  summarise(Proportion=n())|>
  mutate(Canton=as.factor(Canton),Proportion=Proportion/total)->Data2 


total<-sum(tmp$People)#all the people in BFS
tmp|>
  group_by(Canton)|>
  summarise(Proportion=sum(People))|>
  mutate(Canton=as.factor(Canton),Proportion=Proportion/total)->Data3

# Canton Comparison
colors <- c("red","green","blue")
ggplot() +
  geom_line(data = Data1, aes(x = Canton, y = Proportion, color = 'SHPPOP.ALL' ,group = 1))+
  geom_line(data=Data2, aes(x=Canton, y = Proportion,color = 'SHPPOP.COMPLETE', group = 1 )) + 
  geom_line(data=Data3, aes(x=Canton, y = Proportion,color = "BFSPOP", group = 1)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(color = "Source")
  

ggsave(path ="output", filename = "Canton.jpeg",width = 12,
       height = 7)




















































##EXPD Bivariate------------------------------------------
#type can be "stack" or "fill"
coll <- c("blue","green","red")
bivariate_kategorical<-function(tibble,kat_var,xlab,ylab,typ, caption){
  
  tibble|>
    count(.data[[kat_var]], Change_satisfaction) %>%
    group_by(.data[[kat_var]]) %>%
    mutate(pct= prop.table(n) * 100) %>%
    ggplot() + aes(.data[[kat_var]], pct,fill=Change_satisfaction) +
    geom_bar(stat="identity") +
    ylab("Number of Participants") +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
              position=position_stack(vjust=0.5)) +
    scale_fill_manual(values = c("better"="lightblue","same"="chartreuse2","worse"="coral2"))+
    ggtitle("England & Australia Team Make Up")+
    theme(axis.text.x = element_text(angle = 90),plot.caption = element_text(hjust = 0.5))+
    coord_flip()+
    labs(caption = caption)
}



bivariate_numerisch<-function(tibble,num_var,xlab,ylab,caption){
  tibble|>
    ggplot() + aes(y=.data[[num_var]], x=Change_satisfaction)+
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=TRUE)+
    labs(caption = caption)+
    scale_y_continuous()
  
}



bivariate_categorical <- function(tibble, cat_var,xlab,ylab,typ, caption){
  tibble |>
    ggplot(aes(x = .data[[cat_var]],fill=Change_satisfaction)) +
    geom_bar(position="stack")+
    theme(axis.text.x = element_text(angle=90), )+
    labs(caption = caption)+
    scale_fill_manual(values = c("better"="lightblue","same"="chartreuse2","worse"="coral2"))+
    theme(plot.caption = element_text(hjust = 0.9))
}



bivariate_kategorical(tibble_na,"Canton","","Häufigkeit","fill","Im Kanton Glarus ist die stärkste positive Veränderung sichtbar, im Kanton Schwyz trat genau das Gegenteil ein")
bivariate_categorical(tibble_na, "Canton","","Häufigkeit","fill","Im Kanton Glarus ist die stärkste positive Veränderung sichtbar, im Kanton Schwyz trat genau das Gegenteil ein")
# Im Kanton Glarus ist die stärkste positive Veränderung sichtbar, im Kanton Schwyz trat genau das Gegenteil ein



bivariate_kategorical(tibble_na,"Commun_type","","Häufigkeit","fill","Der Geimeindetyp 'Industrie' hat die grösste negative Veränderung im Bereich der Lebenszufriedenheit")
bivariate_categorical(tibble_na,"Commun_type","","Häufigkeit","fill","Der Geimeindetyp 'Industrie' hat die grösste negative Veränderung im Bereich der Lebenszufriedenheit")



# Der Geimeindetyp 'Industrie' hat die grösste negative Veränderung im Bereich der Lebenszufriedenheit



bivariate_kategorical(tibble_na,"Edu","","Häufigkeit","fill","Leute mit tiefen Bildungsstand haben, wurden in der Krise unzufriedener")
bivariate_categorical(tibble_na,"Edu","","Häufigkeit","fill","Leute mit tiefen Bildungsstand haben, wurden in der Krise unzufriedener")



# Leute mit tiefen Bildungsstand haben, wurden in der Krise unzufriedener



bivariate_kategorical(tibble_na,"Relation_status","","Häufigkeit","fill","Der Beziehungsstatus hat keinen markanten Einfluss auf die Zufriedenheit")
bivariate_categorical(tibble_na,"Relation_status","","Häufigkeit","fill","Der Beziehungsstatus hat keinen markanten Einfluss auf die Zufriedenheit")
#Der Beziehungsstatus hat keinen markanten Einfluss auf die Zufriedenheit



bivariate_kategorical(tibble_na,"Change_in_relation","","Häufigkeit","fill","Die Grafik zeigt, dass es den Menschen mit einer neuen Beziehung besser geht als den Menschen,\n die sich von ihrem Partner getrennt haben.")
bivariate_categorical(tibble_na,"Change_in_relation","","Häufigkeit","fill","Die Verteilung ist gleichmäßig über alle Variablen verteilt,\n die Mehrheit der Datenpunkte fällt in die Kategorie 'No change'")

bivariate_kategorical(tibble_na,"Workingsituation","","Häufigkeit","fill","Die Änderung der Arbeitsituation hat keinen prozentual gesehen Einfluss")
bivariate_categorical(tibble_na,"Workingsituation","","Häufigkeit","fill","Die Änderung der Arbeitsituation hat keinen prozentual gesehen Einfluss")
# Die Änderung der Arbeitsituation hat keinen prozentual gesehen Einfluss



bivariate_kategorical(tibble_na,"Corona_infection","In Beziehung?","","Häufigkeit in Prozent","Menschen mit einer Corona-Infektion büssten in Lebenszufriedenheit ein")
bivariate_categorical(tibble_na,"Corona_infection","In Beziehung?","","Häufigkeit in Prozent","Menschen mit einer Corona-Infektion büssten in Lebenszufriedenheit ein")



# Menschen mit einer Corona-Infektion büssten in Lebenszufriedenheit ein




bivariate_kategorical(tibble_na,"Household_type","","Häufigkeit","fill","Veränderung in diesem Vergleich zu erkennen sind sinnlos")
bivariate_categorical(tibble_na,"Household_type","","Häufigkeit","fill","Veränderung in diesem Vergleich zu erkennen sind sinnlos")



#Veränderung in diesem Vergleich zu erkennen sind sinnlos




bivariate_kategorical(tibble_na,"Income_fac","","Häufigkeit","fill","Nicht verzerrte Aussagen sind nicht möglich")
bivariate_categorical(tibble_na,"Income_fac","","Häufigkeit","fill","Nicht verzerrte Aussagen sind nicht möglich")
#Nicht verzerrte Aussagen sind nicht möglich


bivariate_kategorical(tibble_na,"Commun_type","","Häufigkeit","fill","Der Geimeindetyp 'Industrie' hat die grösste negative Veränderung im Bereich der Lebenszufriedenheit")
bivariate_categorical(tibble_na,"Commun_type","","Häufigkeit","fill","Der Geimeindetyp 'Industrie' hat die grösste negative Veränderung im Bereich der Lebenszufriedenheit")

# 


bivariate_kategorical(tibble_na,"Financial_problem","","Häufigkeit","fill","Erstaunlicherweise erging es Leuten mit ehemaligen Finanziellen Problemen besser")
bivariate_categorical(tibble_na,"Income_fac","","Häufigkeit","fill","Erstaunlicherweise erging es Leuten mit ehemaligen Finanziellen Problemen besser")
#Erstaunlicherweise erging es Leuten mit ehemaligen Finanziellen Problemen besser




bivariate_kategorical(tibble_na,"Unemployed","","Häufigkeit","fill","Hat keinen merklichen Einfluss auf die Zielgrösse")
bivariate_categorical(tibble_na,"Unemployed","","Häufigkeit","fill","Hat keinen merklichen Einfluss auf die Zielgrösse")
#Hat keinen merklichen Einfluss auf die Zielgrösse



bivariate_kategorical(tibble_na,"Illness_accident","","Häufigkeit","fill","Menschen die einen Unfall oder Krankheit erlitten, erfuhren negative Auswirkungen in der Lebenqualität")
bivariate_categorical(tibble_na,"Illness_accident","","Häufigkeit","fill","Menschen die einen Unfall oder Krankheit erlitten, erfuhren negative Auswirkungen in der Lebenqualität")
#Menschen die einen Unfall oder Krankheit erlitten, erfuhren negative Auswirkungen in der Lebenqualität




bivariate_kategorical(tibble_na,"Happy_with_partner","","Häufigkeit","fill","Leute die unglückich in der Beziehung sind, verloren auch an Lebensqualität")
bivariate_categorical(tibble_na,"Happy_with_partner","","Häufigkeit","fill","Leute die unglückich in der Beziehung sind, verloren auch an Lebensqualität")
#Leute die unglückich in der Beziehung sind, verloren auch an Lebensqualität



bivariate_kategorical(tibble_na,"Trust_government","","Häufigkeit","fill","Menschen mit Vertrauensverlust in die Regierung erlitten erhöhte negative Auswirkungen in der Lebensqualität")
bivariate_categorical(tibble_na,"Trust_government","","Häufigkeit","fill","Menschen mit Vertrauensverlust in die Regierung erlitten erhöhte negative Auswirkungen in der Lebensqualität")
#Menschen mit Vertrauensverlust in die Regierung erlitten erhöhte negative Auswirkungen in der Lebensqualität



#Numerical
bivariate_numerisch(tibble_na,"Age","","Häufigkeit","")
#Doesn't look good!
#bivariate_numerisch(tibble_na,"Income","","Häufigkeit")