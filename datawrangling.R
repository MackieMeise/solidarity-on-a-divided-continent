#### LOAD NECESSARY PACKAGES ####

library(readstata13)
library(tidyverse)
library(sjPlot)
library(forcats)
library(MASS)
library(crosstable)
library(gmodels)
library(ggpubr)
library(car)
library(sjstats)
library(broom)
library(eurostat)
library(broom.mixed)
library(sampleSelection)
library(gridExtra)
library(miceadds)

#### LOAD THE DATA FILE ####

rawdata <- read.dta13("data/EUI-YouGov individual.dta")
rawdata2 <- read.csv("data/2021_EUI_YouGov_dataset.csv")

df <- rawdata[, c("recordno", "q4","q12a_1", "q12a_2","q2","q43",
                  "q20_1","q20_2","q20_3","q20_6","q20_7","q20_8","q20_9","q20_10",
                  "q20_11","q20_12","q20_13","q20_14","q20_15","q20_17","q20_18","q20_20",
                  "q20_21","q20_22","q20_23","q20_24","q20_26","q20_27","q20_28","q20_29","q20_30",
                  "q20_31","q20_32",
                  "q61", "q62","age_grp_all","gender_all", "qcountry")]

colnames(df) <- c("recordno", "identity", "inflcntry", "inflpers", "demosat","fundben",
                  "Austria","Belgium","Bulgaria","Croatia", "Cyprus","Czechia","Denmark","Estonia",
                  "Finland","France","Germany","Greece","Hungary","Ireland","Italy", "Latvia",
                  "Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia",
                  "Slovenia","Spain","Sweden",
                  "income", "polselfpl","age","gender", "country")

df2 <- rawdata2[, c("RecordNo", "Q4","Q12a_1", "Q12a_2","Q2","Q43",
                  "q20_1","q20_2","q20_3","q20_6","q20_7","q20_8","q20_9","q20_10",
                  "q20_11","q20_12","q20_13","q20_14","q20_15","q20_17","q20_18","q20_20",
                  "q20_21","q20_22","q20_23","q20_24","q20_26","q20_27","q20_28","q20_29","q20_30",
                  "q20_31","q20_32",
                  "Q61", "Q62","age_grp_all","gender_all", "Qcountry")]

colnames(df2) <- c("recordno", "identity", "inflcntry", "inflpers", "demosat","fundben",
                  "Austria","Belgium","Bulgaria","Croatia", "Cyprus","Czechia","Denmark","Estonia",
                  "Finland","France","Germany","Greece","Hungary","Ireland","Italy", "Latvia",
                  "Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia",
                  "Slovenia","Spain","Sweden",
                  "income", "polselfpl","age","gender", "country")

col_names <- names(df)
df[,col_names] <- lapply(df[,col_names], factor)
df2[,col_names] <- lapply(df2[,col_names], factor)

#### CLEAN UP DATA #####
### change variables as reasonable ====

df$country <- as.character(df$country)
df["country"][df["country"] == "1"] <- "the United Kingdom"
df["country"][df["country"] == "2"] <- "Denmark"
df["country"][df["country"] == "3"] <- "Finland"
df["country"][df["country"] == "4"] <- "France"
df["country"][df["country"] == "5"] <- "Germany"
df["country"][df["country"] == "6"] <- "Sweden"
df["country"][df["country"] == "7"] <- "Greece"
df["country"][df["country"] == "8"] <- "Hungary"
df["country"][df["country"] == "9"] <- "Italy"
df["country"][df["country"] == "10"] <- "Lithuania"
df["country"][df["country"] == "11"] <- "Netherlands"
df["country"][df["country"] == "12"] <- "Poland"
df["country"][df["country"] == "13"] <- "Romania"
df["country"][df["country"] == "14"] <- "Spain"
df$country <- as.factor(df$country)

df$gender <- as.character(df$gender)
df["gender"][df["gender"] == "1"] <- "Male"
df["gender"][df["gender"] == "2"] <- "Female"
df$gender <- as.factor(df$gender)

df$age <- as.character(df$age)
df["age"][df["age"] == "1"] <- "18 - 24"
df["age"][df["age"] == "2"] <- "25 - 34"
df["age"][df["age"] == "3"] <- "35 - 44"
df["age"][df["age"] == "4"] <- "45 - 54"
df["age"][df["age"] == "5"] <- "55+"
df$age <- as.factor(df$age)

df <- df %>%
  mutate(polselfpl=as.character(polselfpl)) %>%
  mutate(polselfpl=ifelse(
    polselfpl<3,"Left",ifelse(
      polselfpl>=3 & polselfpl<6,"Centre",ifelse(
        polselfpl>=6 & polselfpl<8,"Right",NA
      )
    )
  )) %>%
  mutate(polselfpl=as.factor(polselfpl))

df2 <- df2 %>%
  mutate(polselfpl=as.character(polselfpl)) %>%
  mutate(polselfpl=ifelse(
    polselfpl=="Very left-wing"|polselfpl=="Fairly left-wing","Left",ifelse(
      polselfpl=="Centre"|polselfpl=="Sligthly left-of-centre"|polselfpl=="Slightly right-of-centre","Centre",ifelse(
        polselfpl=="Fairly right-wing"|polselfpl=="Very right-wing","Right",NA
      )
    )
  )) %>%
  mutate(polselfpl=as.factor(polselfpl))

df$inflcntry <- as.character(df$inflcntry)
df["inflcntry"][df["inflcntry"] == "1"] <- "Agree"
df["inflcntry"][df["inflcntry"] == "2"] <- "Agree"
df["inflcntry"][df["inflcntry"] == "5"] <- NA
df["inflcntry"][df["inflcntry"] == "3"] <- "Disagree"
df["inflcntry"][df["inflcntry"] == "4"] <- "Disagree"
df$inflcntry <- as.factor(df$inflcntry)

df2$inflcntry <- as.character(df2$inflcntry)
df2["inflcntry"][df2["inflcntry"] == "Strongly agree"] <- "Agree"
df2["inflcntry"][df2["inflcntry"] == "Agree"] <- "Agree"
df2["inflcntry"][df2["inflcntry"] == "Don't know"] <- NA
df2["inflcntry"][df2["inflcntry"] == "Disagree"] <- "Disagree"
df2["inflcntry"][df2["inflcntry"] == "Strongly disagree"] <- "Disagree"
df2$inflcntry <- as.factor(df2$inflcntry)

df$inflpers <- as.character(df$inflpers)
df["inflpers"][df["inflpers"] == "1"] <- "Agree"
df["inflpers"][df["inflpers"] == "2"] <- "Agree"
df["inflpers"][df["inflpers"] == "5"] <- NA
df["inflpers"][df["inflpers"] == "3"] <- "Disagree"
df["inflpers"][df["inflpers"] == "4"] <- "Disagree"
df$inflpers <- as.factor(df$inflpers)

df2$inflpers <- as.character(df2$inflpers)
df2["inflpers"][df2["inflpers"] == "Strongly agree"] <- "Agree"
df2["inflpers"][df2["inflpers"] == "Agree"] <- "Agree"
df2["inflpers"][df2["inflpers"] == "Don't know"] <- NA
df2["inflpers"][df2["inflpers"] == "Disagree"] <- "Disagree"
df2["inflpers"][df2["inflpers"] == "Strongly disagree"] <- "Disagree"
df2$inflpers <- as.factor(df2$inflpers)

df$fundben <- as.character(df$fundben)
df["fundben"][df["fundben"] == "1"] <- "Winner: the help received from this fund by [country] would be higher than the resources put in by it (the national net-balance is positive)"
df["fundben"][df["fundben"] == "2"] <- "Loser: the resources put into this fund by [country] would be higher than the help received by it (the national net-balance is negative)"
df["fundben"][df["fundben"] == "3"] <- NA
df$fundben <- as.factor(df$fundben)

df2$fundben <- as.character(df2$fundben)
df2["fundben"][df2["fundben"] == "Winner: the help received from this fund by $Qcountry would be higher than the resources put in by it (the national net-"] <- "Winner: the help received from this fund by [country] would be higher than the resources put in by it (the national net-balance is positive)"
df2["fundben"][df2["fundben"] == "Loser: the resources put into this fund by $Qcountry would be higher than the help received by it (the national net-bala"] <- "Loser: the resources put into this fund by [country] would be higher than the help received by it (the national net-balance is negative)"
df2["fundben"][df2["fundben"] == "Don't know"] <- NA
df2$fundben <- as.factor(df2$fundben)

df <- df %>%
  mutate(demosat=as.numeric(demosat)) %>%
  mutate(demosat=ifelse(
    demosat<4,"Low satisfaction",ifelse(
      demosat>=4 & demosat<9,"Medium satisfaction",ifelse(
        demosat>=9 & demosat<11,"High satisfaction",NA
      )
    )
  )) %>%
  mutate(demosat=as.factor(demosat))

df2 <- df2 %>%
  mutate(demosat=as.character(demosat)) %>%
  mutate(demosat=ifelse(
    demosat=="0 - Extremely dissatisfied"|demosat=="1"|demosat=="2"|demosat=="3","Low satisfaction",ifelse(
      demosat=="5"|demosat=="6"|demosat=="7"|demosat=="8","Medium satisfaction", ifelse(
        demosat=="9"|demosat=="10 - Extremely satisfied","High satisfaction",NA
      )
    )
  )) %>%
  mutate(demosat=as.factor(demosat))

df <- df %>%
  mutate(euidentity=ifelse(
    identity=="3"|identity=="4",1,0
  )) %>%
  mutate(euidentity=as.factor(euidentity))

df2$euidentity <- as.character(df2$identity)
df2["euidentity"][df2["euidentity"] == "$Qnationality only"] <- "0"
df2["euidentity"][df2["euidentity"] == "$Qnationality and European"] <- "0"
df2["euidentity"][df2["euidentity"] == "European only"] <- "1"
df2["euidentity"][df2["euidentity"] == "European and $Qnationality"] <- "1"
df2["euidentity"][df2["euidentity"] == "Don't know"] <- "0"
df2["euidentity"][df2["euidentity"] == "None of these"] <- "0"
df2$euidentity <- as.factor(df2$euidentity)

df$income <- as.character(df$income)
df["income"][df["income"] == "1"] <- "Better off"
df["income"][df["income"] == "2"] <- "Better off"
df["income"][df["income"] == "3"] <- "Not better nor worse off"
df["income"][df["income"] == "4"] <- "Worse off"
df["income"][df["income"] == "5"] <- "Worse off"
df["income"][df["income"] == "6"] <- NA
df$income <- as.factor(df$income)

df2$income <- as.character(df2$income)
df2["income"][df2["income"] == "I am much better off than most other people my age"] <- "Better off"
df2["income"][df2["income"] == "I am a little better off than most other people my age"] <- "Better off"
df2["income"][df2["income"] == "I am no better or worse off than most other people my age"] <- "Not better nor worse off"
df2["income"][df2["income"] == "I am a little worse off than most other people my age"] <- "Worse off"
df2["income"][df2["income"] == "I am much worse off than most other people my age"] <- "Worse off"
df2["income"][df2["income"] == "Don't know"] <- NA
df2$income <- as.factor(df2$income)

df$year <- "2020"
df$recordno <- paste(df$recordno,df$year)
df2$year <- "2021"
df2$recordno <- paste(df2$recordno,df2$year)

# transform solidarity variables in binary, make DK answers as NA
solvar <- colnames(df[7:33])
for (i in solvar) {
  df[, i] <- as.integer(df[, i])
  df[i][df[i] == 1] <- NA
  df[i][df[i] == 2] <- 1
  df[i][df[i] == 3] <- -1
  df[i][df[i] == 4] <- 0
  df[, i] <- as.factor(df[, i])
}

solvar <- colnames(df2[7:33])
for (i in solvar) {
  df2[, i] <- as.character(df2[, i])
  df2[i][df2[i] == ""] <- NA
  df2[i][df2[i] == "Should be willing to help"] <- 1
  df2[i][df2[i] == "Should not be willing to help"] <- -1
  df2[i][df2[i] == "Don't know"] <- 0
  df2[, i] <- as.factor(df2[, i])
}

#### merge both years ####
dfx <- rbind(df, df2)

#### drop UK respondents ####
dfx <- filter(dfx, country!= "the United Kingdom")

# change solidarity variable from wide to long variable
df1 <- pivot_longer(dfx, cols =Austria:Sweden, names_to="reccountry", values_to ="solidarity")
df1 <- filter(df1, !is.na(solidarity))

# create a variable to identify individuals
df1 <- df1 %>% mutate(id=paste(as.character(recordno),as.character(country)))
df1$id <- as.factor(df1$id)

# transform solidarity variable
df1 <- df1 %>% mutate(solidaritysalience=ifelse(solidarity==0,0,1))
df1 <- df1 %>% mutate(solidarityfeel=ifelse(solidarity==1,1,0))

# drop respondents that give DK answers only
p <- df1 %>%
  group_by(id) %>%
  summarise(drop=sum(abs(as.numeric(solidarity))))

df1 <- left_join(df1,p)
df1 <- filter(df1,drop!=0)
df1["solidarity"][df1["solidarity"] == 0] <- NA
df1$solidarity <- as.factor(df1$solidarity)

#### LOAD MACRO_ECONOMIC DATA ####
countrydata <- read.csv("C:/Users/pclas/Desktop/PhD Stuff/country-specific solidarity/countryvar.csv", sep=";")
df1 <- left_join(df1,countrydata)
colnames(countrydata) <- c("reccountry","receumbrshp","rectseumbrshp","recweumbrshp","receurombmr","recschengen","rectaxonomy", "rectaxonomy2", "recbailout")
df1 <- left_join(df1,countrydata)

# load country-pair data
countrypdata <- read.csv("C:/Users/pclas/Desktop/PhD Stuff/country-specific solidarity/countrypvar.csv", sep=";")
df1 <- left_join(df1,countrypdata)

# merge finance and core variables into one
df1$taxonomy <- as.factor(df1$taxonomy)
df1$rectaxonomy <- as.factor(df1$rectaxonomy)
df1$taxonomy2 <- as.factor(df1$taxonomy2)
df1$rectaxonomy2 <- as.factor(df1$rectaxonomy2)

# load data for cultural distance
culturaldistance <- read.csv("C:/Users/pclas/Desktop/PhD Stuff/country-specific solidarity/culturaldistances.csv", sep=";")
culturaldistance <- pivot_longer(culturaldistance, cols =Belgium:Ukraine, names_to="reccountry", values_to ="culdis")
df1 <- left_join(df1,culturaldistance)

# load data for social connectedness
socialconnectedness <- read.csv("C:/Users/pclas/Desktop/PhD Stuff/country-specific solidarity/scidata.csv", sep=";")
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "AT"] <- "Austria"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "BE"] <- "Belgium"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "BG"] <- "Bulgaria"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "CY"] <- "Cyprus"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "CZ"] <- "Czechia"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "DE"] <- "Germany"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "DK"] <- "Denmark"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "EE"] <- "Estonia"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "GR"] <- "Greece"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "ES"] <- "Spain"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "FI"] <- "Finland"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "FR"] <- "France"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "HR"] <- "Croatia"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "HU"] <- "Hungary"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "IE"] <- "Ireland"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "IT"] <- "Italy"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "LT"] <- "Lithuania"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "LU"] <- "Luxembourg"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "LV"] <- "Latvia"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "MT"] <- "Malta"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "NL"] <- "Netherlands"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "PL"] <- "Poland"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "PT"] <- "Portugal"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "RO"] <- "Romania"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "SE"] <- "Sweden"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "SI"] <- "Slovenia"
socialconnectedness["user_loc"][socialconnectedness["user_loc"] == "SK"] <- "Slovakia"

socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "AT"] <- "Austria"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "BE"] <- "Belgium"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "BG"] <- "Bulgaria"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "CY"] <- "Cyprus"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "CZ"] <- "Czechia"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "DE"] <- "Germany"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "DK"] <- "Denmark"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "EE"] <- "Estonia"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "GR"] <- "Greece"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "ES"] <- "Spain"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "FI"] <- "Finland"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "FR"] <- "France"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "HR"] <- "Croatia"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "HU"] <- "Hungary"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "IE"] <- "Ireland"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "IT"] <- "Italy"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "LT"] <- "Lithuania"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "LU"] <- "Luxembourg"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "LV"] <- "Latvia"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "MT"] <- "Malta"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "NL"] <- "Netherlands"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "PL"] <- "Poland"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "PT"] <- "Portugal"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "RO"] <- "Romania"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "SE"] <- "Sweden"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "SI"] <- "Slovenia"
socialconnectedness["fr_loc"][socialconnectedness["fr_loc"] == "SK"] <- "Slovakia"
colnames(socialconnectedness) <- c("country", "reccountry", "sci")
df1 <- left_join(df1,socialconnectedness)

# load economic data
econdata <- get_eurostat(id="nama_10_pp")
popdata <- get_eurostat(id="tps00001")
colnames(popdata) <- c("indic", "geo","time","pop")
econdata <- left_join(econdata,popdata, by="geo", "time")
econdata <- econdata %>% filter(time.y=="2020-01-01" & time.x=="2020-01-01")
econdata[1] <- NULL
econdata[2] <- NULL
econdata[3:4] <- NULL 
econdata["geo"][econdata["geo"] == "AT"] <- "Austria"
econdata["geo"][econdata["geo"] == "BE"] <- "Belgium"
econdata["geo"][econdata["geo"] == "BG"] <- "Bulgaria"
econdata["geo"][econdata["geo"] == "CY"] <- "Cyprus"
econdata["geo"][econdata["geo"] == "CZ"] <- "Czechia"
econdata["geo"][econdata["geo"] == "DE"] <- "Germany"
econdata["geo"][econdata["geo"] == "DK"] <- "Denmark"
econdata["geo"][econdata["geo"] == "EE"] <- "Estonia"
econdata["geo"][econdata["geo"] == "EL"] <- "Greece"
econdata["geo"][econdata["geo"] == "ES"] <- "Spain"
econdata["geo"][econdata["geo"] == "FI"] <- "Finland"
econdata["geo"][econdata["geo"] == "FR"] <- "France"
econdata["geo"][econdata["geo"] == "HR"] <- "Croatia"
econdata["geo"][econdata["geo"] == "HU"] <- "Hungary"
econdata["geo"][econdata["geo"] == "IE"] <- "Ireland"
econdata["geo"][econdata["geo"] == "IT"] <- "Italy"
econdata["geo"][econdata["geo"] == "LT"] <- "Lithuania"
econdata["geo"][econdata["geo"] == "LU"] <- "Luxembourg"
econdata["geo"][econdata["geo"] == "LV"] <- "Latvia"
econdata["geo"][econdata["geo"] == "MT"] <- "Malta"
econdata["geo"][econdata["geo"] == "NL"] <- "Netherlands"
econdata["geo"][econdata["geo"] == "PL"] <- "Poland"
econdata["geo"][econdata["geo"] == "PT"] <- "Portugal"
econdata["geo"][econdata["geo"] == "RO"] <- "Romania"
econdata["geo"][econdata["geo"] == "SE"] <- "Sweden"
econdata["geo"][econdata["geo"] == "SI"] <- "Slovenia"
econdata["geo"][econdata["geo"] == "SK"] <- "Slovakia"

colnames(econdata) <- c("country","gdppc", "population")
df1 <- left_join(df1,econdata)
colnames(econdata) <- c("reccountry","gdppc2", "recpopulation")
df1 <- left_join(df1,econdata)
df1 <- df1 %>% mutate(gdp=gdppc/1000, recgdp=gdppc2/1000, population=population/1000, recpopulation=recpopulation/1000000)

# change data format of macro-variables
df1["eurombmr"][df1["eurombmr"] == ""] <- NA
df1$eurombmr <- as.factor(df1$eurombmr)
df1["receurombmr"][df1["receurombmr"] == ""] <- NA
df1$receurombmr <- as.factor(df1$receurombmr)
df1["border"][df1["border"] == ""] <- NA
df1$border <- as.factor(df1$border)
df1$country <- as.factor(df1$country)
df1$reccountry <- as.factor(df1$reccountry)

#### DROPPING COLUMNS THAT I DONT NEED ####
df1["recordno"] <- NULL
df1["wahlverhalten"] <- NULL
df1["solidarity"] <- NULL
df1["schenge"] <- NULL
df1["bailout"] <- NULL
df1["receumbrshp"] <- NULL
df1["recweumbrshp"] <- NULL
df1["recbailout"] <- NULL
df1["culdis"] <- NULL
df1["population"] <- NULL
df1["gdppc"] <- NULL
df1["gdppc2"] <- NULL
df1["weumbrshp"] <- NULL
df1["tseumbrshp"] <- NULL
df1["rectseumbrshp"] <- NULL
df1["drop"] <- NULL
df1["identity"] <- NULL

#### DROPPING DATASETS THAT I DONT NEED ####
rm(list=c("countrydata", "countrypdata", "econdata", "popdata", "rawdata", "rawdata2", "culturaldistance", "socialconnectedness"))

# save file for Stata ----
write.csv(df1,"C:/Users/pclas/Desktop/Phd Stuff/country-specific solidarity/df1.csv", row.names = FALSE)
save(df1, file = "data/data.rda")
