#### LOAD NECESSARY PACKAGES ####

library(readstata13)
library(tidyverse)
library(ggplot2)
library(lme4)
library(sjPlot)
library(texreg)
library(forcats)
library(eurostat)
library(MASS)
library(glm.predict)
library(sjPlot)
library(crosstable)
library(gmodels)
library(ggpubr)
library(car)
library(lmerTest)
library(sjstats)
library(broom)
library(knitr)
library(eurostat)
library(eumaps)
library(mice)
library(broom.mixed)
library(sampleSelection)
library(gridExtra)
library(miceadds)


#### LOAD THE DATA FILE ####

rawdata <- read.dta13("C:/Users/pclas/Desktop/PhD Stuff/EUI-YouGov individual.dta")
rawdata2 <- read.csv("C:/Users/pclas/Desktop/PhD Stuff/2021_EUI_YouGov_dataset.csv")

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

df$polselfpl <- as.character(df$polselfpl)
df["polselfpl"][df["polselfpl"] == "1"] <- "Left"
df["polselfpl"][df["polselfpl"] == "2"] <- "Left"
df["polselfpl"][df["polselfpl"] == "3"] <- "Centre"
df["polselfpl"][df["polselfpl"] == "4"] <- "Centre"
df["polselfpl"][df["polselfpl"] == "5"] <- "Centre"
df["polselfpl"][df["polselfpl"] == "6"] <- "Right"
df["polselfpl"][df["polselfpl"] == "7"] <- "Right"
df["polselfpl"][df["polselfpl"] == "8"] <- NA
df["polselfpl"][df["polselfpl"] == "100"] <- NA
df$polselfpl <- as.factor(df$polselfpl)

df2$polselfpl <- as.character(df2$polselfpl)
df2["polselfpl"][df2["polselfpl"] == "Very left-wing"] <- "Left"
df2["polselfpl"][df2["polselfpl"] == "Fairly left-wing"] <- "Left"
df2["polselfpl"][df2["polselfpl"] == "Centre"] <- "Centre"
df2["polselfpl"][df2["polselfpl"] == "Slightly left-of-centre"] <- "Centre"
df2["polselfpl"][df2["polselfpl"] == "Slightly right-of-centre"] <- "Centre"
df2["polselfpl"][df2["polselfpl"] == "Fairly right-wing"] <- "Right"
df2["polselfpl"][df2["polselfpl"] == "Very right-wing"] <- "Right"
df2["polselfpl"][df2["polselfpl"] == "Prefer not to say"] <- NA
df2["polselfpl"][df2["polselfpl"] == "Don't know"] <- NA
df2$polselfpl <- as.factor(df2$polselfpl)

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

df$demosat <- as.character(df$demosat)
df["demosat"][df["demosat"] == "0"] <- "Low satistfaction"
df["demosat"][df["demosat"] == "1"] <- "Low satistfaction"
df["demosat"][df["demosat"] == "2"] <- "Low satistfaction"
df["demosat"][df["demosat"] == "3"] <- "Low satistfaction"
df["demosat"][df["demosat"] == "4"] <- "Medium satistfaction"
df["demosat"][df["demosat"] == "5"] <- "Medium satistfaction"
df["demosat"][df["demosat"] == "6"] <- "Medium satistfaction"
df["demosat"][df["demosat"] == "7"] <- "Medium satistfaction"
df["demosat"][df["demosat"] == "8"] <- "Medium satistfaction"
df["demosat"][df["demosat"] == "9"] <- "High satistfaction"
df["demosat"][df["demosat"] == "10"] <- "High satistfaction"
df["demosat"][df["demosat"] == "11"] <- NA
df$demosat <- as.factor(df$demosat)

df2$demosat <- as.character(df2$demosat)
df2["demosat"][df2["demosat"] == "0 - Extremely dissatisfied"] <- "Low satistfaction"
df2["demosat"][df2["demosat"] == "1"] <- "Low satistfaction"
df2["demosat"][df2["demosat"] == "2"] <- "Low satistfaction"
df2["demosat"][df2["demosat"] == "3"] <- "Low satistfaction"
df2["demosat"][df2["demosat"] == "4"] <- "Medium satistfaction"
df2["demosat"][df2["demosat"] == "5"] <- "Medium satistfaction"
df2["demosat"][df2["demosat"] == "6"] <- "Medium satistfaction"
df2["demosat"][df2["demosat"] == "7"] <- "Medium satistfaction"
df2["demosat"][df2["demosat"] == "8"] <- "Medium satistfaction"
df2["demosat"][df2["demosat"] == "9"] <- "High satistfaction"
df2["demosat"][df2["demosat"] == "10 - Extremely satisfied"] <- "High satistfaction"
df2["demosat"][df2["demosat"] == "Don't know"] <- NA
df2$demosat <- as.factor(df2$demosat)

df$euidentity <- as.character(df$identity)
df["euidentity"][df["euidentity"] == "1"] <- "0"
df["euidentity"][df["euidentity"] == "2"] <- "0"
df["euidentity"][df["euidentity"] == "3"] <- "1"
df["euidentity"][df["euidentity"] == "4"] <- "1"
df["euidentity"][df["euidentity"] == "5"] <- "0"
df["euidentity"][df["euidentity"] == "6"] <- "0"
df$euidentity <- as.factor(df$euidentity)

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

# save file for Stata ----
write.csv(df1,"C:/Users/pclas/Desktop/Phd Stuff/country-specific solidarity/df1.csv", row.names = FALSE)

#### DATA EXPLORATION ####

m <- df1 %>%
  group_by(id) %>%
  summarise(count=n())

summary(m$count)

summary(df1$solidaritysalience)
m <- filter(df1, solidaritysalience==1)
summary(m$solidarityfeel)


# plotting solidarity salience and feeling on aggregated level
df_test <- df1 %>%
  group_by(country, reccountry, rectaxonomy2) %>%
  summarise(count=n(), solsal=sum(solidaritysalience), solfee=sum(solidarityfeel)) %>%
  mutate(sallvl=solsal/count, feelvl=solfee/count, solnet=solfee/solsal)

# analyse response patterns of solidarity variable

# the graph shows the profiles of respondents - some show solidarity regardless of the country, other show none regardless. But many do indeed distinguish between countries
p <- df1 %>%
  group_by(id, country) %>%
  summarise(sol=sum(solidarityfeel), dksol=sum(solidaritysalience), count=n()) %>%
  mutate(profile=ifelse(dksol == count & sol == count,"fullsolidarity",ifelse(dksol==0,"noopinion",ifelse(sol>=0.5*dksol,"mostly solidary","mostly not solidary"))))

ggplot(p, aes(x=country, fill=profile)) +
  geom_bar(position="fill")

#### PROVIDE SUMMARY STATISTICS ####
outcomedata <- filter(df1, solidaritysalience==1)
selectiondata <- df1
mean(outcomedata$polselfpl, na.rm=T)
sd(outcomedata$polselfpl, na.rm=T)
mean(selectiondata$polselfpl, na.rm=T)
sd(selectiondata$polselfpl,na.rm=T)

mean(as.numeric(outcomedata$gender, na.rm=T))
sd(as.numeric(outcomedata$gender, na.rm=T))
mean(as.numeric(selectiondata$gender, na.rm=T))
sd(as.numeric(selectiondata$gender,na.rm=T))

mean(selectiondata$tseumbrshp, na.rm=T)
sd(selectiondata$tseumbrshp,na.rm=T)

mean(as.numeric(outcomedata$eurombmr, na.rm=T))
sd(as.numeric(outcomedata$eurombmr, na.rm=T))
mean(as.numeric(selectiondata$eurombmr, na.rm=T))
sd(as.numeric(selectiondata$eurombmr,na.rm=T))

mean(as.numeric(outcomedata$receurombmr, na.rm=T))
sd(as.numeric(outcomedata$receurombmr, na.rm=T))
mean(as.numeric(selectiondata$receurombmr, na.rm=T))
sd(as.numeric(selectiondata$receurombmr,na.rm=T))

mean(as.numeric(outcomedata$solidarityfeel, na.rm=T))
sd(as.numeric(outcomedata$solidarityfeel, na.rm=T))
mean(as.numeric(selectiondata$solidaritysalience, na.rm=T))
sd(as.numeric(selectiondata$solidaritysalience,na.rm=T))

mean(as.numeric(outcomedata$euidentity), na.rm=T)
sd(as.numeric(outcomedata$euidentity), na.rm=T)
mean(as.numeric(selectiondata$euidentity), na.rm=T)
sd(as.numeric(selectiondata$euidentity),na.rm=T)

summary(outcomedata$age)
summary(selectiondata$age)

mean(as.numeric(outcomedata$border, na.rm=T))
sd(as.numeric(outcomedata$border, na.rm=T))
mean(as.numeric(selectiondata$border, na.rm=T))
sd(as.numeric(selectiondata$border,na.rm=T))

mean(as.numeric(outcomedata$sci, na.rm=T))
sd(as.numeric(outcomedata$sci, na.rm=T))
mean(as.numeric(selectiondata$sci, na.rm=T))
sd(as.numeric(selectiondata$sci,na.rm=T))

mean(as.numeric(selectiondata$recpopulation, na.rm=T))
sd(as.numeric(selectiondata$recpopulation,na.rm=T))

mean(as.numeric(outcomedata$gdp, na.rm=T))
sd(as.numeric(outcomedata$gdp, na.rm=T))
mean(as.numeric(selectiondata$gdp, na.rm=T))
sd(as.numeric(selectiondata$gdp,na.rm=T))

mean(as.numeric(outcomedata$recgdp, na.rm=T))
sd(as.numeric(outcomedata$recgdp, na.rm=T))
mean(as.numeric(selectiondata$recgdp, na.rm=T))
sd(as.numeric(selectiondata$recgdp,na.rm=T))

outcomedata <- outcomedata %>% mutate(inflcntry_b = ifelse(inflcntry=="Agree" | inflcntry =="Strongly agree",1, ifelse(inflcntry == "Don't know", NA,0)))
selectiondata <- selectiondata %>% mutate(inflcntry_b = ifelse(inflcntry=="Agree" | inflcntry =="Strongly agree",1, ifelse(inflcntry == "Don't know", NA,0)))
mean(outcomedata$inflcntry_b, na.rm=T)
sd(outcomedata$inflcntry_b, na.rm=T)
mean(selectiondata$inflcntry_b, na.rm=T)
sd(selectiondata$inflcntry_b,na.rm=T)

selectiondata <- selectiondata %>% mutate(inflpers_b = ifelse(inflpers=="Agree" | inflpers =="Strongly agree",1, ifelse(inflpers == "Don't know", NA,0)))
mean(selectiondata$inflpers_b, na.rm=T)
sd(selectiondata$inflpers_b,na.rm=T)

selectiondata$demosat <- as.character(selectiondata$demosat)
selectiondata$demosat <- as.numeric(selectiondata$demosat)
mean(selectiondata$demosat, na.rm=T)
sd(selectiondata$demosat,na.rm=T)

test <- outcomedata %>%
  group_by(age) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(age) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- outcomedata %>%
  group_by(taxonomy) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(taxonomy) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- outcomedata %>%
  group_by(polselfpl) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(polselfpl) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- outcomedata %>%
  group_by(demosat) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(demosat) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- outcomedata %>%
  group_by(income) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(income) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- outcomedata %>%
  group_by(fundben) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

test <- selectiondata %>%
  group_by(fundben) %>%
  summarise(n=n()) %>%
  mutate(perc=n/sum(n))

#### MULTILEVEL LOGISTIC REGRESSION ####
### First Calulcate Whether A Citizen Has An Opinion Or Not ====
# create modeldata df
modeldata <- filter(df1, !is.na(inflpers) & !is.na(demosat) & !is.na(inflcntry) & !is.na(identity) & !is.na(polselfpl) & !is.na(income))

# null model
m0 <- glmer(solidaritysalience ~ (1|id) + (1|country), data=modeldata, family="binomial", nAGQ=0)
performance::icc(m0, by_group=T)

# only country factors
m1<- glmer(solidaritysalience ~ recpopulation + rectseumbrshp + recgdp + rectaxonomy + receurombmr + recbailout + border + sci + (1|id) + (1|country), 
           data=modeldata, 
           family="binomial", 
           nAGQ=0)

#level 2 variables added
m2<- glmer(solidaritysalience ~ recpopulation + rectseumbrshp + inflpers + demosat + inflcntry + identity + polselfpl + age + gender + income + recgdp + rectaxonomy + receurombmr + recbailout+ border + sci + (1|id) + (1|country), 
           data=modeldata, 
           family="binomial", 
           nAGQ=0)

#level 3 variables added
m3<- glmer(solidaritysalience ~ recpopulation + rectseumbrshp + inflpers + demosat + inflcntry + identity + polselfpl + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + recbailout+ border + sci + (1|id) + (1|country), 
           data=modeldata, 
           family="binomial", 
           nAGQ=0)

#interaction effects added
m4<- glmer(solidaritysalience ~ recpopulation + rectseumbrshp + inflpers + demosat + inflcntry + identity + polselfpl + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + recbailout+ border + sci + taxonomy*rectaxonomy + eurombmr*eurombmr + eurombmr*rectaxonomy + (1|id) + (1|country), 
           data=df1, 
           family="binomial", 
           nAGQ=0)

#display results of regression analysis 
screenreg(list(m1,m2,m3,m4))
htmlreg(list(m1, m2, m3, m4), file="regtable_sal_2.doc", single.row=T, stars=0.01)
plot_model(m4, type="pred", terms=c("receurombmr"))

# test assumptions

dfx$pred <- predict(m4,type="response", newdata=dfx)
ggplot(dfx, aes(x=culdis, y=pred)) + geom_jitter() + geom_smooth(method="glm")

vif(m3)

## calculate weights ----
modeldata$pred <- predict(m4,type="response", newdata=modeldata)
modeldata <- modeldata %>%
  mutate(weight=(ifelse(solidaritysalience==1, 1/pred, 1/(1-pred))))

modeldatad <- filter(modeldata, solidaritysalience==1)
#calculate icc of null model
m0 <- glmer(solidarityfeel ~ (1|id) + (1|country), data=modeldatad, family="binomial", nAGQ=0)
performance::icc(m0, by_group=T)

#level 1 variables only
m1<- glmer(solidarityfeel ~ recgdp + rectaxonomy + receurombmr + border + sci + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0)

m1w<- glmer(solidarityfeel ~ recgdp + rectaxonomy + receurombmr + border + sci + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0,
           weights=weight)

#level 2 variables added
m2<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + age + gender + income + recgdp + rectaxonomy + receurombmr + border + sci + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0)

m2w<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + age + gender + income + recgdp + rectaxonomy + receurombmr + border + sci + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0,
           weights=weight)

#level 3 variables added
m3<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + fundben + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + border + sci + (1|id) + (1|country), 
           data=df1, 
           family="binomial", 
           nAGQ=0)
m3w<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + border + sci + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0,
           weights=weight)

#interaction effects added
m4<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + fundben + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + border + sci + taxonomy*rectaxonomy + (1|id) + (1|country), 
           data=df1, 
           family="binomial", 
           nAGQ=0)
m5<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + fundben + age + gender + income + taxonomy + rectaxonomy + eurombmr + receurombmr + border + sci + taxonomy*rectaxonomy + (1|id) + (1|country), 
           data=df1, 
           family="binomial", 
           nAGQ=0)
m6<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + fundben + age + gender + income + gdp + recgdp + eurombmr + receurombmr + border + sci + gdp*recgdp + (1|id) + (1|country), 
           data=df1, 
           family="binomial", 
           nAGQ=0)

m4w<- glmer(solidarityfeel ~ inflcntry + identity + polselfpl + age + gender + income + gdp + recgdp + taxonomy + rectaxonomy + eurombmr + receurombmr + border + sci + eurombmr*receurombmr + taxonomy*rectaxonomy + eurombmr*rectaxonomy + (1|id) + (1|country), 
           data=modeldatad, 
           family="binomial", 
           nAGQ=0,
           weights=weight)

#display results of regression analysis 
screenreg(list(m1,m2,m3,m4, m1w,m2w,m3w,m4w))
htmlreg(list(m1, m2, m3, m4, m1w, m2w, m3w, m4w), file="regtable_sal_3.doc", single.row=T, stars=0.01)

#effect plotting
plot_model(m6, type="pred", terms=c("recgdp","gdp")) +
  labs(title="", y="Predicted probability of European fiscal solidarity", x="Recipient country") + 
  scale_colour_manual(values=c("#ca0020", "black", "#0571b0"), name="Respondent's country") +
  scale_fill_manual(values=c("#ca0020", "darkgrey", "#0571b0"), name="Respondent's country") +
  theme_pubclean()
ggsave("plotx.png", width=150, height=100, unit="mm")

plot_model(m4, type="pred", terms=c("eurombmr", "receurombmr")) 


#### experimental play around ####

p <- df1 %>%
  group_by(country, reccountry) %>%
  summarise(solidarity=sum(ifelse(solidarity=="1",1,0)/n()))

p2 <- p %>%
  group_by(reccountry) %>%
  summarise(solidarity=mean(solidarity))
p3 <- p %>%
  group_by(country) %>%
  summarise(solidarity=mean(solidarity))
px <- left_join(p2,p3, by=c("reccountry" = "country"))
ggplot(px, aes(solidarity.x, solidarity.y)) + geom_point() + geom_label(aes(label=reccountry)) + geom_abline(slope=1,intercept=0)

m2 <- glmer(solidarity ~ 1 + (1|id) + (1|country), data=filter(df1,solidarity!=99 & country != "United Kingdom"), family="binomial",nAGQ=0)
m3 <- glmer(solidarity ~ border+ eurombmr*receurombmr + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
m4 <- glmer(solidarity ~ eurombmr*receurombmr + weumbrshp*rectweumbrshp + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
m5 <- glmer(solidarity ~ border + moneyspent2 + eurombmr*receurombmr + weumbrshp*rectweumbrshp + (1|id) + (1|country), data=filter(df1, solidarity!=99 & country != "United Kingdom"), family="binomial", nAGQ=0)
screenreg(list(m5))
plot_model(m5, type="pred", terms=c("eurombmr","receurombmr"))
plot_model(m5, type="pred", terms=c("border"))

mx4 <- glmer(solidarity ~ inflcntry*rectaxonomy + (1|id) + (1|country), data=df1, family="binomial", nAGQ=0)

screenreg(mx4)
screenreg(list(mx1,mx2,mx3))
plot_model(mx3, type="pred", terms=c("taxonomy", "rectaxonomy"))
plot_model(mx3, type="pred", terms=c("eurombmr", "receurombmr"))
plot_model(mx4, type="pred", terms=c("inflcntry", "rectaxonomy"))

vif(mx2)
mx4 <- glmer(solidarity ~ moneyspent2 + eurombmr*receurombmr + border + culdis + (1|id) + (1|country), data=filter(df1, solidarity!=99 & eurombmr != "0" & receurombmr != "0"), family="binomial", nAGQ=0)
plot_model(mx4, type="pred", terms=c("eurombmr", "receurombmr"))

newdf <- filter(df1,solidarity!=99 & country != "United Kingdom") %>%
  mutate(yhat=predict(mx, type="response", newdata=filter(df1,solidarity!=99 & country != "United Kingdom")))


#### HAND-MADE SAMPLE SELECTION MODEL ####
df2 <- df1 %>%
  mutate(demosat=as.character(demosat),
         polselfpl=as.character(polselfpl),
         inflcntry=as.character(inflcntry),
         inflpers=as.character(inflpers),
         income=as.character(income),
         fundben=as.character(fundben)) %>%
  mutate(demosat=ifelse(is.na(demosat),"NA", demosat),
         polselfpl=ifelse(is.na(polselfpl),"NA",polselfpl),
         inflcntry=ifelse(is.na(inflcntry),"NA",inflcntry),
         inflpers=ifelse(is.na(inflpers),"NA",inflpers),
         income=ifelse(is.na(income),"NA",income),
         fundben=ifelse(is.na(fundben),"NA",fundben)) %>%
  mutate(demosat=as.factor(demosat),
         polselfpl=as.factor(polselfpl),
         inflcntry=as.factor(inflcntry),
         inflpers=as.factor(inflpers),
         income=as.factor(income),
         fundben=as.factor(fundben))

df2 <- na.omit(df2)

cluster_selectionmodel <- function(selformula, outformula, cluster, data) {
  m1_s <- glm.cluster(selformula,
                      data=data,
                      cluster=cluster,
                      family=binomial(link="probit"))
  
  first_stage_lp <- predict(m1_s$glm_res)
  data$IMR <- dnorm(first_stage_lp)/pnorm(first_stage_lp)
  
  m1_o <- glm.cluster(outformula,
                      data=data,
                      cluster=cluster,
                      family=binomial(link="probit"))
  return(list(selm=m1_s, outm=m1_o))
  
}
m1 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + IMR,
                             data=df2,
                             cluster=df2$id)

m2 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + IMR,
                             data=df2,
                             cluster=df2$id)

m3 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*inflcntry,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*inflcntry + IMR,
                             data=df2,
                             cluster=df2$id)

m4 <- cluster_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry,
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry + IMR,
                             data=df2,
                             cluster=df2$id)
screenreg(list(m1$outm, m2$outm, m3$outm, m4$outm))
htmlreg(list(m1$outm, m2$outm, m3$outm, m4$outm), file="regtable.doc", single.row=T, stars=0.01)
screenreg(list(m1$selm, m2$selm, m3$selm, m4$selm))
htmlreg(list(m1$selm, m2$selm, m3$selm, m4$selm), file="regtable_selm.doc", single.row=T, stars=0.01)
summary(m1$selm)

p1 <- plot_model(m4$outm$glm_res, type="pred", terms=c("taxonomy2", "rectaxonomy2"), ci.lvl=0.99) +
  labs(title="", y="Predicted probability of solidarity", x="Cleavage group of respondent") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Cleavage group of recipient country") +
  scale_fill_manual(values=c("gray", "lightgray"), name="Cleavage group of recipient country") +
  scale_y_continuous(limits=c(0.3,0.6))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

p2 <- plot_model(m4$outm$glm_res, type="pred", terms=c("taxonomy2", "inflcntry[Agree, Disagree]"), ci.lvl=0.99) +
  labs(title="", y="", x="Cleavage group of respondent") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Sociotropic political efficacy") +
  scale_fill_manual(values=c("gray", "lightgray"), name="Sociotropic political efficacy") +
  scale_y_continuous(limits=c(0.3,0.6))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

ggarrange(p1,p2, ncol=2, labels="AUTO", common.legend=F)
ggsave("interaction_bw.png", width=190, height=130, unit="mm")

# LRT for the interaction effects
anova(m4$outm$glm_res, m3$outm$glm_res, test="LRT")
anova(m4$outm$glm_res, m2$outm$glm_res, test="LRT")
anova(m3$outm$glm_res, m1$outm$glm_res, test="LRT")
anova(m2$outm$glm_res, m1$outm$glm_res, test="LRT")

#R2
with(summary(m1$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m2$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m3$outm$glm_res), 1 - deviance/null.deviance)
with(summary(m4$outm$glm_res), 1 - deviance/null.deviance)

multilevel_selectionmodel <- function(selformula, outformula, data) {
  m1_s <- glmer(selformula,
                data=data,
                family=binomial(link="probit"),
                nAGQ = 0)

  first_stage_lp <- predict(m1_s)
  data$IMR <- dnorm(first_stage_lp)/pnorm(first_stage_lp)
  
  m1_o <- glmer(outformula,
                data=data,
                family=binomial(link="probit"),
                nAGQ = 0)
  return(list(selm=m1_s, outm=m1_o))
  
}

m1ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + year + (1|id),
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + IMR + year + (1|id),
                             data=df2)

m2ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2+ year + (1|id),
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + IMR+ year + (1|id),
                             data=df2)

m3ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*inflcntry+ year + (1|id),
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*inflcntry + IMR+ year + (1|id),
                             data=df2)

m4ml <- multilevel_selectionmodel(selformula=solidaritysalience ~ inflcntry + inflpers + demosat + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + recpopulation + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry+ year + (1|id),
                             outformula=solidarityfeel ~ inflcntry + fundben + income + polselfpl + age + gender + euidentity + taxonomy2 + rectaxonomy2 + border + sci + gdp + recgdp + eurombmr + receurombmr + taxonomy2*rectaxonomy2 + taxonomy2*inflcntry + IMR+ year + (1|id),
                             data=df2)
screenreg(list(m1ml$outm, m2ml$outm, m3ml$outm, m4ml$outm))

library(jtools)
library(margins)
export_summs(m1ml$outm,m2ml$outm,m3ml$outm,m4ml$outm)
mod1 <- glm(solidarityfeel ~ gender + polselfpl, data=df2, family=binomial(link="probit"))
mod2 <- glm(solidarityfeel ~ gender + polselfpl + age, data=df2, family=binomial(link="probit"))

export_summs(m1ml$outm,m2ml$outm,m3ml$outm,m4ml$outm)
m1_ma <- margins(m1ml$outm)
m2_ma <- margins(m2ml$outm)
m3_ma <- margins(m3ml$outm)
m4_ma <- margins(m4ml$outm)
export_summs(m1_ma, m2_ma, m3_ma, m4_ma)
export_summs(m1ml$outm, m1_ma, r.sqaured=F)
screenreg(list(mod1, mod2))

mod1_margins <- margins(mod2)
mod2_margins <- margins(mod2)
export_summs(mod1_margins, mod2_margins)
plot_model(m4$selm$glm_res, type="pred", terms=c("taxonomy2", "rectaxonomy2"), ci.lvl=0.99)

rawdata <- rawdata %>%
  mutate(timeA=strptime(endtime,format="%d/%m/%Y %H:%M"), timeB=strptime(starttime,format="%d/%m/%Y %H:%M")) %>%
  mutate(time=timeA-timeB)

ggplot(rawdata, aes(x=time, y=age_grp_all)) +
  geom_point() +
  geom_smooth(method="lm")
plot(rawdata$time~rawdata$q12a_1)
