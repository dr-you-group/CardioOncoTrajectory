library(tidyverse)
library(dplyr)

#### Echo #### 
cardioToxi <- read.csv("./data/cardioToxi.csv",stringsAsFactors = F)

patients <- cardioToxi[,c(1:21)]
patients <- rename(patients, ptno = ID)

echo1 <- cardioToxi[,c(1,22:42)]
echo2 <- cardioToxi[,c(1,43:63)]
echo3 <- cardioToxi[,c(1,64:84)]

tmpDF <- NULL
echo <- NULL

for (i in 1:3) {
  
  tmpDF <- rename(get((paste0('echo',i))), ptno = ID, studyDate = paste0('Echo_',i), 
                  ef = paste0('EF',i), lvGls = paste0('LVGLS',i), la4ch_r = paste0('LA.4CH_R',i),
                  la4ch_Cd = paste0('LA.4CH_Cd',i),la4ch_Ct = paste0('LA.4CH_Ct',i), la2ch_r = paste0('LA2CH_R',i), 
                  la2ch_Cd = paste0('LA2CH_Cd',i),la2ch_Ct = paste0('LA2CH_Ct',i),laStrain = paste0('LA.strain',i),
                  rvGls = paste0('RVGLS',i), lvedd = paste0('LVEDD',i,'.mm'),lvesd = paste0('LVESD',i,'.mm'),
                  lavi = paste0('LAVI',i,'.ml.m2'),mitralE = paste0('Mitral.E',i,'.cm.s'), mitralA = paste0('Mitral.A',i,'.cm.s'),
                  s = paste0('S.',i),e = paste0('e',i,'.'),a = paste0('a.',i),ee = paste0('E.e.',i),rvsp = paste0('RVSP',i,'.mmHg'))
  
  echo <- rbind(echo, tmpDF)
  
}

echo$studyDate <- as.Date(echo$studyDate)

echo <- echo %>%
  group_by(ptno) %>%
  arrange(ptno,studyDate) %>%
  filter(!is.na(ef)) %>%
  filter(length(ef) >= 3)

#### Get birth date and sex of ptno #### 
birthDate <- read.csv("./data/birthdate.csv")
birthDate <- rename(birthDate, birthdate = X.실명.생년월일, ptno = ID)
birthDate$birthdate <- as.Date(birthDate$birthdate)

echo <- left_join(echo,birthDate,by="ptno")
patients <- left_join(patients,birthDate,by="ptno")

patientSex <- patients %>%
  select(ptno,Sex)

echo <- left_join(echo,patientSex,by="ptno")

#### Function for calculating age #### 

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

## calculating age
echo$age<-age(echo$birthdate, echo$studyDate)
echo$age65 <- (echo$age-65)/10 


## change unknown variables into NA 

columnName <- c("lvGls","la4ch_r","la4ch_Cd","la4ch_Ct","la2ch_r","la2ch_Cd","la2ch_Ct","laStrain",
                "rvGls","lvedd","lvesd","lavi","mitralE","mitralA","s","e","a","ee","rvsp")

for (col in columnName) {
  
  echo <- echo %>%
    mutate(!!as.name(col) := as.numeric(ifelse(!!as.name(col) %in% c("","na","N/A","#VALUE!","M/A","#DIV/0!","N/A d/t poor echo window"),NA,!!as.name(col))))
}


#### mutate columns and remove echo period > 730 days #### 

echo <- echo %>%
  mutate(sexDummy = as.factor(ifelse(Sex == 2, 0, 1))) %>%
  mutate(diffInDay = as.numeric(studyDate - first(studyDate))) %>%
  group_by(ptno) %>%
  arrange(ptno,studyDate) %>%
  mutate(studySeq = 1:n())

sample <- echo %>%
  group_by(ptno) %>%
  arrange(ptno,studyDate) %>%
  filter(as.numeric(studyDate - first(studyDate)) <= 730) %>%
  filter(length(ef) >= 2) %>%
  ungroup() %>%
  mutate(diffYear = diffInDay/365)

# obs : 964, patientNum : 325