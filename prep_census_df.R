library(dplyr)
library(tidyr)
library(readr)

#read the df
census_df <- read_csv("Downloads/csvoutput_HC35_2021_03_25_14_57.csv",
                      col_types = cols(YAE = col_skip(),
                                       CAS = col_skip(),
                                       COC = col_skip(),
                                       TIME = col_skip(), 
                                       FLAGS = col_skip(), FOOTNOTES = col_skip()))
# kick germany overall out
census_df <- census_df%>%filter(GEO!="DE")
unique(census_df$GEO)

#rename columns
census_df <- census_df%>%rename(state = GEO,
                   gender = SEX,
                   education = EDU,
                   age = AGE,
                   n = VALUE)

# RECODING VALUES
# 1) age
unique(census_df$age)
census_df <- census_df %>% mutate(age=case_when(
  age == "Y15-19" ~ 1,
  age == "Y20-24" ~ 2,
  age == "Y25-29" ~ 2,
  age == "Y30-34" ~ 3,
  age == "Y35-39" ~ 3,
  age == "Y40-44" ~ 4,
  age == "Y45-49" ~ 4,
  age == "Y50-54" ~ 5,
  age == "Y55-59" ~ 5,
  age == "Y60-64" ~ 6,
  age == "Y65-69" ~ 6,
  age == "Y70-74" ~ 7,
  age == "Y75-79" ~ 7,
  age == "Y80-84" ~ 8,
  age == "Y85-89" ~ 8,
  age == "Y90-94" ~ 8,
  age == "Y95-99" ~ 8,
  age == "Y_GE100" ~ 8))

# 2) State Names
census_df <- census_df%>%mutate(state = case_when(state == "DE1" ~ "Baden-Württemberg",
                                     state == "DE2" ~ "Bayern",
                                     state == "DE3" ~ "Berlin",
                                     state == "DE4" ~ "Brandenburg",
                                     state == "DE5" ~ "Bremen",
                                     state == "DE6" ~ "Hamburg",
                                     state == "DE7" ~ "Hessen",
                                     state == "DE8" ~ "Mecklenburg-Vorpommern",
                                     state == "DE9" ~ "Niedersachsen",
                                     state == "DEA" ~ "Nordrhein-Westfalen",
                                     state == "DEB" ~ "Rheinland-Pfalz",
                                     state == "DEC" ~ "Saarland",
                                     state == "DED" ~ "Sachsen",
                                     state == "DEE" ~ "Sachsen-Anhalt",
                                     state == "DEF" ~ "Schleswig-Holstein",
                                     state == "DEG" ~ "Thüringen"))

# recode education levels
census_df <- census_df%>%mutate(education = case_when(education == "ED1" ~ 2,
                                                      education == "ED2" ~ 2,
                                                      education == "ED3" ~ 4,
                                                      education == "ED4" ~ 4,
                                                      education == "ED5" ~ 5,
                                                      education == "ED6" ~ 5,
                                                      education == "NONE"~ 0,
                                                      education == "UNK" ~ -1 ))

# recode gender levels
census_df <- census_df%>%mutate(gender = case_when(gender == "F" ~ "female",
                                                  gender == "M" ~ "male" ))

# create unique values grid
census_df <- census_df%>%group_by(state, gender, education, age)%>%summarise(n = sum(n))

# save it
readr::write_csv(x = census_df, file = "census_df.csv")



