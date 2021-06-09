library(dplyr)
library(readr)
library(tidyr)

# read survey_df
survey_df <- read_delim("Downloads/ZA7675_v1-0-0.csv", 
                        ";", escape_double = FALSE, locale = locale(date_names = "de", 
                                                                    encoding = "latin1"), trim_ws = TRUE)

# define columns to keep
cols <- c("V0A",
          "VA", 
          "VB",
          "VF",
          "VG",
          "V19A"
          )

# filter the df by the column names
model_df <- survey_df[cols]

# rename columns for the purpose of readability
model_df <- model_df%>%rename(
                              state = V0A,
                              gender = VA,
                              age = VB,
                              education = VF,
                              uni = VG,
                              cohesion = V19A
                             )



# clear Keine Angabes
model_df <- filter(model_df, cohesion != "keine Angabe")


#factorise gender
model_df$gender <- factor(model_df$gender)

# recode age into numbers
model_df <- model_df %>% mutate(age=case_when(
  age == "18 bis 20 Jahre" ~ 1,
  age == "21 bis 24 Jahre" ~ 2,
  age == "25 bis 29 Jahre" ~ 2,
  age == "30 bis 34 Jahre" ~ 3,
  age == "35 bis 39 Jahre" ~ 3,
  age == "40 bis 44 Jahre" ~ 4,
  age == "45 bis 49 Jahre" ~ 4,
  age == "50 bis 59 Jahre" ~ 5,
  age == "60 bis 69 Jahre" ~ 6,
  age == "70 bis 79 Jahre" ~ 7,
  age == "80 Jahre und älter" ~ 8))


# recode gender levels
model_df <- model_df%>%mutate(gender = case_when(gender == "männlich" ~ "male", 
                                                 gender == "weiblich" ~ "female"))


# Arrange education
model_df <- model_df%>%mutate(education = case_when( (education == "Abitur/Hochschulreife/Fachhochschulreife (Ost: frühere 12-klassige erweiterte Oberschule/EOS)" & 
                                                        uni == "ja") ~ 5, 
                                                     (education == "Abitur/Hochschulreife/Fachhochschulreife (Ost: frühere 12-klassige erweiterte Oberschule/EOS)" & 
                                                        uni == "nein") ~ 4,
                                                     education == "Mittlere Reife, Realschulabschluss (mittlerer Abschluss, Ost: frühere 10-klassige politechn. Oberschule/POS)" ~ 2,
                                                     education == "Hauptschulabschluss (Volksschule, Ost: frühere 8-klassige Schule)" ~2,
                                                     education == "kein Schulabschluss" ~ 0,
                                                     education == "keine Angabe"~ -1,                                                                                             
                                                     education == "noch in der Schule"~ -1))



# Create unique values grid
survey_grouped <- model_df%>%group_by(state, gender, age, education, cohesion)%>%summarise(n = n())%>%
      spread(cohesion, n)%>%
      tidyr::replace_na(list(`eher mehr` = 0,  `eher weniger`  = 0, `kein großer Unterschied` = 0))%>%
      mutate(cohesion_c = cbind(`eher mehr`, `eher weniger`, `kein großer Unterschied`),
             n = `eher mehr`+ `eher weniger` + `kein großer Unterschied`)

# save it
readr::write_csv(x = model_df, file = "survey_grouped.csv")
