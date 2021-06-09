library(tidyr)
library(dplyr)
library(readr)
library(RCurl)

# read the dataframes from github
#Covid 
covid_df <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/e8eed0b5763fcd1835a2a9fe095271e9574a71a8/csse_covid_19_data/csse_covid_19_daily_reports/06-23-2020.csv")
covid_df <- read.csv(text = covid_df)

#indices
indices_df <- read_csv("Downloads/GDL-Indices-(2018)-data.csv", 
                       col_types = cols(Country = col_skip(), 
                                        ISO_Code = col_skip(), Level = col_skip(), 
                                        GDLCODE = col_skip()))

# rename region to state
indices_df <- indices_df%>%rename(state = Region)


# COVID DATA
# select only germany
covid_df <- filter(covid_df, Country_Region == "Germany")

#pick relevant columns
cols <- c("Province_State", "Incidence_Rate", "Case.Fatality_Ratio")
covid_df <- covid_df[cols]%>%drop_na()

#rename 
covid_df <- covid_df%>%rename(state = Province_State)

# join df_s
m_level <- left_join(x =covid_df, y = indices_df, by="state" )


# state names should match in both dataframes.
m_level <- m_level%>%mutate(state = case_when(state == "Baden-Wurttemberg" ~ "Baden-Württemberg",
                                   state == "Bayern"  ~ "Bayern",
                                   state == "Berlin"  ~  "Berlin",
                                   state == "Brandenburg"  ~ "Brandenburg",
                                   state == "Bremen" ~  "Bremen",
                                   state == "Hamburg"  ~ "Hamburg",
                                   state == "Hessen"  ~ "Hessen",
                                   state == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
                                   state == "Niedersachsen"  ~ "Niedersachsen",
                                   state == "Nordrhein-Westfalen" ~ "Nordrhein-Westfalen",
                                   state == "Rheinland-Pfalz" ~  "Rheinland-Pfalz",
                                   state == "Saarland" ~ "Saarland",
                                   state == "Sachsen" ~ "Sachsen",
                                   state == "Sachsen-Anhalt" ~ "Sachsen-Anhalt" ,
                                   state == "Schleswig-Holstein" ~ "Schleswig-Holstein",
                                   state == "Thuringen" ~ "Thüringen" ))


# join state level predictors 
df1 <- left_join(x =survey_grouped, y = m_level, by="state" )
df2 <- left_join(x =census_df, y = m_level, by="state" )

#rename column names in survey 
df1 <- df1%>%rename(incidence_rate = "Incidence_Rate",
                          case_fatality_ratio = "Case.Fatality_Ratio",
                          sub_national_hdi = "Sub-national HDI",
                          health_index = "Health index",
                          income_index = "Income index",
                          educational_index ="Educational index")

#rename column names in census
df2 <- df2%>%rename(incidence_rate = "Incidence_Rate",
                          case_fatality_ratio = "Case.Fatality_Ratio",
                          sub_national_hdi = "Sub-national HDI",
                          health_index = "Health index",
                          income_index = "Income index",
                          educational_index ="Educational index")
#kick 0 n out
df2 <- filter(df2, n!=0)

# save them
write_csv(df1, file = "survey_final.csv")
write_csv(df2, file = "census_final.csv")








