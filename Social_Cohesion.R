library(tidyr)
library(dplyr)
library(tidybayes)
library(bayesplot)
library(brms)
library(lubridate)
library(ggplot2)
library(readr)
library(insight)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggmcmc)
library(ggpubr)
library(broom)
library(rgdal)
library(here)

#load survey data
survey_final <- read_csv("survey_final.csv")

#load census data
census_final <- read.csv("census_final.csv")

survey_final <- survey_final%>%rename(more = `eher mehr`,
                      less = `eher weniger`,
                      nc = `kein großer Unterschied`)

#create comp
survey_final <- survey_final%>%mutate(cohesion_c = cbind(more, less, nc))
survey_final <- filter(survey_final, education >= 0)


# BUILD MODEL
# Model_A
# get overview of prior structure
get_prior(cohesion_c|trials(n) ~ 0 + education + gender + 
            age  + income_index + 
            incidence_rate + age:gender:education +
          (0 +  education + gender + age | state), survey_final, family=multinomial())

# define the priors
prior_a <- c(prior(normal(0, 1.5), class = b, dpar=muless),
            prior(normal(0, 1.5), class = b, dpar=munc),
            prior(normal(0, 1.5), class = Intercept, dpar=muless),
            prior(normal(0, 1.5), class = Intercept, dpar=munc),
            prior(lkj(2), class = L),
            prior(normal(0,1), class = sd, dpar=munc),
            prior(normal(0,1), class = sd, dpar=muless))


# write the formula
formula_a <- brmsformula(cohesion_c|trials(n) ~ 1 + education + gender + 
                           age  + sub_national_hdi + 
                           incidence_rate + age:gender:education +
                           (1 +  education + gender + 
                               age  | state), family=multinomial())

# ensemble the model
model_a <- brm(formula_a, survey_final,list(adapt_delta = 0.9), chains = 4, iter=1000, cores=4, prior = prior_a)

# model summary
model_a




# Hypothesis
plot(hypothesis(model_a, "muless_incidence_rate  > 0 "))




# plot conditional effects
conditional_effects(model_a, categorical = T)

# posterior predictive checks for eher mehr
posterior_p <- posterior_predict(model_a)
posterior_p
yrep1 <- posterior_p[,,1]
y1 <- survey_final$more
y_bar1 <-ppc_bars(y1, yrep1[1:500, ]) + ggtitle("More Cohesion")
y_bar1

# posterior predictice checks for eher weniger
yrep2 <- posterior_p[,,2]
y2 <- survey_final$less
y_bar2 <- ppc_bars(y2, yrep2[1:500, ]) + ggtitle("Less Cohesion")
y_bar2

ppc_stat_2d(y2, yrep2[1:1000, ])
ppc_bars_grouped(y2, yrep2[1:1000, ], group=survey_final$state)
ppc_error_hist(y1, yrep1[1:10, ])

# posterior predictice checks for kgu
yrep3 <- posterior_p[,,3]
y3 <- survey_final$nc
y_bar3 <- ppc_bars(y3, yrep3[1:500, ]) + ggtitle("No Change on Cohesion")
y_bar3

y_bar1


ppc_ecdf_overlay(y1, yrep1[1:100, ])



# modeltransformed
modeltransformed <- ggs(model_a)
plot_model(model_a, type="re")

# MU_less_EDUCATION
mu2_education <- ggplot(filter(modeltransformed,
              Parameter == "b_muless_education", 
              Iteration > 250),
       aes(x = value))+
  geom_density(fill  = "lightblue", 
               alpha = .5)+
  geom_vline(xintercept = 0, 
             col  = "black",
             size = 1)+
  scale_x_continuous(name   = "Value",
                     limits = c(-2, 2)) + 
  geom_vline(xintercept = summary(model_a)$fixed[1,3:4],
             col = "navy",
             linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Education for Eher Weniger")

mu2_education

# MU_2_GENDER
mu2_gender <- ggplot(filter(modeltransformed,
                               Parameter == "b_muless_gendermale", 
                               Iteration > 250),
                        aes(x = value))+
  geom_density(fill  = "lightblue", 
               alpha = .5)+
  geom_vline(xintercept = 0, 
             col  = "black",
             size = 1)+
  scale_x_continuous(name   = "Value",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(model_a)$fixed[2,3:4],
             col = "navy",
             linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Gender for Eher Weniger")

mu2_gender

# MU_2_AGE
mu2_age <- ggplot(filter(modeltransformed,
                            Parameter == "b_mu2_age", 
                            Iteration > 500),
                     aes(x = value))+
  geom_density(fill  = "lightblue", 
               alpha = .5)+
  geom_vline(xintercept = 0, 
             col  = "black",
             size = 1)+
  scale_x_continuous(name   = "Value",
                     limits = c(-.5, .5)) + 
  geom_vline(xintercept = summary(model_b)$fixed[5,3:4],
             col = "navy",
             linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Age for Eher Weniger")



ggplot(filter(modeltransformed,
              Parameter == "b_muless_incidence_rate", 
              Iteration > 500),
       aes(x = value))+
  geom_density(fill  = "lightblue", 
               alpha = .5)+
  geom_vline(xintercept = 0, 
             col  = "black",
             size = 1)+
  scale_x_continuous(name   = "Value",
                     limits = c(-.01, .01)) + 
  geom_vline(xintercept = summary(model_b)$fixed[12,3:4],
             col = "navy",
             linetype = 2) +
  theme_light() +
  labs(title = "Posterior Density of Age for Eher Weniger")


model_b

# Poststratification
census_final <- filter(census_final, n != 0)
predicted <- add_predicted_draws(model_a, newdata = census_final, n=500, allow_new_levels=T)
summary <- predicted%>%group_by(state, .category)%>%summarise(sum(.prediction)/sum(n))
summary_g <- predicted%>%group_by(state, gender, age, .category)%>%
  summarise(sum(.prediction)/sum(n))%>%
  filter(.category=="more")%>%spread(gender,`sum(.prediction)/sum(n)`)%>%mutate(diff = female -male)



# MRP 
summary <-predicted%>%group_by(state, .category, n)%>%median_hdci(.prediction)%>%summarise(est = sum(.prediction)/sum(n))
# MRP MORE
summary_m <- summary%>%filter(.category =="more")%>%mutate(model = "MRP")
#MRP less
summary_l <- predicted%>%group_by(state, .category)%>%summarise(est = sum(.prediction)/sum(n))%>%filter(.category =="less")%>%mutate(model = "MRP")



# aggregated More
agg_m <- survey_final%>%group_by(state)%>%summarise(est = sum(more)/sum(n))%>%mutate(model = "Aggregated")
# aggregated less
agg_l <- survey_final%>%group_by(state)%>%summarise(est = sum(less)/sum(n))%>%mutate(model = "Aggregated")

#comparison tables
#MORE
table_more <- bind_cols(x=agg_m, y=summary_m)
# LESS
table_less <- bind_cols(x=agg_l, y=summary_l)

# Plot For More
bind_rows(agg_m,summary_m) %>%
group_by(model) %>%
  arrange(est, .by_group=TRUE)%>%ggplot(aes(x = est, y=forcats::fct_inorder(state), color=model)) + 
  geom_point() + 
  theme_linedraw() + theme(legend.position = 'left') +  
  ylab('Federal States') +xlab("More Social Cohesion")

# Plot For less
bind_rows(agg_l,summary_l) %>%
  group_by(model) %>%
  arrange(est, .by_group=TRUE)%>%ggplot(aes(x = est, y=forcats::fct_inorder(state), color=model)) + 
  geom_point() + 
  theme_linedraw() + theme(legend.position = 'left') +  
  ylab('Federal States') +xlab("Less Social Cohesion")







# Age 
summary_age <- predicted%>%group_by(state, age, .category)%>%
  summarise(sum(.prediction)/sum(n))%>%
  filter(.category=="more")%>%rename(est = `sum(.prediction)/sum(n)`)
  
ggplot(summary_age, aes(x=age, y=est, colour=state)) +
  geom_point() +
  geom_line()

# EDUCATION
summary_ed <- predicted%>%group_by(state, age, gender, education, .category)%>%
  summarise(sum(.prediction)/sum(n))%>%
  filter(.category=="more")%>%rename(est = `sum(.prediction)/sum(n)`)

ggplot(summary_ed, aes(x=age, y=est, colour=factor(education))) +
  geom_smooth(method = "lm") +
  facet_wrap(~gender) +
  geom_point()

summary_ed_l <- predicted%>%group_by(state, age, gender, education, .category)%>%
  summarise(sum(.prediction)/sum(n))%>%
  filter(.category=="less")%>%rename(est = `sum(.prediction)/sum(n)`)


ggplot(summary_ed_l, aes(x=age, y=est, colour=factor(education))) +
  geom_smooth(method = "lm") +
  facet_wrap(~gender) +
  geom_point()



















# GERMAN MAP
germany <- readOGR(here(layer = "gadm36_DEU_1.shp"), use_iconv = TRUE, encoding = "UTF-8")
#plot(germany)


#test <- filter(summary, .category =="more")


germany_states <- germany@data$NAME_1
germany@data$germany_states <- germany_states 

elections_data_m <- summary_m %>% 
  filter(state %in% germany_states) 

#elections_data_m <- elections_data_m%>%rename(est = `sum(.prediction)/sum(n)`)

germany@data <- germany@data %>% 
  left_join(elections_data_m,by = c("germany_states" = "state")) 

#germany@data$germany_states

#elections_data$state

germany@data <- germany@data%>%mutate(nr = case_when(germany_states == "Baden-Württemberg"~ "0",
                                     germany_states =="Bayern" ~ "1",             
                                     germany_states =="Berlin"~ "2",              
                                     germany_states =="Brandenburg" ~ "3" ,          
                                     germany_states =="Bremen" ~ "4",         
                                     germany_states =="Hamburg"~ "5",           
                                     germany_states =="Hessen" ~ "6",             
                                     germany_states =="Mecklenburg-Vorpommern"~ "7",
                                     germany_states =="Niedersachsen" ~ "8",  
                                     germany_states =="Nordrhein-Westfalen"~ "9",
                                     germany_states =="Rheinland-Pfalz" ~ "10",    
                                     germany_states =="Saarland"~ "11",              
                                     germany_states =="Sachsen" ~ "12",
                                     germany_states =="Sachsen-Anhalt" ~ "13",
                                     germany_states =="Schleswig-Holstein"~ "14",
                                     germany_states =="Thüringen" ~ "15" ))


rownames(germany@data) <- germany@data$germany_states 

germany_dataframe <- tidy(germany) %>% 
  left_join(., germany@data, by = c("id" = "nr")) 

#library(mapproj)
germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  # we need to add this filter so that the Berlin 
  # and Bremen results can be mapped and 
  # distinguished from the Brandeburg and Niedersachsen results, respectively
  geom_polygon(aes(fill=est), color = "white", 
               data =filter(germany_dataframe, !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=est), color = "white", 
               data =filter(germany_dataframe, NAME_1 %in%  c("Berlin", "Bremen"))) + 
  theme_minimal() +
  coord_map() +
  labs(title = "Percentage of People Who Perceived More Cohesion During Pandemic", 
       subtitle = "Share of the 'More' Category (%)", 
       caption = "Model Based Estimation" ) + 
  scale_fill_gradient2(
    low = "#FBEFEF",
    mid= "#F78181",
    high = "#E3000F",
    midpoint = 0.30,
    name = "Share of People Percevied More Social Cohesion (%)",
    limits = c(0.26,0.42), 
    breaks = c(0.25, 0.30, 0.35, 0.40),
    guide = guide_legend)






































#-------------------------------------------------------------------------------
# creation of a common theme for all plots
common_theme <- theme(title = element_text(family = "Cambria", size = 12, hjust = 0.5),
                      plot.title = element_text(family = "Cambria", 
                                                color = "#22211d", 
                                                size = 20, 
                                                hjust = 0),
                      plot.subtitle = element_text(family = "Cambria", 
                                                   face = "italic", 
                                                   color = "#22211d", 
                                                   size = 14, 
                                                   hjust = 0), 
                      plot.caption = element_text(family = "Cambria", 
                                                  color = "#22211d", 
                                                  size = 10, 
                                                  hjust = 0),
                      plot.background = element_rect(fill = "#f5f5f2", 
                                                     color = NA), 
                      panel.background = element_rect(fill = "#f5f5f2", 
                                                      color = NA),
                      legend.background = element_rect(fill = "#f5f5f2", 
                                                       color = NA),
                      legend.text = element_text(family = "Cambria", 
                                                 size = 10),
                      axis.line = element_blank(), 
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(), 
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      legend.position = "right")

# create a common legend design - given that the dependent variable is continous, we'll use guide_colorbar
guide_legend <- guide_colorbar(
  direction = "horizontal",
  barheight = unit(2, units = "mm"),
  barwidth = unit(50, units = "mm"),
  draw.ulim = F,
  title.position = 'top',
  title.hjust = 0.5,
  label.hjust = 0.5)



# pLOT
germany_dataframe %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=est), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            !NAME_1 %in% c("Berlin", "Bremen"))) + 
  geom_polygon(aes(fill=est), 
               color = "#7F7F7F", 
               data =filter(germany_dataframe, 
                            NAME_1 %in%  c("Berlin", "Bremen"))) +
  coord_map() +
  labs(title = "German Federal Election 2017", 
       subtitle = "Share of the CDU/CSU Vote (%)", 
       caption = "Source: Federal Returning Office" ) + 
  common_theme +
  scale_fill_gradient2(
    low = "#F2F2F2",
    mid= "#A4A4A4",
    high = "#000000",
    midpoint = 0.30,
    name = "Share of Vote (%)",
    limits = c(0.25,0.42), 
    breaks = c(0.25, 0.28, 0.33, 0.36, 0.39, 0.42),
    guide = guide_legend) 





