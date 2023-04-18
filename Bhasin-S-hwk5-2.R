install.packages("acs")
library(acs)

api.key.install(key='6021a8053424e534b078a5c62f340cba62da55bb')

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)
library(fixest)
library(modelsummary)

final.data <- read_tsv('data/output/acs_medicaid.txt')

#1 

final.data$share <- final.data$ins_direct/ final.data$adult_pop

share_avg <- aggregate(final.data$share, by = list(year = final.data$year), FUN = mean)

graph1 <- ggplot(data = share_avg, aes(x = year, y = x)) +
  geom_line() +
  labs(x = "Year", y = "Share of Individuals with Direct Purchase Health Insurance") +
  theme_bw()

graph1 

#2 

#There have been several policy changes leading to the reduction in direct purchase health insurance recently. In 2012 under the ACA, Medicaid coverage was expanded. This led to several states adopting medicaid expansion, which may the share of insured individuals with direct purchase health insurance since they are eligible for medicaid. Thus, they will enroll in medicaid over direct purchase health insurance. In addition, subsidies and tax credits from the ACA made insurance purchases through exchanges more affordable compared to direct purchase products.

#3 

final.data$share_medicaid <- final.data$ins_medicaid/ final.data$adult_pop

final.data <- final.data %>%
  filter(State != "Puerto Rico" & State != "District of Columbia")

question3 <- final.data %>% 
  group_by(year) %>% 
  summarize(mean = mean(share_medicaid)) %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  labs(
    x = "Year",
    y = "Share of Individuals with Medicaid", 
    title = "") 

question3

#4 
final.data$share_uninsured <- final.data$uninsured/ final.data$adult_pop

ins.plot.dat <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% 
  summarize(mean = mean(share_uninsured))

uninsurance.plot <- ggplot(data = ins.plot.dat, aes(x = year, y = mean, group = expand_ever)) +
  geom_line() +
  geom_point() +
  theme_bw() + 
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-Expansion", "Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype = "none") +
  labs(
    x = "Year",
    y = "", 
    title = ""
  )

uninsurance.plot      

#5 

question5 <- final.data %>%
  filter(!is.na(expand_ever), year %in% c(2012, 2015)) %>%
  group_by(expand_ever, year) %>%
  summarize(uninsured = mean(share_uninsured))

question5_table <- pivot_wider(question5, names_from = "year", values_from = "uninsured") %>%
  ungroup() %>%
  mutate(expand_ever = case_when(
    expand_ever == FALSE ~ "Non-Expansion", 
    expand_ever == TRUE ~ "Expansion")
  ) %>%
  rename(Group = expand_ever)

question5_table

#6 

question6 <-final.data%>%
  filter(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                      "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                      "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                      "New Hampshire", "New Jersey", "New Mexico", 
                      "New York", "North Dakota", 
                      "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia",
                      "Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                      "North Carolina", "South Carolina", "South Dakota", "Tennessee",
                      "Texas", "Wisconsin", "Wyoming"))%>%
  mutate(group=ifelse(State %in% c("Arizona", "Arkansas", "California", "Colorado", "Delaware", 
                                   "District of Columbia", "Connecticut", "Hawaii", "Illinois", "Iowa", "Kentucky", 
                                   "Maryland", "Massachusetts", "Michigan", "Minnesota", "Nevada", 
                                   "New Hampshire", "New Jersey", "New Mexico","New York", "North Dakota", 
                                   "Ohio", "Oregon", "Rhode Island", "Vermont", "Washington", "West Virginia"), 
                      "Treatment", "Control"))%>%
  mutate(post=year>=2014) %>%
  mutate(share_uninsured=uninsured/adult_pop)%>%
  mutate(treat=post*expand_ever)

Q6 <- lm(share_uninsured ~post+group+post*group, data=question6)
modelsummary(Q6)

#7

question7_data <- final.data %>%
  mutate(treat = ifelse(year >= expand_year & !is.na(expand_year), 1, 0))

question7 <- feols(perc_unins ~ treat | State + year, data = question7_data)

modelsummary(question7)

#8

question8_data <- final.data %>%
  mutate(treat = case_when(
    year >= expand_year & !is.na(expand_year) ~ 1, 
    is.na(expand_year) ~ 0,
    year < expand_year & !is.na(expand_year) ~ 0)
  )

question8 <- feols(share_uninsured ~ treat | State + year, data = question8_data)

modelsummary(question8)

#My results are slightly different but not my much. This may be because the percent of uninsured individuals may have not drastically changed due to the expansion. 

#9

question9 <- feols(share_uninsured~i(year, expand_ever, ref=2013) | State +year, 
                  cluster = ~State, 
                  data=question7_data)

event.plot <- iplot(question9,
                    xlab = "Time Until Treatment",
                    main = "Event Study")

#10 

question10_a <- question8_data %>%
  mutate(time_to_treat =ifelse(expand_ever== TRUE, year -expand_year, -1),
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))

question10 <- feols(share_uninsured~i(time_to_treat, expand_ever, ref=-1) | State +year, 
                   cluster = ~State, 
                   data=question10_a)

event.plot2 <- iplot(question10,
                     xlab = "Time Until Treatment",
                     main = "Event Study")

save.image("Hwk5_workspace.Rdata")
