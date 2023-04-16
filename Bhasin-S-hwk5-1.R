install.packages("acs")
library(acs)

api.key.install(key='6021a8053424e534b078a5c62f340cba62da55bb')

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)
library(fixest)

#1 

final.data$share <- final.data$ins_direct/ final.data$adult_pop

share_avg <- aggregate(final.data$share, by = list(year = final.data$year), FUN = mean)

graph1 <- ggplot(data = share_avg, aes(x = year, y = x)) +
  geom_line() +
  labs(x = "Year", y = "Share of Insured Individuals with Direct Purchase Health Insurance") +
  theme_bw()

graph1 

#2 

#There have been several policy changes leading to the reduction in direct purchase health insurance recently. In 2012 under the ACA, Medicaid coverage was expanded. This led to several states adopting medicaid expansion, which may the share of insured individuals with direct purchase health insurance since they are eligible for medicaid. Thus, they will enroll in medicaid over direct purchase health insurance. In addition, subsidies and tax credits from the ACA made insurance purchases through exchanges more affordable compared to direct purchase products.

#3 

final.data$share_medicaid <- final.data$ins_medicaid/ final.data$adult_pop

share_avg_medicaid <- aggregate(final.data$share_medicaid, by = list(year = final.data$year), FUN = mean)

graph3 <- ggplot(data = share_avg_medicaid, aes(x = year, y = x)) +
  geom_line() +
  labs(x = "Year", y = "Share of Individuals with Medicaid") +
  theme_bw()

graph3


#4 
not_expanded <- kff.final[kff.final$expanded == FALSE, ]

kff.final$date_adopted <- as.Date(kff.final$date_adopted)

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2014-12-31")
subset_rows <- kff.final[kff.final$date_adopted >= start_date & kff.final$date_adopted <= end_date, ]

expanded_2014 <- subset_rows

clean_rows <- subset_rows[complete.cases(subset_rows), ]

clean_2014_expanded <- clean_rows

expanded <- c('Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Hawaii', 'Illinois', 'Iowa', 'Kentucky', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Dakota', 'Ohio', 'Oregon', 'Rhode Island', 'Vermont', 'Washington', 'West Virginia')
not_expanded <- c('Alabama', 'Florida', 'Georgia', 'Kansas')

final.data <- final.data[final.data$State %in% c(expanded, not_expanded),]

df_grouped <- aggregate(final.data$uninsured, by=list(final.data$year, final.data$State), mean)

df_expanded <- subset(df_grouped, Group.2 %in% expanded)
df_not_expanded <- subset(df_grouped, Group.2 %in% not_expanded)

df_expanded_grouped <- aggregate(df_expanded$x, by=list(df_expanded$Group.1), mean)
df_not_expanded_grouped <- aggregate(df_not_expanded$x, by=list(df_not_expanded$Group.1), mean)

final.data$Group <- ifelse(final.data$State %in% expanded, "Expanded", "Not Expanded")

final.data$uninsured_share <- final.data$uninsured / final.data$adult_pop

df_grouped <- final.data %>% 
  group_by(Group, year) %>% 
  summarize(Avg_share_Uninsured = mean(uninsured_share))

graph4 <- ggplot(df_grouped, aes(x = year, y = Avg_share_Uninsured, color = Group, group = Group)) +
  geom_line() +
  labs(color = "Group", x = "Year", y = "Average Number of Uninsured") +
  theme_bw()
graph4 

#5 

question5 <- final.data %>% filter(year %in% c(2012, 2015)) %>% 
  group_by(expand_ever, year) %>% 
  summarize(uninsured_share = mean(uninsured_share)) %>%
  spread(year, uninsured_share)%>% 
  mutate(expand_ever = if_else(expand_ever == FALSE, "States that did not expand Medicaid", "States that expanded"))
question5 

#6 

final.data$treatment <- ifelse(final.data$expand_ever == TRUE, 1,0)

final.data$time <- ifelse(final.data$year >= 2014, 1, 0)

question6 <- lm(uninsured_share ~ treatment + time + treatment*time, data = final.data)

summary(question6)

#7 

question7 <- feols(uninsured_share~i(year, expand_ever, ref= 2013) | State + year,
               cluster=~State,
               data=final.data)

summary(question7)

#8 

ATE_data <- final.data %>% 
  filter(!is.na(expand_ever) & State != "South Dakota") %>%
  mutate(time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year), 
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))


question8<- feols(uninsured_share~i(time_to_treat, expand_ever, ref=-1) | State + year,
              cluster=~State,
              data=ATE_data)

question8

#My results are not different. This may be because the percent of uninsured individuals may have not drastically changed due to the expansion. 

#9 

final.data$expansion_dummy <- ifelse(final.data$year >= 2014 & final.data$expand == TRUE, 1, 0)

model <- feols(uninsured_share ~ expansion_dummy + factor(year) + factor(State), data = final.data)

event_data <- final.data
event_years <- intersect(unique(final.data$year), unique(event_data$year))
event_data$year <- rep(event_years, each = nrow(event_data) / length(event_years))
event_data$event_value <- predict(model, final.data = event_data, type = "response")

event_plot <- ggplot(event_data, aes(x = year, y = event_value, group = expansion_dummy)) +
  geom_line(aes(color = factor(expansion_dummy))) +
  scale_color_discrete(name = "Expansion", labels = c("Not Expanded", "Expanded")) +
  xlab("Year") +
  ylab("Uninsurance Rate") +
  ggtitle("Effects of Medicaid Expansion")

event_plot

#10 

event_data2 <- expand.grid(year = 2010:2018, state = unique(final.data$state))
event_data2 <- left_join(event_data2, final.data, by = c("year", "state"))
event_data2$expansion[is.na(event_data2$expansion)] <- 0
event_data2 <- event_data2 %>% 
  group_by(year) %>%
  summarize(uninsured_share = mean(uninsured_share),
            expansion_dummy = max(expansion))

event_plot2 <- ggplot(event_data2, aes(x = year, y = uninsured_share)) +
  geom_line(aes(color = factor(expansion_dummy))) +
  scale_color_discrete(name = "Expansion", labels = c("Not Expanded", "Expanded")) +
  xlab("Year") +
  ylab("Uninsurance Rate") +
  ggtitle("Effects of Medicaid Expansion")

event_plot2



save.image("Hwk5_workspace.Rdata")
