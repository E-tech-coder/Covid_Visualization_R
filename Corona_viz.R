# This project is to visualize COVID-19 data from the first several weeks 
# of the outbreak to see at what point this virus became a global pandemic.

# Data source : 
# a publicly available data repository created by the Johns Hopkins University 
# Center for Systems Science and Engineering to consolidate this data from sources 
# like the WHO, the Centers for Disease Control and Prevention (CDC), and the Ministry of Health from multiple countries.

# Load the readr, ggplot2, and dplyr packages
library(readr)
library(ggplot2)
library(dplyr)

# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide
confirmed_cases <- read_csv("F:/Corona_Viz_R/coronavirus.csv")

confirmed_cases_country <- confirmed_cases_perday %>%
  group_by(country, date) %>%
  filter(type == "confirmed",date <= as.Date('2020-03-17')) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cum_cases = cumsum(cases))

confirmed_cases_worldwide <- confirmed_cases %>%
  group_by(date) %>%
  filter(type == "confirmed") %>%
  summarise(cases = sum(cases)) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(date <= as.Date('2020-03-17'))


# See the result

summary(confirmed_cases_worldwide)
str(confirmed_cases_worldwide)

#  Let's draw a line plot to visualize the confirmed cases worldwide.

ggplot(confirmed_cases_worldwide) +
  geom_line(mapping = aes(x = date, y = cum_cases)) +
  ylab("Cumulative confirmed cases")

# Early on in the outbreak, the COVID-19 cases were primarily centered in China. 
# Let's plot confirmed COVID-19 cases in China and the rest of the world separately to see if it gives us any insight.

confirmed_cases_china <-
  confirmed_cases_country %>%
  filter(country == "China") %>%
  group_by(date)

confirmed_cases_china_vs_world <- 
  confirmed_cases_worldwide %>%
  mutate(China.cases = confirmed_cases_china$cum_cases) %>%
  mutate(others.cases = cum_cases - confirmed_cases_china$cum_cases)

confirmed_cases_china_vs_world <- confirmed_cases_china_vs_world[-c(2)]

View(confirmed_cases_china_vs_world)

library(reshape2)

china_vs_world_melt <-
  melt(confirmed_cases_china_vs_world, id = "date")

china_vs_world_melt

plt_cum_confirmed_cases_china_vs_world<- ggplot(china_vs_world_melt)+
  geom_line(mapping = aes(x= date, y = value, color = variable, group = variable))+
  labs(title = "Number of Corona cases in China and worldwide", y = "Number of cases")
  
plt_cum_confirmed_cases_china_vs_world

# In February, the majority of cases were in China. 
# That changed in March when it really became a global outbreak: around March 14, 
# the total number of cases outside China overtook the cases inside China. 
# This was days after the WHO declared a pandemic.

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-02-13", "China reporting\nchange",
  "2020-03-11", "Pandemic\ndeclared"
) %>%
  mutate(date = as.Date(date))

# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 10000 on the y-axis
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = who_events$event), data = who_events , y = 1e5)
            
# Observe the number of cases in China after Feb 15th.

china_after_feb15 <- china_vs_world_melt %>%
  filter(variable == "China.cases", date >= as.Date("2020-02-15"))

glimpse(china_after_feb15)

View(china_vs_world_melt)

plt_china_after_feb15 <- ggplot(china_after_feb15, aes(x = date, y = value)) +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Number of confirmed Corona cases after Frb 15th in China")

plt_china_after_feb15

# From the plot above, the growth rate in China is slower than linear. 
# That's great news because it indicates China has at least somewhat contained the virus in late February and early March.

# How does the rest of the world compare to linear growth?

non_china_after_feb15 <- china_vs_world_melt %>%
  filter(date >= "2020-02-15", variable == "others.cases")


glimpse(non_china_after_feb15)

plt_not_china_trend_lin <- ggplot(data = non_china_after_feb15, aes(x= date, y = value)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Number of confirmed Corona cases after Frb 15th outside China", y = "Cumulative confirmed cases")

plt_not_china_trend_lin

# From the plot above, we can see a straight line does not fit well at all, and the rest of the world is growing much faster than linearly. 
# What if we added a logarithmic scale to the y-axis?

plt_not_china_trend_lin +
  scale_y_log10()

# With the logarithmic scale, we get a much closer fit to the data. 
# From a data science point of view, a good fit is great news. 
# Unfortunately, from a public health point of view, that means that cases of COVID-19 in 
# the rest of the world are growing at an exponential rate, which is terrible news.

# Plot which country was hit the most by Covid outside China by the Mid March

glimpse(confirmed_cases)
confirmed_cases_country

top_countries_by_total_cases_perday <- confirmed_cases_country %>%
  filter(country != "China",date <= as.Date('2020-03-17'), date > as.Date('2020-02-15'))

head(top_countries_by_total_cases_perday)

top_countries_by_total_cases <- confirmed_cases_country %>%
  filter(country != "China",date <= as.Date('2020-03-17'), date > as.Date('2020-02-15')) %>%
  group_by(country) %>%
  summarise(sum = sum(cases))%>%
  arrange(-sum)%>%
  top_n(7)

top_countries_by_total_cases

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, colored by country

top_seven <- top_countries_by_total_cases_perday %>% filter(country %in% pull(top_countries_by_total_cases,country))

glimpse(top_seven)

plt_top_countries <- ggplot(top_seven) + 
  geom_line(aes(x= date, y = cum_cases, color = country)) + labs(title = "Top seven countries hit the most by Covid by Mid March", y = "Confirmed cases per day") +
  geom_text(data = top_seven %>% filter(date == max(date)), aes(label = country,x = date + 1,y = cum_cases,color = country),size = 2)
                                                      
                                                            

plt_top_countries




