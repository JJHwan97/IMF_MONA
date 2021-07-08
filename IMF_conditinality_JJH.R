library(countrycode)

url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Combined.xlsx"
mona <- rio::import(file = url,which = 1) %>% 
  glimpse()

temp2 <- mona$`Country Code` %>% countrycode(., origin = 'imf', destination = 'iso3n')
temp1 <- mona$`Country Code`
temp2[temp1 == 965] <- 890

mona$`ISO3N` <- temp2

mona.clean <- mona %>% dplyr::select(`Arrangement Number`, ISO3N, `Key Code`, `Economic Code`, `Approval Year`, `Initial End Year`)

for (i in 2010:2021){
  j <- i%>% as.character()
  mona.clean$j <- 0
  mona.clean$j[mona.clean$`Approval Year` <= i & i <= mona.clean$`Initial End Year`] <- 1
  colnames(mona.clean)[colnames(mona.clean)%>%length()] <- j
}

length <- mona.clean %>% colnames() %>% length()

temp <- mona.clean[,7:length]

mona.clean$sum <- rowSums(temp) %>% as.data.frame()

mona.clean.10year <- mona.clean %>% filter(sum != 0) %>% .[,-c(5:length)]
  
mona.clean <- mona.clean %>% dplyr::select(!c(`Approval Year`, `Initial End Year`))

mona.clean <- mona.clean %>% 
  dplyr::select(!c(`sum`)) %>%
  pivot_longer(!c("Arrangement Number","ISO3N","Key Code","Economic Code"), names_to = "year", values_to = "under") %>%
  filter(under != 0)

mona.clean$year <- mona.clean$year %>% as.numeric()

mona.clean.num <- mona.clean %>%
  dplyr::select(!c(`Arrangement Number`, `Key Code`, `Economic Code`)) %>%
  group_by(ISO3N, year)%>%
  summarize(condition_num = n())
mona.clean.num$condition_num <- mona.clean.num$condition_num %>% as.numeric()

mona.clean.key <- mona.clean %>%
  group_by(ISO3N, year, `Key Code`)%>%
  summarize(condition_num = n())

mona.clean.economic <- mona.clean %>%
  group_by(ISO3N, year, `Economic Code`)%>%
  summarize(condition_num = n())
mona.clean.economic$`Economic Code` <- mona.clean.economic$`Economic Code` %>% as.character()

mona.clean.10year.num <- mona.clean.10year %>%
  dplyr::select(!c(`Arrangement Number`, `Key Code`, `Economic Code`)) %>%
  group_by(ISO3N)%>%
  summarize(condition_num = sum(sum))

mona.clean.10year.key <- mona.clean.10year %>%
  group_by(ISO3N, `Key Code`)%>%
  summarize(Key_num = sum(sum))

mona.clean.10year.economic <- mona.clean.10year %>%
  group_by(ISO3N, `Economic Code`)%>%
  summarize(Economic_num = sum(sum))

mona.clean.num %>% filter(ISO3N == 32) %>%
  ggplot(aes(x=year, y=condition_num)) +
  geom_line() +
  expand_limits(y = 0) +
  labs(xlab = "Year", ylab = "Conditionality Number", title = "Argentina") +
  theme_jhp()

mona.clean.key %>% filter(ISO3N == 32) %>%
  ggplot(aes(x=year, y=condition_num, group = `Key Code`, colour = `Key Code`)) +
  geom_line() +
  expand_limits(y = 0) +
  labs(xlab = "Year", ylab = "Economic conditionality Number", title = "Argentina") +
  theme_jhp()

mona.clean.economic %>% filter(ISO3N == 32) %>%
  ggplot(aes(x=year, y=condition_num, group = `Economic Code`, colour = `Economic Code`)) +
  geom_line() +
  expand_limits(y = 0) +
  labs(xlab = "Year", ylab = "Specific conditionality Number", title = "Argentina") +
  theme_jhp()
  
covid19.foriegn <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

covid19.foriegn$date <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")

covid19.foriegn$year <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%Y")

covid19.foriegn$weeknum <-  format(covid19.foriegn$date, format = "%V")

date.yesterday <- Sys.Date() - 1

covid19.foriegn <- covid19.foriegn %>% filter(date == date.yesterday)

covid19.foriegn.death <- covid19.foriegn %>% dplyr::select(c("iso_code", "total_deaths_per_million", 
                                                             "population_density", "gdp_per_capita", "life_expectancy"))

covid19.foriegn.death$ISO3N <- covid19.foriegn.death$iso_code %>% countrycode(., origin = 'iso3c', destination = 'iso3n')
covid19.foriegn.death$ln.gdp <- log(covid19.foriegn.death$gdp_per_capita)

covid19.foriegn.death <- drop_na(covid19.foriegn.death)

final <- left_join(covid19.foriegn.death, mona.clean.10year.num)
# final <- left_join(final, mona.clean.10year.economic)
# final <- left_join(final, mona.clean.10year.key)


final[final %>% is.na()] <- 0

final.fake <- final %>% filter(condition_num != 0)

l1 <- final.fake %>% lm(formula = total_deaths_per_million ~ condition_num)
l2 <- final.fake %>% lm(formula = total_deaths_per_million ~ condition_num + ln.gdp)
l3 <- final.fake %>% lm(formula = total_deaths_per_million ~ condition_num + population_density + ln.gdp + life_expectancy )

library(stargazer)
stargazer(l1, l2, l3, type="text", title="Results", align=TRUE)

