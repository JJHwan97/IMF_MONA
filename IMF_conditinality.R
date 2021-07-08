url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Combined.xlsx"
mona <- rio::import(file = url,which = 1) %>% 
  glimpse()

temp2 <- mona$`Country Code` %>% countrycode(., origin = 'imf', destination = 'iso3n')
temp1 <- mona$`Country Code`
temp2[temp1 == 965] <- 890

mona$`ISO3N` <- temp2

mona.clean <- mona %>% dplyr::select(`Arrangement Number`, ISO3N, `Key Code`, `Economic Code`, `Approval Year`, `Initial End Year`)

for (i in 2000:2021){
  j <- i%>% as.character()
  mona.clean$j <- 0
  mona.clean$j[mona.clean$`Approval Year` <= i & i <= mona.clean$`Initial End Year`] <- 1
  colnames(mona.clean)[colnames(mona.clean)%>%length()] <- j
}

mona.clean <- mona.clean %>% dplyr::select(!c(`Approval Year`, `Initial End Year`))

mona.clean <- mona.clean %>% 
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

covid19.foriegn.summarize <- dplyr::select(covid19.foriegn, -c(iso_code, date, continent,tests_units))

covid19.foriegn.summarize.sum <- covid19.foriegn.summarize %>% group_by(location, weeknum, year) %>% summarise_all(list(sum), na.rm = TRUE)

temp.name <- covid19.foriegn[,c("iso_code", "continent","location")] %>% unique()

covid19 <- left_join(covid19.foriegn.summarize.sum, temp.name)

covid19_2020 <- covid19[covid19$year == "2020",]

covid19_2021 <- covid19[covid19$year == "2021",]

covid19_2021 <- covid19_2021 %>% filter(weeknum != 53)
