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

#number of program
length <- mona.clean %>% colnames() %>% length()

temp <- mona.clean[,7:length]

mona.clean$sum <- rowSums(temp) %>% as.data.frame()

mona.clean.10year <- mona.clean %>% filter(sum != 0)

mona.clean.10year$sum <-1

mona.clean.num.conditionality <- mona.clean.10year %>% 
                                group_by(ISO3N, `Economic Code`)%>%
                                summarize(condition_num = sum(sum)) 
mona.clean.num.conditionality$condition_num <- 1 
mona.clean.num.conditionality <- mona.clean.num.conditionality%>%
                                group_by(ISO3N) %>%
                                summarize(final_sum = sum(condition_num)) %>%
                                drop_na()

mona.clean.num.programs <- mona.clean.10year %>% 
  group_by(ISO3N, `Arrangement Number`, `Key Code`)%>%
  summarize(condition_num = sum(sum)) %>% 
  group_by(ISO3N, `Key Code`) %>%
  summarize(final_sum = sum(condition_num)) %>%
  drop_na() %>%
  pivot_wider(names_from = `Key Code`, values_from = `final_sum`, values_fill = 0)

mona.clean.num.programs$totalprogram <- rowSums(mona.clean.num.programs[,2:5])

mona.clean.under.year <- mona.clean.10year %>% 
  dplyr::select(!c(`Arrangement Number`, `Key Code`, `Economic Code`, `Approval Year`, `Initial End Year`,sum)) %>%
  group_by(ISO3N)%>%
  summarise(across(everything(), sum))

time <- mona.clean.under.year[, 2:length(colnames(mona.clean.under.year))]
time[time >0] <- 1
time <- rowSums(time)

mona.clean.under.year <- cbind(mona.clean.under.year$ISO3N,time) %>% as.data.frame()
colnames(mona.clean.under.year)[1]<-"ISO3N"

# mona.clean <- mona.clean %>% dplyr::select(!c(`Approval Year`, `Initial End Year`))
# 
# mona.clean <- mona.clean %>% 
#   dplyr::select(!c(`sum`)) %>%
#   pivot_longer(!c("Arrangement Number","ISO3N","Key Code","Economic Code"), names_to = "year", values_to = "under") %>%
#   filter(under != 0)
# 
# mona.clean$year <- mona.clean$year %>% as.numeric()
# 
# mona.clean.num <- mona.clean %>%
#   dplyr::select(!c(`Arrangement Number`, `Key Code`, `Economic Code`)) %>%
#   group_by(ISO3N, year)%>%
#   summarize(condition_num = n())
# mona.clean.num$condition_num <- mona.clean.num$condition_num %>% as.numeric()
# 
# mona.clean.key <- mona.clean %>%
#   group_by(ISO3N, year, `Key Code`)%>%
#   summarize(condition_num = n())
# 
# mona.clean.economic <- mona.clean %>%
#   group_by(ISO3N, year, `Economic Code`)%>%
#   summarize(condition_num = n())
# mona.clean.economic$`Economic Code` <- mona.clean.economic$`Economic Code` %>% as.character()
# 
# mona.clean.10year.num <- mona.clean.10year %>%
#   dplyr::select(!c(`Arrangement Number`, `Key Code`, `Economic Code`)) %>%
#   group_by(ISO3N)%>%
#   summarize(condition_num = sum(sum))
# 
# mona.clean.10year.key <- mona.clean.10year %>%
#   group_by(ISO3N, `Key Code`)%>%
#   summarize(Key_num = sum(sum))
# 
# mona.clean.10year.key <- mona.clean.10year.key %>% pivot_wider(names_from = `Key Code`, values_from = `Key_num`, values_fill = 0)
# 
# mona.clean.10year.economic <- mona.clean.10year %>%
#   group_by(ISO3N, `Economic Code`)%>%
#   summarize(Economic_num = sum(sum))

# mona.clean.num.conditionality %>% filter(ISO3N == 32) %>%
#   ggplot(aes(x=year, y=condition_num)) +
#   geom_line() +
#   expand_limits(y = 0) +
#   labs(xlab = "Year", ylab = "Conditionality Number", title = "Argentina") +
#   theme_jhp()
# 
# mona.clean.key %>% filter(ISO3N == 32) %>%
#   ggplot(aes(x=year, y=condition_num, group = `Key Code`, colour = `Key Code`)) +
#   geom_line() +
#   expand_limits(y = 0) +
#   labs(xlab = "Year", ylab = "Economic conditionality Number", title = "Argentina") +
#   theme_jhp()
# 
# mona.clean.economic %>% filter(ISO3N == 32) %>%
#   ggplot(aes(x=year, y=condition_num, group = `Economic Code`, colour = `Economic Code`)) +
#   geom_line() +
#   expand_limits(y = 0) +
#   labs(xlab = "Year", ylab = "Specific conditionality Number", title = "Argentina") +
#   theme_jhp()
  
covid19.foriegn <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

covid19.foriegn$date <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")

covid19.foriegn$year <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%Y")

covid19.foriegn$weeknum <-  format(covid19.foriegn$date, format = "%V")

date.yesterday <- Sys.Date() - 2

covid19.foriegn <- covid19.foriegn %>% filter(date == date.yesterday)

covid19.foriegn.death <- covid19.foriegn %>% dplyr::select(c("iso_code", "total_deaths_per_million", 
                                                             "population_density", "gdp_per_capita", "life_expectancy",
                                                             "aged_65_older", "continent"))

covid19.foriegn.death$deathofcase <- covid19.foriegn$total_deaths_per_million/covid19.foriegn$total_cases_per_million

covid19.foriegn.death$ISO3N <- covid19.foriegn.death$iso_code %>% countrycode(., origin = 'iso3c', destination = 'iso3n')
covid19.foriegn.death$ln.gdp <- log(covid19.foriegn.death$gdp_per_capita)

covid19.foriegn.death <- drop_na(covid19.foriegn.death)

for (i in 1:length(covid19.foriegn.death$continent %>% unique())){
  j <- covid19.foriegn.death$continent %>% unique() %>% .[i]
  covid19.foriegn.death$j <- 0
  colnames(covid19.foriegn.death)[length(covid19.foriegn.death %>% colnames() %>% unique())] <- j
  covid19.foriegn.death[covid19.foriegn.death$continent == j , length(covid19.foriegn.death %>% colnames() %>% unique())] <- 1
  }

url <- "http://www.systemicpeace.org/inscr/p5v2018.xls"
dem5 <- rio::import(file = url,which = 1) %>% 
  glimpse()

dem5 <- dem5 %>% dplyr::select(ccode, year, polity)
dem5$ISO3N <- dem5$ccode %>% countrycode(., origin = 'p4n', destination = 'iso3n')
dem5.2018 <- dem5 %>% filter(year == 2018)
dem5.2018 <- dem5.2018[,c(4,3)]

dem5.2018 <- dem5.2018 %>% dplyr::filter(polity > -10.5)

library(wbstats)
urban <- wb_data("sp.urb.totl.in.zs")
urban <- urban %>% filter(date == 2020)
urban$ISO3N <- urban$iso3c %>% countrycode(.,origin = "iso3c", destination = "iso3n")
urban <- urban[,c("ISO3N","SP.URB.TOTL.IN.ZS")]
colnames(urban)[2]<-"urban_pop"

oil <- wb_data("NY.GDP.PETR.RT.ZS")
oil <- oil %>% filter(date == 2019)
oil$ISO3N <- oil$iso3c %>% countrycode(.,origin = "iso3c", destination = "iso3n")
oil <- oil[,c("ISO3N","NY.GDP.PETR.RT.ZS")]
colnames(oil)[2]<-"oil"

library(vdemdata)
vdem.data <- vdem
vdem.data <- vdem.data %>% filter(year == 2020)

vdem.data <- vdem.data %>% dplyr::select(year, country_id, v2x_egaldem)

vdem.data$ISO3N <- vdem.data$country_id %>% countrycode(.,origin = "vdem", destination = "iso3n")

vdem.data <- vdem.data[,c("ISO3N", "v2x_egaldem")]

colnames(vdem.data)[2]<-"egaldem"

gov.policy <- read.csv(url("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv"))

gov.policy <- gov.policy %>% dplyr::select(CountryCode, Date, H2_Testing.policy)

yesterday <- (Sys.Date() -1) %>% as.character()
yesterday <- paste0(substr(yesterday,1,4), substr(yesterday,6,7), substr(yesterday,9,10))

gov.policy$Date <- gov.policy$Date %>% as.character()
gov.policy <- gov.policy %>% filter(Date == yesterday)

covid19.foriegn.death <- drop_na(covid19.foriegn.death)
final <- left_join(covid19.foriegn.death, mona.clean.num.conditionality)
final <- left_join(final, mona.clean.num.programs)
final <- left_join(final, mona.clean.under.year)
final <- left_join(final, vdem.data)
final <- left_join(final, oil)
final[is.na(final)] <- 0

final <- left_join(final, dem5.2018)
final <- left_join(final, urban)

final <- final %>% drop_na()

l.conditionality.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `final_sum` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.program.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `PA` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SB.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `SB` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SPC.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `SPC` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SAC.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `SAC` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.total.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                              `totalprogram` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.time.death <- final %>% lm(formula = `total_deaths_per_million` ~ 
                           `time` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

library(stargazer)

stargazer(l.conditionality.death, l.program.death,l.SB.death,l.SPC.death,l.SAC.death,l.total.death,l.time.death, type="html", title="Results", out = "C:/Users/joshu/OneDrive/문서/Git/IMF_MONA/result.htm", align=TRUE)

###

l.conditionality.dc <- final %>% lm(formula = `deathofcase` ~ 
                                         `final_sum` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.program.dc <- final %>% lm(formula = `deathofcase` ~ 
                                  `PA` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SB.dc <- final %>% lm(formula = `deathofcase` ~ 
                             `SB` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SPC.dc <- final %>% lm(formula = `deathofcase` ~ 
                              `SPC` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.SAC.dc <- final %>% lm(formula = `deathofcase` ~ 
                              `SAC` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.total.dc <- final %>% lm(formula = `deathofcase` ~ 
                                `totalprogram` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.time.dc <- final %>% lm(formula = `deathofcase` ~ 
                               `time` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + Europe + Africa + `North America` + `South America` + Oceania)


stargazer(l.conditionality.dc, l.program.dc, l.SB.dc, l.SPC.dc, l.SAC.dc, l.total.dc, l.time.dc, type="html", title="Results", out = "C:/Users/joshu/OneDrive/문서/Git/IMF_MONA/result_doverc.htm", align=TRUE)

final$imf <- 0
final$imf[final$time != 0] <- 1

m.out <- matchit(`imf` ~ `ln.gdp` + `polity`, data = final,
                  method = NULL, distance = "glm")
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

plot(m.out, type = "qq", interactive = FALSE,
     which.xs = c("ln.gdp"))

plot(summary(m.out))

z.out <- zelig(`total_deaths_per_million` ~  `imf`,
               model = "ls", data = m.out)

fit2 <- lm(`total_deaths_per_million` ~ 
             `imf` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + Asia + 
             Europe + Africa + `North America` + `South America` + Oceania
           , data = m.out, weights = weights)



