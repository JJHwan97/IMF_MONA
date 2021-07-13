library(countrycode)
library(tidyr)
library(dplyr)

url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Combined.xlsx"
mona <- rio::import(file = url,which = 1) %>% 
  glimpse()

temp2 <- mona$`Country Code` %>% countrycode(., origin = 'imf', destination = 'iso3n')
temp1 <- mona$`Country Code`
temp2[temp1 == 965] <- 890

mona$`ISO3N` <- temp2

mona.clean <- mona %>% dplyr::select(`Arrangement Number`, ISO3N, `Arrangement Type`, `Economic Code`, `Approval Year`, `Initial End Year`)

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
  dplyr::select(ISO3N, `Arrangement Number`, `Arrangement Type`, c(2011:2021)%>% as.character()) %>% unique() %>%
  group_by(ISO3N, `Arrangement Type`, `Arrangement Number`)%>%
  summarize(count = n()) %>%
  group_by(ISO3N,`Arrangement Type`)%>%
  summarize(program_count = n())

mona.clean.num.programs <- pivot_wider(mona.clean.num.programs, names_from = `Arrangement Type`, values_from = `program_count`, values_fill =0)
mona.clean.num.programs$totalprogram <- rowSums(mona.clean.num.programs[,2:length(colnames(mona.clean.num.programs))])

mona.clean.num.programs <- mona.clean.num.programs %>% filter(totalprogram != 0) %>% drop_na()

mona.clean.under.year <- mona.clean.10year %>% 
  dplyr::select(!c(`Arrangement Number`, `Arrangement Type`, `Economic Code`, `Approval Year`, `Initial End Year`,sum)) %>%
  group_by(ISO3N)%>%
  summarise(across(everything(), sum))

time <- mona.clean.under.year[, 2:length(colnames(mona.clean.under.year))]
time[time >0] <- 1
time <- rowSums(time)

mona.clean.under.year <- cbind(mona.clean.under.year$ISO3N,time) %>% as.data.frame()
colnames(mona.clean.under.year)[1]<-"ISO3N"

mona.clean.lastyear <- mona.clean.10year %>%
  filter(`2020` == 1) %>%
  dplyr::select(`Arrangement Number`, `ISO3N`, `Arrangement Type`, `2020`)%>%
  unique() %>% 
  group_by(ISO3N, `Arrangement Type`) %>%
  summarize(program_number = n())

mona.clean.lastyear <-mona.clean.lastyear %>% filter(`Arrangement Type` != "PCI")

mona.clean.lastyear[,3] <- 1
mona.clean.lastyear <- mona.clean.lastyear[,c(1,3)]

mona.clean.lastyear.condition <- mona.clean.10year %>%
  filter(`2020` == 1) %>%
  dplyr::select(`Arrangement Number`, `ISO3N`, `Economic Code`,`2020`)%>%
  group_by(ISO3N, `Arrangement Number`,`Economic Code`) %>%
  summarize(condition_number = n()) %>%
  group_by(ISO3N)%>%
  summarize(condition_number = sum(condition_number))

# mona.clean <- mona.clean %>% dplyr::select(!c(`Approval Year`, `Initial End Year`))
# 
# mona.clean <- mona.clean %>% 
#   dplyr::select(!c(`sum`)) %>%
#   pivot_longer(!c("Arrangement Number","ISO3N","Arrangement Type","Economic Code"), names_to = "year", values_to = "under") %>%
#   filter(under != 0)
# 
# mona.clean$year <- mona.clean$year %>% as.numeric()
# 
# mona.clean.num <- mona.clean %>%
#   dplyr::select(!c(`Arrangement Number`, `Arrangement Type`, `Economic Code`)) %>%
#   group_by(ISO3N, year)%>%
#   summarize(condition_num = n())
# mona.clean.num$condition_num <- mona.clean.num$condition_num %>% as.numeric()
# 
# mona.clean.key <- mona.clean %>%
#   group_by(ISO3N, year, `Arrangement Type`)%>%
#   summarize(condition_num = n())
# 
# mona.clean.economic <- mona.clean %>%
#   group_by(ISO3N, year, `Economic Code`)%>%
#   summarize(condition_num = n())
# mona.clean.economic$`Economic Code` <- mona.clean.economic$`Economic Code` %>% as.character()
# 
# mona.clean.10year.num <- mona.clean.10year %>%
#   dplyr::select(!c(`Arrangement Number`, `Arrangement Type`, `Economic Code`)) %>%
#   group_by(ISO3N)%>%
#   summarize(condition_num = sum(sum))
# 
# mona.clean.10year.key <- mona.clean.10year %>%
#   group_by(ISO3N, `Arrangement Type`)%>%
#   summarize(Key_num = sum(sum))
# 
# mona.clean.10year.key <- mona.clean.10year.key %>% pivot_wider(names_from = `Arrangement Type`, values_from = `Key_num`, values_fill = 0)
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
#   ggplot(aes(x=year, y=condition_num, group = `Arrangement Type`, colour = `Arrangement Type`)) +
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

date.yesterday <- "2020-12-31"

covid19.foriegn <- covid19.foriegn %>% filter(date == date.yesterday)

covid19.foriegn.death <- covid19.foriegn %>% dplyr::select(c("iso_code", "total_deaths_per_million", 
                                                             "population_density", "gdp_per_capita", "life_expectancy",
                                                             "aged_65_older", "continent"))

covid19.foriegn.death$deathofcase <- covid19.foriegn$total_deaths_per_million/covid19.foriegn$total_cases_per_million

covid19.foriegn.death$ISO3N <- covid19.foriegn.death$iso_code %>% countrycode(., origin = 'iso3c', destination = 'iso3n')
covid19.foriegn.death$ln.gdp <- log(covid19.foriegn.death$gdp_per_capita)

covid19.foriegn.death$l.total_deaths_per_million <- log(covid19.foriegn.death$total_deaths_per_million)
covid19.foriegn.death$l.population_density <- log(covid19.foriegn.death$population_density)

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
library(WDI)
# urban <- wb_data("sp.urb.totl.in.zs")
urban <- WDI(indicator="sp.urb.totl.in.zs", start = 2020, end=2020)
urban <- urban[3:nrow(urban),]
# urban <- urban %>% filter(date == 2020)
urban$ISO3N <- urban$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")
urban <- urban[,c("ISO3N","sp.urb.totl.in.zs")] %>% drop_na()
colnames(urban)[2]<-"urban_pop"

# oil <- wb_data("NY.GDP.PETR.RT.ZS")
# oil <- oil %>% filter(date == 2019)
oil <-WDI(indicator="NY.GDP.PETR.RT.ZS", start = 2019, end=2019)
oil$ISO3N <- oil$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")
oil <- oil[,c("ISO3N","NY.GDP.PETR.RT.ZS")] %>% drop_na()
oil[,2] <- log(oil[,2]+1)
colnames(oil)[2]<-"l.oil"

#get interest
interest <-WDI(indicator="FR.INR.RINR", start = 2019, end=2020)
interest$ISO3N <- interest$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")
interest <- interest[,c("year","ISO3N","FR.INR.RINR")] %>% drop_na()
interest <- interest %>% filter(ISO3N == 840)
interest.value <- interest[1,3] - interest[2,3]

# Trade
trade <-WDI(indicator="NE.TRD.GNFS.ZS", start = 2019, end=2019)
trade$ISO3N <- trade$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")
trade <- trade[,c("ISO3N","NE.TRD.GNFS.ZS")] %>% drop_na()
colnames(trade)[2] <- "Trade"

# FDI
fdi <-WDI(indicator="BX.KLT.DINV.WD.GD.ZS", start = 2019, end=2019)
fdi$ISO3N <- fdi$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")
fdi <- fdi[,c("ISO3N","BX.KLT.DINV.WD.GD.ZS")] %>% drop_na()
colnames(fdi)[2] <- "Fdi"


# distance from US
unvote <- read_csv("C:/Users/joshu/Desktop/Agreement.csv")
unvote <- unvote %>% filter(iso3c.x == "USA" & year == 2019)
unvote$ISO3N <- unvote$iso3c.y %>% countrycode(.,origin = "iso3c", destination = "iso3n")
unvote <- unvote %>% dplyr::select(ISO3N, IdealPointDistance)
colnames(unvote)[1] <- "ISO3N"

library(vdemdata)
vdem.data <- vdem
vdem.data <- vdem.data %>% filter(year == 2020)

vdem.data <- vdem.data %>% dplyr::select(year, country_id, v2x_egaldem)

vdem.data$ISO3N <- vdem.data$country_id %>% countrycode(.,origin = "vdem", destination = "iso3n")

vdem.data <- vdem.data[,c("ISO3N", "v2x_egaldem")]

colnames(vdem.data)[2]<-"egaldem"

gov.policy <- read.csv(url("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv"))

gov.policy <- gov.policy %>% dplyr::select(CountryCode, Date, H2_Testing.policy)

yesterday <- "2020-12-31"
yesterday <- paste0(substr(yesterday,1,4), substr(yesterday,6,7), substr(yesterday,9,10))

gov.policy$Date <- gov.policy$Date %>% as.character()
gov.policy <- gov.policy %>% filter(Date == yesterday)

covid19.foriegn.death <- drop_na(covid19.foriegn.death)
final <- left_join(covid19.foriegn.death, mona.clean.num.conditionality)
final <- left_join(final, mona.clean.num.programs)
final <- left_join(final, mona.clean.under.year)
final <- left_join(final, vdem.data)
final <- left_join(final, oil)
final <- left_join(final, mona.clean.lastyear)
final <- left_join(final, mona.clean.lastyear.condition)
final[is.na(final)] <- 0

final <- left_join(final, dem5.2018)
final <- left_join(final, urban)
final <- left_join(final, fdi)
final <- left_join(final, trade)
final <- left_join(final, unvote)
final <- final %>% drop_na()

final$lfdi <- log(final$Fdi)
final$lfdi[final$Fdi < 0] <- -log(final$Fdi %>% abs())
final$ltrade <-log( final$Trade + 1 )

# write.csv(final, paste0(getwd(), "/final.csv"))

  l.conditionality.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                             `final_sum` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                             + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  l.program.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                             `totalprogram` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                              + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  l.time.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                             `time` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                             + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  l.lastyear <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                                 `final_sum`*program_number + final_sum + `program_number` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                               + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  library(stargazer)
  
  stargazer(l.conditionality.death, l.program.death, l.time.death,l.lastyear,  type="html", title="Results", out = "C:/Users/joshu/OneDrive/문서/Git/IMF_MONA/result.htm", align=TRUE)

###

l.conditionality.dc <- final %>% lm(formula = `deathofcase` ~ 
                                         `final_sum` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                                    + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.total.dc <- final %>% lm(formula = `deathofcase` ~ 
                                `totalprogram` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                           + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.time.dc <- final %>% lm(formula = `deathofcase` ~ 
                               `time` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                          + Asia + Europe + Africa + `North America` + `South America` + Oceania)


stargazer(l.conditionality.dc, l.total.dc, l.time.dc, type="html", title="Results", out = "C:/Users/joshu/OneDrive/문서/Git/IMF_MONA/result_doverc.htm", align=TRUE)

final$imf <- 0
final$imf[final$time != 0] <- 1

final$ltotalprogram <- log(final$totalprogram + 1)

library(Zelig)
library(MatchIt)

m.out <- matchit(`program_number` ~ ln.gdp + ltrade + lfdi + polity + `l.oil`
                 + Asia + Europe + Africa + `North America` + `South America`,
                  method = "optimal", distance = "glm", data = final)
summary(m.out)

plot(m.out, type = "jitter", interactive = FALSE)

plot(m.out, type = "qq", interactive = FALSE,
     which.xs = c("ln.gdp"))

plot(summary(m.out))

z.out <- zelig(`l.total_deaths_per_million` ~  `program_number` + program_number*time + time 
               + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop`  + `egaldem` + l.population_density,
               model = "ls", data = m.out)

z.out.save <- z.out

z.out <- setx(z.out, `program_number` = 0)
z.out <- setx1(z.out, `program_number` = 1)

summary(z.out)

z5_1 <- sim(z.out)

plot(z5_1)

summary(z.out.save)

