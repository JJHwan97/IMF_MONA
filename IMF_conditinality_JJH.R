library(countrycode)
library(tidyr)
library(dplyr)

url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Combined.xlsx"
mona <- rio::import(file = url,which = 1) %>% 
  glimpse()

temp1 <- mona %>% filter(`Review Type` ==  "R10" |`Review Type` == "R11" |`Review Type` == "R11R12" |`Review Type` == "R12" |
                  `Review Type` == "R13" |`Review Type` == "R9R10")
temp2 <- mona %>% filter(`Review Type` !=  "R10" &`Review Type`!= "R11" &`Review Type` != "R11R12" &`Review Type` != "R12" &
                           `Review Type` != "R13" &`Review Type` != "R9R10")
temp1$reviewnum <- substr(temp1$`Review Type`, nchar(temp1$`Review Type`)-1, nchar(temp1$`Review Type`)) %>% as.numeric()
temp2$reviewnum <- substr(temp2$`Review Type`, nchar(temp2$`Review Type`), nchar(temp2$`Review Type`)) %>% as.numeric()

mona <- rbind(temp1, temp2)

arrangement <- mona[,c("Arrangement Number", "Country Code")] %>% unique()

for ( i in 1 : nrow(arrangement)){
  if ( i == 1){
    ar <- arrangement[i,1]
    co <- arrangement[i,2]
    temp <- mona %>% filter(`Arrangement Number` == ar & `Country Code` == co)
    ma <- temp$reviewnum %>% max()
    temp_max <- temp %>% filter(reviewnum == ma)
    mona_clean <- temp_max
  }else{
    ar <- arrangement[i,1]
    co <- arrangement[i,2]
    temp <- mona %>% filter(`Arrangement Number` == ar & `Country Code` == co)
    ma <- temp$reviewnum %>% max()
    temp_max <- temp %>% filter(reviewnum == ma)
    mona_clean <- rbind(mona_clean, temp_max)
  }
}

temp2 <- mona_clean$`Country Code` %>% countrycode(., origin = 'imf', destination = 'iso3n')
temp1 <- mona_clean$`Country Code`

mona_clean$`ISO3N` <- temp2

mona_clean <- mona_clean[(!is.na(mona_clean$`ISO3N`)),]

mona_clean$`Revised End Date` <- mona_clean$`Revised End Date` %>% as.character()

mona_clean$`Revised End Date`[mona_clean$`Revised End Date` %>% is.na()] <- mona_clean$`Initial End Year`[mona_clean$`Revised End Date` %>% is.na()]

mona_clean$`Revised End Date` <- substr(mona_clean$`Revised End Date`, 1, 4) %>% as.numeric()

mona_clean <- mona_clean %>% filter(`Arrangement Type` == "ECF"| `Arrangement Type` == "ECF-EFF"|
                                      `Arrangement Type` == "EFF"|`Arrangement Type` == "PRGF"|
                                      `Arrangement Type` == "PRGF-EFF")

mona.clean <- mona_clean %>% dplyr::select(`Arrangement Number`, ISO3N, `Arrangement Type`, `Approval Year`, `Revised End Date`)

for (i in 2000:2020){
  j <- i %>% as.character()
  mona.clean$j <- 0
  mona.clean$j[mona.clean$`Approval Year` <= i & i <= mona.clean$`Revised End Date`] <- 1
  colnames(mona.clean)[colnames(mona.clean)%>%length()] <- j
}

#number of program
length <- mona.clean %>% colnames() %>% length()

temp <- mona.clean[,6:length]

mona.clean$sum <- rowSums(temp) %>% as.data.frame()

mona.clean.10year <- mona.clean %>% filter(sum != 0)

mona.clean.10year$sum <-1

mona.clean.num.conditionality <- mona.clean.10year %>% 
                                group_by(ISO3N)%>%
                                summarize(condition_num = sum(sum))

mona.clean.num.programs <- mona.clean.10year %>% 
  dplyr::select(ISO3N, `Arrangement Number`) %>% unique() %>%
  group_by(ISO3N)%>%
  summarize(count = n())

# mona.clean.num.programs <- pivot_wider(mona.clean.num.programs, names_from = `Arrangement Type`, values_from = `program_count`, values_fill =0)
# mona.clean.num.programs$totalprogram <- rowSums(mona.clean.num.programs[,2:length(colnames(mona.clean.num.programs))])

# mona.clean.num.programs <- mona.clean.num.programs %>% filter(totalprogram != 0) %>% drop_na()

mona.clean.under.year <- mona.clean.10year %>% 
  dplyr::select(!c(`Arrangement Number`, `Arrangement Type`, `Approval Year`, `Revised End Date`,sum)) %>%
  group_by(ISO3N)%>%
  summarise(across(everything(), sum))

time <- mona.clean.under.year[, 2:length(colnames(mona.clean.under.year))]
time[time >0] <- 1
time <- rowSums(time)

mona.clean.under.year <- cbind(mona.clean.under.year$ISO3N,time) %>% as.data.frame()
colnames(mona.clean.under.year)[1]<-"ISO3N"

# mona.clean.lastyear <- mona.clean.10year %>%
#   filter(`2020` == 1) %>%
#   dplyr::select(`Arrangement Number`, `ISO3N`, `Arrangement Type`, `2020`)%>%
#   unique() %>% 
#   group_by(ISO3N, `Arrangement Type`) %>%
#   summarize(program_number = n())
# 
# mona.clean.lastyear <-mona.clean.lastyear %>% filter(`Arrangement Type` != "PCI")
# 
# mona.clean.lastyear[,3] <- 1
# mona.clean.lastyear <- mona.clean.lastyear[,c(1,3)]
# 
# mona.clean.lastyear.condition <- mona.clean.10year %>%
#   filter(`2020` == 1) %>%
#   dplyr::select(`Arrangement Number`, `ISO3N`, `Economic Code`,`2020`)%>%
#   group_by(ISO3N, `Arrangement Number`,`Economic Code`) %>%
#   summarize(condition_number = n()) %>%
#   group_by(ISO3N)%>%
#   summarize(condition_number = sum(condition_number))

# mona.clean <- mona.clean %>% dplyr::select(!c(`Approval Year`, `Revised End Date`))
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

# wdi_temp <- WDI(indicator= c("sp.urb.totl.in.zs","NY.GDP.PETR.RT.ZS", 
#                              "NE.TRD.GNFS.ZS", "BX.KLT.DINV.WD.GD.ZS", start = 2020, end=2020))
# 
# wdi_temp <- as.data.frame(wdi_temp)
# 
# colnames(wdi_temp)[4:7] <- c("urban_pop","l.oil","Trade","Fdi")
# 
# wdi_temp$ISO3N <- wdi_temp$iso2c %>% countrycode(.,origin = "iso2c", destination = "iso3n")

# 
# # distance from US
# unvote <- read_csv("C:/Users/joshu/Desktop/Agreement.csv")
# unvote <- unvote %>% filter(iso3c.x == "USA" & year == 2019)
# unvote$ISO3N <- unvote$iso3c.y %>% countrycode(.,origin = "iso3c", destination = "iso3n")
# unvote <- unvote %>% dplyr::select(ISO3N, IdealPointDistance)
# colnames(unvote)[1] <- "ISO3N"

library(vdemdata)
vdem.data <- vdem
vdem.data <- vdem.data %>% filter(year == 2020)

vdem.data <- vdem.data %>% dplyr::select(year, country_id, v2x_egaldem)

vdem.data$ISO3N <- vdem.data$country_id %>% countrycode(.,origin = "vdem", destination = "iso3n")

vdem.data <- vdem.data[,c("ISO3N", "v2x_egaldem")]

colnames(vdem.data)[2]<-"egaldem"

gov.policy <- read.csv(url("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv"))

gov.policy <- gov.policy %>% dplyr::select(CountryCode, Date, H2_Testing.policy)

yesterday <- Sys.Date() -1
yesterday <- paste0(substr(yesterday,1,4), substr(yesterday,6,7), substr(yesterday,9,10))

gov.policy$Date <- gov.policy$Date %>% as.character()
gov.policy <- gov.policy %>% filter(Date == yesterday)


border <- read.csv("E:/datafile/country-borders-master/GEODATASOURCE-COUNTRY-BORDERS.CSV")
colnames(border)[1] <- "main"
colnames(border)[3] <- "ISO2C"
border$ISO3N <- border$ISO2C %>% countrycode(., origin = 'iso2c', destination = 'iso3n')

border1 <- left_join(border , covid19.foriegn.death)
border1 <- border1 %>% dplyr::select(main, ISO3N, total_deaths_per_million)
border1$mainISO3N <- border1$main %>% countrycode(., origin = 'iso2c', destination = 'iso3n')
border2 <- border1 %>% group_by(mainISO3N) %>% summarize(sum = sum (total_deaths_per_million, na.rm = TRUE))
border2$num <- border1 %>% group_by(mainISO3N) %>% summarize(num = n())
border2 <- border2 %>% as.matrix() %>% as.data.frame() %>% .[,c(1,2,4)]
colnames(border2)[3] = "num"
border2$av <- border2[,2]/border2[,3]
border2$lav <- log(border2$av +1)
colnames(border2)[1] <- "ISO3N"

agree <- read.csv("E:/datafile/Agreement.csv")
agree1 <- agree %>% filter(year > 1999 & ccode1 == 2) %>% dplyr::select(year, iso3c.x, iso3c.y, IdealPointDistance)
agree1$ISO3N <- agree1$iso3c.y %>% countrycode(., origin = "iso3c", destination = "iso3n")
agree2 <- agree1 %>% group_by(ISO3N) %>% summarize(mean = mean(IdealPointDistance)) %>% drop_na()
temp <- c(840, 0)
unvote <- rbind(agree2, temp)

covid19.foriegn.death <- drop_na(covid19.foriegn.death)
final <- left_join(covid19.foriegn.death, mona.clean.num.conditionality)
final <- left_join(final, mona.clean.num.programs)
final <- left_join(final, mona.clean.under.year)
final <- left_join(final, vdem.data)
final <- left_join(final, oil)
# final <- left_join(final, mona.clean.lastyear)
# final <- left_join(final, mona.clean.lastyear.condition)
final[is.na(final)] <- 0

final <- left_join(final, dem5.2018)
final <- left_join(final, fdi)
final <- left_join(final, unvote)
final <- left_join(final, border2)
final <- left_join(final, urban)
final <- final %>% drop_na()

final$lfdi <- log(final$Fdi)
final$lfdi[final$Fdi <= 0] <- -log(final$Fdi[final$Fdi <= 0] %>% abs())
final$ltrade <- log( final$Trade + 1 )
final$mean2 <- final$mean * final$mean

# write.csv(final, paste0(getwd(), "/final.csv"))

# write.csv(final, paste0(getwd(), "/final.csv"))
l.conditionality.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                                      `condition_num` 
                                      + mean + mean2 
                                      + `ln.gdp` + `aged_65_older` + `polity` + `urban_pop` 
                                      + num + lav + num * lav)
summary(l.conditionality.death)

l.conditionality.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                                        `condition_num`
                                        + mean + mean2 + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` 
                                         + `l.oil` + `egaldem` + l.population_density)
summary(l.conditionality.death)

l.conditionality.death <- final %>%lm(formula = `deathofcase` ~ 
                                        `condition_num`+`count` + `condition_num`*`count` + time + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density)
summary(l.conditionality.death)

l.conditionality.death <- final %>%lm(formula = `deathofcase` ~ 
                                        `condition_num`+`count` + `condition_num`*`count` + time + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` 
                                      + `l.oil` + `egaldem` + l.population_density + mean + mean2)
summary(l.conditionality.death)

l.conditionality.death <- final %>%lm(formula = `deathofcase` ~ 
                                        `condition_num`+`count` + `condition_num`*`count` + time 
                                      +num + lav + num * lav)
summary(l.conditionality.death)


  l.conditionality.death <- final %>%lm(formula = `l.total_deaths_per_million` ~ 
                             `condition_num`+`count` + `condition_num`*`count` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                             + num + lav + num * lav + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  l.program.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                             `count` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                              + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  l.time.death <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
                             `time` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                             + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  # l.lastyear <- final %>% lm(formula = `l.total_deaths_per_million` ~ 
  #                                `final_sum`*program_number + final_sum + `program_number` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
  #                              + Asia + Europe + Africa + `North America` + `South America` + Oceania)
  
  library(stargazer)
  
  stargazer(l.conditionality.death, l.program.death, l.time.death,  type="html", title="Results", out = "C:/Users/joshu/OneDrive/문서/Git/IMF_MONA/result.htm", align=TRUE)

###

l.conditionality.dc <- final %>% lm(formula = `deathofcase` ~ 
                                         `condition_num` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
                                    + Asia + Europe + Africa + `North America` + `South America` + Oceania)

l.total.dc <- final %>% lm(formula = `deathofcase` ~ 
                                `count` + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` + `l.oil` + `egaldem` + l.population_density
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

m.out <- matchit(`program_number` ~ + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop` 
                 + `l.oil` + `egaldem` + l.population_density + num + lav + num * lav,
                  method = "optimal", distance = "glm", data = final)
summary(m.out)

plot(m.out, type = "jitter", interactive = FALSE)

plot(m.out, type = "qq", interactive = FALSE,
     which.xs = c("ln.gdp"))

plot(summary(m.out))

z.out <- zelig(`l.total_deaths_per_million` ~  `program_number` + program_number*time + ltotalprogram 
               + `aged_65_older` + `ln.gdp` + `polity` + `urban_pop`  + `egaldem` + l.population_density,
               model = "ls", data = m.out)

z.out.save <- z.out

z.out <- setx(z.out, `program_number` = 0)
z.out <- setx1(z.out, `program_number` = 1)

summary(z.out)

z5_1 <- sim(z.out)

plot(z5_1)

summary(z.out.save)

