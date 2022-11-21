#PARTE 1 

#Base de Dados
Base <- SocialSpending_Remittances_LAC_2000_2020

#Pacotes
library(tidyverse)
install.packages("stargazer")
library(stargazer)

#Ajustes
Base$Remittances <- as.numeric(Base$Remittances)
Base$`Total Social Spending` <- as.numeric(Base$`Total Social Spending`)

#Remittances and Total Social Spending (without control variables)
lm_sspending <- lm(Base$`Total Social Spending` ~Base$Remittances)
coefficients(lm_sspending)
summary(lm_sspending)
ggplot(Base, aes(x = Base$Remittances, y = Base$`Total Social Spending`))+geom_point() + geom_smooth(method = lm, se = FALSE)


#Remittances and Health Spending
lm_health <- lm(Base$Health ~Base$Remittances)
summary(lm_health)
coefficients(lm_health)

#Remittances and Social Protection Spending
lm_sprotect <- lm(Base$`Social Protection` ~Base$Remittances)
summary(lm_sprotect)
coefficients(lm_sprotect)

#Remittances and Education Spending
lm_educ <- lm(Base$Education ~Base$Remittances)
coefficients(lm_educ)

#Regression
stargazer(lm_sspending, type = "text", title = "Relationship Between Remittance Flows and Social Spending - Without Controls")

stargazer(lm_sspending, lm_health, lm_educ, lm_sprotect, type = "html", title = "Relationship between Remittance Flows and Social Spending (2000-2020)", out = "modelo.html")

#Remittances and Social Spending + Controls

reg2 <- lm(Base$`Total Social Spending` ~ Base$Remittances + Base$`Urban population (% of total population)` + Base$`Population ages 65 and above (% of total population)` + Base$`School enrollment, primary (% net)` + Base$`School enrollment, secondary (% net)` + Base$`School enrollment, tertiary (% gross)` + log(Base$`GDP per capita (current US$)`) + Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)` + log(Base$`Inflation, consumer prices (annual %)`) + Base$COVID)

coefficients(reg1)
summary(reg1)


#Dummy
Base$COVID <- ifelse(Base$Year == 2019 | Base$Year == 2020, 1, 0)

reg_health <- lm(Base$Health ~Base$Remittances + Base$`Urban population (% of total population)` + Base$`Population ages 65 and above (% of total population)` + Base$`School enrollment, primary (% net)` + Base$`School enrollment, secondary (% net)` + Base$`School enrollment, tertiary (% gross)` + log(Base$`GDP per capita, PPP (constant 2017 international $)`) + log(Base$`GDP per capita (current US$)`) + Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)` + log(Base$`Inflation, consumer prices (annual %)`) + Base$HealthCrisis)
reg_educ <- lm(Base$Education ~Base$Remittances + Base$`Urban population (% of total population)` + Base$`Population ages 65 and above (% of total population)` + Base$`School enrollment, primary (% net)` + Base$`School enrollment, secondary (% net)` + Base$`School enrollment, tertiary (% gross)` + log(Base$`GDP per capita, PPP (constant 2017 international $)`) + log(Base$`GDP per capita (current US$)`) + Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)` + log(Base$`Inflation, consumer prices (annual %)`)+ Base$HealthCrisis)
reg_sprotec <- lm(Base$Education ~Base$Remittances + Base$`Urban population (% of total population)` + Base$`Population ages 65 and above (% of total population)` + Base$`School enrollment, primary (% net)` + Base$`School enrollment, secondary (% net)` + Base$`School enrollment, tertiary (% gross)` + log(Base$`GDP per capita, PPP (constant 2017 international $)`) + log(Base$`GDP per capita (current US$)`) + Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)` + log(Base$`Inflation, consumer prices (annual %)`) + Base$HealthCrisis)

Base$Year <- as.numeric(Base$Year)

Base$`Urban population (% of total population)` <- as.numeric(Base$`Urban population (% of total population)`)
Base$`Population ages 65 and above (% of total population)` <- as.numeric(Base$`Population ages 65 and above (% of total population)`)
Base$`School enrollment, primary (% net)` <- as.numeric(Base$`School enrollment, primary (% net)`)
Base$`School enrollment, secondary (% net)` <- as.numeric(Base$`School enrollment, secondary (% net)`)
Base$`School enrollment, tertiary (% gross)` <- as.numeric(Base$`School enrollment, tertiary (% gross)`)
Base$`GDP per capita, PPP (constant 2017 international $)`<- as.numeric(Base$`GDP per capita, PPP (constant 2017 international $)`)
Base$`GDP per capita (current US$)` <- as.numeric(Base$`GDP per capita (current US$)`)
Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)` <- as.numeric(Base$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)
Base$`Inflation, consumer prices (annual %)` <- as.numeric(Base$`Inflation, consumer prices (annual %)`)


stargazer(reg2, type = "html", title = "Relationship between Remittance Flows and Social Spending (2000-2020)", out = "regressao.html")


# PARTE 2

library(tidyverse)


Honduras <- X1645121539honduras_lapop_dims_2008_final_data_set_v10

HON_SEL <- select(Honduras, l1, q10b)

HON_SEL$q10b <- factor(HON_SEL$q10b,
                       label = c("A lot", "Some", "A little", "Nothing", "DK/NA", "Inap"),
                       levels = c(1, 2, 3, 4, 8, 9))


table(HON_SEL)

reactable(HON_SEL)
install.packages(reactable)


# El Salvador 2018/2019

library(haven)
ElSalvador_LAPOP_AmericasBarometer_2018_v1_0_W_1_ <- read_dta("C:/Users/Gigi/Downloads/ElSalvador LAPOP AmericasBarometer 2018 v1.0_W (1).dta")

ElSalvador_2018_2019 <- ElSalvador_LAPOP_AmericasBarometer_2018_v1_0_W_1_

ELSAL_SEL_2018_19 <- select(ElSalvador_2018_2019, l1 , q10a)
table(ELSAL_SEL_2018_19)

# Honduras
Honduras_LAPOP_AmericasBarometer_2018_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Honduras LAPOP AmericasBarometer 2018 v1.0_W.dta")

Honduras_2018_2019 <- Honduras_LAPOP_AmericasBarometer_2018_v1_0_W

HOND_2018_2019 <- select(Honduras_2018_2019, l1, q10a)
table(HOND_2018_2019)


#Honduras
X428805428Jamaica_LAPOP_AmericasBarometer_2017_V1_0_W <- read_dta("C:/Users/Gigi/Downloads/428805428Jamaica LAPOP AmericasBarometer 2017 V1.0_W.dta")

Jamaica_2016_2017 <- X428805428Jamaica_LAPOP_AmericasBarometer_2017_V1_0_W
JAM_2016_2017 <- select(Jamaica_2016_2017, l1, q10a)

table(JAM_2016_2017)

#Nicaragua
Nicaragua_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Nicaragua LAPOP AmericasBarometer 2019 v1.0_W.dta")
Nicaragua_2018_2019 <- Nicaragua_LAPOP_AmericasBarometer_2019_v1_0_W

NIC_2018_2019 <- select(Nicaragua_2018_2019, l1, q10a)
table(NIC_2018_2019)

#Guatemala
Guatemala_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Guatemala LAPOP AmericasBarometer 2019 v1.0_W.dta")
Guatemala_2018_2019 <- Guatemala_LAPOP_AmericasBarometer_2019_v1_0_W

GUA_2018_2019 <- select(Guatemala_2018_2019, l1, q10a)
table(GUA_2018_2019)

#Dominican Republic
DominicanRepublic_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/DominicanRepublic LAPOP AmericasBarometer 2019 v1.0_W.dta")
DOMREP_2018_2019 <- DominicanRepublic_LAPOP_AmericasBarometer_2019_v1_0_W

DOM_2018_2019 <- select(DOMREP_2018_2019, l1, q10a)
table(DOM_2018_2019)

#Mexico
Mexico_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Mexico LAPOP AmericasBarometer 2019 v1.0_W.dta")

MEXICO_2018_2019 <- Mexico_LAPOP_AmericasBarometer_2019_v1_0_W
MEX_2018_2019 <- select(MEXICO_2018_2019, l1, q10a)
table(MEX_2018_2019)

#Ecuador
Ecuador_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Ecuador LAPOP AmericasBarometer 2019 v1.0_W.dta")
Ecuador <- Ecuador_LAPOP_AmericasBarometer_2019_v1_0_W

ECU_2018_2019 <- select(Ecuador, l1, q10a)
table(ECU_2018_2019)

#Bolivia
Bolivia_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Bolivia LAPOP AmericasBarometer 2019 v1.0_W.dta")

Bolivia <- Bolivia_LAPOP_AmericasBarometer_2019_v1_0_W

BOL_2018_2019 <- select(Bolivia, l1, q10a)
table(BOL_2018_2019)


#Paraguay
Paraguay_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Paraguay LAPOP AmericasBarometer 2019 v1.0_W.dta")
Paraguay <- Paraguay_LAPOP_AmericasBarometer_2019_v1_0_W

PAR_2018_2019 <- select(Paraguay, l1, q10a)
table(PAR_2018_2019)

#Costa Rica
CostaRica_LAPOP_AmericasBarometer_2018_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/CostaRica LAPOP AmericasBarometer 2018 v1.0_W.dta")

CostaRica <- CostaRica_LAPOP_AmericasBarometer_2018_v1_0_W
CRIC <- select(CostaRica, l1, q10a)
table(CRIC)

#Panama
Panama_LAPOP_AmericasBarometer_2018_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Panama LAPOP AmericasBarometer 2018 v1.0_W.dta")

Panama <- Panama_LAPOP_AmericasBarometer_2018_v1_0_W

PAN_2018_2019 <- select(Panama, l1, q10a)
table(PAN_2018_2019)

#Uruguay
Uruguay_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Uruguay LAPOP AmericasBarometer 2019 v1.0_W.dta")
Uruguay <- Uruguay_LAPOP_AmericasBarometer_2019_v1_0_W

URU_2018_2019 <- select(Uruguay, l1, q10a)
table(URU_2018_2019)

#Venezuela
X25258094Venezuela_LAPOP_AmericasBarometer_2016_17_V1_0_W <- read_dta("C:/Users/Gigi/Downloads/25258094Venezuela LAPOP AmericasBarometer 2016-17 V1.0_W.dta")

Venezuela <- X25258094Venezuela_LAPOP_AmericasBarometer_2016_17_V1_0_W

VEN_2016_20217 <- select(Venezuela, l1, q10a)
table(VEN_2016_20217)

#Argentina
Argentina_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("C:/Users/Gigi/Downloads/Argentina LAPOP AmericasBarometer 2019 v1.0_W.dta")

Argentina <- Argentina_LAPOP_AmericasBarometer_2019_v1_0_W
ARG_2018_2019 <- select(Argentina, l1, q10a)
table(ARG_2018_2019)

#Brazil
X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W <- read_dta("C:/Users/Gigi/Downloads/636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")

Brazil <- X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W
BRA_2014 <- select(Brazil, l1, q10a)
table(BRA_2014)




