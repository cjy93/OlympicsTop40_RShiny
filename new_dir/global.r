## Library packages 
library(dplyr)
library(reshape2)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(MASS)
library(png)
library(jpeg)
library(ERSA)
library(car)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(countrycode)
library(purrr)
library(datasets)
library(tmap)
library(leaflet)
library(readr)
library(plotly)

setwd('C:\\Users\\jia yi\\Desktop\\MITB\\Sem1\\ISSS616 Applied Statistical Analysis with R\\Project\\Olympics project')
#setwd('/Users/ruiboon/Documents/jy/code/mas/R/prjShiny')
my_data <- read.csv("Top40.csv")
#options(shiny.error = browser)

# add the continent column to the df
my_data$continent <- countrycode(sourcevar = my_data[, "Country.Name"],
                                 origin = "country.name",
                                 destination = "continent")

cm = my_data$communism
sov = my_data$soviet
cpi = my_data$cpi
home = my_data$homeadvantage
cnn = my_data$connectivity
hdi = my_data$hdi
gdp = my_data$GDP_per_cap
gdp_ppp = my_data$GDP_PPP
health = my_data$Health_exp
pop = my_data$Population_total
life = my_data$Life_exp
cm2= cm^2
sov2 = sov^2
cpi2= cpi^2
cnn2 = cnn^2
hdi2= hdi^2
gdp2 = gdp^2
health2 = health^2
pop2 = pop^2
life2 = life^2
cm_pop = cm*pop
cm_sov = cm*sov
home_health = home*health
pop_life = pop*life
home_gdp = home*gdp
home_gdp_ppp = home*gdp_ppp
fit1 <- lm(medal~cm+sov+cpi+home+cnn+hdi+gdp+gdp_ppp+health+pop+life+cm2+sov2+cpi2
           +cnn2+hdi2+gdp2+health2+pop2+life2+cm_pop+cm_sov+home_health+pop_life+home_gdp+home_gdp_ppp, data = my_data)
# summary(fit1)
# 
# anova(fit1)

library(MASS)
step <- stepAIC(fit1, direction = "both")

#summary(step)

model1<-lm(medal~cm+sov+cpi+home+cnn+hdi+gdp+gdp_ppp+health+pop+life, data = my_data)

model2 <- lm(medal~cm+sov+cpi+home+cnn+hdi+gdp+gdp_ppp+health+pop+life+cm2+sov2+cpi2
             +cnn2+hdi2+gdp2+health2+pop2+life2, data = my_data)

model3<- lm(medal~cm+sov+cpi+home+cnn+hdi+gdp+gdp_ppp+health+pop+life+cm2+sov2+cpi2
            +cnn2+hdi2+gdp2+health2+pop2+life2+cm_pop+cm_sov+home_health+pop_life+home_gdp+home_gdp_ppp, data = my_data)


# AIC step wise to find best model
step1 <- stepAIC(model1, direction = "both") # linear terms only
#summary(step1)
#anova(step1)

step2 <- stepAIC(model2, direction = "both") # linear + quadratic terms
#summary(step2)
#anova(step2)

step3 <- stepAIC(model3, direction = "both") # linear + quadratic + interactive terms
#summary(step3)
#anova(step3)

#summary(step3)

###### Creating New Dataframe for Medal Tally for PredApp ##############
medal_pred<-predict(step3, newdata=my_data, interval = "confidence")
medal_pred<-data.frame(medal_pred[201:240])
medal_pred
a<-my_data %>%
  select(c(Country.Name,medal)) %>%
  filter(my_data$Time=="2000")
a

b<-my_data %>%
  select(medal) %>%
  filter(my_data$Time=="2004")
b

c<-my_data %>%
  select(medal) %>%
  filter(my_data$Time=="2008")

d<-my_data %>%
  select(medal) %>%
  filter(my_data$Time=="2012")

e<-my_data %>%
  select(medal) %>%
  filter(my_data$Time=="2016")

new_data<-data.frame(c(a,b,c,d,e,medal_pred))
head(new_data)
names(new_data)[2]<-"2000"
names(new_data)[3]<-"2004"
names(new_data)[4]<-"2008"
names(new_data)[5]<-"2012"
names(new_data)[6]<-"2016"
names(new_data)[7]<-"2020"
names(new_data)[1]<-"Country_name"

medaldata<-new_data
medaldata$`2000` <- round(medaldata$`2000`, digits = 0)
medaldata$`2004` <- round(medaldata$`2004`, digits = 0)
medaldata$`2008` <- round(medaldata$`2008`, digits = 0)
medaldata$`2012` <- round(medaldata$`2012`, digits = 0)
medaldata$`2016` <- round(medaldata$`2016`, digits = 0)
medaldata$`2020` <- round(medaldata$`2020`, digits = 0)
k<-data.frame(medaldata[order(-medaldata$`2020`),])
top3<-k %>% top_n(3)
top5<-k %>% top_n(5)
top10<-k %>% top_n(10)
############################# for creating world map ###################
data("World")
year2000 <- read.csv("2000.csv")
year2004 <- read.csv("2004.csv")
year2008 <- read.csv("2008.csv")
year2012 <- read.csv("2012.csv")
year2016 <- read.csv("2016.csv")
medal2000 <- left_join(World, year2000, by = c("name" = 'Country.Name'))
medal2004 <- left_join(World, year2004, by = c("name" = 'Country.Name'))
medal2008 <- left_join(World, year2008, by = c("name" = 'Country.Name'))
medal2012 <- left_join(World, year2012, by = c("name" = 'Country.Name'))
medal2016 <- left_join(World, year2016, by = c("name" = 'Country.Name'))

############################ for scatter plot ##############################
olympics <- read.csv("Top40_Jiayi_edit_our_version.csv")
names(olympics)[2] <- "Country_Name"
names(olympics)[5] <-"Life_Expectancy"
names(olympics)[6] <- "Health_Expenditure"
names(olympics)[7] <- "GDP_Per_Capita"
names(olympics)[8] <- "GDP_PPP"
names(olympics)[14] <- "Home_Advantage"