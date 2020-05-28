# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")    

install.packages("readstata13")

library(haven)
library(readstata13)
library(reshape2)
library(zoo)
library(dplyr)

setwd("C:/Users/brian/Documents/Development Econ/Research/RawData")

capital.stock <- read.csv("capitalstock.csv")
labor.force <- read.csv("laborforce.csv")
schooling <- read.csv("schooling.csv")

#===========================================================================================================
#Data Cleaning
#===========================================================================================================

capital.stock <- data.frame(sapply(capital.stock, function(x) (gsub("[-]", 0, x))))

capital.stock$capital.stock <- as.numeric(as.character(capital.stock$kgov_rppp)) + as.numeric(as.character(capital.stock$kpriv_rppp)) + as.numeric(as.character(capital.stock$kppp_rppp))

labor.force$code <- paste(labor.force$ï..LOCATION, labor.force$TIME)
capital.stock$code <- paste(capital.stock$isocode, capital.stock$year)

labor.force[c("INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "Flag.Codes", "ï..LOCATION", "TIME")] <- NULL
colnames(labor.force)[colnames(labor.force)=="Value"] <- "labor.force"

capital.stock[c("ifscode", "kgov_rppp", "kpriv_rppp", "kppp_rppp")] <- NULL

tfpdata <- merge(capital.stock, labor.force, by = "code", all.x = TRUE, all.y = TRUE)

schooling$ï..HDI.Rank <- NULL

schooling <- melt(schooling, id = "Country")
schooling$variable <- as.numeric(gsub("X", "", schooling$variable))
schooling$id <- paste(schooling$Country, schooling$variable)
tfpdata$id <- paste(tfpdata$country, tfpdata$year)

tfpdata <- merge(tfpdata, schooling, by = "id", all.x = TRUE)
tfpdata[c("Country", "variable", "id")] <- NULL

colnames(tfpdata)[colnames(tfpdata)=="value"] <- "schooling.years"

rm(capital.stock, labor.force, schooling)

tfpdata$schooling.years <- as.numeric(tfpdata$schooling.years)

data <- data.frame()
isocode <- unique(tfpdata$isocode)
for(i in isocode){
  temp <- filter(tfpdata, isocode == i)
  yrsschool <- na.locf(temp$schooling.years, fromLast = FALSE, na.rm = FALSE)
  temp$schooling.years <- yrsschool
  data <- rbind(data, temp)
}

tfpdata <- data
rm(i, isocode, yrsschool, temp, data)

tfpdata$yl <- as.numeric(as.character(tfpdata$GDP_rppp))/as.numeric(as.character(tfpdata$labor.force))
tfpdata$kl <- as.numeric(as.character(tfpdata$capital.stock))/ as.numeric(as.character(tfpdata$labor.force))
tfpdata$ky <- as.numeric(as.character(tfpdata$capital.stock))/ as.numeric(as.character(tfpdata$GDP_rppp))

a <- 0.3/(1-0.3)
tfpdata$kya <- tfpdata$ky^a

for(i in 1:length(tfpdata$schooling.years)){
    if(is.na(tfpdata$schooling.years[i])){
      tfpdata$hl[i] <- NA
    } else if(tfpdata$schooling.years[i] <= 4){
      tfpdata$hl[i] <- exp(0.105*tfpdata$schooling.years[i])
    } else if(tfpdata$schooling.years[i] > 4 & tfpdata$schooling.years[i] <= 8){
      tfpdata$hl[i] <- exp(0.105*4 + (tfpdata$schooling.years[i] - 4)*0.088)
    } else {
      tfpdata$hl[i] <- exp(0.105*4 + 0.088*4 + (tfpdata$schooling.years[i] - 8)*0.08)
    }
}

tfpdata$tfp <- tfpdata$yl/(tfpdata$kya*tfpdata$hl)
#TFP YoY growth
tfpdata$tfp_g <- with(tfpdata, ave(tfp, isocode, 
                            FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
#GDP YoY growth
tfpdata$gdp_g <- with(tfpdata, ave(GDP_rppp, isocode, 
                                   FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
#K/Y YoY growth
tfpdata$kya_g <- with(tfpdata, ave(kya, isocode, 
                                   FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

#Capital Stock Growth YoY
tfpdata$k_g <- with(tfpdata, ave(capital.stock, isocode, 
                                   FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

tfpdata$decade <- 0
count = 0
for(var in tfpdata$year){
  count = count + 1
  if(var < 1970){
    tfpdata$decade[count] <- 1960
    }
  else if(var < 1980){
    tfpdata$decade[count] <- 1970
    }
  else if(var < 1990){
    tfpdata$decade[count] <- 1980
    }
  else if(var < 2000){
    tfpdata$decade[count] <- 1990
    }
  else if(var < 2010){
    tfpdata$decade[count] <- 2000
    }
  else if(var < 2020){
    tfpdata$decade[count] <- 2010
    }
}

tfpdata <- do.call(data.frame,lapply(tfpdata, function(x) replace(x, is.infinite(x), NA)))

rm(a,i)
setwd("C:/Users/brian/Documents/Development Econ/Research/CleanedData")
write.csv(tfpdata, file = "tfp_data.csv")

fullTFP <- subset(tfpdata, is.na(tfpdata$tfp) == FALSE)
length(unique(fullTFP$country))
setwd("C:/Users/brian/Documents/Development Econ/Research/RawData")
geomet <- read.dta13("IfoGAME_balanced_panel.dta")
#tfpdata <- read.csv("tfp_data.csv")
geomet$code <- paste(geomet$iso, geomet$year)

data <- merge(tfpdata, geomet[,c("code", "disindexla")], by = "code")

rm(geomet)
shocks <- read_dta("table_replication_data.dta")

shocks$code <- paste(shocks$country, shocks$year)
data <- merge(data[,c("code", "disindexla", "GDP_rppp", "gdp_g", "k_g", "tfp", "tfp_g", "decade")], shocks, by = "code")

rm(shocks)

#fullTFP <- subset(data, is.na(data$tfp_g) == FALSE)
#Only have 16 unique countries for which all data is available
str(data)


length(unique(fullTFP$country))
#Only have 16 unique countries for which all data is available
length(unique(data$country))
#Only 28 countries available for disasterindex/shock data merge
length(unique(shocks$country))
#From the original 60 countries
length(unique(geomet$country))



rm(geomet)

#Write to .dta file for regressions in Stata
write_dta(data, path = "data_shocks.dta")
write_dta(fullTFP, path = "balanced_data_shocks.dta")
#===========================================================================================================
#Analysis
#===========================================================================================================
library(reshape2)

colnames(data)
plot1 <- aggregate(gdp_g~isocode + decade, tfpdata, mean)
plot1 <- dcast(plot1, isocode~decade, mean)
plot1 <- na.omit(plot1)

plot2 <- aggregate(tfp_g~isocode + decade, tfpdata, mean)
plot2 <- dcast(plot2, isocode~decade, mean)
plot2 <- na.omit(plot2)

plot3 <- aggregate(k_g~isocode + decade, tfpdata, mean)
plot3 <- dcast(plot3, isocode~decade, mean)
plot3 <- na.omit(plot3)

#Figure 1
par(mfrow=c(2,2))
plot(plot1$`1960`, plot1$`1980`)
abline(v = mean(plot1$`1960`))
abline(h = mean(plot1$`1980`))

plot(plot1$`1970`, plot1$`1990`)
abline(v = mean(plot1$`1970`))
abline(h = mean(plot1$`1990`))

plot(plot1$`1980`, plot1$`2000`)
abline(v = mean(plot1$`1980`))
abline(h = mean(plot1$`2000`))

plot(plot1$`1990`, plot1$`2010`)
abline(v = mean(plot1$`1990`))
abline(h = mean(plot1$`2010`))

par(mfrow=c(1,1))
plot(plot1$`1960`, plot1$`2000`)                               
abline(v = mean(plot1$`1960`))
abline(h = mean(plot1$`2000`))          


reg1.1 <- lm(plot1$`1970` ~ plot1$`1960`)
summary(reg1.1)

reg1.2 <- lm(plot1$`2000` ~ plot1$`1990`)
summary(reg1.2)

reg1.3 <- lm(plot1$`2010` ~ plot1$`2000`)
summary(reg1.3)

cor.test(plot1$`1970`, plot1$`1960`)
cor.test(plot1$`2000`, plot1$`1990`)
cor.test(plot1$`2010`, plot1$`2000`)
cor.test(plot1$`2000`, plot1$`1970`)


#Figure 2
plot(plot2$`1980`, plot2$`1990`)
abline(v = mean(plot2$`1980`))
abline(h = mean(plot2$`1990`)) 

plot(plot2$`1990`, plot2$`2000`)
abline(v = mean(plot2$`1990`))
abline(h = mean(plot2$`2000`)) 

plot(plot2$`2000`, plot2$`2010`)
abline(v = mean(plot2$`2000`))
abline(h = mean(plot2$`2010`)) 

plot(plot2$`2000`, plot2$`1980`)
abline(v = mean(plot2$`2000`))
abline(h = mean(plot2$`1980`))

reg2.1 <- lm(plot2$`1990` ~ plot2$`1980`)
summary(reg2.1)

reg2.2 <- lm(plot2$`2000` ~ plot2$`1990`)
summary(reg2.2)

reg2.3 <- lm(plot2$`2010` ~ plot2$`2000`)
summary(reg2.3)

cor.test(plot2$`1990`, plot2$`1980`)
cor.test(plot2$`2000`, plot2$`1990`)
cor.test(plot2$`2010`, plot2$`2000`)
cor.test(plot2$`2010`, plot2$`1980`)

#Figure 3
plot(plot3$`1980`, plot3$`1990`)
abline(v = mean(plot3$`1980`))
abline(h = mean(plot3$`1990`)) 

plot(plot3$`1990`, plot3$`2000`)
abline(v = mean(plot3$`1990`))
abline(h = mean(plot3$`2000`)) 

plot(plot3$`2000`, plot3$`2010`)
abline(v = mean(plot3$`2000`))
abline(h = mean(plot3$`2010`)) 

plot(plot3$`2000`, plot3$`1980`)
abline(v = mean(plot3$`2000`))
abline(h = mean(plot3$`1980`))

reg3.1 <- lm(plot3$`1990` ~ plot3$`1980`)
summary(reg3.1)

reg3.2 <- lm(plot2$`2000` ~ plot2$`1990`)
summary(reg3.2)

reg3.3 <- lm(plot2$`2010` ~ plot2$`2000`)
summary(reg3.3)

cor.test(plot3$`1990`, plot3$`1980`)
cor.test(plot3$`2000`, plot3$`1990`)
cor.test(plot3$`2010`, plot3$`2000`)
cor.test(plot3$`2010`, plot3$`1980`)


