#Replicability script
#Pablo Varas Enríquez, Luseadra McKerracher, Nicolás Montalva Rivera 

# Workspace management ----------------------------------------------------

#Establishing directory
getwd()
setwd("folder_directory")
#Establishing history and image
savehistory(".~RB_SES_CL.Rhistory")
save.image(".~RB_SES_CL.RData")
load(".~RB_SES_CL.RData")
#Checking memory usage
memory.limit()
memory.size(max = FALSE)


# #1.- Cohort selection, data mining and management -----------------------


#R package to use
#"Hmisc" package
install.packages("Hmisc")
library(Hmisc)
#"tidyverse" package
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

#importing CASEN with spss.get()
casen <- spss.get("CASEN_2013_MN_B_Principal.sav", use.value.labels=TRUE)
#corroboration that is a data frame
is.data.frame(casen)


# #Data mining and management ---------------------------------------------

#Generation of deciles by income
casen$decilth <- ntile(casen$ytotcorh, 10)
casen$quintilth <- ntile(casen$ytotcorh, 5)
casen$cuartilth <- ntile(casen$ytotcorh, 4)
casen$tercilth <- ntile(casen$ytotcorh, 3)
casen$altobajoth <- ntile(casen$ytotcorh, 2)

#mononuclear household selection
sum(is.na(casen$nucleo))
mononuc <- casen[which(casen$nucleo == 1),]
sum(is.na(mononuc$nucleo))

#women selection that are head or partner of the head of the household and have between 45 and 50 years old
wom <- mononuc[which(mononuc$sexo == "Mujer"),]
#45-49 years old selection
wom4549 <- wom[which(wom$edad >= 45 & wom$edad <= 49),]
#head of the household selection
wom4549boss <- wom4549[which(wom4549$pco1 == "Jefe(a) de hogar"),]
wom4549boss$s5[which(wom4549boss$s5 == "No Sabe")] <- NA 
wom4549boss$s6[which(wom4549boss$s6 == "No sabe / No recuerda")] <- NA
sum(is.na(wom4549boss$edad))
sum(is.na(wom4549boss$s5))
sum(is.na(wom4549boss$s6))
which(colnames(wom4549boss)=="s5")
which(colnames(wom4549boss)=="s6")
wom4549boss <- wom4549boss[complete.cases(wom4549boss[,151:152]),]
#partner of the head of the household
wom4549part <- wom4549[which(wom4549$pco1 == "Esposo(a) o pareja"),]
wom4549part$s5[which(wom4549part$s5 == "No Sabe")] <- NA 
wom4549part$s6[which(wom4549part$s6 == "No sabe / No recuerda")] <- NA
sum(is.na(wom4549part$edad))
sum(is.na(wom4549part$s5))
sum(is.na(wom4549part$s6))
wom4549part <- wom4549part[complete.cases(wom4549part[,151:152]),]
#combining head and partner of the head of the household selections
wom4549f <- rbind(wom4549boss, wom4549part)
wom4549f$s5[which(wom4549f$s5 == "No Sabe")] <- NA 
wom4549f$s6[which(wom4549f$s6 == "No sabe / No recuerda")] <- NA
sum(is.na(wom4549f$edad))
sum(is.na(wom4549f$s5))
sum(is.na(wom4549f$s6))
wom4549f <- wom4549f[complete.cases(wom4549f[,151:152]),]

#Children selection of mononuclear households
#children of both partners in the household selection
chboth <- mononuc[which(mononuc$pco1 == "Hijo(a) de ambos"),]
sum(is.na(chboth$pco1))
#children of son of the head of the household selection
chboss <- mononuc[which(mononuc$pco1 == "Hijo(a) s?lo del jefe"),]
sum(is.na(chboss$pco1))
#children of the partner of the household selection
chpart <- mononuc[which(mononuc$pco1 == "Hijo(a) s?lo del esposo(a) o pareja"),]
sum(is.na(chpart$pco1))
#combination of children selections
child <- rbind(chboth, chboss, chpart)

#Selection of children of women in the cohort
childwom4549 <- child[which((child$folio %in% wom4549f$folio) == TRUE),]

#Selection of youngest children of women in the cohort
childwom4549byfolio <- group_by(childwom4549, folio)
ychild <- filter(childwom4549byfolio, edad == min(edad))
ychild <- data.frame(ychild)
ychild <- ychild[order(ychild[,1]),]
sum(is.na(ychild$edad))
#Selection of women and youngest children of the cohort
womych <- rbind(wom4549f, ychild)
womych <- womych[order(womych[,1]),]
summary(womych$s5)
summary(womych$s6)


# #Number of children (s5) into numeric -----------------------------------

womych$s5[which(womych$s5 == "No sabe / No recuerda")] <- NA
womych$s5[which(womych$s5 == "No sabe")] <- NA
summary(womych$s5)
is.factor(womych$s5)
womych$s5 <- as.numeric(levels(womych$s5))[womych$s5]
is.numeric(womych$s5)


# #Age at first birth (s6) into numeric -----------------------------------

womych$s6[which(womych$s6 == "No sabe / No recuerda")] <- NA
womych$s6[which(womych$s6 == "No sabe")] <- NA
summary(womych$s6)
is.factor(womych$s6)
womych$s6 <- as.numeric(levels(womych$s6))[womych$s6]
is.numeric(womych$s6)


# #Age at last birth ------------------------------------------------------


womych <- arrange(womych, folio, desc(pco1))
womych <- group_by(womych, folio)
womych <- mutate(womych, euh = edad - lag(edad))
summary(womych$euh)
sum(is.na(womych$euh))


# #Interbirth intervals ---------------------------------------------------


womych <- mutate(womych, ien = (euh - s6)/s5)
summary(womych$ien)
womych$euh
womych$ien
sum(is.na(womych$ien))
womych <- data.frame(womych)


# #Modes of parity --------------------------------------------------------


womych <- mutate(womych, rd = s5/(euh - s6))
summary(womych$rd)
sum(is.na(womych$rd))
womych <- data.frame(womych)


# #Cleaning final cohort --------------------------------------------------


sum(is.na(womych$s5))
sum(is.na(womych$s6))
sum(is.na(womych$euh))
sum(is.na(womych$ien))
sum(is.na(womych$rd))
fcohort <- womych[complete.cases(womych[,c("s5","s6","euh","ien","rd")]),]
fcohort <- fcohort[which(fcohort$edad >= 45 & fcohort$edad <= 49),]
fcohort <- fcohort[which(fcohort$euh >= fcohort$s6),]
fcohort <- fcohort[which(fcohort$r1a == "Chilena (exclusivamente)" | fcohort$r1a == "Chilena y otra (doble nacionalidad)"),]
summary(fcohort$r1a)
fcohort$r1a <- droplevels(fcohort$r1a)
fcohort$rd[fcohort$s6 == fcohort$euh & fcohort$s5 == 1] <- 0
fcohort <- fcohort[which(fcohort$rd != Inf),]
fcohort <- fcohort[which(fcohort$rd <= 1),]


# #Selection of childless women -------------------------------------------


nochild <- wom4549[which(wom4549$s5 ==0),]
nochild$s5[which(nochild$s5 == "No sabe / No recuerda")] <- NA
nochild$s5[which(nochild$s5 == "No sabe")] <- NA
summary(nochild$s5)
is.factor(nochild$s5)
nochild$s5 <- as.numeric(levels(nochild$s5))[nochild$s5]
is.numeric(nochild$s5)
is.factor(nochild$s6)
nochild$s6 <- as.numeric(levels(nochild$s6))[nochild$s6]
is.numeric(nochild$s6)
summary(nochild$s6)
nochild$euh <- NA
nochild$ien <- NA
nochild$rd <- NA
nochild <- nochild[which(nochild$r1a == "Chilena (exclusivamente)" | nochild$r1a == "Chilena y otra (doble nacionalidad)"),]
summary(nochild$r1a)
nochild$r1a <- droplevels(nochild$r1a)
nochild <- nochild[which(nochild$pco1 == "Jefe(a) de hogar" | nochild$pco1 == "Esposo(a) o pareja"),]
summary(nochild$pco1)
nochild$pco1 <- droplevels(nochild$pco1)

# #Final cohort database --------------------------------------------------


fcohort <- rbind(fcohort,nochild)
sum(is.na(fcohort$edad))
summary(fcohort$edad)
sum(is.na(fcohort$s5))
summary(fcohort$s5)
sum(is.na(fcohort$s6)) 
summary(fcohort$s6)
sum(is.na(fcohort$euh))
summary(fcohort$euh)
sum(is.na(fcohort$ien)) 
summary(fcohort$ien)
sum(is.na(fcohort$rd)) 
summary(fcohort$rd)


# #2.- Socioeconomic groups -----------------------------------------------


test <- fcohort

# #Geographic and ID variables -----
#region
is.factor(test$region)
sum(is.na(test$region))
levels(test$region)
summary(test$region)
summary(aov(s5~region, data=test))
summary(aov(s6~region, data=test))
summary(aov(euh~region, data=test))
summary(aov(ien~region, data=test))
summary(aov(rd~region, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~region, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ region")
plot(s6~region, data=test, ylab=c("AFR"), main="AFR ~ region")
plot(euh~region, data=test, ylab=c("ALR"), main="ALR ~ region")
plot(ien~region, data=test, ylab=c("IBI"), main="IBI ~ region")
plot(rd~region, data=test, ylab=c("RD"), main="RD ~ region")
#comuna
is.factor(test$comuna)
sum(is.na(test$comuna))
levels(test$comuna)
test$comuna[which(test$comuna == "No contesta")] <- NA
sum(is.na(test$comuna))
summary(test$comuna)
summary(aov(s5~comuna, data=test))
summary(aov(s6~comuna, data=test))
summary(aov(euh~comuna, data=test))
summary(aov(ien~comuna, data=test))
summary(aov(rd~comuna, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~comuna, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ comuna")
plot(s6~comuna, data=test, ylab=c("AFR"), main="AFR ~ comuna")
plot(euh~comuna, data=test, ylab=c("ALR"), main="ALR ~ comuna")
plot(ien~comuna, data=test, ylab=c("IBI"), main="IBI ~ comuna")
plot(rd~comuna, data=test, ylab=c("RD"), main="RD ~ comuna")
#zona
is.factor(test$zona)
sum(is.na(test$zona))
levels(test$zona)
summary(test$zona)
summary(aov(s5~zona, data=test))
summary(aov(s6~zona, data=test))
summary(aov(euh~zona, data=test))
summary(aov(ien~zona, data=test))
summary(aov(rd~zona, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~zona, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ zona")
plot(s6~zona, data=test, ylab=c("AFR"), main="AFR ~ zona")
plot(euh~zona, data=test, ylab=c("ALR"), main="ALR ~ zona")
plot(ien~zona, data=test, ylab=c("IBI"), main="IBI ~ zona")
plot(rd~zona, data=test, ylab=c("RD"), main="RD ~ zona")
#relationship with the head of the household
is.factor(test$pco1)
sum(is.na(test$pco1))
levels(test$pco1)
summary(test$pco1)
test$pco1 <- droplevels(test$pco1)
summary(test$pco1)
summary(aov(s5~pco1, data=test))
summary(aov(s6~pco1, data=test))
summary(aov(euh~pco1, data=test))
summary(aov(ien~pco1, data=test))
summary(aov(rd~pco1, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~pco1, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ pco1")
plot(s6~pco1, data=test, ylab=c("AFR"), main="AFR ~ pco1")
plot(euh~pco1, data=test, ylab=c("ALR"), main="ALR ~ pco1")
plot(ien~pco1, data=test, ylab=c("IBI"), main="IBI ~ pco1")
plot(rd~pco1, data=test, ylab=c("RD"), main="RD ~ pco1")

## Education ------
#years of schooling
is.numeric(test$ESC)
sum(is.na(test$ESC))
summary(test$ESC)
test <- test[complete.cases(test[,c("ESC")]),]
summary(test$ESC)
summary(lm(s5~ESC, data=test))
summary(lm(s6~ESC, data=test))
summary(lm(euh~ESC, data=test))
summary(lm(ien~ESC, data=test))
summary(lm(rd~ESC, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~ESC, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ESC")
plot(s6~ESC, data=test, ylab=c("AFR"), main="AFR ~ ESC")
plot(euh~ESC, data=test, ylab=c("ALR"), main="ALR ~ ESC")
plot(ien~ESC, data=test, ylab=c("IBI"), main="IBI ~ ESC")
plot(rd~ESC, data=test, ylab=c("RD"), main="RD ~ ESC")
#educational level
is.factor(test$educ)
sum(is.na(test$educ))
levels(test$educ)
summary(test$educ)
test$educ[which(test$educ == "NS/NR")] <- NA
summary(test$educ)
test <- test[complete.cases(test[,c("educ")]),]
sum(is.na(test$educ))
test$educ <- droplevels(test$educ)
summary(test$educ)
summary(aov(s5~educ, data=test))
summary(aov(s6~educ, data=test))
summary(aov(euh~educ, data=test))
summary(aov(ien~educ, data=test))
summary(aov(rd~educ, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~educ, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ educ")
plot(s6~educ, data=test, ylab=c("AFR"), main="AFR ~ educ")
plot(euh~educ, data=test, ylab=c("ALR"), main="ALR ~ educ")
plot(ien~educ, data=test, ylab=c("IBI"), main="IBI ~ educ")
plot(rd~educ, data=test, ylab=c("RD"), main="RD ~ educ")

## Income ----
#household salary
is.numeric(test$ytrabajoCorh)
sum(is.na(test$ytrabajoCorh))
summary(test$ytrabajoCorh)
summary(lm(s5~ytrabajoCorh, data=test))
summary(lm(s6~ytrabajoCorh, data=test))
summary(lm(euh~ytrabajoCorh, data=test))
summary(lm(ien~ytrabajoCorh, data=test))
summary(lm(rd~ytrabajoCorh, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(ytrabajoCorh), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ytrabajoCorh")
plot(s6~log(ytrabajoCorh), data=test, ylab=c("AFR"), main="AFR ~ ytrabajoCorh")
plot(euh~log(ytrabajoCorh), data=test, ylab=c("ALR"), main="ALR ~ ytrabajoCorh")
plot(ien~log(ytrabajoCorh), data=test, ylab=c("IBI"), main="IBI ~ ytrabajoCorh")
plot(rd~log(ytrabajoCorh), data=test, ylab=c("RD"), main="RD ~ ytrabajoCorh")
#household other autonomous incomes
is.numeric(test$yoautCorh)
sum(is.na(test$yoautCorh))
summary(test$yoautCorh)
summary(lm(s5~yoautCorh, data=test))
summary(lm(s6~yoautCorh, data=test))
summary(lm(euh~yoautCorh, data=test))
summary(lm(ien~yoautCorh, data=test))
summary(lm(rd~yoautCorh, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(yoautCorh), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ yoautCorh")
plot(s6~log(yoautCorh), data=test, ylab=c("AFR"), main="AFR ~ yoautCorh")
plot(euh~log(yoautCorh), data=test, ylab=c("ALR"), main="ALR ~ yoautCorh")
plot(ien~log(yoautCorh), data=test, ylab=c("IBI"), main="IBI ~ yoautCorh")
plot(rd~log(yoautCorh), data=test, ylab=c("RD"), main="RD ~ yoautCorh")
#household autonomous incomes
is.numeric(test$yautcorh)
sum(is.na(test$yautcorh))
summary(test$yautcorh)
summary(lm(s5~yautcorh, data=test))
summary(lm(s6~yautcorh, data=test))
summary(lm(euh~yautcorh, data=test))
summary(lm(ien~yautcorh, data=test))
summary(lm(rd~yautcorh, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(yautcorh), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ yautcorh")
plot(s6~log(yautcorh), data=test, ylab=c("AFR"), main="AFR ~ yautcorh")
plot(euh~log(yautcorh), data=test, ylab=c("ALR"), main="ALR ~ yautcorh")
plot(ien~log(yautcorh), data=test, ylab=c("IBI"), main="IBI ~ yautcorh")
plot(rd~log(yautcorh), data=test, ylab=c("RD"), main="RD ~ yautcorh")
#subsidies
is.numeric(test$ysubh)
sum(is.na(test$ysubh))
summary(test$ysubh)
summary(lm(s5~ysubh, data=test))
summary(lm(s6~ysubh, data=test))
summary(lm(euh~ysubh, data=test))
summary(lm(ien~ysubh, data=test))
summary(lm(rd~ysubh, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(ysubh), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ysubh")
plot(s6~log(ysubh), data=test, ylab=c("AFR"), main="AFR ~ ysubh")
plot(euh~log(ysubh), data=test, ylab=c("ALR"), main="ALR ~ ysubh")
plot(ien~log(ysubh), data=test, ylab=c("IBI"), main="IBI ~ ysubh")
plot(rd~log(ysubh), data=test, ylab=c("RD"), main="RD ~ ysubh")
#total household income
is.numeric(test$ytotcorh)
sum(is.na(test$ytotcorh))
summary(test$ytotcorh)
summary(lm(s5~ytotcorh, data=test))
summary(lm(s6~ytotcorh, data=test))
summary(lm(euh~ytotcorh, data=test))
summary(lm(ien~ytotcorh, data=test))
summary(lm(rd~ytotcorh, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(ytotcorh), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ytotcorh")
plot(s6~log(ytotcorh), data=test, ylab=c("AFR"), main="AFR ~ ytotcorh")
plot(euh~log(ytotcorh), data=test, ylab=c("ALR"), main="ALR ~ ytotcorh")
plot(ien~log(ytotcorh), data=test, ylab=c("IBI"), main="IBI ~ ytotcorh")
plot(rd~log(ytotcorh), data=test, ylab=c("RD"), main="RD ~ ytotcorh")
#per capita household total income
is.numeric(test$ypchtot)
sum(is.na(test$ypchtot))
summary(test$ypchtot)
summary(lm(s5~ypchtot, data=test))
summary(lm(s6~ypchtot, data=test))
summary(lm(euh~ypchtot, data=test))
summary(lm(ien~ypchtot, data=test))
summary(lm(rd~ypchtot, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~log(ypchtot), data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ypchtot")
plot(s6~log(ypchtot), data=test, ylab=c("AFR"), main="AFR ~ ypchtot")
plot(euh~log(ypchtot), data=test, ylab=c("ALR"), main="ALR ~ ypchtot")
plot(ien~log(ypchtot), data=test, ylab=c("IBI"), main="IBI ~ ypchtot")
plot(rd~log(ypchtot), data=test, ylab=c("RD"), main="RD ~ ypchtot")

## Healthcare and pension ----

#healthcare system
is.factor(test$s14)
sum(is.na(test$s14))
levels(test$s14)
summary(test$s14)
test$s14[which(test$s14 == "No sabe")] <- NA
summary(test$s14)
test <- test[complete.cases(test[,c("s14")]),]
sum(is.na(test$s14))
test$s14 <- droplevels(test$s14)
summary(test$s14)
summary(aov(s5~s14, data=test))
summary(aov(s6~s14, data=test))
summary(aov(euh~s14, data=test))
summary(aov(ien~s14, data=test))
summary(aov(rd~s14, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~s14, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ s14")
plot(s6~s14, data=test, ylab=c("AFR"), main="AFR ~ s14")
plot(euh~s14, data=test, ylab=c("ALR"), main="ALR ~ s14")
plot(ien~s14, data=test, ylab=c("IBI"), main="IBI ~ s14")
plot(rd~s14, data=test, ylab=c("RD"), main="RD ~ s14")
#pension system
is.factor(test$o29)
sum(is.na(test$o29))
levels(test$o29)
summary(test$o29)
test$o29[which(test$o29 == "No sabe")] <- NA
summary(test$o29)
test <- test[complete.cases(test[,c("o29")]),]
sum(is.na(test$o29))
test$o29 <- droplevels(test$o29)
summary(test$o29)
summary(aov(s5~o29, data=test))
summary(aov(s6~o29, data=test))
summary(aov(euh~o29, data=test))
summary(aov(ien~o29, data=test))
summary(aov(rd~o29, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~o29, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ o29")
plot(s6~o29, data=test, ylab=c("AFR"), main="AFR ~ o29")
plot(euh~o29, data=test, ylab=c("ALR"), main="ALR ~ o29")
plot(ien~o29, data=test, ylab=c("IBI"), main="IBI ~ o29")
plot(rd~o29, data=test, ylab=c("RD"), main="RD ~ o29")

## Ethnic identity ----
#ethnic group
is.factor(test$r6)
sum(is.na(test$r6))
levels(test$r6)
summary(test$r6)
test$r6[which(test$r6 == "NS/NR")] <- NA
summary(test$r6)
test <- test[complete.cases(test[,c("r6")]),]
sum(is.na(test$r6))
test$r6 <- droplevels(test$r6)
summary(test$r6)
summary(aov(s5~r6, data=test))
summary(aov(s6~r6, data=test))
summary(aov(euh~r6, data=test))
summary(aov(ien~r6, data=test))
summary(aov(rd~r6, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~r6, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ r6")
plot(s6~r6, data=test, ylab=c("AFR"), main="AFR ~ r6")
plot(euh~r6, data=test, ylab=c("ALR"), main="ALR ~ r6")
plot(ien~r6, data=test, ylab=c("IBI"), main="IBI ~ r6")
plot(rd~r6, data=test, ylab=c("RD"), main="RD ~ r6")
#ethnicity
is.factor(test$r6)
sum(is.na(test$r6))
levels(test$r6)
summary(test$r6)
test$ethnic[which(test$r6 == "Aymara")] <- "Sí"
test$ethnic[which(test$r6 == "Quechua")] <- "Sí"
test$ethnic[which(test$r6 == "Mapuche")] <- "Sí"
test$ethnic[which(test$r6 == "Atacameño (Likán Antai)")] <- "Sí"
test$ethnic[which(test$r6 == "Coya")] <- "Sí"
test$ethnic[which(test$r6 == "Kawésqar (Alacalufes)")] <- "Sí"
test$ethnic[which(test$r6 == "Diaguita")] <- "Sí"
test$ethnic[which(test$r6 == "No pertenece a ningún pueblo indígena")] <- "No"
is.factor(test$ethnic)
test$ethnic <- as.factor(test$ethnic)
sum(is.na(test$ethnic))
levels(test$ethnic)
summary(test$ethnic)
summary(aov(s5~ethnic, data=test))
summary(aov(s6~ethnic, data=test))
summary(aov(euh~ethnic, data=test))
summary(aov(ien~ethnic, data=test))
summary(aov(rd~ethnic, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~ethnic, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ ethnic")
plot(s6~ethnic, data=test, ylab=c("AFR"), main="AFR ~ ethnic")
plot(euh~ethnic, data=test, ylab=c("ALR"), main="ALR ~ ethnic")
plot(ien~ethnic, data=test, ylab=c("IBI"), main="IBI ~ ethnic")
plot(rd~ethnic, data=test, ylab=c("RD"), main="RD ~ ethnic")

##Living conditions and property ownership ----
#mobile phone
is.factor(test$r19)
sum(is.na(test$r19))
levels(test$r19)
summary(test$r19)
test$r19[which(test$r19 == "NS/NR")] <- NA
summary(test$r19)
test <- test[complete.cases(test[,c("r19")]),]
sum(is.na(test$r19))
test$r19 <- droplevels(test$r19)
summary(test$r19)
summary(aov(s5~r19, data=test))
summary(aov(s6~r19, data=test))
summary(aov(euh~r19, data=test))
summary(aov(ien~r19, data=test))
summary(aov(rd~r19, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~r19, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ r19")
plot(s6~r19, data=test, ylab=c("AFR"), main="AFR ~ r19")
plot(euh~r19, data=test, ylab=c("ALR"), main="ALR ~ r19")
plot(ien~r19, data=test, ylab=c("IBI"), main="IBI ~ r19")
plot(rd~r19, data=test, ylab=c("RD"), main="RD ~ r19")
#type of house
is.factor(test$v1)
sum(is.na(test$v1))
levels(test$v1)
summary(test$v1)
test$v1 <- droplevels(test$v1)
summary(test$v1)
summary(aov(s5~v1, data=test))
summary(aov(s6~v1, data=test))
summary(aov(euh~v1, data=test))
summary(aov(ien~v1, data=test))
summary(aov(rd~v1, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v1, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v1")
plot(s6~v1, data=test, ylab=c("AFR"), main="AFR ~ v1")
plot(euh~v1, data=test, ylab=c("ALR"), main="ALR ~ v1")
plot(ien~v1, data=test, ylab=c("IBI"), main="IBI ~ v1")
plot(rd~v1, data=test, ylab=c("RD"), main="RD ~ v1")
#walls material
is.factor(test$v2)
sum(is.na(test$v2))
levels(test$v2)
summary(test$v2)
test$v2[which(test$v2 == "NS/NR")] <- NA
summary(test$v2)
test <- test[complete.cases(test[,c("v2")]),]
sum(is.na(test$v2))
test$v2 <- droplevels(test$v2)
summary(test$v2)
summary(aov(s5~v2, data=test))
summary(aov(s6~v2, data=test))
summary(aov(euh~v2, data=test))
summary(aov(ien~v2, data=test))
summary(aov(rd~v2, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v2, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v2")
plot(s6~v2, data=test, ylab=c("AFR"), main="AFR ~ v2")
plot(euh~v2, data=test, ylab=c("ALR"), main="ALR ~ v2")
plot(ien~v2, data=test, ylab=c("IBI"), main="IBI ~ v2")
plot(rd~v2, data=test, ylab=c("RD"), main="RD ~ v2")
#floor material
is.factor(test$v4)
sum(is.na(test$v4))
levels(test$v4)
summary(test$v4)
test$v4 <- droplevels(test$v4)
summary(test$v4)
summary(aov(s5~v4, data=test))
summary(aov(s6~v4, data=test))
summary(aov(euh~v4, data=test))
summary(aov(ien~v4, data=test))
summary(aov(rd~v4, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v4, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v4")
plot(s6~v4, data=test, ylab=c("AFR"), main="AFR ~ v4")
plot(euh~v4, data=test, ylab=c("ALR"), main="ALR ~ v4")
plot(ien~v4, data=test, ylab=c("IBI"), main="IBI ~ v4")
plot(rd~v4, data=test, ylab=c("RD"), main="RD ~ v4")
#roof material
is.factor(test$v6)
sum(is.na(test$v6))
levels(test$v6)
summary(test$v6)
test$v6[which(test$v6 == "NS/NR")] <- NA
summary(test$v6)
test <- test[complete.cases(test[,c("v6")]),]
sum(is.na(test$v6))
test$v6 <- droplevels(test$v6)
summary(test$v6)
summary(aov(s5~v6, data=test))
summary(aov(s6~v6, data=test))
summary(aov(euh~v6, data=test))
summary(aov(ien~v6, data=test))
summary(aov(rd~v6, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v6, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v6")
plot(s6~v6, data=test, ylab=c("AFR"), main="AFR ~ v6")
plot(euh~v6, data=test, ylab=c("ALR"), main="ALR ~ v6")
plot(ien~v6, data=test, ylab=c("IBI"), main="IBI ~ v6")
plot(rd~v6, data=test, ylab=c("RD"), main="RD ~ v6")
#land ownership
is.factor(test$v9)
sum(is.na(test$v9))
levels(test$v9)
summary(test$v9)
test$v9 <- droplevels(test$v9)
summary(test$v9)
summary(aov(s5~v9, data=test))
summary(aov(s6~v9, data=test))
summary(aov(euh~v9, data=test))
summary(aov(ien~v9, data=test))
summary(aov(rd~v9, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v9, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v9")
plot(s6~v9, data=test, ylab=c("AFR"), main="AFR ~ v9")
plot(euh~v9, data=test, ylab=c("ALR"), main="ALR ~ v9")
plot(ien~v9, data=test, ylab=c("IBI"), main="IBI ~ v9")
plot(rd~v9, data=test, ylab=c("RD"), main="RD ~ v9")
#range of household square meters
is.factor(test$v11)
sum(is.na(test$v11))
levels(test$v11)
summary(test$v11)
test$v11[which(test$v11 == "No sabe")] <- NA
summary(test$v11)
test <- test[complete.cases(test[,c("v11")]),]
sum(is.na(test$v11))
test$v11 <- droplevels(test$v11)
summary(test$v11)
summary(aov(s5~v11, data=test))
summary(aov(s6~v11, data=test))
summary(aov(euh~v11, data=test))
summary(aov(ien~v11, data=test))
summary(aov(rd~v11, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v11, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v11")
plot(s6~v11, data=test, ylab=c("AFR"), main="AFR ~ v11")
plot(euh~v11, data=test, ylab=c("ALR"), main="ALR ~ v11")
plot(ien~v11, data=test, ylab=c("IBI"), main="IBI ~ v11")
plot(rd~v11, data=test, ylab=c("RD"), main="RD ~ v11")
#house ownership
is.factor(test$v12)
sum(is.na(test$v12))
levels(test$v12)
summary(test$v12)
test$v12 <- droplevels(test$v12)
summary(test$v12)
summary(aov(s5~v12, data=test))
summary(aov(s6~v12, data=test))
summary(aov(euh~v12, data=test))
summary(aov(ien~v12, data=test))
summary(aov(rd~v12, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v12, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v12")
plot(s6~v12, data=test, ylab=c("AFR"), main="AFR ~ v12")
plot(euh~v12, data=test, ylab=c("ALR"), main="ALR ~ v12")
plot(ien~v12, data=test, ylab=c("IBI"), main="IBI ~ v12")
plot(rd~v12, data=test, ylab=c("RD"), main="RD ~ v12")
#water supply
is.factor(test$v23)
sum(is.na(test$v23))
levels(test$v23)
summary(test$v23)
test$v23[which(test$v23 == "NS/NR")] <- NA
summary(test$v23)
test <- test[complete.cases(test[,c("v23")]),]
sum(is.na(test$v23))
test$v23 <- droplevels(test$v23)
summary(test$v23)
summary(aov(s5~v23, data=test))
summary(aov(s6~v23, data=test))
summary(aov(euh~v23, data=test))
summary(aov(ien~v23, data=test))
summary(aov(rd~v23, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v23, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v23")
plot(s6~v23, data=test, ylab=c("AFR"), main="AFR ~ v23")
plot(euh~v23, data=test, ylab=c("ALR"), main="ALR ~ v23")
plot(ien~v23, data=test, ylab=c("IBI"), main="IBI ~ v23")
plot(rd~v23, data=test, ylab=c("RD"), main="RD ~ v23")
#water distribution system
is.factor(test$v24)
sum(is.na(test$v24))
levels(test$v24)
summary(test$v24)
test$v24 <- droplevels(test$v24)
summary(test$v24)
summary(aov(s5~v24, data=test))
summary(aov(s6~v24, data=test))
summary(aov(euh~v24, data=test))
summary(aov(ien~v24, data=test))
summary(aov(rd~v24, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v24, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v24")
plot(s6~v24, data=test, ylab=c("AFR"), main="AFR ~ v24")
plot(euh~v24, data=test, ylab=c("ALR"), main="ALR ~ v24")
plot(ien~v24, data=test, ylab=c("IBI"), main="IBI ~ v24")
plot(rd~v24, data=test, ylab=c("RD"), main="RD ~ v24")
#sewage system
is.factor(test$v25)
sum(is.na(test$v25))
levels(test$v25)
summary(test$v25)
test$v25[which(test$v25 == "NS/NR")] <- NA
summary(test$v25)
test <- test[complete.cases(test[,c("v25")]),]
sum(is.na(test$v25))
test$v25 <- droplevels(test$v25)
summary(test$v25)
summary(aov(s5~v25, data=test))
summary(aov(s6~v25, data=test))
summary(aov(euh~v25, data=test))
summary(aov(ien~v25, data=test))
summary(aov(rd~v25, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v25, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v25")
plot(s6~v25, data=test, ylab=c("AFR"), main="AFR ~ v25")
plot(euh~v25, data=test, ylab=c("ALR"), main="ALR ~ v25")
plot(ien~v25, data=test, ylab=c("IBI"), main="IBI ~ v25")
plot(rd~v25, data=test, ylab=c("RD"), main="RD ~ v25")
#electricity system
is.factor(test$v26)
sum(is.na(test$v26))
levels(test$v26)
summary(test$v26)
test$v26[which(test$v26 == "NS/NR")] <- NA
summary(test$v26)
test <- test[complete.cases(test[,c("v26")]),]
sum(is.na(test$v26))
test$v26 <- droplevels(test$v26)
summary(test$v26)
summary(aov(s5~v26, data=test))
summary(aov(s6~v26, data=test))
summary(aov(euh~v26, data=test))
summary(aov(ien~v26, data=test))
summary(aov(rd~v26, data=test))
layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
plot(s5~v26, data=test, ylab=c("Nº Offs."), main="Nº Offs. ~ v26")
plot(s6~v26, data=test, ylab=c("AFR"), main="AFR ~ v26")
plot(euh~v26, data=test, ylab=c("ALR"), main="ALR ~ v26")
plot(ien~v26, data=test, ylab=c("IBI"), main="IBI ~ v26")
plot(rd~v26, data=test, ylab=c("RD"), main="RD ~ v26")

#check correlation of variables
# #install.packages("psych")
library(psych)
#income variables
#Pearson
pairs.panels(test[,c("ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot")], method = "pearson",hist.col = "light grey", density = TRUE,ellipses = F, lm= T,cex=1, main="Pearson correlation")
#Spearman
pairs.panels(test[,c("ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot")], method = "spearman",hist.col = "light grey", density = TRUE,ellipses = F, lm=T, cex=1, main="Spearman correlation")
#education variables
summary(aov(ESC~educ, test))
#household variables
chisq.test(test$v2, test$v6, simulate.p.value = T)
chisq.test(test$v9, test$v12, simulate.p.value = T)
chisq.test(test$v24, test$v25, simulate.p.value = T)
chisq.test(test$v24, test$v26, simulate.p.value = T)
chisq.test(test$v25, test$v26, simulate.p.value = T)
#other combinations
chisq.test(test$region, test$zona, simulate.p.value = T)
chisq.test(test$region, test$comuna, simulate.p.value = T)
#Pearson
pairs.panels(test[,c("ESC","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot")], method = "pearson",hist.col = "light grey", density = TRUE,ellipses = F, lm= T,cex=1, main="Pearson correlation")
#Spearman
pairs.panels(test[,c("ESC","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot")], method = "spearman",hist.col = "light grey", density = TRUE,ellipses = F, lm=T, cex=1, main="Spearman correlation")
summary(aov(ytotcorh~v2+v6+v9+v12+v24+v25+v26, test))

#we use region, zona, and ethnic group to control for the other socioeconomic variables

# #FAMD to get socioeconomic scores (is a PCA for mixed data) -------------

#install FactoMineR
install.packages("FactoMineR")
library(FactoMineR)
#FAMD with all the SES variables
summary(test[,c("ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","o29","r19","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")])
famd1 <- FAMD(test[,c("ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","o29","r19","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")])
summary(famd1)
hist(famd1$ind$coord[,1])
which(famd1$ind$coord[,1] > 17)
#FAMD without outliers
famd2 <-FAMD(test[which(famd1$ind$coord[,1] < 17),c("ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","o29","r19","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")])
summary(famd2)
hist(famd2$ind$coord[,1])
famdsub <- famd2$ind$coord[,1]

# #Standarization, socioeconomic score and deciles ------------------------


q <- scale(famdsub, center = T, scale = T)
par(mfrow=c(1,1))
hist(q)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
final <- test[ which(famd1$ind$coord[,1] < 17),c("folio","edad", "s5", "s6", "euh", "ien","rd","region","comuna","zona","r6","ethnic","ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","o29","r19","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")]
final$ses <- range01(q)
final$ses <- as.vector(final$ses)
hist(final$ses, breaks = 100)
#Quantiles
final$ses10 <- ntile(final$ses, 10)
final$ses5 <- ntile(final$ses, 5)
final$ses4 <- ntile(final$ses, 4)
final$ses3 <- ntile(final$ses, 3)
final$ses2 <- ntile(final$ses, 2)


# #3.- Regression models ----------------------------------------------------


#install package lme4
install.packages("lme4")
library(lme4)
#"AER" package
install.packages("AER")
library(AER)

# #Nº of Offspring --------------------------------------------------------

#run a generalized mixed-effect model
#Poisson regression only with ses
lmes5.1 <- glm(s5~sqrt(ses),data=final, family = poisson(link = "log"))
summary(lmes5.1)
#Poisson regression with multiple variables
lmes5.2 <- glm(s5~sqrt(ses)+region+zona+ethnic,data=final, family = poisson(link = "log"))
summary(lmes5.2)
#Poisson regression with multiple variables after step AIC
step(lmes5.2, direction="both")
lmes5.3 <- glm(s5~sqrt(ses)+region,data=final, family = poisson(link = "log"))
summary(lmes5.3)
#generalized Poisson mixed-effect model
lmes5.4 <- glmer(s5~sqrt(ses)+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic),data=final, family = poisson(link = "log"))
summary(lmes5.4)
#generalized Poisson mixed-effect model with step AIC
lmes5.5 <- glmer(s5~sqrt(ses)+(1+ses|region),data=final, family = poisson(link = "log"))
summary(lmes5.5)

#AIC
AIC(lmes5.1,lmes5.2,lmes5.3,lmes5.4, lmes5.5)
#lmes5.3 is the best model
glm.diag.plots(lmes5.3)
dispersiontest(lmes5.3,trafo=1)
#not overdispersed

#Negative binomial regression models
#Poisson regression only with ses
nblmes5.1 <- glm.nb(s5~ses,data=final,link=log)
summary(nblmes5.1)
#Poisson regression with multiple variables
nblmes5.2 <- glm.nb(s5~ses+region+zona+ethnic,data=final,link=log)
summary(nblmes5.2)
#Poisson regression with multiple variables after step AIC
step(nblmes5.2, direction="both")
nblmes5.3 <- glm.nb(s5~ses+comuna+zona+r6,data=final,link=log)
summary(nblmes5.3)
#generalized Poisson mixed-effect model
nblmes5.4 <- glmer.nb(s5~ses+(1+ses|region)+(1+ses|comuna)+(1+ses|zona)+(1+ses|r6),data=final)
summary(nblmes5.4)
#generalized Poisson mixed-effect model with step AIC
nblmes5.5 <- glmer.nb(s5~ses+(1+ses|comuna)+(1+ses|zona)+(1+ses|r6),data=final)
summary(nblmes5.5)
#Poisson regression with multiple variables
nblmes5.6 <- glm.nb(s5~ses+region,data=final,link=log)
summary(nblmes5.6)
#Poisson regression with multiple variables
nblmes5.7 <- glmer.nb(s5~ses+(1+ses|region),data=final)
summary(nblmes5.7)
#AIC
AIC(nblmes5.1,nblmes5.2,nblmes5.3,nblmes5.4,nblmes5.5,lmes5.3)
#nblmes5.6 and lmes5.3 are the best models
glm.diag.plots(nblmes5.3)
glm.diag.plots(lmes5.3)

#check models
#Poisson model lmes5.3
#test and graph the original count data
library(MASS)
library(vcd)
fit_s5 <- goodfit(final$s5,type="poisson") 
summary(fit_s5) 
rootogram(fit_s5)
Ord_plot(final$s5)
distplot(final$s5, type="poisson")
#good-of-fitness measure
anova(lmes5.3,test = "Chisq")
#check under/overdispersion
deviance(lmes5.3)/lmes5.3$df.residual
dispersiontest(lmes5.3,trafo=1)
#check influence points
influencePlot(lmes5.3)
#compare to zero inflated model
install.packages("pscl")
library(pscl)
lmes5.zinf <- zeroinfl(s5~ses+region,data=final, dist="poisson")
AIC(lmes5.3, lmes5.zinf)
#plot residuals
res <- residuals(lmes5.3, type="deviance")
plot(log(predict(lmes5.3)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
#diagnostic plots
plot(s5~ses, data=final) 
prs  <- predict(lmes5.3, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ final$ses, col="red")
points(pris$lwr  ~ final$ses, col="pink", pch=19)
points(pris$upr  ~ final$ses, col="pink", pch=19)
#Negative binomial model nblmes5.6
#test and graph the original count data
library(MASS)
library(vcd)
fit_s52 <- goodfit(final$s5,type="nbinom") 
summary(fit_s52) 
rootogram(fit_s52)
Ord_plot(final$s5)
distplot(final$s5, type="nbinom")
#good-of-fitness measure
anova(nblmes5.6,test = "Chisq")
#check under/overdispersion
deviance(nblmes5.6)/nblmes5.6$df.residual
dispersiontest(nblmes5.6,trafo=1)
#check influence points
influencePlot(nblmes5.6)
#compare to zero inflated model
install.packages("pscl")
library(pscl)
nblmes3.zinf <- zeroinfl(s5~sqrt(ses)+region,data=final, dist="negbin")
AIC(nblmes5.3, nblmes3.zinf)
#plot residuals
res <- residuals(lmes5.3, type="deviance")
plot(log(predict(lmes5.3)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
#diagnostic plots
plot(s5~ses, data=final) 
prs  <- predict(nblmes5.6, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ final$ses, col="red")
points(pris$lwr  ~ final$ses, col="grey", pch=19)
points(pris$upr  ~ final$ses, col="grey", pch=19)

#Use DHARMa
#Poisson model
install.packages("DHARMa")
library(DHARMa)
simulationOutput <- simulateResiduals(nblmes5.3, n = 250, use.u = T)
testResiduals(simulationOutput)

plotResiduals(simulationOutput)
hist(simulationOutput$scaledResiduals)
plot(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#Negative binomial model
install.packages("DHARMa")
library(DHARMa)
simulationOutput2 <- simulateResiduals(nblmes5.6, n = 250, use.u = T)
testResiduals(simulationOutput2)

plotResiduals(simulationOutput2)
hist(simulationOutput2$scaledResiduals)
plot(simulationOutput2)
testUniformity(simulationOutput2)
testDispersion(simulationOutput2)
testZeroInflation(simulationOutput2)

#add folio as random effect
final$folio <- as.numeric(final$folio)
#Poisson model
lmes5.3.1<- glmer(s5~ses+region+(1|folio),data=final, family = poisson(link = "log"))
summary(lmes5.3.1)
#Negative binomial model
nblmes5.6.1<- glm.nb(s5~ses+region+(1|folio),data=final)
summary(nblmes5.6.1)

# #NM --------------
#Let's check nblmes5.3 for assumptions, based in https://stats.stackexchange.com/questions/70558/diagnostic-plots-for-count-regression:
final$s5

library(MASS)
library(vcd)
explore_s5 <- goodfit(final$s5)
summary(explore_s5)
rootogram(explore_s5)
Ord_plot(final$s5)

distplot(final$s5, type = "nbinom")
distplot(final$s5, type = "poisson")

#So ngebin is much better fit.
summary(nblmes5.3)
anova(nblmes5.3, test = "Chisq")

deviance(nblmes5.3) / nblmes5.3$df.residual
#library(AER)
#dispersiontest(nblmes5.3)
influencePlot(nblmes5.3)

#Now for assumptions proper, let's see DHARMA's approach https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(DHARMa)
simulationOutput <- simulateResiduals(nblmes5.3, n = 250, use.u = T)
testResiduals(simulationOutput)


hist(simulationOutput$scaledResiduals)
plot(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

#Let's try something else
nblmes5.3.1 <- glm.nb(s5 ~ ses + region + zona + (1 |
                                                    folio), data = final)
summary(nblmes5.3.1)

simulationOutput <- simulateResiduals(nblmes5.3.1)
plot(simulationOutput)

# #Age at first reproduction ----------------------------------------------

#prepare data
sum(is.na(final$s6))
finals6 <- final[complete.cases(final$s6),]
#distribution
par(mfrow=c(1,1))
descdist(finals6$s6, discrete = F, boot = 500)
fits6 <- fitdist(log(finals6$s6), "norm")
summary(fits6)
plot(fits6)
#run a generalized mixed-effect model
#simple linear regression model
lmes6.1 <- lm(log(s6)~ses,data=finals6)
summary(lmes6.1)
#multiple linear regression model
lmes6.2 <- lm(log(s6)~ses+region+zona+ethnic,data=finals6)
summary(lmes6.2)
#multiple linear regression model with multiple variables after step AIC
step(lmes6.2, direction="both")
lmes6.3 <- lm(log(s6)~ses+region,data=finals6)
summary(lmes6.3)
#generalized mixed-effect model
lmes6.4 <- lmer(log(s6)~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic),data=finals6)
summary(lmes6.4)
#generalized mixed-effect model with step AIC variables
lmes6.5 <- lmer(log(s6)~ses+(1+ses|region),data=finals6)
summary(lmes6.5)
#AIC
AIC(lmes6.1,lmes6.2,lmes6.3, lmes6.4, lmes6.5)
#lmes6.3 is the best model
par(mfrow=c(2,2))
plot(lmes6.3)

# #Age at last reproduction -----------------------------------------------

#prepare data
sum(is.na(final$euh))
finaleuh <- final[complete.cases(final$euh),]
finaleuh$euh <- as.numeric(finaleuh$euh)
#distribution
par(mfrow=c(1,1))
descdist(finaleuh$euh, discrete = F, boot = 500)
fiteuh <- fitdist(finaleuh$euh, "norm")
summary(fiteuh)
plot(fiteuh)
#run a generalized mixed-effect model
#simple linear regression model
lmeeuh.1 <- lm(euh~ses, data=finaleuh)
summary(lmeeuh.1)
#multiple linear regression model
lmeeuh.2 <- lm(euh~ses+region+zona+ethnic, data=finaleuh)
summary(lmeeuh.2)
#model with multiple variables after step AIC
step(lmeeuh.2, direction="both")
lmeeuh.3<-lm(euh~ses+zona,data=finaleuh)
summary(lmeeuh.3)
#generalized mixed-effect model
lmeeuh.4 <- lmer(euh~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finaleuh)
summary(lmeeuh.4)
#generalized mixed-effect model with step AIC variables
lmeeuh.5 <- lmer(euh~ses+(1+ses|zona), data=finaleuh)
summary(lmeeuh.5)
#AIC
AIC(lmeeuh.1,lmeeuh.2,lmeeuh.3,lmeeuh.4,lmeeuh.5)
#lmeeuh.3 is the best model
par(mfrow=c(2,2))
plot(lmeeuh.3)

# #Interbirth intervals ---------------------------------------------------

#prepare data
sum(is.na(final$ien))
finalien <- final[complete.cases(final$ien),]
sum(is.na(finalien$ien))
finalien[which(finalien$ien==0),c("ien")] <- NA
sum(is.na(finalien$ien))
finalien <- finalien[complete.cases(finalien$ien),]
sum(is.na(finalien$ien))
finalien$ien <- as.numeric(finalien$ien)
#distribution
par(mfrow=c(1,1))
descdist(finalien$ien, discrete = F, boot = 500)
fitien <- fitdist(finalien$ien, "gamma")
summary(fitien)
plot(fitien)
#run a generalized mixed-effect model
#Gamma regression model
lmeien.1 <- glm(ien~ses, data=finalien, family = Gamma(link = "inverse"))
summary(lmeien.1)
#Gamma regression model with multiple variables
lmeien.2 <- glm(ien~ses+region+zona+ethnic, data=finalien, family = Gamma(link = "inverse"))
summary(lmeien.2)
#Gamma regression model with multiple variables after step AIC
step(lmeien.2, direction="both")
#step is the same as multiple Gamma regresion model
#Gamma mixed-effect model
lmeien.3 <- glmer(ien~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finalien, family = Gamma(link = "inverse"))
summary(lmeien.3)
#AIC
AIC(lmeien.1,lmeien.2,lmeien.3)
#lmeien.2 is the best model
glm.diag.plots(lmeien.2)

# Reproductive density --------------------------------------------------------

#prepare data
sum(is.na(final$rd))
finalrd <- final[complete.cases(final$rd),]
sum(is.na(finalrd$rd))
finalrd[which(finalrd$rd==0),c("rd")] <- NA
sum(is.na(finalrd$rd))
finalrd <- finalrd[complete.cases(finalrd$rd),]
sum(is.na(finalrd$rd))
finalrd$rd <- as.numeric(finalrd$rd)
#distribution
par(mfrow=c(1,1))
descdist(log(finalrd$rd), discrete = F, boot = 500)
fitrd <- fitdist(log(finalrd$rd), "norm")
summary(fitrd)
plot(fitrd)
#run a generalized mixed-effect model
#simple linear regression model
lmerd.1 <- lm(log(rd)~ses, data=finalrd)
summary(lmerd.1)
#multiple linear regression model
lmerd.2 <- lm(log(rd)~ses+region+zona+ethnic, data=finalrd)
summary(lmerd.2)
#multiple linear regression model with multiple variables after step AIC
step(lmerd.2, direction="both")
lmerd.3 <- lm(log(rd)~ses+zona+region,data=finalrd)
summary(lmerd.3)
#Multiple linear mixed-effect model
lmerd.4 <- lmer(log(rd)~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finalrd)
summary(lmerd.4)
#Multiple linear mixed-effect model with step AIC
lmerd.5 <- lmer(log(rd)~ses+(1+ses|zona)+(1+ses|region), data=finalrd)
summary(lmerd.5)
#AIC
AIC(lmerd.1,lmerd.2,lmerd.3,lmerd.4,lmerd.5)
#lmerd.3 are the best model
par(mfrow=c(2,2))
plot(lmerd.3)
par(mfrow=c(1,1))

# #4.- let's do the plots -----------------------------------------------------


#install ggplot2
install.packages("ggplot2")
library(ggplot2)

# #Nº of offspring --------------------------------------------------------

#prepare data

plots5 <- final[,c("ses","s5","region")]
plots5$phat <- predict(lmes5.3,type="response")
plots5 <- plots5[with(plots5,order(region)),]

#plot it

ggplot(plots5, aes(x = sqrt(ses), y = phat, colour = region, shape = region)) +
  geom_point(aes(y = s5), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1, aes(linetype=region)) +
  labs(x = expression(sqrt(SEP)), y = "N° of Offspring") +
  scale_shape_manual("Region",values=c(17,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16)) +
  scale_linetype_manual("Region",values=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)) +
  scale_colour_manual("Region",values=c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")) +
  theme_classic()


# #Age at first reproduction --------------------------------------------------------

#prepare data

plots6 <- finals6[,c("ses","s6","region")]
plots6$phat <- predict(lmes6.3,type="response")
plots6 <- plots6[with(plots6,order(region)),]

#plot it

ggplot(plots6, aes(x = ses, y = exp(phat), colour = region, shape = region)) +
  geom_point(aes(y = s6), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1, aes(linetype=region)) +
  labs(x = "SEP", y = "AFR") +
  scale_shape_manual("Region",values=c(17,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16)) +
  scale_linetype_manual("Region",values=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)) +
  scale_colour_manual("Region",values=c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")) +
  theme_classic()

# #Age at last reproduction --------------------------------------------------------

#prepare data

ploteuh <- finaleuh[,c("ses","euh","zona")]
ploteuh$zona<-factor(ploteuh$zona,levels = c("Urbano","Rural"),labels = c("Urban","Rural"))
ploteuh$phat <- predict(lmeeuh.3,type="response")
ploteuh <- ploteuh[with(ploteuh,order(zona)),]

#plot it

ggplot(ploteuh, aes(x = ses, y = phat, colour = zona)) +
  geom_point(aes(y = euh), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "SEP", y = "ALR") +
  scale_colour_manual("Rural vs Urban",values=c("#E69F00","#009E73")) +
  theme_classic()

# #Interbirth interval --------------------------------------------------------

#prepare data

plotien <- finalien[,c("ses","ien","zona","region","ethnic")]
plotien$zona<-factor(plotien$zona,levels = c("Urbano","Rural"),labels = c("Urban","Rural"))
plotien$ethnic<-factor(plotien$ethnic,levels = c("No","Sí"),labels = c("Not indigineous","Indigenous"))
plotien$phat <- predict(lmeien.2,type="response")
plotien <- plotien[with(plotien,order(zona,region,ethnic)),]

#plot it

ggplot(plotien, aes(x = ses, y = phat, colour = region, shape=region)) +
  geom_point(aes(y = ien), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1, aes(linetype=region)) +
  labs(x = "SEP", y = "IBI") +
  scale_shape_manual("Region",values=c(17,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16)) +
  scale_linetype_manual("Region",values=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)) +
  scale_colour_manual("Region",values=c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")) +
  facet_grid(ethnic~zona)+
  theme_classic()

# Birth density --------------------------------------------------------

#prepare data

plotrd <- finalrd[,c("ses","rd","zona","region")]
plotrd$zona<-factor(plotrd$zona,levels = c("Urbano","Rural"),labels = c("Urban","Rural"))
plotrd$phat <- predict(lmerd.3,type="response")
plotrd <- plotrd[with(plotrd,order(zona,region)),]

#plot it

ggplot(plotrd, aes(x = ses, y = exp(phat), colour = region, shape = region)) +
  geom_point(aes(y = rd), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1, aes(linetype=region)) +
  labs(x = "SEP", y = "Birth density") +
  scale_shape_manual("Region",values=c(17,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16)) +
  scale_linetype_manual("Region",values=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)) +
  scale_colour_manual("Region",values=c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")) +
  facet_grid(.~zona)+
  theme_classic()
