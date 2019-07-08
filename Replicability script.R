#Replicability script
#Pablo Varas Enr?quez, Luseadra McKerracher, Nicol?s Montalva Rivera 

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
install.packages("tidyverse")
library(tidyverse)

#importing CASEN with spss.get()
casen <- spss.get("C:/Users/Pablo/Documents/Dropbox antiguo/Publicacion/RB_SES_CL/CASEN_2013_MN_B_Principal.sav", use.value.labels=TRUE)
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
install.packages("psych")
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
summary(test[,c("pco1","ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")])
famd1 <- FAMD(test[,c("pco1","ESC","educ","ytrabajoCorh","yoautCorh","yautcorh","ysubh","ytotcorh","ypchtot","s14","v1","v2","v4","v6","v9","v11","v12","v23","v24","v25","v26")])
summary(famd1)
summary(famd1$var$contrib[,1])
which(famd1$var$contrib[,1] >= mean(famd1$var$contrib[,1]))
#FAMD with all the SES variables that contributed equal of higher to the mean
summary(test[,c("ESC","educ","ytrabajoCorh","yautcorh","ytotcorh","ypchtot","s14","v9","v12")])
famd2 <- FAMD(test[,c("ESC","educ","ytrabajoCorh","yautcorh","ytotcorh","ypchtot","s14","v9","v12")])
summary(famd2)
summary(famd2$var$contrib[,1])
#check for outliers
hist(famd2$ind$coord[,1])
which(famd2$ind$coord[,1]>=15)
test[which(famd2$ind$coord[,1]>=15),c("folio","edad", "s5", "s6", "euh", "ien","rd","region","comuna","zona","r6","ESC","educ","ytrabajoCorh","yautcorh","ytotcorh","ypchtot","s14","v9","v12")] #"Brachyteles_hypoxanthus"
#get rid of outlier
#FAMD without outliers
famdsub <-famd2$ind$coord[which(famd2$ind$coord[,1]<=20),1]
hist(famdsub)


# #Standarization, socioeconomic score and deciles ------------------------


q <- scale(famdsub, center = T, scale = T)
par(mfrow=c(1,1))
hist(q)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
final <- test[which(famd2$ind$coord[,1]<=20),c("folio","edad", "s5", "s6", "euh", "ien","rd","region","comuna","zona","r6","ethnic", "ESC","educ","ytrabajoCorh","yautcorh","ytotcorh","ypchtot","s14","v9","v12")]
final$ses <- range01(q)
hist(final$ses, breaks = 100)
#Quantiles
final$ses10 <- ntile(final$ses, 10)
final$ses5 <- ntile(final$ses, 5)
final$ses4 <- ntile(final$ses, 4)
final$ses3 <- ntile(final$ses, 3)
final$ses2 <- ntile(final$ses, 2)


# #3.- mixed effect models ----------------------------------------------------


#install package lme4
install.packages("lme4")
library(lme4)
#"fitdistrplus" package
update.packages("fitdistrplus")
install.packages("fitdistrplus")
library(fitdistrplus)

# #Nº of Offspring --------------------------------------------------------


#distribution
par(mfrow=c(1,1))
descdist(final$s5, discrete = T, boot = 500)
fits5 <- fitdist(final$s5, "pois")
summary(fits5)
plot(fits5)
#run a generalized mixed-effect model
#Poisson regression only with ses
lmes5.1 <- glm(s5~ses,data=final, family = poisson(link = "log"))
summary(lmes5.1)
#Poisson regression with multiple variables
lmes5.2 <- glm(s5~ses+region+zona+ethnic,data=final, family = poisson(link = "log"))
summary(lmes5.2)
#Poisson regression with multiple variables after step AIC
step(lmes5.2, direction="both")
lmes5.3 <- glm(s5~ses+region+zona,data=final, family = poisson(link = "log"))
summary(lmes5.3)
#generalized Poisson mixed-effect model
lmes5.4 <- glmer(s5~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic),data=final, family = poisson(link = "log"))
summary(lmes5.4)
#generalized Poisson mixed-effect model with step AIC
lmes5.5 <- glmer(s5~ses+(1+ses|region)+(1+ses|zona),data=final, family = poisson(link = "log"))
summary(lmes5.5)
#AIC
AIC(lmes5.1,lmes5.2,lmes5.3,lmes5.4, lmes5.5)
#lmes5.3 is the best model

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
lmes6.3 <- lm(log(s6)~ses+region+zona,data=finals6)
summary(lmes6.3)
#generalized mixed-effect model
lmes6.4 <- lmer(log(s6)~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic),data=finals6)
summary(lmes6.4)
#generalized mixed-effect model with step AIC variables
lmes6.5 <- lmer(log(s6)~ses+(1+ses|region)+(1+ses|zona),data=finals6)
summary(lmes6.5)
#AIC
AIC(lmes6.1,lmes6.2,lmes6.3, lmes6.4, lmes6.5)
#lmes6.3 is the best model

# #Age at last reproduction -----------------------------------------------


#prepare data
sum(is.na(final$euh))
finaleuh <- final[complete.cases(final$euh),]
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
#step model same as lmeeuh.1
#generalized mixed-effect model
lmeeuh.3 <- lmer(euh~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finaleuh)
summary(lmeeuh.3)
#AIC
AIC(lmeeuh.1,lmeeuh.2,lmeeuh.3)
#lmeeuh.1 is the best model

# #Interbirth intervals ---------------------------------------------------


#prepare data
sum(is.na(final$ien))
finalien <- final[complete.cases(final$ien),]
sum(is.na(finalien$ien))
finalien[which(finalien$ien==0),c("ien")] <- NA
sum(is.na(finalien$ien))
finalien <- finalien[complete.cases(finalien$ien),]
sum(is.na(finalien$ien))
#distribution
par(mfrow=c(1,1))
descdist(finalien$ien, discrete = F, boot = 500)
fitien <- fitdist(finalien$ien, "gamma")
summary(fitien)
plot(fitien)
#run a generalized mixed-effect model
#Gamma regression model
lmeien.1 <- glm(ien~ses, data=finalien, family = Gamma(link = "identity"))
summary(lmeien.1)
#Gamma regression model with multiple variables
lmeien.2 <- glm(ien~ses+region+zona+ethnic, data=finalien, family = Gamma(link = "identity"))
summary(lmeien.2)
#Gamma regression model with multiple variables after step AIC
step(lmeien.2, direction="both")
lmeien.3 <- glm(ien~ses+region, data=finalien, family = Gamma(link = "identity"))
summary(lmeien.3)
#Gamma mixed-effect model
lmeien.4 <- glmer(ien~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finalien, family = Gamma(link = "identity"))
summary(lmeien.4)
#Gamma mixed-effect model with step AIC
lmeien.5 <- glmer(ien~ses+(1+ses|region), data=finalien, family = Gamma(link = "identity"))
summary(lmeien.5)
#AIC
AIC(lmeien.1,lmeien.2,lmeien.3,lmeien.4, lmeien.5)
#lmeien.5 is the best model

# #Modes of parity --------------------------------------------------------

#prepare data
sum(is.na(final$rd))
finalrd <- final[complete.cases(final$rd),]
sum(is.na(finalrd$rd))
finalrd[which(finalrd$rd==0),c("rd")] <- NA
sum(is.na(finalrd$rd))
finalrd <- finalrd[complete.cases(finalrd$rd),]
sum(is.na(finalrd$rd))
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
lmerd.3 <- lm(log(rd)~ses+region,data=finalrd)
summary(lmerd.3)
#Multiple linear mixed-effect model
lmerd.4 <- lmer(log(rd)~ses+(1+ses|region)+(1+ses|zona)+(1+ses|ethnic), data=finalrd)
summary(lmerd.4)
#Multiple linear mixed-effect model with step AIC
lmerd.5 <- lmer(log(rd)~ses+(1+ses|region), data=finalrd)
summary(lmerd.5)
#AIC
AIC(lmerd.1,lmerd.2,lmerd.3,lmerd.4,lmerd.5)
#lmerd.3 are the best model

# #4.- let's do the plots -----------------------------------------------------


#install ggeffects package
install.packages("ggeffects")
library(ggeffects)

# #Nº of offspring --------------------------------------------------------

#overall model
ggplot(final,aes(x=ses, y=s5)) +
  geom_point()+
  geom_smooth(method="glm",method.args=list(family=poisson), se=T) +
  theme_classic()
#zona
ggplot(final,aes(x=ses, y=s5, colour=zona)) +
  geom_point()+
  geom_smooth(method="glm",method.args=list(family=poisson), se=F) +
  theme_classic()
#region
ggplot(final,aes(x=ses, y=s5, colour=region)) +
  geom_point()+
  geom_smooth(method="glm",method.args=list(family=poisson), se=F) +
  theme_classic()

# #Age at first reproduction --------------------------------------------------------

#overall model
ggplot(finals6,aes(x=ses, y=s6)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  theme_classic()
#zona
ggplot(finals6,aes(x=ses, y=s6, colour=zona)) +
  geom_point()+
  geom_smooth(method="lm", se=F) +
  theme_classic()
#region
ggplot(finals6,aes(x=ses, y=s6, colour=region)) +
  geom_point()+
  geom_smooth(method="lm", se=F) +
  theme_classic()

# #Age at last reproduction --------------------------------------------------------

#overall model
ggplot(finaleuh,aes(x=ses, y=euh)) +
  geom_point()+
  geom_smooth(method="lm", se=T) +
  theme_classic()

# #Interbirth interval --------------------------------------------------------

#overall model
ggplot(finalien,aes(x=ses, y=ien)) +
  geom_point()+
  geom_smooth(method="glm",method.args=list(family=Gamma), se=T) +
  theme_classic()
#region
ggplot(finalien,aes(x=ses, y=ien, colour=region)) +
  geom_point()+
  geom_smooth(method="glm",method.args=list(family=Gamma), se=F) +
  theme_classic()

# #Degree of parity --------------------------------------------------------

#overall model
ggplot(finalrd,aes(ses,rd))+
  geom_point()+
  geom_smooth(method="lm", se=T) +
  theme_classic()
#region
ggplot(finalrd,aes(x=ses, y=rd, colour=region)) +
  geom_point()+
  geom_smooth(method="lm", se=F) +
  theme_classic()

# #former code...useful for recycle ---------------------------------------


# #R packages to use
# #Package FactoMineR
# install.packages("FactoMineR")
# library(FactoMineR)
# 
# #Socioeconomic groups generation
# #NA management
# nonamcch1 <- mcch[which((mcch$educ == "NS/NR") == FALSE),]
# sum(is.na(nonamcch1$educ))
# nonamcch2 <- nonamcch1[which((nonamcch1$s14 == "No sabe") == FALSE),]
# sum(is.na(nonamcch2$s14))
# nonamcch3 <- nonamcch2[which((nonamcch2$r6 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch3$r6))
# nonamcch4 <- nonamcch3[which((nonamcch3$v2 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch4$v2))
# nonamcch5 <- nonamcch4[which((nonamcch4$v4 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch5$v4))
# nonamcch6 <- nonamcch5[which((nonamcch5$v6 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch6$v6))
# nonamcch7 <- nonamcch6[which((nonamcch6$v9 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch7$v9))
# nonamcch8 <- nonamcch7[which((nonamcch7$v11 == "No sabe") == FALSE),]
# sum(is.na(nonamcch8$v11))
# nonamcch9 <- nonamcch8[which((nonamcch8$v12 == "NS/NR") == FALSE),]
# sum(is.na(nonamcch9$v12))
# #Deciles generation
# nonamcch9$decilt <- ntile(nonamcch9$ytotcorh, 10)
# nonamcch9$decilpc <- ntile(nonamcch9$ypchtot, 10)
# x <- nonamcch9[,c("folio", "ytotcorh", "decilt", "ypchtot", "decilpc")]
# #Socioeconomic variables selection
# gse <- nonamcch9[,c("folio", "region", "zona", "educ", "decilt", "decilpc", "s14", "r6", "v1", "v2", "v4", "v6", "v9", "v11", "v12")]
# #Transformation to factors
# factor <- as.data.frame(unclass(gse))
# str(factor)
# factor$folio <- as.factor(factor$folio)
# factor$decilt <- as.factor(factor$decilt)
# factor$decilpc <- as.factor(factor$decilpc)
# str(factor)
# cats <- apply(factor, 2, function(x) nlevels(as.factor(x)))
# 
# #Multiple correspondance analysis
# #MCA with socioeconomic variables
# mca1 <- MCA(factor[,2:15], ncp=5, axes = c(1,2))
# summary(mca1)
# sum(test$var$eta2[,1])/14
# test$var$eta2[,1]
# #MCA with selected socioeconomic variables (equal or higher than the mean contribution in dim 1)
# factor2 <- factor[,c("folio", "educ", "decilt", "decilpc", "s14", "v2", "v9", "v12")]
# cats2 <- apply(factor2, 2, function(x) nlevels(as.factor(x)))
# mca2 <- MCA(factor2[,2:8], ncp = 5, axes = c(1,2))
# pdf("MCA.pdf")
# par(mfrow=c(1,2))
# plot(mca2, axes = c(1,2), choix = "ind", invisible = "var", label="none", title = "Dim. 1 vs Dim. 2", col.ind="light gray")
# plot(mca2, axes = c(1,2), choix = "var", title = "Socio-economic variables\ncontribution", col.var = "black")
# dev.off()
# 
# #Standarization, socioeconomic score and deciles
# q <- scale(mca2$ind$coord[,1], center = T, scale = T)
# par(mfrow=c(1,1))
# hist(q)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# final <- nonamcch9[,c("folio","edad", "s5", "s6", "euh", "ien", "ytotcorh", "decilth", "quintilth", "cuartilth", "tercilth", "altobajoth")]
# final$pse <- range01(q)
# #socioeconomic score testing
# prueba <- nonamcch9[,c("folio", "educ", "ytotcorh", "ypchtot", "educ", "s14", "v9", "v12")]
# prueba$pse <- range01(q)
# f <- prueba[which(prueba$pse == 1),]
# g <- prueba[which(prueba$pse == 0),]
# #Histograms
# hpse<-hist(final$pse, xlab="Socioeconomic score", ylab ="Frequency", main = NULL)
# #Quantiles
# final$decilpse <- ntile(final$pse, 10)
# final$quintilpse <- ntile(final$pse, 5)
# final$cuartilpse <- ntile(final$pse, 4)
# final$tercilpse <- ntile(final$pse, 3)
# final$altobajose <- ntile(final$pse, 2)
# y <- final[,c("folio", "pse", "decilpse", "quintilpse", "cuartilpse", "tercilpse", "altobajose")]
# 
# #3.- Descriptive statistics
# 
# #Socioeconomic score
# summary(final$pse)
# sd(final$pse)
# pdf("Histpse.pdf")
# hpse<-hist(final$pse, xlab="Socioeconomic score", ylab = "Frequency", main=NULL)
# dev.off()
# 
# #Life-history traits
# #Number of offspring
# summary(final$s5)
# sd(final$s5)
# pdf("Hists5.pdf")
# hs5<-hist(final$s5, xlab="Number of offspring", ylab = "Frequency", main=NULL)
# dev.off()
# #Age at first birth
# finals6 <- final[complete.cases(final$s6),]
# summary(finals6$s6)
# sd(finals6$s6)
# pdf("Hists6.pdf")
# hs6<-hist(finals6$s6, xlab="Age at first birth", ylab = "Frequency", main= NULL)
# dev.off()
# #age at last birth
# finaleuh <- final[complete.cases(final$euh),]
# summary(finaleuh$euh)
# sd(finaleuh$euh)
# pdf("Histeuh.pdf")
# heuh<-hist(finaleuh$euh, xlab="Age last birth", ylab = "Frequency", main=NULL)
# dev.off()
# #Interbirth intervals
# finalien <- final[which(final$ien > 0),]
# finalien <- finalien[which(finalien$s5 > 1),]
# summary(finalien$ien)
# sd(finalien$ien)
# pdf("Histien.pdf")
# hien<-hist(finalien$ien, xlab="Interbirth intervals", ylab = "Frequency", main=NULL)
# dev.off()
# 
# #4.- Life-history traits~socioeconomic score analysis
# 
# #R packages to use
# #R packages
# #"survival" package
# update.packages("survival")
# install.packages("survival")
# library(survival)
# #"fitdistrplus" package
# update.packages("fitdistrplus")
# install.packages("fitdistrplus")
# library(fitdistrplus)
# #"qcc" package
# install.packages("qcc")
# library(qcc)
# #"MASS" package
# install.packages("MASS")
# library(MASS)
# 
# #Socioeconomic score distribution
# final$pse <- as.numeric(final$pse)
# is.numeric(final$pse)
# descdist(final$pse, discrete = FALSE, boot = 500)
# fit.pse <- fitdist(final$pse, "norm")
# summary(fit.pse)
# plot(fit.pse)
# 
# #Life-history analysis
# #Number of offspring
# #Distribution
# descdist(residuals(mods5), discrete = FALSE, boot = 500)
# fit.s5 <- fitdist(final$s5, "pois")
# summary(fit.s5)
# png("Fits5.png", width = 4.9, height = 5.7, units = "in", res = 500)
# plot(fit.s5)
# dev.off()
# #Poisson regression
# mods5 <- glm(s5 ~ pse, family = poisson, data = final)
# summary(mods5)
# par(mfrow=c(2,2))
# plot(mods5)
# coef(mods5)
# exp(coef(mods5))
# #Overdispersion testing
# qcc.overdispersion.test(final$s5, type="poisson")
# #Figure
# pdf("regs5.pdf")
# par(mfrow=c(1,1))
# plot(final$pse, final$s5, xlab = "Socioeconomic score", ylab = "Number of offspring", main = NULL, col = (gray(0.5)))
# newx <- seq(min(final$pse), max(final$pse), length.out=4766)
# predProbs<-predict(mods5,data.frame(pse=newx), type="response")
# lines(seq(min(final$pse), max(final$pse), length.out=4766), predProbs, col=("black"), lwd=2)
# dev.off()
# #Age at first birth
# #Variables definition
# evento1 <- finals6$s6
# grupo1.1 <- finals6$decilpse
# grupo1.1 <- as.factor(grupo1.1)
# grupo1.2 <- finals6$quintilpse
# grupo1.2 <- as.factor(grupo1.2)
# grupo1.3 <- finals6$cuartilpse
# grupo1.3 <- as.factor(grupo1.3)
# grupo1.4 <- finals6$tercilpse
# grupo1.4 <- as.factor(grupo1.4)
# grupo1.5 <- finals6$altobajose
# grupo1.5 <- as.factor(grupo1.5)
# #Kaplan-Meier survival analysis
# kms5survival1 <- survfit(Surv(evento1) ~ 1)
# summary(kms5survival1)
# plot(kms5survival1, xlab = "Age at first birth")
# title("Kaplan-Meier survival analysis for age first birth")
# #Kaplan-Meier survival analysis by socioeconomic group
# kms5survivalgse1.1 <- survfit(Surv(evento1) ~ grupo1.1)
# summary(kms5survivalgse1.1)
# pdf("surv10afb.png")
# plot(kms5survivalgse1.1, xlab = "Age at first birth", col = (gray(0:7/10)), lty = (1:10))
# legend("topright", ncol = 2, legend = c("decile 1", "decile 2", "decile 3", "decile 4", "decile 5", "decile 6", "decile 7", "decile 8", "decile 9", "decile 10"), col = gray(0:7/10), lty = (1:10), cex = 0.8)
# dev.off()
# kms5survivalgse1.2 <- survfit(Surv(evento1) ~ grupo1.2)
# summary(kms5survivalgse1.2)
# pdf("surv5afb.pdf")
# plot(kms5survivalgse1.2, xlab = "Age at first birth", col = gray(0:5/5), lty = (1:5))
# legend("topright", ncol = 2, legend = c("quintile 1", "quintile 2", "quintile 3", "quintile 4", "quintile 5"), col = gray(0:5/5), lty=(1:5), cex=0.8)
# dev.off()
# kms5survivalgse1.3 <- survfit(Surv(evento1) ~ grupo1.3)
# summary(kms5survivalgse1.3)
# pdf("surv4afb.pdf")
# plot(kms5survivalgse1.3, xlab = "Age at first birth", col = gray(0:4/4), lty=(1:4))
# legend("topright", ncol = 2, legend = c("quartile 1", "quartile 2", "quartile 3", "quartile 4"), col = gray(0:4/4), lty=(1:4), cex=0.8)
# dev.off()
# kms5survivalgse1.4 <- survfit(Surv(evento1) ~ grupo1.4)
# summary(kms5survivalgse1.4)
# pdf("surv3afb.pdf")
# plot(kms5survivalgse1.4, xlab = "Age at first birth", col = gray(0:3/3), lty=(1:3))
# legend("topright", legend = c("tertile 1", "tertile 2", "tertile 3"), col = gray(0:3/3), lty=(1:3), cex=0.8)
# dev.off()
# kms5survivalgse1.5 <- survfit(Surv(evento1) ~ grupo1.5)
# summary(kms5survivalgse1.5)
# pdf("surv2afb.pdf")
# plot(kms5survivalgse1.5, xlab = "Age at first birth", col = gray(0:2/2), lty=(1:2))
# legend("topright", legend = c("Low SES", "High SES"), col = gray(0:2/2), lty=(1:2), cex=0.8)
# dev.off()
# #Distribution
# descdist(finals6$s6, discrete = TRUE, boot = 500)
# fit.s6 <- fitdist(finals6$s6, "nbinom")
# summary(fit.s6)
# pdf("Fits6.pdf")
# plot(fit.s6)
# dev.off()
# #Negative binomial regression
# mods6 <- glm.nb(s6 ~ as.vector(pse), data = finals6)
# summary(mods6)
# par(mfrow=c(2,2))
# plot(mods6)
# coef(mods6)
# exp(coef(mods6))
# #Figure
# pdf("regs6.pdf")
# par(mfrow=c(1,1))
# plot(finals6$pse, finals6$s6, xlab = "Socioeconomic score", ylab = "Age at first birth", main = NULL, col = gray(0.5))
# newx1 <- seq(min(finals6$pse), max(finals6$pse), length.out=4496)
# predProbs1<-predict(mods6,data.frame(pse=newx1), type="response")
# lines(seq(min(finals6$pse), max(finals6$pse), length.out=4496), predProbs1, col=1, lwd=2)
# dev.off()
# 
# #Age at last birth
# #Variables definition
# evento2 <- finaleuh$euh
# grupo2.1 <- finaleuh$decilpse
# grupo2.1 <- as.factor(grupo2.1)
# grupo2.2 <- finaleuh$quintilpse
# grupo2.2 <- as.factor(grupo2.2)
# grupo2.3 <- finaleuh$cuartilpse
# grupo2.3 <- as.factor(grupo2.3)
# grupo2.4 <- finaleuh$tercilpse
# grupo2.4 <- as.factor(grupo2.4)
# grupo2.5 <- finaleuh$altobajose
# grupo2.5 <- as.factor(grupo2.5)
# #Kaplan-Meier survival analysis
# kms5survival2 <- survfit(Surv(evento2) ~ 1)
# summary(kms5survival2)
# plot(kms5survival2, xlab = "Age at last birth")
# title("Kaplan-Meier survival analysis for \nage at last birth")
# #Kaplan-Meier survival analysis by socioeconomic groups
# kms5survivalgse2.1 <- survfit(Surv(evento2) ~ grupo2.1)
# summary(kms5survivalgse2.1)
# pdf("surv10euh.pdf")
# plot(kms5survivalgse2.1, xlab = "Age at last birth", col = gray(0:7/10), lty=(1:10))
# legend("bottomleft", ncol = 2, legend = c("decile 1", "decile 2", "decile 3", "decile 4", "decile 5", "decile 6", "decile 7", "decile 8", "decile 9", "decile 10"), col = gray(0:7/10), lty=(1:10), cex = 0.8)
# dev.off()
# kms5survivalgse2.2 <- survfit(Surv(evento2) ~ grupo2.2)
# summary(kms5survivalgse2.2)
# pdf("surv5euh.pdf")
# plot(kms5survivalgse2.2, xlab = "Age at last birth", col = gray(0:5/5), lty=(1:5))
# legend("bottomleft", ncol = 2, legend = c("quintile 1", "quintile 2", "quintile 3", "quintile 4", "quintile 5"), col = gray(0:5/5), lty = (1:5), cex=0.8)
# dev.off()
# kms5survivalgse2.3 <- survfit(Surv(evento2) ~ grupo2.3)
# summary(kms5survivalgse2.3)
# pdf("surv4euh.pdf")
# plot(kms5survivalgse2.3, xlab = "Age at last birth", col = gray(0:4/4), lty=(1:4))
# legend("bottomleft", ncol = 2, legend = c("quartile 1", "quartile 2", "quartile 3", "quartile 4"), col = gray(0:4/4), lty=(1:4), cex=0.8)
# dev.off()
# kms5survivalgse2.4 <- survfit(Surv(evento2) ~ grupo2.4)
# summary(kms5survivalgse2.4)
# pdf("surv3euh.pdf")
# plot(kms5survivalgse2.4, xlab = "Age at last birth", col = gray(0:3/3), lty=(1:3))
# legend("bottomleft", legend = c("tertile 1", "tertile 2", "tertile 3"), col = gray(0:3/3), lty=(1:3), cex=0.8)
# dev.off()
# kms5survivalgse2.5 <- survfit(Surv(evento2) ~ grupo2.5)
# summary(kms5survivalgse2.5)
# pdf("surv2euh.pdf")
# plot(kms5survivalgse2.5, xlab = "Age at last birth", col = gray(0:2/2), lty = (1:2))
# legend("bottomleft", legend = c("Low SES", "High SES"), col = gray(0:2/2), lty = (1:2), cex=0.8)
# dev.off()
# #Distribution
# descdist(finaleuh$euh, discrete = TRUE, boot = 500)
# fit.euh <- fitdist(finaleuh$euh, "norm")
# summary(fit.euh)
# pdf("Fiteuh.pdf")
# plot(fit.euh)
# dev.off()
# #Linear regression
# modeuh <- lm(euh ~ as.vector(pse), data = finaleuh)
# summary(modeuh)
# par(mfrow=c(2,2))
# plot(modeuh)
# coef(modeuh)
# #Figure
# pdf("regeuh.pdf")
# par(mfrow=c(1,1))
# plot(finaleuh$pse, finaleuh$euh, xlab = "Socioeconomic score", ylab = "Age at last birth", main = NULL, col=gray(0.5))
# newx2 <- seq(min(finaleuh$pse), max(finaleuh$pse), length.out=4496)
# predProbs2<-predict(modeuh,data.frame(pse=newx2), type="response")
# lines(seq(min(finaleuh$pse), max(finaleuh$pse), length.out=4496), predProbs2, col=1, lwd=2)
# dev.off()
# 
# #Interbirth intervals
# #Distribution
# descdist(finalien$ien, discrete = FALSE, boot = 500)
# fit.ien <- fitdist(finalien$ien, "gamma")
# pdf("Fitien.pdf")
# plot(fit.ien)
# dev.off()
# #Gamma regresion
# modien <- glm(ien ~ as.vector(pse), family = Gamma(), data = finalien)
# summary(modien)
# par(mfrow=c(2,2))
# plot(modien)
# exp(coef(modien))
# #Figure
# pdf("regien.pdf")
# par(mfrow=c(1,1))
# plot(finalien$pse, finalien$ien, xlab = "Socioeconomic score", ylab = "Interbirth intervals", main = NULL, col=gray(0.5))
# newx3 <- seq(min(finalien$pse), max(finalien$pse), length.out=4766)
# predProbs3<-predict(modien,data.frame(pse=newx3), type="response")
# lines(seq(min(finalien$pse), max(finalien$pse), length.out=4766), predProbs3, col=1, lwd=2)
# dev.off()
