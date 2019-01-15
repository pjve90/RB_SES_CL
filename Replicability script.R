#Replicability script
#Pablo Varas Enr?quez, Luseadra McKerracher, Nicol?s Montalva Rivera 

#Workspace management

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

#1.- Cohort selection, data mining and management

#R package to use
#"Hmisc" package
install.packages("Hmisc")
library(Hmisc)
#"dplyr" package
install.packages("dplyr")
library(dplyr)

#Importing CASEN database

#importing CASEN with spss.get()
casen <- spss.get("C:/Users/Pablo/Documents/Dropbox antiguo/Publicacion/RB_SES_CL/CASEN_2013_MN_B_Principal.sav", use.value.labels=TRUE)
#corroboration that is a data frame
is.data.frame(casen)

#Data mining and management

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

#Number of children (s5) into numeric
womych$s5[which(womych$s5 == "No sabe / No recuerda")] <- NA
womych$s5[which(womych$s5 == "No sabe")] <- NA
summary(womych$s5)
is.factor(womych$s5)
womych$s5 <- as.numeric(levels(womych$s5))[womych$s5]
is.numeric(womych$s5)
#Age at first birth (s6) into numeric
womych$s6[which(womych$s6 == "No sabe / No recuerda")] <- NA
womych$s6[which(womych$s6 == "No sabe")] <- NA
summary(womych$s6)
is.factor(womych$s6)
womych$s6 <- as.numeric(levels(womych$s6))[womych$s6]
is.numeric(womych$s6)

#Age at last birth
womych <- arrange(womych, folio, desc(pco1))
womych <- group_by(womych, folio)
womych <- mutate(womych, euh = edad - lag(edad))
summary(womych$euh)
sum(is.na(womych$euh))

#Interbirth intervals
womych <- mutate(womych, ien = (euh - s6)/s5)
summary(womych$ien)
womych$euh
womych$ien
sum(is.na(womych$ien))
womych <- data.frame(womych)

#Reproductive density
womych <- mutate(womych, rd = s5/(euh - s6))
summary(womych$rd)
sum(is.na(womych$rd))
womych <- data.frame(womych)

#Cleaning final cohort
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

#Selection of childless women
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

#Final cohort database
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



# #2.- Socioeconomic groups
# 
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
