#Replicability script
#Pablo Varas Enríquez, Luseadra McKerracher, Nicolás Montalva Rivera 

#Workspace management

#Establishing directory
getwd()
setwd("folder_directory")
#Establishing history and image
savehistory("folder_directory")
save.image("folder_directory")
load("folder_directory")
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
casen <- spss.get("folder_directory", use.value.labels=TRUE)
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
is.na(casen$nucleo)
unnucleo <- casen[which(casen$nucleo == 1),]
sum(is.na(unnucleo$nucleo))

#women selection that are head or partner of the head of the household and have between 45 and 50 years old
mun <- unnucleo[which(unnucleo$sexo == "Mujer"),]
#45-50 years old selection
cedad <- mun[which(mun$edad >= 45 & mun$edad <= 50),]
#head of the household selection
cjefa <- cedad[which(cedad$pco1 == "Jefe(a) de hogar"),]
cjefa$s5[which(cjefa$s5 == 99)] <- NA 
cjefa$s6[which(cjefa$s6 == 99)] <- NA
sum(is.na(cjefa$edad))
sum(is.na(cjefa$s5))
sum(is.na(cjefa$s6))
which(colnames(cjefa)=="s5")
which(colnames(cjefa)=="s6")
nacjefa <- cjefa[complete.cases(cjefa[,151:152]),]
#partner of the head of the household
cesposa <- cedad[which(cedad$pco1 == "Esposo(a) o pareja"),]
cesposa$s5[which(cesposa$s5 == 99)] <- NA 
cesposa$s6[which(cesposa$s6 == 99)] <- NA
sum(is.na(cesposa$edad))
sum(is.na(cesposa$s5))
sum(is.na(cesposa$s6))
nacesposa <- cesposa[complete.cases(cesposa[,151:152]),]
#combining head and partner of the head of the household selections
mc <- rbind(cjefa, cesposa)
mc$s5[which(mc$s5 == 99)] <- NA 
mc$s6[which(mc$s6 == 99)] <- NA
sum(is.na(mc$edad))
sum(is.na(mc$s5))
sum(is.na(mc$s6))
namc <- mc[complete.cases(mc[,151:152]),]

#Children selection of mononuclear households
#children of both partners in the household selection
haun <- unnucleo[which(unnucleo$pco1 == "Hijo(a) de ambos"),]
sum(is.na(haun$pco1))
#children of son of the head of the household selection
hjun <- unnucleo[which(unnucleo$pco1 == "Hijo(a) sólo del jefe"),]
sum(is.na(hjun$pco1))
#children of the partner of the household selection
hpun <- unnucleo[which(unnucleo$pco1 == "Hijo(a) sólo del esposo(a) o pareja"),]
sum(is.na(hpun$pco1))
#combination of children selections
hun <- rbind(hjun, haun, hpun)

#Selection of children of women in the cohort
hmc <- hun[which((hun$folio %in% namc$folio) == TRUE),]

#Selection of youngest children of women in the cohort
tblhmc <- tbl_df(hmc)
hmcbyfolio <- group_by(tblhmc, folio)
hmc1 <- filter(hmcbyfolio, edad == min(edad))
hmc1 <- data.frame(hmc1)
hmc1 <- hmc1[order(hmc1[,1]),]
sum(is.na(hmc1$edad))
#Selection of women and youngest children of the cohort
mhmc <- rbind(namc, hmc1)
mhmc <- mhmc[order(mhmc[,1]),]
summary(mhmc$s5)
summary(mhmc$s6)

#Number of children (s5) into numeric
mhmc$s5[which(mhmc$s5 == "No sabe / No recuerda")] <- NA
mhmc$s5[which(mhmc$s5 == "No sabe")] <- NA
summary(mhmc$s5)
is.factor(mhmc$s5)
mhmc$s5 <- as.numeric(levels(mhmc$s5))[mhmc$s5]
is.numeric(mhmc$s5)
#Age at first birth (s6) into numeric
mhmc$s6[which(mhmc$s6 == "No sabe / No recuerda")] <- NA
mhmc$s6[which(mhmc$s6 == "No sabe")] <- NA
summary(mhmc$s6)
is.factor(mhmc$s6)
mhmc$s6 <- as.numeric(levels(mhmc$s6))[mhmc$s6]
is.numeric(mhmc$s6)

#Age at last birth
tblmhmc <- tbl_df(mhmc)
ordtblmhmc <- arrange(tblmhmc, folio, desc(pco1))
mhmcbyfolio <- group_by(ordtblmhmc, folio)
mhmceuh <- mutate(mhmcbyfolio, euh = edad - lag(edad))
summary(mhmceuh$euh)
sum(is.na(mhmceuh$euh))

#Interbirth intervals
mhmceuhien <- mutate(mhmceuh, ien = (euh - s6)/s5)
summary(mhmceuhien$ien)
mhmceuhien$euh
mhmceuhien$ien
sum(is.na(mhmceuhien$ien))
mhmceuhien <- data.frame(mhmceuhien)
#The selection of the cohort for Interbirth intervals will be done just before the analysis

#Selection of women from the cohort with age at last birth and Interbirth intervals
jceuhien <- mhmceuhien[which(mhmceuhien$pco1 == "Jefe(a) de hogar"),]
sum(is.na(jceuhien$pco1))
pceuhien <- mhmceuhien[which(mhmceuhien$pco1 == "Esposo(a) o pareja"),]
sum(is.na(pceuhien$pco1))
mceuhien <- rbind(jceuhien, pceuhien)
sum(is.na(mceuhien$edad))
summary(mceuhien$edad)
sum(is.na(mceuhien$s5))
summary(mceuhien$s5)
sum(is.na(mceuhien$s6)) 
summary(mceuhien$s6)
sum(is.na(mceuhien$euh))
summary(mceuhien$euh)
sum(is.na(mceuhien$ien)) 
summary(mceuhien$ien) 
#NA management
ccmceuhien <- mceuhien[complete.cases(mceuhien[,152]),]
ccmceuhien <- mceuhien[complete.cases(mceuhien[,606:607]),]
nonamceuhien <- ccmceuhien[which(ccmceuhien$ien >= 0),]
sum(is.na(nonamceuhien$euh))
sum(is.na(nonamceuhien$ien))
#Selection of Chilean women from the cohort
chilesolo <- nonamceuhien[which(nonamceuhien$r1a == "Chilena (exclusivamente)"),]
chiledoble <- nonamceuhien[which(nonamceuhien$r1a == "Chilena y otra (doble nacionalidad)"),]

#Final cohort database
mcch <- rbind(chilesolo, chiledoble)
sum(is.na(mcch$edad))
summary(mcch$edad)
sum(is.na(mcch$s5))
summary(mcch$s5)
sum(is.na(mcch$s6)) 
summary(mcch$s6)
sum(is.na(mcch$euh))
summary(mcch$euh)
sum(is.na(mcch$ien)) 
summary(mcch$ien)

#2.- Socioeconomic groups

#R packages to use
#Package "ca"
install.packages("ca")
library(ca)

#Socioeconomic groups generation
#NA management
nonamcch1 <- mcch[which((mcch$educ == "NS/NR") == FALSE),]
sum(is.na(nonamcch1$educ))
nonamcch2 <- nonamcch1[which((nonamcch1$s14 == "No sabe") == FALSE),]
sum(is.na(nonamcch2$s14))
nonamcch3 <- nonamcch2[which((nonamcch2$r6 == "NS/NR") == FALSE),]
sum(is.na(nonamcch3$r6))
nonamcch4 <- nonamcch3[which((nonamcch3$v2 == "NS/NR") == FALSE),]
sum(is.na(nonamcch4$v2))
nonamcch5 <- nonamcch4[which((nonamcch4$v4 == "NS/NR") == FALSE),]
sum(is.na(nonamcch5$v4))
nonamcch6 <- nonamcch5[which((nonamcch5$v6 == "NS/NR") == FALSE),]
sum(is.na(nonamcch6$v6))
nonamcch7 <- nonamcch6[which((nonamcch6$v9 == "NS/NR") == FALSE),]
sum(is.na(nonamcch7$v9))
nonamcch8 <- nonamcch7[which((nonamcch7$v11 == "No sabe") == FALSE),]
sum(is.na(nonamcch8$v11))
nonamcch9 <- nonamcch8[which((nonamcch8$v12 == "NS/NR") == FALSE),]
sum(is.na(nonamcch9$v12))
#Deciles generation
nonamcch9$decilt <- ntile(nonamcch9$ytotcorh, 10)
nonamcch9$decilpc <- ntile(nonamcch9$ypchtot, 10)
x <- nonamcch9[,c("folio", "ytotcorh", "decilt", "ypchtot", "decilpc")]
#Socioeconomic variables selection
gse <- nonamcch9[,c("folio", "region", "zona", "educ", "decilt", "decilpc", "s14", "r6", "v1", "v2", "v4", "v6", "v9", "v11", "v12")]
#Transformation to factors
factor <- as.data.frame(unclass(gse))
str(factor)
factor$folio <- as.factor(factor$folio)
factor$decilt <- as.factor(factor$decilt)
factor$decilpc <- as.factor(factor$decilpc)
str(factor)
cats <- apply(factor, 2, function(x) nlevels(as.factor(x)))

#Multiple correspondance analysis
#MCA with socioeconomic variables
mca1 <- mjca(factor[,2:15], lambda = "indicator")
plot(mca1, labels = 0, col = c("black", rainbow(15)), pch=15)
legend(-37, 58, ncol = 2, legend = factor(colnames(factor[,2:15])), col = rainbow(15), pch=15)
summary(mca1, scree = TRUE, rows = FALSE, columns = FALSE)

#MCA with selected socioeconomic variables
factor2 <- factor[,c("folio", "educ", "decilt", "decilpc", "s14", "v1", "v2", "v4", "v6", "v9", "v12")]
cats2 <- apply(factor2, 2, function(x) nlevels(as.factor(x)))
mca3 <- mjca(factor2[,2:11], lambda = "indicator")
png("MCA.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(mca3,  labels = 0, col = c("black", gray(0:10/10)), pch=15)
legend(4.6, -43, ncol = 2, legend = factor(colnames(factor2[,2:11])), col = gray(0:10/10), pch=15, cex = 0.75)
dev.off()
summary(mca3, scree = TRUE, rows = FALSE, columns = FALSE)

#Standarization, socioeconomic score and deciles
q <- scale(mca3$rowcoord[,1], center = T, scale = T)
hist(q)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
final <- nonamcch9[,c("folio","edad", "s5", "s6", "euh", "ien", "ytotcorh", "decilth", "quintilth", "cuartilth", "tercilth", "altobajoth")]
final$pse <- range01(q)
#socioeconomic score testing
prueba <- nonamcch9[,c("folio", "educ", "ytotcorh", "ypchtot", "educ", "s14", "v9", "v12")]
prueba$pse <- range01(q)
f <- prueba[which(prueba$pse == 1),]
g <- prueba[which(prueba$pse == 0),]
#Histograms
hpse<-hist(final$pse, xlab="Socioeconomic score", ylab ="Frequency", main = NULL)
#Quantiles
final$decilpse <- ntile(final$pse, 10)
final$quintilpse <- ntile(final$pse, 5)
final$cuartilpse <- ntile(final$pse, 4)
final$tercilpse <- ntile(final$pse, 3)
final$altobajose <- ntile(final$pse, 2)
y <- final[,c("folio", "pse", "decilpse", "quintilpse", "cuartilpse", "tercilpse", "altobajose")]

#3.- Descriptive statistics

#Socioeconomic score
summary(final$pse)
sd(final$pse)
png("Histpse.png", width = 4.9, height = 5.7, units = "in", res = 500)
hpse<-hist(final$pse, xlab="Socioeconomic score", ylab = "Frequency", main=NULL)
dev.off()

#Life-history traits
#Number of offspring
summary(final$s5)
sd(final$s5)
png("Hists5.png", width = 4.9, height = 5.7, units = "in", res = 500)
hs5<-hist(final$s5, xlab="Number of offspring", ylab = "Frequency", main=NULL)
dev.off()
#Age at first birth
summary(final$s6)
sd(final$s6)
png("Hists6.png", width = 4.9, height = 5.7, units = "in", res = 500)
hs6<-hist(final$s6, xlab="Age at first birth", ylab = "Frequency", main= NULL)
dev.off()
#age at last birth
summary(final$euh)
sd(final$euh)
png("Histeuh.png", width = 4.9, height = 5.7, units = "in", res = 500)
heuh<-hist(final$euh, xlab="Age last birth", ylab = "Frequency", main=NULL)
dev.off()
#Interbirth intervals
finalien <- final[which(final$ien > 0),]
test <- finalien[which(finalien$s5 > 1),]
summary(test$ien)
sd(test$ien)
png("Histien.png", width = 4.9, height = 5.7, units = "in", res = 500)
hien<-hist(final$ien, xlab="Interbirth intervals", ylab = "Frequency", main=NULL)
dev.off()
#summary plot of descriptive statistics
png("boxplotlht.png", width = 4.9, height = 5.7, units = "in", res = 500)
boxplot(final$s5, final$s6, final$euh, final$ien, xaxt="n", xlab="Life-history traits", main=NULL)
axis(1, at=1:4, labels = c("NºOff", "AFR", "ALR", "IBI"))
dev.off()

#4.- Life-history traits~socioeconomic score analysis

#R packages to use
#R packages
#"survival" package
update.packages("survival")
install.packages("survival")
library(survival)
#"fitdistrplus" package
update.packages("fitdistrplus")
install.packages("fitdistrplus")
library(fitdistrplus)
#"qcc" package
install.packages("qcc")
library(qcc)
install.packages("MASS")
library(MASS)

#Socioeconomic score distribution
final$pse <- as.numeric(final$pse)
is.numeric(final$pse)
descdist(final$pse, discrete = FALSE, boot = 500)
fit.pse <- fitdist(final$pse, "norm")
summary(fit.pse)
plot(fit.pse)

#Life-history analysis
#Number of offspring
#Distribution
descdist(final$s5, discrete = TRUE, boot = 500)
fit.s5 <- fitdist(final$s5, "pois")
summary(fit.s5)
png("Fits5.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(fit.s5)
dev.off()
#Poisson regression
mods5 <- glm(s5 ~ pse, family = poisson(), data = final)
summary(mods5)
coef(mods5)
exp(coef(mods5))
#Overdispersion testing
qcc.overdispersion.test(final$s5, type="poisson")
#Figure
png("regs5.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(final$pse, final$s5, xlab = "Socioeconomic score", ylab = "Number of offspring", main = NULL, col = (gray(0.3)))
newx <- seq(min(final$pse), max(final$pse), length.out=6504)
predProbs<-predict(mods5,data.frame(pse=newx), type="response")
lines(seq(min(final$pse), max(final$pse), length.out=6504), predProbs, col=("black"), lwd=2)
dev.off()
#Age at first birth
#Variables definition
evento1 <- final$s6
grupo1.1 <- final$decilpse
grupo1.1 <- as.factor(grupo1.1)
grupo1.2 <- final$quintilpse
grupo1.2 <- as.factor(grupo1.2)
grupo1.3 <- final$cuartilpse
grupo1.3 <- as.factor(grupo1.3)
grupo1.4 <- final$tercilpse
grupo1.4 <- as.factor(grupo1.4)
grupo1.5 <- final$altobajose
grupo1.5 <- as.factor(grupo1.5)
#Kaplan-Meier survival analysis
kms5survival1 <- survfit(Surv(evento1) ~ 1)
summary(kms5survival1)
plot(kms5survival1, xlab = "Age at first birth")
title("Kaplan-Meier survival analysis for age first birth")
#Kaplan-Meier survival analysis by socioeconomic group
kms5survivalgse1.1 <- survfit(Surv(evento1) ~ grupo1.1)
summary(kms5survivalgse1.1)
png("surv10afb.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse1.1, xlab = "Age at first birth", col = (gray(0:7/10)), lty = (1:10))
legend("topright", ncol = 2, legend = c("decile 1", "decile 2", "decile 3", "decile 4", "decile 5", "decile 6", "decile 7", "decile 8", "decile 9", "decile 10"), col = gray(0:7/10), lty = (1:10), cex = 0.8)
dev.off()
kms5survivalgse1.2 <- survfit(Surv(evento1) ~ grupo1.2)
summary(kms5survivalgse1.2)
png("surv5afb.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse1.2, xlab = "Age at first birth", col = gray(0:5/5), lty = (1:5))
legend("topright", ncol = 2, legend = c("quintile 1", "quintile 2", "quintile 3", "quintile 4", "quintile 5"), col = gray(0:5/5), lty=(1:5), cex=0.8)
dev.off()
kms5survivalgse1.3 <- survfit(Surv(evento1) ~ grupo1.3)
summary(kms5survivalgse1.3)
png("surv4afb.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse1.3, xlab = "Age at first birth", col = gray(0:4/4), lty=(1:4))
legend("topright", ncol = 2, legend = c("quartile 1", "quartile 2", "quartile 3", "quartile 4"), col = gray(0:4/4), lty=(1:4), cex=0.8)
dev.off()
kms5survivalgse1.4 <- survfit(Surv(evento1) ~ grupo1.4)
summary(kms5survivalgse1.4)
png("surv3afb.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse1.4, xlab = "Age at first birth", col = gray(0:3/3), lty=(1:3))
legend("topright", legend = c("tertile 1", "tertile 2", "tertile 3"), col = gray(0:3/3), lty=(1:3), cex=0.8)
dev.off()
kms5survivalgse1.5 <- survfit(Surv(evento1) ~ grupo1.5)
summary(kms5survivalgse1.5)
png("surv2afb.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse1.5, xlab = "Age at first birth", col = gray(0:2/2), lty=(1:2))
legend("topright", legend = c("Low SES", "High SES"), col = gray(0:2/2), lty=(1:2), cex=0.8)
dev.off()
#Distribution
descdist(final$s6, discrete = TRUE, boot = 500)
fit.s6 <- fitdist(final$s6, "nbinom")
summary(fit.s6)
png("Fits6.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(fit.s6)
dev.off()
#Negative binomial regression
mods6 <- glm.nb(s6 ~ pse, data = final)
summary(mods6)
coef(mods6)
exp(coef(mods6))
#Figure
png("regs6.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(final$pse, final$s6, xlab = "Socioeconomic score", ylab = "Age at first birth", main = NULL, col = gray(0.3))
newx1 <- seq(min(final$pse), max(final$pse), length.out=6504)
predProbs1<-predict(mods6,data.frame(pse=newx1), type="response")
lines(seq(min(final$pse), max(final$pse), length.out=6504), predProbs1, col=1, lwd=2)
dev.off()

#Age at last birth
#Variables definition
evento2 <- final$euh
grupo2.1 <- final$decilpse
grupo2.1 <- as.factor(grupo2.1)
grupo2.2 <- final$quintilpse
grupo2.2 <- as.factor(grupo2.2)
grupo2.3 <- final$cuartilpse
grupo2.3 <- as.factor(grupo2.3)
grupo2.4 <- final$tercilpse
grupo2.4 <- as.factor(grupo2.4)
grupo2.5 <- final$altobajose
grupo2.5 <- as.factor(grupo2.5)
#Kaplan-Meier survival analysis
kms5survival2 <- survfit(Surv(evento2) ~ 1)
summary(kms5survival2)
plot(kms5survival2, xlab = "Age at last birth")
title("Kaplan-Meier survival analysis for \nage at last birth")
#Kaplan-Meier survival analysis by socioeconomic groups
kms5survivalgse2.1 <- survfit(Surv(evento2) ~ grupo2.1)
summary(kms5survivalgse2.1)
png("surv10euh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse2.1, xlab = "Age at last birth", col = gray(0:7/10), lty=(1:10))
legend("bottomleft", ncol = 2, legend = c("decile 1", "decile 2", "decile 3", "decile 4", "decile 5", "decile 6", "decile 7", "decile 8", "decile 9", "decile 10"), col = gray(0:7/10), lty=(1:10), cex = 0.8)
dev.off()
kms5survivalgse2.2 <- survfit(Surv(evento2) ~ grupo2.2)
summary(kms5survivalgse2.2)
png("surv5euh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse2.2, xlab = "Age at last birth", col = gray(0:5/5), lty=(1:5))
legend("bottomleft", ncol = 2, legend = c("quintile 1", "quintile 2", "quintile 3", "quintile 4", "quintile 5"), col = gray(0:5/5), lty = (1:5), cex=0.8)
dev.off()
kms5survivalgse2.3 <- survfit(Surv(evento2) ~ grupo2.3)
summary(kms5survivalgse2.3)
png("surv4euh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse2.3, xlab = "Age at last birth", col = gray(0:4/4), lty=(1:4))
legend("bottomleft", ncol = 2, legend = c("quartile 1", "quartile 2", "quartile 3", "quartile 4"), col = gray(0:4/4), lty=(1:4), cex=0.8)
dev.off()
kms5survivalgse2.4 <- survfit(Surv(evento2) ~ grupo2.4)
summary(kms5survivalgse2.4)
png("surv3euh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse2.4, xlab = "Age at last birth", col = gray(0:3/3), lty=(1:3))
legend("bottomleft", legend = c("tertile 1", "tertile 2", "tertile 3"), col = gray(0:3/3), lty=(1:3), cex=0.8)
dev.off()
kms5survivalgse2.5 <- survfit(Surv(evento2) ~ grupo2.5)
summary(kms5survivalgse2.5)
png("surv2euh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(kms5survivalgse2.5, xlab = "Age at last birth", col = gray(0:2/2), lty = (1:2))
legend("bottomleft", legend = c("Low SES", "High SES"), col = gray(0:2/2), lty = (1:2), cex=0.8)
dev.off()
#Distribution
descdist(final$euh, discrete = TRUE, boot = 500)
fit.euh <- fitdist(final$euh, "norm")
summary(fit.euh)
png("Fiteuh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(fit.euh)
dev.off()
#Linear regression
modeuh <- lm(euh ~ pse, data = final)
summary(modeuh)
coef(modeuh)
#Figure
png("regeuh.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(final$pse, final$euh, xlab = "Socioeconomic score", ylab = "Age at last birth", main = NULL, col=gray(0.3))
newx2 <- seq(min(final$pse), max(final$pse), length.out=6504)
predProbs2<-predict(modeuh,data.frame(pse=newx2), type="response")
lines(seq(min(final$pse), max(final$pse), length.out=6504), predProbs2, col=1, lwd=2)
dev.off()

#Interbirth intervals
#Distribution
descdist(test$ien, discrete = FALSE, boot = 500)
fit.ien <- fitdist(test$ien, "gamma")
png("Fitien.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(fit.ien)
dev.off()
#Gamma regresion
modien <- glm(ien ~ pse, family = Gamma(), data = test)
summary(modien)
exp(coef(modien))
#Figure
png("regien.png", width = 4.9, height = 5.7, units = "in", res = 500)
plot(test$pse, test$ien, xlab = "Socioeconomic score", ylab = "Interbirth intervals", main = NULL, col=gray(0.3))
newx3 <- seq(min(test$pse), max(test$pse), length.out=5791)
predProbs3<-predict(modien,data.frame(pse=newx3), type="response")
lines(seq(min(test$pse), max(test$pse), length.out=5791), predProbs3, col=1, lwd=2)
dev.off()
