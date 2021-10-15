getwd()
setwd("/Users/agnesdetert/Desktop/Agnes/")
library(dplyr)
#"rctyn", "instno", "expwp5 will be excluded

#CHECKLIST & START_POINT_ST
CHECKLISTa <- read.csv("CHECKLIST.csv", header=T)
CHECKLIST <- subset(CHECKLISTa, select = c("twuid", "centre", "partno", "tt", "id", "yob","gender", "group", "age", "compl", "saliva1_stat", "saliva2_stat", "ansa_stat", "hexagon_stat", "gonogo_stat", "avoidance_stat", "smri_stat", "dti_stat", "fmri_stat", "neuropsych_stat", "mri_stat", "ff_elig", "elig_stat", "iq_t1"))

START_POINT_STa <- read.csv("~/Desktop/Agnes/START_POINT_ST.csv", header=TRUE)
START_POINT_ST <- subset(START_POINT_STa, select = c("twuid", "tt", "centre", "partno", "SESfeb2019", "SESfeb2019withincountry" )) #centre & partno & tt, weil zum Teil nicht vollständig bei Checklist

# I didn't use sdv (&compl later as well), because I don't know if it's important and there's a mistake in one of the two dataframes

#merging
dataset1a <- merge(CHECKLIST, START_POINT_ST, by=c("twuid","tt"), all.x=T, all.y=T)
dataset1b <- arrange(dataset1a, twuid)

#combine variables
centre <- rowMeans(dataset1b[, c("centre.x", "centre.y")], na.rm=TRUE)
dataset1c <- cbind(dataset1b, centre)

partno <- rowMeans(dataset1c[, c("partno.x", "partno.y")], na.rm=TRUE)
dataset1d <- cbind(dataset1c, partno)

#removing the not needed variables
dataset1 = subset(dataset1d, select = -c(centre.x, centre.y, partno.x, partno.y))

#remove Objects
remove(START_POINT_ST, START_POINT_STa, CHECKLIST, CHECKLISTa, dataset1a, dataset1b, dataset1c,dataset1d, centre, partno)

#Dataset1 & CBCLCMPL
CBCLCMPLa <- read.csv("CBCLCMPL.csv", header = T)
CBCLCMPL <- subset(CBCLCMPLa, select = c("twuid", "tt", "rctyn", "id", "yob", "instno", "expwp5", "gender", "group", "age", "compl", "cbcl_stat"))

#merging
dataset2a <- merge(dataset1, CBCLCMPL, by=c("twuid","tt"), all.x=T, all.y=T)
dataset2b <- arrange(dataset2a, twuid)
#write.csv(dataset2, "dataset2a.csv")

#combine variables
rctyn <- rowMeans(dataset2b[, c("rctyn.x", "rctyn.y")], na.rm=TRUE)
dataset2c <- cbind(dataset2b, rctyn)

dataset2a[,c("expwp5", "rctyn")] <- list(NULL)

#instno <- rowMeans(dataset2a[, c("instno.x", "instno.y")], na.rm=TRUE)
#dataset2b <- cbind(dataset2a, instno)
dataset2a[,c("instno")] <- list(NULL)

gender <- rowMeans(dataset2a[, c("gender.x", "gender.y")], na.rm=TRUE)
dataset2b <- cbind(dataset2a, gender)

group <- rowMeans(dataset2b[, c("group.x", "group.y")], na.rm=TRUE)
dataset2c <- cbind(dataset2b, group)

age <- rowMeans(dataset2c[, c("age.x", "age.y")], na.rm=TRUE)
dataset2d <- cbind(dataset2c, age)

compl <- rowMeans(dataset2d[, c("compl.x", "compl.y")], na.rm=TRUE)
dataset2e <- cbind(dataset2d, compl)

dataset2e$id <- ifelse(!is.na(dataset2e$id.x), dataset2e$id.x, dataset2e$id.y)

dataset2e$yob <- ifelse(!is.na(dataset2e$yob.x), dataset2e$yob.x, dataset2e$yob.y)

write.csv(dataset2e, "dataset2e.csv")

#removing not needed variables
dataset2 = subset(dataset2e, select = -c(rctyn.x, rctyn.y, instno.x, instno.y, expwp5.x, expwp5.y, gender.x, gender.y, group.x, group.y, age.x, age.y, compl.x, compl.y, id.x, id.y, yob.x, yob.y))

#remove Objects
remove(CBCLCMPL, CBCLCMPLa, dataset2a, dataset2b, dataset2c,dataset2d,dataset2, dataset2f, dataset2g, dataset2h, dataset2i, compl, expwp5, gender, group, age, instno, rctyn)

#dataset2 & CBCLSUB
CBCLSUBa <- read.csv("CBCLSUB.csv", header = T)
CBCLSUB <- subset(CBCLSUBa, select = c("twuid", "tt", "scale", "rawscore", "tscore"))

#CBCLSUB: rows to columns
library(reshape2)
CBCLSUBb <- dcast(CBCLSUB, twuid + tt ~ scale, value.var = "tscore")
CBCLSUBc <- dcast(CBCLSUB, twuid + tt ~ scale, value.var = "rawscore")

#rename variables
library(questionr)

#rename tscores
CBCLSUBd <- rename.variable(CBCLSUBb, "1", "cbcl_tscore_1")
CBCLSUBe <- rename.variable(CBCLSUBd, "2", "cbcl_tscore_2")
CBCLSUBf <- rename.variable(CBCLSUBe, "3", "cbcl_tscore_3")
CBCLSUBg <- rename.variable(CBCLSUBf, "4", "cbcl_tscore_4")
CBCLSUBh <- rename.variable(CBCLSUBg, "5", "cbcl_tscore_5")
CBCLSUBi <- rename.variable(CBCLSUBh, "6", "cbcl_tscore_6")
CBCLSUBj <- rename.variable(CBCLSUBi, "7", "cbcl_tscore_7")
CBCLSUBk <- rename.variable(CBCLSUBj, "8", "cbcl_tscore_8")
CBCLSUBl <- rename.variable(CBCLSUBk, "9", "cbcl_tscore_9")
CBCLSUBm <- rename.variable(CBCLSUBl, "10", "cbcl_tscore_10")
CBCLSUB_tscore <- rename.variable(CBCLSUBm, "11", "cbcl_tscore_11")

#rename rawscores 
CBCLSUBo <- rename.variable(CBCLSUBc, "1", "cbcl_rawscore_1")
CBCLSUBp <- rename.variable(CBCLSUBo, "2", "cbcl_rawscore_2")
CBCLSUBq <- rename.variable(CBCLSUBp, "3", "cbcl_rawscore_3")
CBCLSUBr <- rename.variable(CBCLSUBq, "4", "cbcl_rawscore_4")
CBCLSUBs <- rename.variable(CBCLSUBr, "5", "cbcl_rawscore_5")
CBCLSUBt <- rename.variable(CBCLSUBs, "6", "cbcl_rawscore_6")
CBCLSUBu <- rename.variable(CBCLSUBt, "7", "cbcl_rawscore_7")
CBCLSUBv <- rename.variable(CBCLSUBu, "8", "cbcl_rawscore_8")
CBCLSUBw <- rename.variable(CBCLSUBv, "9", "cbcl_rawscore_9")
CBCLSUBx <- rename.variable(CBCLSUBw, "10", "cbcl_rawscore_10")
CBCLSUB_rawscore <- rename.variable(CBCLSUBx, "11", "cbcl_rawscore_11")

#remove objects
remove(CBCLSUB, CBCLSUBa, CBCLSUBb, CBCLSUBc, CBCLSUBd, CBCLSUBe, CBCLSUBf, CBCLSUBg, CBCLSUBh, CBCLSUBi, CBCLSUBj, CBCLSUBk, CBCLSUBl, CBCLSUBm, CBCLSUBo, CBCLSUBp, CBCLSUBq, CBCLSUBr, CBCLSUBs, CBCLSUBt, CBCLSUBu, CBCLSUBv, CBCLSUBw, CBCLSUBx)

#merging
dataset3a <- merge(dataset2e, CBCLSUB_rawscore, by=c("twuid","tt"), all.x=T, all.y=T)
dataset3b <- merge(dataset3a, CBCLSUB_tscore, by=c("twuid", "tt"), all.x=T, all.y=T)
dataset3 <- arrange(dataset3b, twuid)

#remove objects
remove(dataset2e, dataset3a, dataset3b, CBCLSUB_tscore, CBCLSUB_rawscore)

#dataset3 & CECA
CECAa <- read.csv("CECA_Q_IMPUTED.csv", header = T)
CECA <- subset(CECAa, select = c("twuid", "tt","yob", "gender", "group", "c2m1","c2m2", "c2m3", "c2m4", "c2m5", "c2m6", "c2m7", "c2m8", "c2m9", "c2m10", "c2m11", "c2m12", "c2m13", "c2m14", "c2m15", "c2m16", "c3f1", "c3f2", "c3f3", "c3f4", "c3f5", "c3f6", "c3f7", "c3f8", "c3f9", "c3f10", "c3f11", "c3f12", "c3f13", "c3f14", "c3f15", "c3f16", "n_cecaq", "miss_cecaq", "miss_all_cecaq", "moth_anti_sum_imp", "moth_neg_sum_imp", "fath_anti_sum_imp", "fath_neg_sum_imp", "moth_anti_coa_imp", "fath_anti_coa_imp", "moth_neg_coa_imp", "fath_neg_coa_imp", "moth_anti_cob_imp", "fath_anti_cob_imp", "moth_neg_cob_imp", "fath_neg_cob_imp"))

#merging
dataset4a <- merge(dataset3, CECA, by=c("twuid","tt"), all.x=T, all.y=T)

#combine variables
gender <- rowMeans(dataset4a[, c("gender.x", "gender.y")], na.rm=TRUE)
dataset4b <- cbind(dataset4a, gender)

group <- rowMeans(dataset4b[, c("group.x", "group.y")], na.rm=TRUE)
dataset4c <- cbind(dataset4b, group)

dataset4c$yob <- ifelse(!is.na(dataset4c$yob.x), dataset4c$yob.x, dataset4c$yob.y)

#removing not needed variables
dataset4 = subset(dataset4c, select = -c(gender.y, group.x, group.y, yob.x, yob.y))

#remove Objects
remove(CECA, CECAa, dataset4a, dataset4b, dataset4c, gender, group)

#dataset4 & CTQ
CTQa <- read.csv("CTQ.csv",sep = ";", header = TRUE)
CTQ = subset(CTQa, select = -c(instno,rctyn, dt, partno, centre, compl, questid, sdv, RECORD_STATUS, LAST_MODIFIED, USER_FULL_NAME, USERNAME))

#merging
dataset5a <- merge(dataset4, CTQ, by=c("id","tt"), all.x=T, all.y=T)

#combine variables
age <- rowMeans(dataset5a[, c("age.x", "age.y")], na.rm=TRUE)
dataset5b <- cbind(dataset5a, age)

group <- rowMeans(dataset5b[, c("group.x", "group.y")], na.rm=TRUE)
dataset5c <- cbind(dataset5b, group)

#removing not needed variables
dataset5d = subset(dataset5c, select = -c(gender.x, gender.y, group.x, group.y, yob.x, yob.y, compl.x))

dataset5e <- arrange(dataset5d, id)

#combine variables
gender <- rowMeans(dataset5e[, c("gender.x", "gender.y")], na.rm=TRUE)
dataset5f <- cbind(dataset5e, gender)
group <- rowMeans(dataset5f[, c("group.x", "group.y")], na.rm=TRUE)
dataset5g <- cbind(dataset5f, group)

age <- rowMeans(dataset5g[, c("age.x", "age.y")], na.rm=TRUE)
dataset5h <- cbind(dataset5g, age)

dataset5h$yob <- ifelse(!is.na(dataset5h$yob.x), dataset5h$yob.x, dataset5h$yob.y)

dataset5h$id <- ifelse(!is.na(dataset5h$id.x), dataset5h$id.x, dataset5h$id.y)

#removing not needed variables
dataset5 = subset(dataset5d, select = -c(gender.x, gender.y, group.x, group.y, yob.x, yob.y, age.x, age.y, id.x, id.y))

#remove Objects
remove(CTQ, CTQa, dataset5a, dataset5b, dataset5c,dataset5d, dataset5e, dataset5f, gender, group, age, expwp5)

#dataset5 & ERQ
ERQa <- read.csv("ERQ_IMPUTED.csv", header = T)
ERQ <- subset(ERQa, select = c("twuid", "tt","n_erq", "miss_erq", "miss_all_erq", "reap_sum_imp", "suppr_sum_imp"))

#merging
dataset6a <- merge(dataset5, ERQ, by=c("twuid","tt"), all.x=T, all.y=T)
dataset6 <- arrange(dataset6a, twuid)


#remove Objects
remove(ERQ, ERQa, dataset6a)

#Dataset6 & femNAT_CSS-T1_KSLDC.DIAG_200708_EU
KSLDC.DIAGa <- read.csv("femNAT_CSS-T1_KSLDC.DIAG_200708_EU.csv", header = T)
KSLDC.DIAG = subset(KSLDC.DIAGa, select = -c(partno, centre))

#merging
dataset7a <- merge(dataset6, KSLDC.DIAG, by=c("twuid","tt"), all.x=T, all.y=T)
dataset7 <- arrange(dataset7a, twuid)
#write.csv(dataset2, "dataset2a.csv")

#remove Objects
remove(KSLDC.DIAG, KSLDC.DIAGa, dataset7a)

#Dataset7 & femNAT_CSS-T1_SAHA-Drugs.r_200602_EU
SAHA_Drugsa <- read.csv("femNAT_CSS-T1_SAHA-Drugs_200602_EU.csv", header = T)

SAHA_Drugsb = subset(SAHA_Drugsa, select = -c(USERNAME, LAST_MODIFIED, RECORD_STATUS, centre, partno, sdv, questid, id, yob, gender, group, age, compl, dt))
SAHA_Drugs <- arrange(SAHA_Drugsb, twuid)

#merging
dataset8a <- merge(dataset7, SAHA_Drugs, by=c("twuid","tt"), all.x=T, all.y=T)
dataset8 <- arrange(dataset8a, twuid)

#remove Objects
remove(SAHA_Drugs, SAHA_Drugsa, SAHA_Drugsb, dataset8a)

#Dataset9 & ICU
ICUa <- read.csv("ICU_IMPUTED.csv", header = T)
ICUb = subset(ICUa, select = -c(centre, partno, yob, gender, group))

#rename Variables (because of ICUCA)
ICUc <- rename.variable(ICUb, "callous_sum_imp", "callous_sum_imp_icu")
ICUd <- rename.variable(ICUc, "uncare_sum_imp", "uncare_sum_imp_icu")
ICUe <- rename.variable(ICUd, "unem_sum_imp", "unem_sum_imp_icu")
ICU <- rename.variable(ICUe, "total_sum_imp", "total_sum_imp_icu")

#merging
dataset9a <- merge(dataset8, ICU, by=c("twuid","tt"), all.x=T, all.y=T)
dataset9 <- arrange(dataset9a, twuid)

#remove Objects
remove(ICU, ICUa, ICUb, ICUc, ICUd, ICUe, dataset9a)

#Dataset10 & ICUCA
ICUCAa <- read.csv("ICUCA_IMPUTED.csv", header = T)
ICUCAb = subset(ICUCAa, select = -c(centre, partno, yob, gender, group))

#rename ICU to ICUCA
ICUCAc <- rename.variable(ICUCAb, "icu1", "icuca1")
ICUCAd <- rename.variable(ICUCAc, "icu2", "icuca2")
ICUCAe <- rename.variable(ICUCAd, "icu3", "icuca3")
ICUCAf <- rename.variable(ICUCAe, "icu4", "icuca4")
ICUCAg <- rename.variable(ICUCAf, "icu5", "icuca5")
ICUCAh <- rename.variable(ICUCAg, "icu6", "icuca6")
ICUCAi <- rename.variable(ICUCAh, "icu7", "icuca7")
ICUCAj <- rename.variable(ICUCAi, "icu8", "icuca8")
ICUCAk <- rename.variable(ICUCAj, "icu9", "icuca9")
ICUCAl <- rename.variable(ICUCAk, "icu10", "icuca10")
ICUCAm <- rename.variable(ICUCAl, "icu11", "icuca11")
ICUCAn <- rename.variable(ICUCAm, "icu12", "icuca12")
ICUCAo <- rename.variable(ICUCAn, "icu13", "icuca13")
ICUCAp <- rename.variable(ICUCAo, "icu14", "icuca14")
ICUCAq <- rename.variable(ICUCAp, "icu15", "icuca15")
ICUCAr <- rename.variable(ICUCAq, "icu16", "icuca16")
ICUCAs <- rename.variable(ICUCAr, "icu17", "icuca17")
ICUCAt <- rename.variable(ICUCAs, "icu18", "icuca18")
ICUCAu <- rename.variable(ICUCAt, "icu19", "icuca19")
ICUCAv <- rename.variable(ICUCAu, "icu20", "icuca20")
ICUCAw <- rename.variable(ICUCAv, "icu21", "icuca21")
ICUCAx <- rename.variable(ICUCAw, "icu22", "icuca22")
ICUCAy <- rename.variable(ICUCAx, "icu23", "icuca23")
ICUCAz <- rename.variable(ICUCAy, "icu24", "icuca24")
ICUCA1 <- rename.variable(ICUCAz, "callous_sum_imp", "callous_sum_imp_icuca")
ICUCA2 <- rename.variable(ICUCA1, "uncare_sum_imp", "uncare_sum_imp_icuca")
ICUCA3 <- rename.variable(ICUCA2, "unem_sum_imp", "unem_sum_imp_icuca")
ICUCA <- rename.variable(ICUCA3, "total_sum_imp", "total_sum_imp_icuca")

#merging
dataset10 <- merge(dataset9, ICUCA, by=c("twuid","tt"), all.x=T, all.y=T)
dataset10a <- arrange(dataset10, twuid)

#remove Objects
remove(ICUCA, ICUCAa, ICUCAb, ICUCAc, ICUCAd, ICUCAe, ICUCAf, ICUCAg, ICUCAh, ICUCAi, ICUCAj, ICUCAk, ICUCAl, ICUCAm, ICUCAn, ICUCAo, ICUCAp, ICUCAq, ICUCAr, ICUCAs, ICUCAt, ICUCAu, ICUCAv, ICUCAw, ICUCAx, ICUCAy, ICUCAz, ICUCA1, ICUCA2, ICUCA3, )


#Dataset11 & IQ
IQa <- read.csv("IQ_TRANSFORM.csv", header = T)
IQ = subset(IQa, select = -c(USERNAME, USER_FULL_NAME, LAST_MODIFIED, RECORD_STATUS, centre, partno, sdv))

#merging
dataset11 <- merge(dataset10a, IQ, by=c("twuid","tt"), all.x=T, all.y=T)
dataset11a <- arrange(dataset11, twuid)

dataset11a[,c("instno.x", "instno.y", "expwp5.x", "expwp5.y")] <- list(NULL)
dataset11a[,c("compl.x")] <- list(NULL)

#remove Objects
remove(IQ, IQa, dataset11a, dataset11b, dataset11c, dataset11d, dataset11e, expwp5, instno, rctyn)

#dataset12 & YPI
YPIa <- read.csv("YPI_IMPUTED.csv", header = T)
YPI = subset(YPIa, select = -c(centre, partno, yob, group, gender))

#merging
dataset12 <- merge(dataset11, YPI, by=c("twuid","tt"), all.x=T, all.y=T)
dataset13 <- arrange(dataset12, twuid)

#remove Objects
remove(YPI, YPIa, dataset12, dataset3, dataset4, dataset5, dataset6, dataset7,dataset8, dataset9, dataset10a)
remove(dataset1, dataset10, dataset11)

#merging
Neuro_psych<- read.csv("Neuropsych_data_selected_May2021_withNaNs_complete.csv", sep = ",") # csv files
names(Neuro_psych)[names(Neuro_psych) == 'group'] <- 'Group'
Final_Dataset<- merge(dataset13, Neuro_psych, by=c("twuid"), all.x=T, all.y=T)
Final_Dataset <- arrange(Final_Dataset, twuid)

Final_Dataset[,c("group.y","gender.y", "yob.x" )] <- list(NULL) 
names(Final_Dataset)[names(Final_Dataset) == 'group.y'] <- 'Group'
Final_Dataset[,c("yob.y","age.y" )] <- list(NULL) 

#remove Objects
remove(dataset13, dataset5g, dataset5h, Neuro_psych)

#replace NaN with NA
Final_Dataset$gender[is.nan(Final_Dataset$gender)] <- NA
Final_Dataset$age[is.nan(Final_Dataset$age)] <- NA
Final_Dataset$group[is.nan(Final_Dataset$group)] <- NA

#remove tt = 2
Final_Dataset <- Final_Dataset[!Final_Dataset$tt == 2, ]

#export to csv file
write.csv(Final_Dataset, "Final_Dataset.csv")

#for compl, expwp5, instno & rctyn as well?

#removing not needed variables 
Final_Dataset[,c("cbcl_rawscore_1","cbcl_rawscore_2" ,"cbcl_rawscore_3", "cbcl_rawscore_4", "cbcl_rawscore_5", "cbcl_rawscore_6", "cbcl_rawscore_7", "cbcl_rawscore_8", "cbcl_rawscore_9","cbcl_rawscore_10", "cbcl_rawscore_11" )] <- list(NULL)

#--------------------------------------------------------------------------------------------------
#PCA of CTQ

#load libraries
library(stats)
library(psych)
library(missMDA)
library(nFactors)
library(dplyr)

#set working directory
setwd("/Users/agnesdetert/Desktop/CTQ/")

#load CTQ
load("CTQ_scores.RData")

#rename
CTQ_scores <- rename(CTQ_scores, CTQ_Denial = CTQ_Bagatell)
CTQ_scores <- rename(CTQ_scores, tt = time)

#remove not needed variables
CTQb = subset(CTQ_scores, select = c("twuid","tt","CTQ_EmoAbuse","CTQ_PhysAbuse","CTQ_SexAbuse","CTQ_EmoNegl","CTQ_PhysNegl","CTQ_Denial"))

#remove NAs
CTQ_noNAs <- na.omit(CTQb)

#delete tt==2 (otherwise the rename rownames codes doesn't work, because there's more than one row with some of the twuid's)
CTQd <- CTQ_noNAs[!CTQ_noNAs$tt == 2, ]
CTQ = subset(CTQd, select = -c(tt))

#rename Rownames to identifier and remove identifier
rownames(CTQ)<- as.character(CTQ$twuid)
names(CTQ)
CTQ <- CTQ[,-1] #remove twuid
psych::describe(CTQ)
CTQ <- CTQ[, -6] #remove bv_sum
CTQ <- CTQ[, -3]

#round?
#round(CTQ, digits = 0)

#look at histograms
par(mfrow=c(1,2,3,4))
hist(CTQ$CTQ_EmoAbuse)
hist(CTQ$CTQ_PhysAbuse)
hist(CTQ$CTQ_EmoNegl)
hist(CTQ$CTQ_PhysNegl)

#make sure everything's numeric
apply(CTQ, 2, is.numeric)

#Associations Heatmap
COR <- cor(CTQ,CTQ, use="pairwise", method="spearman")
CorrPlot <- cor.plot(COR,numbers=TRUE,colors=TRUE,n=100,main="",zlim=c(-1,1), show.legend=TRUE, labels=NULL,n.legend=10,keep.par=TRUE,select=NULL, cuts=c(.001,.01),cex.axis=0.8) 

#Nice Correlation Matrix
require(Hmisc) 
x <- as.matrix(CTQ) 
cor <- rcorr(x, type="spearman")
R <- cor$r 
p <- cor$P 
n <- cor$n 
mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
diag(Rnew) <- paste(diag(R), " ", sep="") 
rownames(Rnew) <- colnames(x) 
colnames(Rnew) <- paste(colnames(x), "", sep="") 
Rnew <- as.matrix(Rnew)
Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
Rnew <- as.data.frame(Rnew) 
Rnew <- cbind(Rnew[1:length(Rnew)-1])
Rnew

#Is data suited for PCA?
cortest.bartlett(CTQ, n=nrow(CTQ)) #should be significant -> error message?
KMO<-KMO(CTQ); dim(CTQ); KMO #should be >0.5; consider removal of variables <0.5


#Check Number of Components (Scree plot and parallel analysis)
fitPCA <- princomp(CTQ, cor=TRUE)
par(mfrow=c(1,1))
plot(fitPCA, type="lines") # scree plot with Eigenvalues 

eigenvals <- eigen(cor(CTQ)) # compute eigenvalues
par <- parallel(subject=nrow(CTQ), var=ncol(CTQ), rep=100, cent=0.05)
Plot <- nScree(x= eigenvals$values, aparallel=par$eigen$qevpea)
plotnScree(Plot)


#.....parameters for factors & rotation method....#
nfac <- 2
rotation <-"varimax"

# ... choose data:
pcadata <- CTQ #data all measured on same scale
pcadata <- as.data.frame(lapply(CTQ, scale)) #if scales are measured in same units, otherwise optional
rownames(pcadata) <- rownames(CTQ)

#do we need this? we use pearson, right?
#pcadata <- cor(CTQ,CTQ, use="pairwise", method="spearman")

#Run PCA
PCA <- psych::principal(pcadata, nfactors = nfac, rotate = rotation)
print.psych(PCA, cut=0.3, sort = TRUE)
#capture.output(print.psych(PCA, cut=0.3, sort = TRUE), file="PCA_noNA.txt")

#Extract scores: this only workes if you have used data as input, not correlation matrix
#PCAscores <- data.frame(PCA$scores)
#PCAscores$twuid <- rownames(PCAscores)

#save(PCAscores, file="DatafilePCA_noNA.RData")


###Compute Scores - Dimensional Model:
# The idea is, that you save the weights and use them to calculate the factor scores.
# Like: FAScore1 = "CTQscore1"* weight1 + "CTQscore2"* weight2, etc
#rename the PC1.1 to PCA_Neglect
PCA_Neglect <-  scale(CTQ$CTQ_EmoAbuse)*PCA$weights[1]+
  scale(CTQ$CTQ_PhysAbuse)*PCA$weights[2]+
  scale(CTQ$CTQ_EmoNegl)*PCA$weights[3]+
  scale(CTQ$CTQ_PhysNegl)*PCA$weights[4]

#rename the PC1.2 to PCA_Abuse
PCA_Abuse <-  scale(CTQ$CTQ_EmoAbuse)*PCA$weights[5]+
  scale(CTQ$CTQ_PhysAbuse)*PCA$weights[6]+
  scale(CTQ$CTQ_EmoNegl)*PCA$weights[7]+
  scale(CTQ$CTQ_PhysNegl)*PCA$weights[8]

#rename the PC1.2 to PCA_Abuse
colnames(PCA_Neglect) <- "PCA_Neglect"
colnames(PCA_Abuse) <- "PCA_Abuse"

###Save Scores:
dimensional_scores <- data.frame(cbind(rownames(CTQ), scale(PCA_Neglect), scale(PCA_Abuse)))
names(dimensional_scores) <- c("twuid", "PCA_Neglect", "PCA_Abuse")
dimensional_scores$PCA_Neglect <- as.numeric(dimensional_scores$PCA_Neglect)
dimensional_scores$PCA_Abuse <- as.numeric(dimensional_scores$PCA_Abuse)

Dimensional_scores <- as.data.frame(apply(dimensional_scores, 2, as.numeric))  # Convert all variable types to numeric
sapply(Dimensional_scores, class)                                # Print classes of all columms

par(mfrow=c(1,2))
hist(Dimensional_scores$PCA_Neglect)
hist(Dimensional_scores$PCA_Abuse)

#remove Objects
remove(cor, COR, CTQ_noNAs, CTQb, CTQd, eigenvals, fitPCA, KMO, mystars, n, p, par, Plot, R, Rnew, x, nfac, rotation)

#.....parameters for factors & rotation method....#
nfac <- 1
rotation <-"varimax"

#do we need this? we use pearson, right?
#pcadata <- cor(CTQ,CTQ, use="pairwise", method="spearman")

#Run PCA
PCA_cumulative <- psych::principal(pcadata, nfactors = nfac, rotate = rotation)
print.psych(PCA_cumulative, cut=0.3, sort = TRUE)
#capture.output(print.psych(PCA, cut=0.3, sort = TRUE), file="PCA_noNA.txt")

#Extract scores: this only workes if you have used data as input, not correlation matrix
#PCAscores <- data.frame(PCA$scores)
#PCAscores$twuid <- rownames(PCAscores)

#save(PCAscores, file="DatafilePCA_noNA.RData")


###Compute Scores - Dimensional Model:
# The idea is, that you save the weights and use them to calculate the factor scores.
# Like: FAScore1 = "CTQscore1"* weight1 + "CTQscore2"* weight2, etc

PCA_Cumulative <-  scale(CTQ$CTQ_EmoAbuse)*PCA_cumulative$weights[1]+
  scale(CTQ$CTQ_PhysAbuse)*PCA_cumulative$weights[2]+
  scale(CTQ$CTQ_EmoNegl)*PCA_cumulative$weights[3]+
  scale(CTQ$CTQ_PhysNegl)*PCA_cumulative$weights[4]

colnames(PCA_Cumulative) <- "PCA_Cumulative"

###Save Scores:
cumulative_scores <- data.frame(cbind(rownames(CTQ), scale(PCA_Cumulative)))
names(cumulative_scores) <- c("twuid", "PCA_Cumulative")
cumulative_scores$PCA_Cumulative <- as.numeric(cumulative_scores$PCA_Cumulative)


par(mfrow=c(1,3))
hist(Dimensional_scores$PCA_Neglect)
hist(Dimensional_scores$PCA_Abuse)
hist(cumulative_scores$PCA_Cumulative)


#standard mean of trauma
#cumulative_scores$trauma_standardmean <- mean(scale(CTQ_scores$CTQ_EmoAbuse), scale(CTQ_scores$CTQ_PhysAbuse), scale(CTQ_scores$CTQ_EmoNegl), scale(CTQ_scores$CTQ_PhysNegl))
cumulative_scores$trauma_standardmean <- (scale(CTQ$CTQ_EmoAbuse) + scale(CTQ$CTQ_PhysAbuse) + scale(CTQ$CTQ_EmoNegl) + scale(CTQ$CTQ_PhysNegl))/4

#merge in the df
Cumulative_Scores <-subset (cumulative_scores, select = c ("twuid", "trauma_standardmean", "PCA_Cumulative" ))
Final_Dataset <- merge(Final_Dataset, Cumulative_Scores, by=c("twuid"), all.x=T, all.y=T, na.rm=TRUE)
#Dimensional_Scores <-subset (Dimensional_scores, select = c ("twuid", "PCA_Neglect", "PCA_Abuse" ))

Final_Dataset<- merge(Final_Dataset,Dimensional_scores , by=c("twuid"), all.x=T, all.y=T)

remove(dimensional_scores, Dimensional_Scores)
remove(cumulative_scores,Cumulative_Scores, PCA_Neglect, pcadata, PCA_Abuse, CTQ, CTQ_scores)
remove(PCA_Cumulative,Dimensional_scores, PCA_cumulative, PCA)
remove(nfac, rotation,CorrPlot)


setwd("/Users/agnesdetert/Desktop/Agnes/")
save(Final_Dataset,  file="Final_Dataset.RData")

#----------------------------------------------------------------------------------------

remove(CTQ_SexAbuse, CTQ_PhysNegl, CTQ_PhysAbuse, CTQ_EmoNegl, CTQ_EmoAbuse, CTQ_data3, CTQ_DATA2scale, CTQ_data2, CTQ_data1, CTQ_DATA_without_sgm, CTQ_data_no_sgm2, CTQ_data_no_sgm, CTQ_data, CTQ_Bagatell)
remove(fitPCA, fitPCA_without_sgm, KMO_without_sgm, model1, PCAscores)
remove(scores, Final_dataset_with_PCA, Final_dataset_withdprime, Final_dataset_DONE)
remove(CorrPlot, COR_without_sgm, dataset_legrand_final, data)
remove(c, r, function1)
remove(ImpMDA, my_predicted_height, my_predicted_shoesize, naidx, naidxperc, rev_EmoNegl, rev_PhysNegl, reverse, Neuro_psych)

# checking through and cleaning up the data structure
Final_Dataset[,c("group.1", "yob.y", "gender.y", "yob.x", "group.x")] <- list(NULL)
colnames(Final_Dataset)

Final_Dataset[,c("compl.y")] <- list(NULL) 

Final_Dataset[,c("yob.x","yob.y")] <- list(NULL) 

Final_Dataset[,c("group.x.1")] <- list(NULL) 
names(Final_Dataset)[names(Final_Dataset) == 'group.x'] <- 'group'

Final_Dataset <- rename.variable(Final_Dataset, "gender.x.1", "gender")
Final_Dataset <- rename.variable(Final_Dataset, "age.x.1", "age")

#remove t2 from the final dataset
Final_Dataset<- Final_Dataset [!Final_Dataset $tt == 2, ]


#Save the whole final dataset
save(Final_Dataset,  file="Final_Dataset.RData")



