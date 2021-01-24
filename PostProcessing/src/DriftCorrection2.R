#rm(list = ls())

#DO NOT MODIFY THIS CODE AND PUSH TO THE REPOSITORY

#Names of columns, to run this code you need at least "Analysis" which refers to the injection number,
#"ID1" which is the sample ID, "RT" which is the retention time, "d15N" the nitrogen stable isotope value in permille
# and "AAID" which the the amino acid identification from the GC output

name <- c("Analysis", "ID1", "RT", "AreaAll", "d29N", "d15N", "AAID", "ID2") 

#This is the stable isotpe ratios of the internal and external standards. 
# MAKE SURE THIS IS UP TO DATE BASED ON HEEL STANDARDS!!! These values are as of 01/23/2020. 
#If your samples were esterified after 01/23/2020 these values should be verfied with the standard file on the HEEL drive
GLY <- 1.77
ALA <- -1.21
SER <- 1.135
PRO <-  1.55
VAL <- 0.361
ILE <- -2.344
NLE <- 14.163
ASP <- -9.93
PHE <- 2.192
LEU <- 0.35
GLU <- -3.336
TYR <- 3.24
THR <- -1.46

#Reading in the .csv of the NACHO data file and setting the file name for your output file
data.1 <- SL.1 <- read.csv("Data/Clean/TODO/20200127_Feddern_CSIA.csv") #modify with name of your data file
colnames(data.1)<-name
file.name <- "Data/Processed/SL.1.csv" #file name for output file including relative file path

###### Linear Model for Drift Correction #####
#Fit a linear model to your external standards with "Analysis" (injection number) as the dependent variable and 
#d15N as the response variable

data.1STD <- subset(data.1, ID1=="12AA") #get only the standard data 
AA<- unique(unlist(data.1STD$AAID)) #make a list of the AAs in the data

Intercept<-data.frame(Intercept=rep(NA,length(AA))) #initiate a dataframe for the intercepts of the linear model
for(i in 1:length(AA)){
    data <- subset(data.1STD, AAID==AA[i])
    Intercept[i,1]<- coef(summary(lm(as.numeric(d15N)~as.numeric(Analysis), data=data)))[1,1]
  }
Intercept #intercept values looped by aa


Slope<-data.frame(Slope=rep(NA,length(AA))) #initiate a dataframe for the slopes of the linear model
for(i in 1:length(AA)){
  data <- subset(data.1STD, AAID==AA[i])
  Slope[i,1]<- coef(summary(lm(as.numeric(d15N)~as.numeric(Analysis), data=data)))[2,1]
}
Slope#slope values looped by aa


Coef<- data.frame(AA, Intercept, Slope) #creating a dataframe of the slope and intercepts values for each AA

##### Adding Coefs and Standard Values to the dataset####


actual <- ifelse(data.1$AAID=="NLE",NLE, 
                 ifelse(data.1$AAID=="ALA",ALA,
                        ifelse(data.1$AAID=="VAL", VAL,
                               ifelse(data.1$AAID=="ILE", ILE, 
                                      ifelse(data.1$AAID=="ASP", ASP,
                                             ifelse(data.1$AAID=="PHE", PHE, 
                                                    ifelse(data.1$AAID=="SER", SER, 
                                                           ifelse(data.1$AAID=="THR", THR, 
                                                                  ifelse(data.1$AAID=="PRO", PRO,
                                                                         ifelse(data.1$AAID=="TYR", TYR,
                                                                                ifelse(data.1$AAID=="LEU", LEU,
                                                                                   ifelse(data.1$AAID=="GLU", -3.336,0))))))))))))

actual #check your data -- if there are 0s than you have an AA that is not included in the standard 12AA mix and the code will need
#to be modified acordingly. Data should only be used for AAs that are included in the external standard or standards that 
#do not have a substantial drift (check your d15N verse Analysis plot to verify if the AA is drifting)

slope <- ifelse(data.1$AAID=="NLE", filter(Coef, AA=="NLE")[1,3], 
                ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,3],
                       ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,3],
                              ifelse(data.1$AAID=="ILE", filter(Coef, AA=="ILE")[1,3], 
                                     ifelse(data.1$AAID=="ASP", filter(Coef, AA=="ASP")[1,3],
                                            ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,3],
                                                   ifelse(data.1$AAID=="THR", filter(Coef, AA=="THR")[1,3],
                                                          ifelse(data.1$AAID=="SER", filter(Coef, AA=="SER")[1,3],
                                                                 ifelse(data.1$AAID=="TYR", filter(Coef, AA=="TYR")[1,3],
                                                                        ifelse(data.1$AAID=="PRO", filter(Coef, AA=="PRO")[1,3],
                                                                               ifelse(data.1$AAID=="TYR", filter(Coef, AA=="TYR")[1,3],
                                                                                   ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,3], 0))))))))))))

intercept <-   ifelse(data.1$AAID=="NLE", filter(Coef, AA=="NLE")[1,2], 
                      ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,2],
                             ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,2],
                                    ifelse(data.1$AAID=="ILE", filter(Coef, AA=="ILE")[1,2], 
                                           ifelse(data.1$AAID=="ASP", filter(Coef, AA=="ASP")[1,2],
                                                  ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,2],
                                                         ifelse(data.1$AAID=="THR", filter(Coef, AA=="THR")[1,2],
                                                                ifelse(data.1$AAID=="SER", filter(Coef, AA=="SER")[1,2],
                                                                       ifelse(data.1$AAID=="TYR", filter(Coef, AA=="TYR")[1,2],
                                                                              ifelse(data.1$AAID=="PRO", filter(Coef, AA=="PRO")[1,2],
                                                                                     ifelse(data.1$AAID=="TYR", filter(Coef, AA=="TYR")[1,2],
                                                                                            ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,2], 0))))))))))))


#####Applying Drift Correction####
difference <- actual-(data.1$Analysis*slope+intercept) #Applying both a drift and step correction in on estep from linear model data
adj <- data.1$d15N+ difference
data <- cbind(data.1, adj)

#####Consolidating Triplicates#####
#samples should be run in triplicate, the mean and SD of the triplicate should be taken. Output data will include mean and SD for each sample,
#the standards, with a column for every Amino Acid
#NOTE: the SD of the standards IS NOT the standard precision, column conditioning injections should be omitted to calculated standard precision
#otherwise conditioning introduces extra variability into the data. See "CalculatingStandardPrecision.R" for a file that calculates this while 
#omitting the conditioning injections

mean <- aggregate(data['adj'], by = list(data$ID1, data$AAID), mean)
#names.means should be in the same order as the "AA" oject to make sure they are correctly ordered based on the order of the loop
#if they are not correctly ordered reorder names.meanss accordingly
names.means<- c("Sample.ID","ALA.mean", "ASP.mean", "GLU.mean", "ILE.mean", "LEU.mean", "NLE.mean", "PHE.mean", "PRO.mean", "SER.mean", "TYR.mean", "VAL.mean")
meanfull<-data.frame(matrix(0, nrow = length(unique(mean$Group.1)), ncol = length(AA)+1)) #initiate a dataframe for the intercepts of the linear model
meanfull[,1]<-mean%>%
  filter(Group.2  == AA[3])
for(i in 1:length(AA)){
  meanfull[1:length(unique(mean$Group.1)),i+1]<- mean%>%
  filter(Group.2  == AA[i])%>%
  select(adj)
}
colnames(meanfull)<- names.means
meanfull #ALWAYS check to make sure everything looks right! small errors can break the code


meanfull<-data.frame(matrix(0, nrow = length(unique(mean$Group.1)), ncol = length(AA)+1)) #initiate a dataframe for the intercepts of the linear model
meanfull[,1]<-mean%>%
  filter(Group.2  == AA[3])
colnames(meanfull)<- names.means

subset<-function(dataframe, AA) {
  data.frame(subset(data.1, AAID==AA))
}


sd<- aggregate(data['adj'], by = list(data$ID1, data$AAID), sd)
  #names.sds should be in the same order as the "AA" oject to make sure they are correctly ordered based on the order of the loop
  #if they are not correctly ordered reorder names.sds accordingly
names.sds<- c("Sample.ID","ALA.sd", "ASP.sd", "GLU.sd", "ILE.sd", "LEU.sd", "NLE.sd", "PHE.sd", "PRO.sd", "SER.sd", "TYR.sd", "VAL.sd")
sdfull<-data.frame(matrix(0, nrow = length(unique(sd$Group.1)), ncol = length(AA)+1)) #initiate a dataframe for the intercepts of the linear model
sdfull[,1]<-sd%>%
  filter(Group.2  == AA[1])
for(i in 1:length(AA)){
  sdfull[1:length(unique(sd$Group.1)),i+1]<- sd%>%
    filter(Group.2  == AA[i])%>%
    select(adj)
}
colnames(sdfull)<- names.sds 
Corrected <- merge(meanfull,sdfull, by="Sample.ID") #this merges the columns in both the SD and mean dataframes

write.csv(Corrected, file = file.name)

