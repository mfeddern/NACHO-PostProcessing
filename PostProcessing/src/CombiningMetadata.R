rm(list = ls())

BM.1 <- read.csv('Data/Processed/BM1.csv')#[,-3]
BM.2 <- read.csv("Data/Processed/BM2.csv")#[,-3]
BM.3 <- read.csv("Data/Processed/BM3.csv")#[,-3]
BM.4 <- read.csv("Data/Processed/BM4.csv")
BM.5 <- read.csv("Data/Processed/BM5.csv")
BM.6 <- read.csv("Data/Processed/BM6.csv")
BM.7 <- read.csv("Data/Processed/BM7.csv")
BM.8 <- read.csv("Data/Processed/BM8.csv")
BM.9 <- read.csv("Data/Processed/BM9.csv")
BM.10 <- read.csv("Data/Processed/BM10.csv")
BM.11 <- read.csv("Data/Processed/BM11.csv")
BM.12 <- read.csv("Data/Processed/BM12.csv")
BM.13 <- read.csv("Data/Processed/BM13.csv")
BM.14 <- read.csv("Data/Processed/BM14.csv")#bm.6 and bm.2
BM.15<- read.csv("Data/Processed/BM15.csv")
BM.16<- read.csv("Data/Processed/BM16.csv")
BM.17<- read.csv("Data/Processed/BM17.csv")
BM.18 <- read.csv("Data/Processed/BM18.csv")
BM.19 <- read.csv("Data/Processed/BM19.csv")

RM.1 <-  read.csv("Data/Processed/RM1.csv")
RM.2 <-  read.csv("Data/Processed/RM2.csv")
RM.3 <-  read.csv("Data/Processed/RM3.csv")
RM.4 <-  read.csv("Data/Processed/RM4.csv")
RM.5 <-  read.csv("Data/Processed/RM5.csv")
RM.6 <-  read.csv("Data/Processed/RM6.csv")
RM.7 <-  read.csv("Data/Processed/RM7.csv")
RM.8 <-  read.csv("Data/Processed/RM8.csv")
RM.9 <-  read.csv("Data/Processed/RM9.csv")
RM.10 <-  read.csv("Data/Processed/RM10.csv")
RM.11<-  read.csv("Data/Processed/RM11.csv")
RM.12 <-  read.csv("Data/Processed/RM12.csv")
RM.13 <-  read.csv("Data/Processed/RM13.csv")
RM.14 <-  read.csv("Data/Processed/RM14.csv")
RM.15 <-  read.csv("Data/Processed/RM15.csv")
RM.16 <-  read.csv("Data/Processed/RM16.csv")
RM.17 <-  read.csv("Data/Processed/RM17.csv")
RM.18 <-  read.csv("Data/Processed/RM18.csv")
RM.19 <-  read.csv("Data/Processed/RM19.csv")

NM.1 <-  read.csv("Data/Processed/NM1.csv")
NM.2 <-  read.csv("Data/Processed/NM2.csv")
NM.3 <-  read.csv("Data/Processed/NM3.csv")
NM.4 <-  read.csv("Data/Processed/NM4.csv")
NM.5 <-  read.csv("Data/Processed/NM5.csv")
NM.6 <-  read.csv("Data/Processed/NM6.csv")
NM.7 <-  read.csv("Data/Processed/NM7.csv")
NM.8 <-  read.csv("Data/Processed/NM8.csv")
NM.9 <-  read.csv("Data/Processed/NM9.csv")
TD.1 <-  read.csv("Data/Processed/TD1.csv")
TD.2 <-  read.csv("Data/Processed/TD2.csv")
TD.3 <-  read.csv("Data/Processed/TD3.csv")
TD.4 <-  read.csv("Data/Processed/TD4.csv")
TD.5 <-  read.csv("Data/Processed/TD5.csv")
TD.6 <-  read.csv("Data/Processed/TD6.csv")





mast <- Reduce(function(x, y) merge(x, y,  all=TRUE), list(BM.1, BM.2, BM.3, BM.4, BM.5, BM.6, BM.7, BM.8,
                                                           BM.9, BM.10, BM.11, BM.12, BM.13, BM.14, BM.15,
                                                           BM.16, BM.17, BM.18, BM.19, RM.1, RM.2, RM.3, RM.4, RM.5, RM.6, RM.7, RM.8,
                                                           RM.9, RM.10, RM.11, RM.12, RM.13, RM.14, RM.15,
                                                           RM.16, RM.17, RM.18, RM.19, NM.1, NM.2, NM.3, NM.4, NM.5, NM.6, NM.7, NM.8,
                                                           NM.9, TD.1, TD.2, TD.3, TD.4, TD.5, TD.6))

#########################################################################################################################################
#############################################         UA Samples                            #############################################
#########################################################################################################################################
UA.1 <- read.csv('Data/Processed/UA1.csv')#[,-3]
UA.2 <- read.csv("Data/Processed/UA2.csv")#[,-3]
UA.3 <- read.csv("Data/Processed/UA3.csv")#[,-3]
UA.4 <- read.csv("Data/Processed/UA4.csv")
UA.5 <- read.csv("Data/Processed/UA5.csv")
UA.6 <- read.csv("Data/Processed/UA6.csv")
UA.7 <- read.csv("Data/Processed/UA7.csv")
UA.8 <- read.csv("Data/Processed/UA8.csv")
UA.9 <- read.csv("Data/Processed/UA9.csv")
UA.10 <- read.csv("Data/Processed/UA10.csv")
UA.11 <- read.csv("Data/Processed/UA11.csv")
UA.12 <- read.csv("Data/Processed/UA12.csv")
UA.13 <- read.csv("Data/Processed/UA13.csv")
UA.14 <- read.csv("Data/Processed/UA14.csv")#bm.6 and bm.2
UA.15<- read.csv("Data/Processed/UA15.csv")
UA.16<- read.csv("Data/Processed/UA16.csv")
UA.17<- read.csv("Data/Processed/UA17.csv")
UA.18 <- read.csv("Data/Processed/UA18.csv")
UA.19 <- read.csv("Data/Processed/UA19.csv")

UA.20 <- read.csv("Data/Processed/UA20.csv")
UA.21 <- read.csv("Data/Processed/UA21.csv")
UA.22 <- read.csv("Data/Processed/UA22.csv")
UA.23 <- read.csv("Data/Processed/UA23.csv")#bm.6 and bm.2
UA.24<- read.csv("Data/Processed/UA24.csv")
UA.25<- read.csv("Data/Processed/UA25.csv")
UA.26<- read.csv("Data/Processed/UA26.csv")
UA.27 <- read.csv("Data/Processed/UA27.csv")
#UA.28 <- read.csv("Data/Processed/UA28.csv")
SL.1 <- read.csv("Data/Processed/SL.1.csv")
SL.2 <- read.csv("Data/Processed/SL.2.csv")
SL.3 <- read.csv("Data/Processed/SL.3.csv")
SL.4 <- read.csv("Data/Processed/SL.4.csv")
SL.5 <- read.csv("Data/Processed/SL.5.csv")
SL.6 <- read.csv("Data/Processed/SL.6.csv")
SL.7 <- read.csv("Data/Processed/SL.7.csv")
SL.8 <- read.csv("Data/Processed/SL.8.csv")
SL.9 <- read.csv("Data/Processed/SL.9.csv")


mast <- Reduce(function(x, y) merge(x, y,  all=TRUE), list(UA.1, UA.2, UA.3, UA.4, UA.5, UA.6, UA.7, UA.8,
                                                           UA.9, UA.10, UA.11, UA.12, UA.13, UA.14, UA.15,
                                                           UA.16, UA.17, UA.18, UA.19, UA.20,UA.21,UA.22,UA.23,
                                                           UA.24,UA.25,UA.26,UA.27, 
                                                           SL.1, SL.2, SL.3, SL.4,SL.5, SL.6, SL.7, SL.8, SL.9))



#mast <- mast%>%arrange(Sample.ID)

unique(mast$sample.ID)
lookup <- unique(mast[!is.na(mast$"ALA.mean"),][c('Sample.ID', "ALA.mean")])
mast[is.na(mast$ALA.mean),]$ALA.mean<- lookup[match(mast[is.na(mast$ALA.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$ALA.mean
head(mast)


lookup <- unique(mast[!is.na(mast$"VAL.mean"),][c('Sample.ID', "VAL.mean")])
mast[is.na(mast$VAL.mean),]$VAL.mean<- lookup[match(mast[is.na(mast$VAL.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$VAL.mean

lookup <- unique(mast[!is.na(mast$"ILE.mean"),][c('Sample.ID', "ILE.mean")])
mast[is.na(mast$ILE.mean),]$ILE.mean<- lookup[match(mast[is.na(mast$ILE.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$ILE.mean

lookup <- unique(mast[!is.na(mast$"ASP.mean"),][c('Sample.ID', "ASP.mean")])
mast[is.na(mast$ASP.mean),]$ASP.mean<- lookup[match(mast[is.na(mast$ASP.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$ASP.mean

lookup <- unique(mast[!is.na(mast$"PHE.mean"),][c('Sample.ID', "PHE.mean")])
mast[is.na(mast$PHE.mean),]$PHE.mean<- lookup[match(mast[is.na(mast$PHE.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$PHE.mean

lookup <- unique(mast[!is.na(mast$"GLU.mean"),][c('Sample.ID', "GLU.mean")])
mast[is.na(mast$GLU.mean),]$GLU.mean<- lookup[match(mast[is.na(mast$GLU.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$GLU.mean

lookup <- unique(mast[!is.na(mast$"TYR.mean"),][c('Sample.ID', "TYR.mean")])
mast[is.na(mast$TYR.mean),]$TYR.mean<- lookup[match(mast[is.na(mast$TYR.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$TYR.mean

lookup <- unique(mast[!is.na(mast$"THR.mean"),][c('Sample.ID', "THR.mean")])
mast[is.na(mast$THR.mean),]$THR.mean<- lookup[match(mast[is.na(mast$THR.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$THR.mean

lookup <- unique(mast[!is.na(mast$"SER.mean"),][c('Sample.ID', "SER.mean")])
mast[is.na(mast$SER.mean),]$SER.mean<- lookup[match(mast[is.na(mast$SER.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$SER.mean

lookup <- unique(mast[!is.na(mast$"PRO.mean"),][c('Sample.ID', "PRO.mean")])
mast[is.na(mast$PRO.mean),]$PRO.mean<- lookup[match(mast[is.na(mast$PRO.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$PRO.mean

lookup <- unique(mast[!is.na(mast$"GLY.mean"),][c('Sample.ID', "GLY.mean")])
mast[is.na(mast$GLY.mean),]$GLY.mean<- lookup[match(mast[is.na(mast$GLY.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$GLY.mean

lookup <- unique(mast[!is.na(mast$"LEU.mean"),][c('Sample.ID', "LEU.mean")])
mast[is.na(mast$LEU.mean),]$LEU.mean<- lookup[match(mast[is.na(mast$LEU.mean),]$Sample.ID,
                                                    lookup$Sample.ID),]$LEU.mean




mast.1 <- mast[,2:14]
n_occur <- data.frame(table(mast$Sample.ID))
n_occur[n_occur$Freq > 1,]
reps <- data.frame(mast.1[mast.1$Sample.ID %in% n_occur$Var1[n_occur$Freq > 1],])
nonreps <- mast.1[mast.1$Sample.ID %in% n_occur$Var1[n_occur$Freq == 1],]
ag <- list(reps$Sample.ID)
con <-  aggregate(reps, by=ag, FUN=mean)
con.1 <- con[-2]
colnames(con.1) <- colnames(nonreps)

mast.2 <- rbind(nonreps, con.1)


metadataHS <- read.csv("Data/Processed/AKMetaData.csv")
metadataSL <- read.csv("Data/Processed/StellerSeaLionMetadataAK.csv")
metadata<-rbind(metadataHS,metadataSL)

data <- merge(mast.2, metadata, by="Sample.ID", all.x=TRUE)
write.csv(data, file = "Data/AKSealSeaLionAA.csv")



mean(mast$GLU.sd) #0.5212027
mean(mast$PHE.sd) #1.14
mean(mast$ASP.sd) #0.57
mean(mast$SER.sd) #1.17
mean(mast$GLY.sd) #0.339
mean(na.omit(mast$ALA.sd)) #0.27
mean(mast$VAL.sd) #1.125
mean(mast$PRO.sd) #0.316
mean(na.omit(mast$ILE.sd))
mean(na.omit(mast$LEU.sd)) #0.5543956
mean(na.omit(mast$NLE.sd)) #1.27
mean(na.omit(mast$THR.sd)) #1.27
