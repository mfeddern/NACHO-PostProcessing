rm(list = ls())


name <- c("Analysis", "ID1", "RT", "AreaAll", "d29N", "d15N", "AAID", "Conc")

ALA <- -1.21
VAL <- 0.361
ILE <- -2.344
NLE <- 14.163
ASP <- -9.93
PHE <- 2.192
#PRO 
#GLY
#LEU
GLU <- -3.336
TYR <- 3.24
THR <- -1.46
SER <- 1.135

data.1 <- BM.1 <- read.csv("Peak IDs CSV/BurkeMuseum_04-12-2018.csv")
#BM.2 <- read.csv("Peak IDs CSV/BurkeMuseum_04-15-2018.csv")
#BM.3 <- read.csv("Peak IDs CSV/BurkeMuseum_05-06-2018.csv")
#BM.4 <- read.csv("Peak IDs CSV/BurkeMuseum_05-08-2018.csv")
#BM.5 <- read.csv("Peak IDs CSV/BurkeMuseum_05-10-2018.csv")
#BM.6 <- read.csv("Peak IDs CSV/BurkeMuseum_Batch1_100ul_03-10-2018.csv")
#BM.7 <- read.csv("Peak IDs CSV/BurkeMuseum_Batch1_100ul_03-12-2018.csv")


colnames(data.1) <- name

NLE.data <- data.frame(subset(data.1, AAID=="NLE"))
ALA.data <- data.frame(subset(data.1, AAID=="ALA"))
VAL.data <- data.frame(subset(data.1, AAID=="VAL"))
ILE.data <- data.frame(subset(data.1, AAID=="ILE"))
ASP.data <- data.frame(subset(data.1, AAID=="ASP"))
PHE.data <- data.frame(subset(data.1, AAID=="PHE"))
#PRO.data <- data.frame(subset(data.1, AAID=="PRO"))
#GLY.data <- data.frame(subset(data.1, AAID=="GLY"))
#LEU.data <- data.frame(subset(data.1, AAID=="LEU"))
GLU.data <- data.frame(subset(data.1, AAID=="GLU"))
TYR.data <- data.frame(subset(data.1, AAID=="TYR"))
THR.data <- data.frame(subset(data.1, AAID=="THR"))
SER.data <- data.frame(subset(data.1, AAID=="SER"))

data.2 <- cbind(NLE.data[,1:2], NLE.data[,6], ALA.data[,6], VAL.data[,6], ILE.data[,6], ASP.data[,6], 
                PHE.data[,6], GLU.data[,6], TYR.data[,6], THR.data[,6], SER.data[,6])

names.2 <- c("Analysis", "ID1", "d15N.NLE", "d15N.ALA", "d15N.VAL", "d15N.ILE", "d15N.ASP", "d15N.PHE",
             "d15N.GLU", "d15N.TYR", "d15N.THR", "d15N.SER")
colnames(data.2) <- names.2

NLE.true <- rep(14.163, length(data.2$d15N.NLE))
nlediff <- as.numeric(data.2$d15N.NLE)-NLE.true
data.2 <- cbind(data.2, nlediff)

as.numeric(data.2[,3:12])-as.numeric(data.2[,13])

data.1STD <- subset(data.1, ID1=="12AA")
True <- ifelse(data.1STD$AAID == "ALA", ALA, ifelse(data.1STD$AAID == "VAL", VAL, ifelse(data.1STD$AAID == "ILE", ILE, ifelse(data.1STD$AAID == "ILE", ILE,ifelse(data.1STD$AAID == "NLE", NLE,ifelse(data.1STD$AAID == "ASP", ASP,ifelse(data.1STD$AAID == "PHE", PHE,ifelse(data.1STD$AAID == "TYR", TYR,ifelse(data.1STD$AAID == "GLU", GLU,ifelse(data.1STD$AAID == "THR", THR,0))))))))))
data.1STD <- cbind(data.1STD, True)

plot(as.numeric(subset(data.1STD, AAID=="ALA")$Analysis), as.numeric(subset(data.1STD, AAID=="ALA")$d15N.raw))  
model.ALA <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="ALA"))
ALA.coef <- data.frame(c("ALA", coef(summary(model.ALA))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="VAL")$Analysis), as.numeric(subset(data.1STD, AAID=="VAL")$d15N.raw))  
model.VAL <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="VAL"))
VAL.coef <- data.frame(c("VAL", coef(summary(model.VAL))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="ILE")$Analysis), as.numeric(subset(data.1STD, AAID=="ILE")$d15N.raw))  
model.ILE <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="ILE"))
ILE.coef <- data.frame(c("ILE", coef(summary(model.ILE))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="NLE")$Analysis), as.numeric(subset(data.1STD, AAID=="NLE")$d15N.raw))  
model.NLE <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="NLE"))
NLE.coef <- data.frame(data.frame(c("NLE", coef(summary(model.NLE))[1:2,1])))

plot(as.numeric(subset(data.1STD, AAID=="ASP")$Analysis), as.numeric(subset(data.1STD, AAID=="ASP")$d15N.raw))  
model.ASP <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="ASP"))
ASP.coef <- data.frame(c("ASP", coef(summary(model.ASP))[1:2,1]))

plot(subset(data.1STD, AAID=="PHE")$Analysis, subset(data.1STD, AAID=="PHE")$d15N.raw)  
model.PHE <- lm(d15N.raw~Analysis, data=subset(data.1STD, AAID=="PHE"))
PHE.coef <- data.frame(c("PHE", coef(summary(model.PHE))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="GLU")$Analysis), as.numeric(subset(data.1STD, AAID=="GLU")$d15N.raw))  
model.GLU <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="GLU"))
GLU.coef <- data.frame(c("GLU", coef(summary(model.GLU))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="TYR")$Analysis), as.numeric(subset(data.1STD, AAID=="TYR")$d15N.raw))  
model.TYR <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="TYR"))
TYR.coef <- data.frame(c("TYR", coef(summary(model.TYR))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="THR")$Analysis), as.numeric(subset(data.1STD, AAID=="THR")$d15N.raw))  
model.THR <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="THR"))
THR.coef <- data.frame(c("THR", coef(summary(model.THR))[1:2,1]))

plot(as.numeric(subset(data.1STD, AAID=="SER")$Analysis), as.numeric(subset(data.1STD, AAID=="SER")$d15N.raw))  
model.SER <- lm(as.numeric(d15N.raw)~as.numeric(Analysis), data=subset(data.1STD, AAID=="SER"))
SER.coef <- data.frame(data.frame(c("SER", coef(summary(model.SER))[1:2,1])))

Coefs <- rbind(ALA.coef, VAL.coef, ILE.coef, NLE.coef, ASP.coef, PHE.coef, GLU.coef, TYR.coef, THR.coef)
colnames(Coefs)<- c("AA", "Intercept", "Slope")

actual <- ifelse(data.1$AAID=="NLE",14.1, 
                 ifelse(data.1$AAID=="ALA",-1.21,
                        ifelse(data.1$AAID=="VAL", 0.36,
                               ifelse(data.1$AAID=="ILE", -2.34, 
                                      ifelse(data.1$AAID=="ASP", -9.93,
                                             ifelse(data.1$AAID=="PHE", 2.19, 
                                                    ifelse(data.1$AAID=="SER", 1.135, 
                                                           ifelse(data.1$AAID=="THR", -1.46, 
                                                                  ifelse(data.1$AAID=="GLU", -3.336,0)))))))))


slope <- ifelse(data.1$AAID=="NLE", coef(summary(model.NLE))[2,1], 
                ifelse(data.1$AAID=="ALA", coef(summary(model.ALA))[2,1],
                       ifelse(data.1$AAID=="VAL", coef(summary(model.VAL))[2,1],
                              ifelse(data.1$AAID=="ILE", coef(summary(model.ILE))[2,1], 
                                     ifelse(data.1$AAID=="ASP", coef(summary(model.ASP))[2,1],
                                            ifelse(data.1$AAID=="GLU", coef(summary(model.GLU))[2,1],
                                                   ifelse(data.1$AAID=="THR", coef(summary(model.THR))[2,1],
                                                          ifelse(data.1$AAID=="SER", coef(summary(model.SER))[2,1],
                                                                 ifelse(data.1$AAID=="TYR", coef(summary(model.TYR))[2,1],
                                                                        ifelse(data.1$AAID=="PHE", coef(summary(model.PHE))[2,1], 0))))))))))

intercept <-  ifelse(data.1$AAID=="NLE", coef(summary(model.NLE))[1,1], 
                     ifelse(data.1$AAID=="ALA", coef(summary(model.ALA))[1,1],
                            ifelse(data.1$AAID=="VAL", coef(summary(model.VAL))[1,1],
                                   ifelse(data.1$AAID=="ILE", coef(summary(model.ILE))[1,1], 
                                          ifelse(data.1$AAID=="ASP", coef(summary(model.ASP))[1,1],
                                                 ifelse(data.1$AAID=="GLU", coef(summary(model.GLU))[1,1],
                                                        ifelse(data.1$AAID=="THR", coef(summary(model.THR))[1,1],
                                                               ifelse(data.1$AAID=="SER", coef(summary(model.SER))[1,1],
                                                                      ifelse(data.1$AAID=="TYR", coef(summary(model.TYR))[1,1],
                                                                             ifelse(data.1$AAID=="PHE", coef(summary(model.PHE))[1,1], 0))))))))))

####NLE fix...subset calculate difference then merge???

difference <- actual-(data.1$Analysis*slope+intercept)
adj <- data.1$d15N+ difference
data <- cbind(data.1, adj)
#length(adj)
#length(data.1$Analysis)







FunctionRemoveReplactes <- function(dataFrame, groupingColumns, dataColumns, FUN) {
  
  colNames <- names(dataFrame)
  index <-  colNames %in% dataColumns
  
  listo <- vector("list", length(groupingColumns))
  for (i in 1:sum(index)) listo[[i]] <- dataFrame[,groupingColumns[i]]
  
  out <- dataFrame[,index]
  
  #names(data) <- c(groupingColumns, dataColumns)
  
  return (out)
}

groupcol <- c("ID", "AAID")
datacol <- c("adj")
#FUN <- mean()

out <- FunctionRemoveReplactes(data, groupcol, datacol, mean)
data.2 <- aggregate(out, by = list(data$ID1, data$ID), mean)
data.3 <- aggregate(out, by = list(data$ID1, data$ID), sd)
data.mean <- cbind(data.2, data.3$x)
colnames(data.mean)<- c("ID", "AA", "Mean", "SD")

data.mean <- format(data.mean, digits=3, nsmall=3, scientific=NA)


write.csv(data3, file = "Calculated_Data.csv")