
# --------- Assignement 2 ------------------

# ---------- LIBRARIES ---------------------

library(ggplot2)
library(PerformanceAnalytics) # for chart.correlation
library(caTools)



#-------------------------------------------

Airfare <- read.csv("AirfaresData.csv",header=TRUE)

templist <- c(6,10,11,12,13,16,17,18)

Temp <- Airfare[,templist]
rm(templist)

##### ------------------------ ADD NEW -------------- RECHECK COLUMNS 

# a

ggplot(Airfare,aes(x=S_INCOME,y=FARE)) + geom_point()

cor(Temp)

chart.Correlation(Temp, histogram = TRUE, pch = 19)

# b

summaryVacation <- aggregate(FARE~VACATION, data = Airfare, FUN = mean)
summarySW <- aggregate(FARE~SW, data = Airfare, FUN = mean)
summarySlot <- aggregate(FARE~SLOT, data = Airfare, FUN = mean)
summaryGate <- aggregate(FARE~GATE, data = Airfare, FUN = mean)

meanVacation <- abs(summaryVacation$FARE[1] - summaryVacation$FARE[2])
meanSW <- abs(summarySW$FARE[1] - summarySW$FARE[2])
meanSlot <- abs(summarySlot$FARE[1] - summarySlot$FARE[2])
meanGate <- abs(summaryGate$FARE[1] - summaryGate$FARE[2])

VarRows <- c("Vac","SW","Slot","Gate")
meanRows <- c(meanVacation,meanSW,meanSlot,meanGate)

df <- data.frame(VarRows,meanRows)
df


# c

set.seed(12345)

split = sample.split(Airfare$FARE, SplitRatio = 0.6)

training <- subset(Airfare, split == TRUE)
validation <- subset(Airfare, split == FALSE)

model <- lm(FARE ~ SW + DISTANCE, data = training)

summary(model)


plot(model)


#AIC(model)
BIC(model)

fitsummary = summary(model)
fitsummary$r.squared
#fitsummary$adj.r.squared
#rm(fitsummary)

# residuals(model)                      # residuals
# 
# hist(residuals(model),breaks=20)
# 
# qqnorm((residuals(model)), ylab="Standardized Residuals", xlab="Normal Scores", main="Residual")
# qqline(residuals(model))
# 
PredBase<-predict(model, validation, se.fit=TRUE)
PredBase
mean((validation[,"FARE"] - PredBase$fit)^2)
# 
# plot( (validation$FARE-PredBase$fit),PredBase$fit)


# d

model2 <- lm(FARE ~ SW + DISTANCE + COUPON + HI + S_INCOME
             + E_INCOME + S_POP + E_POP + NEW + PAX + VACATION + SLOT +
               GATE , data = training)

backward<-step(model2, direction='backward')
coefficients(backward)

BIC(backward)

fitsummary2 <- summary(backward)
fitsummary2$r.squared


PredBase2<-predict(backward, validation, se.fit=TRUE)  
PredBase2
mean((validation[,"FARE"] - PredBase2$fit)^2)










