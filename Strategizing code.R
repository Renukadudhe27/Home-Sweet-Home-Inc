dataset <- read.csv("house.csv")
if(typeof(dataset$LOT.SQFT) == "character") {dataset$LOT.SQFT <- factor(dataset$LOT.SQFT)}
if(typeof(dataset$YR.BUILT) == "character") {dataset$YR.BUILT <- factor(dataset$YR.BUILT)}
if(typeof(dataset$GROSS.AREA) == "character") {dataset$GROSS.AREA <- factor(dataset$GROSS.AREA)}
if(typeof(dataset$LIVING.AREA) == "character") {dataset$LIVING.AREA <- factor(dataset$LIVING.AREA)}
if(typeof(dataset$FLOORS) == "character") {dataset$FLOORS <- factor(dataset$FLOORS)}
if(typeof(dataset$ROOMS) == "character") {dataset$ROOMS <- factor(dataset$ROOMS)}
if(typeof(dataset$BEDROOMS) == "character") {dataset$BEDROOMS <- factor(dataset$BEDROOMS)}
if(typeof(dataset$FULL.BATH) == "character") {dataset$FULL.BATH <- factor(dataset$FULL.BATH)}
if(typeof(dataset$HALF.BATH) == "character") {dataset$HALF.BATH <- factor(dataset$HALF.BATH)}
if(typeof(dataset$KITCHEN) == "character") {dataset$KITCHEN <- factor(dataset$KITCHEN)}
if(typeof(dataset$FIREPLACE) == "character") {dataset$FIREPLACE <- factor(dataset$FIREPLACE)}
if(typeof(dataset$REMODEL) == "character") {dataset$REMODEL <- factor(dataset$REMODEL)}
training <- tail(dataset,5220-20)
testing <- head(dataset,nrow(dataset)-(5220-20))
#install.packages("randomForest")
library(randomForest)
model <- randomForest(TOTAL.VALUE~LOT.SQFT+YR.BUILT+GROSS.AREA+LIVING.AREA+FLOORS+ROOMS+BEDROOMS+FULL.BATH+HALF.BATH+KITCHEN+FIREPLACE+REMODEL,data=training)
testingout <- subset(testing, FALSE)
REMODEL <- sort(unique(training$REMODEL))
d1 <- expand.grid(REMODEL = REMODEL)
for (row in 1:nrow(testing))
{
  testingbkup <- testing[row,]
  testingtemp <- testing[row,]
  for (i in 2:nrow(d1)) {testingtemp <- rbind(testingtemp, testingbkup)}
  testingtemp$REMODEL <- d1$REMODEL
  testingtemp$predicted <- predict(model,testingtemp,type="class")
  testingout <- rbind(testingout, testingtemp[which.max(testingtemp$predicted),])
}
write.csv(testingout,"house_Strategy.csv",row.names=FALSE)
training$predicted <- predict(model,training,type="class")
accuracy <- 100 * cor(training$predicted,training$TOTAL.VALUE) ^ 2
paste(round(accuracy,2),"%",sep="")
testingout
