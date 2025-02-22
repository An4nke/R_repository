### Random forest

## libarys
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE) # install ggplot2 if requiered
}
library(ggplot2) # load ggplot2 package

if (!require("cowplot")) {
  install.packages("cowplot", dependencies = TRUE) # install cowplot if requiered
}
library(cowplot) # load cowplot package

if (!require("randomForest")) {
  install.packages("randomForest", dependencies = TRUE) # install randomForest if requiered
}
library(randomForest) # load randomForest package

library(randomForest) # load randomForest package
if (!require("ROCR")) {
  install.packages("ROCR", dependencies = TRUE) # install randomForest if requiered
}
library(ROCR) # load randomForest package


## data

# get link
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

# read data from link
data <- read.csv(url, header = F)

# show first 10 lines of data
head(data)

# naming of column
colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)

# show structure of data
str(data)

## clean up data
# change '?' to 'NA'
data[data == "?"] <- NA

# convert sex
data[data$sex == 0,]$sex <- "F" 
data[data$sex == 1,]$sex <- "M"

# convert into dataframe
data$cp <- as.factor(data$cp)
data$sex <- as.factor(data$sex)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

# convert intot integer because of original '?'
data$ca <- as.integer(data$ca)
data$thal <- as.integer(data$thal)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)

## make hd = heart disease easy on the eyes -> convert 0s to healthy & 1s to unhealthy
data$hd <- ifelse(test = data$hd == 0, yes = "Healthy", no = "Unhealthy")
data$hd <- as.factor(data$hd)
str(data)

## test data
# set set for random samples
set.seed(42)

# use random forest -> use all parameters
data.imputed <- rfImpute(hd ~ ., data = data, iter = 6)
model <- randomForest(hd ~., data = data.imputed, proximity = TRUE)

## create dataframe of error rate for plotting
oob.error.data <- data.frame(
  Trees = rep(1:nrow(model$err.rate), times = 3),
  Type = rep(c("OOB", "Healthy", "Unhealthy"), each = nrow(model$err.rate)),
  Error = c(model$err.rate[,"Healthy"],
            model$err.rate[,"Unhealthy"]))

### other example
## set random seed
set.seed(17)

# calculate size of each data set
data_set_size <- floor(nrow(iris)/2)

# generate random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)

# assign data to correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

# perform training 
rf_classifier = randomForest(Species ~., data = training, ntree = 100, mtry = 2, improtance = T)
# ntree number of generated trees
# mtry  number of features used in construction of each tree
# importance  enables calculation variable improtance

## look up model
rf_classifier

## importance parameters
varImpPlot(rf_classifier)
# MeanDecreaseAccuracy: rough estimate loss in prediction performance when variable omitted from training set
# MeanDecreaseGini:     measure of node impurity, highest purity -> each node contains only elements of a single class

## predict
predTab <- predict(rf_classifier, validation1[,-5])
predROC <- predict(rf_classifier,validation1[,-5],type="prob")

# confusion matrix
table(observed = validation1[,5], predicted = predTab)

## generate ROC curve
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")

# Specify the different classes 
classes <- levels(validation1$Species)

# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(predROC[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

############################################
## prediction of A/CC/CCAII..
############################################

# occurance of different Motifs?
# Motif A   0/1
# Motif B   0/1
# Motif C   0/1
# Motif D   0/1
# Motif E   0/1
# length flexible loop  Int

## table to be produced
# Prot_ID Motif_A flexible_loop Motif_B Motif_C Motif_D Motif_E
# MeBa_CCAI 1 70  1 1 1 1 1 1
# ..

# perlskript for motif search?? biostrings?? other R package?

