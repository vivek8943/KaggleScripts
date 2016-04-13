library(h2o)
library(h2oRClient)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    max_mem_size = '4g')
setwd("~/Desktop/Titanic")

train <- read.csv("train.csv")
test <- read.csv("test.csv")


train$Name[1]
test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

str(combi)


# Set a random seed
set.seed(129)
library(mice)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(combi[, !names(combi) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')
mice_output <- complete(mice_mod)
combi$Age <- mice_output$Age


# Split back into test and train sets
trainmice <- combi[1:891,]
testmice <- combi[892:1309,]

# Build a new tree with our new features
save(trainmice,testmice,file='mice.rda')

str(trainmice)
str(testmice)

trainmice$Survived <- as.factor(trainmice$Survived)
trainmice$Embarked <- as.factor(trainmice$Embarked)
trainmice$FamilyID <- as.factor(trainmice$FamilyID)
trainmice$Title <- as.factor(trainmice$Title)
trainmice$Surname <- as.factor(trainmice$Surname)
trainmice$Pclass <- as.factor(trainmice$Pclass)


testmice$Embarked <- as.factor(testmice$Embarked)
testmice$FamilyID <- as.factor(testmice$FamilyID)
testmice$Title <- as.factor(testmice$Title)
testmice$Surname <- as.factor(testmice$Surname)
testmice$Pclass <- as.factor(testmice$Pclass)



train.h2o <- as.h2o(trainmice)


test.h2o<-as.h2o(testmice)
head(train.h2o)

columnvector=colnames(train.h2o)
independant_vector=subset(columnvector,!(columnvector %in% c("Survived","Name","Surname","Ticket","PassengerId")))
dependant_vector=c("Survived")
train.h2o['Ticket']<-NULL
train.h2o['Name']<-NULL

model_1 <- h2o.deeplearning(x=independant_vector, y=dependant_vector,
                            l2=1e-3,
                            training_frame = train.h2o, # % of inputs dropout
                            hidden_dropout_ratios = c(0.45,0.35,0.5), # % for nodes dropout
                            activation = "RectifierWithDropout", epochs = 30,l1=1e-5,
                            hidden = c(200,158,50),variable_importances=TRUE,
                            validation=test.h2o ,classification_stop = 1,max_w2=10,
                            seed=5,loss = "CrossEntropy",stopping_metric="misclassification",
                            stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
                            stopping_rounds=4
) 

prediction.hex <- h2o.predict(model_1, test.h2o)
prediction <- as.numeric(prediction.hex$predict)
aa=as.data.frame(prediction$predict)

submit <- data.frame(PassengerId = test.h2o$PassengerId, Survived = aa)
write.csv(aa, file = "predict.csv", row.names = FALSE)


str(test)
h2o.shutdown(prompt=FALSE)




hyper_params <- list(
  hidden=list(c(102,52,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params
grid <- h2o.grid(
  "deeplearning",
  model_id="dl_grid", 
  training_frame=sampled_train,
  validation_frame=valid, 
  x=predictors, 
  y=response,
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)