
#### Read in Data ####
# Macroclimate Data
guel <- read.csv("F:\\Data Science\\FogNets\\Data\\macro_guelmim.csv")
sidi <- read.csv("F:\\Data Science\\FogNets\\Data\\macro_sidi.csv")

# Microclimate Data
train.2hr <- read.csv("F:\\Data Science\\FogNets\\Data\\train_micro_2hr.csv")

# Water yield
yield <- read.csv("F:\\Data Science\\FogNets\\Data\\wateryield.csv")

#### Preprocessing MICROclimate data ####
library(dplyr);

train <- left_join(train.2hr, yield, by="X"); rm(train.2hr);
sapply(train, function(x) sum(is.na(x))) # Missing values map

## Remove random missings
# We leave leafwet460_min with 1164 missing values as it is a very significant variable
train <- train[is.na(train$temp)==F,]
train <- train[is.na(train$wind_dir)==F,]

## Create date/time variables
# We have 3 representations of date/time...
# $x is the original date/time as a character object
# $x.lt is the list time for feature engineering
#...DEFER CREATION OF LIST TIME B/C IT'S NOT SUPPORTED BY DPLYR
# $x.ct is the calendar time for time matching

train$x.lt <-  as.POSIXlt(train$X, format = "%Y-%m-%d %H:%M:%S") #Date and time!
train$x.ct <-  as.POSIXct(train$X, format = "%Y-%m-%d %H:%M:%S")
train$x <- train$X
train <- select(train, -X)


#### Adding MACROclimate data ####
# Sidi ifni and Guelmim are much closer to the fog nets, so we will
# only be using the macroclimate data from those airports

## Process date/time guel
guel <- rename(guel, x=Local.time.in.Guelmim)
guel$x.ct <- as.POSIXct(guel$x, format = "%Y-%m-%d %H:%M:%S")
guel$x.lt <- as.POSIXlt(guel$x, format = "%Y-%m-%d %H:%M:%S")


# Reduce to Set 1 - microclimate data is available
