install.packages("lubridate")  # to handle the date objects
install.packages("vcd")  # to do mosaic plot for contingency table
install.packages("gmodels")  # to do the contingency table
install.packages("data.table")  # to carry out some advanced data.frame operations
install.packages("tree")   # to fit trees (Ensemble Learning)
install.packages("randomForest")  # to Bagging and Random Forest (Ensemble Learning)
install.packages("gbm")  # to perform Boosting


library(lubridate)
library(vcd)
library(gmodels)
library(data.table)
library(tree)
library(randomForest)
library(gbm)

# Read the data from the csv file #
# It is a their location 1 year sales transaction data

sales_data = read.csv('Case Study Transaction Data 201407.csv', header = TRUE, na.strings = "NA")

str(sales_data)

dim(sales_data)  # 53836 rows and 28  columns

names(sales_data)  # names of all the columns

# Now let's take one column at a time and study.


#################### DMSTDESL : Sale Date  ############################
sales_data$DMSTDESL = ymd(sales_data$DMSTDESL)

sum(is.na(sales_data$DMSTDESL))  # No Null values

summary(sales_data$DMSTDESL)

# Min : 2013-01-02
# Max : 2013-12-18

# The data set has the entire year's transactiosns for a specific location.

sales_data$DMSTDESL[1]  # "2013-12-04 UTC"

####################### DMMONTH : Sale Month ########################
class(sales_data$DMMONTH) # the class is "integer" as expected.

sum(is.na(sales_data$DMMONTH))  # No Null values

sales_data$DMMONTH[1]  # 12

####################### DMSALWK : Sale Week  ########################
class(sales_data$DMSALWK)  # the class is "integer" as expected.

sum(is.na(sales_data$DMSALWK))  # No Null values

sales_data$DMSALWK[1]  # 49

############  SSALE_ : Sale # - given by aution for a particular sale #######
class(sales_data$SSALE_)  # the class is "integer" as expected.

sum(is.na(sales_data$SSALE_))  # No Null values

sales_data$SSALE_[1]  # 49

# SSALE_ looks like same as DMSALWK, let's check that

sum(sales_data$SSALE_ != DMSALWK)  # So, SSALE_ == DMSALWK

############ SLANE_ : Lane # that the vehicle is on sale ##########
class(sales_data$SLANE_) # the class is "integer" as expected.

sum(is.na(sales_data$SLANE_))  # No Null values

sales_data$SLANE_[1]  # 12

########### SRUN_ : Run # that the vehicle was given ########
class(sales_data$SRUN_)  # the class is integer as expected.

sum(is.na(sales_data$SRUN_))  # No Null values

sales_data$SRUN_[1]  # 227

summary(sales_data$SRUN_)

#########  STIMES : Run Time is the time the vehilce runs through the lane  ####
class(sales_data$STIMES)  # integer

summary(sales_data$STIMES)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0     105700   84720  122500  173200

# 173200 means 17:32:00 i.e. 17 hrs 32 minutes and 0 seconds
# 84720 means   8:47:20 i.e. 8 hrs 47 minutes and 20 seconds

# Let's convert it to seconds 

convertToSec = function(element)
{
  y = as.numeric(strsplit(as.character(element), "")[[1]])
  
  l = length(y)
  
  if (l <= 2)
  {
    ss = as.numeric(paste(y[l-1], y[l], sep = ""))
    tt = ss
  }
  else if (l > 2 & l <= 4)
  {
    ss = as.numeric(paste(y[l-1], y[l], sep = ""))
    mm = as.numeric(paste(y[l-3], y[l-2], sep = "")) * 60
    tt = ss + mm
  }
  else
  {
    ss = as.numeric(paste(y[l-1], y[l], sep = ""))
    mm = as.numeric(paste(y[l-3], y[l-2], sep = "")) * 60
    hh = as.numeric(paste(y[l-5], y[l-4], sep = "")) * 3600
    tt = ss + mm + hh
  }
  
  return(tt)
}

# creating a new column in the data frame called "STIMES_SEC" which has the Run time in seconds 
for (i in 1:nrow(sales_data))
{
  sales_data[i, "STIMES_SEC"] = convertToSec(sales_data$STIMES[i])
}

summary(sales_data$STIMES_SEC)

sum(is.na(sales_data$STIMES_SEC))  # No Null values

# But it has 0 values which is part of a data collection flaw, so let's handle it.

# So, Run Time increases as Run Number increases which is a good assumption to hold.
# Let's figure out the following distribution to understand the relation b/w Run time and Run Number.
# a) Rs = (Runtime / Run Number) given DMSOLD = Y (the car got sold)
# b) Rn = (Runtime / Run Number) given DMSOLD = N (the car did not get sold)
# c) Ru = (Runtime / Run Number) for all the cars which has a NON ZERO value in Runtime.

# Compute Rs

TotSoldCar = sum(sales_data$DMSOLD == 'Y')  # 25461

RunTimeSoldCar = sales_data[sales_data$DMSOLD == 'Y', ]$STIMES_SEC

RunNumberSoldCar = sales_data[sales_data$DMSOLD == 'Y', ]$SRUN_

Rs = RunTimeSoldCar / RunNumberSoldCar  # calculating the slope (i.e. the rate of change)

AvgRs = mean(Rs)  # Avg Slope

print(AvgRs)  # 720.2471

plot(RunNumberSoldCar, RunTimeSoldCar, xlab = 'Run Numbers', ylab = 'Run Time (seconds)',
     main = "Plotting RunTime and RunNumbers for 'Sold' cars", col = "blue")

# Compute Rn

TotNotSoldCar = sum(sales_data$DMSOLD == 'N') # 28375

TotNotSoldCar_NZroRnTm = sum(sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC != 0)  # 13760

RunTimeNotSoldCar = sales_data[sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC != 0, ]$STIMES_SEC

RunNumberNotSoldCar = sales_data[sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC != 0, ]$SRUN_

Rn = RunTimeNotSoldCar / RunNumberNotSoldCar  # calculating the slope (i.e. the rate of change)

AvgRn = mean(Rn)  # (Avg Slope)

print(AvgRn)  # 662.0829

plot(RunNumberNotSoldCar, RunTimeNotSoldCar, xlab = "Run Numbers", ylab = "Run Time (seconds)",
     main = "Plotting RunTime and RunNumbers for 'Sold' and 'NonZero Run Time' cars", col = "dark grey")

# Compute Ru

TotCar_NZroRnTm = sum(sales_data$STIMES_SEC != 0)  # 39221

RunTime_NZro = sales_data[sales_data$STIMES_SEC != 0, ]$STIMES_SEC

RunNumber_NZroRnTm = sales_data[sales_data$STIMES_SEC != 0, ]$SRUN_

Ru = RunTime_NZro / RunNumber_NZroRnTm

AvgRu = mean(Ru) # (Avg Slope)

print(AvgRu)  # 699.8412

plot(RunNumber_NZroRnTm, RunTime_NZro, xlab = "Run Numbers", ylab = "Run Time (seconds)",
     main = "Plotting RunTime and RunNumbers for all 'NonZero Run Time' cars", col = "black")

barplot(c(AvgRs/60, AvgRn/60, AvgRu/60), 
        col="light blue", names.arg=c("RunTime Slope (Sold Cars)", 
                                      "RunTime Slope (Not Sold & NonZero RnTm Cars)", 
                                      "RunTime Slope (NonZero RnTm Cars)"),
        ylim = c(1, 15),
        main="Average Run Time Slopes (in minutes)")

# A kind of pattern comes from this Average Run Time Slopes plot
# a) The cars "SOLD" were eventually run on the lane for longer time interval per run i.e. on average 720 seconds (i.e 12 minutes)
# b) The cars "NOT SOLD" were eventually run on the lane for shorter time interval per run i.e. on average 662 seconds (i.e 11 minutes)

# So, to fill up the Zero Run times which has been only for few of the 'Non Sold' cars we will use 
# the Average Run Time for the Not Sold cars for which we have a Non Zero value of RunTime.

TotNotSoldCar_ZroRnTm = sum(sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC == 0)  # 14615

TotCar_ZroRnTm = sum(sales_data$STIMES_SEC == 0)  # 14615

# Both the numbers are same so, we can conclude that Zero Run Time only exists in Not Sold class.

run_num_zrt = sales_data[sales_data$STIMES_SEC == 0, ]$SRUN_

sales_data[sales_data$STIMES_SEC == 0, ]$STIMES_SEC = run_num_zrt * AvgRn

TotCar_ZroRnTm = sum(sales_data$STIMES_SEC == 0)  # 0  # No, cars with 0 runtime

# A quick santy check on the Avg slope of Run time in the Not Sold class.

RunTimeNotSoldCar = sales_data[sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC != 0, ]$STIMES_SEC

RunNumberNotSoldCar = sales_data[sales_data$DMSOLD == 'N' & sales_data$STIMES_SEC != 0, ]$SRUN_

Rn = RunTimeNotSoldCar / RunNumberNotSoldCar  # calculating the slope (i.e. the rate of change)

AvgRn = mean(Rn)  # (Avg Slope)

print(AvgRn)  # 662.0829


# It is still the same and hence our records updation is successfull.

sales_data$STIMES_SEC = as.integer(sales_data$STIMES_SEC)  # converting the run time in seconds into a integer value.

sales_data$STIMES_H = sales_data$STIMES_SEC / 3600

############## DMTRANTYPE : Purchasing Channel ##################
class(sales_data$DMTRANTYPE)  # factor as expected

sales_data$DMTRANTYPE[1:5]

#[1]     LNE LNE        
#Levels:  LNE OVE SIM
# You see there are some missing values, they must be in the level, let's check that

levels(sales_data$DMTRANTYPE)
# [1] ""    "LNE" "OVE" "SIM"
# Now it's confirmed that there is a missing value in the levels, which we can convert to "NA"

sales_data$DMTRANTYPE_P = sapply(strsplit(as.character(sales_data$DMTRANTYPE), "\\s"), function(x){x[1]})  # space character

table(sales_data$DMTRANTYPE_P, exclude = "NA")

#   LNE   OVE   SIM  <NA> 
#  23072    19  2370 28375

sum(is.na(sales_data$DMTRANTYPE_P))  # 28375  so, these numbers tally.

sum(is.na(sales_data$DMTRANTYPE_P) & sales_data$DMSOLD == 'Y')  # 0 
# i.e. When the car is SOLD then the Purchasing Channel is recorded.

sum(is.na(sales_data$DMTRANTYPE_P) & sales_data$DMSOLD == 'N')  # 28375
# i.e. For all NOT SOLD cars the Purchasing Channel is not recorded.

# Let's try to see the distribution of SOLD cars by channels

barplot(table(sales_data$DMTRANTYPE_P), main="Distribution of Sold Cars by Channels")

############# DMSOLD : Sold (Y/N) ############
class(sales_data$DMSOLD)  # factor as expected.

sum(table(sales_data$DMSOLD))  # no missing values

barplot(table(sales_data$DMSOLD),
        ylim = c(0, 40000), main = "Distribution of Cars by SOLD and NOT SOLD")

# The distribution is pretty even.

############ DMOPCSUID : Seller ID ###########
class(sales_data$DMOPCSUID) # integer as expected

sales_data$DMOPCSUID[1:5]  # [1] 4900557 4907812 4908299 4908390 4910205

sum(is.na(sales_data$DMOPCSUID)) # No NA values.

############ DMSELLRNM : Seller Name #########
class(sales_data$DMSELLRNM)  # a factor as expected.

sum(is.na(sales_data$DMSELLRNM))  # 0

sales_data$DMSELLRNM[1:2]
# [1] JPMORGAN CHASE BANK,N.A. MANHEIM HOUSTON

############# SFLOOR : Floor Price (The seller's asking price for a vehicle, buyers would not know ) #####
class(sales_data$SFLOOR)  # integer as expected

sum(is.na(sales_data$SFLOOR))  # no NA vales

flrZero_NoSold = nrow(sales_data[sales_data$SFLOOR == 0 & sales_data$DMSOLD == 'N', ])  # 25935

flrZero_Sold = nrow(sales_data[sales_data$SFLOOR == 0 & sales_data$DMSOLD == 'Y', ])  # 23678

flrNonZero_NoSold = nrow(sales_data[sales_data$SFLOOR != 0 & sales_data$DMSOLD == 'N', ]) # 2440

flrNonZero_Sold = nrow(sales_data[sales_data$SFLOOR != 0 & sales_data$DMSOLD == 'Y', ]) # 1783

CrossTable(x = sales_data$SFLOOR == 0, y = sales_data$DMSOLD, prop.chisq = FALSE,
           digits = 4, prop.t = FALSE, prop.r = TRUE, prop.c = TRUE, dnn = c('FLOOR_Zero', 
                                                                             '*************SOLD**************'))

# There are total 7.84 % cars which has FLOOR values
# There are total 92.16 % cars which has NO FLOOR values
## The above pattern can be seen from the height of the Mosaic plot.


# Given FLOOR = 0 (NO FLOOR values)
# There are 52.27 % cars NOT SOLD
# There are 47.73 % cars SOLD
## This means that, when a seller doesn't provide a floor value then there is almost an equal distribution
## between SOLD and NOT SOLD classes, reason might be there is a less friction between the Buyer and Seller transactions.


# Given Floor != 0 (FLOOR values)
# There are 57.78 % cars NOT SOLD
# There are 42.22 % cars are SOLD
## This means that, when a seller provides a floor value then the distribution between both the classes are not equal.
## There are more cars not sold than sold. This might be because the Floor price is not reasonable and not been in sync
## with the Wholesale price guide MMR values.

# The above can be seen diagrammatically in the below Mosaic plot.

mosaic(with(sales_data, table(SFLOOR == 0, DMSOLD, dnn = c('FLOOR_Zero', 'Sold'))))

############### VNMMR : Manheim Marketing Report (MMR: National Average whole sale price for the particular vehicle)
class(sales_data$VNMMR)  # integer

sum(is.na(sales_data$VNMMR))  # there are 1457 NA values.

sales_data[is.na(sales_data$VNMMR), "VNMMR"] = 0  # updating all NA values to 0

summary(sales_data$VNMMR)
summary(sales_data$SFLOOR)

# Now, VNMMR might be used as a reference to set the FLOOR price (assumption)
# so let's analyze FLOOR just on the basis of VNMMR price.

# I want to create a factor column which will have values based on Floor and VNMMR prices.
# An example is given below-

# Floor  VNMMR    NewCOL
# 0       0        NotApplicable
# 100     0        NotConsulted
# 200     100      High
# 300     300      Equal
# 400     600      Low

sales_data$Floor_VNMMR = with(data = sales_data,
                      ifelse ((SFLOOR == 0), "NotApplicable",
                              ifelse ((SFLOOR != 0 & VNMMR == 0), "NotConsulted",
                                      ifelse ((SFLOOR > VNMMR & VNMMR != 0), "High",
                                              ifelse ((SFLOOR < VNMMR & SFLOOR != 0), "Low",
                                                      ifelse((SFLOOR == VNMMR), "Equal", "Unknown"))))))

sales_data$Floor_VNMMR = as.factor(sales_data$Floor_VNMMR)

par(mfrow = c(1, 1))

with(sales_data, CrossTable(DMSOLD, Floor_VNMMR,
                            prop.chisq = FALSE,
                            digits = 4, prop.t = FALSE, prop.r = FALSE, prop.c = TRUE))

# Print this table in your presentation (IMPORTANT)

########## DMMODELYR : Vehicle Model Year ###########
class(sales_data$DMMODELYR)  # integer as expected

sum(is.na(sales_data$DMMODELYR))  # no NA values

sales_data$DMMODELYR[1:5]  # [1] 2010 2009 2005 2002 2011

summary(sales_data$DMMODELYR)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1956    2003    2006    2005    2008    2014

with(sales_data, table(DMMODELYR))

#DMMODELYR
#1956 1961 1964 1965 1966 1967 1968 1970 1975 1976 1978 1980 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 
#2    1    3    3    1    7    3    5    1    7    6    2    2    1   11    6    8   11   15   24   11   36   45 
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#55  152  203  301  500  775 1161 1693 2323 3437 4412 5166 5233 5729 5641 4634 3044 4142 3140 1420  460    4 

par(mfrow = c(1, 1))
plot(with(sales_data, table(DMMODELYR)), xlab = "Model Year", ylab = "Number of Cars Registered for Sale",
     main = "Distribution of Registered Car for Sales by Model Year", type = "o")

sales_data[which(sales_data$DMMODELYR == 1984), "DMSOLD"]  # for testing purpose.
# [1] Y N Y N N N N N N N N

par(mfrow = c(1, 2))

plot(with(sales_data[sales_data$DMSOLD == 'Y', ], table(DMMODELYR)),
     xlab = "Model Year", ylab = "Number of Cars got SOLD",
     main = "Actual Distribution of SOLD cars by Model Year", type = "o", col = "brown")

plot(with(sales_data[sales_data$DMSOLD == 'N', ], table(DMMODELYR)),
     xlab = "Model Year", ylab = "Number of Cars NOT SOLD",
     main = "Actual Distribution of NOT SOLD cars by Model Year", type = "o", col = "blue")

carModelYr_dataframe = data.table(with(sales_data, table(DMSOLD, DMMODELYR)))

carModelYr_dataframe[, percen := sum(N), by=list(DMMODELYR)]
carModelYr_dataframe[, percen := (N/percen)*100]

sold_percen_byMdYr.df = carModelYr_dataframe[DMSOLD == 'Y', ]

notSold_percen_byMdYr.df = carModelYr_dataframe[DMSOLD == 'N', ]

par(mfrow = c(1, 2))

plot(sold_percen_byMdYr.df$DMMODELYR, sold_percen_byMdYr.df$percen, 
     xlab = "Model Year", ylab = "Percentage of Cars got SOLD", 
     main = "Percentage Distribution of Cars getting SOLD by Model Year", type = "o", col = "brown")

plot(notSold_percen_byMdYr.df$DMMODELYR, notSold_percen_byMdYr.df$percen, 
     xlab = "Model Year", ylab = "Percentage of Cars NOT SOLD", 
     main = "Percentage Distribution of Cars NOT SOLD by Model Year", type = "o", col = "blue")


# Now let's see if we can bin the Model Year.

summary(sales_data$DMMODELYR)

# Let 1956 to 1999 be VeryOld
# Let 2000 to 2006 be Old
# Let 2007 to 2015 be New

sales_data[(sales_data$DMMODELYR >= 1956 & sales_data$DMMODELYR < 1999), "ModelYear_P"] = 'VeryOld'

sales_data[(sales_data$DMMODELYR >= 1956 & sales_data$DMMODELYR < 1999), c("DMMODELYR", "ModelYear_P")]  # to check

sales_data[(sales_data$DMMODELYR >= 2000 & sales_data$DMMODELYR < 2006), "ModelYear_P"] = 'Old'

sales_data[(sales_data$DMMODELYR >= 2000 & sales_data$DMMODELYR < 2006), c("DMMODELYR", "ModelYear_P")] # to check

sales_data[(sales_data$DMMODELYR >= 2007 & sales_data$DMMODELYR < 2015), "ModelYear_P"] = 'New'

sales_data[(sales_data$DMMODELYR >= 2007 & sales_data$DMMODELYR < 2015), c("DMMODELYR", "ModelYear_P")] # to check

sales_data$ModelYear_P = as.factor(sales_data$ModelYear_P)

table(sales_data$ModelYear_P)

#   New     Old   VeryOld 
# 53215     595      26

tot_New = nrow(sales_data[sales_data$ModelYear_P == 'New', ])  # 53215
tot_Old = nrow(sales_data[sales_data$ModelYear_P == 'Old', ])  # 595
tot_VeryOld = nrow(sales_data[sales_data$ModelYear_P == 'VeryOld', ])  # 26

new_Y = nrow(sales_data[sales_data$ModelYear_P == 'New' & sales_data$DMSOLD == 'Y', ]) # 25150
old_Y = nrow(sales_data[sales_data$ModelYear_P == 'Old' & sales_data$DMSOLD == 'Y', ]) # 298
veryold_Y = nrow(sales_data[sales_data$ModelYear_P == 'VeryOld' & sales_data$DMSOLD == 'Y', ]) # 13

per_New_Y = (new_Y / tot_New)*100  # 47.26 %
per_Old_Y = (old_Y / tot_Old)*100  # 50.08 %
per_VeryOld_Y = (veryold_Y / tot_VeryOld)*100  # 50 %

par(mfrow = c(1, 1))

barplot(c(per_New_Y, per_Old_Y, per_VeryOld_Y), 
        col="light blue", names.arg=c("New (2007 to 2015)", 
                                      "Old (2000 to 2006)", 
                                      "Very Old (1956 to 1999)"),
        ylim = c(0, 70),
        main="Percentage of Cars SOLD by Model Year")

# Based on the Model Year the distribution of SOLD cars are about the same.

############# DMMAKE : Vehicle Make ################

class(sales_data$DMMAKE)  # factor as expected.

sum(is.na(sales_data$DMMAKE))  # No NA values

sales_data$DMMAKE[1:5]  # [1] SUBARU B M W  NISSAN NISSAN RAM

############# DMMODEL: Vehicle Model #############
class(sales_data$DMMODEL)  # factor as expected.

sum(is.na(sales_data$DMMODEL)) # no NA values

sales_data$DMMODEL[1:5]  # [1] OUTBACK 4C       X SERIES         ALTIMA 4C        XTERRA 4WD V6    2500 4WD 6C TDSL

############## DMBODY : Vehicle Body ############
class(sales_data$DMBODY)  # factor as expected

sum(is.na(sales_data$DMBODY)) # no NA values

sales_data$DMBODY[1:2] # [1] 4D WAGON 2.5I                   X5 3.0SI SPORT ACTIVITY VEHICLE

############ SSER17 : Vin # #############
class(sales_data$SSER17) # factor

sum(is.na(sales_data$SSER17))  # no NA values

sales_data$SSER17[1]  # 4S4BRBAC9A3355912  (not unique)

########### SSLEPR : Sale Price ################
class(sales_data$SSLEPR)  # integer as expected.

sum(is.na(sales_data$SSLEPR))  # no NA values

sales_data$SSLEPR[1:3]  # [1]     0 18500  5000

########### SMILES : Vehicle Mileage #############
class(sales_data$SMILES)  # integer as expected.

sum(is.na(sales_data$SMILES))  # no NA values

summary(sales_data$SMILES)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#21   66470   96680   99240  128100  435600

plot(with(sales_data[sales_data$DMSOLD == 'Y', ], table(SMILES)), type = "l")

plot(sales_data$DMSOLD, sales_data$SMILES, xlab = "Cars Sold No/Yes",
     ylab = "Vehicle Mileage", main = "Vehicle Mileage by Cars Sold/Not Sold", col = "purple")

########### DMJDCAT : Vehicle Category #################
class(sales_data$DMJDCAT)  # factor as expected.

sum(is.na(sales_data$DMJDCAT))  # no NA values

sales_data$DMJDCAT[1:3]  # MIDSIZE CAR SUV         MIDSIZE CAR

########### AGEDDAYS : # Days it takes to sell a car from vehicle received to vehicle sold ####
class(sales_data$AGEDDAYS)  # integer as expected

sum(is.na(sales_data$AGEDDAYS))  # no NA values

summary(sales_data$AGEDDAYS)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   5.893   7.000 407.000 

par(mfrow = c(1, 1))

plot(with(sales_data[sales_data$DMSOLD == 'Y', ], table(AGEDDAYS)), 
     xlab = "Days to sell a car", ylab = "Num of Cars Sold", 
     main = "Number of Cars Sold by Days", type = "l")


###########  DMPRECOND : Condition Grade BEFORE Electric Condition Report (0.0-5.0, blank/9.9 meaning no condition grade) ###
##########   DMPOSTCOND : Condition Grade AFTER Electric Condition Report (0.0-5.0, blank/9.9 meaning no condition grade)
##########  DMECRDATE : Date of Electric Condition Report (ECR, blank meaning no ECR report)

class(sales_data$DMPRECOND) # numeric
class(sales_data$DMPOSTCOND) # numeric
class(sales_data$DMECRDATE) # integer

sum(is.na(sales_data$DMPRECOND))
sum(is.na(sales_data$DMPOSTCOND))
sum(is.na(sales_data$DMECRDATE))

summary(sales_data$DMPRECOND)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   3.252   9.900   9.900

# DMPRECOND only available for SOLD cars. For Non-Sold cars it is 0, means not populated.
# Let's verify this.

nrow(sales_data[sales_data$DMPRECOND == 0 & sales_data$DMSOLD == 'N', ])  # 28375

nrow(sales_data[sales_data$DMPRECOND == 0, ]) # 28401

# So, there are few cars (26) that are SOLD but DMPRECOND is 0, which means they get a poor grade.



summary(sales_data$DMPOSTCOND)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   4.677   9.900   9.900

# DMPOSTCOND only available for SOLD cars. For Non-Sold cars it is 0, means not populated.
# Let's verify this.

nrow(sales_data[sales_data$DMPOSTCOND == 0 & sales_data$DMSOLD == 'N', ])  # 28375

nrow(sales_data[sales_data$DMPOSTCOND == 0, ]) # 28376

# So, there is just 1 record which is SOLD but the POSTCOND is recorded as 0 which means it did badly.


nrow(sales_data[sales_data$DMPRECOND == 9.9 & sales_data$DMSOLD == 'Y', ])  # 14296

nrow(sales_data[sales_data$DMPRECOND == 9.9, ]) # 14296

# SO, it is clear that for SOLD cars, 9.9 represents No Pre-Conditioning Grade.

nrow(sales_data[sales_data$DMPOSTCOND == 9.9 & sales_data$DMSOLD == 'Y', ])  # 25422

nrow(sales_data[sales_data$DMPOSTCOND == 9.9, ]) # 25422

# So, it is clear that for SOLD cars, 9.9 represents No Post-Conditioning Grade.

# Observations-

# a) DMECRDATE is NON ZERO if either a PreCond or a PostCond is done.
# b) DMECRDATE is ZERO if both Cond are not done.
# c) DMPRECOND only available for SOLD cars. For Non-Sold cars it is 0, means not populated.
# d) DMPOSTCOND only available for SOLD cars. For Non-Sold cars it is 0, means not populated.
# e) For SOLD cars, if pre cond is not done then DMPRECOND is 9.9
# f) For SOLD cars, if post cond is not done then DMPOSTCOND is 9.9

summary(sales_data$DMECRDATE)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0        0        0  8684000 20130000 20140000

table(sales_data[sales_data$DMECRDATE == 0, c("DMPRECOND", "DMPOSTCOND"), ])

# from the above table you can be clear that DMECRDATE should be Non Zero if one of the Cond is done.

with(sales_data, table(DMSOLD, DMECRDATE))

with(sales_data, table(DMECRDATE == 0, DMSOLD))

CrossTable(x = sales_data$DMECRDATE == 0, y = sales_data$DMSOLD, prop.chisq = FALSE,
           digits = 4, prop.t = FALSE, prop.r = TRUE, prop.c = TRUE, dnn = c('DMECRDATE_Zero', 
                                                                             '*************SOLD**************'))

# Given DMECRDATE = 0 (i.e. NO ECR Report)
# 53.17 % of the cars are NOT SOLD
# 46.83 % of the cars are SOLD

# Given DMECRDATE != 0 (i.e. ECR Report Done)
# 52.1 % of the cars are NOT Sold
# 47.9 % of the cars are SOLD


mosaic(with(sales_data, table(DMECRDATE == 0, DMSOLD, dnn = c('DMECRDATE_Zero', 'Sold'))))


# First let's explore the varieties of distributions of ****** SOLD ******* cars.

preND_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPRECOND == 9.9, ])  # PreCond Not Done

preD_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPRECOND != 9.9, ])  # PreCond Done

totSold = nrow(sales_data[sales_data$DMSOLD == 'Y', ])  # tot Sold Cars


preND_PER =  100*(preND_C / totSold)
preD_PER = 100*(preD_C / totSold)


posND_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPOSTCOND == 9.9, ]) # PostCond Not Done

posD_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPOSTCOND != 9.9, ])  # PostCond Done

posND_PER =  100*(posND_C / totSold)
posD_PER = 100*(posD_C / totSold)

bothND_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPRECOND == 9.9 & sales_data$DMPOSTCOND == 9.9, ]) # Both Not Done

bothD_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPRECOND != 9.9 & sales_data$DMPOSTCOND != 9.9, ]) # Both Done


bothND_PER =  100*(bothND_C / totSold)
bothD_PER = 100*(bothD_C / totSold)


preDpostND_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & (sales_data$DMPRECOND != 9.9 & sales_data$DMPOSTCOND == 9.9), ]) # Both Not Done

preNDpostD_C = nrow(sales_data[sales_data$DMSOLD == 'Y' & (sales_data$DMPRECOND == 9.9 & sales_data$DMPOSTCOND != 9.9), ]) # Both Done

preDpostND_PER =  100*(preDpostND_C / totSold)
preNDpostD_PER = 100*(preNDpostD_C / totSold)


par(mfrow = c(2, 2))

barplot(c(preND_PER, preD_PER), 
        col="light blue", names.arg=c("PreCond Not Done", 
                                      "PreCond Done"),
        ylim = c(0, 100),
        main="Percentage of Cars SOLD by PreCond")

barplot(c(posND_PER, posD_PER), 
        col="light blue", names.arg=c("PostCond Not Done", 
                                      "PostCond Done"),
        ylim = c(0, 100),
        main="Percentage of Cars SOLD by PostCond")

barplot(c(bothND_PER, bothD_PER), 
        col="light blue", names.arg=c("No Cond Done", 
                                      "Both Cond Done"),
        ylim = c(0, 100),
        main="Percentage of Cars SOLD by PreCond & PostCond")

barplot(c(preDpostND_PER, preNDpostD_PER), 
        col="light blue", names.arg=c("Pre Done Post NOTDone", 
                                      "Pre NOTDone Post Done"),
        ylim = c(0, 100),
        main="Percentage of Cars SOLD by PreCond & PostCond")

par(mfrow = c(2, 2))

plot(with(sales_data[sales_data$DMSOLD == 'Y', ], table(DMPRECOND)),
     xlab = "Pre-Condition Rating", ylab = "Number of Cars Sold", 
     main = "Pre-Condition rating for SOLD cars", type = "l", col = "blue")

plot(with(sales_data[sales_data$DMSOLD == 'Y', ], table(DMPOSTCOND)),
     xlab = "Post-Condition Rating", ylab = "Number of Cars Sold", 
     main = "Post-Condition rating for SOLD cars", type = "l", col = "blue")


plot(with(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPRECOND != 9.9, ], table(DMPRECOND)),
     xlab = "Pre-Condition Rating", ylab = "Number of Cars Sold", 
     main = "Pre-Condition rating for SOLD cars who actually did ECR", type = "l", col = "green")

plot(with(sales_data[sales_data$DMSOLD == 'Y' & sales_data$DMPOSTCOND != 9.9, ], table(DMPOSTCOND)),
     xlab = "Post-Condition Rating", ylab = "Number of Cars Sold", 
     main = "Post-Condition rating for SOLD cars who actually did ECR", type = "l", col = "green")

# For statistical modeling purpose let's create a separate column which will have a binary value.
# Yes / No based on if the ECR report if available or not respectively.


sales_data$ECR = with(data = sales_data,
               ifelse ((DMECRDATE == 0), "No", "Yes"))

sales_data$ECR = as.factor(sales_data$ECR)

table(sales_data$ECR)

#  No   Yes 
#30612 23224

sales_data$PreCond_B = with(data = sales_data,
                            ifelse ((DMPRECOND == 0 & DMSOLD == 'N'), "No", 
                                    ifelse ((DMPRECOND == 9.9 & DMSOLD == 'Y'), "No", "Yes")))

sales_data$PreCond_B = as.factor(sales_data$PreCond_B)

table(sales_data$PreCond_B)

#No   Yes 
#42671 11165

sales_data$PostCond_B = with(data = sales_data,
                            ifelse ((DMPOSTCOND == 0 & DMSOLD == 'N'), "No", 
                                    ifelse ((DMPOSTCOND == 9.9 & DMSOLD == 'Y'), "No", "Yes")))

sales_data$PostCond_B = as.factor(sales_data$PostCond_B)

table(sales_data$PostCond_B)

par(mfrow = c(1, 2))
with(sales_data, plot(PreCond_B, col = as.numeric(DMSOLD) + 1),
     main = "Red : NOT SOLD 
     Green : SOLD")  # color = 2 (red) and 3 (green)

with(sales_data, plot(PostCond_B, col = as.numeric(DMSOLD) + 1),
     main = "Red : NOT SOLD 
     Green : SOLD")  # color = 2 (red) and 3 (green)


# DMSOLD = N (1) and Y (2)

############## DMDETFEE : Vehicle Detailing Fee  ####################
class(sales_data$DMDETFEE)  # numeric as expected.

sum(is.na(sales_data$DMDETFEE))  # no NA values

summary(sales_data$DMDETFEE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   17.47    0.00  230.00 

with(sales_data, table(DMDETFEE == 0, DMSOLD))

mosaic(with(sales_data, table(DMDETFEE == 0, DMSOLD, dnn = c('DMDETFEE_Zero', 'Sold'))))

############ DMRECONFEE : Vehicle Reconditioning Fee ################
class(sales_data$DMRECONFEE)  # numeric as expected

sum(is.na(sales_data$DMRECONFEE))  # no NA values.

summary(sales_data$DMRECONFEE)

with(sales_data, table(DMRECONFEE <= 0, DMSOLD))

mosaic(with(sales_data, table(DMRECONFEE <= 0, DMSOLD, dnn = c('DMRECONFEE_LesOrEqualZero', 'Sold'))))

par(mfrow = c(1, 3))

with(sales_data, plot(DMRECONFEE, DMDETFEE, col = as.numeric(DMSOLD) + 1))

# All cars which were NOT SOLD did not have any DMRECONFEE & DMDETFEE

with(sales_data, plot(DMDETFEE, col = as.numeric(DMSOLD) + 1))

# There are few cars which got SOLD but were not charged DMDETFEE (Vehicle Detailing Fee)

with(sales_data, plot(DMRECONFEE, col = as.numeric(DMSOLD) + 1))

# There are few cars which got SOLD but were not charged DMRECONFEE (Vehicle Reconditioning Fee)
# Moreover DMRECONFEE is on an average more than DMDETFEE


############# % Arbitration : For each seller, % Arbitration = # arbitrated vehicles / total sold vehicles (at Manheim in 12 months)
class(sales_data$X..Arbitration)  # factor

class(levels(sales_data$X..Arbitration))  # character


levels(sales_data$X..Arbitration)
# [1] ""        "0.00%" ...
# Now it's confirmed that there is a missing value in the levels, which we can convert to "NA"

sales_data$X..Arbitration_P = sapply(strsplit(as.character(sales_data$X..Arbitration), "\\s"), function(x){x[1]})  # space character

class(sales_data$X..Arbitration_P)  # character

table(sales_data$X..Arbitration_P)

sales_data$X..Arbitration_P = as.numeric(gsub("\\%", "", sales_data$X..Arbitration_P))

class(sales_data$X..Arbitration_P) # numeric

sum(is.na(sales_data$X..Arbitration_P))

table(sales_data$X..Arbitration_P)

sellerRepu_dataframe = data.table(with(sales_data, table(DMSOLD, X..Arbitration_P)))

sellerRepu_dataframe[, percen := sum(N), by=list(X..Arbitration_P)]
sellerRepu_dataframe[, percen := (N/percen)*100]

sold_percen_byRepu.df = sellerRepu_dataframe[DMSOLD == 'Y', ]


notSold_percen_byRepu.df = sellerRepu_dataframe[DMSOLD == 'N', ]

par(mfrow = c(1, 1))

plot(sold_percen_byRepu.df$X..Arbitration_P, sold_percen_byRepu.df$percen, 
     xlab = "% Arbitration", ylab = "Percentage of Cars SOLD", 
     main = "Cars SOLD by % Arbitration for Seller", pch = 19)



######################################################################################################

################################# Statistical Modeling #####################################

s_data.modeling.df = with(sales_data, data.frame(DMSOLD, DMSTDESL, DMMONTH, DMSALWK, SSALE_, SLANE_, SRUN_, 
                                                 STIMES_H, SFLOOR, Floor_VNMMR, ModelYear_P, DMMODELYR,
                                                 SMILES, DMJDCAT, AGEDDAYS, X..Arbitration_P,
                                                 PreCond_B, PostCond_B, ECR))


s_data.modeling.df$DMSOLD = with(s_data.modeling.df,
                                 ifelse((DMSOLD == 'Y'), 1, 0))

s_data.modeling.df$DMSOLD = as.factor(s_data.modeling.df$DMSOLD)

class(s_data.modeling.df$DMSOLD)

sum(is.na(s_data.modeling.df))  # there are 162 NA values

s_data.modeling.df[is.na(s_data.modeling.df$X..Arbitration_P), "X..Arbitration_P"] = -1  # setting it to -1 (numeric) yet we'll be able to identify

sum(is.na(s_data.modeling.df))  # no NA values


set.seed(101)  # setting the seed value

# Setting the Train and Test regimen

train = sample(1:nrow(s_data.modeling.df), dim(s_data.modeling.df)[1]/2)

s_data.modeling.df.train = s_data.modeling.df[train, ]
s_data.modeling.df.test = s_data.modeling.df[-train, ]

DMSOLD.train = s_data.modeling.df.train$DMSOLD
DMSOLD.test = s_data.modeling.df.test$DMSOLD

#### Fitting Classification Tree (non-parametric method)

tree.auto = tree(DMSOLD ~ ., data = s_data.modeling.df, subset = train)

summary(tree.auto)

#Classification tree:
#  tree(formula = DMSOLD ~ ., data = s_data.modeling.df, subset = train)
#Variables actually used in tree construction:
#  [1] "AGEDDAYS"  "STIMES_H"  "ECR"       "PreCond_B"
#Number of terminal nodes:  8 
#Residual mean deviance:  0.4755 = 12790 / 26910 
#Misclassification error rate: 0.1133 = 3049 / 26918

par(mfrow = c(1, 1))

plot(tree.auto)
text(tree.auto, pretty = 0)


auto.pred = predict(tree.auto, s_data.modeling.df.test, type = "class")

table(DMSOLD.test, auto.pred)

mean(DMSOLD.test != auto.pred)

# The test classification error rate is 11.52 %


############### Bagging ##################

bag.auto = randomForest(DMSOLD ~ ., data = s_data.modeling.df, subset = train, 
                        mtry = 18, ntree = 500, importance = TRUE)


#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 18

#OOB estimate of  error rate: 9.19%
#Confusion matrix:
#  N     Y class.error
#N 12722  1443  0.10187081
#Y  1031 11722  0.08084372


bag.pred = predict(bag.auto, s_data.modeling.df.test, type = "class")

mean(DMSOLD.test != bag.pred)

# The test classification error rate is 9.32 %

################# Random Forest #######################

rf.auto = randomForest(DMSOLD ~ ., data = s_data.modeling.df, subset = train, 
                       mtry = 5, ntree = 500, importance = TRUE)  # mtry = sqrt(no. of predictors) for classification problem.

#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 5

#OOB estimate of  error rate: 8.71%
#Confusion matrix:
#  N     Y class.error
#N 12796  1369  0.09664666
#Y   976 11777  0.07653101


rf.pred = predict(rf.auto, s_data.modeling.df.test, type = "class")

mean(DMSOLD.test != rf.pred)

# The test classification error rate is 9.07 %

importance(rf.auto)

varImpPlot(rf.auto, sort=TRUE)  

# mean decrease in accuracy : how much inclusion of this predictor in the model reduces classification error.
# A low Gini (i.e. higher descrease in Gini) means that a particular predictor variable plays a greater role in partitioning the data into the defined classes.

################## Boosting ######################

### Using Additive Model ###

boost.auto_A = gbm(DMSOLD ~ .- DMSTDESL, data = s_data.modeling.df.train, 
                 distribution = "adaboost", n.trees = 5000, interaction.depth = 1,
                 shrinkage = 0.01, verbose = F)

#A gradient boosted model with adaboost loss function.
#5000 iterations were performed.
#There were 17 predictors of which 3 had non-zero influence.

summary(boost.auto_A)

#                           var     rel.inf
#AGEDDAYS                 AGEDDAYS 97.94729983
#STIMES_H                 STIMES_H  2.03307580
#ECR                           ECR  0.01962437

boost.prob_A = predict(boost.auto_A, newdata = s_data.modeling.df.test, n.trees = 5000, type = "response")

boost.pred_A = rep(0, nrow(s_data.modeling.df.test))

boost.pred_A[(boost.prob_A > 0.99)] = as.numeric(DMSOLD.test) - 1  # the factor's changing value as (1, 2)

mean(DMSOLD.test != boost.pred_A)

# 0 % test error rate

### Now introducing interaction terms ###

boost.auto_I = gbm(DMSOLD ~ .- DMSTDESL, data = s_data.modeling.df.train, 
                   distribution = "adaboost", n.trees = 5000, interaction.depth = 4,
                   shrinkage = 0.01, verbose = F)

#A gradient boosted model with adaboost loss function.
#5000 iterations were performed.
#There were 17 predictors of which 6 had non-zero influence.

summary(boost.auto_I)

#                          var      rel.inf
#AGEDDAYS                 AGEDDAYS 84.097232917
#ECR                           ECR  6.355445918
#STIMES_H                 STIMES_H  5.119030716
#PreCond_B               PreCond_B  4.341981076
#X..Arbitration_P X..Arbitration_P  0.078079735
#SRUN_                       SRUN_  0.008229637

boost.prob_I = predict(boost.auto_I, newdata = s_data.modeling.df.test, n.trees = 5000, type = "response")

boost.pred_I = rep(0, nrow(s_data.modeling.df.test))

boost.pred_I[(boost.prob_I > 0.99)] = as.numeric(DMSOLD.test) - 1  # the factor's changing value as (1, 2)

mean(DMSOLD.test != boost.pred_I)

