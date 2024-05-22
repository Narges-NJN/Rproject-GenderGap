#install the data table package if you don't have it
library(data.table)
#loading the dataset
dt <- fread("lfsi_emp_a__custom_11491788_linear.csv")
#removing the columns that are not needed
dt  <- dt[ , -2] 
dt  <- dt[ , -2]
dt  <- dt[ , -1] 
dt  <- dt[ , -4]
dt  <- dt[ , -7] 
dt  <- dt[ , -1] 
head(dt)

gapDT <- dcast(dt, age + geo + TIME_PERIOD ~ sex, value.var = "OBS_VALUE")

# Calculate the difference and create a new column
gapDT[, gap := M - F]

