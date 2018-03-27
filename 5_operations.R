###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate using math functions  
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script walks through working with specific variables and using basic math operators
###############################################################################################################################################################################.

# this command deletes all objects to give the program a clean slate when running from the top 
rm(list=ls())

#====================================#
# ==== 1. load data and packages ====
#====================================#

# load the data.table package 
library(data.table) 

# load the data package
library(DelawareDataPackage)

# load the raw test data from the DelawareDataPackage 
raw_test_data 	<- data.table(test_multi_row) 

#==================================#
# ==== 2. simple base commands ====
#==================================#

## the data contains multiple grades, subjects, years and test types. 
## we want to subset to only one test so the scores are comparable, and only keep scores greater 
## than 0 to throw out invalid events.

## we have forced the import to store all variables as "character" so first we switch the 
## raw_test_score variable to be re-stored as "numeric" 

# re-store test_scale_score as numeric variable 
raw_test_data[ , test_scale_score := as.numeric(test_scale_score) ]

# subset to 9th grade ELA State Test in 2012 
sub_test_ela_g9_2012 <- subset(raw_test_data, 
                               test_grade==9 & test_content_area=="ELA" & school_year==2012 & 
                                 test_name=="State Test" & test_scale_score > 0)

## next we will use base R commands to compute some overall summary statistics. note we are going back to non-data.table syntax, but we will go 
## back soon 

# compute and display the mean of test scale score
mean(sub_test_ela_g9_2012$test_scale_score)

# compute and display the standard deviation of test scale score
sd(sub_test_ela_g9_2012$test_scale_score)

# create a histogram of test scale score
hist(sub_test_ela_g9_2012$test_scale_score)

## numbers can also be saved as objects

# store the mean of test score
test_mean <- mean(sub_test_ela_g9_2012$test_scale_score)

# display test_mean and class of test_mean 
test_mean 
class(test_mean) 

# store the standard deviation of test score
test_stdev <- sd(sub_test_ela_g9_2012$test_scale_score)

# display test_stdev and class of test_stdev 
test_stdev
class(test_stdev) 

#========================================================#
# ==== 3. create new variables from existing objects ====
#========================================================#

## we can use numbers saved as objects to create new variables. data.table looks within the dataset for a 
## column with a supplied name, and if it doesn't find one, it looks in the global environment for an object
## with that name. 

# create standardized z score variable in sub_test_ela_g9_2012 dataset 
sub_test_ela_g9_2012[ , standardized_z_score_t := (test_scale_score - test_mean) / test_stdev ]

## data.table can be used to calculate thing directly in the second argument-- this can be used to replace the 
## dollar sign syntax we used to calculate the mean earlier 

# calculate the mean of the scale score
sub_test_ela_g9_2012[ , mean(test_scale_score)]

## using the row subset functionality, we could also have calculated the mean directly from the raw test data
raw_test_data[ test_grade==9 & test_content_area=="ELA" & school_year==2012 & test_name=="State Test" & test_scale_score > 0, mean(test_scale_score) ]

# create a new variable in the original data.table that repeats the mean for each row in the data set  
sub_test_ela_g9_2012[, test_mean := test_mean]

## it's not necessary to always calculate things separately-- to create the standardized test score variable, 
## we could also just calculate the mean within the data.table as well

# create standardized z score variable without using the pre-calculated mean
sub_test_ela_g9_2012[ , standardized_z_score_t_v2 := (test_scale_score - mean(test_scale_score)) / sd(test_scale_score) ]

#============================#
# ==== 4. do it yourself ====
#============================#

## use the space below and the "copy_test_data" dataset to subset the "raw_test_data" to test_period 1. 
## name the subset "myanswer". 
## then, create a new column, "arbitrary_math" by dividing each "test_raw_score" by the sum of
## the column "test_performance_level" and then subtract "test_scale_score". 
## HINT: remember to check the variable types of the dataset.

# create the myanswer dataset
copy_test_data <- copy(raw_test_data)

# to check your answer, run the following code
# if you are correct, this line will evaluate to "TRUE"
all.equal(myanswer,answer_5operations, check.attributes=FALSE)
