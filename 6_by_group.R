###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate using by group argument in data.table  
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script covers applying simple functions by group with data.table syntax 
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

#================================#
# ==== 2. aggregate by group ====
#================================#

## we have now explored using data.table syntax to create/assign columns and apply aggregation functions like mean() or max() to one column in a 
## data.table, possibly dependent on the rows selected  

## in this module we explore adding the "by = " argument to data.tables, to create aggregate results by some other columns in the dataset

## in module 5 we subsetted the data down to one test type and used data.table syntax to calculate the mean
## of this group's scale score. now we will use the same syntax but add a "by group" 

# convert test scale score variable to numeric using data.table assignment operator 
raw_test_data[ , test_scale_score := as.numeric(test_scale_score)]

# create a dataset of test scale score means by grade. this function now creates a new dataset, so it is printed to the console.  
raw_test_data[ , mean(test_scale_score), by = "test_grade"]

## the list function can name created variables and allow multiple different functions to be applied 
## to the same "by" group, on different columns  

# name the mean scale score column "test_mean" in output data.table
raw_test_data[ , list(test_mean = mean(test_scale_score)), by = "test_grade"]

# calculate mean and standard deviation by grade 
raw_test_data[ , list(test_mean = mean(test_scale_score), test_stdev = sd(test_scale_score)), 
               by = "test_grade" ]

## we can also put multiple variables into the by group 

# calculate mean and standard deviation by grade, content area, and test name 
raw_test_data[ , list(test_mean = mean(test_scale_score), test_stdev = sd(test_scale_score)), 
               by = c("test_grade", "test_content_area", "test_name")]

## any number of functions can replace or be added to the "mean" and "sd" in the syntax above
## to calculate different summary statistics by group  

## for example, the built-in function ".N" "length" returns the number of rows in the data per group. this is useful as a more flexible version of the 
## table() function in base R. 

# create frequency table of test events by grade, content area, and name
raw_test_data[ , list(count = .N), 
               by = c("test_grade", "test_content_area", "test_name")]

## we can also save the output as a data.table for future use. also note that data.table supports the (.) function as an alias for list(), so it is
## commonly used in its place.

# save frequency table as a data.table  
test_frequency_by_group <- raw_test_data[ , .(count = .N), 
                                          by = c("test_grade", "test_content_area", "test_name")]

#============================#
# ==== 3. do it yourself ====
#============================#

## use the space below and the "raw_test_data" set to do the following:
## create a table of with 2 aggregate statistics: 
## the mean "test_performance_level" by "school_year" and name the column of means "avg_perform", 
## the frequency table of the number of entries by "school_year" and name the column "count" 
## call the dataset "myanswer"

# to check your answer, run the following code
load("P:/GitHub/training/answers_doit_yourself.Rdata")
# if you are correct, this line will evaluate to "TRUE"
all.equal(myanswer,answer_6bygroup)
