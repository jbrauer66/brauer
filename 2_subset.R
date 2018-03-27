###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate functionality to subset rows and columns in data 
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script walks through basic subsetting with data tables using subset()
###############################################################################################################################################################################.

# this command deletes all objects to give the program a clean slate when running from the top 
rm(list=ls())

#====================================#
# ==== 1. load data and packages ====
#====================================#

# load the data.table package 
library(data.table) 

# load the module data package
library(DelawareDataPackage)

# load the raw test data from the DelawareDataPackage 
raw_test_data	<- data.table(test_multi_row) 

#==============================================#
# ==== 2. subset columns using data tables ====
#==============================================#

## it is common that we will want to filter down a dataset to some portion of its rows and columns. there are several ways to do this. this module will 
## focus on a useful function called subset()

## the subset command takes the name of the dataset, 
## the second argument (subset = ) operates on the rows, and the third argument (select = ) operates on the columns. 

# select list of columns to keep, store new data table named "sub_cols_test_data" 
sub_cols_test_data <- subset(raw_test_data, select = c("school_year", "fake_student_id", "test_name",
                                                       "test_grade", "test_content_area", "test_scale_score"))

#========================================================#
# ==== 3. subset rows using data tables (1 variable) ====
#========================================================#

## next we want to only keep certain rows in the test data-- ones where the test is the State Test. 
## the table command creates a frequency table of items within a column to check what types of 
## values are contained within the column

# display a frequency table of the column "test_name" to the console 
table(sub_cols_test_data$test_name)

## we want to only keep observation rows where the variable test_name is equal to "State Test", so we 
## use a logical test on the column test_name to return only the rows where the condition is true: 
## that test_name is equal to "State Test"

# subset based on variable test name equal to "State Test"  
sub_rows_state_test <- subset(sub_cols_test_data, test_name == "State Test")

## alternatively we can subset by all values not equal to "Alternate Test" with an exclamation 
## point (not equal to) preceding the argument

# subset based on key variable test name not equal to "Alternate Test" 
sub_rows_state_test <- subset(sub_cols_test_data, test_name != "Alternate Test")

## a table of the same variable after the subset shows that the new data table only includes rows
## where the variable "test_name" is equal to "State Test" 

# display frequency tables of test name in original data 
table(sub_cols_test_data$test_name)

# display frequency table of test name in subset data
table(sub_rows_state_test$test_name)

#======================================================#
# ==== 4. subset rows, keeping more than one value ====
#======================================================#

## next we want to subset the test data further to keep only rows with content area of ELA and math

# display frequency table of test_content_area (note that the math abbreviation is "MAT") 
table(sub_cols_test_data$test_content_area)

## to select more than one value, use the "%chin%" (character in) command on variables with the 
## class of "character". 
## the %in% command works in the same way but on variables with the class of "numeric" 
## this is equivalent to testing if the test content area is equal to "ELA" or equal to "MAT" 

# subset to rows where test_content_area equal to "ELA" or "MAT"  
sub_rows_state_test_years <- subset(sub_cols_test_data, test_content_area %chin% c("ELA","MAT")) 

# display frequency table of test_content_area in subset data to confirm our subset worked correctly
table(sub_rows_state_test_years$test_content_area)

#===================================================#
# ==== 5. subset rows by more than one variable ====
#===================================================#

## next we want to keep only rows where the test is the State Test **and** the content area is ELA

# display frequency tables of test_name and test_content_area
table(sub_cols_test_data$test_name)
table(sub_cols_test_data$test_content_area)

# display frequency crosstab of test_name and test_content_area
table(sub_cols_test_data$test_name, sub_cols_test_data$test_content_area)

# subset to test_name equal to "State Test" and test_content_area equal to "ELA" using "&" operator 
sub_rows_state_test_and_ela <- subset(sub_cols_test_data, test_name == "State Test" & test_content_area == "ELA")

# display frequency crosstab of test_name and test_content_area to confirm subset worked as intended
table(sub_rows_state_test_and_ela$test_name, sub_rows_state_test_and_ela$test_content_area)

#================================================#
# ==== 5. subset rows based on numeric logic ====
#================================================#

## row subset logic can also choose rows based on numeric logic. test_scale_score is currently stored as
## a character so we will need to convert it to a numeric value first

# convert test scale score variable to numeric
sub_cols_test_data$test_scale_score <- as.numeric(sub_cols_test_data$test_scale_score)

# subset rows to eliminate test scores equal to zero
subset_rows_nonzero_test <- subset(sub_cols_test_data, test_scale_score > 0)

## one way to understand the data is to visualize it. we can make a histogram with the hist() function.
## the histogram will appear in the plots pane of RStudio

# produce histogram of test scale scores (note that no scores equal to 0 remain in the set)
hist(subset_rows_nonzero_test$test_scale_score)

#============================#
# ==== 6. do it yourself ====
#============================#

## use the space below to create a subset of "raw_test_data" that includes only 2011 and 2014 data. 
## only include the columns "school_year", "fake_student_id", "test_grade", and "test_scale_score".
## Name your final subset "myanswer".

# check your answer by running the following code
# if it evaluates to "TRUE", your code is correct!
all.equal(myanswer, answer_2subset, check.attributes=FALSE)