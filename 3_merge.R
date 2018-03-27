###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate merging two datasets together 
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script covers merging data in r, with an overview of the implications of each merge type
###############################################################################################################################################################################.

# this command deletes all objects to give the program a clean slate when running from the top 
rm(list=ls())

#====================================#
# ==== 1. load data and packages ====
#====================================#

# load data table package 
library(data.table) 

# load the data package from github
library(DelawareDataPackage)

# load the raw test data and demographics data from the DelawareDataPackage 
raw_test_data 				  <- data.table(test_multi_row)
raw_test_data_subset 		<- data.table(test_single_row)
raw_student_dems		  	<- data.table(dems_single_row_year)
raw_student_dems_subset <- data.table(dems_single_row_student)

#==============================#
# ==== 2. one-to-one merge ====
#==============================#

## first we will take two small data sets and match them based on a common variable. 
## the data set we have saved as "raw_test_data_subset" has a math score from 2014 from a handful 
## of students

# view the raw test data 
raw_test_data_subset

## the data set we have saved as "raw_student_dems_subset" has demographic information for some of 
## the same students

# view the raw student demographic data
raw_student_dems_subset

## we want to combine this information into one wide data set with the merge command and save it as 
## a new R object
## to optimize speed and computing time we sort and index the "by" variables first with setkey 

# set key variables 
setkey(raw_test_data_subset,fake_student_id)
setkey(raw_student_dems_subset,fake_student_id)

# merge raw test data and raw demographics data 
student_test_and_dems <- merge(raw_test_data_subset, raw_student_dems_subset, by = "fake_student_id")

## an important note about this merge:  by default, R keeps only observations in the new dataset 
## that show up in both input sets.

## in this case, 3 students drop out because they don't exist in the subset test file. you can see 
## this in # of rows.

# display number of rows in initial datasets 
nrow(raw_test_data_subset)
nrow(raw_student_dems_subset)

# display number of rows in merged dataset 
nrow(student_test_and_dems)	

#===============================#
# ==== 3. many-to-one merge ====
#===============================#

## next we add one level of complexity and merge some information that is specific to a 
## student (their gender) and combine it with a larger data set for which there are multiple 
## records per student (the test file). 

## the first step is to subset the raw demographic data to the information we need 

# store a list of columns to subset from the demographics data
cols_to_subset <-  c("fake_student_id","gender")

# subset data table to student and gender 
student_gender <- subset(raw_student_dems_subset, select = cols_to_subset)

## next we will subset the test data so we are only looking at 2014 tests
## 	- instead of operating on the columns, we want to only keep rows where the school year is 2014 

# subset rows by keeping only records where school year is 2014 - student ids still appear more 
# than once if students take tests in mutliple subject tests in the same year
student_tests_2014 <- subset(raw_test_data, school_year == "2014")

## next we create a new data set that includes the gender information in each row of the test file 
## based on the student's id

# set key variables 
setkey(student_tests_2014, fake_student_id)
setkey(student_gender, fake_student_id)

# merge tests with student gender 
student_tests_with_gender_2014 <- merge(student_tests_2014, student_gender, by = "fake_student_id") 

# view the first 5 rows of the merged data.table (note the gender column)
head(student_tests_with_gender_2014)

#=======================================================#
# ==== 4. failed many-to-many merge and fixed merge ====
#=======================================================#

## next we want to merge a file containing multiple records per student with another file
## containing multiple records per student. 
## 	- to do this without errors we will need multiple unique identifiers to go along with the 
##    student id and match up the records properly 
## 	- in this case, these unique identifiers are the student id and the school year 

## first we attempt to merge the raw data by only student id

# set key variables 
setkey(raw_test_data,fake_student_id)
setkey(raw_student_dems,fake_student_id)

# merge test data and student demographics by student id
student_info_and_test <- merge(raw_test_data, raw_student_dems, by = "fake_student_id") 

## because we are working with data.tables, R errors and warns us that there are not enough unique 
## identifiers to match these records properly (simple data.frames would have merged improperly) 

## the merge should be by an additional variable, the year, to avoid the bad merge (for example, 
## matching a student's test record from 2012 to their demographic record from 2014) 

## specify all keys for each dataset in one command
# set key variables 
setkey(raw_test_data, fake_student_id, school_year)
setkey(raw_student_dems, fake_student_id, school_year)

# merge test data and student demographics by student id and school year 
student_info_and_test_fixed <-  merge(raw_test_data, raw_student_dems,
                                      by = c("fake_student_id","school_year"))

## now the variables are unique and we are sure that we are merging student records from the same 
## school years

# display the first 5 rows of the second attempted merge 
head(student_info_and_test_fixed) 

## when there is not a group of unique key variables, be very careful when attempting these types 
## of merges 

#===============================================#
# ==== 5. mismatched merges and merge rates ====
#===============================================#

## in many cases the rows of the key variables in each set will not match up exactly. here we want 
## to merge 2013 test scores with 2013 demographic information (a many-to-one merge).

## first we subset our two datasets to only 2013 data

# subset test data to 2013 
student_test_2013 <- subset(raw_test_data, school_year == "2013" )

# subset student demographic data to 2013 
student_info_2013 <- subset(raw_student_dems, school_year == "2013")

## next we check how many student demographic records we have in 2013 and how many unique students 
## show up in test file

# display number of rows in the 2013 demographics set 
nrow(student_info_2013)

# set key to fake_student_id
setkey(student_test_2013, fake_student_id)

# display number of unique student ids in the 2013 test set 
length(unique(student_test_2013$fake_student_id)) 

## there's a mismatch that would not have been obvious if we had only looked at the output of the 
## merge

# set key variables 
setkey(student_info_2013,fake_student_id,school_year)
setkey(student_test_2013,fake_student_id,school_year)

# merge subset student dems and subset student tests by student id and school year
student_test_and_dems_2013 <- merge(student_info_2013, student_test_2013,
                                    by = c("fake_student_id","school_year")) 

## the number of rows in the merged set is the same as the number of rows in the test data. the 
## extra student demographic record is dropped 

# display number of rows in initial test data
nrow(student_test_2013)

# display number of rows in the merged dataset 
nrow(student_test_and_dems_2013) 

## in many cases we want to know when records aren't matching up and why. to keep all records, add 
## an argument all = TRUE.

## you can also use this argument to only keep "extra" rows on one or the other side of the merge 
## with "all.x = TRUE" or "all.y = TRUE"

#============================#
# ==== 6. do it yourself ====
#============================#

## use the space below to create a dataset that brings together students' test data for students who have a 
## "test_scale_score" greater than or equal to 693 with their demographic information
## use the datasets "raw_test_data" and "raw_student_dems" 
## only include the students who's "race_descr" is "Black"
## HINT: is there a single entry per student in each of the datasets? how many keys will you need to set?

# to check your answer, run the following code
# if you are correct, this line will evaluate to "TRUE"
all.equal(myanswer,answer_3merge, check.attributes=FALSE)

