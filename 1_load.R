###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate loading data into R and initial data structure investigation
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script provides an overview of loading data into r and investigating its properties
###############################################################################################################################################################################.

# this command deletes all objects to give the program a clean slate when running from the top 
rm(list=ls())

#===========================#
# ==== 1. load packages ====
#===========================#

# load data.table package 
library(data.table)

# load the sample data package
library(DelawareDataPackage)

#=======================#
# ==== 2. load data ====
#=======================#

## to work with any existing data it must be loaded into r. read.table can read any kind of data 
## saved as a text data file 

## in these modules we will be using pre-loaded data from a package, so we will just cover the 
## commands used to load data and then begin investigating its structure. 

## commands in R generally either display something in the console or save the output of the 
## command as an object 

## if we were loading this data from a file on on our computer we would store it as an object named 
## "raw_test_data_frame" for use later. 
## there are many classes of objects in R; by default read.table saves the object as a 
## "data.frame".

## here's the syntax we would use to load an external data file delimited by commas-- 
## it's commented out because running it won't do anything unless you replace file_path with a file path

# load a sample raw test data set and save it as a data frame called "raw_test_data_frame"
# raw_test_data_frame 		<- read.table(file_path,
#                                     sep = ",",
#                                     header = TRUE,
#                                     stringsAsFactors = FALSE)

## in many cases we will want to work with data as a special type of data.frame called a data.table. 
## fread has the same functionality as read.table but is faster and automatically saves the dataset 
## as a data.table.  

# load a sample raw test data set and save it as a data table, force columns to import as "character"
# raw_test_data_table 		<- fread(file_path, colClasses = "character")

## the colClasses = "character" argument forces all of the data that we load to be imported as a character and not numeric. this is a safe first step.
## for the rest of this module we'll just create these two objects from data pre-loaded in the package

# store test_multi_row as a data.frame
raw_test_data_frame <- data.frame(test_multi_row)

# store test multi row as a data.table
raw_test_data_table <- data.table(test_multi_row)

#================================#
# ==== 3. check object class ====
#================================#

## objects can have many classes. R stores our initial data set as an object called a data frame. a data.frame is a list of vectors. 

# display all objects stored in current r session 
ls()

# display class of object "raw_test_data_frame"
class(raw_test_data_frame)

# display class of object "raw_test_data_table"
class(raw_test_data_table)

# display the dimensions of "raw_test_data_frame" 
dim(raw_test_data_frame)

# display the dimensions of "raw_test_data_table" 
dim(raw_test_data_table)

## note that the data.table is **also** a data.frame, just a special kind of one

# display the first 6 rows of the data frame "raw_test_data_frame" with head() command
head(raw_test_data_frame) 

# display a preview of the data table "raw_test_data_table"
raw_test_data_table

#==================================#
# ==== 4. check variable class ====
#==================================#

## the variables or columns stored within data frames or data tables also have classes of their own. they are vectors of a type that have been joined
## together in a list to become a data.frame

# display class of variables within "raw_test_data_frame"
str(raw_test_data_frame) 

# display class of variables within "raw_test_data_table"
str(raw_test_data_table) 

## the fundamental way to access something within an object is the bracket operator "["

# display the variable school_year within the data.frame "raw_test_data_frame"
raw_test_data_frame[["school_year"]]

## we will frequently use the dollar sign shortcut to access "down" a level into an object

# display the variable "school_year" within the data.frame "raw_test_data_frame"
raw_test_data_frame$school_year

## we can also use the class command on an individual variable

# display class of the variable "school_year"
class(raw_test_data_frame$school_year)

#=================================#
# ==== 5. convert object type ====
#=================================#

## in most cases we will want to work with data.frames by converting them to data.tables 

# create a data table out of raw_test_data 
dt_test_data <- data.table(raw_test_data_frame) 

# display class of dt_test_data
class(dt_test_data)

# display preview of dt_test_data
dt_test_data

## data.frame syntax can till work on data tables, but data tables have additional functionality

#===================================#
# ==== 6. convert variable type ====
#===================================#

## When we looked at the command to load raw_test_data_table with fread above, we specified an option 
## to force all variables to import as "character". If we want to use math operators on a variable, 
## it must be numeric. 

# display class of test_raw_score
class(raw_test_data_table$test_raw_score)

# convert test_raw_score to numeric 
raw_test_data_table$test_raw_score <- as.numeric(raw_test_data_table$test_raw_score)

# display class of test_raw_score
class(raw_test_data_table$test_raw_score)

#============================#
# ==== 7. do it yourself ====
#============================#

## in upcoming modules, there will be "do it yourself" excercises to test your skills.
## for now, make sure that you can answer the following questions. to check your answers, 
## refer to the powerpoint presentation. 

## why would you want to use fread() instead of read.table()?

## what does the "colClasses=" argument do?

## what functions could you use to check that your data has imported correctly?

## why should unique student, teacher, or school ID numbers be imported as characters, not numbers?

## what command can you use to find out if an imported data set is a data.table or data.frame?

