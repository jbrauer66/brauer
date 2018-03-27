####################################################################################.
# purpose: this scipt gives an introduction ea functions 
#
# notes: - there are two ea packages: easimple and eaanalysis 
#
#        - this only covers the less complex functions
#       
#        - by typing ?"name of a function" into the console you can view the functions documentation
#
#        - you can view full ea function documentation on github 
#
#        - many of these functions are similar to base functions with slight changes
#
####################################################################################.

#========================#
# ==== load packages ====
#========================#

# load ea simple
library(easimple)


# clear objects and console
ea_start()

# load ea analysis
library(eaanalysis)

# load other packages
library(data.table)
library(DelawareDataPackage)

#=============================#
# ==== load practice data ====
#=============================#

# load the raw test data from the DelawareDataPackage 
raw_test_data 	<- data.table(test_multi_row) 

# ea_load() loads an Rdata object - the argument is the file path of the data to be loaded

# subset the raw data into two data sets - 2011/2012 and 2013/2014
data_table1 <- subset(raw_test_data, school_year == "2011" | school_year == "2012")
data_table2 <- subset(raw_test_data, school_year == "2013" | school_year == "2014")

#==============================#
# ==== EA Simple functions ====
#==============================#

# merging 
# ea_merge prints the merge rate to the console and allows the option for storing mismatched data 
# when opt_out_mismatches = 1, ea_merge returns a list of data tables- one of the merged data, and one of the mismatched data
# when opt_out_mismatches = 0, ea_merge returns a single data table of merged data 
# for this example we're merging by student id and test content and test name and test grade
?ea_merge
# merged_data <- ea_merge(data_table1, data_table2, in_vars_by = c("var1", "var2"), opt_out_mismatches = 1)
merged_data <- ea_merge(data_table1, data_table2, c("fake_student_id", "test_content_area", "test_name"))


# duplicates
# ea_out_dups allows you to output a data table of duplicates from a given data set by specific variables 
# for example: this allows you to view if one student has multiple rows per year by making "var1" the student id variable and "var2" the year variable
# the results is a data table of duplicates stored in a Rdata object
?ea_out_dups
# dups <-ea_out_dups(data_table1, in_vars_key = c("var1", "var2"))
dups <- ea_out_dups(data_table1, c("fake_student_id", "school_year"))


# removing duplicates
# instead of just viewing or storing duplicates you can also remove them from your data 
# this is useful for preparing data for a merge- it can prevent many-to-many merges, but be careful which duplicates you remove
# ea_no_dups removes duplicates randomly so you could lose important data 
# there is an option to print out duplicates that are removed
# by default the function outputs how many duplicates were found/dropped - check the console after running the commnad 
?ea_no_dups
# no_dups_data <- ea_no_dups(data_table1, in_vars_key = c("var1", "var2"))
no_dups_data <- ea_no_dups(data_table1, c("fake_student_id", "school_year"))


# renaming a list 
# this is most useful for adding a file name to data read in as a list 
# this function takes two arguments: a list, and a list of names to apply to this list - make these in the same order
# now you can access and reference the list elements by name
?ea_rename_list
list1 <- list(data_table1, data_table2)
names_list <- c("data_table1", "data_table2")
ea_rename_list(list1, names_list)


# extract part of a character string
# if a variable test_name is coded as testname_year_grade and you want to extract just the grade part you can use this
# the arguments are the string to split, the position of the split string you want, how to split the string
?ea_scan
test_name <- "occt_2014_03"
grade <- ea_scan(test_name, 3, "_")
grade


# split a character string
# instead of only extracting a piece of a string, you want to separate a string into separate strings
# returns a vector of character strings 
?ea_split
test_name <- "occt_2014_03"
split_test_name <- ea_split(test_name, "_")
split_test_name


# subset
# almost identical to data.table subset but prints the subset rate 
?ea_subset
sub_data <- ea_subset(data_table1, school_year == "2011", select = c("school_year", "fake_student_id"))


# frequency table 
# returns a data table with counts of each variable input
# operates on a data table 
# option to include percentages 
# option to include a by group
?ea_table
freq_table <- ea_table(data_table1, c("var1", "var2"), opt_percent = 1, opt_var_per_by_group = "var3")


# writing a document 
# create a csv from an Rdata object and export 
# arguments are the Rdata object and a file path 
?ea_write
ea_write(object, "directory/file/file_name.csv")


