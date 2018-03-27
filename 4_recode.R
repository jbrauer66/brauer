###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate re-coding and creating variables using data.table syntax 
# - inputs: data from DelawareDataPackage
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script walks through re-coding variables and creating new variables by if then logic
###############################################################################################################################################################################.

# this command deletes all objects to give the program a clean slate when running from the top 
rm(list=ls())

#====================================#
# ==== 1. load data and packages ====
#====================================#

# load data table package 
library(data.table) 

# load the data package
library(DelawareDataPackage)

# load the demographics data from the DelawareDataPackage 
raw_student_dems	<- data.table(dems_single_row_student_wgrade)

#=====================================#
# ==== 2. basic data.table syntax ====
#=====================================#

## data.table commands have the structure: 
## before the comma, we execute a logical test on the rows
## after the comma, we tell R to execute some function of the columns 

## there are not many limits to the types of functions we can execute using this formula. many of the commands we have investigated so far 
## can be implemented more efficiently in data.table. for example, here we find the maximum grade in the dataset using conventional R syntax. 

# find the max grade in the vector "grade" in the "raw_student_dems" dataset  
max_grade_df <- max(raw_student_dems$grade)

# find the max grade in the dataset using data.table syntax. skip the rows argument because we want to evaluate the max grade across all rows.  
max_grade_dt <- raw_student_dems[ , max(grade) ]

# test whether the two functions returned the same result
identical(max_grade_df, max_grade_dt)

#======================================================#
# ==== 3. recode character value within a variable ====
#======================================================#

## in the data cleaning and manipulation process we will regularly want to recode values within a column, or create new columns
## first we want to recode the "gender" variable to say "male" and "female" instead of "M" and "F" 

# preserve raw data
student_dems <- copy(raw_student_dems)

## within data.table syntax we can define columns with a special function called :=. instead of returning a value like we did in the previous example,
## it assigns the specified value to a column (new or overwritten) defined before the := 

## before the comma, we execute a logical test on the rows for if gender is equal to M
## after the comma, we tell R to assign the string "male" to the gender column where the condition on the rows is true.  

# recode M to male 
student_dems[ gender == "M", gender := "male"]

# recode F to female 
student_dems[ gender == "F", gender := "female"]

# display table of raw data gender column 
table(raw_student_dems$gender)

# display table of recoded gender column 
table(student_dems$gender)

## we can also skip the rows filter to create new variables or overwrite old ones. here, we just apply the as.character function to overwrite a column. 

# convert fake_student_id to a character 
student_dems[ , fake_student_id := as.character(fake_student_id) ]

## going forward we will use this method of variable type conversion instead of the dollar sign syntax used earlier.

#=================================================================#
# ==== 4. apply logic to create a new variable in the dataset ====
#=================================================================#

## this syntax also allows for the creation of new variables conditionally based on existing 
## variables. 

## Here we assign school levels to students based on their grades.

# apply logic to create a new variable called "school_level" based on existing variable "grade"  
student_dems[ grade <= 5, school_level := "Elementary"]
student_dems[ grade >= 6 & grade <= 8, school_level := "Middle"]
student_dems[ grade >= 9, school_level := "High"]

# preview the data set (note the newly created variable "school_level")
student_dems

#======================================================================================#
# ==== 5. apply logic to create new variable with existing data from another field ====
#======================================================================================#

## instead of manually assigning a character or number to a new variable, sometimes we want to 
## assign the contents of a different variable

## in this case we only want to make a second "grade" variable that copies the "grade" column for 
## students in "Middle" and "Elementary", and place the string "HS" for students in "High"
##	- note: the pipe symbol "|" is the operator for "or" in R

# create new variable "alt_grade" that is equal to "grade" for Elementary and Middle 
student_dems[ school_level == "Elementary" | school_level == "Middle", alt_grade := grade]

# assign a placeholder 99 for high school grades 
student_dems[school_level == "High", alt_grade := 99]

## note that we had to put a numeric value in "alt_grade" here because the variable is stored as 
## numeric (specifically as an integer)

# display crosstab table of "alt_grade" and "school_level" 
table(student_dems$alt_grade, student_dems$school_level)

## instead of the "or" operator, the %chin% or %in% commands also allow access to multiple rows.
## the following commands another grade variable with similar logic. 

# create new variable "alt_grade_v2" that equals "HS" or "Non-HS"
student_dems[ school_level %chin% c("Elementary","Middle"), alt_grade_v2 := "Non-HS"]
student_dems[ school_level == "High", alt_grade_v2 := "HS"]

#============================#
# ==== 6. do it yourself ====
#============================#

## use the space below to create a new column in "myanswer" for a bianary coding of gender, 
## "0" for male and "1" for female (make the entries of the column character values). call the column "bi_gender". 
## then, make another new column called "lower_upper" with entries of either "lower" for students
## in grades 3-6 or "upper" for students in grades 7-11

# create the myanswer dataset
myanswer <- copy(raw_student_dems)

# to check your answer, run the following code
# if you are correct, this line will evaluate to "TRUE"
all.equal(myanswer,answer_4recode, check.attributes=FALSE)
