###############################################################################################################################################################################.
# notes:
# - purpose: demonstrate R functionality 
# - inputs: none
# - outputs: none
# - notes: 
# - keywords: 
# - general: this script covers installing/loading packages and general syntax 
###############################################################################################################################################################################.


#=======================================#
# ==== 1. install and load packages ====
#=======================================#

## to run code from a script, place your cursor in the line you want to run, or highlight
## multiple lines, and then press ctrl + enter on your keyboard. You can also use the 
## run button in the top right corner of the script window.

## Many functions are included in base R, but other functions are stored within packages that must 
## be manually installed (once) and loaded (in each session).

## In these modules we will make use of the below packages. Before beginning they must be 
## installed. This action is only needed once. 

## Installing packages is disabled for individual users on Rstudio server. If you are running these scripts on Rstudio server, skip these lines.  

# install the data.table package 
install.packages("data.table")

# nate makes an edit

# install plyr 
install.packages("plyr")

# install reshape 2 
install.packages("reshape2")

# install ggplot2 
install.packages("ggplot2")

# install the devtools and DelawareDataPackage
install.packages("devtools")
library(devtools)
install_github("edanalytics/DelawareDataPackage")

# DelawareDataPackage is a package created by EA, it contains all of the data you will need to 
# complete the training modules. because the package was created by EA, installing it is slightly different
# DelawareDataPackage is installed from github using the install_github function (as used above)

## to load and attach the packages within an R session, use the "library" command 
## the library command will error if a package has not been previously installed on the computer or server 

# load the data.table package 
library(data.table) 

## data.table is an advanced data manipulation package built for large data sets. 
## this will allow us to work with bigger data without actually shuffling data around 
## in resident memory and instead using pointer logic from C++ 

#=======================================#
# ==== 2. R objects and R functions ====
#=======================================#

## R evaluates lines of code and either displays the evaluation in the console or stores the 
## evaluation as an "object"

# evaluate math expression and display result to console  
1 + 1 

## The "arrow" <- sign indicates assignment from the tail to the point and typically points left 
## (nonstandard use may point right as well).

# save math expression into variable "answer"
answer <- 1 + 1 

# display content of the object "answer" in console. this "silently" calls the print function, so is equivalent to print(answer)
answer

# display the class of the object "answer" 
class(answer) 

## Note that the assignment operator "<-" created a numeric object and stored the value 2 and named 
## the object "answer" all in one step

## objects can be numeric values as this one is or a variety of other types. some common examples 
## in these modules are: character, vector, list, factor, matrix, data.frame, or data.table 

# save another math expression 
answer_2 <- 2 * 2

# save output of (1+1) + (2*2)
final_answer <- answer + answer_2 

# display final answer in console 
final_answer 

## + is a nonstandard R function. most function calls use parentheses syntax.

# evaluate the function "sum" with arguments (1,2,3)
sum(1, 2, 3)

## almost all functions have default values for options
## sum defaults to na.rm=FALSE which means that it returns NA if any value is NA
## check function documentation for syntax help by typing ?function into the console

# show documentation of the sum function
?sum

# evaluate the function "sum" with arguments (1, 2, 3, NA)
sum(1, 2, 3, NA)

## one can override these behaviors,in this case we are telling R to ignore missing values in the sum

# evaluate the function "sum" with arguments (1,2,3,NA), specify to exclude "NA" values 
sum(1, 2, 3, NA, na.rm = TRUE)

#=============================#
# ==== 3. R logical tests ====
#=============================#

## in R, a double equal sign is a logical test 

# test if 1 is equal to 1 and display output in console 
1 == 1

## this allows the execution of commands based on a condition 

# set a test object equal to 1 
test_value <- 1 

# conditionally execute a print command based on a logical test 
if (test_value == 1) {
  print("the test value was equal to 1 so the logical test evaluated to true and printed 
        this message to the console") 
} 

## we can also store the result of a test in an object of class "logical"

# store test result
test_object <- 1 == 1

# display test result 
test_object

# display class of test_object
class(test_object)

# conditionally execute a print command based on a logical test 
if (test_object) {
  print("the test object is true so this message printed to the console") 
} 

#===========================================#
# ==== 4. reference list of R operators ====
#===========================================#

## the following list is a reference list of logical and arithmetic operators, not commands to be 
## run 

## LOGICAL OPERATORS
# &	and 
# |	or 
# <	less than 
# >	greater than 
# <=	less than or equal to 
# >=	greater than or equal to 
# ==	equal to 
# !=	not equal to
# 
## ARITHMETIC OPERATORS
# +	addition
# -	subtraction
# *	multiplication
# /	division
# ^	exponent

#========================================#
# ==== 5. common objects & operators ====
#========================================#

## 5.1 c()
## c() is a generic function that combines objects and typically returns a vector

# combine numeric values 1, the sequence 2 through 4, and 5
num_vector <- c(1, 2:4, 5)

# display in console 
num_vector

# display class in console 
class(num_vector)

## data type will be determined by the following hierarchy:
## NULL < raw < logical < integer < real < complex < character < list < expression
## such that c() will convert every element to the highest level in that hierarchy

# combine numeric values 1, the sequence 2 through 4, and the character "five"
char_vector <- c(1, 2:4, "five")
char_vector
class(char_vector)

## c() can combine just about anything and more advanced use can result in unintuitive behavior

# create a list of vectors
vec_list <- list(char_vector, num_vector)

# combine a list with 2 integers and a character
list_vector <- c(vec_list, 6, 7, "eight")
list_vector

## c() converts this to a vector of lists since list is the highest data type  

# display class of "list_vector"
class(list_vector)

# 5.2
# combining and referencing vectors or rows

## Once you have a set of vectors you can "bind" them together with cbind() (column bind) and 
## rbind() (row bind) into a matrix (or other data type in special cases). Before this operation 
## vectors are neither rows nor columns. Note that rbind() and cbind() use the same data type 
## precedence as c().

# combine char_vector and num_vector as stacked rows and display them in the console 
# (note the data type)
rbind(num_vector, char_vector)

# combine char_vector and num_vector as side by side columns and save to char_matrix 
char_matrix <- cbind(num_vector, char_vector)

## matrix notation operates on rows first and then columns

# display "char_matrix" in the console
char_matrix

# display the first row of the matrix in the console  
char_matrix[ 1, ]

# display the first column of the matrix in the console 
char_matrix[ , 1 ] 

# display the 3rd through 5th rows of the matrix and the 2nd column of the matrix 
char_matrix[ 3:5, 2 ]

# 5.3 data frame

## data.frames are like matrices but can store different variable types/classes

# convert char_matrix to a data frame and store in "data_frame"
data_frame <- data.frame(char_matrix)

# display the class of the data_frame
class(data_frame)

# use matrix notation to display the first column of data_frame
data_frame[ , 1 ]

# display the variables in the data frame (column names)
ls(data_frame)

# use data_frame notation to display the first column of data_frame
data_frame$num_vector

## note that the output of this looks odd - data frames do not require each column to be the same 
## class and the data.frame command converts "characters" to "factors"

# display the class of the char_vector variable in the data_frame
class(data_frame$char_vector)

# view the structure of the data frame
str(data_frame)

## factors can be useful but for now they cause more complications than they help so we convert to the
## original classes of the vectors

# convert data_frame$char_vector to character and write over itself
data_frame$char_vector <- as.character(data_frame$char_vector)

# convert data_frame$num_vector to numeric and write over itself
data_frame$num_vector <- as.numeric(data_frame$num_vector)

# check the class of the numeric values
class(data_frame$num_vector)

# check the class structure of the entire data frame
str(data_frame)

## later in these modules we will come back to data frames and their more powerful sibling the 
## data table 

## 5.4 accessing lists of data frames

## A very common technique in R is to have nested lists. Data frames are actually structured lists
## and as such a list of data frames is a nested list.

# create a list of data frames -- in this case each entry contains the same copy of data_frame
df_list <- list(df_1 = data_frame, df_2 = data_frame)
class(df_list)

# display the contents of the list in the console
ls(df_list)

## note that ls() only shows the top level, if we want to see more we have to reference the next 
## level down

# display the contents of df_1 
ls(df_list$df_1)
class(df_list$df_1)

## again ls() shows the top level (in this case it is the last level) of its argument. note the 
## class of this argument is a data.frame

# display contents of df_list$df_1$num_vector. Note: ls() will not show data and returns an error 
# since this is the data layer
ls(df_list$df_1$num_vector)

## display contents of df_list$df_1$num_vector, since we are back to the data layer it shows the 
## class of the data as numeric
class(df_list$df_1$num_vector)

## if we want to see the structure of complex structures,str() can be much more informative than ls()

# display the structure of df_list
str(df_list)

