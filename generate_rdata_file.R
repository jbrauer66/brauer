rm(list=ls())
####################################################
# Create answer keys for "Do it yourself" sections #
####################################################

#########################################
# Create answer data set for 2_subset.r #
#########################################

# load the data.table package
library(data.table)

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load the raw test data csv and make it a data table
raw_test_data 	<- fread(paste0(sample_data_dir,"/deid_test_multi_row.csv"),colClasses = "character")

# view the data
## this is fake student-level data
# View(raw_test_data)

# subset to only 2011 and 2014 data and just 4 columns
answer_2subset <- subset(raw_test_data, school_year %in% c(2011,2014),select = c(school_year,fake_student_id,test_grade,test_scale_score))


########################################
# Create answer data set for 3_merge.r #
########################################

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load raw data, save as data.tables
raw_test_data 				  <- fread(paste0(sample_data_dir,"deid_test_multi_row.csv"),colClasses="character")
raw_student_dems		  	<- fread(paste0(sample_data_dir,"deid_dems_single_row_per_year.csv"),
                              colClasses="character")

## create a dataset that brings together students' test data for students who have a
## test_scale_score greater than or equal to 693 with thier demographic information
## only include the students who's race_descr is "Black"

test_subset <- subset(raw_test_data, subset = as.numeric(test_scale_score) >= 693,)
dems_subset <- subset(raw_student_dems, subset = race_descr == "Black")

setkey(test_subset, school_year, fake_student_id)
setkey(dems_subset, school_year, fake_student_id)

answer_3merge <- merge(test_subset, dems_subset, by = c("fake_student_id", "school_year"))


#########################################
# Create answer data set for 4_recode.r #
#########################################

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load raw student demographic data
raw_student_dems	<- fread(paste0(sample_data_dir,"deid_dems_single_row_per_student_with_grade.csv"),
                          stringsAsFactors=FALSE)


## create a new column in raw_student_dems for a bianary coding of gender, "0" for male and "1" for female
## call the column "bi_gender". then, make another new column called "lower_upper" with entries of either
## "lower" for students in grades 3-6 or "upper" for students in grades 7-11

#create a copy of the data to work with
answer_4recode <- copy(raw_student_dems)

# create bi_gender
answer_4recode[gender=="F", bi_gender := "1"]
answer_4recode[gender=="M", bi_gender := "0"]

#create lower_upper
answer_4recode[grade %in% c(3:6), lower_upper := "lower"]
answer_4recode[grade %in% c(7:11), lower_upper := "upper"]


#############################################
# Create answer data set for 5_operations.r #
#############################################

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load the raw test data csv and make it a data table
raw_test_data 	<- fread(paste0(sample_data_dir,"/deid_test_multi_row.csv"),colClasses="character")

## use the space bellow to subset the raw_test_data to test_period 1. then, create a new column, "arbitrary_math"
## by dividing each "test_raw_score" by the sum of the column "test_performance_level" and then
## subtract "test_scale_score".
answer_5operations <- subset(raw_test_data, test_period == "1",)

answer_5operations[,test_raw_score:=as.numeric(test_raw_score)]
answer_5operations[,test_performance_level:=as.numeric(test_performance_level)]
answer_5operations[,test_scale_score:=as.numeric(test_scale_score)]

answer_5operations[,arbitrary_math := (test_raw_score/sum(test_performance_level)) - test_scale_score]

#############################################
# Create answer data set for 6_by_group.r #
#############################################

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load the raw test data csv and make it a data table
raw_test_data 	<- fread(paste0(sample_data_dir,"/deid_test_multi_row.csv"),colClasses="character")

## create a table of with 2 aggregate statistics: 
## the mean "test_performance_level" by "school_year" and name the column of means "avg_perform", 
## the frequency table of the number of entries by "school_year" and name the column "count" 
## call the dataset "myanswer"

answer_6bygroup <- raw_test_data[ , list(avg_perform=mean(as.numeric(test_performance_level)), count=.N), by="school_year"]

#############################################
# Create answer data set for 7_transpose.r #
#############################################

# load data table package
library(data.table)

# load plyr package
library(plyr)

# load reshape package
library(reshape2)

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load the raw test data csv and make it a data table
raw_test_data 	<- fread(paste0(sample_data_dir,"/deid_test_multi_row.csv"),stringsAsFactors=FALSE)

## use the space bellow and the "raw_test_data" set to do the following:
## first, make the "raw_test_data" "go wide," the values should be "test_performance_level"
## and the variable should be "school_year" and "test_grade."

first_step <- data.table(dcast(data = raw_test_data,
                               formula = fake_student_id+test_name+opportunity_number+test_period+test_content_area
                               ~ school_year+test_grade,
                               value.var = "test_performance_level"))

## then, take the wide set you've just created, and make it "go long." The variable name
## should be "year_test_grade" and the value name should be "test_performance_level".
## call this new, long dataset "myanswer".

answer_7transpose <- melt(first_step,
                          id=c("fake_student_id","test_name","opportunity_number","test_period","test_content_area"),
                          na.rm=TRUE,
                          variable.name = "year_test_grade",
                          value.name = "test_performance_level")

#######################################
# Create answer data set for 9_loop.r #
#######################################

# set directory of sample data
sample_data_dir <- "N:/general/archived_code/training/sample_data/"

# load the raw test data csv and make it a data table
raw_test_data 	<- fread(paste0(sample_data_dir,"/deid_test_multi_row.csv"),stringsAsFactors=FALSE)

## use the space bellow and the "raw_test_data" set to do the following:
## create a subset of the raw_test_data of only entries from schools 103 and 70 and call it "subset"

subset = subset(raw_test_data,fake_school_id == "103" | fake_school_id == "70")

## for each entry in subset, print the "test_start_date" and, if the "test_content_area" is "ELA",
## add the "test_scale_score" to a variable named "total_test_scale_score_ELA"

total_test_scale_score_ELA = 0

for(i in 1:nrow(subset)){
  print(subset$test_start_date[i])
  if(subset$test_content_area[i]=="ELA"){
    total_test_scale_score_ELA <- total_test_scale_score_ELA + subset$test_scale_score[i]
  }
}

answer_9loop = total_test_scale_score_ELA

###################################
# Create answers for 10_ggplot2.r #
###################################
library(ggplot2)
# set file location of example data (forward slashes only)
in_file_location <- "N:/delaware_training/module_powerpoints/sample_test_data_ggplot2.csv"


####################### load example data and store it as a data.table
in_data <- data.table(read.csv(in_file_location,colClasses="character"))

# convert numeric values to numbers for plots
in_data[ , math_scale_score_2013:=as.numeric(math_scale_score_2013)]
in_data[ , read_scale_score_2013:=as.numeric(read_scale_score_2013)]


# 1a. create a histogram of reading scale scores using "in_data"
hist_data <- ggplot(data = in_data, aes(x = read_scale_score_2013))
hist_data+geom_histogram()

# 1b. facet the histogram, grouped by economic disadvantage status
hist_data+geom_histogram()+ facet_wrap(~ d_econdisadv_y)

# 2a. create a boxplot of the reading test score distribution  using geom_boxplot()
box_data <- ggplot(data = in_data, aes(x=as.factor(0), y = read_scale_score_2013))
box_data + geom_boxplot(stat = "boxplot",position = "dodge")

# 2b. facet the previous boxplot by tested_grade
box_data <- ggplot(data = in_data, aes(x=as.factor(tested_grade), y = read_scale_score_2013))
box_data + geom_boxplot(stat = "boxplot",position = "dodge")

# 3a. create a density plot of reading scores using geom_density()
dens_data <- ggplot(data = in_data, aes(x=read_scale_score_2013))
dens_data + geom_density()

# 3b. add math scores density line to the plot, and then adjust legend (first melt data by uncommenting the lines)
melted_data <- reshape2::melt(in_data, "student_id",
                              c("math_scale_score_2013", "read_scale_score_2013"), variable.name="subject", value.name = "scale_score")
dens_data2 <-ggplot(data = melted_data, aes(x=scale_score, color = subject))
dens_data2 + geom_density(alpha=0.25)


###################################################
# output the file to be used in the training code #
###################################################
save(file = "P:/GitHub/DelawareDataPackage/data_raw/answers_doit_yourself.Rdata", answer_2subset,answer_3merge,
     answer_4recode, answer_5operations, answer_6bygroup,answer_7transpose,answer_9loop)
