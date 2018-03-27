#=====================================================#
# notes:
# - purpose: format the VA-input sets for propensity score calculations
# - inputs: 
# - outputs:
# - keywords: #brule #drops #check
# - general:
#=====================================================#

#========================#
# ==== load packages ====
#========================#

library(easimple)
ea_start()
library(data.table)

# source glm wrapper
source("~/code/relay/psm_2017/functions/glm_wrapper.R")

#=====================#
# ==== set params ====
#=====================#

# export toggl
p_opt_exp <- FALSE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/psm_2017/data/20_set_levels_for_ps_calculation/most_recent/"

# output directory
# p_dir_out <- "/projects/relay/psm_2017/data/30_calculate_propensity_scores/"

#===============================#
# ==== set model parameters ====
#===============================#

# choose RHS demographics
p_rhs_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y",
                  "d_race_asian", "d_race_black", "d_race_hispanic",
                  "d_race_native", "d_race_multi", "d_race_white")

# set RHS grade variables
#p_rhs_grades <- c("d_grade_05", "d_grade_06", "d_grade_07", "d_grade_08", "z_adrops_math_pre", "z_adrops_ela_pre")

# + student_level: a cubic polynomial of the prior-year math and reading test scores
# + student_level: indicators for each grade level
# + student_level: gender, age (-), ELL, IEP, frl, race
# - student_level: prior-year absence rate 
# + classroom:     the average same-subject prior-year test score of the student’s classroom
# + classroom:     the standard deviation of same-subject prior-year test scores in the student’s classroom
# + classroom:     class size
# + classroom:     class %frl 


#==================================================#
# ==== function to calculate propensity scores ====
#==================================================#

# create an empty data frame to append propenisty scores into

all_obs <- data.frame()

# pool regression by grade for all models

file_list <- list.files(p_dir_in)

for (name in file_list) {
  
  x_model <- name
  
  #model
  x_data <- ea_load(paste0(p_dir_in, x_model))
  
  # temp: change relay flag to numeric
  x_data[, relay_flag := as.numeric(relay_flag)]
  
  # choose only grades that are in the dataset
  name_list <- names(x_data)
  grade_list <- name_list[(1:length(name_list))[startsWith(name_list,"d_grade")]]
  p_rhs_grades <- c(grade_list,"z_adrops_math_pre", "z_adrops_ela_pre")
  
  
  #p_rhs_grades <- c("d_grade_05", "d_grade_06", "d_grade_07", "d_grade_08", "z_adrops_math_pre", "z_adrops_ela_pre")
  
  # set formula
  overall_formula <- as.formula(paste0("relay_flag ~ ", paste0(paste(p_rhs_dems, collapse = " + "), " + ", paste(p_rhs_grades, collapse = " + "))))
  
  #x_data[d_race_other == 1, d_race_white := 1]
  #x_data[, d_race_other := NULL]
  
  # run overall regression
  overall_output <- glm_wrapper(in_data = x_data, 
                                formula = overall_formula, 
                                in_var_lhs = c(p_rhs_dems, p_rhs_grades), 
                                out_val_model_descr = x_model, 
                                opt_re = FALSE)
  
  
  # save fitted values
  #overall_coeffs <- copy(overall_output$model_coeffs)
  overall_obs_level <- copy(overall_output$model_obs_level)
  #overall_varcov <- copy(overall_output$model_varcov)
  #overall_object <- copy(overall_output$model_object)
  #pred <- copy(overall_output$model_obs_level$prediction)
  
  # append all of the propensity scores
  all_obs <- rbind(all_obs,overall_obs_level,fill=TRUE)
  
}


# Use Propensity Score Mathcing on each model split further by grade level

mds <- data.frame()


for (name in file_list) {
  
  # get inside loop
  #name <- file_list[[2]]
  
  x_model <- name
  x_data <- ea_load(paste0(p_dir_in, x_model))
  x_data[, relay_flag := as.numeric(relay_flag)]
  
  
  name_list <- names(x_data)
  grade_list <- name_list[(1:length(name_list))[startsWith(name_list,"d_grade")]]
  
  # grab grade columns
  grade_list <- grep("d_grade", colnames(x_data), value = TRUE)
  
  
  for (x_grade in grade_list) {
    
    # get inside loop
    #x_grade <- grade_list[[3]]
    
    #grade_c=paste("all_obs$",grade,sep="")
    #subset the data to only model_descr=x_model and grade==1
    cur_data <- all_obs[model_descr == x_model & get(x_grade) == 1]
    
        
    #assign treatment and propensity scores
    X <- cur_data$prediction
    Tr <- cur_data$relay_flag
    #Perform matching procedure, no impact analysis
    rr_null <- Matching::Match(Y=NULL,Tr=Tr,X=X,caliper=.25)
    md <- rr_null$mdata$X
    mds <- rbind(mds,md,fill=TRUE)
    
    
  } 
    
  }








# example: "exp_2_teacher_2016_math.csv"

# model
x_model <- "exp_2_teacher_2016_math.rdata"

# load data


x_data <- ea_load(paste0(p_dir_in, x_model))
#x_data <- fread(paste0(p_dir_in, x_model))
name_list <- names(x_data)
grade_list <- name_list[(1:length(name_list))[startsWith(name_list,"d_grade")]]

p_rhs_grades <- c(grade_list,"z_adrops_math_pre", "z_adrops_ela_pre")

# temp: change relay flag to numeric
x_data[, relay_flag := as.numeric(relay_flag)]

# set formula
overall_formula <- as.formula(paste0("relay_flag ~ ", paste0(paste(p_rhs_dems, collapse = " + "), " + ", paste(p_rhs_grades, collapse = " + "))))

x_data[d_race_other == 1, d_race_white := 1]
x_data[, d_race_other := NULL]

# run overall regression
overall_output <- glm_wrapper(in_data = x_data, 
                              formula = overall_formula, 
                              in_var_lhs = c(p_rhs_dems, p_rhs_grades), 
                              out_val_model_descr = x_model, 
                              opt_re = FALSE)


# save output
overall_coeffs <- copy(overall_output$model_coeffs)
overall_obs_level <- copy(overall_output$model_obs_level)
overall_varcov <- copy(overall_output$model_varcov)
overall_object <- copy(overall_output$model_object)


pred <- copy(overall_output$model_obs_level$prediction)


# take only fitted values and plug into Match function

X <- overall_obs_level$prediction
Y <- x_data$z_adrops_post_test_scale_score
Tr <- x_data$relay_flag

rr <- Matching::Match(Y=Y,Tr=Tr,X=X,caliper=.25)

rr_null <- Matching::Match(Y=NULL,Tr=Tr,X=X,caliper=.25)

mb <- Matching::MatchBalance(overall_formula, data=x_data,match.out=rr,nboots=10)


