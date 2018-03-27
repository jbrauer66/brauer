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
p_rhs_grades <- c("d_grade_04", "d_grade_05", "d_grade_06", "d_grade_07", "d_grade_08", "z_adrops_pre1", "z_adrops_pre2")

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

# example: "exp_2_teacher_2016_math.csv"

# model
x_model <- "exp_2_teacher_2016_math.csv"

# load data
x_data <- fread(paste0(p_dir_in, x_model))

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





# run regression separated by grade

#subset data
x4_data <- subset(x_data,d_grade_04==1)

# run overall regression
overall_output4 <- glm_wrapper(in_data = x4_data, 
                              formula = overall_formula, 
                              in_var_lhs = c(p_rhs_dems, p_rhs_grades), 
                              out_val_model_descr = x_model, 
                              opt_re = FALSE)

# save output
overall_coeffs4 <- copy(overall_output4$model_coeffs)
overall_obs_level4 <- copy(overall_output4$model_obs_level)
overall_varcov4 <- copy(overall_output4$model_varcov)
overall_object4 <- copy(overall_output4$model_object)




#subset data
x5_data <- subset(x_data,d_grade_05==1)

# run overall regression
overall_output5 <- glm_wrapper(in_data = x5_data, 
                               formula = overall_formula, 
                               in_var_lhs = c(p_rhs_dems, p_rhs_grades), 
                               out_val_model_descr = x_model, 
                               opt_re = FALSE)

# save output
overall_coeffs5 <- copy(overall_output5$model_coeffs)
overall_obs_level5 <- copy(overall_output5$model_obs_level)
overall_varcov5 <- copy(overall_output5$model_varcov)
overall_object5 <- copy(overall_output5$model_object)









ztest <- glm(formula = overall_formula, family = binomial(link = "logit"), data = in_data)


# run easier regression
z_rhs_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y")
ztest_formula <- as.formula(paste0("relay_flag ~ z_adrops_pre1 + z_adrops_pre2 + ", paste(p_rhs_grades, collapse = " + "), " + ", paste(z_rhs_dems, collapse = " + ")))

ztest <- glm(formula = ztest_formula, family = binomial(link = "logit"), data = in_data)


# works without races...





# save fitted values


in_x_set <- ea_load("/projects/relay/growth_2017/data/45_input_sets/most_recent/nys_math_grade_04_2016_spring_both_pre.rdata")

x_student <- subset(in_x_set$out_data_controls, ea_student_id == "102000031")



.0005


















