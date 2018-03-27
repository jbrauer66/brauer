#=========================================================================#
# notes:
# - purpose: set levels & create datasets for running the "impact model"
# - inputs: matched datasets
# - outputs: impact_model datasets
# - keywords: #brule #drops #check
# - general:
#=========================================================================#

#========================#
# ==== load packages ====
#========================#

library(easimple)
ea_start()
library(data.table)

# library(eagrowth, lib.loc = "/home/general/package_repo/eagrowth_2_mr/")
# library(eainput, lib.loc = "/home/general/package_repo/eainput_2_mr/")  

#=====================#
# ==== set params ====
#=====================#

# export toggl
p_opt_exp <- FALSE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/psm_2017/data/50_create_impact_model_sets/most_recent/"

# output directories
p_dir_out    <- "/projects/relay/psm_2017/data/60_run_impact_model/"
p_dir_out_qc <- "/projects/relay/psm_2017/qc/60_run_impact_model/"

#===========================#
# ==== set model params ====
#===========================#

# set student demographics
p_student_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y",
                      "d_race_asian", "d_race_black", "d_race_hispanic",
                      "d_race_native", "d_race_multi", "d_race_white")

# set pretest variables
p_pretests <- c("z_adrops_math_pre", "z_adrops_ela_pre")

# set classroom characteristics
p_classroom_vars <- c("avg_same_subj_pre", "avg_d_frl_y")

#====================#
# ==== load data ====
#====================#

# load all the input sets
in_impact_model_sets <- ea_load(list.files(p_dir_in, full.names = TRUE))

#===========================#
# ==== run impact model ====
#===========================#

# 8 impact models = (4 experience levels) * (2 school types - cmo -nyc) 

# # run function line by line:
# in_data_single_model <- in_impact_model_sets[[1]]
# f_posttest           <- "z_adrops_post_test_scale_score"
# f_pretests           <- p_pretests
# f_student_xvars      <- p_student_dems
# f_classroom_xvars    <- p_classroom_vars
# f_program_effect     <- "relay_flag"
# f_random_effect      <- "(1 | teacher_id)"

# wrapper that runs a fixed effects model and a mixed model (only adding a random intercept)
run_regs <- function(in_data_single_model = NULL, 
                     
                     # outcome variable 
                     f_posttest = "z_adrops_post_test_scale_score", 
                     
                     # rhs: pretests
                     f_pretests = p_pretests,
                     
                     # rhs: student x-variables
                     f_student_xvars = p_student_dems,
                     
                     # rhs: classroom x-variables
                     f_classroom_xvars = p_classroom_vars,
                     
                     # rhs: program effect
                     f_program_effect = "relay_flag", 
                     
                     # random effect terms (just a random intercept)
                     f_random_effect = "(1 | teacher_id)"){
  
  # copy data table so doesn't edit by ref 
  rr_single_model <- copy(in_data_single_model)
  
  #==============================#
  # ==== ~ normalize weights ====
  #==============================#
  
  # create column in the input dataset that just stores total obs in the set  
  rr_single_model[ , total_obs := .N] 
  
  # add column: sum of weights in the weight column repeated on each row
  rr_single_model[ , sum_weight := sum(weight) ]
  
  # proportion of the weight out of sum weights
  rr_single_model[ , prop_weight := weight / sum_weight ]
  
  # normalize weights by dividing by total observations
  rr_single_model[ , norm_weight := prop_weight  * total_obs ]
  
  #===============================#
  # ==== ~ construct formulas ====
  #===============================#
  
  # grab "model" (d_grade_subj_yr) inputs
  model_controls <- grep("d_grade_subj_yr", colnames(rr_single_model), value = TRUE)
  
  # construct the fixed effects part of the formula 
  fe_formula <- paste0(f_posttest, " ~ ", paste0(f_pretests, collapse = " + "), 
                       " + ", paste0(f_student_xvars, collapse = " + "),
                       " + ", paste0(f_classroom_xvars, collapse = " + "),
                       " + ", paste0(model_controls, collapse = " + "),
                       " + ", f_program_effect)
  
  # add in a random effect term for teacher to run a mixed model 
  me_formula <- paste0(as.character(fe_formula), " + ", f_random_effect)
  
  #=======================#
  # ==== ~ run models ====
  #=======================#
  
  # fixed effects only (standard errors are too low)
  fe_reg_output <- lm(fe_formula, data = rr_single_model, weights = norm_weight)
  
  # store fixed effect coefficients  
  fe_coeffs <- data.table(broom::tidy(fe_reg_output))
  
  # mixed model-- same fixed effects with a random intercept for teacher
  #me_reg_output <- lme4::lmer(me_formula, data = rr_single_model, weights = norm_weight)
  me_reg_output <- lmerTest::lmer(me_formula, data = rr_single_model, weights = norm_weight)
  
  
  # coefficients 
  me_coeffs <- data.tabsle(broom::tidy(me_reg_output))
  
  
  # return data
  return(list(fe_coeffs = fe_coeffs, 
              me_coeffs = me_coeffs))
              #me_reg_output =   me_reg_output))
  
}  

# apply function to all models
output_by_input_data <- lapply(in_impact_model_sets, run_regs)



#========================#
# ==== format output ====
#========================#

# group by fe / me
output_by_model_type <- purrr::transpose(output_by_input_data)

# stacked estimates of the mixed model coefficients 
stacked_mm_coeffs <- rbindlist(output_by_model_type$me_coeffs, idcol = "input_abbrev")
stacked_fe_coeffs <- rbindlist(output_by_model_type$fe_coeffs, idcol = "input_abbrev")

# clean up output
stacked_fe_coeffs[, p.value := round(p.value, 5)]

#===============================#
# ==== analyze relay effect ====
#===============================#

# subset to the relay effects
stacked_mm_coeffs[ term == "relay_flag"]

# for comparison, the model without the random effect for teacher
stacked_fe_coeffs[ term == "relay_flag"]

