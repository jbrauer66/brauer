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
library(eagrowth, lib.loc = "/home/general/package_repo/eagrowth_2_mr/")  

#=====================#
# ==== set params ====
#=====================#

# export toggl
p_opt_exp <- FALSE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/psm_2017/data/40_match_propensity_scores/most_recent/"

# output directories
p_dir_out    <- "/projects/relay/psm_2017/data/50_impact_model/"
p_dir_out_qc <- "/projects/relay/psm_2017/qc/50_impact_model/"

#====================#
# ==== load data ====
#====================#

# load all the input sets
in_matched_sets <- lapply(list.files(p_dir_in, full.names = TRUE), ea_load, opt_print = 0)

# give names
names(in_matched_sets) <- gsub("_matched_set.rdata", "", list.files(p_dir_in))

#======================================#
# ==== set levels for impact model ====
#======================================#

# we match data at the ps_match_level (ex. cmo_exp_1_teacher_2016_ela_04)
# 
# for the impact we will split by charter & experience ONLY (cmo_exp_1)

# find all possible levels
all_possible_levels <- unique(substr(names(in_matched_sets), 1, 9))

# create list of data for all levels
impact_model_sets <- ea_init_list(names = all_possible_levels)

# add matched sets to impact_model_sets list
for(x_impact_model_level in all_possible_levels){
  
  # grab names of all datasets
  x_matched_set_names <- grep(x_impact_model_level, names(in_matched_sets), value = TRUE)
  
  # subset in_matched_sets to these datasets
  x_matched_sets <- in_matched_sets[x_matched_set_names]
  
  # stack these datasets
  x_impact_model_set <- rbindlist(x_matched_sets, use.names = TRUE, fill = TRUE)
  
  # save data to list
  impact_model_sets[[x_impact_model_level]] <- x_impact_model_set
}

#===========================#
# ==== run impact model ====
#===========================#

# so here's where our LME4 / WLS regressions come into play
#
# Use the 8 datasets saved in impact_model_sets for our 8 models
#
# 8 impact models = (4 experience levels) * (2 school types - cmo -nyc) 

# if you want to just try it out with a single model do
single_model <- copy(impact_model_sets$nyc_exp_1)

# eventually we'll probably need parallel for this so list is good

# single_model <- impact_model_sets[[1]]


# wrapper that runs a fixed effects model and a mixed model (only adding a random intercept)
run_regs <- function(in_data_single_model, 
                     
                     # outcome variable 
                     f_posttest = "z_adrops_post_test_scale_score", 
                     
                     # rhs fixed effect terms to string together in the formula 
                     f_pretests = c("z_adrops_math_pre", "z_adrops_ela_pre"), 
                     f_xvars = c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y", "d_race_asian", "d_race_black", "d_race_hispanic", "d_race_native", "d_race_multi"), 
                     f_program_effect = "relay_flag", 
                     
                     # random effect terms (just a random intercept)
                     f_random_effect = "(1 | teacher_id)"){
  
  # construct the fixed effects part of the formula 
  fe_formula <- paste0(f_posttest, " ~ ", paste0(f_pretests, collapse = " + "), " + ", paste0(f_xvars, collapse = " + "), " + relay_flag")
  
  # add in a random effect term for teacher to run a mixed model 
  me_formula <- paste0(as.character(fe_formula), " + ", f_random_effect)
  
  # fixed effects only (standard errors are too low)
  fe_reg_output <- lm(fe_formula, data = in_data_single_model)
  
  
  #EDIT: 
  # function that calculates cluster robust standard errors
  #dat=data, fm=basic lm regression output, cluster=level of error clustering
  cl   <- function(dat,fm, cluster){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL) }
  
  # apply the 'cl' function by choosing a variable to cluster on.
  # here, we are clustering on teacher_id
  test_cluster <- cl(in_data_single_model, fe_reg_output, in_data_single_model$teacher_id)
  
  cl_coeffs <- data.table(broom::tidy(test_cluster))
  #END EDIT
  
  
  # store fixed effect coefficients  
  fe_coeffs <- data.table(broom::tidy(fe_reg_output))
  
  # mixed model-- same fixed effects with a random intercept for teacher
  me_reg_output <- lme4::lmer(me_formula, data = in_data_single_model)
  
  # coefficients 
  me_coeffs <- data.table(broom::tidy(me_reg_output))
  
  list(fe_coeffs = fe_coeffs, 
       me_coeffs = me_coeffs,
       cl_coeffs= cl_coeffs)
  
  
  
}  


output_by_input_data <- lapply(impact_model_sets, run_regs)

# group by fe / me
output_by_model_type <- purrr::transpose(output_by_input_data)

# stacked estimates of the mixed model coefficients 
stacked_mm_coeffs <- rbindlist(output_by_model_type$me_coeffs, idcol = "input_abbrev")
stacked_fe_coeffs <- rbindlist(output_by_model_type$fe_coeffs, idcol = "input_abbrev")
stacked_cl_coeffs <- rbindlist(output_by_model_type$cl_coeffs, idcol = "input_abbrev")

# subset to the relay effects
stacked_mm_coeffs[ term == "relay_flag"]

# for comparison, the model without the random effect for teacher
stacked_fe_coeffs[ term == "relay_flag"]

# for comparison, the model without cluster robust standard errors
stacked_cl_coeffs[ term == "relay_flag"]

