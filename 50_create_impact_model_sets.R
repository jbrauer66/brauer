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
library(eainput, lib.loc = "/home/general/package_repo/eainput_2_mr/")  

#=====================#
# ==== set params ====
#=====================#

# export toggl
p_opt_exp <- TRUE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/psm_2017/data/40_match_propensity_scores/most_recent/"

# output directories
p_dir_out    <- "/projects/relay/psm_2017/data/50_create_impact_model_sets/"
p_dir_out_qc <- "/projects/relay/psm_2017/qc/main_code/50_create_impact_model_sets/"

#============================================#
# ==== set covariate balance check vars: ====
#============================================#

# set student demographics
p_student_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y",
                      "d_race_asian", "d_race_black", "d_race_hispanic",
                      "d_race_native", "d_race_multi", "d_race_white")

# set pretest variables
p_rhs_pretests <- c("z_adrops_math_pre", "z_adrops_math_pre_2", "z_adrops_math_pre_3",
                    "z_adrops_ela_pre",  "z_adrops_ela_pre_2",  "z_adrops_ela_pre_3")

# set school characteristics
p_school_vars <- c("avg_same_subj_pre", "avg_d_frl_y")

# paste together MPR vars
p_covariates <- c(p_student_dems, p_rhs_pretests, p_school_vars)

#=======================#
# ==== 1. load data ====
#=======================#

# load all the input sets
in_matched_sets <- lapply(list.files(p_dir_in, full.names = TRUE), ea_load, opt_print = 0)

# give names
names(in_matched_sets) <- gsub("_matched_set.rdata", "", list.files(p_dir_in))

#==========================================#
# ==== 2. create impact model datasets ====
#==========================================#

# we match data at the ps_match_level (ex. cmo_exp_1_teacher_2016_ela_04)
# for the impact we will split by charter & experience ONLY (cmo_exp_1)

# find all possible levels
all_possible_levels <- unique(paste0(substr(names(in_matched_sets), 1, 9), substr(names(in_matched_sets), 23, nchar(names(in_matched_sets)) - 3)))

# create list of data for all levels
impact_model_sets <- ea_init_list(names = all_possible_levels)

# run line by line:
# x_impact_model_level <- all_possible_levels[[2]]

# add matched sets to impact_model_sets list
for(x_impact_model_level in all_possible_levels){
  
  #============================================#
  # ==== ~ grab pertinent matched datasets ====
  #============================================#
  
  # split off subj
  x_subj <- substr(x_impact_model_level, 11, nchar(x_impact_model_level))
  
  # save first part of this
  x_first_part <- substr(x_impact_model_level, 1, 9)
  
  # grab names of all datasets
  x_matched_set_names <- grep(x_first_part, names(in_matched_sets), value = TRUE)
  
  # only keep correct subject
  x_matched_set_names <- grep(x_subj, x_matched_set_names, value = TRUE)
  
  # subset in_matched_sets to these datasets
  x_matched_sets <- in_matched_sets[x_matched_set_names]
  
  # stack these datasets
  x_impact_model_set <- rbindlist(x_matched_sets, use.names = TRUE, fill = TRUE)
  
  #===================================#
  # ==== ~ create "model" dummies ====
  #===================================#
  
  # create model dummy?
  x_impact_model_set[ , grade_subj_yr := paste0(post_test_subject, "_", grade, "_", year)]
  
  # dummy out grade subject combinations
  x_impact_model_set <- dr_dummy(x_impact_model_set, "grade_subj_yr", opt_force_d_missing = 0)$out_data_dummy
  
  #=============================#
  # ==== ~ final formatting ====
  #=============================#
  
  # add model id
  x_impact_model_set[, psm_impact_level := x_impact_model_level]
  
  # save data to list
  impact_model_sets[[x_impact_model_level]] <- x_impact_model_set
}

#=====================================#
# ==== 3. covariate balance table ====
#=====================================#

# 16 impact models = (4 experience levels) * (2 school types - cmo -nyc) * (2 subjects)

# get inside function
# input_data <- copy(impact_model_sets$cmo_exp_1_ela)

# define function
f_covariate_balance <- function(input_data){
  
  #============================#
  # ==== ~ save model data ====
  #============================#
  
  # impact model name
  x_model_name <- input_data[, unique(psm_impact_level)]
  
  #==================================================#
  # ==== ~ calculate mean & sd of all covariates ====
  #==================================================#
  
  # get means & sd's: relay
  relay_means <- input_data[relay_flag == 1, lapply(.SD, mean), .SDcols = p_covariates]
  relay_sd    <- input_data[relay_flag == 1, lapply(.SD, sd),   .SDcols = p_covariates]
  
  # get WTD means & WTD vars : control
  control_means <- input_data[relay_flag == 0, lapply(.SD, Hmisc::wtd.mean, weights = weight), .SDcols = p_covariates]
  control_var   <- input_data[relay_flag == 0, lapply(.SD, Hmisc::wtd.var,  weights = weight), .SDcols = p_covariates]
  
  # melt them
  relay_mean_melted <- melt.data.table(relay_means, variable.name = "covariate", value.name = "relay_mean", variable.factor = FALSE)
  relay_sd_melted   <- melt.data.table(relay_sd,    variable.name = "covariate", value.name = "relay_sd", variable.factor = FALSE)
  
  # melt them
  control_mean_melted <- melt.data.table(control_means, variable.name = "covariate", value.name = "control_wtd_mean", variable.factor = FALSE)
  control_var_melted  <- melt.data.table(control_var,   variable.name = "covariate", value.name = "control_wtd_var", variable.factor = FALSE)
  
  # merge everything
  relay_balance   <- ea_merge(relay_mean_melted,   relay_sd_melted,    "covariate", opt_error_100 = 1, opt_print = 0)
  control_balance <- ea_merge(control_mean_melted, control_var_melted, "covariate", opt_error_100 = 1, opt_print = 0)
  
  # merge #'s
  x_means_sds <- ea_merge(relay_balance, control_balance, "covariate", opt_error_100 = 1, opt_print = 0)
  
  #=============================================#
  # ==== ~ calculate standardized mean diff ====
  #=============================================#
  
  # save n-sizes
  n_control <- input_data[relay_flag == 0, .N]
  n_relay   <- input_data[relay_flag == 1, .N]
  
  # construct within stanndard error
  x_means_sds[, within_se := sqrt(((n_relay-1)*(relay_sd^2)+(n_control-1)*(control_wtd_var))/(n_relay+n_control-2))]
  
  # raw mean difference
  x_means_sds[, raw_md := relay_mean - control_wtd_mean ]
  
  # standardized mean difference
  x_means_sds[, standardized_md := raw_md / within_se ]
  
  # grab necessary vars
  x_smd_table <- subset(x_means_sds, select = c("covariate", "relay_mean", "control_wtd_mean", "standardized_md", "relay_sd", "control_wtd_var"))
  
  #==================================#
  # ==== ~ calculate t statistic ====
  #==================================#
  
  # set covariates
  x_t_covariates <- x_smd_table[is.na(standardized_md) == 0, unique(covariate)]
  
  # get inside function
  # in_covariate <- x_t_covariates[[1]]
  
  # define function
  t_stat_function <- function(in_covariate){
    
    # save control info
    x_control_weight <- input_data[relay_flag == 0, weight]
    x_control_value  <- input_data[relay_flag == 0, get(in_covariate)]
    
    # save control info
    x_relay_weight <- input_data[relay_flag == 1, weight]
    x_relay_value  <- input_data[relay_flag == 1, get(in_covariate)]
    
    # run t-test
    t_stat <- weights::wtd.t.test(x       = x_relay_value,
                                  y       = x_control_value,
                                  weight  = x_relay_weight,
                                  weighty = x_control_weight)
    
    # save p-value
    x_p_value <- as.data.table(t_stat$coefficients, keep.rownames = TRUE)[V1 == "p.value", V2]
    
    
    # create little data.table
    out_p_value <- data.table(covariate = in_covariate, p_value = x_p_value)
    
    # return it
    return(out_p_value)
  }
  
  # apply function to all covariates
  p_value_list <- lapply(x_t_covariates, t_stat_function)
  
  # stack list
  p_value_table <- rbindlist(p_value_list, use.names = TRUE)
  
  #========================#
  # ==== ~ return data ====
  #========================#
  
  # merge data
  out_data <- ea_merge(x_smd_table, p_value_table, "covariate", "x", opt_print = 0)
  
  # add model name
  out_data[, model := x_model_name]
  
  # return
  return(out_data)
}


# apply function
balance_qc <- lapply(impact_model_sets, f_covariate_balance)

# stack em
out_balance_qc <- rbindlist(balance_qc, use.names = TRUE)

# sort columns
ea_colorder(out_balance_qc, c("model"))

# round vars
x_round_vars <- setdiff(colnames(out_balance_qc), c("model", "covariate"))

# round
out_balance_qc[, (x_round_vars) := round(.SD, 4), .SDcols = x_round_vars]

# flag problems
out_balance_qc[, flag_no_balance := ifelse(p_value < .05, 1, 0)]

# rename some vars?
# setnames(out_balance_qc, c("control_wtd_mean"))

#====================#
# ==== 4. export ====
#====================#

# check toggl
if(p_opt_exp == 1){
  
  # save impact model datasets
  ea_save(in_data          = impact_model_sets,
          in_val_path      = p_dir_out,
          in_val_file      = "impact_model_sets.rdata",
          in_val_timestamp = p_timestamp)
  
  # save covariate balance
  ea_save(in_data          = out_balance_qc,
          in_val_path      = p_dir_out_qc,
          in_val_file      = "qc_covariate_balance.csv",
          in_val_timestamp = p_timestamp)
  
}