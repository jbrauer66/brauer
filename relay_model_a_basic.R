#=====================================================#
# notes:
# - purpose:
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

#=====================#
# ==== set params ====
#=====================#

# export toggl
p_opt_exp <- FALSE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/psm_2017/data/10_psm_input_sets/most_recent/"

# output directory
p_dir_out <- "/projects/general/sullivan/projects/relay/psm/model_a_basic/"

#===============================#
# ==== set model parameters ====
#===============================#

#============================#
# ==== ~ grouping/levels ====
#============================#

# when we run the regression to calculate propensity scores, how do we want to group our data?
# options...
#
# 1) MPR 
#   - by: teacher experience, subject and year
#   - ex: 1st_year_math_2016
#
# 2) MPR plus grade
#   - by: teacher experience, subject, grade and year
#   - vars: 
#   - ex: 1st_year_math_6_2016

# I don't beleive we have sufficient N for 1.
# but, if instead we pool grades and run 2, then we'll likely have noisy grade flags as well (may be less of a problem)

#================================================#
# ==== ~ propensity score regression formula ====
#================================================#

# MPR's independent variables included:
# student_level: a cubic polynomial of the prior-year math and reading test scores
# student_level: indicators for each grade level
# student_level: gender, age, ELL, IEP, frl, race
# student_level: prior-year absence rate 
# classroom:     the average same-subject prior-year test score of the student’s classroom
# classroom:     the standard deviation of same-subject prior-year test scores in the student’s classroom
# classroom:     class size
# classroom:     class %frl


# for this example, model a:
# - grouping-ps-calculation: pool grades (MPR method)
# - grouping-ps-matching   : pool grades (not MPR method)
# - ps-formula: prior achievement, grade flags, gender, ELL, IEP, frl, race

#====================#
# ==== load data ====
#====================#

# load all the input sets
in_all_inp_sets <- lapply(list.files(p_dir_in, full.names = TRUE), ea_load, opt_print = 0)

# give names
names(in_all_inp_sets) <- gsub(".rdata", "", list.files(p_dir_in))

# temp: this grade dummying should also happen inside the input sets code
library(eainput, lib.loc = "/home/general/package_repo/eainput_2_mr/")

#=======================================#
# ==== stack & create grade dummies ====
#=======================================#

# stack data
stacked_input_sets <- rbindlist(in_all_inp_sets)

# create grade dummies
out_grade_dummy <- dr_dummy(in_data = stacked_input_sets, in_vars_dummy = "grade", opt_force_d_missing = FALSE)

# save & overwrite dummied data
stacked_input_sets <- copy(out_grade_dummy$out_data_dummy)

#=======================================================================#
# ==== split all input sets by teacher experience, subject and year ====
#=======================================================================#

# create unique variable
stacked_input_sets[, psm_model := paste0("exp_" ,ea_exp_level, "_teacher_", year, "_", post_test_subject)]

# put as first column
ea_colorder(stacked_input_sets, "psm_model")

# split data by teacher experience, subject and year
split_sets <- split(stacked_input_sets, by = "psm_model")

#================================#
# ==== export these datasets ====
#================================#

# function to export 
psm_export <- function(in_data){
  
  # save dataset name
  psm_model <- in_data[, unique(psm_model)]
  
  # lil ea-save
  ea_save(in_data          = in_data,
          in_val_path      = p_dir_out,
          in_val_file      = paste0(psm_model, ".csv"),
          in_val_timestamp = p_timestamp)
  
  # return nothing
  return(psm_model)
}

# apply export function
out_names <- lapply(split_sets, psm_export)




# random nate qc
# exp_by_site_all   <- ggplot2::ggplot(ela_data, ggplot2::aes(total_experience, colour = site, fill = site)) + ggplot2::geom_histogram(alpha = .5)
# exp_by_site_young <- ggplot2::ggplot(subset(ela_data, total_experience < 4), ggplot2::aes(total_experience, colour = site, fill = site)) + ggplot2::geom_histogram(alpha = .5)



#===============================#
# ==== show matchit example ====
#===============================#

# going to do a quick matchit example for one year
p_model <- as.data.frame(split_ela$`3`)

# write formula: pretests, gender, ELL, IEP, frl, grade flags
ps_reg_formula <- as.formula(relay_flag ~ z_adrops_pre1 + z_adrops_pre2 + dm_d_gender_f + dm_d_ell_y + dm_d_sped_y + dm_d_frl_y + d_grade_04 + d_grade_05 + d_grade_06 + d_grade_07 + d_grade_08)

# add rownames
rownames(p_model) <- p_model$ea_student_id

# run matchit
p_output <- MatchIt::matchit(formula = ps_reg_formula,
                             data    = p_model,
                             method  = "nearest",
                             caliper = .25,
                             ratio   = 2)  

# look at matches sucks
p_matches <- MatchIt::match.data(p_output, group = "all")

# 
p_actual_matches <- data.table(p_output$match.matrix, keep.rownames = TRUE)

# rename so the matches make sense
setnames(p_actual_matches, "rn", "relay_obs")
setnames(p_actual_matches, "1",  "control_obs")

# any dup?
ea_out_dups(p_actual_matches, "relay_obs")
ea_out_dups(p_actual_matches, "control_obs")

# no duplicates....even with multiple matches
# interesting.
# thats a MatchIt thing because MPR had multiple matches (hence the weighting stuff)
zz <- c(p_actual_matches$control_obs, p_actual_matches$`2`)
ea_out_dups(zz)

# - ps-formula: prior achievement, grade flags, gender, ELL, IEP, frl, race












