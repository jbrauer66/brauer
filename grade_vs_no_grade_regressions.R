#=====================================================#
# notes:
# - purpose: prepare data for PSM model
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

#=============================#
# ==== set general params ====
#=============================#

# export toggl
p_opt_exp <- TRUE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/general/sullivan/projects/relay/psm/model_a_basic/most_recent/"

# # output directory
# p_dir_out <- "/projects/relay/psm_2017/"
# 
# # parallel option
# p_opt_parallel <- TRUE
# 
# # number of clusters to run parallel on
# p_n_clusters <- 5

# choose RHS demographics
p_rhs_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y",
                  "d_race_asian", "d_race_black", "d_race_hispanic",
                  "d_race_native", "d_race_multi", "d_race_white", "d_race_other")

# set RHS grade variables
p_rhs_grades <- c("d_grade_04", "d_grade_05", "d_grade_06", "d_grade_07", "d_grade_08")


#=======================================#
# example: "exp_2_teacher_2016_math.csv"
#=======================================#

# model
x_model <- "exp_2_teacher_2016_math.csv"

# load data
in_data <- fread(paste0(p_dir_in, x_model))

# temp: change relay flag to numeric
in_data[, relay_flag := as.numeric(relay_flag)]

overall_formula <- as.formula(paste0("relay_flag ~ ", paste0(paste(p_rhs_dems, collapse = " + "), " + ", paste(p_rhs_grades, collapse = " + "))))

# run overall regression
ztest <- glm(formula = overall_formula, family = binomial(link = "logit"), data = in_data)


# run easier regression
z_rhs_dems <-   c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y")
ztest_formula <- as.formula(paste0("relay_flag ~ z_adrops_pre1 + z_adrops_pre2 + ", paste(p_rhs_grades, collapse = " + "), " + ", paste(z_rhs_dems, collapse = " + ")))

ztest <- glm(formula = ztest_formula, family = binomial, data = in_data)


# works without races...





# save fitted values


in_x_set <- ea_load("/projects/relay/growth_2017/data/45_input_sets/most_recent/nys_math_grade_04_2016_spring_both_pre.rdata")

x_student <- subset(in_x_set$out_data_controls, ea_student_id == "102000031")



.0005











