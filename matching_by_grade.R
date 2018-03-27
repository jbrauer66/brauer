#=====================================================#
# notes:
# - purpose: Matching Relay and Non-Relay teachers using PSM
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
p_dir_in <- "/projects/relay/psm_2017/data/30_calculate_propensity_scores/most_recent/"

# list of filenames
file_list <- list.files(p_dir_in)

# bring in test file
#x_model <- file_list[2]
#x_data <- ea_load(paste0(p_dir_in, x_model))



mds <- data.frame()

for (name in file_list) {
  
  # get inside loop
  #name <- file_list[[2]]
  
  x_data <- ea_load(paste0(p_dir_in, name))
  x_data[, relay_flag := as.numeric(relay_flag)]
  
  
  name_list <- names(x_data)
  grade_list <- name_list[(1:length(name_list))[startsWith(name_list,"d_grade")]]
  
  # grab grade columns
  grade_list <- grep("d_grade", colnames(x_data), value = TRUE)
  
  
  for (x_grade in grade_list) {
    
    # get inside loop
    #x_grade <- grade_list[[3]]
    
    cur_data <- x_data[get(x_grade) == 1]
    
    p_scores <- cur_data$prediction
    
    # matchit with distance option
    # KEEP RATIO<=10
    m_out <- MatchIt::matchit(relay_flag ~ z_adrops_math_pre +  z_adrops_ela_pre + avg_same_subj_pre + sd_same_subj_pre + avg_d_frl_y + d_gender_f,
                                 data    = cur_data,
                                 distance = p_scores,
                                 method  = "nearest",
                                 caliper = .25,
                                 replace = FALSE,
                                 ratio=10) 
    
    
    # add the weights onto the original dataset
    matched <- cbind(cur_data,m_out$weights)
    # append the weighted datasets
    mds <- rbind(mds,matched,fill=TRUE)
    
  } 
  
}

# test a basic regression
#outcome of interest

impact_test <- lm (z_adrops_post_test_scale_score ~ relay_flag, data=mds, weights=V2)
