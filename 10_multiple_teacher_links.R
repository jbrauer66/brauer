#=====================================================#
# notes:
# - purpose: grab input sets from VA for PSM models
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
library(eainput, lib.loc = "/home/general/package_repo/eainput_3_mr/")

# for visuals
# library(ggplot2)
# library(plotly)
# source("~/code/working_code/sullivan/ea_theme.R")

#=============================#
# ==== set general params ====
#=============================#

# export toggl
p_opt_exp <- TRUE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/growth_2017/data/45_input_sets/most_recent/"

# output directories
p_dir_out    <- "/projects/relay/psm_2017/data/10_format_va_input_sets_for_psm/"
p_dir_out_qc <- "/projects/relay/psm_2017/qc/main_code/10_format_va_input_sets_for_psm/"

# parallel option
p_opt_parallel <- TRUE

# number of clusters to run parallel on
p_n_clusters <- 5

#===============================#
# ==== set model parameters ====
#===============================#

# choose model vars
p_post_year   <- c(2016, 2017)
p_dr_abbrev   <- "both_pre"
p_spec_abbrev <- "model_c"

# set student demographics
p_student_dems <- c("d_gender_f", "d_ell_y", "d_sped_y", "d_frl_y",
                    "d_race_asian", "d_race_black", "d_race_hispanic",
                    "d_race_native", "d_race_multi", "d_race_white")

# set classroom demographics
# p_class_dems <- c("same_subject_prior_achievement", "sd_of_ss_pretest", "class_size", "avg_d_frl_y")

# set average method
p_avg_method <- "bdrops"

# choose doseage threshold (drops all student-teacher links with < X percent dose)
p_doseage_threshold <- .5

#====================#
# ==== load data ====
#====================#

# load model_xwalk
in_model_xwalk <- data.table(readxl::read_excel("/projects/relay/growth_2017/data/40_dr_xwalk/most_recent/dr_xwalk_manual_test.xlsx"))

# load link weight
in_link_weight <- ea_load("/projects/relay/growth_2017/data/20_link_weight/most_recent/link_weight.rdata")

# load relay teacher info
#in_relay_info <- fread("/projects/relay/growth_2017/data/00_xwalks/long_relay_xwalk.csv", colClasses = "character")

in_relay_info <- fread("/projects/relay/growth_2017/data/00_xwalks/relay_xwalk/most_recent/relay_xwalk.csv", colClasses = "character")


#==========================#
# ==== format raw data ====
#==========================#

# subset relay data
relay_info <- subset(in_relay_info, select = c("teacher_id","school_year", "relay_flag", "ea_exp_level", "teacher_flag"))

# convert blank teacher-flags to 1
relay_info[teacher_flag == "", teacher_flag := "1"]

# only keep real teachers
relay_info <- ea_subset(relay_info, teacher_flag == 1)

# drop teacher flag
relay_info[, teacher_flag := NULL]

# temp: (put this in the convert things to numeric
relay_info[, relay_flag   := as.numeric(relay_flag)]
relay_info[, ea_exp_level := as.numeric(ea_exp_level)]

# remove exact duplicates
relay_info <- ea_no_dups(relay_info)

#===========================================#
# ==== what models are we going to run? ====
#===========================================#

# note: - right now I'm only going to run this using BOTH pretests
#       - I don't see a scenario where we'd want only one pretest
#       - also only taking model c

# only keep "both_pre" and model c
split_model_xwalk <- ea_subset(in_model_xwalk, pl_post_year %in% p_post_year & spec_abbrev == p_spec_abbrev & dr_abbrev == p_dr_abbrev)

# split model_xwalk by input_abbrev
split_model_xwalk <- split(split_model_xwalk, by = "input_abbrev")  

# save names of model xwalk
all_models <- names(split_model_xwalk)

#========================================#
# ==== define PSM input set function ====
#========================================#

# go inside function
psm_input_xwalk       <- split_model_xwalk[["nys_math_grade_07_2017_spring_both_pre"]]
psm_link_weight       <- in_link_weight
psm_student_dems      <- p_student_dems
psm_relay_info        <- relay_info
psm_doseage_threshold <- p_doseage_threshold
psm_avg_method        <- p_avg_method
psm_dir_in            <- p_dir_in
psm_dir_out           <- p_dir_out
psm_export            <- p_opt_exp
psm_timestamp         <- p_timestamp

# define function
create_psm_input_sets <- function(psm_input_xwalk,
                                  psm_link_weight,
                                  psm_student_dems,
                                  psm_relay_info,
                                  psm_doseage_threshold,
                                  psm_avg_method,
                                  psm_dir_in,
                                  psm_dir_out,
                                  psm_export,
                                  psm_timestamp){
  
  #========================================#
  # ==== i. grab model info from xwalk ====
  #========================================#
  
  # save pretest names
  x_pretests     <- eainput::pl_format_chparm(psm_input_xwalk[, unique(spec_vars_pretest)])
  x_post_subj    <- psm_input_xwalk[, unique(pl_post_subj)]
  x_post_year    <- psm_input_xwalk[, unique(pl_post_year)]
  x_post_grade   <- psm_input_xwalk[, unique(pl_post_grade)]
  x_input_abbrev <- psm_input_xwalk[, unique(input_abbrev)]
  
  #=====================================#
  # ==== ii. load & format controls ====
  #=====================================#
  
  # load data
  x_data <- ea_load(paste0(psm_dir_in, x_input_abbrev, ".rdata"))
  
  # save controls
  x_controls <- subset(x_data$out_data_controls, select = c("ea_student_id", "post_test_name", "post_test_school_year",
                                                            "post_test_subject", "post_test_grade",
                                                            "z_adrops_post_test_scale_score", x_pretests, psm_student_dems))
  
  # temp: loop over student dems & remove observations that are missing them
  for (x_dem in psm_student_dems){ x_controls <- subset(x_controls, is.na(get(x_dem)) == 0) }
  
  # find math/ela pretests
  x_math_pre <- grep("math", x_pretests, value = TRUE)
  x_ela_pre  <- grep("ela", x_pretests, value = TRUE)
  
  # rename data
  setnames(x_controls, x_math_pre, "z_adrops_math_pre")
  setnames(x_controls, x_ela_pre,  "z_adrops_ela_pre")
  setnames(x_controls, "post_test_grade", "grade")
  setnames(x_controls, "post_test_school_year", "year")
  
  #=========================================================#
  # ==== iii. save set of control students with linkage ====
  #=========================================================#
  
  # save list of all students in input set
  x_students <- subset(x_controls, select = "ea_student_id")
  
  # subset linkweight to year & subject
  x_linkage <- subset(psm_link_weight, ea_course_subject == x_post_subj & link_school_year == x_post_year, 
                      select = c("ea_student_id", "teacher_id", "weight", "site"))
  
  # merge onto controls
  student_w_link <- ea_merge(x_students, x_linkage, "ea_student_id", "x", opt_print = 0)
  
  # qc: save drops?
  # x_dropped_no_linkage <- ea_subset(student_w_link, is.na(teacher_id) == 1)
  
  # drop students without linkage
  student_w_link <- ea_subset(student_w_link, is.na(teacher_id) == 0, opt_print = 0)
  
  #=============================================#
  # ==== iv. adjust student-teacher weights ====
  #=============================================#
  
  # create total weight 
  student_w_link[, sum_weight := sum(as.numeric(weight)), by = "ea_student_id"]
  
  # create a new weight
  student_w_link[, new_weight := as.numeric(weight) / sum_weight]
  
  # drop other weights
  student_w_link[, weight := NULL]
  student_w_link[, sum_weight := NULL]
  
  # rename wieght
  setnames(student_w_link, "new_weight", "weight")

  
  #=========================================#
  # ==== v. merge on relay teacher info ====
  #=========================================#
  
  # subset relay info to current year
  x_relay_info <- subset(psm_relay_info, school_year == x_post_year)
  
  # drop school year
  x_relay_info[, school_year := NULL]
  
  # merge relay info onto linkage
  student_w_link <- ea_merge(student_w_link, x_relay_info, "teacher_id", "x", opt_print = 0)
  
  # drop teachers missing experience data
  student_w_link <- ea_subset(student_w_link, is.na(ea_exp_level) == 0, opt_print = 0)
  
  #============================================================#
  # ==== ~ qc: show distribution/histograms of the weights ====
  #============================================================#
  
  # need some qc code in here relative to co-teaching problems
  
  #=======================#
  # ==== ~ histograms ====
  #=======================#
  
  # # 1. by site
  # density_by_site <- ggplot(student_w_link, aes(weight, colour = site, fill = site)) + geom_density(alpha = .5)
  # density_by_site <- density_by_site + ggtitle(paste0("Weight Distribution: ", x_post_grade, " ", toupper(x_post_subj), " ", x_post_year))
  # density_by_site <- density_by_site + ea_theme()
  # density_by_site <- ggplotly(density_by_site)
  
  # # 2. overall
  # overall_density <- ggplot(student_w_link, aes(weight)) + geom_density(alpha = .5) + ea_theme()
  # overall_density <- overall_density + ggtitle(paste0("Weight Density Plot: ", x_post_grade, " ", toupper(x_post_subj), " ", x_post_year))
  
  #================================================================#
  # ==== vi. brule: drop observations with less than X doseage ====
  #================================================================#
  
  # save student_w_link for dropping qc
  #out_dose_qc_data <- subset(student_w_link, select = -c(ea_exp_level))
  
  # add model
  #out_dose_qc_data[, model := paste0(x_post_subj, "_", x_post_grade, "_", x_post_year)]       
  
  # flag if doseage exceeds requirement
  #student_w_link[, flag_drop_low_weight := ifelse(weight <= psm_doseage_threshold, 1, 0)]
  
  # drop teachers missing weight threshold
  #link_adrops <- ea_subset(student_w_link, weight > psm_doseage_threshold, opt_print = 0)
  
  
  #================================================================#
  # ==== vi.NEW BRULE: DROP STUDENTS W RELAY AND NON-RELAY LINKS====
  #================================================================# 
  
  # save student_w_link for dropping qc
  out_dose_qc_data <- subset(student_w_link, select = -c(ea_exp_level))
  
  # add model
  out_dose_qc_data[, model := paste0(x_post_subj, "_", x_post_grade, "_", x_post_year)]       
  
  # cout relay teachers for each student
  student_w_link[, num_relay_teachers := sum(relay_flag==1), by = "ea_student_id"]
  
  # count non_relay teachers for each student
  student_w_link[, num_non_relay_teachers := sum(relay_flag==0), by = "ea_student_id"]
  
  # flag any student that has both relay and non-relay
  student_w_link[, flag_heterogeneous_teachers := ifelse(num_relay_teachers>0 & num_non_relay_teachers>0, 1, 0)]
  
  
  #QC - How much does this increse sample size of relay students?
  #look at students flagged and students with multiple teachers that are kept
  flag_test <- subset(student_w_link, flag_heterogeneous_teachers == 1)
  
  multiple_relay <- subset(student_w_link, num_relay_teachers>1 & flag_heterogeneous_teachers == 0)
  #count unique relay students that have mutiple relay teachers
  count_relay <- subset(multiple_relay, select = c(ea_student_id) )
  multiple_non_relay <- subset(student_w_link, num_non_relay_teachers>1 & flag_heterogeneous_teachers == 0)
  length(unique(count_relay))

  
  #=======================================#
  # ==== vii. merge linkage & conrols ====
  #=======================================#
  
  # merge 
  out_data <- ea_merge(x_controls, link_adrops, "ea_student_id", "both", opt_print = 0)
  
  # drop weight
  out_data[, weight                 := NULL]
  out_data[, flag_heterogeneous_teachers   := NULL]
  
  #=============================================#
  # ==== viii. calculate classroom averages ====
  #=============================================#
  
  # set same subject pretest
  x_same_subj_pretest <- grep(x_post_subj, colnames(out_data), value = TRUE)
  
  # makes you think we should hsave some sort of minimum number of students in your classroom rule. 
  # If you are only linked to 1 student what sort of a classroom is that?
  # Came up when I was calculating the SD of the avg pretest and there were teachers with only 1 student ... gave NA value
  
  # ALso, HUGE class sizes don't make sense. If a teacher is linked to > 60 students then they definitley had mutliple
  # class hours.
  # EA todo: look for better classroom level averages
  
  # temp brule: minimum number of students in a classroom is 5
  # p_temp_min_students <- 5
  
  #====================#
  # ==== ~~ bdrops ====
  #====================#
  
  # parameter: calculate averages BEFORE dropping students because of low doseage
  if(psm_avg_method == "bdrops"){
    
    # question: should this be a weighted mean?
    
    # save links
    all_links <- subset(student_w_link, select = c("teacher_id", "ea_student_id"))
    
    # merge onto controls
    avg_bdrops_set <- ea_merge(x_controls, all_links, "ea_student_id", "both", opt_print = 0)
    
    # get averages
    avgs_bdrops <- avg_bdrops_set[, .(avg_same_subj_pre = mean(get(x_same_subj_pretest)),
                                      sd_same_subj_pre  = sd(get(x_same_subj_pretest)),
                                      avg_d_frl_y       = mean(d_frl_y),
                                      class_size        = .N), by = "teacher_id"]
    
    # merge it onto out_data
    out_data <- ea_merge(out_data, avgs_bdrops, "teacher_id", "x", opt_print = 0)
  }
  
  #====================#
  # ==== ~~ adrops ====
  #====================#
  
  # parameter: calculate averages AFTER dropping students because of low doseage
  if(psm_avg_method == "adrops"){
    
    # get averages
    out_data[, avg_same_subj_pre := mean(get(x_same_subj_pretest)), by = "teacher_id"]
    out_data[, sd_same_subj_pre  := sd(get(x_same_subj_pretest)),   by = "teacher_id"]
    out_data[, avg_d_frl_y       := mean(d_frl_y),                  by = "teacher_id"]
    out_data[, class_size        := .N,                             by = "teacher_id"]
  }
  
  #=========================================================#
  # ==== ix. drop teachers with low numbers of students ====
  #=========================================================#
  
  # this is just a thought because 1 student classrooms make no sense to me and SD is low
  
  # # save number of relay teachers
  # n_relay_pre_drop <- out_data[ relay_flag == 1, length(unique(teacher_id))]
  # 
  # # out_d
  # out_data <- ea_subset(out_data, class_size > p_temp_min_students)
  # 
  # # save new # of relay teachers
  # n_relay_after_drop <- out_data[ relay_flag == 1, length(unique(teacher_id))]
  # 
  # # create qc table
  # n_relay_low_n_drop <- data.table(model = paste0(x_post_subj, "_", x_post_grade, "_", x_post_year),
  #                                  n_relay_dropped_less_6_students = n_relay_pre_drop - n_relay_after_drop)
  
  #==============================#
  # ==== x. final formatting ====
  #==============================#
  
  # column order
  ea_colorder(out_data, c("site", "ea_student_id", "teacher_id", "relay_flag", "ea_exp_level"))
  
  # create math squares & cubes
  out_data[, z_adrops_math_pre_2 := z_adrops_math_pre ^ 2]
  out_data[, z_adrops_math_pre_3 := z_adrops_math_pre ^ 3]
  
  # create ela squares & cubes
  out_data[, z_adrops_ela_pre_2 := z_adrops_ela_pre ^ 2]
  out_data[, z_adrops_ela_pre_3 := z_adrops_ela_pre ^ 3]      
  
  #============================#
  # ==== ~ export datasets ====
  #============================#
  
  # check export toggl
  if(psm_export){
    
    # save input set
    ea_save(in_data          = out_data,
            in_val_path      = psm_dir_out,
            in_val_file      = paste0(x_input_abbrev, ".rdata"),
            in_val_timestamp = psm_timestamp,
            opt_compress     = TRUE)
    
    # # set path for density plot
    # dir_dense_plot <- paste0(psm_dir_out, "qc/10_psm_input_sets/density_plots/", psm_timestamp, "/")
    # 
    # # create directory
    # dir.create(dir_dense_plot, showWarnings = FALSE, recursive = TRUE)
    # 
    # # save density plots
    # htmlwidgets::saveWidget(density_by_site, paste0(dir_dense_plot, x_input_abbrev, ".html"))        
    
    
  }
  
  #========================#
  # ==== ~ return data ====
  #========================#
  
  # only return qc right now, to stack
  return(list(qc_doseage_dataset = out_dose_qc_data))
  
}

#==========================================#
# ==== parallel: create PSM input sets ====
#==========================================#

# store starting time
start_time <- Sys.time()

# print start_time
print(start_time)

# run in parallel
if(p_opt_parallel){
  
  # set mkl threads (if statement checks if DINO/BAMMBAMM)
  eaparallel::ea_set_mkl_threads(1)
  data.table::setDTthreads(1)
  
  # nate trying just an mapply
  out_psm_input_set_qc <- parallel::mcmapply(FUN             = create_psm_input_sets,
                                             psm_input_xwalk = split_model_xwalk[all_models],
                                             MoreArgs        = list(psm_link_weight       = in_link_weight,
                                                                    psm_student_dems      = p_student_dems,
                                                                    psm_relay_info        = relay_info,
                                                                    psm_doseage_threshold = p_doseage_threshold,
                                                                    psm_avg_method        = p_avg_method,
                                                                    psm_dir_in            = p_dir_in,
                                                                    psm_dir_out           = p_dir_out,
                                                                    psm_export            = p_opt_exp,
                                                                    psm_timestamp         = p_timestamp),
                                             mc.cores       = p_n_clusters,
                                             mc.preschedule = FALSE,
                                             mc.cleanup     = TRUE,
                                             SIMPLIFY       = FALSE)
  
  
  # return mkl threads to 24 after parallel
  eaparallel::ea_set_mkl_threads(24)
  data.table::setDTthreads(24)
  
} else {
  
  #
  stop("I need to write this normal mapply too...to-do...")
}


# store complete time
end_time <- Sys.time()

# print total growth model run time
print(paste0("Total create PSM input set run time: ", end_time - start_time))

#================================#
# ==== extract & format data ====
#================================#

# extract data
extracted_data <- ea_extract(out_psm_input_set_qc)

# save dataset to perfrom more qc checks on
out_linkage_and_weights <- copy(extracted_data$qc_doseage_dataset)

#============================#
# ==== export stacked qc ====
#============================#

# check toggl
if(p_opt_exp){
  
  # save input set
  ea_save(in_data          = out_linkage_and_weights,
          in_val_path      = p_dir_out_qc,
          in_val_file      = "out_linkage_and_weights.csv",
          in_val_timestamp = p_timestamp)
}


