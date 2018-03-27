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
library(eainput, lib.loc = "/home/general/package_repo/eainput_3_mr/")

# for visuals
library(ggplot2)
library(plotly)
source("~/code/working_code/sullivan/ea_theme.R")

#=============================#
# ==== set general params ====
#=============================#

# export toggl
p_opt_exp <- TRUE

# time
p_timestamp <- ea_timestamp()

# input directory
p_dir_in <- "/projects/relay/growth_2017/data/45_input_sets/most_recent/"

# output directory
p_dir_out <- "/projects/relay/psm_2017/"

# parallel option
p_opt_parallel <- TRUE

# number of clusters to run parallel on
p_n_clusters <- 8

#===============================#
# ==== set model parameters ====
#===============================#

# choose model vars
p_post_year   <- "2016"
p_dr_abbrev   <- "both_pre"
p_spec_abbrev <- "model_c"

# choose demographics
p_keep_dems <-   c("dm_d_gender_f", "dm_d_ell_y", "dm_d_sped_y", "dm_d_frl_y",
                   "dm_d_race_asian", "dm_d_race_black", "dm_d_race_hispanic",
                   "dm_d_race_native", "dm_d_race_multi", "dm_d_race_white", "dm_d_race_other")

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
in_relay_info <- fread("/projects/relay/growth_2017/data/00_xwalks/long_relay_xwalk.csv", colClasses = "character")

#===========================================#
# ==== what models are we going to run? ====
#===========================================#

# note: - right now I'm only going to run this using BOTH pretests
#       - I don't see a scenario where we'd want only one pretest
#       - also only taking model c

# only keep "both_pre" and model c
split_model_xwalk <- ea_subset(in_model_xwalk, pl_post_year == p_post_year & spec_abbrev == p_spec_abbrev & dr_abbrev == p_dr_abbrev)

# split model_xwalk by input_abbrev
split_model_xwalk <- split(split_model_xwalk, by = "input_abbrev")  

# save names of model xwalk
all_models <- names(split_model_xwalk)

#========================================#
# ==== define PSM input set function ====
#========================================#

# go inside function
# psm_input_xwalk       <- split_model_xwalk[["nys_math_grade_06_2016_spring_both_pre"]]
# psm_link_weight       <- in_link_weight
# psm_demo_list         <- p_keep_dems
# psm_relay_info        <- in_relay_info
# psm_doseage_threshold <- p_doseage_threshold
# psm_dir_out           <- p_dir_out
# psm_export            <- p_opt_exp
# psm_timestamp         <- p_timestamp

# define function
create_psm_input_sets <- function(psm_input_xwalk, 
                                  psm_link_weight, 
                                  psm_demo_list, 
                                  psm_relay_info, 
                                  psm_doseage_threshold, 
                                  psm_dir_out,
                                  psm_export,
                                  psm_timestamp){
  
  #=================================#
  # ==== ~ save data from xwalk ====
  #=================================#
  
  # save pretest names
  x_pretests     <- eainput::pl_format_chparm(psm_input_xwalk[, unique(spec_vars_pretest)])
  x_post_subj    <- psm_input_xwalk[, unique(pl_post_subj)]
  x_post_year    <- psm_input_xwalk[, unique(pl_post_year)]
  x_post_grade   <- psm_input_xwalk[, unique(pl_post_grade)]
  x_input_abbrev <- psm_input_xwalk[, unique(input_abbrev)]
  
  #==========================#
  # ==== ~ load controls ====
  #==========================#
  
  # load data
  x_data <- ea_load(paste0(p_dir_in, x_input_abbrev, ".rdata"))
  
  # save controls
  x_controls <- subset(x_data$out_data_controls, select = c("ea_student_id", "post_test_name", "post_test_subject", 
                                                            "z_adrops_post_test_scale_score", x_pretests, psm_demo_list))
  
  # rename pretests?
  setnames(x_controls, x_pretests, c("z_adrops_pre1", "z_adrops_pre2"))
  
  #=====================================#
  # ==== ~ merge on teacher linkage ====
  #=====================================#
  
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
  student_w_link <- ea_subset(student_w_link, is.na(teacher_id) == 0)
  
  #===================================#
  # ==== ~ adjust teacher weights ====
  #===================================#
  
  # create total wieght 
  student_w_link[, sum_weight := sum(as.numeric(weight)), by = "ea_student_id"]
  
  # create a new weight
  student_w_link[, new_weight := as.numeric(weight) / sum_weight]
  
  # drop other weights
  student_w_link[, weight := NULL]
  student_w_link[, sum_weight := NULL]
  
  # rename wieght
  setnames(student_w_link, "new_weight", "weight")
  
  #========================================#
  # ==== ~ merge on relay teacher info ====
  #========================================#
  
  # subset relay info to current year
  x_relay_info <- subset(psm_relay_info, school_year == x_post_year, select = c("teacher_id", "relay_flag"))
  
  # merge relay info onto linkage (if this isn't 100% we need to fix/update our relay-xwalk)
  student_w_link <- ea_merge(student_w_link, x_relay_info, "teacher_id", "x", opt_error_100 = 1, opt_print = 0)
  
  #============================================================#
  # ==== ~ qc: show distribution/histograms of the weights ====
  #============================================================#
  
  # need some qc code in here relative to co-teaching problems
  
  #=======================#
  # ==== ~ histograms ====
  #=======================#
  
  # # 1. by site
  density_by_site <- ggplot(student_w_link, aes(weight, colour = site, fill = site)) + geom_density(alpha = .5)
  density_by_site <- density_by_site + ggtitle(paste0("Weight Distribution: ", x_post_grade, " ", toupper(x_post_subj), " ", x_post_year))
  density_by_site <- density_by_site + ea_theme()
  density_by_site <- ggplotly(density_by_site)
  
  # # 2. overall
  # overall_density <- ggplot(student_w_link, aes(weight)) + geom_density(alpha = .5) + ea_theme()
  # overall_density <- overall_density + ggtitle(paste0("Weight Density Plot: ", x_post_grade, " ", toupper(x_post_subj), " ", x_post_year))
  
  #==============================================================#
  # ==== ~ brule: drop observations with less than X doseage ====
  #==============================================================#
  
  # flag if doseage exceeds requirement
  student_w_link[, flag_drop_low_weight := ifelse(weight <= psm_doseage_threshold, 1, 0)]
  
  # count # of teachers per student
  student_w_link[, n_teachers_per_student := .N, by = "ea_student_id"]
  
  # drop teachers missing weight threshold
  link_adrops <- ea_subset(student_w_link, weight > psm_doseage_threshold)
  
  #====================================#
  # ==== ~~ qc on dropped students ====
  #====================================#
  
  # the only school where you can be linked to more than one teacher is AF
  # subset(student_w_link, n_teachers_per_student > 1 & weight > .5)      
  
  # students that fell out
  n_stu_bdrop <- student_w_link[, .(n_students_before_dose_rule = length(unique(ea_student_id))), by = c("site")]
  n_stu_adrop <- link_adrops   [, .(n_students_after_dose_rule  = length(unique(ea_student_id))), by = c("site")]
  
  # merge those studetns
  qc_dropped_students <- ea_merge(n_stu_bdrop, n_stu_adrop, c("site"), opt_print = 0)
  
  # fill in NAs with 0
  qc_dropped_students[is.na(qc_dropped_students)] <- 0
  
  # add on total
  overall <- qc_dropped_students[, lapply(.SD, sum), .SDcols = setdiff(colnames(qc_dropped_students), "site")]
  overall[, site := "overall"]
  qc_dropped_students <- rbind(qc_dropped_students, overall)
  
  # calculate the number of students dropping
  qc_dropped_students[, percent_dropped := round(((n_students_before_dose_rule - n_students_after_dose_rule) / n_students_before_dose_rule) *100, 2)]
  
  # add id for model
  qc_dropped_students[, model := paste0(x_post_subj, "_", x_post_grade, "_", x_post_year)]
  
  # put as first column
  ea_colorder(qc_dropped_students, "model")
  
  #=================================================#
  # ==== ~ merge linkage info onto student data ====
  #=================================================#
  
  # merge 
  out_data <- ea_merge(x_controls, link_adrops, "ea_student_id", "both", opt_print = 0)
  
  # drop weight
  out_data[, weight                 := NULL]
  out_data[, n_teachers_per_student := NULL]
  out_data[, flag_drop_low_weight   := NULL]
  
  #============================#
  # ==== ~ export datasets ====
  #============================#
  
  # check export toggl
  if(psm_export){
    
    # save input set
    ea_save(in_data          = out_data,
            in_val_path      = paste0(psm_dir_out, "data/10_psm_input_sets/"),
            in_val_file      = paste0(x_input_abbrev, ".rdata"),
            in_val_timestamp = psm_timestamp,
            opt_compress     = TRUE)
    
    # set path for density plot
    dir_dense_plot <- paste0(psm_dir_out, "qc/10_psm_input_sets/density_plots/", psm_timestamp, "/")
    
    # create directory
    dir.create(dir_dense_plot, showWarnings = FALSE, recursive = TRUE)
    
    # save density plots
    htmlwidgets::saveWidget(density_by_site, paste0(dir_dense_plot, x_input_abbrev, ".html"))        
    
    
  }
  
  #========================#
  # ==== ~ return data ====
  #========================#
  
  # only return qc right now, to stack
  return(qc_dropped_students = qc_dropped_students)
  
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
                                                                    psm_demo_list         = p_keep_dems,
                                                                    psm_relay_info        = in_relay_info,
                                                                    psm_doseage_threshold = p_doseage_threshold,
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

# save the dropped models qc
stacked_psm_qc <- rbindlist(out_psm_input_set_qc, use.names = T)

#============================#
# ==== export stacked qc ====
#============================#

# check toggl
if(p_opt_exp){
  
  # save input set
  ea_save(in_data          = stacked_psm_qc,
          in_val_path      = paste0(p_dir_out, "qc/10_psm_input_sets/dropped_students/"),
          in_val_file      = "dropped_students.csv",
          in_val_timestamp = p_timestamp)
  
}




