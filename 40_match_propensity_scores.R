#======================================================================#
# notes:
# - purpose: split data into desired groups & match propensity scores
# - inputs: 
# - outputs:
# - keywords: #brule #drops #check
# - general:
#=======================================================================#

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
p_dir_in    <- "/projects/relay/psm_2017/data/30_calculate_propensity_scores/most_recent/"
p_dir_in_rf <- "/projects/relay/psm_2017/data/30_calculate_propensity_scores/reg_formulas/most_recent/"

# output directory
p_dir_out    <- "/projects/relay/psm_2017/data/40_match_propensity_scores/"
p_dir_out_qc <- "/projects/relay/psm_2017/qc/main_code/40_match_propensity_scores/"

# parallel option
p_opt_parallel <- TRUE

# number of clusters to run parallel on
p_n_clusters <- 5  

#=============================#
# ==== set matching parms ====
#=============================#

# Matchit parms 
p_ratio   <- 3
p_caliper <- .25
p_replace <- TRUE

#====================#
# ==== load data ====
#====================#

#================================================#
# ==== ~ load inp sets with propensity scors ====
#================================================#

# load list of matched propensity scores  
in_data_list <- lapply(list.files(p_dir_in, full.names = TRUE), ea_load, opt_print = 0)

# assign names to list
names(in_data_list) <- gsub("_w_ps", "", gsub(".rdata", "", list.files(p_dir_in)))

#=====================================#
# ==== ~ load regression formulas ====
#=====================================#

# load list of matched propensity scores  
in_reg_formulas <- lapply(list.files(p_dir_in_rf, full.names = TRUE), ea_load, opt_print = 0)

# assign names to list
names(in_reg_formulas) <- gsub("_reg_formula", "", gsub(".rdata", "", list.files(p_dir_in_rf)))

#=====================================#
# ==== I. set levels for matching ====
#=====================================#

# defin function for setting PSM-match-level
f_ps_match_level <- function(in_data){ in_data[, ps_match_level := paste0(ps_calc_level, "_", grade)] }

# get inside psm_split_levels
# x_data <- copy(in_data_list$cmo_exp_1_teacher_2016_ela)

# function to take in data and split by grade vars
psm_split_levels <- function(in_data){
  
  # split by grade
  x_split <- split(in_data, by = "grade")
  
  # set new ps_match_levels
  x_out <- lapply(x_split, f_ps_match_level)
  
  # stack data
  x_out <- rbindlist(x_out, use.names = TRUE)
  
  # save data
  return(x_out)
}

# apply function to all ps_calc_level input sets
split_psm_sets <- lapply(in_data_list, psm_split_levels)

# stack all datasets 
extracted_data <- rbindlist(split_psm_sets, use.names = TRUE, fill = TRUE)

# re-split data at the "ps_match_level
data_for_matching <- split(extracted_data, by = "ps_match_level")

# save names of data
names_of_input_data <- names(data_for_matching)

#====================================#
# ==== temp? format reg formulas ====
#====================================#

# initialize list
reg_formula_list <- ea_init_list(names = names_of_input_data)

# loop over original formula list 
for(x_formula in names(reg_formula_list)){
  
  # grab equivalent formula
  x_out <- in_reg_formulas[[substr(x_formula, 1, nchar(x_formula) - 3)]]
  
  # add to list
  reg_formula_list[[x_formula]] <- x_out
  
}

#======================================#
# ==== II. match propensity scores ====
#======================================#

# ---------- run line by line--------------------#

# # choose model
# x_model <- names_of_input_data[[10]]
# x_model <- names_of_input_data[[8]]
# x_model <- names_of_input_data[[14]]

# set parms
# match_data      <- data_for_matching[[x_model]]
# ps_formula      <- reg_formula_list[[x_model]]
# match_ratio     <- p_ratio
# match_caliper   <- p_caliper
# match_replace   <- p_replace
# p_match_export  <- p_opt_exp
# p_dir_match_out <- p_dir_out

# ---------- run line by line--------------------#

# show model 31 problem:
# match_data[, .(mean_prop_score = mean(prediction),
#                n_students      = .N), by = c("ps_match_level", "site", "relay_flag")]

# ok.. so let's look at prop score regression,
# pretty sure it's totally screwed up by the input data we fed it... not sure how tho

# define function
match_p_scores <- function(match_data, 
                           ps_formula, 
                           match_ratio, 
                           match_caliper, 
                           match_replace,
                           p_match_export,
                           p_dir_match_out){
  
  #======================#
  # ==== ~ grab data ====
  #======================#
  
  # copy data.table
  in_data <- copy(match_data)
  
  # save name of ps-calculation level
  x_ps_calc_level <- in_data[, unique(ps_calc_level)]
  
  # save name of ps_match level
  x_ps_match_level <- in_data[, unique(ps_match_level)]
  
  #==============================#
  # ==== ~ temp NA value fix ====
  #==============================#
  
  # pretty much if your dataset has any NAs in it, matchit dies
  # this seems counter-intuitive if it is only checking covariate balance for the vars in the reg formula...
  # when splitting data i create these NAs
  
  # get list of all NA columns
  na_col_list <- as.data.table(in_data[, colSums(is.na(in_data)) != nrow(in_data)], keep.rownames = TRUE)
  
  # grab names of all NA columns
  na_col_list <- na_col_list[V2 == FALSE, unique(V1)]
  
  # drop them if they exist
  if(length(na_col_list) > 0) { in_data[, c(na_col_list) := NULL] }
  
  # drop SD of class and class size (lots of NAs ehre)
  in_data <- subset(in_data, select = -c(class_size, sd_same_subj_pre))
  
  #=================================================================#
  # ==== ~ qc on prematching distributions of relay // nonrelay ====
  #=================================================================#
  
  # john found in a model that there all the relay students came from AF and all controls from Uncomoon
  # this seems exrememly dangerous.
  # must do qc on this
  
  # before matching do some qc on teh sites & distribution of relay // non-relay students
  qc_pre_match_ratio <- ea_table(in_data, c("ps_match_level", "site", "relay_flag"))
  
  # cast wide
  qc_pre_match_ratio <- dcast.data.table(qc_pre_match_ratio, ps_match_level + site ~ paste0("n_relay_", relay_flag), value.var = "count")
  
  # add ratio
  qc_pre_match_ratio[, control_to_relay_ratio := round( n_relay_0 / n_relay_1)]
  
  #====================#
  # ==== ~ matchit ====
  #====================#
  
  # temp? for CMO's set match_ratio to 1
  # if(grepl("cmo", x_ps_match_level)) {match_ratio <- 1}
  # if(grepl("cmo", x_ps_match_level)) {match_replace <- TRUE}
  
  
  
  
  # define function to let me know when errors occur
  relay_matchit_function <- purrr::safely(MatchIt::matchit)
  
  # get matches
  out_matchit_list <-relay_matchit_function(formula  = ps_formula,
                                            data     = in_data,
                                            method   = "nearest",
                                            distance = in_data[, prediction],
                                            ratio    = match_ratio, 
                                            caliper  = match_caliper,
                                            replace  = match_replace)
  
  # check for warnings/results
  flag_no_matches <- if (is.null(out_matchit_list$result) == 1) { 1 } else { 0 }
  
  #=================================#
  # ==== ~ if matchit worked... ====
  #=================================#
  
  # if matchit found matches
  if(flag_no_matches == 0){
    
    #===================================#
    # ==== ~ format matched dataset ====
    #===================================#
    
    # **note that MatchIt::get_matches saves a dataset of ALL control observations, regardless if they found
    #   matches or not.
    #   
    #   Because of this I use MatchIt::match.data instead. Also MatchIt::match.data has correct weight inside it
    # 
    
    # temp...get matches again
    # i should really be able to save this from above
    matchit_output <- MatchIt::matchit(formula  = ps_formula,
                                       data     = in_data,
                                       method   = "nearest",
                                       distance = in_data[, prediction],
                                       ratio    = match_ratio, 
                                       caliper  = match_caliper,
                                       replace  = match_replace)        
    
    # save the matched datasets
    matched_data <- MatchIt::match.data(object   = matchit_output,
                                        group    = "all",
                                        distance = "propensity_score", 
                                        weight   = "weight")
    
    # convert to data.table
    matched_data <- as.data.table(matched_data)
    
    # drop variables
    matched_data[, prediction := NULL]
    
    # sort columns
    ea_colorder(matched_data, c("ps_match_level", "ps_calc_level", "post_test_name"))
    
    #===================================================#
    # ==== ~ qc: save % of model that found matches ====
    #===================================================#
    
    # **note the matchit_output$nn output says you are "matched" if you have AT LEAST 1 match
    #        ie, you are only "unmatched" if you have absolutely no matches 
    #        ex. if you have 1 out of 3 matches (b/c ratio = 3) then you are "matched"
    
    # get the n of matches
    qc_match_rate <- as.data.table(matchit_output$nn, keep.rownames = TRUE)
    
    # save n-sizes
    n_relay_orig    <- qc_match_rate[rn == "All",     Treated]
    n_relay_matched <- qc_match_rate[rn == "Matched", Treated]
    
    
    
    # SAVE N-SIZES OF CONTROL STUDENTS
    n_control_orig    <- qc_match_rate[rn == "All",     Control]
    n_control_matched <- qc_match_rate[rn == "Matched", Control]
    
    
    
    
    
    # creat qc table
    qc_match_rate <- data.table(match_model     = x_ps_match_level,
                                n_relay_orig    = n_relay_orig,
                                n_relay_matched = n_relay_matched,
                                n_relay_dropped = n_relay_orig -n_relay_matched, 
                                p_relay_dropped = round (100 *((n_relay_orig -n_relay_matched) / n_relay_orig), 2),
                                n_control_orig    = n_control_orig,
                                n_control_matched = n_control_matched,
                                control_ratio = n_control_matched/n_relay_matched,
                                #n_control_dropped = n_control_orig -n_control_matched, 
                                #p_control_dropped = round (100 *((n_control_orig -n_control_matched) / n_control_orig), 2),
                                ratio           = match_ratio,
                                caliper         = match_caliper,
                                replace         = match_replace)
    
    #================================#
    # ==== ~ qc: student matches ====
    #================================#
    
    #   # create a student id / rn / propensity score xwalk
    #   student_id_xwalk <- data.table(ea_student_id    = in_data[, ea_student_id],
    #                                  rn               = as.character(1:nrow(in_data)),
    #                                  propensity_score = in_data[, prediction])
    # 
    #   # matches data
    # 	student_matches <- as.data.table(matchit_output$match.matrix, keep.rownames = T)
    # 
    # 	# merge on real student id
    # 	student_matches <- ea_merge(student_matches, student_id_xwalk, "rn", "x", opt_error_100 = 1)
    # 
    # 	# drop rn
    # 	student_matches[, rn := NULL]
    # 
    #   # melt data long
    #   student_matches <- melt.data.table(student_matches, id.vars       = c("ea_student_id", "propensity_score"),
    #                                                       variable.name = "match",
    #                                                       value.name    = "matched_rn")
    # 
    #   # drop NA values
    #   student_matches <- subset(student_matches, is.na(matched_rn) == 0)
    # 
    #   # rename vars in xwalk for merge
    #   setnames(student_id_xwalk, c("ea_student_id", "rn", "propensity_score"),
    #                              c("matched_student_id", "matched_rn" ,"matched_p_score"))
    # 
    #  	# merge on real student id
    # 	student_matches <- ea_merge(student_matches, student_id_xwalk, "matched_rn", "x", opt_error_100 = 1)
    # 
    # 	# drop matched rn
    # 	student_matches[, matched_rn := NULL]
    # 
    # 	# calculate the difference in propensity scores
    # 	student_matches[, ps_score_diff := abs(propensity_score - matched_p_score)]
    # 
    # 	# get total n matches
    # 	student_matches[, total_n_matches := .N, by = "ea_student_id"]
    
    #===================#
    # ==== ~ export ====
    #===================#
    
    # check exp dir?
    if(p_match_export == 1){
      
      # save input set
      ea_save(in_data          = matched_data,
              in_val_path      = p_dir_match_out,
              in_val_file      = paste0(x_ps_match_level, "_matched_set.rdata"),
              in_val_timestamp = p_timestamp)        
      
    }
    
    #===================#
    # ==== ~ return ====
    #===================#
    
    # return
    return(list(qc_match_rate      = qc_match_rate,
                qc_no_matches      = NULL, 
                qc_pre_match_ratio = qc_pre_match_ratio))
  }
  
  #=================================#
  # ==== ~ if matchit failed... ====
  #=================================#
  
  # if matchit found zero matches
  if(flag_no_matches == 1){
    
    # create qc table
    qc_match_rate <- data.table(match_model     = x_ps_match_level,
                                n_relay_orig    = in_data[relay_flag == 1, .N],
                                n_relay_matched = 0,
                                n_relay_dropped = in_data[relay_flag == 1, .N], 
                                p_relay_dropped = 100,
                                ratio           = match_ratio,
                                caliper         = match_caliper,
                                replace         = match_replace)
    
    # return data
    return(list(qc_match_rate      = qc_match_rate,
                qc_no_matches      = x_ps_match_level, 
                qc_pre_match_ratio = qc_pre_match_ratio))
  }
}

#================================================#
# ==== parallel: calculate propensity scores ====
#================================================#

# store starting time
start_time <- Sys.time()

# print start_time
print(start_time)

# run in parallel
if(p_opt_parallel){
  
  # set mkl threads (if statement checks if DINO/BAMMBAMM)
  eaparallel::ea_set_mkl_threads(1)
  data.table::setDTthreads(1)
  
  # run
  out_match_qc <- parallel::mcmapply(FUN            = match_p_scores,
                                     match_data     = data_for_matching[names_of_input_data],
                                     ps_formula     = reg_formula_list[names_of_input_data],
                                     MoreArgs       = list(match_ratio        = p_ratio,
                                                           match_caliper      = p_caliper,
                                                           match_replace      = p_replace,
                                                           p_match_export     = p_opt_exp,
                                                           p_dir_match_out    = p_dir_out),
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
print(paste0("Total match PS sets run time: ", end_time - start_time))

#====================#
# ==== save data ====
#====================#

# extract function data
extracted_data <- ea_extract(out_match_qc)

# stack matched datasets
stacked_qc_prematch_ratios  <- copy(extracted_data$qc_pre_match_ratio)
stacked_qc_match_rate       <- copy(extracted_data$qc_match_rate)
models_without_matches      <- unlist(extracted_data$qc_no_matches)

# replace NA's with 0s
stacked_qc_prematch_ratios[is.na(stacked_qc_prematch_ratios)] <- 0
stacked_qc_match_rate [is.na(stacked_qc_match_rate)] <- 0

#======================#
# ==== export data ====
#======================#

# check exp dir?
if(p_opt_exp == 1){
  
  # save stacked_qc_match_rate
  ea_save(in_data          = stacked_qc_match_rate,
          in_val_path      = p_dir_out_qc,
          in_val_file      = "qc_match_rate_3_replace.csv",
          in_val_timestamp = p_timestamp)    
  
  # save stacked_qc_prematch_ratios
  #ea_save(in_data          = stacked_qc_prematch_ratios,
  #        in_val_path      = p_dir_out_qc,
  #        in_val_file      = "qc_prematch_ratios_replace.csv",
  #        in_val_timestamp = p_timestamp)    
}  