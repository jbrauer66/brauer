
#################################################################################################################*
# notes:
# - purpose:  investigate co-teaching in Texas Relay data
# - inputs:   source linkage
# - outputs:  school link weight 
# - keywords: #brule #drops #check #notes
# - general:  subsetting at end to remove students with an NA school id. have qc check to make sure these drops are all non-fay students
##################################################################################################################*

#===============================================#
# ==== load packages and clear objects/log =====
#===============================================# 



# load easimple and clear objects/log
library(easimple)
ea_start()

# load other packages
library(data.table)

#====================#
# ==== set parms ====
#====================#

# load source linkage data
in_source_linkage <- ea_load("/projects/relay/growth_2017/qc/tx_coteaching/qc_teaching_dataset.rdata")

ela_2017_linkage <- subset(in_source_linkage, ea_course_subject=="ela" & link_school_year=="2017")

ela_2017_linkage_only_two <- subset(ela_2017_linkage, n_teachers_in_one_year_and_subject==2)


#===============================#
# ==== deal with duplicates ====
#===============================#

#=============================================#
# ==== ~ keep only max link by start date ====
#=============================================#

# calc longest link length by start date for stud/teach/yr/subj
ela_2017_linkage_only_two[, max_link_length  := max(link_length), by = c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject", "student_teacher_rel_start") ]

# keep only links that are longest for start date
longest_link <- ea_subset(ela_2017_linkage_only_two, link_length == max_link_length)

# drop dups by relationship dates/subj/year/teach/stud
longest_link_no_dups <- ea_no_dups(longest_link, c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject", "student_teacher_rel_start", "student_teacher_rel_end"))

#======================================================#
# ==== ~ if same end date, keep earlier start date ====
#======================================================#

# get those with dup end dates
dup_end_dates <- ea_out_dups(longest_link_no_dups, c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject", "student_teacher_rel_end"))

# drop ALL dups with same end date from remaining links
long_link_no_end_dup <- ea_no_dups(longest_link_no_dups, c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject", "student_teacher_rel_end"), opt_delete_all = TRUE)

# identify min start in dup end dates
dup_end_dates[, min_start := min(student_teacher_rel_start), c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject", "student_teacher_rel_end")]

# from those with dup end dates, keep only longest link
sub_end_dups <- ea_subset(dup_end_dates, student_teacher_rel_start == min_start)

# null min_start variable for binding
sub_end_dups[, min_start := NULL]

# bind "fixed" end dups back to linkages
long_link <- rbind(long_link_no_end_dup, sub_end_dups, use.names = TRUE)


#========================================#
# ==== ~ check for overlapping links ====
#========================================#

# subset remaining repeated entries by teach/stud/subj/year
still_dups <- ea_out_dups(long_link, c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject"))

# drop dups completely from link set
long_link_no_dup <- ea_no_dups(long_link, c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject"), opt_delete_all = TRUE )

# find min start, max end, and sum length
still_dups[, ':='(min_start = min(as.Date(student_teacher_rel_start)),
                  max_end = max(as.Date(student_teacher_rel_end)),
                  sum_diff_links = sum(link_length)),
           by = c("ea_student_id", "teacher_id", "link_school_year", "ea_course_subject")]

# calc length from min to max
still_dups[, min_max_link_length := max_end - min_start + 1]

# indicate if min_max length != sum unique links
still_dups[, flag_overlap := ifelse(min_max_link_length < sum_diff_links, 1, 0)]

# count the number of days overlapping
still_dups[, days_overlap := sum_diff_links-min_max_link_length]

# ratio number of days overlapping by the min
still_dups[, ratio_overlap := days_overlap / as.numeric(min_max_link_length)]



test_overlap <- ea_subset(still_dups, flag_overlap==1)

test_length <- ea_subset(test_overlap, ratio_overlap>.5)

test_one_day_overlap <- ea_subset(test_overlap, days_overlap==1)



# MERGE ON RELAY AND EXPERIENCE INFO
in_relay_xwalk <- fread("/projects/relay/growth_2017/data/00_xwalks/relay_xwalk/most_recent/relay_xwalk.csv")
in_relay_xwalk_subset <- subset(in_relay_xwalk, school_year=="2017")
teacher_relay_exp <- subset(in_relay_xwalk_subset, select = c("teacher_id", "relay_flag", "ea_exp_level"))

still_dups_merge <- ea_merge(in_data_x = still_dups, in_data_y = teacher_relay_exp, in_vars_by = "teacher_id", opt_merge_type = "both")


# GET SOME NUMBERS OF STUDENTS 


