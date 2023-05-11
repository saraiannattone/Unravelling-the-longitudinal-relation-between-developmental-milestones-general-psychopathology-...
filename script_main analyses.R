################################################################################
## PREPARATION                                                                ##
################################################################################
# Loading packages -------------------------------------------------------------

library(haven)
library(psych)
library(Hmisc)
library(pastecs)
library(mice) 
library(lattice)
library(lavaan)
library(semTools)

# Loading dataset --------------------------------------------------------------

DB <- 
  read.csv2()

View(DB)

DB$"geslacht" <- as.character(DB$"geslacht")

################################################################################
## COMPUTE TOTAL SCORES                                                       ##
################################################################################

DB$sdq_tot_t1 <- rowSums(DB[,c("T1_SDQ_S11_17_002", "T1_SDQ_S11_17_003", 
                                   "T1_SDQ_S11_17_005", "T1_SDQ_S11_17_006", 
                                   "T1_SDQ_S11_17_007", "T1_SDQ_S11_17_008", 
                                   "T1_SDQ_S11_17_010", "T1_SDQ_S11_17_011", 
                                   "T1_SDQ_S11_17_012", "T1_SDQ_S11_17_013", 
                                   "T1_SDQ_S11_17_014", "T1_SDQ_S11_17_015", 
                                   "T1_SDQ_S11_17_016", "T1_SDQ_S11_17_018", 
                                   "T1_SDQ_S11_17_019", "T1_SDQ_S11_17_021", 
                                   "T1_SDQ_S11_17_022", "T1_SDQ_S11_17_023", 
                                   "T1_SDQ_S11_17_024", "T1_SDQ_S11_17_025")], 
                           na.rm = F)

DB$sdq_tot_t2 <- rowSums(DB[,c("T2_SDQ_S11_17_FU_002", "T2_SDQ_S11_17_FU_003", 
                                   "T2_SDQ_S11_17_FU_005", "T2_SDQ_S11_17_FU_006", 
                                   "T2_SDQ_S11_17_FU_007", "T2_SDQ_S11_17_FU_008",
                                   "T2_SDQ_S11_17_FU_010", "T2_SDQ_S11_17_FU_011", 
                                   "T2_SDQ_S11_17_FU_012", "T2_SDQ_S11_17_FU_013", 
                                   "T2_SDQ_S11_17_FU_014", "T2_SDQ_S11_17_FU_015", 
                                   "T2_SDQ_S11_17_FU_016", "T2_SDQ_S11_17_FU_018", 
                                   "T2_SDQ_S11_17_FU_019", "T2_SDQ_S11_17_FU_021", 
                                   "T2_SDQ_S11_17_FU_022", "T2_SDQ_S11_17_FU_023", 
                                   "T2_SDQ_S11_17_FU_024", "T2_SDQ_S11_17_FU_025")], 
                           na.rm = F)

DB$sdq_tot_t3 <- rowSums(DB[,c("T3_SDQ_S11_17_FU_002", "T3_SDQ_S11_17_FU_003", 
                                   "T3_SDQ_S11_17_FU_005", "T3_SDQ_S11_17_FU_006", 
                                   "T3_SDQ_S11_17_FU_007", "T3_SDQ_S11_17_FU_008", 
                                   "T3_SDQ_S11_17_FU_010", "T3_SDQ_S11_17_FU_011", 
                                   "T3_SDQ_S11_17_FU_012", "T3_SDQ_S11_17_FU_013", 
                                   "T3_SDQ_S11_17_FU_014", "T3_SDQ_S11_17_FU_015", 
                                   "T3_SDQ_S11_17_FU_016", "T3_SDQ_S11_17_FU_018", 
                                   "T3_SDQ_S11_17_FU_019", "T3_SDQ_S11_17_FU_021", 
                                   "T3_SDQ_S11_17_FU_022", "T3_SDQ_S11_17_FU_023", 
                                   "T3_SDQ_S11_17_FU_024", "T3_SDQ_S11_17_FU_025")], 
                           na.rm = F)

DB$lpfs_tot_t1 <- rowSums(DB[,c("T1_LPFS_001", "T1_LPFS_002", 
                                   "T1_LPFS_003", "T1_LPFS_004", 
                                   "T1_LPFS_005", "T1_LPFS_006", 
                                   "T1_LPFS_007", "T1_LPFS_008", 
                                   "T1_LPFS_009", "T1_LPFS_010", 
                                   "T1_LPFS_011", "T1_LPFS_012")], 
                           na.rm = F)

DB$lpfs_tot_t2 <- rowSums(DB[,c("T2_LPFS_001", "T2_LPFS_002", 
                                   "T2_LPFS_003", "T2_LPFS_004", 
                                   "T2_LPFS_005", "T2_LPFS_006", 
                                   "T2_LPFS_007", "T2_LPFS_008", 
                                   "T2_LPFS_009", "T2_LPFS_010", 
                                   "T2_LPFS_011", "T2_LPFS_012")], 
                           na.rm = F)

DB$lpfs_tot_t3 <- rowSums(DB[,c("T3_LPFS_001", "T3_LPFS_002", 
                                   "T3_LPFS_003", "T3_LPFS_004", 
                                   "T3_LPFS_005", "T3_LPFS_006", 
                                   "T3_LPFS_007", "T3_LPFS_008", 
                                   "T3_LPFS_009", "T3_LPFS_010", 
                                   "T3_LPFS_011", "T3_LPFS_012")], 
                           na.rm = F)

DB$otl_tot_t1 <- rowSums(DB[,c("T1_OTL_J_KJP_001", "T1_OTL_J_KJP_002", 
                                  "T1_OTL_J_KJP_003", "T1_OTL_J_KJP_004", 
                                  "T1_OTL_J_KJP_005", "T1_OTL_J_KJP_006", 
                                  "T1_OTL_J_KJP_007", "T1_OTL_J_KJP_008", 
                                  "T1_OTL_J_KJP_009", "T1_OTL_J_KJP_010", 
                                  "T1_OTL_J_KJP_011", "T1_OTL_J_KJP_012", 
                                  "T1_OTL_J_KJP_013", "T1_OTL_J_KJP_014", 
                                  "T1_OTL_J_KJP_015", "T1_OTL_J_KJP_016", 
                                  "T1_OTL_J_KJP_017", "T1_OTL_J_KJP_018", 
                                  "T1_OTL_J_KJP_019", "T1_OTL_J_KJP_020", 
                                  "T1_OTL_J_KJP_021")], 
                          na.rm = F)

DB$otl_tot_t2 <- rowSums(DB[,c("T2_OTL_J_KJP_001", "T2_OTL_J_KJP_002", 
                                  "T2_OTL_J_KJP_003", "T2_OTL_J_KJP_004", 
                                  "T2_OTL_J_KJP_005", "T2_OTL_J_KJP_006", 
                                  "T2_OTL_J_KJP_007", "T2_OTL_J_KJP_008", 
                                  "T2_OTL_J_KJP_009", "T2_OTL_J_KJP_010", 
                                  "T2_OTL_J_KJP_011", "T2_OTL_J_KJP_012", 
                                  "T2_OTL_J_KJP_013", "T2_OTL_J_KJP_014", 
                                  "T2_OTL_J_KJP_015", "T2_OTL_J_KJP_016", 
                                  "T2_OTL_J_KJP_017", "T2_OTL_J_KJP_018", 
                                  "T2_OTL_J_KJP_019", "T2_OTL_J_KJP_020", 
                                  "T2_OTL_J_KJP_021")], 
                          na.rm = F)

DB$otl_tot_t3 <- rowSums(DB[,c("T3_OTL_J_KJP_001", "T3_OTL_J_KJP_002", 
                                  "T3_OTL_J_KJP_003", "T3_OTL_J_KJP_004", 
                                  "T3_OTL_J_KJP_005", "T3_OTL_J_KJP_006", 
                                  "T3_OTL_J_KJP_007", "T3_OTL_J_KJP_008", 
                                  "T3_OTL_J_KJP_009", "T3_OTL_J_KJP_010", 
                                  "T3_OTL_J_KJP_011", "T3_OTL_J_KJP_012", 
                                  "T3_OTL_J_KJP_013", "T3_OTL_J_KJP_014", 
                                  "T3_OTL_J_KJP_015", "T3_OTL_J_KJP_016", 
                                  "T3_OTL_J_KJP_017", "T3_OTL_J_KJP_018", 
                                  "T3_OTL_J_KJP_019", "T3_OTL_J_KJP_020", 
                                  "T3_OTL_J_KJP_021")], 
                          na.rm = F)

DB$sq_tot_t1 <- rowSums(DB[,c("T1_SQ48_001","T1_SQ48_002","T1_SQ48_003",
                             "T1_SQ48_004","T1_SQ48_005","T1_SQ48_006",
                             "T1_SQ48_007","T1_SQ48_008","T1_SQ48_010",
                             "T1_SQ48_011","T1_SQ48_013","T1_SQ48_014",
                             "T1_SQ48_016","T1_SQ48_017","T1_SQ48_019",
                             "T1_SQ48_021","T1_SQ48_022","T1_SQ48_023",
                             "T1_SQ48_024","T1_SQ48_025","T1_SQ48_026",
                             "T1_SQ48_027","T1_SQ48_028","T1_SQ48_031",
                             "T1_SQ48_032","T1_SQ48_033","T1_SQ48_036",
                             "T1_SQ48_038","T1_SQ48_039","T1_SQ48_040",
                             "T1_SQ48_041","T1_SQ48_043","T1_SQ48_044",
                             "T1_SQ48_045","T1_SQ48_046","T1_SQ48_047",
                             "T1_SQ48_048")], 
                        na.rm = F)

DB$sq_tot_t2 <- rowSums(DB[,c("T2_SQ48_001","T2_SQ48_002","T2_SQ48_003",
                             "T2_SQ48_004","T2_SQ48_005","T2_SQ48_006",
                             "T2_SQ48_007","T2_SQ48_008","T2_SQ48_010",
                             "T2_SQ48_011","T2_SQ48_013","T2_SQ48_014",
                             "T2_SQ48_016","T2_SQ48_017","T2_SQ48_019",
                             "T2_SQ48_021","T2_SQ48_022","T2_SQ48_023",
                             "T2_SQ48_024","T2_SQ48_025","T2_SQ48_026",
                             "T2_SQ48_027","T2_SQ48_028","T2_SQ48_031",
                             "T2_SQ48_032","T2_SQ48_033","T2_SQ48_036",
                             "T2_SQ48_038","T2_SQ48_039","T2_SQ48_040",
                             "T2_SQ48_041","T2_SQ48_043","T2_SQ48_044",
                             "T2_SQ48_045","T2_SQ48_046","T2_SQ48_047",
                             "T2_SQ48_048")], 
                       na.rm = F)

DB$sq_tot_t3 <- rowSums(DB[,c("T3_SQ48_001","T3_SQ48_002","T3_SQ48_003",
                             "T3_SQ48_004","T3_SQ48_005","T3_SQ48_006",
                             "T3_SQ48_007","T3_SQ48_008","T3_SQ48_010",
                             "T3_SQ48_011","T3_SQ48_013","T3_SQ48_014",
                             "T3_SQ48_016","T3_SQ48_017","T3_SQ48_019",
                             "T3_SQ48_021","T3_SQ48_022","T3_SQ48_023",
                             "T3_SQ48_024","T3_SQ48_025","T3_SQ48_026",
                             "T3_SQ48_027","T3_SQ48_028","T3_SQ48_031",
                             "T3_SQ48_032","T3_SQ48_033","T3_SQ48_036",
                             "T3_SQ48_038","T3_SQ48_039","T3_SQ48_040",
                             "T3_SQ48_041","T3_SQ48_043","T3_SQ48_044",
                             "T3_SQ48_045","T3_SQ48_046","T3_SQ48_047",
                             "T3_SQ48_048")], 
                       na.rm = F)

#standardized LPFS

DB$lpfs_scale_t1 = scale(DB$lpfs_tot_t1)
DB$lpfs_scale_t2 = scale(DB$lpfs_tot_t2)
DB$lpfs_scale_t3 = scale(DB$lpfs_tot_t3)

DB$lpfs_scale1 = as.numeric(DB$lpfs_scale_t1) 
DB$lpfs_scale2 = as.numeric(DB$lpfs_scale_t2)
DB$lpfs_scale3 = as.numeric(DB$lpfs_scale_t3)

#standardized OTL

DB$otl_scale_t1 = scale(DB$otl_tot_t1)
DB$otl_scale_t2 = scale(DB$otl_tot_t2)
DB$otl_scale_t3 = scale(DB$otl_tot_t3)

DB$otl_scale1 = as.numeric(DB$otl_scale_t1)
DB$otl_scale2 = as.numeric(DB$otl_scale_t2)
DB$otl_scale3 = as.numeric(DB$otl_scale_t3)

#standardized SDQ

DB$sdq_scale_t1 = scale(DB$sdq_tot_t1)
DB$sdq_scale_t2 = scale(DB$sdq_tot_t2)
DB$sdq_scale_t3 = scale(DB$sdq_tot_t3)

DB$sdq_scale1 = as.numeric(DB$sdq_scale_t1)
DB$sdq_scale2 = as.numeric(DB$sdq_scale_t2)
DB$sdq_scale3 = as.numeric(DB$sdq_scale_t3)

#standardized SQ

DB$sq_scale_t1 = scale(DB$sq_tot_t1)
DB$sq_scale_t2 = scale(DB$sq_tot_t2)
DB$sq_scale_t3 = scale(DB$sq_tot_t3)

DB$sq_scale1 = as.numeric(DB$sq_scale_t1)
DB$sq_scale2 = as.numeric(DB$sq_scale_t2)
DB$sq_scale3 = as.numeric(DB$sq_scale_t3)

#create a single column with SQ and SDQ standardized
#NB This is the "GP scale" in the manuscript

DB$distress_t1 = rowSums(DB[,c("sdq_scale1", "sq_scale1")], na.rm = T)
DB$distress_t2 = rowSums(DB[,c("sdq_scale2", "sq_scale2")], na.rm = T)
DB$distress_t3 = rowSums(DB[,c("sdq_scale3", "sq_scale3")], na.rm = T)

DB$distress_t1[DB$distress_t1 == '0'] <- '' 
DB$distress_t2[DB$distress_t2 == '0'] <- ''
DB$distress_t3[DB$distress_t3 == '0'] <- ''

DB$distress_t1 = as.numeric(DB$distress_t1)
DB$distress_t2 = as.numeric(DB$distress_t2)
DB$distress_t3 = as.numeric(DB$distress_t3)

################################################################################
## ASSESS INTERNAL CONSISTENTCY                                               ##
################################################################################

# SDQ T1

alpha_sdq_t1 <- psych::alpha(DB[,c("T1_SDQ_S11_17_002", "T1_SDQ_S11_17_003", 
                                      "T1_SDQ_S11_17_005", "T1_SDQ_S11_17_006", 
                                      "T1_SDQ_S11_17_007", "T1_SDQ_S11_17_008", 
                                      "T1_SDQ_S11_17_010", "T1_SDQ_S11_17_011", 
                                      "T1_SDQ_S11_17_012", "T1_SDQ_S11_17_013", 
                                      "T1_SDQ_S11_17_014", "T1_SDQ_S11_17_015", 
                                      "T1_SDQ_S11_17_016", "T1_SDQ_S11_17_018", 
                                      "T1_SDQ_S11_17_019", "T1_SDQ_S11_17_021", 
                                      "T1_SDQ_S11_17_022", "T1_SDQ_S11_17_023", 
                                      "T1_SDQ_S11_17_024", "T1_SDQ_S11_17_025")], 
                              check.keys = T)

summary(alpha_sdq_t1)

# SDQ T2 
#item T2_SDQ_S11_17_FU_002 and 003 has a negative correlation with total score 

alpha_sdq_t2 <- psych::alpha(DB[,c("T2_SDQ_S11_17_FU_002", "T2_SDQ_S11_17_FU_003", 
                                      "T2_SDQ_S11_17_FU_005", "T2_SDQ_S11_17_FU_006", 
                                      "T2_SDQ_S11_17_FU_007", "T2_SDQ_S11_17_FU_008",
                                      "T2_SDQ_S11_17_FU_010", "T2_SDQ_S11_17_FU_011", 
                                      "T2_SDQ_S11_17_FU_012","T2_SDQ_S11_17_FU_013", 
                                      "T2_SDQ_S11_17_FU_014", "T2_SDQ_S11_17_FU_015", 
                                      "T2_SDQ_S11_17_FU_016", "T2_SDQ_S11_17_FU_018", 
                                      "T2_SDQ_S11_17_FU_019", "T2_SDQ_S11_17_FU_021", 
                                      "T2_SDQ_S11_17_FU_022", "T2_SDQ_S11_17_FU_023", 
                                      "T2_SDQ_S11_17_FU_024", "T2_SDQ_S11_17_FU_025")], 
                              check.keys=TRUE)
summary(alpha_sdq_t2)

# SDQ T3

alpha_sdq_t3 <- psych::alpha(DB[,c("T3_SDQ_S11_17_FU_002", "T3_SDQ_S11_17_FU_003", 
                               "T3_SDQ_S11_17_FU_005", "T3_SDQ_S11_17_FU_006", 
                               "T3_SDQ_S11_17_FU_007", "T3_SDQ_S11_17_FU_008", 
                               "T3_SDQ_S11_17_FU_010", "T3_SDQ_S11_17_FU_011", 
                               "T3_SDQ_S11_17_FU_012", "T3_SDQ_S11_17_FU_013", 
                               "T3_SDQ_S11_17_FU_014", "T3_SDQ_S11_17_FU_015", 
                               "T3_SDQ_S11_17_FU_016", "T3_SDQ_S11_17_FU_018", 
                               "T3_SDQ_S11_17_FU_019", "T3_SDQ_S11_17_FU_021", 
                               "T3_SDQ_S11_17_FU_022", "T3_SDQ_S11_17_FU_023", 
                               "T3_SDQ_S11_17_FU_024", "T3_SDQ_S11_17_FU_025")], 
                       check.keys = TRUE)
summary(alpha_sdq_t3)

# LPFS T1

alpha_lpfs_t1 <- psych::alpha(DB[,c("T1_LPFS_001", "T1_LPFS_002", 
                                "T1_LPFS_003", "T1_LPFS_004", 
                                "T1_LPFS_005", "T1_LPFS_006", 
                                "T1_LPFS_007", "T1_LPFS_008", 
                                "T1_LPFS_009", "T1_LPFS_010", 
                                "T1_LPFS_011","T1_LPFS_012")])
summary(alpha_lpfs_t1)

# LPFS T2

alpha_lpfs_t2 <- psych::alpha(DB[,c("T2_LPFS_001", "T2_LPFS_002",
                                "T2_LPFS_003", "T2_LPFS_004", 
                                "T2_LPFS_005", "T2_LPFS_006", 
                                "T2_LPFS_007", "T2_LPFS_008", 
                                "T2_LPFS_009", "T2_LPFS_010", 
                                "T2_LPFS_011","T2_LPFS_012")])
summary(alpha_lpfs_t2)

# LPFS T3

alpha_lpfs_t3 <- psych::alpha(DB[,c("T3_LPFS_001", "T3_LPFS_002", 
                                "T3_LPFS_003", "T3_LPFS_004", 
                                "T3_LPFS_005", "T3_LPFS_006",
                                "T3_LPFS_007", "T3_LPFS_008", 
                                "T3_LPFS_009", "T3_LPFS_010", 
                                "T3_LPFS_011", "T3_LPFS_012")])
summary(alpha_lpfs_t3)

# OTL T1

alpha_otl_t1 <- psych::alpha(DB[,c("T1_OTL_J_KJP_001", "T1_OTL_J_KJP_002", 
                              "T1_OTL_J_KJP_003", "T1_OTL_J_KJP_004",
                              "T1_OTL_J_KJP_005", "T1_OTL_J_KJP_006",
                              "T1_OTL_J_KJP_007", "T1_OTL_J_KJP_008",
                              "T1_OTL_J_KJP_009", "T1_OTL_J_KJP_010",
                              "T1_OTL_J_KJP_011", "T1_OTL_J_KJP_012",
                              "T1_OTL_J_KJP_013", "T1_OTL_J_KJP_014",
                              "T1_OTL_J_KJP_015", "T1_OTL_J_KJP_016",
                              "T1_OTL_J_KJP_017", "T1_OTL_J_KJP_018",
                              "T1_OTL_J_KJP_019", "T1_OTL_J_KJP_020", 
                              "T1_OTL_J_KJP_021")])
summary(alpha_otl_t1)

# OTL T2

alpha_otl_t2 <- psych::alpha(DB[,c("T2_OTL_J_KJP_001", "T2_OTL_J_KJP_002", 
                              "T2_OTL_J_KJP_003", "T2_OTL_J_KJP_004", 
                              "T2_OTL_J_KJP_005", "T2_OTL_J_KJP_006", 
                              "T2_OTL_J_KJP_007", "T2_OTL_J_KJP_008", 
                              "T2_OTL_J_KJP_009", "T2_OTL_J_KJP_010", 
                              "T2_OTL_J_KJP_011", "T2_OTL_J_KJP_012", 
                              "T2_OTL_J_KJP_013", "T2_OTL_J_KJP_014", 
                              "T2_OTL_J_KJP_015", "T2_OTL_J_KJP_016", 
                              "T2_OTL_J_KJP_017", "T2_OTL_J_KJP_018", 
                              "T2_OTL_J_KJP_019", "T2_OTL_J_KJP_020", 
                              "T2_OTL_J_KJP_021")])
summary(alpha_otl_t2)

# OTL T3

alpha_otl_t3 <- psych::alpha(DB[,c("T3_OTL_J_KJP_001", "T3_OTL_J_KJP_002", 
                              "T3_OTL_J_KJP_003", "T3_OTL_J_KJP_004", 
                              "T3_OTL_J_KJP_005", "T3_OTL_J_KJP_006", 
                              "T3_OTL_J_KJP_007", "T3_OTL_J_KJP_008", 
                              "T3_OTL_J_KJP_009", "T3_OTL_J_KJP_010", 
                              "T3_OTL_J_KJP_011", "T3_OTL_J_KJP_012", 
                              "T3_OTL_J_KJP_013", "T3_OTL_J_KJP_014", 
                              "T3_OTL_J_KJP_015", "T3_OTL_J_KJP_016", 
                              "T3_OTL_J_KJP_017", "T3_OTL_J_KJP_018", 
                              "T3_OTL_J_KJP_019", "T3_OTL_J_KJP_020", 
                              "T3_OTL_J_KJP_021")])
summary(alpha_otl_t3)

# SQ T1

alpha_sq_t1 <- psych::alpha(DB[,c("T1_SQ48_001","T1_SQ48_002","T1_SQ48_003",
                             "T1_SQ48_004","T1_SQ48_005","T1_SQ48_006",
                             "T1_SQ48_007","T1_SQ48_008","T1_SQ48_010",
                             "T1_SQ48_011","T1_SQ48_013","T1_SQ48_014",
                             "T1_SQ48_016","T1_SQ48_017","T1_SQ48_019",
                             "T1_SQ48_021","T1_SQ48_022","T1_SQ48_023",
                             "T1_SQ48_024","T1_SQ48_025","T1_SQ48_026",
                             "T1_SQ48_027","T1_SQ48_028","T1_SQ48_031",
                             "T1_SQ48_032","T1_SQ48_033","T1_SQ48_036",
                             "T1_SQ48_038","T1_SQ48_039","T1_SQ48_040",
                             "T1_SQ48_041","T1_SQ48_043","T1_SQ48_044",
                             "T1_SQ48_045","T1_SQ48_046","T1_SQ48_047",
                             "T1_SQ48_048")], 
                             check.keys = T)

summary(alpha_sq_t1)

#SQ T2

alpha_sq_t2 <- psych::alpha(DB[,c("T2_SQ48_001","T2_SQ48_002","T2_SQ48_003",
                                  "T2_SQ48_004","T2_SQ48_005","T2_SQ48_006",
                                  "T2_SQ48_007","T2_SQ48_008","T2_SQ48_010",
                                  "T2_SQ48_011","T2_SQ48_013","T2_SQ48_014",
                                  "T2_SQ48_016","T2_SQ48_017","T2_SQ48_019",
                                  "T2_SQ48_021","T2_SQ48_022","T2_SQ48_023",
                                  "T2_SQ48_024","T2_SQ48_025","T2_SQ48_026",
                                  "T2_SQ48_027","T2_SQ48_028","T2_SQ48_031",
                                  "T2_SQ48_032","T2_SQ48_033","T2_SQ48_036",
                                  "T2_SQ48_038","T2_SQ48_039","T2_SQ48_040",
                                  "T2_SQ48_041","T2_SQ48_043","T2_SQ48_044",
                                  "T2_SQ48_045","T2_SQ48_046","T2_SQ48_047",
                                  "T2_SQ48_048")], 
                            check.keys = T)

summary(alpha_sq_t2)

#SQ T3

alpha_sq_t3 <- psych::alpha(DB[,c("T3_SQ48_001","T3_SQ48_002","T3_SQ48_003",
                                  "T3_SQ48_004","T3_SQ48_005","T3_SQ48_006",
                                  "T3_SQ48_007","T3_SQ48_008","T3_SQ48_010",
                                  "T3_SQ48_011","T3_SQ48_013","T3_SQ48_014",
                                  "T3_SQ48_016","T3_SQ48_017","T3_SQ48_019",
                                  "T3_SQ48_021","T3_SQ48_022","T3_SQ48_023",
                                  "T3_SQ48_024","T3_SQ48_025","T3_SQ48_026",
                                  "T3_SQ48_027","T3_SQ48_028","T3_SQ48_031",
                                  "T3_SQ48_032","T3_SQ48_033","T3_SQ48_036",
                                  "T3_SQ48_038","T3_SQ48_039","T3_SQ48_040",
                                  "T3_SQ48_041","T3_SQ48_043","T3_SQ48_044",
                                  "T3_SQ48_045","T3_SQ48_046","T3_SQ48_047",
                                  "T3_SQ48_048")], 
                            check.keys = T)

summary(alpha_sq_t3)


################################################################################
## DESCRIPTIVES                                                               ##
################################################################################

describe(DB[,c("leeftijd","geslacht","lpfs_tot_t1", "lpfs_tot_t2", 
                 "lpfs_tot_t3", "otl_tot_t1", "otl_tot_t2", "otl_tot_t3",
                  "distress_t1", "distress_t2", "distress_t3")])

#check normality + SD

stat.desc((DB[,c( "lpfs_tot_t1", "lpfs_tot_t2","lpfs_tot_t3", "otl_tot_t1", 
                  "otl_tot_t2", "otl_tot_t3","distress_t1", "distress_t2", 
                  "distress_t3")]),norm = T)

#correlations-------------------------------------------------------------------

print(corr.test(DB[,c("lpfs_scale1", "lpfs_scale2", "lpfs_scale3", 
                "otl_scale1", "otl_scale2", "otl_scale3", "distress_t1", 
                "distress_t2", "distress_t3", "leeftijd")],method = "pearson", 
          use = "complete.obs"))

################################################################################
## CLPM                                                                    ##
################################################################################

#initial CLPM with covariates and no constraints over time

CLPM_cov <- '
# Estimate the lagged effects between the observed variables.
lpfs_scale2 + otl_scale2 + distress_t2 ~ lpfs_scale1 + otl_scale1 + distress_t1
lpfs_scale3 + otl_scale3 + distress_t3 ~ lpfs_scale2 + otl_scale2 + distress_t2
  
# Estimate the covariance between the observed variables at the first wave. 
lpfs_scale1 ~~ otl_scale1 # Covariance
lpfs_scale1 ~~ distress_t1
otl_scale1 ~~ distress_t1
  
# Estimate the covariances between the residuals of the observed variables.
lpfs_scale2 ~~ otl_scale2
lpfs_scale2 ~~ distress_t2
otl_scale2 ~~ distress_t2
lpfs_scale3 ~~ otl_scale3
lpfs_scale3 ~~ distress_t3
otl_scale3 ~~ distress_t3

# Estimate the (residual) variance of the observed variables.
lpfs_scale1 ~~ lpfs_scale1 # Variances
otl_scale1 ~~ otl_scale1 
distress_t1 ~~ distress_t1
lpfs_scale2 ~~ lpfs_scale2 # Residual variances
otl_scale2 ~~ otl_scale2 
distress_t2 ~~ distress_t2
lpfs_scale3 ~~ lpfs_scale3 
otl_scale3 ~~ otl_scale3
distress_t3 ~~ distress_t3

lpfs_scale1 ~ leeftijd + geslacht
otl_scale1 ~leeftijd + geslacht
distress_t1 ~leeftijd + geslacht

lpfs_scale1 ~1
lpfs_scale2 ~1
lpfs_scale3 ~1
otl_scale1 ~1
otl_scale2 ~1
otl_scale3 ~1 
distress_t1 ~1
distress_t2 ~1
distress_t3 ~1
  
'
CLPM_cov_fit <- lavaan(CLPM_cov, data = DB, missing = "FIML", meanstructure = T, 
                   int.ov.free = T) 
summary(CLPM_cov_fit, fit.measures = T, standardized = T)

# CLPM with constraints over time and covariates
#add constraints one-by-one in the order of stability, cross-lagged, concurrent

#1. constrained stability paths

CLPM_cons_cov3 <- '

# Estimate the lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ a*lpfs_scale1 
lpfs_scale3 ~ a*lpfs_scale2  
otl_scale2 ~ b*otl_scale1 
otl_scale3 ~ b*otl_scale2 
distress_t2 ~ c*distress_t1
distress_t3 ~ c*distress_t2

# Estimate the cross-lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ otl_scale1
lpfs_scale2 ~ distress_t1
lpfs_scale3 ~ otl_scale2
lpfs_scale3 ~ distress_t2
otl_scale2 ~ lpfs_scale1
otl_scale2 ~ distress_t1
otl_scale3 ~ lpfs_scale2
otl_scale3 ~ distress_t2
distress_t2 ~ lpfs_scale1
distress_t2 ~ otl_scale1
distress_t3 ~ lpfs_scale2
distress_t3 ~ otl_scale2
  
# Estimate the covariance between the observed variables at the first wave.
#constrained
lpfs_scale1 ~~ otl_scale1 # Covariance
lpfs_scale1 ~~ distress_t1
otl_scale1 ~~ distress_t1
  
# Estimate the covariances between the residuals of the observed variables.
#constrained 
lpfs_scale2 ~~ otl_scale2
lpfs_scale2 ~~ distress_t2
otl_scale2 ~~ distress_t2
lpfs_scale3 ~~ otl_scale3
lpfs_scale3 ~~ distress_t3
otl_scale3 ~~ distress_t3

# Estimate the (residual) variance of the observed variables.
#constrained 
lpfs_scale1 ~~ lpfs_scale1 # Variances
otl_scale1 ~~ otl_scale1 
distress_t1 ~~ distress_t1
lpfs_scale2 ~~ vlpfs*lpfs_scale2 # Residual variances
otl_scale2 ~~ votl*otl_scale2 
distress_t2 ~~ vspsy*distress_t2
lpfs_scale3 ~~ vlpfs*lpfs_scale3 
otl_scale3 ~~ votl*otl_scale3
distress_t3 ~~ vspsy*distress_t3

lpfs_scale1 ~ leeftijd + geslacht
otl_scale1 ~leeftijd + geslacht
distress_t1 ~leeftijd + geslacht

lpfs_scale1 ~1
lpfs_scale2 ~1
lpfs_scale3 ~1
otl_scale1 ~1
otl_scale2 ~1
otl_scale3 ~1 
distress_t1 ~1
distress_t2 ~1
distress_t3 ~1
'
CLPM_cons_cov3_fit <- lavaan(CLPM_cons_cov3, data = DB, missing = "FIML", 
                             meanstructure = T, 
                            int.ov.free = T) 
summary(CLPM_cons_cov3_fit, fit.measures = T, standardized = T)

#comparison with baseline CLPM 

compare3 <- compareFit(CLPM_cov_fit, CLPM_cons_cov3_fit)
summary(compare3)

#2. constrained stability + cross-lagged

CLPM_cons_cov4 <- '
# Estimate the lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ a*lpfs_scale1 
lpfs_scale3 ~ a*lpfs_scale2  
otl_scale2 ~ b*otl_scale1 
otl_scale3 ~ b*otl_scale2 
distress_t2 ~ c*distress_t1
distress_t3 ~ c*distress_t2

# Estimate the cross-lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ d*otl_scale1
lpfs_scale2 ~ e*distress_t1
lpfs_scale3 ~ d*otl_scale2
lpfs_scale3 ~ e*distress_t2
otl_scale2 ~ f*lpfs_scale1
otl_scale2 ~ g*distress_t1
otl_scale3 ~ f*lpfs_scale2
otl_scale3 ~ g*distress_t2
distress_t2 ~ h*lpfs_scale1
distress_t2 ~ i*otl_scale1
distress_t3 ~ h*lpfs_scale2
distress_t3 ~ i*otl_scale2
  
# Estimate the covariance between the observed variables at the first wave.
#constrained
lpfs_scale1 ~~ otl_scale1 # Covariance
lpfs_scale1 ~~ distress_t1
otl_scale1 ~~ distress_t1
  
# Estimate the covariances between the residuals of the observed variables.
#constrained 
lpfs_scale2 ~~ otl_scale2
lpfs_scale2 ~~ distress_t2
otl_scale2 ~~ distress_t2
lpfs_scale3 ~~ otl_scale3
lpfs_scale3 ~~ distress_t3
otl_scale3 ~~ distress_t3

# Estimate the (residual) variance of the observed variables.
#constrained 
lpfs_scale1 ~~ lpfs_scale1 # Variances
otl_scale1 ~~ otl_scale1 
distress_t1 ~~ distress_t1
lpfs_scale2 ~~ vlpfs*lpfs_scale2 # Residual variances
otl_scale2 ~~ votl*otl_scale2 
distress_t2 ~~ vspsy*distress_t2
lpfs_scale3 ~~ vlpfs*lpfs_scale3 
otl_scale3 ~~ votl*otl_scale3
distress_t3 ~~ vspsy*distress_t3

lpfs_scale1 ~ leeftijd + geslacht
otl_scale1 ~leeftijd + geslacht
distress_t1 ~leeftijd + geslacht

lpfs_scale1 ~1
lpfs_scale2 ~1
lpfs_scale3 ~1
otl_scale1 ~1
otl_scale2 ~1
otl_scale3 ~1 
distress_t1 ~1
distress_t2 ~1
distress_t3 ~1
'
CLPM_cons_cov4_fit <- lavaan(CLPM_cons_cov4, data = DB, missing = "FIML", 
                            meanstructure = T, int.ov.free = T) 
summary(CLPM_cons_cov4_fit, fit.measures = T, standardized = T)

#compare with CLPM with constrained stability paths 

compare4 <- compareFit(CLPM_cons_cov3_fit, CLPM_cons_cov4_fit)
summary(compare4)

#3. constrained stability, cross-lagged, and concurrent paths 

CLPM_cons_cov <- '

# Estimate the lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ a*lpfs_scale1 
lpfs_scale3 ~ a*lpfs_scale2  
otl_scale2 ~ b*otl_scale1 
otl_scale3 ~ b*otl_scale2 
distress_t2 ~ c*distress_t1
distress_t3 ~ c*distress_t2

# Estimate the cross-lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ d*otl_scale1
lpfs_scale2 ~ e*distress_t1
lpfs_scale3 ~ d*otl_scale2
lpfs_scale3 ~ e*distress_t2
otl_scale2 ~ f*lpfs_scale1
otl_scale2 ~ g*distress_t1
otl_scale3 ~ f*lpfs_scale2
otl_scale3 ~ g*distress_t2
distress_t2 ~ h*lpfs_scale1
distress_t2 ~ i*otl_scale1
distress_t3 ~ h*lpfs_scale2
distress_t3 ~ i*otl_scale2
  
# Estimate the covariance between the observed variables at the first wave.
#constrained
lpfs_scale1 ~~ cov1*otl_scale1 # Covariance
lpfs_scale1 ~~ cov2*distress_t1
otl_scale1 ~~ cov3*distress_t1
  
# Estimate the covariances between the residuals of the observed variables.
#constrained 
lpfs_scale2 ~~ cov1*otl_scale2
lpfs_scale2 ~~ cov2*distress_t2
otl_scale2 ~~ cov3*distress_t2
lpfs_scale3 ~~ cov1*otl_scale3
lpfs_scale3 ~~ cov2*distress_t3
otl_scale3 ~~ cov3*distress_t3

# Estimate the (residual) variance of the observed variables.
#constrained 
lpfs_scale1 ~~ lpfs_scale1 # Variances
otl_scale1 ~~ otl_scale1 
distress_t1 ~~ distress_t1
lpfs_scale2 ~~ vlpfs*lpfs_scale2 # Residual variances
otl_scale2 ~~ votl*otl_scale2 
distress_t2 ~~ vspsy*distress_t2
lpfs_scale3 ~~ vlpfs*lpfs_scale3 
otl_scale3 ~~ votl*otl_scale3
distress_t3 ~~ vspsy*distress_t3

lpfs_scale1 ~ leeftijd + geslacht
otl_scale1 ~leeftijd + geslacht
distress_t1 ~leeftijd + geslacht

lpfs_scale1 ~1
lpfs_scale2 ~1
lpfs_scale3 ~1
otl_scale1 ~1
otl_scale2 ~1
otl_scale3 ~1 
distress_t1 ~1
distress_t2 ~1
distress_t3 ~1
'
CLPM_cons_cov_fit <- lavaan(CLPM_cons_cov, data = DB, missing = "FIML", 
                            meanstructure = T, int.ov.free = T) 
summary(CLPM_cons_cov_fit, fit.measures = T, standardized = T)

# compare with CLPM with constrained stability and autoregressive paths

compare <- compareFit(CLPM_cons_cov4_fit, CLPM_cons_cov_fit)
summary(compare)

#remove constraints concurrent paths at T1 

CLPM_cons_cov2 <- '

# Estimate the lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ a*lpfs_scale1 
lpfs_scale3 ~ a*lpfs_scale2  
otl_scale2 ~ b*otl_scale1 
otl_scale3 ~ b*otl_scale2 
distress_t2 ~ c*distress_t1
distress_t3 ~ c*distress_t2

# Estimate the cross-lagged effects between the observed variables.
#constrained 
lpfs_scale2 ~ d*otl_scale1
lpfs_scale2 ~ e*distress_t1
lpfs_scale3 ~ d*otl_scale2
lpfs_scale3 ~ e*distress_t2
otl_scale2 ~ f*lpfs_scale1
otl_scale2 ~ g*distress_t1
otl_scale3 ~ f*lpfs_scale2
otl_scale3 ~ g*distress_t2
distress_t2 ~ h*lpfs_scale1
distress_t2 ~ i*otl_scale1
distress_t3 ~ h*lpfs_scale2
distress_t3 ~ i*otl_scale2
  
# Estimate the covariance between the observed variables at the first wave. 
lpfs_scale1 ~~ otl_scale1 # Covariance
lpfs_scale1 ~~ distress_t1
otl_scale1 ~~ distress_t1
  
# Estimate the covariances between the residuals of the observed variables.
#constrained 
lpfs_scale2 ~~ cov1*otl_scale2
lpfs_scale2 ~~ cov2*distress_t2
otl_scale2 ~~ cov3*distress_t2
lpfs_scale3 ~~ cov1*otl_scale3
lpfs_scale3 ~~ cov2*distress_t3
otl_scale3 ~~ cov3*distress_t3

# Estimate the (residual) variance of the observed variables.
#constrained 
lpfs_scale1 ~~ lpfs_scale1 # Variances
otl_scale1 ~~ otl_scale1 
distress_t1 ~~ distress_t1
lpfs_scale2 ~~ vlpfs*lpfs_scale2 # Residual variances
otl_scale2 ~~ votl*otl_scale2 
distress_t2 ~~ vspsy*distress_t2
lpfs_scale3 ~~ vlpfs*lpfs_scale3 
otl_scale3 ~~ votl*otl_scale3
distress_t3 ~~ vspsy*distress_t3

lpfs_scale1 ~ leeftijd + geslacht
otl_scale1 ~leeftijd + geslacht
distress_t1 ~leeftijd + geslacht

lpfs_scale1 ~1
lpfs_scale2 ~1
lpfs_scale3 ~1
otl_scale1 ~1
otl_scale2 ~1
otl_scale3 ~1 
distress_t1 ~1
distress_t2 ~1
distress_t3 ~1
'
CLPM_cons_cov2_fit <- lavaan(CLPM_cons_cov2, data = DB, missing = "FIML", 
                            meanstructure = T, int.ov.free = T) 
summary(CLPM_cons_cov2_fit, fit.measures = T, standardized = T)


#compare with CLPM with constrained stability and concurrent paths 

compare2 <- compareFit(CLPM_cons_cov4_fit, CLPM_cons_cov2_fit)
summary(compare2)

