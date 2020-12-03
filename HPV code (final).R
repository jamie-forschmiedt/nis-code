library(survey)
library(ggplot2)

v18 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc18.csv", header=T)
v17 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc17.csv", header=T)
v16 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc16.csv", header=T)
v15 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc15.csv", header=T)
v14 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc14.csv", header=T)
v13 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc13june.csv", header=T)
v12 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc12redo.csv", header=T)
v11 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc11hpv2.csv", header=T)

v10 <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Data Files/Vacc10rhpv.csv", header=T)
ESTIAPT10Flevels=c(1,10,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPT10Flabels=c("CT", "NY-REST OF STATE", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI",
                   "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV",
                   "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
v10n <- as.data.frame(cbind(ESTIAPT10Flevels, ESTIAPT10Flabels))
v10 <- merge(v10, v10n, by.x = "ESTIAPT10", by.y = "ESTIAPT10Flabels", all.x=T)         


#********************************************************************#
##******************************************************************##
###********* INSURANCE VARIABLES **********************************###
######################################################################
######################################################################
v10$COMBO_INS <- ifelse(v10$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v10$TIS_INS_2=="YES" | v10$TIS_INS_3=="YES" | v10$TIS_INS_3A=="YES" | v10$TIS_INS_4_5=="YES" |
                 v10$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v10$COMBO_INS[is.na(v10$COMBO_INS)] <- "UNKNOWN"

v11$COMBO_INS <- ifelse(v11$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v11$TIS_INS_2=="YES" | v11$TIS_INS_3=="YES" | v11$TIS_INS_3A=="YES" | v11$TIS_INS_4_5=="YES" |
                 v11$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v11$COMBO_INS[is.na(v11$COMBO_INS)] <- "UNKNOWN"

v12$COMBO_INS <- ifelse(v12$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v12$TIS_INS_2=="YES" | v12$TIS_INS_3=="YES" | v12$TIS_INS_3A=="YES" | v12$TIS_INS_4_5=="YES" |
                 v12$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v12$COMBO_INS[is.na(v12$COMBO_INS)] <- "UNKNOWN"

v13$COMBO_INS <- ifelse(v13$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v13$TIS_INS_2=="YES" | v13$TIS_INS_3=="YES" | v13$TIS_INS_3A=="YES" | v13$TIS_INS_4_5=="YES" |
                 v13$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v13$COMBO_INS[is.na(v13$COMBO_INS)] <- "UNKNOWN"

v14$COMBO_INS <- ifelse(v14$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v14$TIS_INS_2=="YES" | v14$TIS_INS_3=="YES" | v14$TIS_INS_3A=="YES" | v14$TIS_INS_4_5=="YES" |
                 v14$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v14$COMBO_INS[is.na(v14$COMBO_INS)] <- "UNKNOWN"

v15$COMBO_INS <- ifelse(v15$TIS_INS_1=="YES", "PRIVATE", 
                 ifelse(v15$TIS_INS_2=="YES" | v15$TIS_INS_3=="YES" | v15$TIS_INS_3A=="YES" |
                 v15$TIS_INS_4_5=="YES" | v15$TIS_INS_6=="YES", "PUBLIC", "UNKNOWN"))
v15$COMBO_INS[is.na(v15$COMBO_INS)] <- "UNKNOWN"

v16$COMBO_INS <- ifelse(v16$INS_STAT_I=="PRIVATE INSURANCE", "PRIVATE", 
                 ifelse(v16$INS_STAT_I=="ANY MEDICAID" | v16$INS_STAT_I=="OTHER INSURANCE", "PUBLIC", "UNKNOWN"))  

v17$COMBO_INS <- ifelse(v17$INS_STAT2_I=="PRIVATE INSURANCE ONLY", "PRIVATE", 
                 ifelse(v17$INS_STAT2_I=="ANY MEDICAID" | v17$INS_STAT2_I=="OTHER INSURANCE", "PUBLIC", "UNKNOWN"))

v18$COMBO_INS <- ifelse(v18$INS_STAT2_I=="PRIVATE INSURANCE ONLY", "PRIVATE", 
                 ifelse(v18$INS_STAT2_I=="ANY MEDICAID" | v18$INS_STAT2_I=="OTHER INSURANCE", "PUBLIC", "UNKNOWN"))


################################################################
################################################################
## SETTING UP SURVEY WEIGHTS TO COMBINE DATASETS ###############
################################################################
################################################################

################################################################
## UNIQUE IDENTIFIERS: SEQNUMT --> YRSEQT #####################
################################################################
v10$YRSEQT <- as.integer(paste0("2010", v10$SEQNUMT))
v11$YRSEQT <- as.integer(paste0("2011", v11$SEQNUMT))
v12$YRSEQT <- as.integer(paste0("2012", v12$SEQNUMT))
v13$YRSEQT <- as.integer(paste0("2013", v13$SEQNUMT))
v14$YRSEQT <- as.integer(paste0("2014", v14$SEQNUMT))
v15$YRSEQT <- as.integer(paste0("2015", v15$SEQNUMT))
v16$YRSEQT <- as.integer(paste0("2016", v16$SEQNUMT))
v17$YRSEQT <- as.integer(paste0("2017", v17$SEQNUMT))
v18$YRSEQT <- as.integer(paste0("2018", v18$SEQNUMT))


###################################################################
## SETTING CDIAP AND STRATUM VARIABLES ##############################
#######################################################################
#2018:
v18$EST_AREA <- v18$STRATUM %% 1000 ## using mod
v18$CDIAP <- ifelse(v18$EST_AREA == 107 | v18$EST_AREA == 109, 51, v18$EST_AREA)

#2017:
v17$EST_AREA <- v17$STRATUM %% 1000
v17$CDIAP <- ifelse(v17$EST_AREA == 53 | v17$EST_AREA == 52 | v17$EST_AREA == 108, 51, v17$EST_AREA)

#2016:
v16$EST_AREA <- v16$STRATUM %% 1000
v16$CDIAP <- ifelse(v16$EST_AREA == 52 | v16$EST_AREA == 53, 51, ifelse(v16$EST_AREA == 106, NA, v16$EST_AREA))

#2015:
v15$EST_AREA <- v15$STRATUM %% 1000
v15$CDIAP <- ifelse(v15$EST_AREA == 53 | v15$EST_AREA == 107, 51, ifelse(v15$EST_AREA == 106, NA, v15$EST_AREA))


#2014:
v14$EST_AREA <- v14$STRATUM %% 1000
v14$CDIAP <- ifelse(v14$EST_AREA == 53, 51, ifelse(v14$EST_AREA == 106, NA, v14$EST_AREA))

#2013:
v13$EST_AREA <- v13$STRATUM %% 1000
v13$CDIAP <- v13$EST_AREA

#2012:
v12$EST_AREA <- v12$STRATUM %% 1000
v12$CDIAP <- v12$EST_AREA

#2011:
v11$EST_AREA <- v11$STRATUM_D %% 1000
v11$CDIAP <- ifelse(v11$EST_AREA == 52 | v11$EST_AREA == 53, 51, v11$EST_AREA)

#2010:
v10$CDIAP <- ifelse(v10$ESTIAPT10Flevels == 52 | v10$ESTIAPT10Flevels == 53, 51, v10$ESTIAPT10Flevels)

v11$STRATUMV <- v11$STRATUM_D
v12$STRATUMV <- v12$STRATUM
v13$STRATUMV <- v13$STRATUM
v14$STRATUMV <- v14$STRATUM
v15$STRATUMV <- v15$STRATUM
v16$STRATUMV <- v16$STRATUM
v17$STRATUMV <- v17$STRATUM
v18$STRATUMV <- v18$STRATUM


########################################################################
## SETTING WEIGHT VARIABLES #############################################
#########################################################################
v11$NEWWT <- (v11$RDDWT)/4
v12$NEWWT <- (v12$RDDWT_D)/4
v13$NEWWT <- (v13$RDDWT_D)/4
v14$NEWWT <- (v14$RDDWT_D)/4

v15$NEWWT <- (v15$RDDWT_D)/4
v16$NEWWT <- (v16$RDDWT_D)/4
v17$NEWWT <- (v17$RDDWT_D)/4
v18$NEWWT <- (v18$RDDWT_C)/4

v10$ALLWT <- (v10$RDDWT)/9
v11$ALLWT <- (v11$RDDWT)/9
v12$ALLWT <- (v12$RDDWT_D)/9
v13$ALLWT <- (v13$RDDWT_D)/9
v14$ALLWT <- (v14$RDDWT_D)/9
v15$ALLWT <- (v15$RDDWT_D)/9
v16$ALLWT <- (v16$RDDWT_D)/9
v17$ALLWT <- (v17$RDDWT_D)/9
v18$ALLWT <- (v18$RDDWT_C)/9


#######################################################################
## SETTING HPVI_ANY VARIABLE FOR 2011-2014 #######################
########################################################################

v10$HPVI_ANY_TOT <- ifelse(v10$HPVI_ANY_REC=="YES" | v10$HPVI_ANY_SC=="YES", "YES", "NO/UNKNOWN")
v10$HPVI_ANY_TOT[is.na(v10$HPVI_ANY_TOT)] <- "NO/UNKNOWN"
v11$HPVI_ANY_TOT <- ifelse(v11$HPVI_ANY_REC=="YES" | v11$HPVI_ANY_SC=="YES", "YES", "NO/UNKNOWN")
v11$HPVI_ANY_TOT[is.na(v11$HPVI_ANY_TOT)] <- "NO/UNKNOWN"
v12$HPVI_ANY_TOT <- ifelse(v12$HPVI_ANY_REC=="YES" | v12$HPVI_ANY_SC=="YES", "YES", "NO/UNKNOWN")
v12$HPVI_ANY_TOT[is.na(v12$HPVI_ANY_TOT)] <- "NO/UNKNOWN"
v13$HPVI_ANY_TOT <- ifelse(v13$HPVI_ANY_REC=="YES" | v13$HPVI_ANY_SC=="YES", "YES", "NO/UNKNOWN")
v13$HPVI_ANY_TOT[is.na(v13$HPVI_ANY_TOT)] <- "NO/UNKNOWN"
v14$HPVI_ANY_TOT <- ifelse(v14$HPVI_ANY_REC=="YES", "YES", "NO/UNKNOWN")
v15$HPVI_ANY_TOT <- ifelse(v15$HPVI_ANY=="YES", "YES", "NO/UNKNOWN")
v16$HPVI_ANY_TOT <- ifelse(v16$HPVI_ANY=="YES", "YES", "NO/UNKNOWN")
v17$HPVI_ANY_TOT <- ifelse(v17$HPVI_ANY=="YES", "YES", "NO/UNKNOWN")
v18$HPVI_ANY_TOT <- ifelse(v18$HPVI_ANY=="YES", "YES", "NO/UNKNOWN")

##########################################################################
################## COMBINING DATASETS ###################################
##########################################################################

## grouping variables
v15v18Vars <- c("YRSEQT", "STRATUMV", "NEWWT", "SEX", "INCPOV1", "AGE", "HPVI_ANY", "HPVI_RECOM",
                "LANGUAGE", "YEAR", "CEN_REG", "RACE_K", "COMBO_INS") 

v11v14Vars <- c("YRSEQT", "STRATUMV", "NEWWT", "SEX", "INCPOV1", "AGE", "HPVI_RECOM",
                "LANGUAGE", "YEAR", "CEN_REG", "RACE_K", "HPVI_ANY_TOT", "HPVI_ANY_REC", "COMBO_INS")

vaccAllVars <- c("YRSEQT", "CDIAP", "ALLWT", "SEX", "HPVI_RECOM", "AGE", "HPVI_ANY_TOT", "INCPOV1", "LANGUAGE", 
                 "CEN_REG", "RACE_K", "COMBO_INS", "HPVI_REAS_1", "HPVI_REAS_2", "HPVI_REAS_3", "HPVI_REAS_5",
                 "HPVI_REAS_6", "HPVI_REAS_9", "HPVI_REAS_10", "HPVI_REAS_11", "HPVI_REAS_12", "HPVI_REAS_13",
                 "HPVI_REAS_14", "HPVI_REAS_15", "HPVI_REAS_16", "HPVI_REAS_17", "HPVI_REAS_18", "HPVI_REAS_19",
                 "HPVI_REAS_20", "HPVI_REAS_21", "HPVI_REAS_22", "HPVI_REAS_23", "HPVI_REAS_24", "HPVI_REAS_25",
                 "HPVI_REAS_26", "HPVI_REAS_27", "HPVI_REAS_28", "HPVI_REAS_29")


## subsetting the data for each year to only include the relevant variables
v14grouped <- v14[v11v14Vars]
v13grouped <- v13[v11v14Vars]
v12grouped <- v12[v11v14Vars]
v11grouped <- v11[v11v14Vars]

v15grouped <- v15[v15v18Vars]
v16grouped <- v16[v15v18Vars]
v17grouped <- v17[v15v18Vars]
v18grouped <- v18[v15v18Vars]

## subsetting data to combine all years 2010-2018
v10all <- v10[vaccAllVars]
v11all <- v11[vaccAllVars]
v12all <- v12[vaccAllVars]
v13all <- v13[vaccAllVars]
v14all <- v14[vaccAllVars]
v15all <- v15[vaccAllVars]
v16all <- v16[vaccAllVars]
v17all <- v17[vaccAllVars]
v18all <- v18[vaccAllVars]

## combining the datasets
v11to14 <- rbind(v11grouped, v12grouped, v13grouped, v14grouped)
v15to18 <- rbind(v15grouped, v16grouped, v17grouped, v18grouped)
vall <- rbind(v10all, v11all, v12all, v13all, v14all, v15all, v16all, v17all, v18all)
vallrecom <- vall[vall$HPVI_RECOM=="YES",]

##############################################################
## IMPLEMENTING THE SURVEY DESIGNS ###########################
#################################################################
nis_all <-
  svydesign (
    id = ~YRSEQT,
    strata = ~CDIAP,
    weights = ~ALLWT,
    data = subset(vall, ALLWT > 0)
  )

nisall_recom <- ## all years, only includes teens who were recommended to receive the vaccine
  svydesign (
    id = ~YRSEQT,
    strata = ~CDIAP,
    weights = ~ALLWT,
    data = subset(vallrecom, ALLWT > 0)
  )

nis_15to18 <-
  svydesign(
    id = ~ YRSEQT,
    strata = ~STRATUMV,
    weights = ~NEWWT,
    data = subset(v15to18, NEWWT > 0)
  )

nis_11to14 <-
  svydesign (
    id = ~YRSEQT,
    strata = ~STRATUMV,
    weights = ~NEWWT,
    data = subset(v11to14, NEWWT > 0)
  )

nis18 <-
  svydesign (
    id = ~SEQNUMT,
    strata = ~STRATUM,
    weights = ~ RDDWT_C,
    data = subset(v18, RDDWT_C > 0)
  )

nis17 <-  #2017 svy design
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v17, RDDWT_D > 0)
  )

nis16 <- 
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v16, RDDWT_D > 0)
  )

nis15 <- 
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v15, RDDWT_D > 0)
  )

nis14 <- 
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v14, RDDWT_D > 0)
  )

nis13 <- 
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v13, RDDWT_D > 0)
  )

nis12 <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~ RDDWT_D,
    data = subset(v12, RDDWT_D > 0)
  )

nis11 <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~STRATUM_D,
    weights = ~ RDDWT_D,
    data = subset(v11, RDDWT_D > 0)
  )

nis10 <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ESTIAPT10,
    weights = ~ RDDWT,
    data = subset(v10, RDDWT > 0)
  )

nis18_male <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~STRATUM,
    weights = ~RDDWT_C,
    data = subset(v18[v18$SEX=="MALE",], RDDWT_C > 0)
  )

nis18_female <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_C,
    data = subset(v18[v18$SEX=="FEMALE",], RDDWT_C > 0)
  )

nis17_female <- #2017 svy design- female
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v17[v17$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis17_male <- #2017 svy design - male
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v17[v17$SEX=="MALE",], RDDWT_D > 0)
  )

nis16_female <-
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v16[v16$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis16_male <- 
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v16[v16$SEX=="MALE",], RDDWT_D > 0)
  )

nis15_female <-
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v15[v15$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis15_male <- 
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v15[v15$SEX=="MALE",], RDDWT_D > 0)
  )

nis14_female <-
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v14[v14$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis14_male <- 
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v14[v14$SEX=="MALE",], RDDWT_D > 0)
  )

nis13_female <-
  svydesign(
    id = ~ SEQNUMT ,
    strata = ~ STRATUM ,
    weights = ~ RDDWT_D ,
    data = subset(v13[v13$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis13_male <- 
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v13[v13$SEX=="MALE",], RDDWT_D > 0)
  )

nis12_female <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v12[v12$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis12_male <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~ STRATUM,
    weights = ~RDDWT_D,
    data = subset(v12[v12$SEX=="MALE",], RDDWT_D > 0)
  )

nis11_female <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~STRATUM_D,
    weights = ~ RDDWT_D,
    data = subset(v11[v11$SEX=="FEMALE",], RDDWT_D > 0)
  )

nis11_male <-
  svydesign (
    id = ~ SEQNUMT,
    strata = ~STRATUM_D,
    weights = ~ RDDWT_D,
    data = subset(v11[v11$SEX=="MALE",], RDDWT_D > 0)
  )

nis10_female <-
  svydesign (
    id = ~SEQNUMT,
    strata = ~ESTIAPT10,
    weights = ~RDDWT,
    data = subset(v10[v10$SEX=="FEMALE",], RDDWT > 0)
  )

nis10_male <-
  svydesign(
    id = ~ SEQNUMT,
    strata = ~ ESTIAPT10,
    weights = ~RDDWT,
    data = subset(v10[v10$SEX=="MALE",], RDDWT > 0)
  )


#################################################################
## RECOMMENDATIONS BY SEX, 2010-2018 #######################
#############################################################
# 2018
mn_recom_f18 <- svymean(~HPVI_RECOM, nis18_female, na.rm=T)
confint(mn_recom_f18)
mn_recom_m18 <- svymean(~HPVI_RECOM, nis18_male, na.rm=T)
confint(mn_recom_m18)
mn_recom_18 <- svymean(~HPVI_RECOM, nis18, na.rm=T)
confint(mn_recom_18)

# 2017
mn_recom_f17 <- svymean(~HPVI_RECOM, nis17_female, na.rm=T)
confint(mn_recom_f17)
mn_recom_m17 <- svymean(~HPVI_RECOM, nis17_male, na.rm=T)
confint(mn_recom_m17)
mn_recom_17 <- svymean(~HPVI_RECOM, nis17, na.rm=T)
confint(mn_recom_17)

# 2016
mn_recom_m16 <- svymean(~HPVI_RECOM, nis16_male, na.rm=T)
confint(mn_recom_m16)
mn_recom_f16 <- svymean(~HPVI_RECOM, nis16_female, na.rm=T)
confint(mn_recom_f16)
mn_recom_16 <- svymean(~HPVI_RECOM, nis16, na.rm=T)
confint(mn_recom_16)

# 2015
mn_recom_m15 <- svymean(~HPVI_RECOM, nis15_male, na.rm=T)
confint(mn_recom_m15)
mn_recom_f15 <- svymean(~HPVI_RECOM, nis15_female, na.rm=T)
confint(mn_recom_f15)
mn_recom_15 <- svymean(~HPVI_RECOM, nis15, na.rm=T)
confint(mn_recom_15)

# 2014
mn_recom_m14 <- svymean(~HPVI_RECOM, nis14_male, na.rm=T)
confint(mn_recom_m14)
mn_recom_f14 <- svymean(~HPVI_RECOM, nis14_female, na.rm=T)
confint(mn_recom_f14)
mn_recom_14 <- svymean(~HPVI_RECOM, nis14, na.rm=T)
confint(mn_recom_14)

# 2013
mn_recom_m13 <- svymean(~HPVI_RECOM, nis13_male, na.rm=T)
confint(mn_recom_m13)
mn_recom_f13 <- svymean(~HPVI_RECOM, nis13_female, na.rm=T)
confint(mn_recom_f13)
mn_recom_13 <- svymean(~HPVI_RECOM, nis13, na.rm=T)
confint(mn_recom_13)

# 2012  
mn_recom_m12 <- svymean(~HPVI_RECOM, nis12_male, na.rm=T)
confint(mn_recom_m12)
mn_recom_f12 <- svymean(~HPVI_RECOM, nis12_female, na.rm=T)
confint(mn_recom_f12)
mn_recom_12 <- svymean(~HPVI_RECOM, nis12, na.rm=T)
confint(mn_recom_12)

# 2011
mn_recom_m11 <- svymean(~HPVI_RECOM, nis11_male, na.rm=T)
confint(mn_recom_m11)
mn_recom_f11 <- svymean(~HPVI_RECOM, nis11_female, na.rm=T)
confint(mn_recom_f11)
mn_recom_11 <- svymean(~HPVI_RECOM, nis11, na.rm=T)
confint(mn_recom_11)

# 2010
mn_recom_m10 <- svymean(~HPVI_RECOM, nis10_male, na.rm=T)
confint(mn_recom_m10)
mn_recom_f10 <- svymean(~HPVI_RECOM, nis10_female, na.rm=T)
confint(mn_recom_f10)
mn_recom_10 <- svymean(~HPVI_RECOM, nis10, na.rm=T)
confint(mn_recom_10)


#######################################################################
########################################################################
############ DEMOGRAPHICS #############################################
#######################################################################
#########################################################################

## Overall ##
nrow(vall)
table(vall$SEX)
table(vall$AGE)
table(vall$CEN_REG)
table(vall$RACE_K)
table(vall$LANGUAGE)
table(vall$INCPOV1)
table(vall$COMBO_INS)

svytotal(~SEX, nis_all, na.rm=T)
svytotal(~as.factor(AGE), nis_all, na.rm=T)
svytotal(~CEN_REG, nis_all, na.rm=T)
svytotal(~RACE_K, nis_all, na.rm=T)
svytotal(~LANGUAGE, nis_all, na.rm=T)
svytotal(~INCPOV1, nis_all, na.rm=T)
svytotal(~COMBO_INS, nis_all, na.rm=T)

svymean(~HPVI_ANY_TOT, nis_all, na.rm=T)
confint(svymean(~HPVI_ANY_TOT, nis_all, na.rm=T))
svyby(~HPVI_ANY_TOT, ~SEX, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~SEX, nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~as.factor(AGE), nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~as.factor(AGE), nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~CEN_REG, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~CEN_REG, nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~RACE_K, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~RACE_K, nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~LANGUAGE, nis_all, svymean, na.rm=T) 
confint(svyby(~HPVI_ANY_TOT, ~LANGUAGE, nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~INCPOV1, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~INCPOV1, nis_all, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~COMBO_INS, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~COMBO_INS, nis_all, svymean, na.rm=T))

svymean(~HPVI_RECOM, nis_all, na.rm=T)
confint(svymean(~HPVI_RECOM, nis_all, na.rm=T))
svyby(~HPVI_RECOM, ~SEX, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~SEX, nis_all, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~as.factor(AGE), nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~as.factor(AGE), nis_all, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~CEN_REG, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis_all, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~RACE_K, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~RACE_K, nis_all, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~LANGUAGE, nis_all, svymean, na.rm=T) 
confint(svyby(~HPVI_RECOM, ~LANGUAGE, nis_all, svymean, na.rm=T)) 
svyby(~HPVI_RECOM, ~INCPOV1, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis_all, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~COMBO_INS, nis_all, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis_all, svymean, na.rm=T))

svymean(~HPVI_ANY_TOT, subset(nis_all, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis_all, HPVI_RECOM=="YES"), na.rm=T))
svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_all, HPVI_RECOM=="YES"), svymean, na.rm=T))

## 2015-2018 ##
nrow(v15to18)
table(v15to18$SEX)
table(v15to18$AGE)
table(v15to18$CEN_REG)
table(v15to18$RACE_K)
table(v15to18$LANGUAGE)
table(v15to18$INCPOV1)
table(v15to18$COMBO_INS)

svytotal(~SEX, nis_15to18, na.rm=T)
svytotal(~CEN_REG, nis_15to18, na.rm=T)
svytotal(~INCPOV1, nis_15to18, na.rm=T)
svytotal(~as.factor(AGE), nis_15to18, na.rm=T) #just gives a total number instead of a count for each age
svytotal(~LANGUAGE, nis_15to18, na.rm=T)
svytotal(~RACE_K, nis_15to18, na.rm=T)
svytotal(~COMBO_INS, nis_15to18, na.rm=T)

svymean(~HPVI_ANY, nis_15to18, na.rm=T)
confint(svymean(~HPVI_ANY, nis_15to18, na.rm=T))
svyby(~HPVI_ANY, ~SEX, nis_15to18, svymean)
confint(svyby(~HPVI_ANY, ~SEX, nis_15to18, svymean))
svyby(~HPVI_ANY, ~CEN_REG, nis_15to18, svymean)
confint(svyby(~HPVI_ANY, ~CEN_REG, nis_15to18, svymean))
svyby(~HPVI_ANY, ~INCPOV1, nis_15to18, svymean)
confint(svyby(~HPVI_ANY, ~INCPOV1, nis_15to18, svymean))
svyby(~HPVI_ANY, ~as.factor(AGE), nis_15to18, svymean)
confint(svyby(~HPVI_ANY, ~as.factor(AGE), nis_15to18, svymean))
vacc_language <- svyby(~HPVI_ANY, ~LANGUAGE, nis_15to18, svymean)
confint(vacc_language)
svyby(~HPVI_ANY, ~COMBO_INS, nis_15to18, svymean)
confint(svyby(~HPVI_ANY, ~COMBO_INS, nis_15to18, svymean))
vacc_race <- svyby(~HPVI_ANY, ~RACE_K, nis_15to18, svymean)
confint(vacc_race)

## p values for recommendation vs other variables
svychisq(~HPVI_RECOM+SEX, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+AGE, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+CEN_REG, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+RACE_K, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+LANGUAGE, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+INCPOV1, nis_15to18, statistic="Chisq")
svychisq(~HPVI_RECOM+COMBO_INS, nis_15to18, statistic="Chisq")

svychisq(~HPVI_ANY+SEX, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+AGE, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+CEN_REG, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+RACE_K, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+LANGUAGE, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+INCPOV1, subset(nis_15to18, HPVI_RECOM=="YES"))
svychisq(~HPVI_ANY+COMBO_INS, subset(nis_15to18, HPVI_RECOM=="YES"))

## HPVI_ANY versus HPVI_RECOM

svymean(~HPVI_RECOM, nis_15to18, na.rm=T)
confint(svymean(~HPVI_RECOM, nis_15to18, na.rm=T))
svymean(~HPVI_ANY, subset(nis_15to18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY, subset(nis_15to18, HPVI_RECOM=="YES"), na.rm=T))

svyby(~HPVI_RECOM, ~SEX, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~SEX, nis_15to18, svymean, na.rm=T))

svyby(~HPVI_ANY, ~HPVI_RECOM, nis_15to18f, svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~HPVI_RECOM, nis_15to18f, svymean, na.rm=T))
svyby(~HPVI_ANY, ~HPVI_RECOM, nis_15to18m, svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~HPVI_RECOM, nis_15to18m, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~CEN_REG, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~CEN_REG, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~CEN_REG, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~CEN_REG, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~INCPOV1, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~INCPOV1, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~INCPOV1, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~INCPOV1, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_RECOM, ~AGE, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~AGE, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~AGE, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~AGE, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~AGE, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~AGE, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_RECOM, ~LANGUAGE, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~LANGUAGE, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~LANGUAGE, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~LANGUAGE, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~LANGUAGE, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~LANGUAGE, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_RECOM, ~RACE_K, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~RACE_K, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~RACE_K, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~RACE_K, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~RACE_K, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~RACE_K, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis_15to18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis_15to18, svymean, na.rm=T))
svyby(~HPVI_ANY, ~COMBO_INS, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~COMBO_INS, subset(nis_15to18, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY, ~COMBO_INS, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY, ~COMBO_INS, subset(nis_15to18, HPVI_RECOM=="NO"), svymean, na.rm=T))


### 2011-2014 ###
nrow(v11to14)
table(v11to14$SEX)
table(v11to14$AGE)
table(v11to14$CEN_REG)
table(v11to14$RACE_K)
table(v11to14$LANGUAGE)
table(v11to14$INCPOV1)
table(v11to14$COMBO_INS)

svytotal(~SEX, nis_11to14, na.rm=T)
svytotal(~CEN_REG, nis_11to14, na.rm=T)
svytotal(~INCPOV1, nis_11to14, na.rm=T)
svytotal(~as.factor(AGE), nis_11to14, na.rm=T)
svytotal(~LANGUAGE, nis_11to14, na.rm=T)
svytotal(~RACE_K, nis_11to14, na.rm=T)
svytotal(~COMBO_INS, nis_11to14, na.rm=T)

svymean(~HPVI_ANY_TOT, nis_11to14, na.rm=T)
confint(svymean(~HPVI_ANY_TOT, nis_11to14, na.rm=T))
svyby(~HPVI_ANY_TOT, ~SEX, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~SEX, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~CEN_REG, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~CEN_REG, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~INCPOV1, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~INCPOV1, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~as.factor(AGE), nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~as.factor(AGE), nis_11to14, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~LANGUAGE, nis_11to14, svymean, na.rm=T) 
confint(svyby(~HPVI_ANY_TOT, ~LANGUAGE, nis_11to14, svymean, na.rm=T)) 
svyby(~HPVI_ANY_TOT, ~RACE_K, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~RACE_K, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~COMBO_INS, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~COMBO_INS, nis_11to14, svymean, na.rm=T))

svymean(~HPVI_RECOM, nis_11to14, na.rm=T)
confint(svymean(~HPVI_RECOM, nis_11to14, na.rm=T))
svyby(~HPVI_RECOM, ~SEX, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~SEX, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~CEN_REG, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~INCPOV1, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~as.factor(AGE), nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~as.factor(AGE), nis_11to14, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~LANGUAGE, nis_11to14, svymean, na.rm=T) 
confint(svyby(~HPVI_RECOM, ~LANGUAGE, nis_11to14, svymean, na.rm=T)) 
svyby(~HPVI_RECOM, ~RACE_K, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~RACE_K, nis_11to14, svymean, na.rm=T))
svyby(~HPVI_RECOM, ~COMBO_INS, nis_11to14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis_11to14, svymean, na.rm=T))

svymean(~HPVI_ANY_TOT, subset(nis_11to14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis_11to14, HPVI_RECOM=="YES"), na.rm=T))

svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~SEX, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~CEN_REG, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~INCPOV1, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~AGE, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~LANGUAGE, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~RACE_K, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))

svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_11to14, HPVI_RECOM=="YES"), svymean, na.rm=T))
svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T)
confint(svyby(~HPVI_ANY_TOT, ~COMBO_INS, subset(nis_11to14, HPVI_RECOM=="NO"), svymean, na.rm=T))


######################################################
################# REASONS ###########################
######################################################

###### Just those who were recommended ###############

### 2018 ####
svytotal(~HPVI_REAS_1, subset(nis18, HPVI_RECOM=="YES"), na.rm=T) #4 Not Recommended
svytotal(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="YES"), na.rm=T) #3 Not Needed or Not Necessary
svytotal(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="YES"), na.rm=T) #5 Lack of Knowledge
svytotal(~HPVI_REAS_5, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_6, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_9, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_10, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="YES"), na.rm=T) #2 Safety Concerns/Side Effects
svytotal(~HPVI_REAS_12, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_13, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_14, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_15, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_16, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_17, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_18, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_19, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_20, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_21, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_22, subset(nis18, HPVI_RECOM=="YES"), na.rm=T) #1 Already Up To Date
svytotal(~HPVI_REAS_23, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_24, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_25, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_26, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_27, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_28, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
svytotal(~HPVI_REAS_29, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)

########## all years ############
svytotal(~HPVI_REAS_1, nisall_recom, na.rm=T) #5th Not Recommended

svymean(~HPVI_REAS_1, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))

svytotal(~HPVI_REAS_2, nisall_recom, na.rm=T) #2nd Not Needed/Not Necessary

svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))

svytotal(~HPVI_REAS_3, nisall_recom, na.rm=T) #4th Lack of Knowledge

svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))

svytotal(~HPVI_REAS_5, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_6, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_9, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_10, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_11, nisall_recom, na.rm=T) #1st Safety Concerns/Side Effects

svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))

svytotal(~HPVI_REAS_12, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_13, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_14, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_15, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_16, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_17, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_18, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_19, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_20, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_21, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_22, nisall_recom, na.rm=T) #3rd Already Up To Date

svymean(~HPVI_REAS_22, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_REAS_22, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_REAS_22, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))

svytotal(~HPVI_REAS_23, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_24, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_25, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_26, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_27, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_28, nisall_recom, na.rm=T)
svytotal(~HPVI_REAS_29, nisall_recom, na.rm=T)


##### just those who weren't recommended & did not initiate the series ####
svymean(~HPVI_REAS_1, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #2 Not Recommended
svymean(~HPVI_REAS_1, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_1, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_1, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_2, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #1 Not Needed or Not Necessary
svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_3, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #3 Lack of Knowledge
svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_5, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #4 Not Sexually Active
svymean(~HPVI_REAS_5, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_6, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_9, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_10, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)

svymean(~HPVI_REAS_11, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #5 Safety Concerns/Side Effects
svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_12, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_13, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_14, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_15, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_16, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_17, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_18, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_19, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_20, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_21, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_22, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) 
svymean(~HPVI_REAS_23, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_24, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_25, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_26, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_27, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_28, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_29, subset(nis_all, HPVI_RECOM=="NO" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)


### just those who were recommended & did not initiate the series ###
svymean(~HPVI_REAS_1, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)

svymean(~HPVI_REAS_2, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #2 Not Needed/necessary
svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_2, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_3, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #4 Lack of Knowledge
svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_3, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_5, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #3 Not Sexually Active
svymean(~HPVI_REAS_5, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_5, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_5, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_6, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #5 Not Appropriate Age
svymean(~HPVI_REAS_6, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_6, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_6, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_9, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_10, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)

svymean(~HPVI_REAS_11, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) #1 Safety concerns/side effects
svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis10, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis11, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis12, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis13, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis14, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis15, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis16, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis17, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))
svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
confint(svymean(~HPVI_REAS_11, subset(nis18, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T))

svymean(~HPVI_REAS_12, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_13, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_14, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_15, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_16, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_17, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_18, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_19, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_20, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_21, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_22, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T) 
svymean(~HPVI_REAS_23, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_24, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_25, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_26, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_27, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_28, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)
svymean(~HPVI_REAS_29, subset(nis_all, HPVI_RECOM=="YES" & HPVI_ANY_TOT=="NO/UNKNOWN"), na.rm=T)


### 5 biggest reasons seen overall: Not Recommended, Not Needed/Necessary, Lack of Knowledge,
### Not Sexually Active, Safety Concerns/Side Effects (variables: 1, 2, 3, 5, 11)
### Comparison among different subgroups

## graph of how safety concerns/side effects have changed over time for each income level
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis10, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis10, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis11, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis11, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis12, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis12, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis13, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis13, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis14, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis14, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis15, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis15, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis16, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis16, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis17, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis17, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis18, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_11, ~INCPOV1, subset(nis18, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))


## graph of how "not recommended" has changed over time by race among those who have not initiated the series
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis10, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis10, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis11, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis11, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis12, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis12, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis13, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis13, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis14, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis14, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis15, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis15, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis16, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis16, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis17, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis17, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))
svyby(~HPVI_REAS_1, ~RACE_K, subset(nis18, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T)
confint(svyby(~HPVI_REAS_1, ~RACE_K, subset(nis18, HPVI_ANY_TOT=="NO/UNKNOWN"), svymean, na.rm=T))


svytotal(~RACE_K, nis18) #weighted number of people of each race in the 2018 dataset
svytotal(~HPVI_REAS_1, subset(nis18, RACE_K=="WHITE ONLY"), na.rm=T) #how many white people reported not being recommended
svytotal(~HPVI_REAS_1, subset(nis18, RACE_K=="BLACK ONLY"), na.rm=T) #how many Black people reported not being recommended
svytotal(~HPVI_REAS_1, subset(nis18, RACE_K=="OTHER + MULTIPLE RACE"), na.rm=T) #how many other/multiple race people reported not being recommended
#divide the number of yes's by the total for each race to get the proportion
#but what is the confidence interval?

svymean(~HPVI_REAS_1, subset(nis18, RACE_K=="WHITE ONLY"), na.rm=T)
v18novacc <- v18[v18$HPVI_ANY_TOT=="NO/UNKNOWN"]
table(v18$HPVI_REAS_11, v18$RACE_K)
table(v18$RACE_K)

nrow(subset(v18, !is.na(v18$RACE_K)))
svytotal(~RACE_K, subset(nis18, RACE_K=="WHITE ONLY"))
svytotal(~HPVI_REAS_1, subset(nis18, RACE_K=="WHITE ONLY"), na.rm=T)

######### Overall #######################################
svytotal(~HPVI_REAS_1, nis_all, na.rm=T) ## 2nd Not Recommended
svymean(~HPVI_REAS_1, nis10, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis10, na.rm=T))
svymean(~HPVI_REAS_1, nis11, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis11, na.rm=T))
svymean(~HPVI_REAS_1, nis12, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis12, na.rm=T))
svymean(~HPVI_REAS_1, nis13, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis13, na.rm=T))
svymean(~HPVI_REAS_1, nis14, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis14, na.rm=T))
svymean(~HPVI_REAS_1, nis15, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis15, na.rm=T))
svymean(~HPVI_REAS_1, nis16, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis16, na.rm=T))
svymean(~HPVI_REAS_1, nis17, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis17, na.rm=T))
svymean(~HPVI_REAS_1, nis18, na.rm=T)
confint(svymean(~HPVI_REAS_1, nis18, na.rm=T))

svytotal(~HPVI_REAS_2, nis_all, na.rm=T) ## 1st Not Needed or Not Necessary
svymean(~HPVI_REAS_2, nis10, na.rm=T)
svymean(~HPVI_REAS_2, nis11, na.rm=T)
svymean(~HPVI_REAS_2, nis12, na.rm=T)
svymean(~HPVI_REAS_2, nis13, na.rm=T)
svymean(~HPVI_REAS_2, nis14, na.rm=T)
svymean(~HPVI_REAS_2, nis15, na.rm=T)
svymean(~HPVI_REAS_2, nis16, na.rm=T)
svymean(~HPVI_REAS_2, nis17, na.rm=T)
svymean(~HPVI_REAS_2, nis18, na.rm=T)

confint(svymean(~HPVI_REAS_2, nis10, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis11, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis12, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis13, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis14, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis15, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis16, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis17, na.rm=T))
confint(svymean(~HPVI_REAS_2, nis18, na.rm=T))

svytotal(~HPVI_REAS_3, nis_all, na.rm=T) ## 3rd Lack of Knowledge
svymean(~HPVI_REAS_3, nis10, na.rm=T)
svymean(~HPVI_REAS_3, nis11, na.rm=T)
svymean(~HPVI_REAS_3, nis12, na.rm=T)
svymean(~HPVI_REAS_3, nis13, na.rm=T)
svymean(~HPVI_REAS_3, nis14, na.rm=T)
svymean(~HPVI_REAS_3, nis15, na.rm=T)
svymean(~HPVI_REAS_3, nis16, na.rm=T)
svymean(~HPVI_REAS_3, nis17, na.rm=T)
svymean(~HPVI_REAS_3, nis18, na.rm=T)

confint(svymean(~HPVI_REAS_3, nis10, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis11, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis12, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis13, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis14, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis15, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis16, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis17, na.rm=T))
confint(svymean(~HPVI_REAS_3, nis18, na.rm=T))

svytotal(~HPVI_REAS_5, nis_all, na.rm=T) ## 5th Not Sexually Active
svymean(~HPVI_REAS_5, nis10, na.rm=T)
svymean(~HPVI_REAS_5, nis11, na.rm=T)
svymean(~HPVI_REAS_5, nis12, na.rm=T)
svymean(~HPVI_REAS_5, nis13, na.rm=T)
svymean(~HPVI_REAS_5, nis14, na.rm=T)
svymean(~HPVI_REAS_5, nis15, na.rm=T)
svymean(~HPVI_REAS_5, nis16, na.rm=T)
svymean(~HPVI_REAS_5, nis17, na.rm=T)
svymean(~HPVI_REAS_5, nis18, na.rm=T)

confint(svymean(~HPVI_REAS_5, nis10, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis11, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis12, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis13, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis14, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis15, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis16, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis17, na.rm=T))
confint(svymean(~HPVI_REAS_5, nis18, na.rm=T))

svytotal(~HPVI_REAS_6, nis_all, na.rm=T)

svytotal(~HPVI_REAS_9, nis_all, na.rm=T)
svytotal(~HPVI_REAS_10, nis_all, na.rm=T)

svytotal(~HPVI_REAS_11, nis_all, na.rm=T) ## 4th Safety Concerns/Side Effects
svymean(~HPVI_REAS_11, nis10, na.rm=T)
svymean(~HPVI_REAS_11, nis11, na.rm=T)
svymean(~HPVI_REAS_11, nis12, na.rm=T)
svymean(~HPVI_REAS_11, nis13, na.rm=T)
svymean(~HPVI_REAS_11, nis14, na.rm=T)
svymean(~HPVI_REAS_11, nis15, na.rm=T)
svymean(~HPVI_REAS_11, nis16, na.rm=T)
svymean(~HPVI_REAS_11, nis17, na.rm=T)
svymean(~HPVI_REAS_11, nis18, na.rm=T)

confint(svymean(~HPVI_REAS_11, nis10, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis11, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis12, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis13, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis14, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis15, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis16, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis17, na.rm=T))
confint(svymean(~HPVI_REAS_11, nis18, na.rm=T))

svytotal(~HPVI_REAS_12, nis_all, na.rm=T)
svytotal(~HPVI_REAS_13, nis_all, na.rm=T)
svytotal(~HPVI_REAS_14, nis_all, na.rm=T)
svytotal(~HPVI_REAS_15, nis_all, na.rm=T)
svytotal(~HPVI_REAS_16, nis_all, na.rm=T)
svytotal(~HPVI_REAS_17, nis_all, na.rm=T)
svytotal(~HPVI_REAS_18, nis_all, na.rm=T)
svytotal(~HPVI_REAS_19, nis_all, na.rm=T)
svytotal(~HPVI_REAS_20, nis_all, na.rm=T)
svytotal(~HPVI_REAS_21, nis_all, na.rm=T)
svytotal(~HPVI_REAS_22, nis_all, na.rm=T) 
svytotal(~HPVI_REAS_23, nis_all, na.rm=T)
svytotal(~HPVI_REAS_24, nis_all, na.rm=T)
svytotal(~HPVI_REAS_25, nis_all, na.rm=T)
svytotal(~HPVI_REAS_26, nis_all, na.rm=T)
svytotal(~HPVI_REAS_27, nis_all, na.rm=T)
svytotal(~HPVI_REAS_28, nis_all, na.rm=T)
svytotal(~HPVI_REAS_29, nis_all, na.rm=T)

#############################################
###### FIGURES #############################
############################################

################################################
## Recommendations by region each year ##
#################################################

svyby(~HPVI_RECOM, ~CEN_REG, nis18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis18, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis17, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis17, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis16, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis16, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis15, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis15, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis14, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis13, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis13, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis12, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis12, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis11, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis11, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis10, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis10, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis09, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis09, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~CEN_REG, nis08, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~CEN_REG, nis08, svymean, na.rm=T))



#################################################
# Recommendations by insurance type each year
##################################################
svyby(~HPVI_RECOM, ~COMBO_INS, nis18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis18, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis17, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis17, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis16, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis16, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis15, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis15, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis14, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis13, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis13, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis12, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis12, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis11, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis11, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~COMBO_INS, nis10, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~COMBO_INS, nis10, svymean, na.rm=T))


##############################################################
# Recommendations by income level each year #
#############################################################
svyby(~HPVI_RECOM, ~INCPOV1, nis18, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis18, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis17, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis17, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis16, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis16, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis15, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis15, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis14, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis14, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis13, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis13, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis12, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis12, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis11, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis11, svymean, na.rm=T))

svyby(~HPVI_RECOM, ~INCPOV1, nis10, svymean, na.rm=T)
confint(svyby(~HPVI_RECOM, ~INCPOV1, nis10, svymean, na.rm=T))


###################################################################
## % out of those recommended who received the vaccine each year ##
###################################################################
svymean(~HPVI_ANY, subset(nis18, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY, subset(nis18, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY, subset(nis17, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY, subset(nis17, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY, subset(nis16, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY, subset(nis16, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY, subset(nis15, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY, subset(nis15, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY_TOT, subset(nis14, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis14, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY_TOT, subset(nis13, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis13, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY_TOT, subset(nis12, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis12, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY_TOT, subset(nis11, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis11, HPVI_RECOM=="YES"), na.rm=T))
svymean(~HPVI_ANY_TOT, subset(nis10, HPVI_RECOM=="YES"), na.rm=T)
confint(svymean(~HPVI_ANY_TOT, subset(nis10, HPVI_RECOM=="YES"), na.rm=T))

##########################################################
##########################################################
########### GGPLOT #######################################
##########################################################

## bar chart of HPVI_RECOM for 2018
ggplot(v18, aes(x=HPVI_RECOM)) + geom_bar() + labs(title="# Recommended the HPV Vaccine in 2018")

## bar graph of main reasons over the years
mainReasons <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Main reasons by year 2.csv", header=T)
mainReasons <- data.frame(mainReasons)
ggplot(mainReasons, aes(x=Reason, y=Percentage, fill=factor(Year))) + geom_bar(position=position_dodge(), 
   stat="identity", color="black", width=0.8) + theme_bw() + 
   labs(title="Reasons for Not Receiving the HPV Vaccine, 2010-2018", x="Reason", fill="Year") + 
   theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
   axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.title=element_text(size=13),
   legend.text=element_text(size=12)) +
   geom_errorbar(aes(ymin=Percentage, ymax=upperCI), width=0.3, position=position_dodge(.8))
#+ coord_flip() 

## bar graph of main reasons among people who were recommended
mainReasonsRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Main reasons among those recommended by year.csv", header=T)
mainReasonsRecom <- data.frame(mainReasonsRecom)
ggplot(mainReasonsRecom, aes(x=Reason, y=Percentage, fill=factor(Year))) + geom_bar(position=position_dodge(), 
  stat="identity", color="black", width=0.8) + theme_bw() + 
  labs(title="Reasons for Not Receiving the HPV Vaccine Among Those Recommended, 2010-2018", x="Reason", fill="Year") + 
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.title=element_text(size=13),
  legend.text=element_text(size=12)) +
  geom_errorbar(aes(ymin=Percentage, ymax=upperCI), width=0.3, position=position_dodge(.8))

## bar graph of main reasons among those recommended who had not initiated the series
mainReasonsRecomNoInit <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Reasons among those recommended who did not initiate.csv", header=T)
mainReasonsRecomNoInit <- data.frame(mainReasonsRecomNoInit)
ggplot(mainReasonsRecomNoInit, aes(x=Reason, y=Percentage, fill=factor(Year))) + geom_bar(position=position_dodge(), 
  stat="identity", color="black", width=0.8) + theme_bw() + 
  labs(title="Reasons for Not Receiving the HPV Vaccine Among Those\n Recommended Who Did Not Initiate the Series, 2010-2018", x="Reason", fill="Year") + 
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.title=element_text(size=13),
  legend.text=element_text(size=12)) +
  geom_errorbar(aes(ymin=Percentage, ymax=upperCI), width=0.3, position=position_dodge(.8))

## bar graph of main reasons among those not recommended who had not initiated the series
mainReasonsNoRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Main reasons among those not recommended by year.csv", header=T)
mainReasonsNoRecom <- data.frame(mainReasonsNoRecom)
ggplot(mainReasonsNoRecom, aes(x=Reason, y=Percentage, fill=factor(Year))) + geom_bar(position=position_dodge(), 
  stat="identity", color="black", width=0.8) + theme_bw() + 
  labs(title="Reasons for Not Receiving the HPV Vaccine Among Those Not Recommended, 2010-2018", x="Reason", fill="Year") + 
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.title=element_text(size=13),
  legend.text=element_text(size=12)) +
  geom_errorbar(aes(ymin=Percentage, ymax=upperCI), width=0.3, position=position_dodge(.8))

## bar graph of % safety concerns over time among those who didn't initiate the series for each income level
safetyIncome <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Safety concerns over time by income.csv", header=T)
safetyIncome <- data.frame(safetyIncome)
ggplot(safetyIncome, aes(x=IncomeLevel, y=Percent, fill=factor(Year))) + 
  geom_bar(position=position_dodge(), stat="identity", color="black", width=0.8) + theme_bw() +
  labs(title="% With Safety Concerns By Income Level, 2010-2018", x="Income Level", fill="Year") +
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), legend.text=element_text(size=12)) + 
  geom_errorbar(aes(ymin=Percent, ymax=UpperCI), width=0.3, position=position_dodge(.8)) +
  scale_x_discrete(limits = c("Below Poverty", "Above Poverty <= 75K", "Above Poverty > 75K"))

## bar graph of % "not recommended" (reason) over time among those who didn't initiate the series by race
recomRace <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Not recommended over time by race.csv", header=T)
recomRace <- data.frame(recomRace)
ggplot(recomRace, aes(x=Race, y=Percent, fill=factor(Year))) +
  geom_bar(position=position_dodge(), stat="identity", color="black", width=0.8) + theme_bw() +
  labs(title="% With 'Not Recommended' As A Reason Not To \n Vaccinate By Race, 2010-2018", x="Race", fill="Year") +
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), legend.text=element_text(size=12)) +
  geom_errorbar(aes(ymin=Percent, ymax=UpperCI), width=0.3, position=position_dodge(.8)) 

## Plot of % recommended the HPV vaccine each year, by sex
sexRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Percent recommended for HPV Vaccine by sex 2.csv", header=T)
sexRecom <- data.frame(sexRecom)
ggplot(sexRecom, aes(x=Year, y=Recom, color=Sex)) + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=.1) +
  geom_line(size=1.25) + ylim(0, 100) +
  scale_color_discrete(name="") + labs(title="% Recommended by Sex, 2010-2018", x="Year", y="% Recommended") +
  theme_bw() + theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), 
  axis.title.y=element_text(size=14), axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
  legend.text=element_text(size=14))

## Plot of % recommended the HPV vaccine each year, by region
regionRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Percent recommended for HPV vaccine by region.csv", header=T)
regionRecom <- data.frame(regionRecom)
ggplot(regionRecom, aes(x=Year, y=Recom, color=Region)) + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=0.1) +
  geom_line(size=1.25) + scale_color_discrete(name="") + ylim(0, 100) +
  labs(title="% Recommended by Region, 2010-2018", x="Year", y="% Recommended") + theme_bw() +
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.text=element_text(size=14))

## Plot of % recommended the HPV vaccine each year, by income
incomeRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Percent recommended by income.csv", header=T)
incomeRecom <- data.frame(incomeRecom)
ggplot(incomeRecom, aes(x=Year, y=Recom, color=Level)) + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=0.1) +
  geom_line(size=1.25) + scale_color_discrete(name="") + ylim(0, 100) +
  labs(title="% Recommended by Income, 2010-2018", x="Year", y="% Recommended") + theme_bw() +
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.text=element_text(size=14))

## Plot of % recommended the HPV vaccine each year, by insurance type
insuranceRecom <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Percent recommended by insurance.csv", header=T)
insuranceRecom <- data.frame(insuranceRecom)
ggplot(insuranceRecom, aes(x=Year, y=Recom, color=insType)) + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=0.1) +
  geom_line(size=1.25) + scale_color_discrete(name="") + ylim(0, 100) +
  labs(title="% Recommended by Insurance Type, 2010-2018", x="Year", y="% Recommended") + theme_bw() + 
  theme(plot.title=element_text(size=18), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), legend.text=element_text(size=14))

## Plot of % who received the vaccine out of those recommended each year
recomReceived <- read.csv(file="C:/Users/jamie/Desktop/NIS Data/Percent recommended who received the vaccine.csv", header=T)
recomReceived <- data.frame(recomReceived)
ggplot(recomReceived, aes(x=Year, y=ReceivedVaccine)) + geom_errorbar(aes(ymin=lowerCI, ymax=upperCI), width=0.1) +
  geom_line(size=1.25) + ylim(0, 100) + labs(title="% Who Received the HPV Vaccine Out Of \n Those Recommended, 2010-2018", 
  x="Year", y="% Who Received the Vaccine") + theme_bw() + 
  theme(plot.title=element_text(size=18), axis.title.x = element_text(size=14), axis.title.y=element_text(size=14),
  axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
