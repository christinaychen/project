# data file too large to push to git - split into two halfs in split_data.R
student_data1 = read.csv("els_student_first_half.csv")
student_data2 = read.csv("els_student_second_half.csv")
els = rbind(student_data1, student_data2)
els_original=els


els <- els %>%
  mutate(TEST_COMP_QU = ifelse(BYTXCQU < 0, NA, BYTXCQU),
         TEST_COMP = ifelse(BYTXCSTD < 0, NA, BYTXCSTD)
  )


els <- els %>%
  mutate(SES_2011 = ifelse(F3SES < 0, NA, F3SES),
         SESQU_2011 = ifelse(F3SESQU < 0, NA, F3SESQU), SESQU_2011=factor(SESQU_2011, levels=c(1,2,3,4)))

els <- els %>%
  mutate(TEST_QU = ifelse(BYTXCQU < 0, NA, BYTXCQU), TEST_QU=factor(TEST_QU))

els <- els %>% mutate(
  ED_ATNMT = case_when(
    F3ATTAINMENT == 1 ~ "No GED",
    F3ATTAINMENT == 2 ~ "GED",
    F3ATTAINMENT ==3 | F3ATTAINMENT ==4 | F3ATTAINMENT ==5 ~ "Associates/\nSome Undergrad",
    F3ATTAINMENT ==6 ~ "Bachelors",
    F3ATTAINMENT ==7 | F3ATTAINMENT ==8~ "Master's/\nPost-Bacc. Cert.",
    F3ATTAINMENT ==10 ~ "PhD"
  )
)
els$ED_ATNMT = factor(els$ED_ATNMT, levels=c( "No GED", "GED", "Associates/\nSome Undergrad", "Bachelors", "Master's/\nPost-Bacc. Cert.", "PhD"))


# AI/AN = American Indian/Alaska Native
# API = Asian, Hawaii/Pacific Islander
els <- els %>% mutate(
  RACE = case_when(
    BYRACE == 1 | BYRACE ==6~ "AI/AN/Mixed",
    BYRACE ==2 ~ "API",
    BYRACE ==3 ~ "Black",
    BYRACE ==4 | BYRACE ==5 ~ "Hispanic",
    BYRACE ==7 ~ "White"
  )
)
els$RACE = factor(els$RACE, levels=c( "AI/AN/Mixed", "API", "Black", "Hispanic","White"))


# used base year data, if na, used f1 data
els <- els %>% mutate(
  BYFCOMP = na_if(BYFCOMP, BYFCOMP<0),
  FAM_COMP = case_when(
    BYFCOMP == 1 | BYFCOMP == 2 | BYFCOMP == 3 | BYFCOMP == 4 ~ "Two Parents",
    BYFCOMP ==5 | BYFCOMP == 6 | BYFCOMP ==7 | BYFCOMP == 8 | BYFCOMP == 9~ "Single/Absent Parent",
    F1FCOMP == 1 | F1FCOMP == 2 | F1FCOMP == 3 | F1FCOMP == 4~ "Two Parents",
    F1FCOMP ==5 | F1FCOMP == 6 | F1FCOMP ==7 | F1FCOMP == 8 | F1FCOMP == 9~ "Single/Absent Parent"
  )
)

els <- els %>% mutate(
  BYPARED = na_if(BYPARED, BYPARED<0),
  PT_ED = case_when(
    BYPARED == 1 | BYPARED == 2 ~ "Some HS/GED",
    BYPARED == 3 | BYPARED == 4| BYPARED == 5 ~ "Attend PS/Associate's",
    BYPARED == 6 ~ "Bachelors",
    BYPARED == 7 | BYPARED == 8 ~ "Advanced Degree"
  ),
  PT_ED_YEARS = case_when(
    BYPARED == 1 ~ 1,
    BYPARED == 2 ~ 4,
    BYPARED == 3 ~ 5,
    BYPARED == 4 ~ 6,
    BYPARED == 5 ~ 7,
    BYPARED == 6 ~ 8,
    BYPARED == 7 ~ 10,
    BYPARED == 8 ~ 12
  )
)
els$PT_ED = factor(els$PT_ED, levels=c("Some HS/GED", "Attend PS/Associate's","Bachelors", "Advanced Degree"))
els$PT_ED_YEARS = as.numeric(els$PT_ED_YEARS)


els <- els %>% mutate(
  SESQU = ifelse(BYSES1QU < 0, NA, BYSES1QU),
  SESQU = case_when(
    SESQU == 1 ~ "1st SES Quartile",
    SESQU == 2 ~ "2nd SES Quartile",
    SESQU == 3 ~ "3rd SES Quartile",
    SESQU == 4 ~ "4th SES Quartile"
  ),
  SESQU=factor(SESQU),
  SES = ifelse(BYSES1 <= -2, NA, BYSES1)
)

# els <- els %>% mutate(FAM_INCOME = factor(BYINCOME))

els <- els %>% mutate(
  PT_PUSH_1 = na_if(BYPARASP, BYPARASP<0),
  PT_PUSH = case_when(
    PT_PUSH_1 == 1 | PT_PUSH_1 ==2 ~ "Some HS/GED",
    PT_PUSH_1 == 3 | PT_PUSH_1 == 4 ~ "Attend PS/Associate's",
    PT_PUSH_1 == 5 ~ "Bachelor's",
    PT_PUSH_1 == 6 | PT_PUSH_1 == 7~ "Advanced Degree"
  ),
  PT_PUSH_YEARS = case_when(
    PT_PUSH_1 == 1 ~ 2,
    PT_PUSH_1 == 2 ~ 4,
    PT_PUSH_1 == 3 ~ 6,
    PT_PUSH_1 == 4 ~ 7,
    PT_PUSH_1 == 5 ~ 8,
    PT_PUSH_1 == 6 ~ 10,
    PT_PUSH_1 == 7 ~ 12
  )
)
els$PT_PUSH = factor(els$PT_PUSH, levels=c("Some HS/GED", "Any college (2 or 4 year)", "Bachelor's","Advanced Degree"))
els$PT_PUSH_YEARS = as.numeric(els$PT_PUSH_YEARS)


els <- els %>% mutate(
  COMP_HOME = ifelse(BYS84C < 0, NA, BYS84C),
  COMP_HOME=factor(COMP_HOME)
)

els <- els %>% mutate(
  HAS_INTERNET = ifelse(BYS84D < 0, NA, BYS84D),
  HAS_INTERNET=factor(HAS_INTERNET),
  
  COMP_AND_INTERNET = HAS_INTERNET == 1 & COMP_HOME == 1
)


els <- els %>% mutate(
  SCHOOL_TYPE=case_when(
    BYSCTRL == 1 ~ "Public",
    BYSCTRL == 2 ~ "Catholic",
    BYSCTRL == 3 ~ "Private"
  )
)

els <- els %>% mutate(
  SCHOOL_URBAN=case_when(
    BYURBAN == 1 ~ "Urban",
    BYURBAN == 2 ~ "Suburban",
    BYURBAN == 3 ~ "Rural"
  )
)

els <- els %>% mutate(
  DROPOUT_PREV_PGM = ifelse(F1A23 < 0, NA, F1A23),
  DROPOUT_PREV_PGM=factor(DROPOUT_PREV_PGM)
)

els <- els %>% mutate(
  PRG_PGM = ifelse(F1A21F < 0, NA, F1A21F),
  PRG_PGM=factor(PRG_PGM)
)

els <- els %>% mutate(
  VOCATIONAL_PGM = ifelse(F1A21A < 0, NA, F1A21A),
  VOCATIONAL_PGM=factor(VOCATIONAL_PGM)
)


els <- els %>% mutate(
  BYA50A_1 = ifelse(BYA50A<0, NA, BYA50A),
  BYA50B_1 = ifelse(BYA50B<0, NA, BYA50B),
  BYA50C_1 = ifelse(BYA50C<0, NA, BYA50C),
  BYA50D_1 = ifelse(BYA50D<0, NA, BYA50D),
  BYA50E_1 = ifelse(BYA50E<0, NA, BYA50E),
  BYA50F_1 = ifelse(BYA50F<0, NA, BYA50F),
  BYA50G_1 = ifelse(BYA50G<0, NA, BYA50G),
  BYA50H_1 = ifelse(BYA50H<0, NA, BYA50H),
  BYA50I_1 = ifelse( BYA50I<0,NA, BYA50I),
  BYA50J_1 = ifelse( BYA50J<0, NA, BYA50J),
  BYA50K_1 = ifelse(BYA50K<0, NA, BYA50K),
  LH_LACK_OF_SPACE = factor(case_when(
    BYA50E_1 <= 1 ~ "No",
    BYA50E_1 <= 4 ~ "Yes"
  ), levels=c("No",  "Yes")),
  LH_POOR_BUILDINGS = factor(case_when(
    BYA50A_1 <= 1 ~ "No",
    BYA50A_1 <= 4 ~ "Yes"
  ), levels=c("No",  "Yes")),
  LH_POOR_HEATING_AIR_LIGHT = factor(case_when(
    BYA50B_1 <= 1 ~ "No",
    # BYA50B_1 <= 2 ~ "Some",
    BYA50B_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  LH_LACK_TEXT = factor(case_when(
    BYA50G_1 <= 1 ~ "No",
    BYA50G_1 <= 4 ~ "Yes"
  ), levels=c("No",  "Yes")),
  LH_LACK_COMPUTERS = factor(case_when(
    BYA50H_1 <= 1 ~ "No",
    BYA50H_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  LH_LACK_TECH_FACILITIES = factor(case_when(
    BYA50K_1 <= 1 ~ "No",
    BYA50K_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  LH_POOR_LIB = factor(case_when(
    BYA50F_1 <= 1 ~ "No",
    BYA50F_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  LH_POOR_SCIENCE_LAB = factor(case_when(
    BYA50C_1 <= 1 ~ "No",
    BYA50C_1 <= 4 ~ "Yes"
  ), levels=c("No","Yes")),
  LH_POOR_ARTS_FACILITIES = factor(case_when(
    BYA50D_1 <= 1 ~ "No",
    BYA50D_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  LH_POOR_MULTIMEDIA = factor(case_when(
    BYA50I_1 <= 1 ~ "No",
    BYA50I_1 <= 4 ~ "Yes"
  ), levels=c("No", "Yes")),
  
  LH_POOR_FACILITIES = LH_POOR_ARTS_FACILITIES=="Yes" | LH_POOR_SCIENCE_LAB=="Yes" | LH_POOR_LIB == "Yes",
  LH_POOR_TECH = LH_LACK_TECH_FACILITIES == "Yes" | LH_POOR_MULTIMEDIA == "Yes" | LH_LACK_COMPUTERS == "Yes",
  LH_POOR_BUILDINGS_AIR = LH_POOR_BUILDINGS == "Yes" | LH_POOR_HEATING_AIR_LIGHT == "Yes"
  
  # LEARNING_HINDERED_POOR_CONDITIONS_SCORE = BYA50A_1 + BYA50B_1 + BYA50E_1,
  # LEARNING_HINDERED_LACK_SUPPLIES_SCORE = BYA50G_1,
  # LEARNING_HINDERED_POOR_TECH_SCORE = BYA50D_1 + BYA50I_1 + BYA50H_1,
  # LEARNING_HINDERED_LACK_SPACE_SCORE = BYA50E_1,
  # LEARNING_HINDERED_POOR_FACILITIES_SCORE = BYA50D_1+BYA50C_1+BYA50F_1
)

els <- els %>% mutate(
  FREE_LUNCH_PERC = factor(ifelse(F1SCFLP < 0, NA, F1SCFLP)),
  FREE_LUNCH_PERC_10 = factor(ifelse(BY10FLP < 0, NA, BY10FLP))
  
)


els <- els %>% mutate(
  SECURITY = factor(ifelse(BYA40A < 0, NA, BYA40A)),
  FREE_LUNCH_CATEGORIES = case_when(
    as.numeric(FREE_LUNCH_PERC) == 1 ~ "<=5%",
    as.numeric(FREE_LUNCH_PERC) <= 4 ~ "<=30%",
    as.numeric(FREE_LUNCH_PERC) <= 5 ~ "<=50%",
    as.numeric(FREE_LUNCH_PERC) <= 6 ~ "<=75%",
    as.numeric(FREE_LUNCH_PERC) <= 7 ~ ">75%",
    
    as.numeric(FREE_LUNCH_PERC_10) == 1 ~ "<=5%",
    as.numeric(FREE_LUNCH_PERC_10) <= 4 ~ "<=30%",
    as.numeric(FREE_LUNCH_PERC_10) <= 5 ~ "<=50%",
    as.numeric(FREE_LUNCH_PERC_10) <= 6 ~ "<=75%",
    as.numeric(FREE_LUNCH_PERC_10) <= 7 ~ ">75%"
    
  )
  
)

els$FREE_LUNCH_CATEGORIES = factor(els$FREE_LUNCH_CATEGORIES, levels = c("<=5%", "<=30%", "<=50%", "<=75%", ">75%"))

# assume negative value is 0
els <- els %>% mutate(
  BYA41A_1 = ifelse( BYA41A<0, 0, BYA41A),
  BYA41B_1 = ifelse( BYA41B<0, 0, BYA41B),
  BYA41C_1 = ifelse( BYA41C<0, 0, BYA41C),
  BYA41D_1 = ifelse( BYA41D<0, 0, BYA41D),
  BYA41E_1 = ifelse( BYA41E<0, 0, BYA41E),
  BYA41F_1 = ifelse( BYA41F<0, 0, BYA41F),
  BYA41G_1 = ifelse( BYA41G<0, 0, BYA41G),
  BYA41H_1 = ifelse( BYA41H<0, 0, BYA41H),
  BYA41I_1 = ifelse( BYA41I<0, 0, BYA41I),
  BYA41J_1 = ifelse( BYA41J<0, 0, BYA41J),
  BYA41K_1 = ifelse( BYA41K<0, 0, BYA41K),
  BYA41L_1 = ifelse( BYA41L<0, 0, BYA41L),
  BYA41M_1 = ifelse(BYA41M<0, 0, BYA41M),
  TEACHER_BASIC_TECH_ACCESS = factor(BYA41K_1 & BYA41L_1 & BYA41M_1),
  TEACHER_TECH_ACCESS = BYA41A_1+ BYA41B_1+BYA41C_1+BYA41D_1+BYA41E_1+BYA41F_1+BYA41G_1+BYA41H_1+BYA41I_1+BYA41J_1+2*BYA41K_1+2*BYA41L_1+ BYA41M_1
)

els <- els %>% mutate(
  BYA43A = ifelse(BYA43A < 0, NA, BYA43A),
  BYA43B = ifelse(BYA43B < 0, NA, BYA43B),
  BYA43C = ifelse(BYA43C < 0, NA, BYA43C),
  BYA43D = ifelse(BYA43D < 0, NA, BYA43D),
  BYA43E = ifelse(BYA43E < 0, NA, BYA43E),
  
  TEACHER_TRAINING_TECH= BYA43A+BYA43B+BYA43C+BYA43D+BYA43E
)


els <- els %>% mutate(
  GOOD_TEACHERS_HIGHER_PAY = ifelse(BYA28F < 0, NA, BYA28F),
  GOOD_TEACHERS_HIGHER_PAY= factor(GOOD_TEACHERS_HIGHER_PAY)
)

els <- els %>% mutate(
  TUTORING = ifelse(F1A17D < 0, NA, F1A17D),
  TUTORING = case_when(
    TUTORING == 1 ~ 0,
    TUTORING == 2 | TUTORING==3 ~ 1
  ),
  TUTORING= factor(TUTORING)
)

els <- els %>% mutate(
  LIBRARY_COMPUTER_LAB = ifelse(BYL03F < 0, NA, BYL03F),
  LIBRARY_COMPUTER_LAB= factor(LIBRARY_COMPUTER_LAB)
)

els <- els %>% mutate(
  HIGHEST_TEACHER_SALARY = ifelse(BYA26B < 0, NA, BYA26B),
  LOWEST_TEACHER_SALARY = ifelse(BYA26A < 0, NA, BYA26A),
  TEACHER_SALARY_DISPARITY = HIGHEST_TEACHER_SALARY - LOWEST_TEACHER_SALARY,
  LOWEST_TEACHER_SALARY_THOUSANDS = LOWEST_TEACHER_SALARY/1000
)
els$PERC_SOPH_COLLEGE_PREP = els$BYA14B

els <- els %>%
  mutate(
    DROPPED_OUT = case_when(
      F3EVERDO == 1 ~ TRUE,
      F3EVERDO == 0 ~ FALSE
    )
  )
els <- els %>%
  mutate(ED_ATNMT_BACHELORS = !(ED_ATNMT == 'No GED' | ED_ATNMT == 'GED' | ED_ATNMT == 'Associates/\nSome Undergrad')
  )

els <- els %>%
  mutate(
    SCHOOL_TYPE1 = SCHOOL_TYPE,
    SCHOOL_TYPE = case_when(
      SCHOOL_TYPE == "Private" | SCHOOL_TYPE =="Catholic" ~ "Private/Catholic",
      SCHOOL_TYPE == "Public" ~ "Public"
    )
  )

els <- els %>%
  mutate(FREE_LUNCH_MAJ = 
           !(FREE_LUNCH_CATEGORIES == "<=30%" |  FREE_LUNCH_CATEGORIES == "<=5%" | FREE_LUNCH_CATEGORIES == "<=50%")
  )

els <- els %>% mutate(
  ENGLISH_PUSH_1 = na_if(BYTE20, BYTE20<0),
  ENGLISH_PUSH_YEARS = case_when(
    ENGLISH_PUSH_1 == 1 ~ 2,
    ENGLISH_PUSH_1 == 2 ~ 4,
    ENGLISH_PUSH_1 == 3 ~ 6,
    ENGLISH_PUSH_1 == 4 ~ 7,
    ENGLISH_PUSH_1 == 5 ~ 8,
    ENGLISH_PUSH_1 == 6 ~ 10,
    ENGLISH_PUSH_1 == 7 ~ 12
  ),
  MATH_PUSH_1 = na_if(BYTM20, BYTM20<0),
  MATH_PUSH_YEARS = case_when(
    MATH_PUSH_1 == 1 ~ 2,
    MATH_PUSH_1 == 2 ~ 4,
    MATH_PUSH_1 == 3 ~ 6,
    MATH_PUSH_1 == 4 ~ 7,
    MATH_PUSH_1 == 5 ~ 8,
    MATH_PUSH_1 == 6 ~ 10,
    MATH_PUSH_1 == 7 ~ 12
  )
)


els_new <- els[,c("SCHOOL_TYPE", "RACE", "FAM_COMP", "SES", "PT_ED", "PT_PUSH", "COMP_AND_INTERNET",  "PERC_SOPH_COLLEGE_PREP", "LOWEST_TEACHER_SALARY_THOUSANDS", "TEST_COMP", "SESQU_2011", "DROPPED_OUT", "ED_ATNMT_BACHELORS", "PT_ED_YEARS", "PT_PUSH_YEARS", "LH_LACK_OF_SPACE", "LH_POOR_BUILDINGS", "LH_POOR_HEATING_AIR_LIGHT", "LH_LACK_TEXT", "LH_POOR_FACILITIES", "LH_POOR_TECH", "FREE_LUNCH_CATEGORIES", "FREE_LUNCH_MAJ", "MATH_PUSH_YEARS", "ENGLISH_PUSH_YEARS")]

els_for_eda <-els[,c("SCHOOL_TYPE", "RACE", "FAM_COMP", "SES", "PT_ED", "PT_PUSH", "COMP_AND_INTERNET",  "PERC_SOPH_COLLEGE_PREP", "LOWEST_TEACHER_SALARY_THOUSANDS", "TEST_COMP", "SESQU_2011", "DROPPED_OUT", "ED_ATNMT_BACHELORS", "PT_ED_YEARS", "PT_PUSH_YEARS", "LH_LACK_OF_SPACE", "LH_POOR_BUILDINGS", "LH_POOR_HEATING_AIR_LIGHT", "LH_LACK_TEXT", "LH_POOR_FACILITIES", "LH_POOR_TECH", "FREE_LUNCH_CATEGORIES", "FREE_LUNCH_MAJ", "MATH_PUSH_YEARS", "ENGLISH_PUSH_YEARS"
,"ED_ATNMT", "TEST_COMP_QU")]
write.csv(els_for_eda, "els_filtered.csv")

# for(var in c("SCHOOL_TYPE", "RACE", "FAM_COMP", "SES", "PT_ED", "PT_PUSH", "COMP_AND_INTERNET",  "PERC_SOPH_COLLEGE_PREP", "LOWEST_TEACHER_SALARY_THOUSANDS", "TEST_COMP", "SESQU_2011", "DROPPED_OUT", "ED_ATNMT_BACHELORS", "PT_ED_YEARS", "PT_PUSH_YEARS", "LH_LACK_OF_SPACE", "LH_POOR_BUILDINGS", "LH_POOR_HEATING_AIR_LIGHT", "LH_LACK_TEXT", "LH_POOR_FACILITIES", "LH_POOR_TECH", "FREE_LUNCH_CATEGORIES", "FREE_LUNCH_MAJ", "MATH_PUSH_YEARS", "ENGLISH_PUSH_YEARS")
# ) {
#   print(sum(is.na(els_new[,var])))
# }

# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
els_imputed <- mice(els_new, m=1, maxit = 50, seed = 500, method="pmm")


els_complete <- complete(els_imputed, 1)
# els_complete <- read.csv("els_imputed.csv")
write.csv(els_complete, "els_imputed.csv")

