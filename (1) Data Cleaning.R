# Final Project for 301-2
# Data cleaning R script
# John Lee

# Load packages
library(tidyverse)
library(forcats)
library(modelr)
library(haven)
library(skimr)

## Part 1: Import and clean the data ------------------------------------------------------------

# Set the random seed to 3
set.seed(3)

# Import the df from Stata 
complete_gss_df <- read_dta("data/unprocessed/GSS.dta") 

# Check the contents of the file 
head(complete_gss_df)

# Look at the var names 
names(complete_gss_df)

# Filter in the data of interest
new_gss_df <- complete_gss_df %>%
  select(
    # Remove redundant predictors
    -conrinc, -degree, -denom, -madeg, -padeg, -hrs1, -reg16, -natcity,
    # Remove predictors related to spouse, b/c this is not N/A to many respondents
    -sphrs2, -sphrs1, -spwrksta, -spevwork, -spwrkslf, -spocc10, -speduc, -spind10,
    # Remove irrelevant predictors
    -indus10, -occ10, -evwork, -famdif16,
    # Drop b/c the vars have too many missing values
    -courts, -finalter, -satfin, -happy, -hrs2, -wrkslf, -realrinc
  )

# Look up the current type of each var (after running it, comment out)
#new_gss_df %>% skim

# Ok, it looks like many of the vars have the wrong type (e.g., char instead of factor)
complete_gss_df %>% count(realrinc)


# Rename vars (to more intuitive names) and convert to the correct var type
# For some of the vars, I'm temporarily setting as numeric (for filtering purposes); I'll reset to factor later
new_gss_df <- new_gss_df %>% 
  mutate(
    age = as.numeric(age),
    weekly_relig_attend = as.factor(attend),
    ballot = as.factor(ballot),
    num_kids = as.numeric(childs),
    subj_classid_text = as.numeric(class_),
    ever_divorced = as.numeric(divorce),
    yrs_educ = as.numeric(educ),
    lwparents_at16 = as.factor(family16),
    fundamentalist = as.factor(fund),
    hh_size = as.numeric(hompop),
    mom_yrs_educ = as.numeric(maeduc),
    married = as.factor(marital),
    geomobile_at16 = as.numeric(mobile16),
    moresp_natforeignaid = as.numeric(nataid),
    moresp_natarms = as.numeric(natarms),
    moresp_natcrime = as.numeric(natcrime),
    moresp_natdrug = as.numeric(natdrug),
    moresp_nateduc = as.numeric(nateduc),
    moresp_natenvir = as.numeric(natenvir),
    moresp_natwelfare = as.numeric(natfare),
    moresp_nathealth = as.numeric(natheal),
    moresp_natrace = as.numeric(natrace),
    moresp_natspace = as.numeric(natspac),
    dad_yrs_educ = as.numeric(paeduc),
    strong_dem = as.numeric(partyid),
    pol_liberalism = as.numeric(polviews),
    race = as.factor(race),
    family_income = as.numeric(realinc),
    geo_region = as.factor(region),
    religion = as.factor(relig),
    rural_restype_at16 = as.numeric(res16),
    female = as.factor(sex),
    num_sibs = as.numeric(sibs),
    ft_job = as.factor(wrkstat)
  )

# Delete obs with missing values 
# The rule: for factor vars, if NA/missing, then delete; if DK and NA are combined in one cat, then delete
# For numeric vars, also delete the "don't know" responses
# Don't filter on ever_divorced (b/c the question will be NA for many Rs)
new_gss_df <- new_gss_df %>% 
  filter(
         between(age, 18, 89),
         weekly_relig_attend != 9,
         num_kids != 9,
         subj_classid_text != 0, 
         between(yrs_educ, 0, 20),
         lwparents_at16 != -1,
         fundamentalist != 9,
         between(hh_size, 1, 16),
         between(mom_yrs_educ, 0, 20),
         married != 9,
         between(geomobile_at16, 1, 8),
         between(moresp_natforeignaid, 1, 8),
         between(moresp_natarms, 1, 8),
         between(moresp_natcrime, 1, 8),
         between(moresp_natdrug, 1, 8),
         between(moresp_nateduc, 1, 8),
         between(moresp_natenvir, 1, 8),
         between(moresp_natwelfare, 1, 8),
         between(moresp_nathealth, 1, 8),
         between(moresp_natrace, 1, 8),
         between(moresp_natspace, 1, 8),
         between(dad_yrs_educ, 0, 20),
         between(strong_dem, 0, 6),
         between(pol_liberalism, 1, 7),
         family_income != 0,
         religion != 99,
         between(rural_restype_at16, 1, 8),
         between(num_sibs, 0, 68),
         ft_job != 9
  )

# Create custom functions to recode the values of the vars

gen_subjclass_text <- function(x){
  case_when(
    x == 1 ~ "Lower Class",
    x == 2 ~ "Working Class",
    x == 3 ~ "Middle Class",
    x == 4 ~ "Upper class",
    TRUE ~ "Other"
  )
}

recode_nat_spending <- function(x){
  case_when(
    x == 1 ~ 1,
    TRUE ~ 0
  )
}

gen_region_text <- function(x){
  case_when(
    x == 1 ~ "New England",
    x == 2 ~ "Middle Atlantic",
    x == 3 ~ "NE Central",
    x == 4 ~ "NW Central",
    x == 5 ~ "South Atlantic",
    x == 6 ~ "SE Central",
    x == 7 ~ "SW Central",
    x == 8 ~ "Mountain",
    x == 9 ~ "Pacific"
  )
}

gen_religion_text <- function(x){
  case_when(
    x == 1 ~ "Protestant",
    x == 2 ~	"Catholic",
    x == 3 ~	"Jewish",
    x == 6 ~	"Buddhism",
    x == 7 ~	"Hinduism",
    x == 8 ~	"Other Eastern",
    x == 9 ~	"Islam",
    x == 10 ~	"Orthodox-Christian",
    x == 11 ~	"Christian",
    x == 12 ~	"Native American",
    x == 13 ~	"Inter-nondenominational",
    TRUE ~ "None, Other, DK"
  )
}

gen_race_text <- function(x){
  case_when(
    x == 1 ~ "White",
    x == 2 ~ "Black",
    x == 3 ~ "Other"
  )
}

rv_code_7 <- function(x){
  case_when(
    x == 1 ~ 7, 
    x == 2 ~ 6, 
    x == 3 ~ 5, 
    x == 4 ~ 4,
    x == 5 ~ 3,
    x == 6 ~ 2,
    x == 7 ~ 1
  )
}

partyid_recode <- function(x){
  case_when(
    x == 0 ~ 7, 
    x == 1 ~ 6, 
    x == 2 ~ 5, 
    x == 3 ~ 4,
    x == 4 ~ 3,
    x == 5 ~ 2,
    x == 6 ~ 1
  )
}

# Recode the var values
new_gss_df <- new_gss_df %>% 
  mutate(
    weekly_relig_attend = as.factor(ifelse(weekly_relig_attend == 7 | weekly_relig_attend == 8, 1, 0)),
    subj_classid_text = as.factor(gen_subjclass_text(subj_classid_text)),
    ever_divorced = as.factor(ifelse(ever_divorced == 1, 1, 0)),
    lwparents_at16 = as.factor(ifelse(lwparents_at16 == 1, 1, 0)),
    fundamentalist = as.factor(ifelse(fundamentalist == 1, 1, 0)),
    married = as.factor(ifelse(married == 1, 1, 0)),
    # yes if R lived in a different state at age 16
    geomobile_at16 = as.factor(ifelse(geomobile_at16 == 3, 1, 0)),
    # Recode the nat spending priority vars: 1 if prefers more spending, 0 otherwise
    moresp_natforeignaid = as.factor(recode_nat_spending(moresp_natforeignaid)),
    moresp_natarms = as.factor(recode_nat_spending(moresp_natarms)),
    moresp_natcrime = as.factor(recode_nat_spending(moresp_natcrime)),
    moresp_natdrug = as.factor(recode_nat_spending(moresp_natdrug)),
    moresp_nateduc = as.factor(recode_nat_spending(moresp_nateduc)),
    moresp_natenvir = as.factor(recode_nat_spending(moresp_natenvir)),
    moresp_natwelfare = as.factor(recode_nat_spending(moresp_natwelfare)),
    moresp_nathealth = as.factor(recode_nat_spending(moresp_nathealth)),
    moresp_natrace = as.factor(recode_nat_spending(moresp_natrace)),
    moresp_natspace = as.factor(recode_nat_spending(moresp_natspace)),
    # Create a new var for black
    black = as.factor(ifelse(race == 2, 1, 0)),
    race_text = as.factor(gen_race_text(race)),
    female = as.factor(ifelse(female == 2, 1, 0)),
    ln_famincome = log(family_income),
    geo_region = as.factor(gen_region_text(geo_region)),
    religion = as.factor(gen_religion_text(religion)),
    rural_restype_at16 = as.factor(ifelse(between(rural_restype_at16, 1, 3), 1, 0)),
    ft_job = as.factor(ifelse(ft_job == 1, 1, 0)),
    pol_liberalism = rv_code_7(pol_liberalism),
    strong_dem = partyid_recode(strong_dem),
    # Recode year so that the starting year is coded as 1 (i.e., 1974 is coded as 1)
    year = as.numeric(year - 1973)
  ) 

# Set the ref cat for the factor vars
new_gss_df <- new_gss_df %>%   
  mutate(
    weekly_relig_attend = relevel(weekly_relig_attend, ref = "0"),
    subj_classid_text = relevel(subj_classid_text, ref = "Middle Class"),
    ever_divorced = relevel(ever_divorced, ref = "0"),
    lwparents_at16 = relevel(lwparents_at16, ref = "0"),
    fundamentalist = relevel(fundamentalist, ref = "0"),
    married = relevel(married, ref = "0"),
    geomobile_at16 = relevel(geomobile_at16, ref = "0"),
    moresp_natforeignaid = relevel(moresp_natforeignaid, ref = "0"),
    moresp_natarms = relevel(moresp_natarms, ref = "0"),
    moresp_natcrime = relevel(moresp_natcrime, ref = "0"),
    moresp_natdrug = relevel(moresp_natdrug, ref = "0"),
    moresp_nateduc = relevel(moresp_nateduc, ref = "0"),
    moresp_natenvir = relevel(moresp_natenvir, ref = "0"),
    moresp_natwelfare = relevel(moresp_natwelfare, ref = "0"),
    moresp_nathealth = relevel(moresp_nathealth, ref = "0"),
    moresp_natrace = relevel(moresp_natrace, ref = "0"),
    moresp_natspace = relevel(moresp_natspace, ref = "0"),
    black = relevel(black, ref = "0"),
    race_text = relevel(race_text, ref = "White"),
    geo_region = relevel(geo_region, ref = "New England"),
    religion = relevel(religion, ref = "None, Other, DK"),
    rural_restype_at16 = relevel(rural_restype_at16, ref = "0"),
    ft_job = relevel(ft_job, ref = "0")
  )

# Just filter in the final set of vars (i.e., with new names, recoded values)
final_gss_df <- new_gss_df %>%
  select(
    # DV
    strong_dem,
    
    # Year (time var)
    year,
    
    # Demographic 
    age, black, race_text, female, num_kids, num_sibs, hh_size,
    
    # Social, cultural
    religion, weekly_relig_attend, fundamentalist, ever_divorced, married, 
    
    # SES
    subj_classid_text, ln_famincome, ft_job, yrs_educ, dad_yrs_educ, mom_yrs_educ, 
    
    # General background 
    lwparents_at16, geomobile_at16, rural_restype_at16, geo_region,

    # Policy prefs
    moresp_natforeignaid, moresp_natarms, moresp_natcrime, moresp_natdrug, moresp_nateduc,
    moresp_natenvir, moresp_natwelfare, moresp_nathealth, moresp_natrace, moresp_natspace,
    pol_liberalism
  )

# Split the dataset into 4 parts
#(1) EDA: 25%
#(2) Model-building: 25%
#(3) Model validation: 25%
#(4) Model testing: 25%

# First, split the full dataset into two parts -- each of which will then be split again 
first_half_df <- final_gss_df %>% sample_frac(0.5)
second_half_df <- final_gss_df %>% setdiff(first_half_df)

# (1) Create the EDA dataset (25%)
EDA_df <- first_half_df %>% sample_frac(0.5)

# (2) Create the model-building dataset (25%)
modbuilding_set <- first_half_df %>% setdiff(EDA_df)

# (3) Create the validation set (25%)
validation_set <- second_half_df %>% sample_frac(0.5)

# (4) Create the test set (25%)
test_set <- second_half_df %>% setdiff(validation_set)

# Save all three subsets -------------------------

write_rds(EDA_df, "data/processed/EDA_df.rds")

write_rds(modbuilding_set, "data/processed/modbuilding_set.rds")

write_rds(validation_set, "data/processed/validation_set.rds")

write_rds(test_set, "data/processed/test_set.rds")

