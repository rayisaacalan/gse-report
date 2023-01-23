# Load packages required for analysis
library(tidyverse)
library(readxl)
library(ggplot2)

# Read in data from XLSX file from Garrett
raw_survey_data = read_xlsx(path = "survey-data.xlsx", 
                            skip = 1,
                            na = c("n/a", "N/A", "N/a", "NA", 
                                   "n/A", "na", "Not applicable",
                                   "Not applicable, or prefer not to share",
                                   "Prefer not to answer",
                                   "prefer not to answer"))
questions = read_xlsx(path = "survey-data.xlsx",
                      n_max = 1,
                      col_names = FALSE) %>% 
            unlist()
names(questions) = colnames(raw_survey_data)

# Function to transmute RACETH into DARS categories
# https://dars.tamu.edu/Data-and-Reports-(1)/miscellaneous/files/new-ethnicity-reporting-dars
# This is an imperfect function because this precise data wasn't collected
# but it should be a relatively close mapping
# White Only, Black Only + 2 or more/1 Black, Hispanic or Latino of any Race,
# Asian Only, American Indian Only, International, Unknown, 
# Native Hawaiian Only, 2 or more/excluding Black, Other/Erroneous
DARS_ethnicity_coding = function(string_vec, international) {
  output = rep("Other", length(string_vec))
  asian_log = str_detect(string_vec, "Asian|asian|filipino")
  black_log = str_detect(string_vec, "Black|African")
  hispanic_log = str_detect(string_vec, "Hispanic|Latin|latin")
  nativeamerican_log = str_detect(string_vec, "Native American")
  nativehawaiian_log = str_detect(string_vec, "Native Hawaiian")
  white_log = str_detect(string_vec, "White|white|Caucasian")
  
  output[is.na(international)] = "Unknown/Not Reported"
  output[international] = "International"
  output[hispanic_log & (output == "Other")] = "Hispanic or Latino of any Race"
  output[black_log & 
           ((asian_log + nativeamerican_log + 
               nativehawaiian_log + white_log) > 0) & 
           (output == "Other")] = "2 or more/1 Black"
  output[black_log & (output == "Other")] = "Black Only"
  output[is.na(string_vec)] = "Unknown/Not Reported"
  output[((asian_log + nativeamerican_log + 
             nativehawaiian_log + white_log) > 1) & 
           (output == "Other")] = "2 or more/excluding Black"
  output[white_log & (output == "Other")] = "White Only"
  output[asian_log & (output == "Other")] = "Asian Only"
  output[nativeamerican_log & (output == "Other")] = "American Indian Only"
  output[nativehawaiian_log & (output == "Other")] = "Native Hawaiian Only"
  return(output)
}


# Process the data into tidy format for analysis
# (tidy format: https://vita.had.co.nz/papers/tidy-data.pdf)
tidy_survey_data = raw_survey_data %>%
  # Break out FUNDSOURCE into individual features
  mutate(ASSTSHP = str_detect(FUNDSOURCE, "Assistantship"),
         FELLSHP = str_detect(FUNDSOURCE, "Fellowship"),
         GRANTS = str_detect(FUNDSOURCE, "Grants"),
         EXTRNL = str_detect(FUNDSOURCE, "External"),
         .after = INTNTL) %>%
# Since NETINC interpretation depends on whether funding is received from
# university, create feature for income outside UNIV source
# This isn't perfect because there could be for example internal fellowships
  mutate(EXTRNLFUNDED = FELLSHP | GRANTS | EXTRNL,
         .after = EXTRNL) %>%
# Break out NETINC into university and external income columns
  mutate(UNIVNETINC = (!EXTRNLFUNDED)*NETINC,
         EXTRNLINC = (EXTRNLFUNDED)*NETINC,
         .after = GURNT) %>%
# Break out SUPP1 into individual features
  mutate(UNIVSCHOLSHP1 = str_detect(SUPP1, "University"),
         FOODBANK1 = str_detect(SUPP1, "Food"),
         PERSONALREL1 = str_detect(SUPP1, "Family"),
         EXTORG1 = str_detect(SUPP1, c("Private|Religious|Charities")),
         LOANS1 = str_detect(SUPP1, c("Loans|loans")),
         EXTWORK1 = str_detect(SUPP1, c("jobs|hustle|tutoring")),
         .after = SOCIALSUP1) %>% 
# Break out SUPP2 into individual features
  mutate(UNIVSCHOLSHP2 = str_detect(SUPP2, "University"),
         FOODBANK2 = str_detect(SUPP2, "Food"),
         PERSONALREL2 = str_detect(SUPP2, "Family"),
         EXTORG2 = str_detect(SUPP2, c("Private|Religious|Charities")),
         LOANS2 = str_detect(SUPP2, c("Loans|loans")),
         EXTWORK2 = str_detect(SUPP2, c("jobs|hustle|tutoring")),
         .after = SOCIALSUP2) %>%
# Refactor WITHOUT as logical
  mutate(WITHOUT = str_detect(WITHOUT, "Yes|Groceries")) %>%
# Break out NEED into individual features
  mutate(NEEDMEDICAL = str_detect(NEED, 
                                  "Medical|medication|medicate|medical|tests|Medicine|vision|eye"),
         NEEDMENTAL = str_detect(NEED,
                                 "mental|Mental|Counseling|Therapy"),
         NEEDFOOD = str_detect(NEED,
                               "Food|food|Lunch|snack"),
         .after = WITHOUT) %>%
  mutate(NEEDOTHER = WITHOUT & is.na(NEED),
         .after = NEEDFOOD) %>%
# Refactor INTNTL, SPOUSE, SOCIALSUP1, SOCIALSUP2, ROOMIE, CANSAVE, 
# STIPENDCHNG as logical  
  mutate(INTNTL = str_detect(INTNTL, "Yes"),
         SPOUSE = str_detect(SPOUSE, "Yes"),
         SOCIALSUP1 = str_detect(SOCIALSUP1, "Yes"),
         SOCIALSUP2 = str_detect(SOCIALSUP2, "Yes|poverty"),
         ROOMIE = str_detect(ROOMIE, "Yes|Spouse"),
         CANSAVE = str_detect(CANSAVE, "Yes|Some|some"),
         STIPENDCHNG = str_detect(STIPENDCHNG, "Yes")) %>%
# Refactor DEPT, YEAR, GRADYR, CONTLENGTH, GURNT, AGE, PERINCRENT, PERINCTMR, 
# TRANSPO, ACADCOST, HRSWORK, EXPNSE as factors
  mutate(DEPT = as.factor(DEPT),
         YEAR = as.ordered(YEAR) %>% 
           fct_relevel(c("1st", "2nd", "3rd", 
                         "4th", "5th", "6th+")),
         GRADYR = as.ordered(GRADYR),
         CONTLENGTH = as.ordered(CONTLENGTH) %>% 
           fct_relevel(c("semester by semester basis", "6-month fellowship",
                         "9-month fellowship", "12-month fellowship")),
         GURNT = as.ordered(GURNT) %>%
           fct_relevel(c("1 year", "2 years", "3 years", "4 years",
                         "5 years", "6 or more years")),
         AGE = as.ordered(AGE) %>% 
           fct_relevel(c("20-24", "25-29", "30-34", "35-39", "40-44+")),
         PERINCRENT = as.ordered(PERINCRENT) %>%
           fct_relevel(c("0-9%", "10-19%", "20-29%", "30-39%", 
                         "40-49%", "50-59%", "60%+")),
         PERINCTMR = as.ordered(PERINCTMR) %>%
           fct_relevel(c("0-9%", "10-19%", "20-29%", "30-39%", 
                         "40-49%", "50-59%", "60%+")),
         TRANSPO = as.ordered(TRANSPO) %>%
           fct_relevel(c("0-9%", "10-19%", "20-29%", "30-39%", 
                         "40-49%", "50-59%", "60%+")),
         ACADCOST = as.ordered(ACADCOST) %>%
           fct_relevel(c("0-9%", "10-19%", "20-29%", "30-39%", 
                         "40-49%", "50-59%", "60%+")),
         EXPNSE = as.factor(EXPNSE) %>% fct_collapse(
           Yes = "Yes", No = "No", other_level = "Maybe"),
         HRSWORK = as.ordered(HRSWORK) %>%
           fct_relevel(c("Less than 10 hours", "10 hours", "20 hours",
                         "21-25 hours", "26-39 hours", "40+ hours"))) %>%
# Recode RACETH into standard DARS categories
  mutate(RACETH = DARS_ethnicity_coding(RACETH, INTNTL)) %>%
# Recode GENDER; unfortunately DARS only includes traditional male/female
# gender identities so while I am including an "Other" factor it cannot
# be directly compared to broader data
  mutate(GENDER = as.factor(GENDER) %>% fct_collapse(
    Male = c("Male", "Cis Male", "Man", "Male/man", "M", "Cisgender Male"),
    Female = c("Female", "Cis Female", "Woman", "Woman/female", "F", 
               "cis woman", "female", "cis female", "FEMALE", "she/her/hers"),
    other_level = "Other"
  )) %>%
# Clean up the PERINCTMR column based on PERINCRENT
  mutate(PERINCTMR = coalesce(PERINCRENT))
# Output cleaned results to a .csv for sharing with others
write.csv(tidy_survey_data, file = "tidy_gse_survey.csv", row.names = FALSE)
# Write out column descriptions for sharing with others
column_descriptions = character(ncol(tidy_survey_data))
names(column_descriptions) = colnames(tidy_survey_data)
column_descriptions[colnames(tidy_survey_data) %in% names(questions)] = str_squish(questions)
column_descriptions["ASSTSHP"] = "Derived from FUNDSOURCE, TRUE if Assistantship is a funding source"
column_descriptions["FELLSHP"] = "Derived from FUNDSOURCE, TRUE if Fellowship is a funding source"
column_descriptions["GRANTS"] = "Derived from FUNDSOURCE, TRUE if Grants is a funding source"
column_descriptions["EXTRNL"] = "Derived from FUNDSOURCE, TRUE if External is a funding source"
column_descriptions["EXTRNLFUNDED"] = "Derived from FUNDSOURCE, since NETINC interpretation depends on whether funding is received from university, feature is true when FELLSHP or GRANTS or EXTRNL. This isn't perfect because there could be, for example, internal fellowships"
column_descriptions["UNIVNETINC"] = "NETINC broken out into when EXTRNLFUNDED is FALSE"
column_descriptions["EXTRNLINC"] = "NETINC broken out into when EXTRNLFUNDED is TRUE"
column_descriptions["UNIVSCHOLSHP1"] = "SUPP1 broken out into when University Scholarships is a support source"
column_descriptions["FOODBANK1"] = "SUPP1 broken out into when Food Banks is a support source"
column_descriptions["PERSONALREL1"] = "SUPP1 broken out into when Family/Friends/etc is a support source"
column_descriptions["EXTORG1"] = "SUPP1 broken out into when Charities/Religious Institutions/Private Entities are a support source"
column_descriptions["LOANS1"] = "SUPP1 broken out into when Loans are a support source"
column_descriptions["EXTWORK1"] = "SUPP1 broken out into when External Employment is a support source"
column_descriptions["UNIVSCHOLSHP2"] = "SUPP2 broken out into when University Scholarships is a support source"
column_descriptions["FOODBANK2"] = "SUPP2 broken out into when Food Banks is a support source"
column_descriptions["PERSONALREL2"] = "SUPP2 broken out into when Family/Friends/etc is a support source"
column_descriptions["EXTORG2"] = "SUPP2 broken out into when Charities/Religious Institutions/Private Entities are a support source"
column_descriptions["LOANS2"] = "SUPP2 broken out into when Loans are a support source"
column_descriptions["EXTWORK2"] = "SUPP2 broken out into when External Employment is a support source"
column_descriptions["NEEDMEDICAL"] = "NEED broken out into when Medical/Physical health was gone without to make ends meet"
column_descriptions["NEEDMENTAL"] = "NEED broken out into when Counseling/Mental health was gone without to make ends meet"
column_descriptions["NEEDFOOD"] = "NEED broken out into when Food was gone without to make ends meet"
column_descriptions["NEEDOTHER"] = "NEED broken out into when another or unlisted need was gone without to make ends meet"
# Output column descriptions to a csv
write.csv(column_descriptions, file = "column-descriptions.csv")
# Output .Rdata file that preserves data formats
save(tidy_survey_data, column_descriptions, file = "gse-cleaned-data.Rda")