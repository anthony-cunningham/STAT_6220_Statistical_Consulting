# Reading pre-Survey Data Into R from SPSS file #

library(haven)
path = file.path("C:/", "Users/Anthony/OneDrive/Documents/STAT 6220 Burroughs", "Urology Physician Survey - pre_January 9, 2019_13.03.sav")
preSurvey = read_sav(path)

    # Strip Away Unnecessary Variables #

preSurveyStripped = subset(preSurvey, select = -c(StartDate:Q34))

# Reading post-Survey Data Into R from SPSS file #

library(haven)
path = file.path("C:/", "Users/Anthony/OneDrive/Documents/STAT 6220 Burroughs", "Urology Physician Survey - post_January 22, 2019_15.07.sav")
postSurvey = read_sav(path)

    # Strip Away Unnecessary Variables #

postSurveyStripped = subset(postSurvey, select = -c(StartDate:Q34))

