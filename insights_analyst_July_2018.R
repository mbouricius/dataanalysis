# Insights Analyst - Analysis Project

# libraries
library(curl)
library(lubridate)
library(gridExtra)
library(bigrquery)

# Bring data into R Studio ####

# upload datasets 

curl_download("https://storage.googleapis.com/ccsaa-exports/wentID.csv", "wentID.csv")
curl_download("https://storage.googleapis.com/ccsaa-exports/task.csv", "task.csv")
curl_download("https://storage.googleapis.com/ccsaa-exports/xrefID.csv", "xrefID.csv")
curl_download("https://storage.googleapis.com/ccsaa-exports/aptID.csv", "atpID.csv")

# read data sets 
# source: Nikeya Elliott

went <- read.csv("wentID.csv", stringsAsFactors = FALSE)
teacherdata <- read.csv("task.csv", stringsAsFactors = FALSE)
dates <- read.csv("xrefID.csv", stringsAsFactors = FALSE)
program <- read.csv("atpID.csv", stringsAsFactors = FALSE)

# Explore datasets to understand contents, missing data, and how to consolidate data frames ####

# view column headers with first few entries

head(went, 5)
head(teacherdata, 5)
head(dates, 5)
head(program, 5)


# consolidate 'teacherdata' with 'program' data sets

teacherdata <- merge(teacherdata, program, by = "ATP_ID", all.x = TRUE)

#review how this merged
table(teacherdata$ATP_ID)
table(teacherdata$Program_Name)

table(teacherdata$ATP_ID, teacherdata$Program_Name) # dataframes merged well

# consolidate 'dates' data with 'went' data sets

dates <- merge(dates, went, by = "Went_Id", all.x = TRUE)

# review how this merged
table(dates$Went_Id)
table(dates$Went_Id, dates$Went_To) # dataframes merged well 

# check for duplicate Teacher_IDs

dups_teacherdata <- teacherdata[duplicated(teacherdata$Teacher_ID), ] # no duplicates found

dups_teacherdata <- teacherdata[teacherdata$Teacher_ID %in% 
                                  teacherdata$Teacher_ID[duplicated(teacherdata$Teacher_ID)],]

dups_dates <- dates[duplicated(dates$Teacher_ID), ] # found 465 duplicate IDs, 5% of dataset

dups_dates <- dates[
  dates$Teacher_ID %in% dates$Teacher_ID[duplicated(dates$Teacher_ID)],] # total of 891 entries of duplicate Teacher IDs

# Duplicate IDs include:
# 1. straight duplicates
# 2. Teachers moving from KIPP school to KIPP school
# 3. Data that I can't parse without more information: Teacher_ID 31128 (eg)
# 4. Teachers with date gaps, some a couple of months (summer), others it's years

# On most of these, I would need more data to be able to properly clean it
# For the purposes of this exercise, I will leave these duplicates as is and 
# treat them as unique teachers 

# consolidate data sets into one "data" frame 
data <- merge(dates, teacherdata, by = "Teacher_ID", all.x = TRUE)

# clean Global Environment ####

rm(dates, teacherdata, program, dups_dates, went, dups_teacherdata)

# clean "data" set ####
data$Teaching_End_Date <- as.Date(data$Teaching_End_Date, "%m/%d/%Y")

data$Teaching_Start_Date <- as.Date(data$Teaching_Start_Date, "%m/%d/%Y")

data$Teaching_Start_Date[data$Xref_ID == 17680] <- "2014-07-22"
data$Teaching_End_Date[data$Xref_ID == 16600] <- "2015-06-30"

data$Teaching_End_Date[data$Xref_ID == 14373] <- NA
data$Teaching_End_Date[data$Xref_ID == 14505] <- NA


# check to make sure start and end date are in the right column 

# days 
data$days <- ifelse((data$Teaching_End_Date - data$Teaching_Start_Date) > 0, 
                          data$Teaching_End_Date - data$Teaching_Start_Date,  
                          data$Teaching_Start_Date - data$Teaching_End_Date)
# month
data$month <- data$days/31

# year
data$year <- data$days/365

# round up year column 
data$year_round <- round(data$year, 0)

# start_year column 
data$start_year <-
  ifelse(
    year(data$Teaching_Start_Date) >= year(data$Teaching_End_Date),
    year(data$Teaching_End_Date),
    year(data$Teaching_Start_Date)
  )

# end_year column
data$end_year <-
  ifelse(
    year(data$Teaching_End_Date) >= year(data$Teaching_Start_Date),
    year(data$Teaching_End_Date),
    year(data$Teaching_Start_Date)
  )

                                          
# create Program column                                           
data$Program <- "Non-TFA"
data$Program[data$Program_Name == "Teach for America"] <- "TFA"
data$Program[data$Program_Name == "Unknown"] <- "Unknown"
data$Program[data$Program_Name == "None"] <- "None"
data$Program[is.na(data$Program_Name)] <- "Unknown"

# create Program2 column with simplified fields
data$Program2 <- "Non-TFA"
data$Program2[data$Program_Name == "Teach for America"] <- "TFA"
data$Program2[is.na(data$Program_Name)] <- "Unknown"
data$Program2[data$Program_Name == "Unknown"] <- "Unknown"

# for decade analysis
data$Decade <- ifelse(data$start_year > 2009, "2010", "Before 2010")

# for categorizing teachers' next step: teaching or non-teaching
data$Next_step <- ""
data$Next_step[data$Went_Id == 1] <- "teaching"
data$Next_step[data$Went_Id == 2] <- "non-teaching"
data$Next_step[data$Went_Id == 3] <- "non-teaching"
data$Next_step[data$Went_Id == 4] <- "non-teaching"
data$Next_step[data$Went_Id == 5] <- "non-teaching"
data$Next_step[data$Went_Id == 6] <- "non-teaching"
data$Next_step[data$Went_Id == 7] <- "non-teaching"
data$Next_step[data$Went_Id == 8] <- "teaching"
data$Next_step[data$Went_Id == 9] <- "non-teaching"
data$Next_step[data$Went_Id == 10] <- "non-teaching"


# rebuild as factors to be able to visualize data
data$Program2 <- as.factor(data$Program2)
data$Teacher_ID <- as.factor(data$Teacher_ID)
data$Decade <- as.factor(data$Decade)

data <- data[data$Program2 != "Unknown",]
# Analysis & Visualizations ####

# t-test

independentSamplesTTest(formula = year ~ Program2, data = data)

# results 

# Welch's independent samples t-test 
# 
# Outcome variable:   year 
# Grouping variable:  Program2 
# 
# Descriptive statistics: 
#             Non-TFA   TFA
#    mean       1.901 2.052
#    std dev.   1.804 1.724
# 
# Hypotheses: 
#    null:        population means equal for both groups
#    alternative: different population means in each group
# 
# Test results: 
#    t-statistic:  -3.14 
#    degrees of freedom:  4884.64 
#    p-value:  0.002 
# 
# Other information: 
#    two-sided 95% confidence interval:  [-0.247, -0.057] 
#    estimated effect size (Cohen's d):  0.086 


# density plots 
ggplot(data) + geom_density(aes(x = year, color = Program))

ggplot(data) + geom_density(aes(x = month, color = Program)) + xlim(0, 100)


# re-scale data to make it a normal distribution
p1 <- ggplot(data, aes(year)) + geom_histogram(fill = "white", color = "grey30")

p2 <- ggplot(data, aes(year)) + geom_histogram(fill = "white", color = "grey30") + scale_x_log10()

grid.arrange(p1, p2, ncol = 2)


# Box plot of distribution - with clear outliers 

ggplot(data, aes(Program2, year)) + geom_boxplot()

# view data by TFA/Non-TFA and rescale 
p1 <- ggplot(data, aes(year)) + geom_histogram(fill = "white", color = "grey30") + facet_wrap(~Program2)
p2 <- ggplot(data, aes(year)) + geom_histogram(fill = "white", color = "grey30") + facet_wrap(~Program2) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)



t.test(year~Program2, data = data)

# result

# Welch Two Sample t-test
# 
# data:  year by Program2
# t = -3.1398, df = 4884.6, p-value = 0.001701
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.24671324 -0.05704848
# sample estimates:
#   mean in group Non-TFA     mean in group TFA 
# 1.900584              2.052465 
# 



wilcox.test(year~Program2, data = data)

# result

# Wilcoxon rank sum test with continuity correction
# 
# data:  year by Program2
# W = 3237500, p-value = 7.206e-11
# alternative hypothesis: true location shift is not equal to 0



# analyze by gender
conso_two_g <- data[data$Gender != "",]

table(conso_two_g$Gender)

# analyze by gender & decade
t.test(formula = month~Gender, data = conso_two_g[conso_two_g$Decade == "2010",])
t.test(formula = month~Gender, data = conso_two_g[conso_two_g$Decade == "Before 2010",])

# analyze by gender & TFA/non-TFA
t.test(formula = month~Gender, data = conso_two_g[conso_two_g$year <20,])
t.test(formula = month~Gender, data = conso_two_g[conso_two_g$Program2 == "TFA",])
t.test(formula = month~Gender, data = conso_two_g[conso_two_g$Program2 == "Non-TFA",])

rm(conso_two_g, p1, p2)

# export dataset

insert_upload_job("ccsaa-1098", "pdidata", "data", data)
