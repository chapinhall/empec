#------------------------------------------------------------------------------#
# TITLE:        Auto-pull CPS
# AUTHOR:       Hyein Kang
# DATE CREATED: 2023-07-17

# This code downloads CPS data from the Census Bureau using API, requires an 
# internet connection and census key.

# See below links for references to package and data sources
## https://cran.r-project.org/web/packages/cpsR/cpsR.pdf
## https://www.census.gov/programs-surveys/cps/data/datasets.html
## https://www.census.gov/data/developers/data-sets/census-microdata-api/cps/basic.html (for Basic Monthly)
## https://www.census.gov/data/developers/data-sets/census-microdata-api.CPS_ASEC.html (for ASEC)

#--------------------------------------------------------------------#
# Note: this code is automatically run from the `run--01c--prep-cps-data.Rmd`
# script if CPS data has not yet been pulled for the current run specification.
# If desiring to run this manually, run the following scripts to load relevant
# libraries and functions:
# - settings--main.R
# - settings--profile.R
# - method--general-helper-functions.R

#(optional) Disable scientific notation in R to force R not to use exponential notation
if (FALSE) {
  options(scipen = 999)
}

# Define function to get basic monthly
get_basic_monthly <- function(i, j){
  get_basic(vars  = pull_vars,
            year  = i,
            month = j,
            key   = {census_key})
}

#------------------------------------------------------------------------------#
# Get basic monthly for recent years and months
#------------------------------------------------------------------------------#
# List variables to pull from Census
pull_vars <- c("hryear4",    # YEAR
               "hrmonth",    # MONTH
               "hrmis",	     # household-month-in-sample
               "hrhhid",     # household identifier, scrambled, suggested-weight: hwhhwgt
               "hrhhid2",    # household identifier, part2
               "pulineno",   # unique person identifier
               "hwhhwgt",    # weight-household
               "pwsswgt",    # weight-person
               "pwfmwgt",	   # weight-family
               "perrp",      # relationship to reference person; 40 as ref person w/ relatives; 41 as ref person w/o relatives
               "hrnumhou",   # total # of persons living in the household
               "prfamnum",   # family number recode
               "prfamrel", 
               "prfamtyp",
               "prchld",     # presence of own children < 18
               "hrhtype", 
               "gestfips",   # STATEFIP
               "gtcbsa",     
               "gtmetsta",   
               "gtcbsast",
               "hefaminc",   # FAMINC
               "prtage",     # AGE
               "pesex",      # SEX
               "ptdtrace",   # RACE
               "prmarsta",   # MARST
               "pepar2", 
               "pepar1",
               "pehspnon",   # HISPAN
               "pemlr",      # EMPSTAT
               "prempnot",   # LABFORCE
               "premphrs", 
               "pruntype", 
               "peio1icd",   # IND
               "peeduca",    # EDUC
               "peschenr",   ##need for SCHLCOLL
               "peschft",    ##need for SCHLCOLL
               "peschlvl")   ##need for SCHLCOLL

# Create an empty data frame
cpsbm_2023 <- data.frame() 

for (i in mostrecentyear:mostrecentyear) {
  for (j in 1:mostrecentmonth) {
    bm <- get_basic_monthly(i, j) 
    cpsbm_2023 <- rbind(cpsbm_2023, bm)
    rm(bm)
  }
}

#------------------------------------------------------------------------------#
# Get basic monthly for 2020 - 2022

cpsbm_2020_to_2022 <- glue("{input_path}cpsbm_2020_to_2022.csv")
if (!file.exists(cpsbm_2020_to_2022)) {
  
  # Empty data frame
  cpsbm_2020_to_2022 <- data.frame() 
  
  # Iterate over 2020 January to 2022 December
  for (i in 2020:2022) {
    for (j in 1:12) {
      bm <- get_basic_monthly(i, j) 
      cpsbm_2020_to_2022 <- rbind(cpsbm_2020_to_2022, bm)
      rm(bm)
    }
  }
  
  # Save
  write.csv(cpsbm_2020_to_2022, file = glue("{input_path}cpsbm_2020_to_2022.csv"))

} else {
  cpsbm_2020_to_2022 <- fread(file = cpsbm_2020_to_2022)
}

# /!\ drop 2019-2020 as our base_year is 2021? We could just keep it but that 
# would accumulate unnecessary data
#------------------------------------------------------------------------------#
# Get basic monthly for 2019
cpsbm_2019 <- glue("{input_path}cpsbm_2019.csv")
if (!file.exists(cpsbm_2019)) {
  # Variables for 2019
  pull_vars <- c("hryear4", "hrmonth", "hrmis",	"hrhhid", "hrhhid2", "pulineno", 
                 "hwhhwgt", "pwsswgt", "pwfmwgt", "perrp", "hrnumhou", "prfamnum", 
                 "prfamrel", "prfamtyp", "hrhtype", "gestfips", "gtcbsa", "gtmetsta", 
                 "gtcbsast", "hefaminc", "prtage", "pesex", "ptdtrace", "prmarsta", 
                 "peparent", "pehspnon", "pemlr", "prempnot", "premphrs", "pruntype", 
                 "peio1icd", "peeduca", "peschenr", "peschft", "peschlvl")
  
  # Empty data frame
  cpsbm_2019 <- data.frame() 
  
  # Iterate over 2019 January to December
  for (i in 2019:2019) {
    for (j in 1:12) {
      bm <- get_basic_monthly(i, j) 
      cpsbm_2019 <- rbind(cpsbm_2019, bm)
      rm(bm)
    }
  }
  
  # Save
  write.csv(cpsbm_2019, file = glue("{input_path}cpsbm_2019.csv"))
  
} else {
  cpsbm_2019 <- fread(file = cpsbm_2019)
}

#------------------------------------------------------------------------------#
# Get basic monthly for 2007 - 2009
cpsbm_2007_to_2009 <- glue("{input_path}cpsbm_2007_to_2009.csv")
if (!file.exists(cpsbm_2007_to_2009)) {
  # Variables for 2007-2009
  pull_vars <- c("hryear4", "hrmonth", "hrmis",	"hrhhid", "hrhhid2", "pulineno",
                 "hwhhwgt", "pwsswgt", "pwfmwgt",	"perrp", "hrnumhou", "prfamnum", 
                 "prfamrel", "prfamtyp", "hrhtype", "gestfips", "gtcbsa", "gtmetsta", 
                 "gtcbsast", "hufaminc", "prtage", "pesex", "ptdtrace", "prmarsta", 
                 "peparent", "pehspnon", "pemlr", "prempnot", "premphrs", "pruntype", 
                 "peio1icd", "peeduca", "peschenr", "peschft", "peschlvl")
  
  # Empty data frame
  cpsbm_2007_to_2009 <- data.frame() 
  
  # Iterate over 2020 January to 2022 December
  for (i in 2007:2009) {
    for (j in 1:12) {
      bm <- get_basic_monthly(i, j) 
      cpsbm_2007_to_2009 <- rbind(cpsbm_2007_to_2009, bm)
      rm(bm)
    }
  }
  
  # Rename family income variable for consistency
  cpsbm_2007_to_2009 <- cpsbm_2007_to_2009 %>%
    mutate(hefaminc = hufaminc) %>%
    select(-hufaminc)
  
  # Save
  write.csv(cpsbm_2007_to_2009, file = glue("{input_path}cpsbm_2007_to_2009.csv"))
  
} else {
  cpsbm_2007_to_2009 <- fread(file = cpsbm_2007_to_2009)
}

#------------------------------------------------------------------------------#
# Family income variable for 2007-2009
cpsbm_2007_to_2009 <- cpsbm_2007_to_2009 %>%
  mutate(hefaminc = ifelse(hefaminc < 0, NA, hefaminc),
         hrhhid   = as.numeric(hrhhid),
         hrhhid2  = as.numeric(hrhhid2))

cpsbm_2019 <- cpsbm_2019 %>%
  mutate(hrhhid   = as.numeric(hrhhid),
         hrhhid2  = as.numeric(hrhhid2))

cpsbm_2020_to_2022 <- cpsbm_2020_to_2022 %>%
  mutate(hrhhid   = as.numeric(hrhhid),
         hrhhid2  = as.numeric(hrhhid2),
         perrp    = recode(as.numeric(perrp),
                           `40` = 1,
                           `41` = 2,
                           `42` = 3,
                           `43` = 13,
                           `44` = 14,
                           `45` = 3,
                           `46` = 13,
                           `47` = 14,
                           `48` = 4,
                           `49` = 5,
                           `50` = 6,
                           `51` = 7,
                           `52` = 8,
                           `53` = 9,
                           `54` = 15,
                           `55` = 16,
                           `56` = 17,
                           `57` = 18,
                           `58` = 10,
                           `59` = 12)
  )

cpsbm_2023 <- cpsbm_2023 %>%
  mutate(hrhhid   = as.numeric(hrhhid),
         hrhhid2  = as.numeric(hrhhid2),
         perrp    = recode(as.numeric(perrp),
                           `40` = 1,
                           `41` = 2,
                           `42` = 3,
                           `43` = 13,
                           `44` = 14,
                           `45` = 3,
                           `46` = 13,
                           `47` = 14,
                           `48` = 4,
                           `49` = 5,
                           `50` = 6,
                           `51` = 7,
                           `52` = 8,
                           `53` = 9,
                           `54` = 15,
                           `55` = 16,
                           `56` = 17,
                           `57` = 18,
                           `58` = 10,
                           `59` = 12)
  )

# Append data
cpsbm <- bind_rows(cpsbm_2007_to_2009, cpsbm_2019, cpsbm_2020_to_2022, cpsbm_2023)

# Remove individual data and temporary list
rm(cpsbm_2007_to_2009, cpsbm_2019, cpsbm_2020_to_2022, cpsbm_2023)

# Generate variables
cpsbm <- cpsbm %>%
  # cpsid: unique identifier of cps households across cps samples
  # cpsidp: unique identifier of cps individuals (cpsid+person#)
  mutate(cpsid = paste0(hrhhid2, hrhhid),
         cpsidp = paste0(cpsid, pulineno),
         metro = ifelse(gtmetsta == 3, 0,
                        ifelse(gtmetsta == 2, 1,
                               ifelse(gtcbsast == 1, 2,
                                      ifelse(gtcbsast == 2, 3,
                                             ifelse(gtcbsast == 4 & gtmetsta !=3, 4, NA))))))

# Generate variables for baseyear/basemonth
cpsbm <- cpsbm %>%
  mutate(year_month = sprintf("%s%02d", hryear4, as.integer(hrmonth)),
         date  = ymd(paste0(year_month, "01")),
         num_months = ifelse(hrmis <=4, hrmis - 1, hrmis + 8 - 1),
         new_date = date %m-% months(num_months),
         start_time = as.integer(format(new_date, "%Y%m"))) %>%
  arrange(hryear4, hrmonth, start_time, cpsid) %>%
  select(-year_month, -date, -num_months, -new_date) %>%
  select(hryear4, hrmonth, start_time, cpsid, everything())

# Save data
write.csv(cpsbm, file = glue("{input_path}cpsbm.csv"))

# /!\ If needed, address "pepar1" and "pepar2" (2020 - ) vs. "peparent" (2007 - 2019)

# To get ASEC and link to Basic Monthly, use below to download ASEC
# Get CPS ASEC using Census API
if (FALSE) {
  cps_asec <- 
    get_asec(vars = ,
             year = ,
    )
}

# List of variables available (as of Jan 2023) at (https://api.census.gov/data/2023/cps/basic/jan/variables.html)
# Variable information available (as of Jan 2023) at (https://www2.census.gov/programs-surveys/cps/datasets/2023/basic/2023_Basic_CPS_Public_Use_Record_Layout_plus_IO_Code_list.txt)
if (FALSE) {
  # List of all variables available
  pull_vars <- c("GEDIV",	      #Geographical Division
                 "GEREG",	      #Geography-region
                 "GESTFIPS",  	#Geography-FIPS state code
                 "GTCBSA",	    #Metropolitan Statistical Area FIPS Code
                 "GTCBSAST",	  #Principal City/Balance Status
                 "GTCBSASZ",  	#Metropolitan Statistical Area Size
                 "GTCO",	      #FIPS County Code. Read the full description prior to using this field.
                 "GTCSA",	      #Combined Statistical Area FIPS Code
                 "GTINDVPC",  	#Individual Central City Code
                 "GTMETSTA",  	#Metropolitan Status
                 "H_ID_PL",	    #HH PP Unique ID
                 "HEFAMINC",  	#Household-total family income in past 12 months
                 "HEHOUSUT",  	#Household-type of living quarters
                 "HEPHONEO",  	#Household-phone interview acceptable,y/n
                 "HETELAVL",  	#Household-no residential phone,available elsewhere,y/n
                 "HETELHHD",  	#Household-phone in living quarters,y/n
                 "HETENURE",  	#Household-own/rent living quarters
                 "HRHHID",	    #Household-identifier,scrambled
                 "HRHHID2",  	  #Household Identification Number, part 2
                 "HRHTYPE",	    #Household-type of family/single individual
                 "HRINTSTA",  	#Household-interview/non-interview status
                 "HRLONGLK",  	#Household-longitudinal link indicator
                 "HRMIS",	      #Household-month-in-sample
                 "HRMONTH",	    #Household-month of interview
                 "HRNUMHOU",  	#Household-total # of members
                 "HRYEAR4",	    #Household-4 digit year of interview
                 "HUBUS",	      #Labor Force-presence of business/farm in hhld,y/n
                 "HUBUSL1",	    #Labor Force-line numbers of business/farm owners 1
                 "HUBUSL2",	    #Labor Force-line numbers of business/farm owners 2
                 "HUBUSL3",	    #Labor Force-line numbers of business/farm owners 3
                 "HUBUSL4",	    #Labor Force-line numbers of business/farm owners 4
                 "HUFINAL",	    #Household-final code,status of interview
                 "HUINTTYP",  	#Household-type of interview,personal/telephone
                 "HUPRSCNT",  	#Household-# of actual & attempted personal contacts
                 "HURESPLI",  	#Household-line number of current respondent
                 "HUTYPB",	    #Household-type B non-interview categories
                 "HUTYPC",	    #Household-type C non-interview categories
                 "HUTYPEA",	    #Household-type A non-interview categories
                 "HWHHWGT",  	  #Weight-household
                 "HWHHWTLN",  	#Line number (PULINENO) of Household Weight donor
                 "HXFAMINC",  	#Household-allocation flag for HEFAMINC
                 "HXHOUSUT",  	#Household-allocation flag for HEHOUSUT
                 "HXPHONEO",  	#Household-allocation flag for HEPHONEO
                 "HXTELAVL",  	#Household-allocation flag for HETELAVL
                 "HXTELHHD",  	#Household-allocation flag for HETELHHD
                 "HXTENURE",  	#HOUSEHOLD - allocation flag for HETENURE
                 "OCCURNUM",  	#Unique person identifier
                 "PEABSPDO",  	#Labor Force-paid absence from work,y/n
                 "PEABSRSN",  	#Labor Force-reason for work absence last week
                 "PEAFEVER",  	#Demographics-ever serve on active duty
                 "PEAFNOW",	    #Demographics-currently in armed forces,y/n
                 "PEAFWHN1",  	#Demographics-past military service, period of active duty
                 "PEAFWHN2",  	#Demographics-past military service, period of active duty
                 "PEAFWHN3",  	#Demographics-past military service, period of active duty
                 "PEAFWHN4",  	#Demographics-past military service, period of active duty
                 "PECERT1",	    #Certification Active
                 "PECERT2",	    #Certification Federal, State, or Local
                 "PECERT3",	    #Certification - Relation to Job
                 "PECOHAB",	    #Line Number of Cohabiting Partner
                 "PECYC",	      #Demographics-years of college credit completed
                 "PEDIPGED",  	#Demographics-high school,graduation/GED
                 "PEDISDRS",  	#Disability - Difficulty dressing or bathing
                 "PEDISEAR",  	#Disability - Deaf or serious difficulty hearing
                 "PEDISEYE",  	#Disability - Blind or difficulty seeing even with glasses
                 "PEDISOUT",  	#Disability - Difficulty doing errands
                 "PEDISPHY",  	#Disability - Difficulty walking or climbing stairs
                 "PEDISREM",  	#Disability - Difficulty remembering or making decisions
                 "PEDW4WK",	    #Labor Force-(not in,discouraged)worked in last 4 weeks
                 "PEDWAVL",	    #Labor Force-(not in,discouraged)available for job last week
                 "PEDWAVR",	    #Labor Force-(not in,discouraged)reason can't work
                 "PEDWLKO",	    #Labor Force-(not in,discouraged)look for work in past year
                 "PEDWLKWK",	  #Labor Force-(not in,discouraged)looked since last job,y/n
                 "PEDWRSN",	    #Labor Force-(not in,discouraged)reason not looking
                 "PEDWWK",	    #Labor Force-(not in,discouraged)worked in past year,y/n
                 "PEDWWNTO",	  #Labor Force-(not in,discouraged)wanted a job,y/n
                 "PEEDUCA",	    #Demographics-highest level of school completed
                 "PEERNCOV",  	#Earnings-covered by labor union/employee contract,y/n
                 "PEERNHRO",  	#Earnings-#hours usually worked
                 "PEERNHRY",  	#Earnings-hourly/non-hourly worker
                 "PEERNLAB",  	#Earnings-union member,y/n
                 "PEERNPER",  	#Earnings-when received,periodicity
                 "PEERNRT",	    #Earnings-paid hourly,y/n
                 "PEERNUOT",  	#Earnings-overtime pay/tips/commission,y/n
                 "PEERNWKP",  	#Earnings-#paid weeks per year
                 "PEFNTVTY",	  #Demographics - native country of father
                 "PEHGCOMP",  	#Demographics-highest grade completed before GED
                 "PEHRACT1",  	#Labor Force-# hours actually worked at main job
                 "PEHRACT2",  	#Labor Force-# hours actually worked at other job(s)
                 "PEHRACTT",  	#Labor Force-# hours actually worked at all jobs
                 "PEHRAVL",	    #Labor Force-(part-timer)available for full-time,y/n
                 "PEHRFTPT",  	#Labor Force-usually work full-time,y/n
                 "PEHRRSN1",  	#Labor Force-(part-timer)reason
                 "PEHRRSN2",  	#Labor Force-(part-timer)reason not full-time
                 "PEHRRSN3",  	#Labor Force-(full-timer)reason part-time last week
                 "PEHRUSL1",  	#Labor Force-# hours usually worked at main job
                 "PEHRUSL2",  	#Labor Force-# hours usually worked at other job(s)
                 "PEHRUSLT",  	#Labor Force-# hours usually worked at all jobs
                 "PEHRWANT",  	#Labor Force-full-time work desired,y/n
                 "PEHSPNON",  	#Demographics- hispanic/non-hispanic origin
                 "PEIO1COW",  	#Indus.&Occ.-(main job)class of worker
                 "PEIO1ICD",  	#Industry and Occupation - Industry Code (Main Job)
                 "PEIO2COW",  	#Indus.&Occ.-(second job)class of worker
                 "PEIO2ICD",  	#Industry and Occupation - Industry Code (Second Job)
                 "PEJHRSN",	    #Labor Force-(not in,job history)reason left last job
                 "PEJHWANT",  	#Labor Force-(not in,job history)look for work in next year
                 "PEJHWKO",	    #Labor Force-(not in,job history)worked in past year,y/n
                 "PELAYAVL",  	#Labor Force-(layoff)available for work if recalled,y/n
                 "PELAYDUR",  	#Labor Force-(layoff)# weeks looking for job
                 "PELAYFTO",  	#Labor Force-(layoff)from full-time job,y/n
                 "PELAYLK",	    #Labor Force-(layoff)called back,still looking for work,y/n
                 "PELKAVL",	    #Labor Force-(unemployed)available for work last week,y/n
                 "PELKDUR",	    #Labor Force-(unemployed)#weeks on job search
                 "PELKFTO",	    #Labor Force-(unemployed)looking-full-time work wanted,y/n
                 "PELKLL1O",  	#Labor Force-(unemployed)looking-activity before search
                 "PELKLL2O",  	#Labor Force-(unemployed)looking-lost/quit job
                 "PELKLWO",	    #Labor Force-(unemployed)looking-when last worked
                 "PELKM1",	    #Labor Force-(unemployed)looking-search methods
                 "PEMARITL",  	#Demographics-marital status
                 "PEMJNUM",	    #Labor Force-# of jobs had
                 "PEMJOT",	    #Labor Force-had multiple jobs,y/n
                 "PEMLR",	      #Labor Force-employment status
                 "PEMNTVTY",  	#Demographics - native country of mother
                 "PENATVTY",  	#Demographics - native country of sample person
                 "PENLFACT",  	#Labor Force-(not in)reason
                 "PENLFJH",	    #Labor Force-(not in)last worked at job/business
                 "PENLFRET",  	#Labor Force-(retired) from job/business,y/n
                 "PEPAR1",	    #Demographics-parent 1 line number
                 "PEPAR1TYP",  	#Demographics - parent 1 type
                 "PEPAR2",	    #Demographics-parent 2 line number
                 "PEPAR2TYP",  	#Demographics - parent 2 type
                 "PEPDEMP1",  	#Individual has paid employees?
                 "PEPDEMP2",  	#Individual has paid employees, second job
                 "PERET1",	    #Labor Force-(retired)want full/part-time job,y/n
                 "PERRP",	      #Demographics - Relationship to Reference Person
                 "PESCHENR",	  #School Enrollment-in high school/college/university,y/n
                 "PESCHFT",	    #School Enrollment-full-time/part-time student
                 "PESCHLVL",  	#School Enrollment-in high school/college/university
                 "PESEX",	      #Demographics-sex
                 "PESPOUSE",  	#Demographics-spouse line number
                 "PRABSREA",  	#Labor Force-work absence,reason & pay status
                 "PRAGNA",	    #Indus.&Occ.-industry,agriculture/non-agriculture
                 "PRCHLD",	    #Demographics-presence of own children <18 years by age group
                 "PRCITFLG",  	#Demographics-citizenship allocation flag
                 "PRCITSHP",  	#Demographics-United States citizenship group
                 "PRCIVLF",	    #Labor Force-part of/not part of
                 "PRCOW1",	    #Indus.&Occ.-(main job)class of worker-recode
                 "PRCOW2",	    #Indus.&Occ.-(second job)class of worker-recode
                 "PRCOWPG",	    #Indus.&Occ.-(main job)class of worker,private/government
                 "PRDASIAN",  	#Demographics Detailed Asian Subgroup
                 "PRDISC",	    #Labor Force-discouraged worker/conditionally interested
                 "PRDISFLG",  	#Disability - recode, disabled
                 "PRDTCOW1",	  #Indus.&Occ.-(main job)detailed class of worker
                 "PRDTCOW2",	  #Indus.&Occ.-(second job)detailed class of worker-recode
                 "PRDTHSP",	    #Detailed Hispanic Recode
                 "PRDTIND1",	  #Indus.&Occ.-(main job)detailed industry
                 "PRDTIND2",	  #Indus.&Occ.-(second job)detailed industry-recode
                 "PRDTOCC1",	  #Indus.&Occ.-(main job)detailed occupation groups-recode
                 "PRDTOCC2",	  #Indus.&Occ.-(second job)detailed occupation groups-recode
                 "PREMP",	      #Labor Force-employed non-farm/non-private hhld industries
                 "PREMPHRS",  	#Labor Force-reason for work absence or hours at work
                 "PREMPNOT",  	#Labor Force-employment status,recode
                 "PRERELG",	    #Earnings-earnings edit eligibility flag
                 "PRERNMIN",  	#FEDERALLY MANDATED MINIMUM WAGE FLAG
                 "PREXPLF",	    #Labor Force-employed/unemployed
                 "PRFAMNUM",  	#Demographics-family number
                 "PRFAMREL",  	#Demographics-family relationship(recode)
                 "PRFAMTYP",  	#Demographics-family type(recode)
                 "PRFTLF",	    #Labor Force-full-time/part-time
                 "PRHERNAL",	  #Earnings-persons hourly earnings recode allocation flag
                 "PRHRUSL",	    #Labor Force-hours usually worked weekly
                 "PRIMIND1",	  #Indus.&Occ.-(main job)detailed industry
                 "PRIMIND2",	  #Indus.&Occ.-(second job)detailed industry
                 "PRINUYER",	  #Recoded Year of Entry for Foreign Born
                 "PRIOELG",	    #Labor Force-industry and occupation edit eligibility flag
                 "PRJOBSEA",	  #Labor Force-job search,recode
                 "PRMARSTA",	  #Demographics-marital status(recode)
                 "PRMJIND1",	  #Indus.&Occ.-(main job)industry,major groups
                 "PRMJIND2",	  #Indus.&Occ.-(second job)industry,major groups-recode
                 "PRMJOCC1",	  #Indus.&Occ.-(main job)occupation,major groups-recode
                 "PRMJOCC2",	  #Indus.&Occ.-(second job)occupation,major groups-recode
                 "PRMJOCGR",  	#Indus.&Occ.-(main job)occupation,7 groups
                 "PRNAGPWS",  	#Indus.&Occ.-non-agriculture/private/salary workers-recode
                 "PRNAGWS",	    #Indus.&Occ.-non-agriculture/salary workers
                 "PRNLFSCH",  	#School Enrollment-(not in labor force)school/not in school
                 "PRNMCHLD",  	#Demographics-number of own children <18 years of age
                 "PRPERTYP",  	#Demographics-person type(recode)
                 "PRPTHRS",	    #Labor Force-(part-timer)economic/non-economic reasons,hours
                 "PRPTREA",	    #Labor Force-(part-timer)specific reason
                 "PRSJMJ",	    #Indus.&Occ.-single/multiple jobholder-recode
                 "PRTAGE",	    #Demographics - age topcoded at 85, 90 or 80 (see full description)
                 "PRTFAGE",	    #Demographics-top coded flag for age
                 "PRUNEDUR",  	#Labor Force-unemployment duration
                 "PRUNTYPE",	  #Labor Force-unemployment reason
                 "PRWERNAL",	  #Earnings-(person)weekly earnings allocation flag-recode
                 "PRWKSCH",	    #Labor Force-employment status by time worked/lost
                 "PRWKSTAT",  	#Labor Force-full-time/part-time work status
                 "PRWNTJOB",  	#Labor Force-(not in)or wants job-recode
                 "PTDTRACE",  	#Demographics- race of respondent
                 "PTERN",	      #Earnings-(weekly)amount of overtime earnings
                 "PTERN2",	    #Earnings-(calculated)weekly overtime
                 "PTERNH1C",  	#Earnings-hourly pay rate,excluding overtime
                 "PTERNH1O",  	#Earnings-hourly pay rate,amount
                 "PTERNH2",	    #Earnings-(main job)hourly pay rate,amount
                 "PTERNHLY",  	#Earnings-hourly pay rate,amount-recode
                 "PTERNWA",	    #Earnings-weekly earnings,amount-recode
                 "PTHR",	      #Earnings-hourly pay-Top Code
                 "PTIO1OCD",  	#Industry and Occupation - Occupation Code (Primary Job)
                 "PTIO2OCD",  	#Industry and Occupation - Occupation Code (Second Job)
                 "PTNMEMP1",  	#Number of Paid Employees--MAIN JOB
                 "PTNMEMP2",	  #Number of paid employees--Second Job
                 "PTOT",	      #Earnings-weekly overtime,amount-top code flag
                 "PTWK",	      #Earnings-weekly-top code flag
                 "PUABSOT",	    #Labor Force-output var determines absence from job
                 "PUBUS1",	    #Labor Force-unpaid work in family business/farm,y/n
                 "PUBUS2OT",  	#Labor Force-stores BUS2 entry
                 "PUBUSCK1",	  #Labor Force-filter for question on unpaid work
                 "PUBUSCK2",	  #Labor Force-(family business)skips owners,no work last week
                 "PUBUSCK3",	  #Labor Force-filter for business owners to absence reason
                 "PUBUSCK4",	  #Labor Force-filter for business owners skip pattern
                 "PUCHINHH",	  #Demographics-reason for the changes in household composition
                 "PUDIS",	      #Labor Force-verify disability status from previous month
                 "PUDIS1",	    #Labor Force-probe #1 for disability
                 "PUDIS2",	    #Labor Force-probe #2 for disability
                 "PUDWCK1",	    #Labor Force-filter for discouraged worker screening
                 "PUDWCK2",	    #Labor Force-filter for disabled
                 "PUDWCK3",	    #Labor Force-filter for retired
                 "PUDWCK4",	    #Labor Force-filter/plug for passive jobseekers
                 "PUDWCK5",	    #Labor Force-filter/plug for passive jobseeker
                 "PUHRCK1",	    #Labor Force-remove groups from actual hours series
                 "PUHRCK12",  	#Labor Force-filter for <15 hours to go to looking series
                 "PUHRCK2",	    #Labor Force-skips persons out of PT series
                 "PUHRCK3",	    #Labor Force-skips persons out of PT series
                 "PUHRCK4",	    #Labor Force-skips persons
                 "PUHRCK5",	    #Labor Force-filter for multiple jobholders for job 2 hours
                 "PUHRCK6",	    #Labor Force-filter for actual hours jobs 1 and 2
                 "PUHRCK7",	    #Labor Force-filter for hours worked paths
                 "PUHROFF1",  	#Labor Force-any work hours were lost last week
                 "PUHROFF2",  	#Labor Force-number of work hours lost last week
                 "PUHROT1",	    #Labor Force-extra hours worked last week
                 "PUHROT2",	    #Labor Force-number of extra hours worked last week
                 "PUIO1MFG",  	#Indus.&Occ.-(main job)in manufacturing/wholesale/retail
                 "PUIO2MFG",  	#Indus.&Occ.-(second job)in manufacturing/wholesale/retail
                 "PUIOCK1",	    #Indus.&Occ.-filter for dependent industry & occupation
                 "PUIOCK2",	    #Indus.&Occ.-filter for previous month's referred I&O cases
                 "PUIOCK3",	    #Indus.&Occ.-filter for previous month's unknown occupation
                 "PUIODP1",	    #Indus.&Occ.-verification of previous month's employer
                 "PUIODP2",	    #Indus.&Occ.-job duties changed since last month,y/n
                 "PUIODP3",	    #Indus.&Occ.-verify previous month's occupation description
                 "PUJHCK1",	    #Labor Force-filter for outgoing rotations
                 "PUJHCK2",	    #Labor Force-filter for persons going thru I&O series
                 "PUJHCK3",	    #Labor Force-filter for unemployed job history
                 "PUJHCK4",	    #Labor Force-(not in)filter for dependent
                 "PUJHCK5",	    #Labor Force-(not in)filter/carryover for dependent
                 "PUJHDP1O",  	#Labor Force-out variable for JHDP1
                 "PULAY",	      #Labor Force-person on layoff from job
                 "PULAY6M",	    #Labor Force-person on layoff,recalled in 6 months,y/n
                 "PULAYAVR",  	#Labor Force-person on layoff,reason unavailable to work
                 "PULAYCK1",  	#Labor Force-filter for previous month layoff status
                 "PULAYCK2",  	#Labor Force-filter/plug for dependent layoff
                 "PULAYCK3",  	#Labor Force-dependent I & O filter/carryover
                 "PULAYDT",     #Labor Force-person on layoff,has return date,y/n
                 "PULINENO",	  #Demographics-line number
                 "PULK",	      #Labor Force-looked for work in last 4 wks
                 "PULKAVR",	    #Labor Force-reason jobseeker unavailable last week
                 "PULKDK1",	    #Labor Force-(unemployed) followup to LKM1
                 "PULKDK2",	    #Labor Force-(looking for work)followup to LKM2
                 "PULKDK3",	    #Labor Force-(looking for work)followup to LKM3
                 "PULKDK4",	    #Labor Force-(looking for work)followup to LKM4
                 "PULKDK5",	    #Labor Force-(looking for work)followup to LKM5
                 "PULKDK6",	    #Labor Force-(looking for work)followup to LKM6
                 "PULKM2",	    #Labor Force-(job search)methods,all in last 4 weeks
                 "PULKM3",	    #Labor Force-(job search)methods-followup
                 "PULKM4",	    #Labor Force-(job search)methods-followup2
                 "PULKM5",	    #Labor Force-(job search)methods followup3
                 "PULKM6",	    #Labor Force-(job search)methods followup4
                 "PULKPS1",	    #Labor Force-(job search)passive entry to LKM1-followup
                 "PULKPS2",	    #Labor Force-(job search)passive entry to LKM2-followup
                 "PULKPS3",	    #Labor Force-(job search)passive entry to LKM3-followup
                 "PULKPS4",	    #Labor Force-(job search)passive entry to LKM4-followup
                 "PULKPS5",	    #Labor Force-(job search)passive entry to LKM5-followup
                 "PULKPS6",	    #Labor Force-(job search)passive entry to LKM6-followup
                 "PUNLFCK1",	  #Labor Force-age filter for retirement question
                 "PUNLFCK2",	  #Labor Force-outgoing rotation filter
                 "PURETOT",	    #Labor Force-verify retirement status,previous month
                 "PUSLFPRX",	  #Labor Force-information given by self/proxy
                 "PUWK",	      #Labor Force-did work for pay/profit
                 "PWCMPWGT",	  #Weight-composited final weight
                 "PWFMWGT",	    #Weight-family
                 "PWLGWGT",	    #Weight-longitudinal weight
                 "PWORWGT",	    #Weight-outgoing rotation weight
                 "PWSSWGT",	    #Weight-second stage weight (rake 6 final step weight)
                 "PWVETWGT",  	#Weight-veterans weight
                 "PXABSPDO",  	#Labor Force-allocation flag for PEABSPDO
                 "PXABSRSN",  	#Labor Force-allocation flag for PEABSRSN
                 "PXAFEVER",	  #Demographics-allocation flag for PEAFEVER
                 "PXAFNOW",	    #Demographics-allocation flag for PEAFNOW
                 "PXAFWHN1",	  #Demographics-allocation flag for PEAFWHN1
                 "PXAGE",	      #Demographics-allocation flag for PEAGE
                 "PXCERT1",	    #Allocation Flag for PECERT1
                 "PXCERT2",	    #Allocation Flag for PECERT2
                 "PXCERT3",	    #Allocation Flag for PECERT3
                 "PXCOHAB",	    #Allocation Flag for Cohabiting Partner
                 "PXCYC",	      #Demographics-allocation flag for PECYC
                 "PXDIPGED",	  #Demographics-allocation flag for PEDIPGED
                 "PXDISDRS",	  #Allocation Flag for PEDISDRS
                 "PXDISEAR",	  #Allocation for PEDISEAR
                 "PXDISEYE",	  #Allocation Flag for PEDISEYE
                 "PXDISOUT",	  #Allocation Flag for PEDISOUT
                 "PXDISPHY",	  #Allocation Flag for PEDISPHY
                 "PXDISREM",	  #Allocation Flag for PEDISREM
                 "PXDW4WK",	    #Labor Force-allocation flag for PEDW4WK
                 "PXDWAVL",	    #Labor Force-allocation flag for D23c
                 "PXDWAVR",	    #Labor Force-allocation flag for PEDWAVR
                 "PXDWLKO",	    #Labor Force-allocation flag for PEDWLKO
                 "PXDWLKWK",	  #Labor Force-allocation flag for PEDWLKWK
                 "PXDWRSN",	    #Labor Force-allocation flag for PEDWRSN
                 "PXDWWK",	    #Labor Force-allocation flag for PEDWWK
                 "PXDWWNTO",	  #Labor Force-allocation flag for PEDWWNTO
                 "PXEDUCA",	    #Demographics-allocation flag for PEEDUCA
                 "PXERN",	      #Earnings-allocation flag for PEERN
                 "PXERNCOV",	  #Earnings-allocation flag for PEERNCOV
                 "PXERNH1O",	  #Earnings-allocation flag for PEERNH1O
                 "PXERNH2",	    #Earnings-allocation flag for PEERNH1O
                 "PXERNHRO",  	#Earnings-allocation flag for PEERNHRO
                 "PXERNHRY",	  #Earnings-allocation flag for PEERNHRY
                 "PXERNLAB",    	#Earnings-allocation flag for PEERNLAB
                 "PXERNPER",  	#Earnings-allocation flag for PEERNPER
                 "PXERNRT",	    #Earnings-allocation flag for PEERNRT
                 "PXERNUOT",	  #Earnings-allocation flag for PEERNUOT
                 "PXERNWKP",	  #Earnings-allocation flag for weeks paid per year PEERNWKP
                 "PXFNTVTY",	  #Demographics-allocation flag for PEFNTVTY
                 "PXHGCOMP",	  #Demographics-allocation flag for PEHGCOMP
                 "PXHRACT1",	  #Labor Force-allocation flag for PEHRACT1
                 "PXHRACT2",	  #Labor Force-allocation flag for PEHRACT2
                 "PXHRACTT",	  #Labor Force-allocation flag for PEHRACTT
                 "PXHRAVL",	    #Labor Force-allocation flag for PEHRAVL
                 "PXHRFTPT",	  #Labor Force-allocation flag for PEHRFTPT
                 "PXHRRSN1",	  #Labor Force-allocation flag for PEHRRSN1
                 "PXHRRSN2",	  #Labor Force-allocation flag for PEHRRSN2
                 "PXHRRSN3",	  #Labor Force-allocation flag for PEHRRSN3
                 "PXHRUSL1",	  #Labor Force-allocation flag for PEHRUSL1
                 "PXHRUSL2",	  #Labor Force-allocation flag for PEHRUSL2
                 "PXHRUSLT",  	#Labor Force-allocation flag for PEHRUSLT
                 "PXHRWANT",  	#Labor Force-allocation flag for PEHRWANT
                 "PXHSPNON",  	#Demographics-allocation flag for PEHSPNON
                 "PXINUSYR",  	#Demographics-allocation flag for PEINUSYR
                 "PXIO1COW",  	#Indus.&Occ.-allocation flag for PEIO1COW
                 "PXIO1ICD",	  #Indus.&Occ.-allocation flag for PEIO1ICD
                 "PXIO1OCD",	  #Indus.&Occ.-allocation flag for PEIO1OCD
                 "PXIO2COW",	  #Indus.&Occ.-allocation flag for PEI02COW
                 "PXIO2ICD",	  #Indus.&Occ.-allocation flag for PEIO2ICD
                 "PXIO2OCD",	  #Indus.&Occ.-allocation flag for PEIO2OCD
                 "PXJHRSN",	    #Labor Force-allocation flag for PEJHRSN
                 "PXJHWANT",	  #Labor Force-allocation flag for PEJHWANT
                 "PXJHWKO",	    #Labor Force-allocation flag for PEJHWKO
                 "PXLAYAVL",	  #Labor Force-allocation flag for PELAYAVL
                 "PXLAYDUR",	  #Labor Force-allocation flag for PELAYDUR
                 "PXLAYFTO",	  #Labor Force-allocation flag for PELAYFTO
                 "PXLAYLK",	    #Labor Force-allocation flag for PELAYLK
                 "PXLKAVL",	    #Labor Force-allocation flag for PELKAVL
                 "PXLKDUR",	    #Labor Force-allocation flag for PELKDUR
                 "PXLKFTO",	    #Labor Force-allocation flag for PELKFTO
                 "PXLKLL1O",	  #Labor Force-allocation flag for PELKLL1O
                 "PXLKLL2O",	  #Labor Force-allocation flag for PELKLL2O
                 "PXLKLWO",	    #Labor Force-allocation flag for PELKLWO
                 "PXLKM1",	    #Labor Force-allocation flag for PELKM1
                 "PXMARITL",	  #Demographics-allocation flag for PEMARITL
                 "PXMJNUM",	    #Labor Force-allocation flag for PEMJNUM
                 "PXMJOT",	    #Labor Force-allocation flag for PEMJOT
                 "PXMLR",	      #Labor Force-allocation flag for PEMLR
                 "PXMNTVTY",	  #Demographics-allocation flag for PEMNTVTY
                 "PXNATVTY",	  #Demographics-allocation flag for PENATVTY
                 "PXNLFACT",	  #Labor Force-allocation flag for PENLFACT
                 "PXNLFJH",	    #Labor Force-allocation flag for PENLFJH
                 "PXNLFRET",	  #Labor Force-allocation flag for PENLFRET
                 "PXNMEMP1",   	#Allocation Flag for PTNMEMP1
                 "PXNMEMP2",  	#Allocation Flag for PTNMEMP2
                 "PXPAR1",	    #Demographics- allocation flag for PEPAR1
                 "PXPAR1TYP",   #Demographics- allocation flag for PXPAR1TYP
                 "PXPAR2",	    #Demographics- allocation flag for PEPAR2
                 "PXPAR2TYP",	  #Demographics- allocation flag for PXPAR2TYP
                 "PXPDEMP1",  	#Allocation Flag for PEPDEMP1
                 "PXPDEMP2",  	#Allocation Flag for PEPDEMP2
                 "PXRACE1",	    #Demographics-allocation flag for PERACE
                 "PXRET1",	    #Labor Force-allocation flag for PERET1
                 "PXRRP",	      #Demographics-allocation flag for PERRP
                 "PXSCHENR",  	#School Enrollment-allocation flag for PESCHENR
                 "PXSCHFT",	    #School Enrollment-allocation flag for PESCHFT
                 "PXSCHLVL",	  #School Enrollment-labor force allocation flag for PESCHLVL
                 "PXSEX",	      #Demographics-allocation flag for PESEX
                 "PXSPOUSE",	  #Demographics-allocation flag for PESPOUSE
                 "QSTNUM",	    #Unique household identifier
                 "RECORD_TYPE",	#Record Type
                 "ucgid",	      #Uniform Census Geography Identifier clause
                 "YYYYMM"	      #Year-Month
  )
}
