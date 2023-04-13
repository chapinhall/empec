#---------------------------------------------------------------------------
## This is simple R-based code to pull poverty-by-race statistics from the 
## ACS 5-year for all tracts in Chicago
## Author: Nick Mader <nmader@chapinhall.org>
#---------------------------------------------------------------------------

# This code is from prior work for DFSS

### Set up workspace -----------------------------------------------------------
grepv <- function(pattern, x) grep(pattern = pattern, x = x, value = TRUE)
cn <- function(x) colnames(x)

### Instantiate key for using Census API ---------------------------------------

api.key.install(census_key) # file = glue("{input_path}key.rda")
  # Note that the `census_key` object is loaded in the `settings--profile.R`

# Looking for tables in the ACS 5-year documentation from Census
  # B17020[A-I] - Poverty Status In The Past 12 Months By Age ... e.g. https://www.socialexplorer.com/data/ACS2012/metadata/?ds=American+Community+Survey+2012&table=B17020A
  # C17020[A-I] - Poverty Status In The Past 12 Months By Age ... distinction with B series is that the age brackets are more aggregated

### Example searches for keywords ----------------------------------------------
acs.lookup(endyear = 2019, span = 5, dataset = "acs", keyword = "poverty")
acs.lookup(endyear = 2019, span = 5, dataset = "acs", keyword = c("poverty", "black"))
  # (Turns out that the indicators of race are in the table name, not variable
  # descriptions, which (I believe) keeps it from coming up in the keyword search)
acs.lookup(endyear = 2019, span = 5, table.number="B17001")
acs.lookup(endyear = 2019, span = 5, table.number="B17001A")
acs.lookup(endyear = 2019, span = 5, table.number="B17020") # Curiously, this table isn't showing up in the API.
                                  # We can use 17001--it'll just take a few extra calculations

### Defining our pull ----------------------------------------------------------

# Note that the elements in these tables can be examined on Social Explorer here:
# https://www.socialexplorer.com/data/ACS2014/metadata/?ds=American+Community+Survey+2014&table=B17001A
pullSt <- my_state_abbr
pullCounties <- "Cook County"
pullPlace <- "Chicago"
pullYears <- 2019
pullSpan <- 5
pullTables <- paste0("B17001", c("", LETTERS[1:9]))

geo.lookup(state=pullSt, county = pullCounties)
geo.lookup(state=pullSt, county = pullPlace)
myGeo <- geo.make(state = pullSt, county = pullCounties, tract = "*")
meta <- acs.lookup(endyear = max(pullYears),
                   span = pullSpan,
                   dataset = "acs",
                   table.number = c("B17001"))
  # XXX Doesn't seem to like multiple arguments to table.number.

demoMap <- c("B17001" = "All", "B17001A" = "WhiteAlone", "B17001B" = "BlackAlone",
             "B17001C" = "AmIndAlone", "B17001D" = "AsianAlone", "B17001E" = "NatAmAlone",
             "B17001F" = "OtherAlone", "B17001G" = "Mult", "B17001H" = "WhiteNonHisp",
             "B17001I" = "Hisp")

### Pull the tables one by one -------------------------------------------------
acsData <- NULL
for (myT in pullTables){
  
  for (pullYear in pullYears) {
    
    print(paste0("Pulling data for table ", myT, ", correspondinding to race ", demoMap[myT], " for year ", pullYear))
    
    # Pull data
    acsPull <- acs.fetch(endyear = pullYear, span = 5, geography = myGeo, table.number = myT)
    acsEst <- acsPull@estimate
    acsEst.df <- data.frame(acsEst)
    
    # Simplify column names
    fieldMap <- meta@results[, c("variable.code", "variable.name")] %>%
      within(variable.name <- gsub("Income in the past 12 months (.+) poverty level: ", "\\1 pov: ", variable.name))
    colnames(acsEst.df) <- fieldMap$variable.name
    
    # Create a variable for tract
    acsEst.df$tract <- acsPull@geography$tract
    rownames(acsEst.df) <- NULL
    
    # Subset data
    acsEst.keep <- 
      acsEst.df %>% 
      select("tract", grepv("below pov.+ 5 years", colnames(acsEst.df))) %>% 
      mutate(demo = demoMap[myT],
             year = pullYear)
    
    # Pull data together
    acsData <- rbind(acsData, acsEst.keep)
  } # close loop across years
} # close loop across tables

### GENERATE CALCULATIONS OF RACE TOTALS AND PROPORTIONS -----------------------

### Get totals of children in poverty by demographic
acsData <-
  acsData %>% 
  mutate(nPovLe5 = `below pov: Male: Under 5 years` +
                   `below pov: Male: 5 years` +
                   `below pov: Female: Under 5 years` +
                   `below pov: Female: 5 years`)

povData <- select(acsData, tract, year, demo, nPovLe5)
povLe5_ty.r <- 
  pivot_wider(povData,
              names_from = demo, values_from = nPovLe5)

### Apportion race totals into reported categories and percentages -------------
# Regarding apportioning Hispanic populations, see the census report http://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf 
# regarding patterns of joint distribution of self-reports of race and Hispanic ethnicity.
#
# The categories that we must report are White, Black, Hispanic, Asian, and Other. Based on the report,
# we count Hispanics as all those self-reporting Hispanic, as well as a portion of single and multiple
# race individuals. Except for white non-Hispanic counts which already have Hispanic individuals 
# separte, We will portion the other race groups based on Table 2 of the above report. See
# the file "./Census Breakdown of Race and Hispanic Ethnicity.xlsx" which creates these proportions
# from that table

povLe5Split_ty.r <- 
  povLe5_ty.r %>% 
  mutate(nHisp      = Hisp,
         nWhiteNonH = WhiteNonHisp,
         nBlackNonH = 0.968*BlackAlone,
         nAsianNonH = 0.986*AsianAlone,
         nOtherNonH = 0.766*AmIndAlone + 0.892*NatAmAlone + 0.032*OtherAlone + 0.662*Mult,
         nAll = nHisp + nWhiteNonH + nBlackNonH + nAsianNonH + nOtherNonH)

# Check this rough split
head(select(povLe5Split_ty.r, All, nAll))
with(povLe5Split_ty.r, cor(All, nAll))

races <- c("Hisp", "WhiteNonH", "BlackNonH", "AsianNonH", "OtherNonH")
for (r in races){
  povLe5Split_ty.r[, paste0("pct", r)] <- povLe5Split_ty.r[, paste0("n", r)] / povLe5Split_ty.r$nAll
}
summary(rowSums(povLe5Split_ty.r[, paste0("pct", races)]))
povLe5Constr_ty.r <- 
  povLe5Split_ty.r %>% 
  select("tract", "year", grepv("^n|^pct", cn(povLe5Split_ty.r)))

### Aggregate and recalculate values at the CCA level
map_tcp   <- read.csv(glue("{input_path}PUMA_Tract_CCA_equivalency2010.csv"))
map_tcp$tract <- sprintf("%06d", map_tcp$tract)
povLe5Constr_cty.r <- merge(x = povLe5Constr_ty.r,
                            y = map_tcp,
                            by = "tract",
                            all.x = TRUE)

sumNA <- function(...) sum(..., na.rm = TRUE)
povLe5Constr_cy.r <- 
  povLe5Constr_cty.r %>% 
  group_by(cca, year) %>% 
  summarize_at(vars(nAll, nOtherNonH, nAsianNonH, nBlackNonH, nWhiteNonH, nHisp), 
               sum, na.rm = TRUE) %>% 
  within({
    for (r in races){
      assign(paste0("pct", r), get(paste0("n",r)) / nAll)
    }
    rm(r)
  })


### Output ---------------------------------------------------------------------

yearSpan <- paste0(min(pullYears), "-", max(pullYears))
write.csv(povLe5Constr_ty.r, file = paste0(glue("{output_path}percent breakdowns of poverty for kids lt age5 by race -- ACS {yearSpan} -- by tract.csv")), row.names = FALSE)
write.csv(povLe5Constr_cy.r, file = paste0(glue("{output_path}percent breakdowns of poverty for kids lt age5 by race -- ACS {yearSpan} -- by cca.csv")),   row.names = FALSE)
