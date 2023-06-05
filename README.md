# Background

This is a project intended to estimate counts of children eligible for a range of child care subsidies and supports. This is relevant to Chapin Hall's work as part of its federally-funded Child Care Policy Research Partnership (CCPRP) project in partnership with the Illinois Department of Human Services (IDHS) as well as local work in collaboration with the Chicago Department of Family and Support Services (DFSS). Each agency is tasked with managing public funding to ensure adequate access to quality childcare in their jurisdictions, and understanding the quantity of need--and eligibility for public supports--is a key consideration.

E-mail [Nick Mader](mailto:nmader@chapinhall.org) for questions related to this method or this codebase.

# Methodology

See the `run--00--main-doc.Rmd` script for the most recent description of the statistical methodology. That script can be "rendered" in RStudio to produce not just a fully typeset description of methods, but to run all data pulls, development, analysis methods, related methods showing results and diagnostics, and to output table and map files.

# Running This Code

Running this code requires contributors to set up a `settings--profile.R` file with the following structure, which you can copy and paste:

```
### Set Run Information --------------------------------------------------------

# Provide a short tag to use in saving results. This can be used to run different
# analysis, e.g. at the Chicago, Cook County, or IL level, and avoid saving over
# results from a different level. Examples could be "Cook" or "IL" to stay short,
# and also appended with other context e.g. "IL2022" to reflect a new year (or
# sensitivity)
my_output_tag <- "<run tag>"

# Indicate base year of analysis
# This will drive the pull of ACS 1-year data
base_year <- 2021

# Provide the names of the CPS data extract to be used in the script
# `run--01c--prep-cps-data.Rmd`. Include the file name but not the file extension.
cps_raw  <- "cps_000XX" # this is the name given to the IPUMS data extract of Current Population Survey data

### Set Paths -----------------------------------------------------------------

# Below, provide file paths that are places for the code to respectively save
# input files (census data, geographic shapefiles, etc), and output files (such
# as estimates and plots)
# Note: Windows users need to change all "\"s in file paths to "/"s
code_path   <- "<root directory for all this code>"
input_path  <- "<file path where the CPS data will be saved, and other input files will be atuo-downloaded to>"
output_path <- "<file path where all intermediate and final output will be saved to"

### Set API Keys ---------------------------------------------------------------

# Application Program Interfaces (APIs) provide an means to directly and
# automatically pull custom data necessary for these methods to work. Typically,
# data resource APIs require users to register with their service, in order to
# track and monitor usage. Providing your APIs keys here allows the code that
# follows to automatically pull all data that is necessary, without any new
# effort on your behalf, or new development of this code. All that is required
# for updates is for all of this code to be re-run

# Census API Key
# Providing this key allows for automatic pulls of American Community Survey
# 1-year and 5-year data releases.
# An API key can be obtained at this address: https://api.census.gov/data/key_signup.html
census_key <- "xxx"

# FRED API Key
# The "FRED" is the Federal Reserve Economic Data service, maintained by the
# Federal Reserve Bank of St Louis (https://fred.stlouisfed.org/). Use of this
# API key automates pulls of the Consumer Price Index, which allows for auto
# adjustments of poverty thresholds to periods in which households are being
# surveyed
# Instructions for obtaining an API key with FRED can be found here:
#   https://fred.stlouisfed.org/docs/api/api_key.html
fred_key <- "xxx"

### Set Geographies ------------------------------------------------------------

# To download Census data, you need to obtain your own Census API key

# Provide your state's two character postal code
my_state_abbr <- "xx"

# Here, specify the starting digits for zip codes for your select region
# Values can be looked up on this webpage: https://codigo-postal.co/en-us/usa/
# Note that these codes treat zip codes as if they were character strings, and
# not numeric. Thus, to identify Illinois zip codes--which range from 60001 to
# 62999--it's necessary to specify `zcta_starts <- c("60", "61", "62")` and
# not simply `zcta_starts <- "600"`
zcta_starts <- c("XX", "XX", "XX")


# (Optional) -- If choosing to focus analysis on a single county, uncomment the
#   following line and provide its name. Note that providing even just "Cook"
#   is sufficient to identify Cook County
#my_county <- "<county name>"

### (Optional) Provide info for a custom geography of interest -----------------
# For example, in Chicago, information is commonly aggregate to the "Chicago
# Community Area (CCA)" level, of which there are 77, compared to ~800 tracts.
# In Illinois, information is sometimes aggregated to "Service Delivery Areas".
# In other geographies, it may be relevant to aggregate to towns, or other
# administrative boundaries

# To establish this level of aggregation -- uncomment the following lines and
# provide the corresponding information
# 1. Full path to the shapefile to use for building geographic crosswalks and for mapping
  my_aux_geo <- "<full file path, including shape file name and extension>"

# 2. Name of the field within the shapefile with the desired label for each geography
  my_aux_geo_field <- "<name of field in the shape file>"  

# 3. Descriptive label to give to the auxiliary field, e.g. "Community Area"
  my_aux_geo_desc <- "<plain language description to give to `my_aux_geo_field`>"

# 4. (Optional) One among the auxiliary geographies to highlight, e.g. "Cook County"
  # my_aux_geo_focal_val <- "<if desired, single value of `my_aux_geo_field` to highlight in figures>"

### Set Local CCDF Parameters --------------------------------------------------

# Specify name and rules for local CCDF program: abbrev, long name, and income
# threshold as a % of the Federal Poverty Line (e.g. a value of 225 for 225% of FPL)
local_ccdf_name_short <- "<local abbreviation for childcare subsidy, if not 'CCDF'>"
local_ccdf_name_long  <- "<local name for childcare subsidy>"
local_ccdf_thresh <- <numeric value for income-to-poverty line threashold for initial CCDF eligibility>

### Set Other Run Parameters ---------------------------------------------------

# Use only "model" estimates from the Small Area Estimation method, rather than
# the blended estimates from the Fay-Herriott method?
# Users should consider setting this option to `TRUE` based on diagnostics
# examining `share_model` vs `share_direct` estimates, to judge whether
# "model" estimates are often skewed away from direct estimates within PUMAs
# (which would lean towards a setting of `TRUE`) or if they are roughly
# consistent with the "direct" estimates (which would lean towards a setting of
# `FALSE`) meaning that a blended estimate may generate useful moderation and
# greater accuracy
use_only_sae_model_estimates <- TRUE

```

Also note that, throughout the codebase, there are two marker that signal attention to users:

* /!\\ -- this is used in comments that are primarily advisory, explaining a key assumption, decision, or opportunity for future reconsideration
* /*\\ -- this is used to invite users to alter or add to the code. Examples include diagnostics where some local information can be provided, or titling or explanation of figures to describe the patterns seen in the user's own data. Efforts have been made to ensure that the code is structured to allow for multiple users to contribute to a common codebase (e.g. by using conditional statements that select the appropriate figure title based on the `my_output_tag` value relevant to the local context), but for cases where larger deviation is required (e.g. where the codebase is modified to focus on a different public program requiring different data development) we recommend "forking" this repository for parallel development.

# Data Sources and their Uses

## American Community Survey (ACS) 1-Year Data

ACS 1-Year data (ACS1) microdata has information about each member of surveyed households, including age, employment/school attendance status, and income. These are used to build estimated counts and shares of children in CCAP-eligible households.
However, ACS1 data are only as geographically specific as the Census Public Use Microdata Code (PUMA) level, which reflects areas containing a minimum of 100,000 individuals. For example, there are 17 PUMAs inside of Chicago, but also 15 PUMAs in Illinois that span multiple counties.

## American Community Survey (ACS) 5-Year Data

ACS 5-Year data (ACS5) are aggregated across five consecutive years of ACS survey data, gaining sample size--and thus greater ability and validity--for reporting at geographically more granular levels. We focus on tables released at the Census tract level, which are intended to represent areas about 4,000 inhabitants on average. With that definition, Census tracts naturally vary in size across urban and rural contexts. However, the ACS5 data are released as aggregate tables -- such as estimated number of kids under each federal poverty level -- and not microdata. While CCAP eligibility is not reflected, the ACS5 data provides information useful for distinguishing the general socioeconomic characteristics of smaller geographic areas.

## Current Population Survey (CPS)

CPS collects information about each member of surveyed households on a monthly basis, providing recent information, only 1-2 months lagged. However, CPS does not provide information at a geographically granular level. CPS is used to build statistical relationships between household socioeconomic status baseline year and CCAP eligibility as of its most recent available data, which is applied to estimate Census-tract level CCAP eligibility as of the most recent month of CPS data.

The CPS data were downloaded interactively from the [IPUMS CPS](https://cps.ipums.org/cps/) (originally "Integrated Public Use Microdata Series") website. Annual Supplementary Economic Characteristics (ASEC) data and Basic Monthly Data were pulled for 2019 and all months forward for the current exercise, and for both 2007-2009 for the sake of validating our method using data from throughout the Great Recession.

Before making selections of variables, it is necessary within Sample Selections to select the "Cross Sectional" option. This makes it possible to download the Basic Monthly Files which have a more complex longitudinal structure than the Annual Social and Economic Supplement (ASEC) sample. See [this article](https://www.census.gov/topics/population/foreign-born/guidance/cps-guidance/cps-vs-asec.html) for a comparison of the general CPS versus ASEC samples. This selection must be made before selecting variables. Otherwise the existing selections will be lost.

The following fields were pulled:

* HOUSEHOLD
	- TECHNICAL: YEAR, SERIAL, MONTH, HWTFINL, ASECFLAG
	- GEOGRAPHIC: STATEFIP, METRO, METAREA, COUNTY
	- ECONOMIC: HHINCOME, FAMINC
* PERSON (all included under "CORE")
	- DEMOGRAPHIC: RELATE, AGE, SEX, RACE, MARST
	- FAMILY INTERRELATIONSHIP: MOMLOC, MOMLOC2, POPLOC, POPLOC2, SPLOC, FAMUNIT
	- ETHNICITY/NATIVITY: HISPAN
	- WORK: EMPSTAT, LABFORCE, OCC, IND, CLASSWKR
	- EDUCATION: EDUC, SCHLCOLL
	- TECHNICAL: PERNUM, WTFINL, FAMID
* PERSON (included under "ANNUAL SOCIAL & ECONOMIC SUPPLEMENT (ASEC)")	
	- INCOME: INCTOT, INCWAGE, INCUNEMP
	- POVERTY: POVERTY

Be sure to download the DDI file which has documentation of fields in this pull. To do so, click on the "DDI" link, and right-click to "Save As" this file to the "input/" subfolder within the repository.

## Poverty Guidelines data

Poverty Guidelines information is a key determinant of eligibility for child care supports. This information is drawn from [this source](https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/).


# Code Files

Code files in this repository include:

## Settings Scripts

* `settings--main.R` -- This script loads libraries, functions, and visual standards whose use is common across multiple scripts below.
* `settings--profile.R` -- This script sets parameters specific to each user's context, both in terms of computing, and in terms of geographic state of focus. See the `Running this Code` section above.

## Pull Scripts

These files can be run manually, but are generally set up to be run automatically by the `run` scripts below that require their output.

* `pull--acs1-microdata.R` -- automatically pulls ACS1 microdata using the Census API
* `pull--acs5-general-data.R` -- pulls and processes many ACS5 tables of interest, used in the Small Area Estimation method
* `pull--acs5-data-on-poverty-by-race.R` -- a select subset of ACS5 data with information about poverty-by-race data, using in postprocessing steps to provide estimated racial breakdowns of eligibility counts. In the future, this can incorporated into the `pull--acs5-general-data.R` script.

## Run Scripts

* `run--00--main-doc.Rmd` -- run and incorporate all following scripts into a main document
* `run--01a--prep-geo-data.Rmd` -- this auto-pulls shape files from the Census TIGER service based on state--and if specified, county--indications in the `settings--profile.R` script, and produces useful cross-walks between tracts and PUMA, zip codes, and (if specified) custom geographies
* `run--01b--prep-acs1-data.Rmd` -- reads pulls of ACS1 microdata and develops necessary structures for use in the Small Area Estimation method
* `run--01c--prep-cps-data.Rmd` -- reads pulls of CPS microdata (instructions provided above) and develops necessary structures for use in the "now-casting" method
* `run--01d--prep-acs5-data.Rmd` -- adds onto the output from the `pull--acs5-general-data.R` script, adding some additional fields, and aggregating calculations and standard errors up to the PUMA level
* `run--01e--prep-pop-by-age-data.Rmd` -- pulls data from the Census redistricting files, SF1 Census files, and ACS5 to get population by age group to use in projecting population counts for young children
* `run--02a--run-and-validate-SAE.Rmd` -- run a range of SAE specifications for a range of measures--including share of households by income-to-poverty ratio, and measures related to predicting CCDF eligibility--and examine their properties to gauge their individual reliability, and compare their output
* `run--02b--run-and-validate-nowcasting.Rmd` -- run a range of "now-casting" analyses, apply them to baseline+SAE measures to estimate counts, and examine patterns and maps of the output.
* `run--03a--postestimation-display-and-output.Rmd` -- after selecting sensitivities based on output guidance from the `02` scripts, this script examines patterns of the results, and outputs the final estimates in maps and Excel tables
* `run--03b--postestimation-disaggregate-estimates.Rmd` -- this file current generates some additional disaggregations of methods by age and by race/ethnicity

## Methods Scripts

* `method--general-helper-functions.R` -- these are general functions used throughout the scripts, including processing for ACS data, calculating income-to-poverty ratios, and converting age, income, and industry codes into categorical groups/classifications
* `method--small-area-estimation-functions.Rmd` -- run Small Area Estimation (SAE) method to estimate, in prior years, counts of program-eligible children for small geographies
* `method--nowcasting-functions.Rmd` -- these are functions that use CPS data to "now-cast" the Small Area counts to a recent month

## Other Diagnostic Scripts

The diagnostic scripts generally represent one-time examinations that were run to examine a phenomenon, motivate a choice, or build a comparison (e.g. of output sensitivities) on occasions of interest. While these were neither common or central enough to be of interest in the main report (although perhaps several could belong as appendix materials in the future), they were meaningful enough to keep around for recordkeeping or occasional use.

* `diagnostic--basictabs_cps.Rmd` -- this investigates the size of the CPS, and in effect establishes the fact that given a focus on children under age 5, it is necessary to pool households across the US to maximize sample size to characterize recent trends in household economic dynamics
* `diagnostic--analyze_familyincome_cps.Rmd` -- this investigates the ability for the CPS to provide income characterizations useful for determining CCAP program eligibility
* `diagnostic--validate_estimates.R` -- build comparison between new statistical estimates and ACS microdata estimates
* `diagnostic--validate_estimates.Rmd` -- displays comparisons built in `diagnostic--validate_estimates.R`

## Sandbox Scripts

* `sandbox--emdi_package_functionality.Rmd` -- this file used as a place to explore functions, output, speed, and specifications of functions in the `emdi` package, which generates the Small Area Estimation output
