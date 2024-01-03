
### Install packages -----------------------------------------------------------
# Due to potential name conflicts (e.g. often with select() or filter()), the
# data manipulation packages--notably dplyr--are loaded last to ensure that
# their versions of these functions are not masked

# Start by installing a package to help search for whether already-installed
# packages are the most recent
install.packages("pkgsearch")
library(pkgsearch)

packages.list <- 
  c("knitr", "ggplot2", "Rcpp", "Hmisc", "VGAM", "stringr", "glmnet", "censusapi",
    "plotly", "crosstalk", "DT", "kableExtra", "sae", "emdi", "glue", "forcats", 
    "zoo", "moments", "leaflet", "RColorBrewer", "ggalluvial", "tigris",
    "extrafont", "remotes", "broom", "sf", "lwgeom", "foreach", "doParallel",
    "xlsx", "tidycensus", "fredr", "ipumsr", "tsibble", "forecast", "fable", 
    "boot", "ggtext", "data.table", "tidyr", "dplyr", "scales", "janitor", "R.utils")

ips <- as.data.frame(installed.packages())

for (p in packages.list) {
  # Check if the package is installed. If not, install it.
  # If so, check whether it is the most recent version of the package.
  if (!p %in% ips[, "Package"]) {
    install.packages(p)
  } else {
    p_ver <- ips[ips$Package == p, "Version"]
    p_srch <- pkg_search(p)
    cran_ver <- p_srch[p_srch$package == p, "version"]
    if (p_ver != cran_ver) install.packages(p)
  }
  
  # Load the package quietly
  suppressPackageStartupMessages(library(p, 
                                         character.only = TRUE,
                                         verbose = FALSE))
}
devtools::install_github("CT-Data-Haven/cwi")
library(cwi)

# Suppress warnings from dplyr::summarise() with group_by()s
options(dplyr.summarise.inform = FALSE)

meanNA <- function(x, ...) mean(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 sumNA <- function(x, ...)  sum(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 minNA <- function(x, ...)  min(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 maxNA <- function(x, ...)  max(x = x[!is.infinite(x)], ..., na.rm = TRUE)
 varNA <- function(x, ...)  var(x = x[!is.infinite(x)], ..., na.rm = TRUE)
  sdNA <- function(x, ...)   sd(x = x[!is.infinite(x)], ..., na.rm = TRUE)
n_nonmiss   <- function(x) sum(!is.na(x))
pct_nonmiss <- function(x) mean(!is.na(x))
bound_val <- function(x, lower, upper) pmin(x, upper) %>% pmax(., lower)
 
whats_big <- function() {
  sapply(ls(), 
         function(x) object.size(get(x))) %>% 
    sort()
}

### Generate aliases for select functions --------------------------------------
cn <- function(x) colnames(x)

### Install fonts --------------------------------------------------------------

# See the following two posts related to installation, and troubleshooting a error
# * https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
# * https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext

if (FALSE) { # (!any(str_detect(windowsFonts(), "Segoe UI"))) {
  library(remotes)
  remotes::install_version("Rttf2pt1", version = "1.3.8")
    # Am having trouble installing this. Error is "cannot remove earlier installation"
    # for now, removing use of custom font
  extrafont::font_import()
  loadfonts(device = "win")
}

### Set visual branding standards ----------------------------------------------
chHex1 <- rgb(128,   0,   0, maxColorValue = 255)
chHex2 <- rgb( 33,  51, 104, maxColorValue = 255)
chHex3 <- rgb( 42,  92, 170, maxColorValue = 255)
chHex4 <- rgb(  0, 156, 222, maxColorValue = 255)
chHexs <- c(chHex1, chHex2, chHex3, chHex4)

font_family = "Segoe UI" # "Arial" 
windowsFonts("Arial" = windowsFont("Arial"),
             "Segoe UI" = windowsFont("Segoe UI"))
  # Undertaking this step based on the input from these GitHub issue comments
  # https://github.com/GuangchuangYu/meme/issues/1
  
geom_45 <- function(...) {
  geom_abline(intercept = 0,
              slope = 1,
              ...)
}

myTheme <- 
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title    = element_text(size = 16, family = font_family), # , hjust = 0.5
        plot.subtitle = element_text(size = 14, family = font_family), # , hjust = 0.5
        axis.text     = element_text(size = 11, family = font_family),
        axis.title    = element_text(size = 12, family = font_family),
        legend.text   = element_text(size = 12, family = font_family),
        strip.text    = element_text(size = 12, family = font_family, margin = margin()),
        strip.background = element_rect(fill = "lightgray", color = NA),
        #legend.title = element_blank(),
        axis.title.x = element_blank())
