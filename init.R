################################################################################
# init.R
################################################################################
# Load packages and scripts and initialise common variables for other scripts.
################################################################################

input.path <- 'input'
data.ext  <- '.csv'
country.groups.path <- file.path(input.path, 'country-groups')

output.path <- 'output'

# This no longer works because the UNESCO site returns a blank XML document if
# you request countries which aren't present. It would be nice to automate it
# again to save updating the two CSV files by hand every time.
# 
# all.countries.query.string <- paste0(
#   read.csv(
#     file.path(country.groups.path, paste0('all', data.ext))
#   )$country.code,
#   collapse = '+'
# )

# Instead, use two separate manually curated files with countries reporting
# these stats...
# All countries reporting GDP
all.gdp <- read.csv(
  file.path(country.groups.path, paste0('all-gdp', data.ext)), as.is=TRUE
)$country.code
# All countries reporting GERD
all.gerd <- read.csv(
  file.path(country.groups.path, paste0('all-gerd', data.ext)), as.is=TRUE
)$country.code

# All countries then becomes only those which report both GDP and GERD.
all.countries <- all.gdp[all.gdp %in% all.gerd]

# Make a query string in the form ALB+AFG+...
all.countries.query.string <- paste0(all.countries, collapse = '+')

indicators <- read.csv(
  file.path(input.path, paste0('indicators', data.ext))
)

source('handy.R')
requirePlus('rsdmx', 'countrycode')

buildSDMXquery <- function(indicator, start = start.date, end = end.date) {
  print(paste0(
    'http://data.uis.unesco.org/RestSDMX/sdmx.ashx/GetData/',
    indicator, '.',
    all.countries.query.string,
    '/all?startTime=', start.date, '&endTime=', end.date
  ))
}