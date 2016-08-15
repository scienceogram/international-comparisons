################################################################################
# most.recent.and.groups.R
################################################################################
# This script takes the UNESCO research indicators and finds the most recent
# values (as determined by the most recent GERD financed by government figure)
# and creates a table of compound indicators for every country. Then, it
# aggregates those indicators across groups of countries.
################################################################################
# User-defined variables
################################################################################
start.date <- 1996
end.date   <- 2014
################################################################################
# End user-defined variables
################################################################################

source('init.R')
source('read.unesco.data.R')

# find most recent GERD financed by government values as a basis for aggregation
aggregate.mostrecent <-
  aggregate(
    date ~ location,
    unesco.data[unesco.data$indicator == 'gerd.gov.ppp', ],
    FUN=max
  )

# attach the full-length country name
aggregate.mostrecent$country.name <-
  countrycode(aggregate.mostrecent$location, 'iso3c', 'country.name')

# loop over the indicators, adding columns for each value as measured in the
# 'most recent' year just defined
for(indicator in indicators$name) {
  aggregate.mostrecent <-
    merge(
      aggregate.mostrecent,
      unesco.data[unesco.data$indicator == indicator, ],
      all.x = TRUE, all.y = FALSE
    )
  # this will be a melted data frame, so remove the 'indicator' column...
  aggregate.mostrecent$indicator <- NULL
  # ...and rename the 'value' column
  names(aggregate.mostrecent)[
    names(aggregate.mostrecent) == 'value'
    ] <- indicator
}

# save the resulting table
write.csv(aggregate.mostrecent,
          file.path(output.path, paste0('most-recent-gerd', data.ext)))

# get the names of the country group tables
country.group.files <- 
  list.files(path = country.groups.path, pattern = data.ext,
             full.names = TRUE)
# loop over the groups, creating a large data frame of groups and members
country.groups <- data.frame()
for(country.group.file in country.group.files) {
  country.groups <- rbind(
    country.groups,
    data.frame(
      # the filename is the country group name
      name = justFilename(basename(country.group.file)),
      # just get the country codes...no supplementary data if they exist
      country.code = read.csv(country.group.file)$country.code
    )
  )
}

# begin the nested looping to create means of all variables of interest by
# country grouping
country.group.means <- data.frame()
# loop over the indicators we're going to compound, ie divide by things by
# choosing those whose role is 'variable'
for(indicator in indicators$name[indicators$role == 'variable']) {
  # and loop over the things we're dividing them by
  for(divisor in indicators$name[indicators$role == 'divisor']) {
    # create a name for this compound indicator by combining the indicator name
    # with what it's being divided by
    indicator.name <- paste0(indicator, '.', divisor)
    # calculate values by dividing the indicator by the divisor
    aggregate.mostrecent[, indicator.name] <-
      aggregate.mostrecent[, indicator] / aggregate.mostrecent[, divisor]
    # having created the new compound variable, loop over country groups and
    # calculate the appropriately-weighted mean
    for(country.group in unique(country.groups$name)) {
      # get a vector specifying which countries are in the group
      country.group.select <-
        aggregate.mostrecent$location %in% 
        country.groups$country.code[country.groups$name == country.group]
      country.group.means <- rbind(
        country.group.means,
        data.frame(
          name = country.group,
          indicator = indicator.name,
          value = 
            weightedMeanPlus(
              aggregate.mostrecent[country.group.select, indicator.name],
              aggregate.mostrecent[country.group.select, divisor],
              na.rm = TRUE
            )
        )
      )
    }
  }
}

# save the resulting table
write.csv(country.group.means,
          file.path(output.path,
                    paste0('most-recent-country-group-means', data.ext)))