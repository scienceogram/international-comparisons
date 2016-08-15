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
end.date   <- 2015
################################################################################
# End user-defined variables
################################################################################

source('init.R')
source('read.unesco.data.R')

# aggregate by year and country
aggregate.all.time <-
  data.frame(
      location = rep(levels(unesco.data$location), each = end.date-start.date+1),
      date = rep(start.date:end.date, length(levels(unesco.data$location)))
    )

# attach the full-length country name
aggregate.all.time$country.name <-
  countrycode(aggregate.all.time$location, 'iso3c', 'country.name')

# loop over the indicators, adding columns for each value as measured in the
# 'most recent' year just defined
for(indicator in indicators$name) {
  aggregate.all.time <-
    merge(
      aggregate.all.time,
      unesco.data[unesco.data$indicator == indicator, ],
      all.x = TRUE, all.y = FALSE
    )
  # this will be a melted data frame, so remove the 'indicator' column...
  aggregate.all.time$indicator <- NULL
  # ...and rename the 'value' column
  names(aggregate.all.time)[
    names(aggregate.all.time) == 'value'
    ] <- indicator
}

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
# hack to fix Italy...
aggregate.all.time$gerd.gov.ppp[aggregate.all.time$location == 'ITA' &
                                  aggregate.all.time$date < 2012] <-
  na.approx(aggregate.all.time$gerd.gov.ppp[aggregate.all.time$location == 'ITA'&
                                              aggregate.all.time$date < 2012])

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
    aggregate.all.time[, indicator.name] <-
      aggregate.all.time[, indicator] / aggregate.all.time[, divisor]
    # having created the new compound variable, loop over country groups and
    # calculate the appropriately-weighted mean
    for(country.group in unique(country.groups$name)) {
      # get a vector specifying which countries are in the group
      country.group.select <-
        aggregate.all.time$location %in% 
        country.groups$country.code[country.groups$name == country.group]
      country.group.means <- rbind(
        country.group.means,
        data.frame(
          name = country.group,
          indicator = indicator.name,
          value = 
            weighted.mean(
              aggregate.all.time[country.group.select, indicator.name],
              aggregate.all.time[country.group.select, divisor]
            )
        )
      )
    }
  }
}

g8.average <- data.frame()
for(year in 1996:2012) {
  countries.this.year <-
    aggregate.all.time$location %in% 
    country.groups$country.code[country.groups$name == 'G8'] &
    aggregate.all.time$date == year
  g8.average <- rbind(
    g8.average,
    data.frame(
      date  = year,
      value = 
        weighted.mean(
          aggregate.all.time$gerd.gov.ppp.gdp.ppp[countries.this.year],
          aggregate.all.time$gdp.ppp[countries.this.year]
        )
    )
  )
}

require(ggplot2)

# make there be hundreds of extra points for compound path goodness
n.points <- 1000
just.g8.interpolated <-
  data.frame(
    location = rep(country.groups$country.code[country.groups$name == 'G8'], each = n.points),
    date = rep(seq(1996,2012,length.out=n.points), length(country.groups$country.code[country.groups$name == 'G8']))
  )
just.g8.interpolated$gdp.ppp <- NA
just.g8.interpolated$gerd.gov.ppp.gdp.ppp <- NA
xout <- seq(1996,2012,length.out=n.points)
for(country in country.groups$country.code[country.groups$name == 'G8']) {
  just.g8.interpolated$gdp.ppp[just.g8.interpolated$location==country] <-
    approx(aggregate.all.time$date[aggregate.all.time$location == country],
           aggregate.all.time$gdp.ppp[aggregate.all.time$location == country],
           xout)$y
  just.g8.interpolated$gerd.gov.ppp.gdp.ppp[just.g8.interpolated$location==country] <-
    approx(aggregate.all.time$date[aggregate.all.time$location == country],
           aggregate.all.time$gerd.gov.ppp.gdp.ppp[aggregate.all.time$location == country],
           xout)$y
}

ggplot(just.g8.interpolated) +
  geom_point(aes(x=date, y=gerd.gov.ppp.gdp.ppp, size=gdp.ppp, colour=location)) +
  geom_line(data = g8.average, aes(x =date, y=value)) +
  geom_text(data = aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date == 2011,
    ],
    aes(label=country.name, x=2013, y=gerd.gov.ppp.gdp.ppp)) +
  ylim(0, 0.01)



ggplot(
  aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date > 0,
    ]
) +
  geom_line(  aes(x = date,
                  y = gerd.gov.ppp.gdp.ppp,
                  size = log(gdp.ppp),
                  group = location)) +
  geom_line(data = g8.average, aes(x =date, y=value)) +
  geom_text(data = aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date == 2011,
    ],
    aes(label=country.name, x=2013, y=gerd.gov.ppp.gdp.ppp)) +
  ylim(0, 0.01)



ggplot(
  aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date > 0,
    ]
) +
  geom_line(  aes(x = date,
                  y = gerd.gov.ppp.gdp.ppp,
                  size = log(gdp.ppp),
                  group = location)) +
  geom_line(data = g8.average, aes(x =date, y=value)) +
  geom_text(data = aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date == 2011,
    ],
    aes(label=country.name, x=2013, y=gerd.gov.ppp.gdp.ppp)) +
  ylim(0, 0.01)


ggplot(
  aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date > 0,
    ]
) +
  geom_line(  aes(x = date,
                  y = log(gdp.ppp),
                  group = location,
                  colour=location)) +
  geom_text(data = aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date == 2011,
    ],
    aes(label=country.name, x=2013, y=log(gdp.ppp), colour=location))

require(plyr)

ggplot(
  aggregate.all.time[
    aggregate.all.time$location %in%
      country.groups$country.code[country.groups$name == 'G8'] &
      aggregate.all.time$date > 0,
    ]
) +
  geom_line(  aes(x = gdp.ppp,
                  y = gerd.gov.ppp,
                  group = location,
                  colour=location,
                  order = date)) +
  geom_point(  aes(x = gdp.ppp,
                  y = gerd.gov.ppp,
                  group = location,
                  colour=location,
                  order = date)) +
  geom_abline(intercept=0, slope=0.01) +
  scale_x_log10() +
  scale_y_log10()