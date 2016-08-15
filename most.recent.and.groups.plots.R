################################################################################
# most.recent.and.groups.plots.R
################################################################################
# Takes UNESCO research indicators and compound indicators and creates some
# simple plots.
################################################################################

source('most.recent.and.groups.R')

requirePlus('ggplot2')

# get the top 20 public R&D spenders per capita
ggplot(
  aggregate.mostrecent[
    order(aggregate.mostrecent$gerd.gov.ppp.total.pop, decreasing = TRUE)[1:20],
    ],
  aes(
    x = factor(location, levels = location[20:1]),
    y = gerd.gov.ppp.total.pop
  )
) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = 0, label = country.name), angle = 90, colour = 'white',
            hjust = 0)

# get the top 20 public R&D spenders per GDP
ggplot(
  aggregate.mostrecent[
    order(aggregate.mostrecent$gerd.gov.ppp.gdp.ppp, decreasing = TRUE)[1:20],
    ],
  aes(
    x = factor(location, levels = location[20:1]),
    y = gerd.gov.ppp.gdp.ppp
  )
) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = 0, label = country.name), angle = 90, colour = 'white',
            hjust = 0)

# public R&D spending as a fraction of GDP for common country groups
ggplot(
  country.group.means[
    country.group.means$indicator == 'gerd.gov.ppp.gdp.ppp',
    ],
  aes(
    x = name,
    y = value
  )
) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = 0, label = name), angle = 90, colour = 'white',
            hjust = 0)