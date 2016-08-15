################################################################################
# read.unesco.data.R
################################################################################
# Download the latest data from UNESCO and concatenate it into a large, melted
# data frame. Requires init.R to ahve been run.
################################################################################

unesco.data <- list()
for(i in 1:nrow(indicators)) {
  print(i)
  raw.data <-
    as.data.frame(
      readSDMX(
        buildSDMXquery(
          indicators$url[i]
        )
      )
    )
  
  unesco.data <- rbind(
    unesco.data,
    data.frame(
      indicator = indicators$name[i],
      location  = raw.data$LOCATION,
      date      = as.integer(raw.data$obsTime),
      value     = raw.data$obsValue * indicators$prefactor[i]
    )
  )
}
rm(raw.data)