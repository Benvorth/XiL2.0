# install.packages("RMySQL")
library(RMySQL)

mydb <- dbConnect(MySQL(), user='', password='', dbname='canlog', host='localhost')

rs <- dbSendQuery(mydb, 
  "SELECT
    `TIMESTAMP`,   
	IF(`CAN` = 'POWERTRAIN' AND  `SIGNAL_NAME` = 'AccelPdlPosn_Cval', `SIGNAL_VALUE`, NULL) as value0,
	IF(`CAN` = 'POWERTRAIN' AND  `SIGNAL_NAME` = 'FuelMassFlowRate_Cval_PT', `SIGNAL_VALUE`, NULL) as value1,
	IF(`CAN` = 'POWERTRAIN' AND  `SIGNAL_NAME` = 'AirMass_Cval_PT', `SIGNAL_VALUE`, NULL) as value2 
  FROM 
    `CANLOG`.`CANLOG_DRIVE` 
  WHERE
    (`CAN` = 'POWERTRAIN' AND `SIGNAL_NAME` = 'AccelPdlPosn_Cval') OR 
	(`CAN` = 'POWERTRAIN' AND `SIGNAL_NAME` = 'FuelMassFlowRate_Cval_PT') OR 
	(`CAN` = 'POWERTRAIN' AND `SIGNAL_NAME` = 'AirMass_Cval_PT') 
  ORDER BY 
    1
  ")

data <- fetch(rs, n=-1)

# clean data - replace units & convert from "factor" to "numeric"
data <- as.data.frame(lapply(data, function(x) {
  as.numeric(as.character(gsub("[ %kg/rh]", "", x)))
}))


findLastExistingValue <- function (measuedValues, timestamps, interpolatePosition) {
  
  lastExistingValue <- NA
  lastExistingTimestampWithValue <- NA
  lastExistingTimestampIndexWithValue <- NA
  
  i <- interpolatePosition - 1
  # browser()
  while (i >= 1) {
    prevValue <- as.numeric(as.character(measuedValues[i])) #https://stackoverflow.com/questions/15013482/whats-wrong-with-as-numeric-in-r
	# cat('i ', i, ' lev: prevValue: ', prevValue, '\n')
	if (!is.na(prevValue)) {
	  # cat('i ', i, ' lev: prevValue: ', prevValue, '\n')
	  lastExistingTimestampIndexWithValue <- i
	  lastExistingTimestampWithValue <- as.numeric(as.character(timestamps[i]))
	  lastExistingValue <- prevValue
	  break
	}  
    i <- i - 1
  }
  # cat('lev: ', lastExistingValue, ' @ ', lastExistingTimestampIndexWithValue, '\n')
  my_list <- list(
      "value" = lastExistingValue, 
	  "timestamp" = lastExistingTimestampWithValue, 
	  "index" = lastExistingTimestampIndexWithValue
  )
  return(my_list)  
}

findNextExistingValue <- function (measuedValues, timestamps, interpolatePosition) {
  
  nextExistingValue <- NA
  nextExistingTimestampWithValue <- NA
  nextExistingTimestampIndexWithValue <- NA
  
  i <- interpolatePosition + 1
  while (i <= length(measuedValues)) {
    prevValue <- as.numeric(as.character(measuedValues[i]))
	if (!is.na(prevValue)) {
	  # cat('i ', i, ' nev: prevValue: ', prevValue, '\n')
	  nextExistingTimestampIndexWithValue <- i
	  nextExistingTimestampWithValue <- as.numeric(as.character(timestamps[i]))
	  nextExistingValue <- prevValue
	  break
	}  
    i <- i + 1
  }
  # cat('nev: ', nextExistingValue, ' @ ', nextExistingTimestampIndexWithValue, '\n')
  my_list <- list(
      "value" = nextExistingValue, 
	  "timestamp" = nextExistingTimestampWithValue, 
	  "index" = nextExistingTimestampIndexWithValue
  )
  return(my_list)  
}

interpolateValue <- function (measuedValues, timestamps, interpolatePosition) {

  lastExistingValue <- findLastExistingValue(measuedValues, timestamps, interpolatePosition)
  nextExistingValue <- findNextExistingValue(measuedValues, timestamps, interpolatePosition)
  # cat('lastExistingValue ', lastExistingValue$value, '\n')
  # cat('nextExistingValue ', nextExistingValue$value, '\n')  
  if (!is.na(lastExistingValue$value) && !is.na(nextExistingValue$value)) {
	a <- as.numeric(as.character(timestamps[interpolatePosition])) - lastExistingValue$timestamp
	b <- nextExistingValue$timestamp - as.numeric(as.character(timestamps[interpolatePosition]))

	interpolateValue <- (b * lastExistingValue$value + a * nextExistingValue$value) / (a + b)

	return(interpolateValue)
  } else {
	return(NA)
  }
}

interpolateValues <- function(data) {
  j <- 2
  while (j <= 4) {
    theTimestamps <- data[, 1] # first col, all rows
    measuedValues <- data[, j] # returns a numeric vector, https://stackoverflow.com/questions/21025609/how-do-i-extract-a-single-column-from-a-data-frame-as-a-data-frame
    k <- 1
    cat('Work on column ', j, '...\n')
    while (k <= nrow(data)) {
      value <- data[k, j]
	  if (is.na(value)&& k > 1) {
	    # cat(k, ' - valueIn: ', value)
	    value <- interpolateValue(measuedValues, theTimestamps, k)
	    data[k, j] = value
	    # cat(' - value out: ', value, '\n')
	  }
	  k <- k + 1
	  if (k %% 100 == 0) {
	    cat(k, '\r lines done\r')
	  }	
    }
    cat('\r', k, ' lines done\n')
    j <- j + 1
  }
}

# interpolate missing values on each timeseries
interpolateValues(data)

# filter unmeaningful Data (change boundaries in other data...)
data <- data[!(data$TIMESTAMP < 10),]
data <- data[!(data$TIMESTAMP > 30),]
data <- data[!(data$TIMESTAMP %in% NA),]


