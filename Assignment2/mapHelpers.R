cities_map <- function(shootings, years){
  # Prepare data
  shootings$city_state <- paste(shootings$city, shootings$state, sep=" ")
  data <-count(shootings[2014 + as.integer(shootings$year) >= years[1] & 2014 + as.integer(shootings$year) <= years[2],], 'city_state')
  data <- merge(data, us.cities, by.x="city_state", by.y="name",no.dups = TRUE, suffixes = c("",""))
  return(data)
}

victims_map <- function(shootings, gender, race, age) {
  
  # Filter data 
  gen <- switch(gender, 
                   "Male and Female" = "male and female",
                   "Male" = "M",
                   "Female" = "F")
  
  if(gen == "male and female" & race == "All races"){
    data <-count(shootings[shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else if(gen == "male and female"){
    data <-count(shootings[shootings$race == race & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else if(race == "All races"){
    data <-count(shootings[shootings$gender == gen & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else {
    data <-count(shootings[shootings$gender == gen & shootings$race == race & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }



colnames(data) <- c("region", "value")
data$region <- state.name[match(data$region,state.abb)]
data$region <- tolower(data$region)
data <- rbind(data, subset(data.frame(region = tolower(state.name), value = replicate(length(state.name), 0)), !(region %in% data$region)))
sum = sum(data$value)
state_choropleth(data, num_colors = 9, title = paste0("US police shootings 2015-2020, victim's profile:\n", race, ", ", gen, ", age between ", age[1], " and ", age[2]), legend = paste0("Total shootings: ", sum))

}

season_map <- function(shootings, season, filt) {
  
  if(season == "Day of the week") {
    day_n <- switch(filt, 
                    "Week days" = "wd",
                    "Weekend" = "we",
                    "Monday" = "1", 
                    "Tuesday" = "2",
                    "Wednesday" = "3",
                    "Thursday" = "4",
                    "Friday" = "5", 
                    "Saturday" = "6",
                    "Sunday" = "7")
     if(day_n == "wd") {
      data <-count(shootings[shootings$day == "1" | shootings$day == "2" | shootings$day == "3" | shootings$day == "4" | shootings$day == "5" , ],"state" )
    } else if (day_n == "we") {
      data <-count(shootings[shootings$day == "6" | shootings$day == "7" ,], "state")
    } else {
      data <-count(shootings[shootings$day == day_n, ], "state")
    }
    
  }
  else if (season == "Month") {
    month_n <- switch(filt, 
                      "January" = "1",
                      "February" = "2",
                      "March" = "3", 
                      "April" = "4",
                      "May" = "5",
                      "June" = "6",
                      "July" = "7", 
                      "August" = "8",
                      "september" = "9", 
                      "October" = "10",
                      "November" = "11", 
                      "December" = "12")
     data <-count(shootings[shootings$month == month_n, ], "state")
  }
  else if (season == "Season") {
    if(filt == "Spring: Mar, Apr, May") {
      data <-count(shootings[shootings$month == "3" | shootings$month == "4" | shootings$month == "5" , ], "state")
    } else if(filt == "Summer: Jun, Jul, Aug") {
      data <-count(shootings[shootings$month == "6" | shootings$month == "7" | shootings$month == "8" , ], "state")
    } else if(filt == "Autumn: Sep, Oct, Nov") {
      data <-count(shootings[shootings$month == "9" | shootings$month == "10" | shootings$month == "11" , ], "state")
    } else {
      data <-count(shootings[shootings$month == "12" | shootings$month == "1" | shootings$month == "2" , ], "state")
    }
  }
  
  else if(season == "Year quarter") {
    if(filt == "Q1: Jan, Feb, Mar") {
      data <-count(shootings[shootings$month == "1" | shootings$month == "2" | shootings$month == "3" , ], "state")
    } else if(filt == "Q2: Apr, May, Jun") {
      data <-count(shootings[shootings$month == "4" | shootings$month == "5" | shootings$month == "6" , ], "state")
    } else if(filt == "Q3: Jul, Aug, Sep") {
      data <-count(shootings[shootings$month == "7" | shootings$month == "8" | shootings$month == "9" , ], "state")
    } else {
      data <-count(shootings[shootings$month == "10" | shootings$month == "11" | shootings$month == "12" , ], "state")
    }
  }
  
  
  colnames(data) <- c("region", "value")
  data$region <- state.name[match(data$region,state.abb)]
  data$region <- tolower(data$region)
  data <- rbind(data, subset(data.frame(region = tolower(state.name), value = replicate(length(state.name), 0)), !(region %in% data$region)))
  sum = sum(data$value)
  state_choropleth(data, num_colors = 9, title = paste0("US police shootings 2015-2020, seasonal patterns:\n Seasonality: ", season, ", filtered to: ", filt), legend = paste0("Total shootings: ", sum))
  
}




