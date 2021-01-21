# count(shootings[shootings$gender == 'M' & shootings$race == 'White',], 'state')
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


choropleth_data <- data.frame(data$state, data$freq)
colnames(data ) <- c("region", "value")
data$region <- state.name[match(data$region,state.abb)]
data$region <- tolower(data$region)
data <- rbind(data, subset(data.frame(region = tolower(state.name), value = replicate(length(state.name), 0)), !(region %in% data$region)))
sum = sum(data$value)
state_choropleth(data, num_colors = 9, title = paste0("US police shootings 2015-2020, victim's profile:\n", race, ", ", gen, ", age between ", age[1], " and ", age[2]), legend = paste0("Total shootings: ", sum))

}