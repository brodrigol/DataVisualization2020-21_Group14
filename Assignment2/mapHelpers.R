# count(shootings[shootings$gender == 'M' & shootings$race == 'White',], 'state')
victims_map <- function(shootings, gender, race, age) {
  
  # Filter data 
  gen <- switch(gender, 
                   "All" = "All",
                   "Male" = "M",
                   "Female" = "F")
  
  if(gen == "All" & race == "All"){
    data <-count(shootings[shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else if(gen == "All"){
    data <-count(shootings[shootings$race == race & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else if(race == "All"){
    data <-count(shootings[shootings$gender == gen & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }
  else {
    data <-count(shootings[shootings$gender == gen & shootings$race == race & shootings$age >= age[1] & shootings$age <= age[2],], 'state')
  }

  # Plot color
shades <- colorRampPalette(c('white', 'darkred'))(100)
sum = data$freq
data$percent <- cut(data$freq, 100, include.lowest = TRUE)
fills <- shades[data$percent]

map("state", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
map("state", col = "grey", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
}