# Plot map with map package

# Plot color
shades <- colorRampPalette(c('white', 'darkred'))(100)
data$percent <- cut(data$freq, 100, include.lowest = TRUE)
fills <- shades[data$percent]

# Plot state fill
map("state", fill = TRUE, col = fills, boundary = TRUE, names = TRUE,
    resolution = 0, lty = 0, lwd = 1, projection = "polyconic", namefield = "state",
    myborder = 0, mar = c(0,0,0,0))
# Plot state boundaries
map("state", col = "grey", fill = FALSE, add = TRUE, 
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))

# Plot tiltle
title(paste0("US police shootings 2015-2020, victim's profile:\n", race, ", ", gen, ", age between ", age[1], " and ", age[2]))

# Plot legend
max = max(data$freq)
min = min(data$freq)
sum = sum(data$freq)
inc <- as.integer((max - min) / 4)

legend.title <- paste0("Total shootings: ", sum)

legend.text <- c(paste0(min, " shootings or less"),
                 paste0(min + inc, " shootings"),
                 paste0(min + 2 * inc, " shootings"),
                 paste0(min + 3 * inc, " shootings"),
                 paste0(max, " shootings or more"))

legend("topright",  
       legend = legend.text, 
       fill = shades[c(1, 25, 50, 75, 100)], 
       title = legend.title)