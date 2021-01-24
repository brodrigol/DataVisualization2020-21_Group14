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
  totals <- count(shootings, 'state')
  
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


colnames(data) <- c("region", "n")
data <- rbind(data, subset(data.frame(region = state.abb, n = replicate(length(state.abb), 0)), !(region %in% data$region)))
data <- merge(data, totals, by.x="region", by.y="state",no.dups = TRUE, suffixes = c("",""))
data$name <- state.name[match(data$region,state.abb)]
data$value <- round((data$n / data$freq)*100, digits = 2)
data$hover <- paste( data$name, "<br>", sep = "")
data <- subset(data, select=c(region, name, value, hover))

return(data)
#state_choropleth(data, num_colors = 9, title = paste0("% US police shootings 2015-2020 with victim's profile:\n", race, ", ", gen, ", age between ", age[1], " and ", age[2]), legend = "% of total state shootings")
}



GenerateHeatmap <- function(dat, x1, y1, grid1 = "", grid2 = ""){
  if(x1=='age'){
    x2 = x1
    x1 = y1
    y1 <- x2
  }
  
  vars = c(x1,y1)
  if(grid1 != "") {
    #dat <- dat %>% mutate_at(vars(starts_with(grid1)), ~(paste(grid1, "=",.) %>%  as.factor()))
    vars <-  c(vars, grid1)
  }
  if(grid2 != "") {
    #dat <- dat %>% mutate_at(vars(starts_with(grid2)), ~(paste(grid2, "=",.) %>%  as.factor()))
    vars <-  c(vars, grid2)}
  
  if ('age' %in% vars) {
    dat <- dat %>% select(all_of(vars))
    if (length(vars) == 2)  {
      names(dat) <- (c("x", "y"))
    } else if (length(vars) == 3) {
      names(dat) <- (c("x", "y", ifelse(grid1 != "",'g1','g2')))
    } else   {
      names(dat) <- (c("x", "y", 'g1', 'g2'))
    }
    
    p <-
      ggplot(dat, aes(x = reorder(x,y,na.rm = T), y, fill = reorder(x,y,na.rm = T))) +
      geom_boxplot() +  scale_fill_brewer(palette = "Blues") + 
      labs(x = x1, y = y1, fill = x1)
  } else {
    dat <- dat %>% select(all_of(vars)) %>%
      group_by_at(vars) %>% dplyr::summarise(n = dplyr::n())
    
    
    if (length(vars) == 2)  {
      names(dat) <- (c("x", "y", "z"))
    } else if (length(vars) == 3) {
      names(dat) <- (c("x", "y", ifelse(grid1 != "",'g1','g2'), "z"))
    } else   {
      names(dat) <- (c("x", "y", 'g1', 'g2', "z"))
    }
    
    dat$text = paste0("x: ", dat$x, "\n", "y: ", dat$y, "\n", "Value: ", round(dat$z, 2))
    
    
    p <-
      ggplot(dat, aes(x, y, fill = desc(z), text = text)) +
      geom_tile() +  scale_fill_distiller(palette = "Blues") +  
      labs(x = x1, y = y1, fill = 'Count')
  }
  
  p <- p + theme_ipsum() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45))
  if(grid1 != "" | grid2 != "") {
    form <- paste(ifelse(grid1!= "",'g1', '.'), '~', ifelse(grid2!= "",'g2', '.')) %>% as.formula()
    p <- p  + facet_grid(form, scales = "free")
  }
  

  
  return(ggplotly(p, tooltip="text"))
}

GenerateTreemap <- function(data, vars){
  
  data <- data %>% select(any_of(vars)) %>%
    group_by_at(vars) %>% dplyr::summarise(n = dplyr::n())
  
  p <- treemap(data,
               index=vars,
               vSize="n",
               type="index",
               fontsize.labels=c(15,12),
               fontcolor.labels=c("black","grey"),
               fontface.labels=c(2,1),
               palette = "Blues",
               bg.labels=c("transparent"),
               align.labels=list(
                 c("center", "center"), 
                 c("right", "bottom")
               )  
  ) 
  
 return(d3tree2( p ,  rootname = "Shootings" ))
}


GenerateTimeLine <- function(data, time, var){
  vars <- c(time, var)
  data$year_month <- paste0(data$year, '-', data$month)
  data <- data %>% select(any_of(vars)) %>%
    group_by_at(vars) %>% dplyr::summarise(n = dplyr::n())
  names(data) <- c('t', 'g', 'n')
  data$text = paste0("x: ", data$t, "\n", "y: ", data$g, "\n", "Value: ",data$n)
  p <- ggplot(data, aes(x = t, y = n)) +
    geom_point(aes(text= paste0(time,": ", data$t, "\n", var,": ", data$g, "\n", "Count: ",data$n)), color = 'steelblue') +
    geom_line(aes(group = g), color = 'steelblue')+
    labs(x = time, y = 'Count') +
    theme_ipsum() + facet_grid(g~., scales = "free_y")
  
  return(ggplotly(p, tooltip="text"))
}
