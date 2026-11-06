install.packages("tidyverse")
install.packages("data.table")
library(tidyverse)
library(data.table)

met <- read.csv("~/Downloads/met_all.gz")
met <- met[met$temp > -10, ]
met$elev[met$elev == 9999.0] <- NA

library(dplyr)
met_avg <- summarize(met, 
                     temp= mean(temp, na.rm=TRUE),
                     rh= mean(rh, na.rm=TRUE),
                     wind.sp= mean(wind.sp, na.rm = TRUE),
                     vis.dist= mean(vis.dist, na.rm= TRUE),
                     lat= mean(lat, na.rm=TRUE),
                     lon= mean(lon, na.rm=TRUE),
                     elev= mean(elev, na.rm=TRUE),
                     .by=c(USAFID, day))


## new variables using ifelse()
met_avg$region <- ifelse(met_avg$lon > -98, "east", "west")
met_avg$elev_cat <- ifelse(met_avg$elev > 252, "high", "low")

# Using the cut() function to create more than 2 categories
met_avg$vis_cat <- cut(met_avg$vis.dist,
                       breaks = c(0, 1000, 6000, 10000, Inf),
                       labels = c("fog", "mist", "haze", "clear"),
                       right = FALSE)
plot(met_avg$temp,met_avg$rh)
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh))
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, color = region))
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, alpha = 0.3))
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, alpha = rh))
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, shape = region))
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh), color = "blue")

plot(1:10, pch = 16)
points(10:1, pch = 16, col = 2)

met_avg[!is.na(met_avg$region), ] %>% 
  ggplot() + 
  geom_point(mapping = aes(x = temp, y = rh, color=region)) + 
  facet_wrap(~ region, nrow = 1)

install.packages("cowplot")
library(cowplot)

scatterplot <- ggplot(data = met_avg) + geom_point(mapping = aes(x = temp, y = rh))
lineplot    <- ggplot(data = met_avg) + geom_smooth(mapping = aes(x = temp, y = rh))

plot_grid(scatterplot, lineplot, labels = "AUTO")

ggplot(data = met_avg) + 
  geom_smooth(mapping = aes(x = temp, y = rh, linetype = region))

hist(met_avg$temp)

ggplot(met_avg) + 
  geom_histogram(mapping = aes(x = temp))

boxplot(met_avg$temp ~ met_avg$elev_cat)
met_avg[!is.na(met_avg$elev_cat), ] %>% 
  ggplot()+
  geom_boxplot(mapping=aes(x=elev_cat, y=temp, fill=elev_cat)) 

plot(met_avg$day[met_avg$elev==4113], met_avg$temp[met_avg$elev==4113], type = 'l')
plot(1:10, pch = 16)
lines(10:1, col = 2, lwd = 3) # add a thick red line


ggplot(data = met_avg[met_avg$elev==4113, ])+
  geom_line(mapping=aes(x=day, y=temp))

install.packages("mapdata")
library(mapdata)

world_map <- map_data("world")
ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkgray", color = "white")

us_map <- map_data("state")
ggplot(data = us_map, aes(x = long, y = lat, fill = region)) +
  geom_polygon(color = "white")

met_avg[!is.na(met_avg$region), ] %>%
  ggplot() + 
  geom_point(mapping = aes(x = temp, y = rh, color = region))+
  geom_smooth(mapping = aes(x = temp, y = rh, linetype = region))