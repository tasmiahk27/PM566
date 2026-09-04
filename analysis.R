# first R script in new Git repo

install.packages(c("usethis", "gitcreds"))
usethis::create_github_token()
gitcreds::gitcreds_set()



world_castles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv')

names(world_castles)
View(world_castles)


library(dplyr)
library(ggplot2)

niche_landmarks <- world_castles %>% 
  filter(!is.na(sitelinks) & !is.na(pageviews) & pageviews > 0 & sitelinks > 1) %>% 
  mutate(views_per_language = pageviews / sitelinks) %>% 
  arrange(views_per_language) %>% 
  select(name, country, category, sitelinks, pageviews, views_per_language)

top10 <- head(niche_landmarks, 10)
View(top10)

ggplot(top10, aes(x = views_per_language, y = reorder(name, -views_per_language), fill = country)) +
  geom_col() +
  labs(
    title = "Top 10 Landmarks with Multi-Language Articles but Few Readers",
    subtitle = "Sorted by lowest average pageviews per language edition",
    x = "Average Pageviews per Language",
    y = "Landmark Name",
    fill = "Country"
  ) +
  theme_minimal()
