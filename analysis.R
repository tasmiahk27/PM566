# first R script in new Git repo

plot(1:10)

summary (1:100)

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2026, week = 35)

rlang::last_trace(drop = FALSE) 