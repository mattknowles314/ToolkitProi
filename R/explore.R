library(tidyverse)
library(ggplot2)
library(ggridges)

fatal <- accidents %>%
    filter(accident_severity == 3)

# Day of the week

week_count <- accidents %>%
    group_by(day_of_week, accident_severity) %>%
    count()

week_count$day_of_week <- as.factor(week_count$day_of_week)
week_count$accident_severity <- as.factor(week_count$accident_severity)
levels(week_count$day_of_week) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ggplot(week_count, aes(x = day_of_week, y = n, fill = accident_severity)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + xlab("Day of the week") + ylab("Number of Accidents")

# Speed limit

speed_limit_count <- accidents %>%
    group_by(speed_limit, accident_severity) %>%
    count() %>%
    mutate(logn = log(n))

speed_limit_count$speed_limit <- as.factor(speed_limit_count$speed_limit)
speed_limit_count$accident_severity <- as.factor(speed_limit_count$accident_severity)

ggplot(speed_limit_count %>% filter(speed_limit != -1), aes(x = speed_limit, y = n, fill = accident_severity)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + xlab("Speed Limit") + ylab("Number of Accidents")

ggplot(speed_limit_count %>% filter(speed_limit != -1), aes(shape = accident_severity)) +
    geom_point(aes(x = speed_limit, y = logn), size = 3) +
    xlab("Speed Limit") + ylab("Log Accident Count")

