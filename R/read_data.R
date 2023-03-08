accidents <- read.csv("Data/dft-road-casualty-statistics-accident-2020.csv")

vehicles <- read.csv("Data/dft-road-casualty-statistics-casualty-2020.csv")

casualties <- read.csv("Data/dft-road-casualty-statistics-vehicle-2020.csv")

use_in_formula <- c(
    9, 10, 13, 14, 18, 20, 21, 22, 24, 28, 29, 30
)

accidents_key <- accidents[, use_in_formula]
accidents_key[,1:13] <- lapply(accidents_key[,1:13], factor)

accidents %>%
    filter(speed_limit != -1) %>%
    ggplot(aes(x = speed_limit)) +
    geom_bar() +
    xlab("Speed Limit") +
    ylab("Count")
