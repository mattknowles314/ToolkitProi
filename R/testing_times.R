# Split time into 6 chunks of four hours
parse_times <- function(time){
    if(!is.character(time)) {
        stop("Time is not character")
    }

    split_time <- stringr::str_split(time, ":", simplify = TRUE)
    hour <- as.integer(split_time[1])
    if (hour < 4) return(1)
    if (hour < 8 ) return(2)
    if (hour < 12) return(3)
    if (hour < 16) return(4)
    ifelse(hour < 20, return(5), return(6))
}

parse_times("01:14")

test <- sub_accidents %>%
    mutate(time_group_1 = purrr::map_int(time, parse_times))
