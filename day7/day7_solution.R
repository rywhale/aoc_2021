library(purrr)
library(dplyr)

input <- scan("day7/input.txt", sep = ",")
# input <- c(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

# Part 1
crabby_bois <- purrr::map_df(
  min(input):max(input),
  ~ {
    tibble(
      fuel_sum = sum(abs(input - .x)),
      position = .x
    )
  }
)

message("Total fuel: ", min(crabby_bois$fuel_sum))

# Part 2
more_crabby_bois <- purrr::map_df(
  min(input):max(input),
  function(position) {
    diff_vals <- purrr::map_dbl(
      abs(input - position),
      ~ {
        if (!.x == 0) {
          sum(1:.x)
        }else{
          0
        }
      }
    )

    tibble(
      fuel_sum = sum(diff_vals),
      position = position
    )
  }
)

message("Total fuel: ", min(more_crabby_bois$fuel_sum))
