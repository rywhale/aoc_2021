# Imports
library(readr)
library(stringr)

input <- read_lines("day6/input.txt")
input <- as.numeric(str_split(input, ",")[[1]])

onefish_twofish <- function(input, days){
  purrr::walk(
    1:days,
    ~{
      
      input <<- input - 1
      
      if(any(input < 0)){
        input <<- c(input, rep(8, sum(input < 0)))
        input[input < 0] <<- 6
      }
      
    }
  )
  input
}

redfish_bluefish <- function(input, days){
  
  fish_count <- tabulate(input, nbins = 8)
 
  # Add '0' counter
  fish_count <- c(0, fish_count)
  
  purrr::walk(
    1:days,
    ~{
     
      new_fish <- fish_count[1]
      
      fish_count[-length(fish_count)] <<- fish_count[-1]
      
      fish_count[7] <<- fish_count[7] + new_fish
      fish_count[9] <<- new_fish
    }
  )
  sum(fish_count)
}

# Part 1
part1_fish <- onefish_twofish(input, 80)
message("There are ", length(part1_fish), " after 80 days!")

# Part 2
part2_fish <- redfish_bluefish(input, 256)
message("There are ", part2_fish, " after 256 days!")


