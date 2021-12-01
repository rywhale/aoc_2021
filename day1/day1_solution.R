input <- readLines("day1/input.txt")
input <- as.numeric(input)

test_input <- c(
  199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263
)

increase_sum <- function(input){
  purrr::map_lgl(
    1:length(input),
    ~{
      if(.x == 1){
        return(FALSE)
      }
      
      status <- input[.x - 1] < input[.x]
      
      status
    }
  )
}

# Part 1
part1 <- increase_sum(input)
message("Increase occured: ", sum(part1), " times!")

# Part 2
new_input <- purrr::map_dbl(
  3:length(input),
  ~{
    
    sum(
      input[.x],
      input[.x - 1],
      input[.x - 2]
    )
  }
)

part2 <- increase_sum(new_input)
message("Increase occured: ", sum(part2), " times!")