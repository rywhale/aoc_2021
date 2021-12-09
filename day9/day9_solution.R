# Imports
library(readr)
library(stringr)

input_raw <- read_lines("day9/input.txt")
input_sp <- str_split(input_raw, "")

input_matrix <- matrix(
  as.numeric(unlist(input_sp)),
  nrow = length(input_raw),
  byrow = TRUE
)

find_neighborus <- function(input, row, col) {
  top <- ifelse(row - 1 > 0, input[row - 1, col], NA)
  bottom <- ifelse(row + 1 <= nrow(input), input[row + 1, col], NA)

  left <- ifelse(col - 1 > 0, input[row, col - 1], NA)
  right <- ifelse(col + 1 <= ncol(input), input[row, col + 1], NA)

  out <- c(top, bottom, right, left)

  out[!is.na(out)]
}

find_low_points <- function(input) {
  all_coords <- purrr::cross2(1:nrow(input), 1:ncol(input))

  low_points <- c()
  coords <- list(row = c(), col = c())

  purrr::walk(
    all_coords,
    ~ {
      val <- input[.x[[1]], .x[[2]]]
      neighbours <- find_neighborus(input, .x[[1]], .x[[2]])

      if (all(val < neighbours)) {
        low_points <<- c(low_points, val)
        coords$row <<- c(coords$row, .x[[1]])
        coords$col <<- c(coords$col, .x[[2]])
      }
    }
  )
  list(
    "vals" = low_points,
    "coords" = coords
  )
}

# Part 1
lp <- find_low_points(input_matrix)
message("Risk levels: ", sum(lp$vals + 1))

# Part 2
marked <- input_matrix
marked[marked != 9] <- 1
marked[marked == 9] <- 0

meow <- raster::raster(marked)
clumps <- matrix(raster::clump(meow, directions = 4))
clump_count <- sort(
  table(clumps[!is.na(clumps)]),
  decreasing = TRUE
)

message("Risk levels: ", clump_count[[1]] * clump_count[[2]] * clump_count[[3]])
