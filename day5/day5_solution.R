# Imports
library(readr)
library(dplyr)
library(purrr)
library(stringr)

input <- read_lines("day5/input.txt")

expand_coords <- function(coords) {
  start_coords <- str_split(coords, "->")[[1]][1]
  end_coords <- str_split(coords, "->")[[1]][2]

  data.frame(
    x1 = as.numeric(str_split(start_coords, ",")[[1]][1]),
    x2 = as.numeric(str_split(end_coords, ",")[[1]][1]),
    y1 = as.numeric(str_split(start_coords, ",")[[1]][2]),
    y2 = as.numeric(str_split(end_coords, ",")[[1]][2])
  )
}

mark_pipe <- function(input_coords, tracker, is_diag = FALSE){
  
  all_x <- seq(
    input_coords$x1, 
    input_coords$x2,
    by = ifelse(input_coords$x1 < input_coords$x2, 1, -1)
    )
  
  all_y <- seq(
    input_coords$y1, 
    input_coords$y2,
    by = ifelse(input_coords$y1 < input_coords$y2, 1, -1)
    )
  
  if(is_diag){
    all_combos <- purrr::map(
      all_x,
      ~{
        list(
          .x, all_y[which(all_x == .x)]
        )
      }
    )
  }else{
    all_combos <- purrr::cross2(.x = all_x, .y = all_y)
  }
  
  purrr::walk(
    all_combos,
    ~{
      # Coords are zero-based
      mark_x <- .x[[1]] + 1
      mark_y <- .x[[2]] + 1
      
      tracker[mark_y, mark_x] <<- tracker[mark_y, mark_x] + 1
    }
  )
  tracker
}

input_coords <- map_df(input, expand_coords)

# Empty matrix to track
tracker <- matrix(
  data = c(0),
  nrow = max(
    input_coords$x1, input_coords$x2
  ) + 2,
  ncol = max(
    input_coords$y1, input_coords$y2
  ) + 2
)

# Part 1
input_coords_pt1 <- input_coords %>%
  filter(
    x1 == x2 |
      y1 == y2
  )

# Mark tracker
tracker_pt1 <- tracker

purrr::walk(
  1:nrow(input_coords_pt1),
  ~{
    tracker_pt1 <<- mark_pipe(
      input_coords_pt1[.x, ], 
      tracker_pt1,
      is_diag = FALSE
      )
  }
)

message("Found ", length(which(tracker_pt1 >= 2)), " overlapping pipes!")

# Part 2
input_coords_pt2 <- input_coords %>%
  mutate(is_diag = abs(x1 - x2) == abs(y1 - y2)) %>%
  filter(
    is_diag | (x1 == x2 | y1 == y2)
  )

tracker_pt2 <- tracker

purrr::walk(
  1:nrow(input_coords_pt2),
  ~{
    tracker_pt2 <<- mark_pipe(
      input_coords_pt2[.x, ], 
      tracker_pt2,
      is_diag = input_coords_pt2$is_diag[[.x]]
      )
  }
)

message("Found ", length(which(tracker_pt2 >= 2)), " overlapping pipes!")
