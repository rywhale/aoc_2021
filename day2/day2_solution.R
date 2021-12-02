# Imports
library(dplyr)
library(readr)

input <- read_table(
  "day2/input.txt",
  col_names = c("direction", "amount")
  )

# Part 1
x_val <- 0
y_val <- 0

purrr::walk(
  1:nrow(input),
  ~{
    direction <- input$direction[[.x]]
    val <- input$amount[[.x]]
    
    if(direction == "forward"){
      x_val <<- x_val + val
    }else{
      
      y_val <<- ifelse(
        direction == "down",
        y_val + val, 
        y_val - val 
      )
    }
  }
)

message("Product is: ", y_val * x_val)

# Part 2
x_val <- 0
y_val <- 0
aim_val <- 0

purrr::walk(
  1:nrow(input),
  ~{
    direction <- input$direction[[.x]]
    val <- input$amount[[.x]]
    
    if(direction == "forward"){
      x_val <<- x_val + val
      y_val <<- y_val + (aim_val * val)
      
    }else{
      aim_val <<- ifelse(
        direction == "down",
        aim_val + val,
        aim_val - val
      )
      
    }
  }
)

message("Product is: ", y_val * x_val)