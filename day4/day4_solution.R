# Imports
library(purrr)
library(stringr)
library(readr)

source("day4/day4_utils.R")

input <- read_lines("day4/input.txt")

# Grab draw numbers + split into 5s]
draw_nums <- str_split(
  input[1], ","
)[[1]]

input <- input[2:length(input)]

card_holder <- purrr::map(
  which(input == ""),
  ~ {
    start <- .x + 1
    end <- .x + 5

    card_array <- str_extract_all(
      input[start:end],
      pattern = "\\d+",
      simplify = TRUE
    )

    list(
      card = card_array,
      tracker = replace(card_array, which(card_array != "o"), "o")
    )
  }
)

names(card_holder) <- as.character(1:length(card_holder))

# Part 1
winning_card <- NA
last_num <- NA
part1_cards <- card_holder

purrr::walk(
  draw_nums,
  ~{
    if(!is.list(winning_card)){
      part1_cards <<- mark_num(part1_cards, .x)
      bingo <- check_bingo(part1_cards)

      if(length(bingo)){
        winning_card <<- bingo
        last_num <<- .x
      }
    }
  }
)

unmarked <- which(winning_card[[1]]$holder$tracker != "x", arr.ind = TRUE)
unmarked_sum <- sum(as.numeric(winning_card[[1]]$holder$card[unmarked]))

message("Card score is: ", unmarked_sum * as.numeric(last_num))

# Part 2
last_num <- NA
winning_cards <- c()
part2_cards <- card_holder
final_card <- NA

purrr::walk(
  draw_nums,
  function(curr_num){
  
    if(length(winning_cards) <= length(card_holder)){
      
      part2_cards <<- mark_num(part2_cards, curr_num)
      bingo <- check_bingo(part2_cards)
      
      if(length(bingo)){
        purrr::walk(
          bingo,
          ~{
            if(!.x$pos %in% winning_cards){
              part2_cards <<- part2_cards[-as.numeric(which(names(part2_cards) == .x$pos))]
              
              winning_cards <<- append(winning_cards, .x$pos)
              final_card <<- .x
              last_num <<- curr_num
              
            }
          }
        )
        
      }
    }
  }
)

unmarked_pt2 <- which(final_card$holder$tracker != "x", arr.ind = TRUE)
unmarked_sum_pt2 <- sum(as.numeric(final_card$holder$card[unmarked_pt2]))

message("Card score is: ", unmarked_sum_pt2 * as.numeric(last_num))
