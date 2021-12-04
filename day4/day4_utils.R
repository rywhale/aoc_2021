mark_num <- function(holders, value) {
  purrr::walk(
    1:length(holders),
    function(holder) {
      value_loc <- data.frame(
        which(
          holders[[holder]]$card == value,
          arr.ind = TRUE
        )
      )

      # print(value_loc)

      if (nrow(value_loc)) {
        purrr::walk(
          1:nrow(value_loc),
          ~ {
            value_row <- value_loc$row[[.x]]
            value_col <- value_loc$col[[.x]]
            holders[[holder]]$tracker[value_row, value_col] <<- "x"
          }
        )
      }
    }
  )

  holders
}

check_bingo <- function(card_holders) {
  bingo <- list()

  purrr::walk(
    names(card_holders),
    function(card_pos) {
      
      # Horiz
      check_rows <- map_lgl(
        1:nrow(card_holders[[card_pos]]$card),
        ~ {
          all(card_holders[[card_pos]]$tracker[.x, ] == "x")
        }
      )
      
      if (any(check_rows)) {
        bingo[[card_pos]] <<- list(
          holder = card_holders[[card_pos]],
          pos = card_pos
        ) 
      }

      # Vert
      check_cols <- map_lgl(
        1:ncol(card_holders[[card_pos]]$card),
        ~ {
          all(card_holders[[card_pos]]$tracker[, .x] == "x")
        }
      )

      if (any(check_cols)) {
        if(!card_pos %in% names(bingo)){
          bingo[[card_pos]] <<- list(
            holder = card_holders[[card_pos]],
            pos = card_pos
          ) 
        }
      }
    }
  )

  bingo
}
