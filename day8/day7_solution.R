library(readr)
library(dplyr)
library(stringr)

count_unique_str <- function(signal) {
  length(unique(unlist(str_split(signal, ""))))
}

input <- read_lines("day8/input.txt")

input_parsed <- purrr::map_df(
  input,
  ~ {
    tibble(
      signal = str_split(.x, " \\| ")[[1]][1],
      output = str_split(.x, " \\| ")[[1]][2]
    )
  }
)

known_signals <- tibble(
  signal = c("1", "4", "7", "8"),
  count = c(2, 4, 3, 7)
)

# Part 1
known_count <- purrr::map_dbl(
  1:nrow(input_parsed),
  ~ {
    output_signal <- input_parsed$output[[.x]]
    output_sp <- str_split(output_signal, " ")[[1]]
    counts <- purrr::map_dbl(output_sp, count_unique_str)
    sum(counts %in% known_signals$count)
  }
) %>%
  sum()

message("Found known signals ", known_count, " times!")

# Part 2
get_known_strings <- function(input, count) {
  purrr::map_chr(
    1:nrow(input),
    ~ {
      signal_sp <- str_split(input$signal[[.x]], " ")[[1]]
      paste(
        sort(
          str_split(signal_sp[which(nchar(signal_sp) == count)], "")[[1]]
        ),
        collapse = ""
      )
    }
  )
}

deduce_signals <- function(input_row) {
  str1 <- str_split(input_row$str_1, "")[[1]]
  str4 <- str_split(input_row$str_4, "")[[1]]
  str7 <- str_split(input_row$str_7, "")[[1]]
  str8 <- str_split(input_row$str_8, "")[[1]]

  undecoded <- str_split(input_row$signal, " ")[[1]]
  undecoded <- undecoded[!undecoded %in% c(
    input_row$str_1, input_row$str_4,
    input_row$str_7, input_row$str_8
  )]

  purrr::walk(
    undecoded,
    ~ {
      undecoded_str_set <- str_split(.x, "")[[1]]
      comp_str8 <- setdiff(str8, undecoded_str_set)

      if (count_unique_str(.x) == 5) {
        # 2, 3 or 5
        if (length(comp_str8) == 2 & all(comp_str8 %in% str4)) {
          input_row[["str_2"]] <<- paste(sort(undecoded_str_set), collapse = "")
        } else if (length(comp_str8) == 2 & !any(comp_str8 %in% str7)) {
          input_row[["str_3"]] <<- paste(sort(undecoded_str_set), collapse = "")
        } else {
          input_row[["str_5"]] <<- paste(sort(undecoded_str_set), collapse = "")
        }
      }

      if (count_unique_str(.x) == 6) {
        # 0, 6 or 9
        if (length(comp_str8) == 1 & all(comp_str8 %in% str4) & !any(comp_str8 %in% str1)) {
          input_row[["str_0"]] <<- paste(sort(undecoded_str_set), collapse = "")
        } else if (length(comp_str8) == 1 & all(comp_str8 %in% str1)) {
          input_row[["str_6"]] <<- paste(sort(undecoded_str_set), collapse = "")
        } else {
          input_row[["str_9"]] <<- paste(sort(undecoded_str_set), collapse = "")
        }
      }
    }
  )

  input_row
}

get_signal_value <- function(input_row, value) {
  val_cln <- paste(sort(str_split(value, "")[[1]]), collapse = "")
  str_col <- which(input_row == val_cln)

  decoded_val <- str_remove(
    names(input_row)[str_col],
    "str_"
  )

  if (!length(decoded_val)) {
    6
  } else {
    as.numeric(decoded_val)
  }
}

# Add known
purrr::walk(
  known_signals$count,
  ~ {
    lab <- known_signals$signal[known_signals$count == .x]
    input_parsed[[paste0("str_", lab)]] <<- get_known_strings(input_parsed, .x)
  }
)

decoded_input <- purrr::map_df(
  1:nrow(input_parsed),
  ~ {
    deduce_signals(input_parsed[.x, ])
  }
)

all_output_vals <- purrr::map_dbl(
  1:nrow(decoded_input),
  ~ {
    output_sp <- str_split(decoded_input$output[[.x]], " ")[[1]]
    output_sp_val <- purrr::map(
      output_sp,
      get_signal_value,
      input_row = decoded_input[.x, ]
    )
    as.numeric(paste(output_sp_val, collapse = ""))
  }
)

message("Total signal values: ", sum(all_output_vals))
