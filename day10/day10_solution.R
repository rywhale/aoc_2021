# Imports
library(stringr)
library(dplyr)
library(readr)

input <- read_lines("day10/input.txt")

openers <- c("(", "[", "{", "<")
closers <- c(")", "]", "}", ">")

is_complete <- function(chunk){
  
  if(str_count(chunk) %% 2 != 0){
    return(FALSE)
  }
  
  openers <- paste0("\\", openers)
  closers <- paste0("\\", closers)
  
  open_count <- purrr::map_dbl(openers, str_count, string = chunk)
  close_count <- purrr::map_dbl(closers, str_count, string = chunk)
  
  all(open_count == close_count) 
}

check_syntax <- function(line){
  
  line_sp <- str_split(line, "")[[1]]
  used <- c()
  bad_chars <- c()
  
  purrr::walk(
    1:length(line_sp),
    ~{
      
      if(line_sp[[.x]] %in% closers){
        
        chunk <- line_sp[1:.x]
        
        prosp_open <- which(
          line_sp == openers[which(closers == line_sp[[.x]])]
          )
        
        prosp_open <- prosp_open[prosp_open < .x]
        prosp_open <- prosp_open[!prosp_open %in% used]
        
        if(length(prosp_open)){
          used <<- c(used, max(prosp_open))
          chunk <- chunk[max(prosp_open):length(chunk)]
          
          if(!is_complete(paste(chunk, collapse = ""))){
            bad_chars <<- c(bad_chars, line_sp[[.x]])
          }
          
        }else{
          bad_chars <<- c(bad_chars, line_sp[[.x]])
        }
         
      }
      
    }
  )
  bad_chars
}

find_completion_strs <- function(line){
  
  line_sp <- str_split(line, "")[[1]]
  
  used <- c()
  to_add <- c()
  
  purrr::walk(
    length(line_sp):1,
    ~{
      
      if(line_sp[[.x]] %in% openers){
        chunk <- line_sp[.x:length(line_sp)]
        
        prosp_close <- which(
          line_sp == closers[which(openers == line_sp[[.x]])]
        )
        
        prosp_close <- prosp_close[prosp_close > .x]
        prosp_close <- prosp_close[!prosp_close %in% used]
        
        if(!length(prosp_close)){
          to_add <<- c(to_add, closers[which(openers == line_sp[[.x]])])
        }else{
          used <<- c(used, min(prosp_close))
        }
      }
    }
  )
  
  to_add
  
}

# Part 1
syntax_score <- purrr::map_dbl(
  input,
  ~{
    bad_chars <- check_syntax(.x)
    
    if(length(bad_chars)){
      switch(
        bad_chars[[1]],
        ")" = 3,
        "]" = 57,
        "}" = 1197,
        ">" = 25137
      )
    }else{
      0
    }
  }
)

message("Total syntax score: ", sum(syntax_score))

# Part 2
req_strs <- purrr::map(
  input,
  find_completion_strs
)

comp_score <- purrr::map_dbl(
  1:length(req_strs),
  ~{
    
    score <- 0
    
    if(syntax_score[[.x]] == 0){
      purrr::walk(
        req_strs[[.x]],
        function(comp_str){
          
          char_score <- switch(
            comp_str,
            ")" = 1,
            "]" = 2,
            "}" = 3,
            ">" = 4
          )
          
          score <<- (score * 5) + char_score
        }
      )
      
    }
    score
  }
)

message("Total completion score: ", median(comp_score[comp_score != 0]))