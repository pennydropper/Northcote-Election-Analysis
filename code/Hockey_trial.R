# Define configuration of file
date_hdr <- " Date / Time"
date_id <- "2019"
date_id_pos <- 8L
datetm_pos <- 2L
home_pos <- 22L
away_pos <- 49L
venue_pos <- 76L

round_num <- 0L

hockey_rounds <- vector("list", 14)  # assumes 14 rounds of hockey

for (i in seq_along(fix_rough_raw_simpl)) {
  # Step through each element and determine the type of row
  
  # i = 6
  elem_line <- fix_rough_raw_simpl[[i]]
  
  if (str_sub(elem_line, 1, 5) == "Round") {
    # Appears to be the Round number
    
    if (round_num > 0L) {
      # Close off previous round
      hockey_rounds[[round_num]] <- bind_rows(rnd_games)
      
    }
    
    round_num <- round_num + 1L
    
    rnd_games <- vector("list", 4) %>% 
      set_names(str_c("game_", 1:4, sep = ""))
    
    game_id <- 0L
    
  # } else if (str_sub(elem_line, date_id_pos, date_id_pos + 3) == date_id) {
  } else if (str_detect(elem_line, "^\\s+\\d")) {
    # Appears to have the date and time of the round
    
    game_dets <- vector("list", 6) %>% 
      set_names(c("round_num", "game_id", "datetm", "home_tm", "away_tm", "venue"))
    
    game_id <- game_id + 1L
    
    game_dets$round_num <- round_num
    
    game_dets$game_id <- game_id
    
    game_dets$datetm <- dmy_hm(str_sub(elem_line, 1, home_pos - 1))
    
    if (str_sub(elem_line, home_pos, home_pos) == " ") {
      # Appears that the home team name has wrapped around...
      game_dets$home_tm <- str_c(fix_rough_raw_simpl[[i - 1]] %>% 
                                   str_sub(home_pos, away_pos - 1) %>% 
                                   str_trim(),
                                 fix_rough_raw_simpl[[i + 1]] %>% 
                                   str_sub(home_pos, away_pos - 1) %>% 
                                   str_trim(),
                                 sep = " ")
    } else {
      game_dets$home_tm <- elem_line %>% str_sub(home_pos, away_pos - 1) %>% str_trim()
      
    }
    
    if (str_sub(elem_line, away_pos, away_pos) == " ") {
      # Appears that the away team name has wrapped around...
      game_dets$away_tm <- str_c(fix_rough_raw_simpl[[i - 1]] %>% 
                                   str_sub(away_pos, venue_pos - 1) %>% 
                                   str_trim(),
                                 fix_rough_raw_simpl[[i + 1]] %>% 
                                   str_sub(away_pos, venue_pos - 1) %>% 
                                   str_trim(),
                                 sep = " ")
    } else {
      game_dets$away_tm <- elem_line %>% str_sub(away_pos, venue_pos - 1) %>% str_trim()
    }
    
    if (str_sub(elem_line, venue_pos, -1L) == "") {
      # Appears that the venue has wrapped around...
      game_dets$venue <- str_c(fix_rough_raw_simpl[[i - 1]] %>% 
                                 str_sub(venue_pos) %>% 
                                 str_trim(),
                               fix_rough_raw_simpl[[i + 1]] %>% 
                                 str_sub(venue_pos) %>% 
                                 str_trim(),
                               sep = " ")
    } else {
      game_dets$venue <- elem_line %>% str_sub(venue_pos, -1L) %>% str_trim()
    }
    
    rnd_games[[game_id]] <- game_dets
    
  }
  
}

# Finish off last round.
hockey_rounds[[round_num]] <- bind_rows(rnd_games)

# Bind rounds together
fixture_tbl <- hockey_rounds %>% bind_rows()