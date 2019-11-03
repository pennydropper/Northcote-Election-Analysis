
# Constants ---------------------------------------------------------------

dom_party_name <- "Australian Labor Party"

# Transformed dataframes
transf_df <- c(two_pp_detail,
               two_pp_all_booth,
               two_pp_by_booth_nondom,
               votes_by_booth_elec,
               votes_by_booth_all,
               party_votes_by_elec,
               pref_w_party,
               cand_list,
               votes_by_phys_booth
)


# Plot distribution share -------------------------------------------------

plot_disn_sh <- function(yr = "2017") {
  # Returns a plot object with the alloction of distributions
  
  # pref_point <- 
    pref %>% 
    filter(year == yr) %>% 
    group_by(year, from_cand) %>% 
    mutate(rounds_max = n()) %>% 
    ungroup() %>% 
    mutate(pref_rank = factor(pref_rank, levels = 1:10),
           `From cand` = fct_reorder(from_cand, desc(rounds_max)),
           `To cand` = fct_reorder(to_cand, votes_sum)) %>% 
    
    ggplot(aes(x = `From cand`, y = `To cand`, size = votes_sh, colour = votes)) +
    geom_point(na.rm = TRUE) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_discrete(position = "top") +
    scale_size_continuous("Share of votes", labels = scales::percent) +
    labs(title = "How votes were preferenced to different candidates",
         x = "From candidate",
         y = "To candidate")
  
  # ggplotly(pref_point)
  # pref_point
  
}


page_length <- function(url, css_sel) {
  # Calculates the html_nodes length that would be returned for the URL and CSS selectors
  read_html(url) %>% html_nodes(css_sel) %>% length()
}

dist_pref_url_ <- function(p_domain, p_yr, p_dist_pref_pg_nm, p_electorate_nm, css_sel) {
  # Returns a vector with possible distribution preference URLs
  dist_url_ <- vector("character", 2)
  
  dist_url_[1] <- str_c(p_domain, p_yr, "/",  p_dist_pref_pg_nm, p_electorate_nm, "District.html", sep = "")
  # e.g. https://www.vec.vic.gov.au/Results/State2018/distributionNorthcoteDistrict.html
  dist_url_[2] <- str_c(p_domain, p_yr,  p_dist_pref_pg_nm, p_electorate_nm, "District.html", sep = "")
  # e.g. https://www.vec.vic.gov.au/Results/state2010distributionNorthcoteDistrict.html
  
  if (page_length(dist_url_[1], css_sel) > 0) {
    # First page is legitmate
    url_rtn <- dist_url_[1]
  } else if (page_length(dist_url_[2], css_sel) > 0) {
    # Alt page is legitimate
    url_rtn <- dist_url_[2]
  } else {
    url_rtn <- NA_character_
  }
  
  url_rtn
  
}

url_by_elec <- function(years, domain, dist_pref_pg_nm, electorate_nm, distn_pref_css_sels) {
  # Returns vector of valid URLs, named by the election
  years %>%
    map_chr(., ~ dist_pref_url_(domain, ., dist_pref_pg_nm, electorate_nm, distn_pref_css_sels)) %>%
    set_names(years) %>%
    keep(., ~!is.na(.))
}

ins_unit <- function(x, pos = 1L, val) {
  # Inserts a unit into a vector, x, at postion pos
  x %>% 
    head(., pos -1) %>% 
    append(., val) %>% 
    append(., tail(x, -pos + 1))
}

bar_chart_voters_by_booth <- function(elections = c("2018", "2017", "1999")) {
  # Returns ggplot bar chart with number of voters by booth
  first_pref %>% 
    filter(year %in% elections) %>% 
    mutate(year = factor(year) %>% fct_rev()) %>% 
    group_by(booth, year) %>%
    summarise(votes_sum = sum(votes, na.rm = TRUE)) %>%
    ungroup() %>% 
    complete(booth, year) %>% 
    group_by(booth) %>% 
    mutate(booth_votes = sum(votes_sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(votes_sum = coalesce(votes_sum, 0),
           booth = booth %>% fct_reorder(booth_votes)) %>% 
    
    ggplot(aes(x = booth, y = votes_sum, fill = year)) +
    geom_col(position = "dodge") +
    scale_y_continuous(breaks = seq(0, 20000, 2000)) +
    scale_fill_discrete(drop = FALSE) +
    coord_flip()
}

party_colours <- function() {
  # Returns named vector mapping parties to colours
  party_std %>% 
    select(party_std, party_colour) %>% 
    distinct() %>% 
    deframe() %>% 
    append(c("Informal" = "grey"))
}

print_booth_map <- function(p_votes_by_phys_booth = votes_by_phys_booth, p_elec = "2018") {
  # Print leaflet map of polling booths
  
  party2 <-
    p_votes_by_phys_booth %>% 
    filter(year == p_elec) %>% 
    pull(party_std) %>% 
    head(1)

  pal_spect <- "RdYlGn"  # Assume that Greens are the 2nd party
  
  if (party2 == "Liberal") { pal_spect <- "RdBu" } # i.e. Liberals are the 2nd party
  
  pal <- colorNumeric(
    palette = pal_spect,
    domain = p_votes_by_phys_booth$votes_sh_scale)
  
  leaflet(options = leafletOptions(minZoom = 12,
                                   maxZoom = 16)) %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    
    addPolygons(data = nrthct_plgon,
                color = "#c994c7", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.2,
                # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE)) %>% 
    
    addCircleMarkers(data = p_votes_by_phys_booth %>% filter(year == p_elec),
                     radius = 0.5,
                     # popup = ~hov_text,
                     label = ~booth,
                     lat = ~lat,
                     lng = ~lon,
                     labelOptions = labelOptions(permanent = FALSE),
                     color = "black",
                     weight = 1,
                     fillColor = "black",
                     fill = TRUE) %>% 

    addCircleMarkers(data = p_votes_by_phys_booth %>% filter(year == p_elec),
                     radius = ~votes / 100,
                     popup = ~hov_text,
                     label = ~booth,
                     lat = ~lat,
                     lng = ~lon,
                     labelOptions = labelOptions(permanent = FALSE),
                     color = "black",
                     weight = 1,
                     fillColor = ~pal(votes_sh_scale),
                     fill = TRUE) %>% 
    
    addLegend(title = str_c(party2, " vs ALP"), pal = pal, values = p_votes_by_phys_booth$votes_sh,
              labels = palette(),
              position = "bottomright")
  
}

cre_two_pp_detail <- function(p_two_pp = two_pp, p_cands_std = cands_std, p_elec_dates = elec_dates) {
  # Create two_pp_detail datafram
  booths_exclude <- c("Ordinary Votes Total")
  
  p_two_pp %>% 
    filter(!(booth %in% booths_exclude)) %>% 
    # join to party
    left_join(p_cands_std %>% select(candidate, party_std, year), by = c("candidate", "year")) %>% 
    # join to date
    left_join(p_elec_dates %>% select(elec_ID, date), by = c("year" = "elec_ID")) 
}

# two_pp_all_booth <-
cre_two_pp_all_booth <- function(p_two_pp_detail = two_pp_detail) {
  # Create 2 party preferred for all booths
  p_two_pp_detail %>% 
    mutate(party_std = coalesce(party_std, "Informal")) %>% 
    group_by(party_std, date) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    # complete(party_std, date) %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(non_dom_party = case_when(
      is.null(party_std) ~ FALSE,
      party_std == dom_party_name ~ FALSE,
      party_std == "Informal" ~ FALSE,
      TRUE ~ TRUE
    ),
    booth = "All booths",
    hov_text = str_c(booth, "<br>", party_std, "<br>2pp: ", scales::percent(votes_sh)))
}

plot_two_pp_all_booth <- function(p_two_pp_all_booth = two_pp_all_booth) {
  # Plot 2 party preferred for all booths
  two_pp_plot <- 
    p_two_pp_all_booth %>% 
    ggplot(aes(x = date, y = votes_sh, colour = party_std, shape = party_std)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, aes(text = hov_text)) +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, colour = "grey") +
    scale_y_continuous("2 party preferred share", labels = scales::percent) +
    scale_color_manual("Party", values = party_colours()) +
    scale_shape_discrete("Party") +
    labs(title = "Two party preferred share", x = "")
  
  ggplotly(two_pp_plot, tooltip = "text")
}

cre_two_pp_by_booth_nondom <- function(p_two_pp_detail = two_pp_detail) {
  # tibble with the non-dominant party
  p_two_pp_detail %>% 
    group_by(booth, date, party_std) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    group_by(booth, date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(!is.na(party_std)) %>% 
    # remove informal votes from chart
    filter(party_std != dom_party_name) %>% 
    # semi_join(booth_addr_lst, by = "booth") %>% 
    mutate(hov_text = str_c(booth, "<br>", party_std, "<br>", scales::percent(votes_sh)))
    
}

plot_two_pp_by_booth_nondom <- 
  function(p_two_pp_by_booth_nondom = two_pp_by_booth_nondom, p_booth = "Westgarth") {
    # Plot 2 party preferred share by booth
    two_pp_by_booth_ggplot <-
      p_two_pp_by_booth_nondom %>% 
      semi_join(booth_addr_lst, by = "booth") %>% 
      # mutate(booth = factor(booth)) %>% 
      ggplot(aes(x = date, y = votes_sh, shape = party_std, group = booth)) +
      geom_line(na.rm = TRUE, colour = "grey", aes(group = booth, shape = party_std)) +
      geom_point(na.rm = TRUE, colour = "grey", size = 0.5, aes(text = hov_text)) +
      geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, colour = "grey") +
      scale_y_continuous("2 party preferred share", labels = scales::percent) +
      # scale_shape_manual("Booth", values = 97:111) +
      # scale_shape_identity() +
      geom_line(data = two_pp_by_booth_nondom %>% filter(booth == p_booth), colour = "black") +
      geom_point(data = two_pp_by_booth_nondom %>% filter(booth == p_booth), aes(text = hov_text, colour = party_std)) +
      scale_color_manual("Party", values = party_colours()) +
      scale_shape_discrete("Party") +
      geom_line(data = two_pp_all_booth %>% filter(non_dom_party), colour = "blue") +
      geom_point(data = two_pp_all_booth %>% filter(non_dom_party), aes(text = hov_text, colour = party_std)) +
      labs(title = str_c("Two party preferred share by booth: ", p_booth, " highlighted", sep = ""), x = "")
    
    
    ggplotly(two_pp_by_booth_ggplot, tooltip = "hov_text")
  }

cre_votes_by_booth_elec <- function(p_first_pref = first_pref, p_elec_dates = elec_dates, p_two_pp_by_booth_nondom = two_pp_by_booth_nondom) {
  p_first_pref %>% 
    inner_join(p_elec_dates %>% select(year = elec_ID, date), by = "year") %>% 
    group_by(date, booth, year) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(p_two_pp_by_booth_nondom %>% select(-votes), by = c("date", "booth")) %>% 
    filter(!is.na(party_std)) 
}

cre_votes_by_booth_all <- function(p_votes_by_booth_elec = votes_by_booth_elec) {
  p_votes_by_booth_elec %>% 
    filter(votes != 0) %>% 
    group_by(date, year, party_std) %>% 
    summarise(booth = "All",
              votes_sh = sum(votes_sh * votes / sum(votes, na.rm = TRUE)),
              votes = sum(votes, na.rm = TRUE),
              hov_text = NA_character_) %>% 
    ungroup() %>% 
    select(names(votes_by_booth_elec)) %>% 
    union(votes_by_booth_elec) %>% 
    filter(votes != 0) 
  # mutate(booth_ggp = fct_lump(booth, 8, w = votes))
} 

plot_votes_by_booth <- function(p_votes_by_booth_all = votes_by_booth_all, p_lump_n = 8){
  votes_by_elect_ggp <-
    p_votes_by_booth_all %>% 
    mutate(booth_ggp = fct_lump(booth, p_lump_n, w = votes) %>% fct_relevel("All")) %>% 
    group_by(date, booth_ggp, year) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(if_else(booth_ggp == "All", votes, 0), na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(hov_text = str_c(booth_ggp, ": ", scales::comma(votes), "<br>Share of All: ", scales::percent(votes_sh))) %>% 
    
    ggplot(aes(x = date, y = votes)) +
    # scale_y_continuous("Votes", labels = scales::comma) +
    scale_y_log10("Votes (log scale)", labels = scales::comma) +
    scale_shape_discrete("Polling Booth") +
    expand_limits(y = 0) +
    geom_point(aes(text = hov_text, shape = booth_ggp)) +
    geom_line(colour = "grey", size = 0.25, aes(group = booth_ggp)) +
    geom_line(data = votes_by_booth_all %>% filter(booth == "All"), colour = "red", size = 0.5) +
    labs(title = "Total votes", x = "")
  
  ggplotly(votes_by_elect_ggp, tooltip = "text")
}

plot_votes_by_booth_all <- function(p_votes_by_booth_all = votes_by_booth_all, p_booth_sel = "Westgarth") {
  votes_by_elect_booth_ggp <-
    p_votes_by_booth_all %>% 
    filter(booth != "All") %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(hov_text = str_c(booth, ": ", scales::comma(votes), "<br>Share of total: ", scales::percent(votes_sh))) %>% 
    
    ggplot(aes(x = date, y = votes)) +
    scale_y_log10("Votes (log scale)", labels = scales::comma) +
    expand_limits(y = 0) +
    geom_point(aes(text = hov_text), size = 0.25) +
    geom_line(colour = "grey", size = 0.25, aes(group = booth)) +
    geom_line(data = votes_by_booth_all %>% filter(booth == p_booth_sel), colour = "red", size = 0.5) +
    labs(title = str_c("Total votes by booth: ", p_booth_sel, " highlighted"), x = "")
  
  ggplotly(votes_by_elect_booth_ggp, tooltip = "text")
}

cre_party_votes_by_elec <- function(p_first_pref = first_pref, p_elec_dates = elec_dates) {
  p_first_pref %>% 
    inner_join(p_elec_dates %>% select(year = elec_ID, date), by = "year") %>% 
    group_by(date, cand_party) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(cand_party = cand_party %>% str_replace_all(., "\\'", "") %>% str_trim()) %>% 
    left_join(party_std, by = "cand_party")
}

plot_party_votes_by_elec <- function(p_party_votes_by_elec = party_votes_by_elec){
  party_votes_by_elec_ggp <-
    p_party_votes_by_elec %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE),
           party_std = coalesce(party_std, "Informal") %>% fct_relevel("Informal", after = Inf),
           hov_text = str_c(party_std, "<br>Votes: ", scales::comma(votes), " (", scales::percent(votes_sh), ")")) %>% 
    ungroup %>% 
    complete(date, party_std) %>% 
    
    ggplot(aes(x = date, y = votes)) +
    geom_point(aes(text = hov_text, colour = party_std, fill = party_std), size = 0.5) +
    geom_line(aes(group = party_std, colour = party_std), size = 0.25, na.rm = TRUE) +
    scale_fill_manual("Party", values = party_colours()) +
    scale_colour_manual("Party", values = party_colours()) +
    scale_y_log10(label = scales::comma) +
    labs(title = "Total first preference votes by party", x = "") +
    expand_limits(y = 0) +
    geom_line(data = votes_by_booth_all %>% filter(booth == "All"), colour = "grey", size = 0.25, aes(group = booth, text = "Total"))
  
  ggplotly(party_votes_by_elec_ggp, tooltip = "text")
}

cre_pref_w_party <- function(p_pref = pref, p_cands_std = cands_std) {
  p_pref %>% 
    left_join(p_cands_std %>% select(-cand_party) %>% rename_all(~str_c(., ".from")), 
              by = c("from_cand" = "candidate.from", "year" = "year.from")) %>% 
    left_join(p_cands_std %>% select(-cand_party) %>% rename_all(~str_c(., ".to")), 
              by = c("to_cand" = "candidate.to", "year" = "year.to"))
}

plot_distn_party_prefs <- function(p_party_from = "Liberal", p_pref_w_party = pref_w_party, p_elec_dates = elec_dates) {
  pref_w_party_ggp <-
    p_pref_w_party %>% 
    # complete(year, party_std.from) %>% 
    filter(party_std.from == p_party_from) %>% 
    filter(party_std.from != party_std.to) %>% 
    # Ignore preferences from Independents to Independents
    group_by(year, party_std.to, to_cand) %>%
    summarise(votes = sum(votes, na.rm = TRUE)) %>%
    group_by(year) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(hovtext = str_c(party_std.to, " (", to_cand, ")<br>", votes, " votes. (", scales::percent(votes_sh), ")")) %>%
    # filter(year == "2017") %>% arrange(party_std.to) %>% 
    full_join(p_elec_dates %>% select(year = elec_ID) %>% filter(year >= "2010"), by = "year") %>% 
    
    ggplot(aes(x = year, y = votes)) +
    geom_bar(aes(fill = party_std.to, text = hovtext), stat = "identity", position = "dodge") +
    scale_fill_manual("Party", values = party_colours()) +
    
    labs(title = str_c("Distribution of ", p_party_from, " votes by election"), x = "", y = "votes")
  
  ggplotly(pref_w_party_ggp, tooltip = "text")
}

readable_nm <- function(nm) {
  # Rewrite the candidate name to something more readable
  str_c(str_replace(nm, "^.*, ", ""), " ", str_replace(nm, ", .*", "") %>% str_to_title()) %>% 
    str_replace_all(., c("Mcc" = "McC", "D'a" =  "D'A", "Maci" = "MacI"))
}

cre_cand_list <- function(p_cands_std = cands_std) {
  # Create named vector of candidates with their party and years of running
  p_cands_std %>% 
    filter(year >= "2010") %>% 
    group_by(candidate) %>% 
    summarise(year_num = n(),
              years = str_c(year, collapse = ","),
              parties = str_c(unique(party_std), collapse = ",")) %>% 
    ungroup %>% 
    # arrange(-year_num)
    arrange(candidate) %>% 
    mutate(cand_full = str_c(readable_nm(candidate), ": ", parties, " (", years, ")")) %>% 
    select(candidate, cand_full) %>% 
    deframe()
}

plot_pref_distn_sel_cand <- function(p_candidate = "BISHOP, Robert", p_pref_w_party = pref_w_party) {
  # Plot distribution of preferences for selected candidate
  pref_w_cand_ggp <- 
    p_pref_w_party %>% 
    filter(from_cand == p_candidate) %>% 
    mutate(hovtext = str_c(party_std.to, " (", to_cand, ")<br>", votes, " votes. (", scales::percent(votes / votes_sum), ")")) %>% 
    # glimpse()
    
    ggplot(aes(x = year, y = votes)) +
    geom_bar(aes(fill = party_std.to, text = hovtext), stat = "identity", position = "dodge") +
    scale_fill_manual("Party", values = party_colours()) +
    
    labs(title = str_c("Distribution of ", readable_nm(p_candidate), " preferences by election"), x = "", y = "votes")  
  
  ggplotly(pref_w_cand_ggp, tooltip = "text")
}

cre_votes_by_phys_booth <- function(p_votes_by_booth_all = votes_by_booth_all, p_booth_addr_lst = booth_addr_lst) {
  p_votes_by_booth_all %>% 
    filter(booth != "All") %>% 
    inner_join(p_booth_addr_lst, by = "booth") %>% 
    mutate(hov_text = str_c(booth, "<br>", booth_addr, "<br>Votes: ", votes, "<br>2pp to ", party_std, ": ", scales::percent(votes_sh) )) %>% 
    group_by(year) %>% 
    mutate(votes_sh_scale = scales::rescale(votes_sh)) %>% # Scale votes for colour coding on chart
    ungroup
}

plot_2pp_vote_ts <- function(p_two_pp_all_booth = two_pp_all_booth) {
  
  two_pp_total <-
    p_two_pp_all_booth %>% 
    group_by(date) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(hov_text = str_c("Total: ", votes))
  
  two_pp_plot <-
    two_pp_all_booth %>% 
    mutate(hov_text = str_c(party_std, "<br>Votes: ", votes, "<br>Share: ", scales::percent(votes_sh))) %>% 
    
    ggplot(aes(x = date, y = votes)) +
    geom_line(na.rm = TRUE, aes(colour = party_std)) +
    geom_point(na.rm = TRUE, aes(colour = party_std, shape = party_std, text = hov_text)) +
    scale_y_log10("2 party preferred votes", labels = scales::comma) +
    scale_color_manual("Party", values = party_colours()) +
    scale_shape_discrete("Party") +
    
    geom_line(data = two_pp_total, colour = "black") +
    geom_point(data = two_pp_total, colour = "black", aes(text = hov_text)) +
    
    labs(title = "Two party preferred votes", x = "")
  
  ggplotly(two_pp_plot, tooltip = "text")  
}  

plot_booth_votes_bar <- function(p_year = "2018", p_votes_by_booth_all = votes_by_booth_all){
  
  votes_for_year <-
    p_votes_by_booth_all %>% 
    filter(year == p_year) %>% 
    filter(booth != "All") %>% 
    group_by(year) %>% 
    mutate(elect_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(hov_text = str_c(booth, "<br>Votes: ", votes, "<br>Share of electorate: ", scales::percent(elect_sh),
                            "<br>Share to ", party_std, ": ", scales::percent(votes_sh)))
  
  party2 <-
    votes_for_year %>% 
    pull(party_std) %>% 
    head(1)
  
  party2_sh_rng <-
    votes_for_year$votes_sh %>% 
    keep(~ . > 0) %>% 
    range()
  
  votes_by_booth_all_ggp <-
    votes_for_year %>% 
    mutate(booth = booth %>% fct_reorder(votes)) %>% 
    # glimpse()
    
    ggplot(aes(x = booth, y = votes)) +
    geom_bar(aes(fill = votes_sh, text = hov_text), stat = "identity") +
    scale_fill_gradientn("2pp share away from ALP", 
                         colours = c(party_colours()["Australian Labor Party"], "grey", party_colours()[party2]),
                         limits = c(floor(party2_sh_rng[1] * 10) / 10, ceiling(party2_sh_rng[2] * 10) / 10),
                         labels = scales::percent) +
    coord_flip()
  
  ggplotly(votes_by_booth_all_ggp, tooltip = "text")
}