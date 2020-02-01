
# Constants ---------------------------------------------------------------

dom_party_name <- "Australian Labor Party"
# data_dir <- "./data"

# Transformed dataframes
transf_df <- c("two_pp_detail",
               "two_pp_all_booth",
               "two_pp_by_booth_nondom",
               "votes_by_booth_elec",
               "votes_by_booth_all",
               "party_votes_by_elec",
               "pref_w_party",
               "cand_list",
               "votes_by_phys_booth",
               "two_pp_sum",
               "distn",
               "elec_dates",
               "pref",
               "two_pp",
               "nrthct_plgon",
               "party_std", #
               "booth_addr_lst",
               "northcote_stn",
               "cands_std",
               "first_pref",
               "tot_enrols"
)

objects_core <- c("booth_addr_lst",
                  "cands_std",
                  "distn",
                  "elec_dates",
                  "first_pref",
                  "nrthct_plgon",
                  "pref",
                  "two_pp",
                  "elec_dates",
                  "party_std")

write_dfs <- function(p_objs, p_dir = data_dir) {
  # Write objects specified in list to rds file
  p_objs %>% 
    walk(., ~ write_rds(get(.), str_c(p_dir, str_c(., ".rds", sep = ""), sep = "/"), "none"))
}


retrieve_dfs <- function(p_objs, p_dir = data_dir){
  # Retrieve objects specified in list from rds file
  p_objs %>% 
    walk(., ~ assign(., read_rds(str_c(p_dir, str_c(., ".rds", sep = ""), sep = "/")), envir = .GlobalEnv))
}

# write_dfs("votes_by_phys_booth", "./Election_EDA/data")
# getwd()

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
    append(c("Informal" = "grey", "Total" = "black", "white" = "white", "Didnt Vote" = "brown"))
}

print_booth_map <- function(p_votes_by_phys_booth = votes_by_phys_booth, p_elec = "2018", p_booth = "") {
  # Print leaflet map of polling booths
  
  p_votes_by_phys_booth <-
    p_votes_by_phys_booth %>% 
    filter(year == p_elec) %>% 
    mutate(alp_sh = 1 - votes_sh) %>% 
    mutate(hov_text = str_c(booth, "<br>", booth_addr, "<br>Votes: ", 
                            votes, "<br>2pp to ", party_std, ": ", scales::percent(votes_sh, accuracy = 0.1) )) 
  
  
  party2 <-
    p_votes_by_phys_booth %>% 
    filter(year == p_elec) %>% 
    pull(party_std) %>% 
    head(1) %>% 
    str_replace(., "Australian ", "") # Trim Australian Greens to Greens
  
  pal_spect <- "RdYlGn"  # Assume that Greens are the 2nd party
  
  if (party2 == "Liberal") { pal_spect <- "RdBu" } # i.e. Liberals are the 2nd party
  
  pal <- colorNumeric(
    palette = pal_spect,
    domain = p_votes_by_phys_booth$votes_sh,
    reverse = FALSE)
  
  booth_map <- 
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
                     fillColor = ~pal(votes_sh),
                     fill = TRUE) %>% 
    
    addLegend(position = "topright",
              title = str_c(party2, " 2pp<br> vs ALP"), 
              pal = pal, 
              values = p_votes_by_phys_booth$votes_sh,
              labels = palette(),
              # opacity = .5,
              labFormat = scales::percent)
  
  if (has_element(p_votes_by_phys_booth$booth, p_booth)) {
    booth_map <-
      booth_map %>% 
      addPopups(data = p_votes_by_phys_booth %>% filter(year == p_elec, booth == p_booth),
                popup = ~booth,
                lat = ~lat,
                lng = ~lon)
    
  }
  
  booth_map
  
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
    mutate(votes_sh = votes / sum(if_else(party_std != "Informal", votes, 0L), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(non_dom_party = case_when(
      is.null(party_std) ~ FALSE,
      party_std == dom_party_name ~ FALSE,
      party_std == "Informal" ~ FALSE,
      TRUE ~ TRUE
    ),
    booth = "All booths",
    hov_text = str_c(booth, "<br>", party_std, "<br>2pp: ", scales::percent(votes_sh, accuracy = 0.1)))
}

plot_two_pp_all_booth <- function(p_two_pp_all_booth = two_pp_all_booth) {
  # Plot 2 party preferred for all booths
  
  didnt_vote <-
    p_two_pp_all_booth %>% 
    group_by(date) %>% 
    summarise(votes = sum(votes, na.rm = TRUE),
              votes_count = sum(if_else(party_std != "Informal", votes, 0L), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(party_std = "Didnt Vote",
           year = year(date) %>% as.character(),
           votes = map_dbl(year, ~tot_enrols[[.]]) %>% as.integer() - votes,
           votes_sh = votes / votes_count,
           non_dom_party = TRUE,
           booth = "All booths",
           hov_text = str_c("Didn't vote: ", scales::percent(votes_sh, accuracy = 0.1))) %>% 
    select(-votes_count) 
  
  two_pp_plot <- 
    p_two_pp_all_booth %>% 
    bind_rows(didnt_vote) %>% 
    ggplot(aes(x = date, y = votes_sh, colour = party_std, shape = party_std, text = hov_text, group = party_std)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 0.5) +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, colour = "grey") +
    scale_y_continuous("2 party preferred share", labels = scales::percent) +
    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y") +
    scale_color_manual("Party", values = party_colours()) +
    scale_shape_discrete("Party") +
    labs(title = "Two party preferred", x = "", y = "Share of eligible votes")
  
  ggplotly(two_pp_plot, tooltip = "text")
}

plot_2pp_vote_ts <- function(p_two_pp_all_booth = two_pp_all_booth) {
  
  two_pp_total <-
    p_two_pp_all_booth %>% 
    group_by(date, booth) %>% 
    summarise(votes = sum(votes, na.rm = TRUE),
              party_std = "Total") %>% 
    ungroup() %>% 
    mutate(hov_text = str_c("Total: ", votes))
  
  didnt_vote_total <- 
    merge_tot_votes_enrolled() %>% 
    select(date, booth, votes, party_std, hov_text)
  
  two_pp_plot <-
    p_two_pp_all_booth %>% 
    mutate(hov_text = str_c(party_std, "<br>Votes: ", votes, "<br>Share: ", scales::percent(votes_sh, accuracy = 0.1))) %>% 
    
    ggplot(aes(x = date, y = votes, colour = party_std, shape = party_std, text = hov_text, group = party_std)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE, size = 0.5) +
    # scale_y_log10("2 party preferred votes", labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual("Party", values = party_colours()) +
    scale_shape_discrete("Party") +
    
    # theme(legend.position = "none") +
    
    # geom_line(data = two_pp_total) +
    # geom_point(data = two_pp_total, size = 0.5) +
    geom_line(data = didnt_vote_total) +
    geom_point(data = didnt_vote_total, size = 0.5) +
    
    labs(title = "Two party preferred votes", x = "", y = "Votes") +
    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y")
  
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
    mutate(hov_text = str_c(booth, "<br>", party_std, "<br>", scales::percent(votes_sh, accuracy = 0.1)))
    
}

plot_two_pp_by_booth_nondom <- 
  function(p_two_pp_by_booth_nondom = two_pp_by_booth_nondom, p_booth = "Westgarth", p_two_pp_all_booth = two_pp_all_booth) {
    # Plot 2 party preferred share by booth
    
    p_two_pp_by_booth_nondom <-
      p_two_pp_by_booth_nondom %>% 
      filter(votes > 0) %>% 
      mutate(non_dom_party = TRUE,
             hov_text = str_c(hov_text, " of ", scales::comma(votes / votes_sh)))  
    
    # Create list of entries for the legend
    # note that this doesn't appear to work for ggplotly
    legend_list <-
      p_two_pp_by_booth_nondom %>% 
      filter(booth == p_booth) %>% 
      mutate(leg_text = str_c(booth, ",<br>", str_replace(party_std, "Australian ", ""), "")) %>% # eg "Westgarth, Liberal 2pp"
      select(party_std, leg_text) %>%
      rbind(tibble(leg_text = "All polling<br>stations", party_std = "Total")) %>% 
      distinct() %>% 
      deframe()
    
    two_pp_by_booth_ggplot <-
      p_two_pp_by_booth_nondom %>% 
      filter(booth != p_booth) %>% # filter out the selected polling station as it will be added later
      ggplot(aes(x = date, y = votes_sh, shape = party_std, group = booth, text = hov_text, colour = party_std)) +
      geom_line(aes(colour = NULL, shape = NULL), na.rm = TRUE, size = 0.15, colour = "grey", show.legend = FALSE, linetype = "dotted") +
      geom_line(na.rm = TRUE, size = 0.15, colour = "grey", show.legend = FALSE) +
      geom_point(na.rm = TRUE, size = 0.5, colour = "grey", show.legend = FALSE) +
      geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, colour = "grey", show.legend = FALSE) +
      scale_y_continuous("2 party preferred share", labels = scales::percent) +
      geom_line(data = p_two_pp_by_booth_nondom %>% filter(booth == p_booth)) +
      geom_point(data = p_two_pp_by_booth_nondom %>% filter(booth == p_booth), size = 1, show.legend = FALSE) +
      theme(legend.position = "bottom") +  # This isn't working with plotly
      geom_line(data = p_two_pp_all_booth %>% filter(non_dom_party) %>% mutate(party_std = "Total")) +
      geom_point(data = p_two_pp_all_booth %>% filter(non_dom_party), colour = "black", size = 1, show.legend = FALSE) +
      
      scale_color_manual(name = "Polling station", values = party_colours(),
                         labels = legend_list %>% names(),
                         breaks = legend_list) +

            labs(title = str_c("Two party preferred share by polling station: ", p_booth, " highlighted", sep = ""), x = "") +
      scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y") +
      theme(legend.position = "none")
    
    
    two_pp_by_booth_ggplotly <-
      ggplotly(two_pp_by_booth_ggplot, tooltip = "hov_text") # %>%  
      # layout(legend = list(orientation = "h", y = -0.2))
    
    two_pp_by_booth_ggplotly$x$data <- 
      two_pp_by_booth_ggplotly$x$data %>% 
      map(rmv_invalid_leg, legend_list)
    
    two_pp_by_booth_ggplotly 
    # two_pp_by_booth_ggplot

  }

rmv_invalid_leg <- function(plotly_x, .legend_list) {
  # Inpects an x$data list in a plotly object, removes any invalid legend entries and returns a transformed x$data object
  if ("legendgroup" %in% names(plotly_x)) {
    # The list includes a legend group
    
    if (str_detect(plotly_x$legendgroup, "^\\(")) {
      # Legend group looks invalid
      plotly_x$showlegend <- FALSE
    }
    
    plotly_x$name <- str_replace_all(plotly_x$name, .legend_list)
    
  }
  plotly_x
  
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
    mutate(hov_text = str_c(booth_ggp, ": ", scales::comma(votes), "<br>Share of All: ", scales::percent(votes_sh, accuracy = 0.1))) %>% 
    
    ggplot(aes(x = date, y = votes)) +
    # scale_y_continuous("Votes", labels = scales::comma) +
    scale_y_log10("Votes (log scale)", labels = scales::comma) +
    scale_shape_discrete("Polling Booth") +
    expand_limits(y = 0) +
    geom_point(aes(text = hov_text, shape = booth_ggp)) +
    geom_line(colour = "grey", size = 0.25, aes(group = booth_ggp)) +
    geom_line(data = votes_by_booth_all %>% filter(booth == "All"), colour = "red", size = 0.5) +
    labs(title = "Total votes", x = "") +
    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y")
  
  ggplotly(votes_by_elect_ggp, tooltip = "text")
}

plot_votes_by_booth_all <- function(p_votes_by_booth_all = votes_by_booth_all, p_booth_sel = "Westgarth") {

  votes_by_booth_prep <-
    p_votes_by_booth_all %>% 
    filter(booth != "All") %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(hov_text = str_c(booth, ": ", scales::comma(votes), "<br>Share of total: ", scales::percent(votes_sh, accuracy = 0.1))) 
    
  # Create list of entries for the legend
  legend_list <-
    votes_by_booth_prep %>% 
    filter(booth == p_booth_sel) %>% 
    mutate(leg_text = str_c(booth, ",<br>", str_replace(party_std, "Australian ", ""))) %>% # eg "Westgarth, Liberal"
    select(party_std, leg_text) %>%
    rbind(tibble(leg_text = "All polling<br>stations", party_std = "Total")) %>% 
    distinct() %>% 
    deframe()
  
  votes_by_elect_booth_ggp <-
    votes_by_booth_prep %>% 
    
    ggplot(aes(x = date, y = votes, text = hov_text, group = booth, colour = party_std, shape = party_std)) +
    # scale_y_log10("Votes (log scale)", labels = scales::comma) +
    scale_y_continuous("Votes", labels = scales::comma) +
    geom_point(size = 0.25, colour = "black") +
    geom_line(colour = "grey", size = 0.25) +
    geom_point(data = votes_by_booth_prep %>% filter(booth == p_booth_sel), size = 1) +
    geom_line(data = votes_by_booth_prep %>% filter(booth == p_booth_sel)) +
    labs(title = str_c("Total votes by polling station: ", p_booth_sel, " highlighted"), x = "") +
    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y") +
    
    scale_color_manual(name = "Polling station", values = party_colours(),
                       labels = legend_list %>% names(),
                       breaks = legend_list) +
    scale_shape_discrete("Polling station")

  votes_by_elect_booth_ggplotly <-
    ggplotly(votes_by_elect_booth_ggp, tooltip = "text") #%>%  
    # layout(legend = list(orientation = "h", y = -0.2))
  
  votes_by_elect_booth_ggplotly$x$data <- 
    votes_by_elect_booth_ggplotly$x$data %>% 
    map(rmv_invalid_leg, legend_list)
  
  votes_by_elect_booth_ggplotly
  
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

merge_tot_votes_enrolled <- function(p_votes_by_booth_all = votes_by_booth_all, p_tot_enrols = tot_enrols) {
  # Merge total votes per election with total enrolled
  
  p_votes_by_booth_all %>% 
    filter(booth == "All") %>%   # Total votes
    select(-votes_sh) %>% 
    mutate(tot_enrol = map_dbl(year, ~p_tot_enrols[[.]]),
           party_std = "Didnt Vote",
           votes = tot_enrol - votes,
           hov_text = str_c("Didn't vote<br>", scales::comma(votes)),
           booth = "All booths")
}

plot_party_votes_by_elec <- function(p_party_votes_by_elec = party_votes_by_elec){
  
  didnt_vote_total <-
    merge_tot_votes_enrolled() %>% 
    select(date, booth, votes, party_std, hov_text)
  
  party_votes_by_elec_ggp <-
    p_party_votes_by_elec %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(party_std = coalesce(party_std, "Informal") %>% fct_relevel("Informal", after = Inf),
           hov_text = str_c(party_std, "<br>Votes: ", scales::comma(votes), " (", scales::percent(votes_sh, accuracy = 0.1), ")")) %>% 
    complete(date, party_std) %>% 
    
    ggplot(aes(x = date, y = votes, group = party_std, text = hov_text, colour = party_std, fill = party_std)) +
    geom_point(size = 1) +
    geom_line(size = 0.25, na.rm = TRUE, linetype = "solid") +
    scale_fill_manual("Party", values = party_colours()) +
    scale_colour_manual("Party", values = party_colours()) +
    # scale_y_log10(label = scales::comma) +
    labs(title = "Total first preference votes by party", x = "") +
    expand_limits(y = 0) +
    # geom_line(data = votes_by_booth_all %>% filter(booth == "All"), colour = "grey", size = 0.25, aes(group = booth)) +
    
    geom_point(data = didnt_vote_total, size = 1) +
    geom_line(data = didnt_vote_total, size = 0.25, na.rm = TRUE, linetype = "solid") +

    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y") +
    scale_y_continuous("Votes", labels = scales::comma)
  
  ggplotly(party_votes_by_elec_ggp, tooltip = "text")
}

cre_pref_w_party <- function(p_pref = pref, p_cands_std = cands_std) {
  p_pref %>% 
    bind_rows(cre_distn_gap()) %>% 
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
    mutate(hovtext = str_c(party_std.to, " (", to_cand, ")<br>", votes, " votes. (", scales::percent(votes_sh, accuracy = 0.1), ")")) %>%
    # filter(year == "2017") %>% arrange(party_std.to) %>% 
    full_join(p_elec_dates %>% select(year = elec_ID) %>% filter(year >= "2010"), by = "year") %>% 
    
    ggplot(aes(x = year, y = votes)) +
    geom_bar(aes(fill = party_std.to, text = hovtext), stat = "identity", position = "dodge") +
    scale_fill_manual("Party", values = party_colours()) +
    
    labs(title = str_c("Distribution of ", p_party_from, " votes by election"), x = "", y = "votes")
  
  ggplotly(pref_w_party_ggp, tooltip = "text")
}

plot_distn_party_prefs_rec <- function(p_party_to = c("Australian Greens", "Australian Labor Party"),
                                       p_pref_w_party = pref_w_party, p_elec_dates = elec_dates) {
  pref_w_party_ggp <-
    p_pref_w_party %>% 
    # complete(year, party_std.from) %>% 
    filter(party_std.to %in% p_party_to) %>% 
    filter(party_std.to != party_std.from) %>% 
    # Ignore preferences from Independents to Independents
    group_by(year, party_std.from, from_cand, party_std.to) %>%
    summarise(votes = sum(votes, na.rm = TRUE)) %>%
    group_by(year) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(hovtext = str_c(party_std.from, " (", from_cand, ")<br>", votes, " votes. (", scales::percent(votes_sh, accuracy = 0.1), ")<br>",
                           "distributed to ", party_std.to)) %>%
    # filter(year == "2017") %>% arrange(party_std.to) %>% 
    full_join(p_elec_dates %>% select(year = elec_ID) %>% filter(year >= "2010"), by = "year") %>% 
    mutate(party_std.to = if_else(is.na(party_std.to), p_party_to[[1]], party_std.to),
           party_std.from = party_std.from %>% abbreviate(minlength = 8) %>% fct_reorder(votes, .fun = sum, .desc = TRUE)) %>%
    
    ggplot(aes(x = year, y = votes, fill = party_std.to, text = hovtext)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual("Party", values = party_colours()) +
    facet_grid(party_std.from ~ ., scales = "fixed") +
    
    labs(title = str_c("Distribution of ", p_party_to, " votes by election"), x = "", y = "votes distributed")
  
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
    select(cand_full, candidate) %>% 
    deframe()
}

plot_pref_distn_sel_cand <- function(p_candidate = "BISHOP, Robert", p_pref_w_party = pref_w_party) {
  # Plot distribution of preferences for selected candidate
  pref_w_cand_ggp <- 
    p_pref_w_party %>% 
    filter(from_cand == p_candidate) %>% 
    mutate(hovtext = str_c(party_std.to, " (", to_cand, ")<br>", votes, " votes. (", scales::percent((votes / votes_sum), accuracy = 0.1), ")")) %>% 
    full_join(elec_dates %>% select(year = elec_ID) %>% filter(year >= "2010"), by = "year") %>%
    # glimpse()
    
    ggplot(aes(x = year, y = votes, fill = party_std.to, text = hovtext)) +
    geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
    scale_fill_manual("Party", values = party_colours(),
                      limits = p_pref_w_party %>% filter(from_cand == p_candidate) %>% pull(party_std.to) %>% unique(),
                      na.translate = TRUE) +
    
    labs(title = str_c("Distribution of ", readable_nm(p_candidate), " preferences by election"), x = "", y = "votes")  
  
  ggplotly(pref_w_cand_ggp, tooltip = "text")
}

cre_votes_by_phys_booth <- function(p_votes_by_booth_all = votes_by_booth_all, p_booth_addr_lst = booth_addr_lst) {
  
  booth_addr_dates <-
    booth_addr_lst %>% 
    mutate(year_fr = coalesce(year_fr, "1999"),
           year_to = coalesce(year_to, "2999"))
  
  p_votes_by_booth_all %>% 
    filter(booth != "All") %>% 
    fuzzyjoin::fuzzy_join(booth_addr_dates, 
                          by = c("booth" = "booth", "year" = "year_fr", "year" = "year_to"),
                          match_fun = list(`==`, `>=`, `<=`)) %>% 
    rename(booth = booth.x) %>% 
    mutate(hov_text = str_c(booth, "<br>", booth_addr, "<br>Votes: ", votes, "<br>2pp to ", party_std, ": ", scales::percent(votes_sh, accuracy = 0.1) )) %>% 
    group_by(year) %>% 
    mutate(votes_sh_scale = scales::rescale(votes_sh)) %>% # Scale votes for colour coding on chart
    ungroup
}


plot_booth_votes_bar <- function(p_year = "2018", p_votes_by_booth_all = votes_by_booth_all){
  
  votes_for_year <-
    p_votes_by_booth_all %>% 
    filter(year == p_year) %>% 
    filter(booth != "All") %>% 
    group_by(year) %>% 
    mutate(elect_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(hov_text = str_c(booth, "<br>Votes: ", votes, "<br>Share of electorate: ", scales::percent(elect_sh, accuracy = 0.1),
                            "<br>Share to ", party_std, ": ", scales::percent(votes_sh, accuracy = 0.1))) %>% 
    mutate(phys_stn = match(booth, booth_addr_lst$booth),
           phys_stn = if_else(is.na(phys_stn), "Remote", "Local"))
  
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
    
    ggplot(aes(x = booth, y = votes, text = hov_text)) +
    geom_bar(aes(colour = phys_stn, fill = votes_sh), stat = "identity") +
    scale_colour_manual("Station type", values = c("Remote" = "#d9d9d9", "Local" = "#636363")) +
    scale_fill_gradientn(str_c(str_replace(party2, "Australian ", ""), " 2pp share\nvs ALP"),
                         colours = c(party_colours()["Australian Labor Party"], "grey", party_colours()[party2]),
                         limits = c(floor(party2_sh_rng[1] * 10) / 10, ceiling(party2_sh_rng[2] * 10) / 10),
                         labels = scales::percent) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = str_c("Polling station attendance in ", p_year),
         x = "", y = "Votes") +
    coord_flip()
  
  ggplotly(votes_by_booth_all_ggp, tooltip = "text", source = "booth_votes_bar")

}

cre_two_pp_sum <- function(p_two_pp = two_pp) {
  p_two_pp %>% 
    filter(!str_detect(booth, "Total")) %>% 
    group_by(year, candidate) %>% 
    summarise(votes = sum(votes, na.rm = TRUE)) %>% 
    ungroup()
}

first_final_votes_no_distn <- function(p_year = "2002") {
  # Summarise candidates first and final votes without access to distribution data
  first_pref %>% 
    filter(year == p_year, candidate != "informal") %>% 
    group_by(year, candidate) %>% 
    summarise(votes_min = sum(votes, na.rm = TRUE)) %>% 
    group_by(year) %>% 
    mutate(votes_min_sum = sum(votes_min)) %>% 
    ungroup() %>% 
    left_join(two_pp_sum %>% dplyr::rename(votes_max = votes), by = c("year", "candidate")) %>% 
    left_join(cands_std %>% select(-party_colour), by = c("candidate", "year")) %>% 
    mutate(votes_redist = coalesce(as.double(votes_max), votes_min) - votes_min,
           votes_max_pool = sum(votes_max, na.rm = TRUE),
           hov_text = str_c("1st pref: ", scales::comma(votes_min), " (", scales::percent((votes_min / votes_min_sum), accuracy = 0.1), ")",
                            ". Pref distn: ", if_else(is.na(votes_max), "NA", 
                                    str_c(scales::comma(votes_redist), ".<br>Final: ",
                                          scales::comma(votes_max), " (", scales::percent((votes_max / votes_max_pool), accuracy = 0.1), ")")))) %>% 
    pivot_longer(cols = c(votes_min, votes_max, votes_redist), names_to = "vote_rnd", values_to = "votes") %>% 
    mutate(hov_text = str_c(readable_nm(candidate), ", ", coalesce(party_std, "NA"), "<br>", hov_text),
           votes = as.integer(votes))
}

first_final_votes_with_distn <- function(p_distn = distn, p_year =  "2017") {
  # Summarise candidates first and final votes with access to richer distribution data
  p_distn %>%
    filter(year == p_year) %>% 
    group_by(year, candidate) %>% 
    summarise(votes_min = min(votes, na.rm = TRUE),
              votes_max = max(votes, na.rm = TRUE),
              rounds = n()) %>% 
    group_by(year) %>% 
    mutate(cand_rank = min_rank(-votes_max),
           rounds_rank = min_rank(-rounds),
           rounds_rank_1 = sum(rounds_rank == 1),
           votes_min_sum = sum(votes_min, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(cands_std %>% select(-party_colour), by = c("candidate", "year")) %>% 
    left_join(two_pp_sum, by = c("year", "candidate")) %>%
    mutate(votes_max = if_else((rounds_rank_1 > 2 & cand_rank <= 2), coalesce(votes, votes_max), votes_max), # accomodate scenarios where distn data stops before last 2 candidates
           votes_max_pool = sum(if_else(cand_rank <= 2, votes_max, 0L), na.rm = TRUE),
           votes_redist = votes_max - votes_min,
           hov_text = str_c("1st pref: ", scales::comma(votes_min), " (", scales::percent((votes_min / votes_min_sum), accuracy = 0.1), ")",
                            ". Pref distn: ", if_else(is.na(votes_max), "NA", 
                                    str_c(scales::comma(votes_redist), ".<br>Final: ", 
                                          scales::comma(votes_max), " (", scales::percent((votes_max / votes_max_pool), accuracy = 0.1), ")")))) %>% 
    select(-votes) %>%
    pivot_longer(cols = c(votes_min, votes_max, votes_redist), names_to = "vote_rnd", values_to = "votes") %>% 
    mutate(hov_text = str_c(readable_nm(candidate), ", ", party_std, "<br>", hov_text))
}

informal_votes <- function(p_year = "2002") {
  # Extract the informal votes from the first_pref data
  first_pref %>% 
    filter(year == p_year, candidate == "informal") %>% 
    group_by(year, candidate) %>% 
    summarise(votes = sum(votes, na.rm = TRUE),
              cand_party = NA_character_,
              party_std = "white",
              hov_text = str_c("Informal votes: ", scales::comma(votes))) %>% 
    ungroup() %>% 
    mutate(candidate = str_to_title(candidate),
           vote_rnd = "votes_min")
}

plot_votes_by_cand <- function(p_year = "2017", p_distn = distn){
  # Plots bar chart of 1st pref votes and final preference votes
  
  # p_year = "2002"
  
  
  if (p_year >= "2010") {
    # Calculate the final votes after distributions and the first preferences
    dist_trans <-
      first_final_votes_with_distn(p_distn, p_year)
    
  } else {
    # Prior to 2010, can only provide final votes for top 2 candidates and first pref for everyone
    # Note that in 2002, the Greens candidate had the 2nd highest number of votes but 2pp went to Liberals
    dist_trans <-
      first_final_votes_no_distn(p_year)
    
  }
  
  votes_win <-
    # Work out the number of votes required to win
    dist_trans %>% 
    mutate(cand_place = row_number(if_else(vote_rnd == "votes_max", -votes, 0L))) %>% 
      # rank the candidates
    mutate(votes_win = (sum(if_else((cand_place <= 2 & vote_rnd == "votes_max"), votes, 0L), na.rm = TRUE) / 2) %>% ceiling()) %>% 
      # calculate the number of votes in the final distribution
    filter(vote_rnd == "votes_min", !is.na(votes), !str_detect(str_to_lower(cand_party), "(didnt vote)|(informal)")) %>% 
    slice(which.min(votes)) %>% 
    mutate(votes_win_txt = str_c("winning line\n", scales::comma(votes_win))) 
  
  dist_trans <-
    # Rebuild the transformed distribution data, starting with didn't vote cohort
    merge_tot_votes_enrolled() %>% 
    filter(year == p_year) %>% 
    mutate(candidate = "Didnt vote",
           cand_party = "Didnt vote",
           vote_rnd = "votes_min") %>%
    select(year, candidate, cand_party, party_std, hov_text, vote_rnd, votes) %>% 
    # Add the informal votes
      bind_rows(informal_votes(p_year)) %>% 
    # Add back the candidate votes
    bind_rows(dist_trans) %>% 
    # Set the candidate order
    mutate(candidate = candidate %>% fct_reorder(., votes, sum, na.rm = TRUE) %>% 
             fct_relevel("Didnt vote", "Informal"))

  cand_elim_margin <-
    distn %>% 
    filter(year == p_year) %>% 
    group_by(year, round) %>% 
    arrange(round, votes) %>% 
    mutate(rev_rank = min_rank(votes),
           votes_next = lead(votes),
           votes_behind = votes_next - votes,
           hov_text = if_else(votes_behind < 1000, str_c(votes_behind, " behind"),
                              str_c("Eliminated in round ", round, " when ", 
                                    votes_behind, " behind next-placed ", readable_nm(lead(candidate)))),
           cands_rem = n()) %>% 
    ungroup() %>% 
    # filter(round >= 9) %>% 
    filter(rev_rank == 1L & cands_rem > 2) # Don't count second place getter
  
  votes_by_cand_ggp <- 
    dist_trans %>%  
    filter(vote_rnd != "votes_max") %>% 

    ggplot(aes(x = candidate, y = votes, text = hov_text, fill = party_std)) +
    geom_hline(yintercept = votes_win$votes_win[1], linetype = "dashed", colour = "grey") +
    geom_bar(stat = "identity", position = "stack", size = .2, na.rm = TRUE, colour = "grey",
             aes(alpha = if_else(vote_rnd == "votes_min", 1, .8) %>% factor())) +
    geom_text(data = votes_win, aes(x = candidate, y = votes_win, label = votes_win_txt, hjust = "left"), 
              inherit.aes = FALSE, size = 3.5, colour = "black", alpha = 0.5) +
    
    labs(title = str_c("First preference and final votes in ", p_year), x = "") +
    scale_fill_manual("Party", values = party_colours()) +
    scale_colour_manual("Party", values = party_colours()) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "none") +
    coord_flip()
  
  if (nrow(cand_elim_margin) > 0) {
    votes_by_cand_ggp <-
      votes_by_cand_ggp +
      geom_point(data = cand_elim_margin, aes(y = votes_next, fill = NA), colour = "grey") +
      geom_segment(data = cand_elim_margin, aes(yend = votes_next, y = votes, fill = NA, xend = candidate),
                   colour = "grey", na.rm = TRUE, linetype = "dashed`")
  }
  
  ggplotly(votes_by_cand_ggp, tooltip = "text")
  # votes_by_cand_ggp

}

cre_distn_gap <- function(p_two_pp_sum = two_pp_sum, p_distn = distn, p_pref = pref) {
  # Data frame showing the gap between the final votes and votes after distribution
  
  distn_gap_last2 <-
    # shows the gap between the distribution data and the 2 candidate preferential data
    two_pp_sum %>% 
    inner_join(distn %>% group_by(year, candidate) %>% summarise(votes = max(votes)), by = c("year", "candidate"), suffix = c("", ".distn")) %>% 
    mutate(votes_last_rnd = votes - votes.distn ) 
  
  p_distn %>% 
    anti_join(p_pref, by = c("year", "candidate" = "from_cand")) %>% 
    group_by(year, candidate) %>%
    summarise(votes = max(votes)) %>%
    ungroup() %>% 
    inner_join(distn_gap_last2, by = "year", suffix = c(".from", ".to")) %>%
    anti_join(distn_gap_last2, by = c("candidate.from" = "candidate")) %>%
    select(year, from_cand = candidate.from, to_cand = candidate.to, votes = votes_last_rnd) %>% 
    group_by(year, from_cand) %>% 
    mutate(pref_rank = NA_integer_,
           votes_sh = votes / sum(votes),
           votes_sum = sum(votes)) %>% 
    ungroup()

}

plot_votes_top3_ts <- function(p_distn = distn) {
  # Plot timeseries of votes for the top 3 parties in each election
  # Useful for assessing which group of supporters failed to vote in 2017
  dist_w_3rem_ggp <-
    p_distn %>% 
    group_by(year, round) %>% 
    mutate(cands_rem = n()) %>% 
    ungroup() %>% 
    filter(cands_rem == 3) %>% 
    left_join(cands_std %>% select(-party_colour), by = c("candidate", "year")) %>% 
    inner_join(elec_dates, by = c("year" = "elec_ID")) %>% 
    select(date, year, votes, party_std) %>% 
    bind_rows(merge_tot_votes_enrolled() %>% select(date, year, votes, party_std)) %>% 
    # Add didn't vote numbers
    
    filter(year >= "2010") %>% # Full distribution data only available from 2010 election
    
    ggplot(aes(x = date, y = votes, group = party_std, colour = party_std)) +
    geom_point(size = 1) +
    geom_line(size = 0.25, na.rm = TRUE, linetype = "solid") +
    scale_colour_manual("Party", values = party_colours()) +
    labs(title = "Allocated distribution with 3 candidates remaining", x = "") +
    expand_limits(y = 0) +
    
    scale_x_date(NULL, breaks = elec_dates$date, date_labels = "%b<br>-%y") +
    scale_y_continuous(breaks = seq(0, 20000, 2000), labels = scales::comma)
  
  ggplotly(dist_w_3rem_ggp)
  # dist_w_3rem_ggp
  
}

plot_distn_sankey <- function(p_year = "2010", p_distn = distn ) {
  # Plot Sankey diagram of distributions for specified year
  
  cand_str_cleanup <- c("\\s+" = "_", "," = "", "[:punct:]" = "_", "_+" = "_", "_+$" = "")
  
  cand_rank <-
    p_distn %>% 
    filter(year == p_year) %>% 
    group_by(year, candidate) %>%
    summarise(votes_max = max(votes),
              votes_min = min(votes)) %>% 
    mutate(cand_rank = row_number(-votes_max) - 1) %>% 
    ungroup() %>% 
    left_join(cands_std, by = c("year", "candidate")) %>% 
    arrange(cand_rank) %>% 
    mutate_at(c("candidate", "party_std"), ~str_replace_all(., cand_str_cleanup)) %>% 
    mutate(node_grey = "grey") 
  
  my_color <-
    str_c('d3.scaleOrdinal() .domain(["',
          str_c(c(cand_rank$candidate, "grey"), collapse = '", "'),
          '"]) .range(["',
          str_c(c(cand_rank$party_colour, "grey"), collapse = '", "'),
          '"])', sep = "") 
  
  vote_distn <-
    pref_w_party %>% 
    filter(year == p_year) %>% 
    mutate_at(c("from_cand", "to_cand"), ~str_replace_all(., cand_str_cleanup)) %>% 
    mutate_at(c("from_cand", "to_cand"), list(id = ~match(., cand_rank$candidate) - 1)) %>% 
    select(year, from_cand, from_cand_id, to_cand, to_cand_id, votes, everything()) 
  
  sankeyNetwork(Links = vote_distn %>% as.data.frame(), Nodes = cand_rank %>% as.data.frame(),
                Source = "from_cand_id", Target = "to_cand_id",
                Value = "votes", NodeID = "candidate",
                colourScale = my_color, LinkGroup = "to_cand", NodeGroup = "node_grey",
                fontSize= 8, nodeWidth = 15,
                fontFamily = "Helvetica", sinksRight = FALSE)
  
}

expand_two_pp_sum <- function(p_year = "2018") {
  # Elaborate on the 2pp summary
  
  two_pp_invalid_str <- "(Informal|sorts)"
  
  two_pp_sum %>% 
    filter(year == p_year &
             !str_detect(candidate, two_pp_invalid_str)) %>% 
    group_by(year) %>% 
    arrange(-votes) %>% 
    mutate(vote_sh = votes / sum(votes, na.rm = TRUE),
           vote_rank = min_rank(-votes),
           vote_margin = votes - lead(votes)) %>% 
    left_join(cands_std, by = c("year", "candidate")) %>% 
    mutate(cand_read = readable_nm(candidate))
}


valbox_win_sh <- function(p_year = "2018") {
  # Create valuebox with winners share
  
  win_sh_lst <-
    expand_two_pp_sum(p_year)[1,] %>% 
    flatten()
  
  valueBox(win_sh_lst$vote_sh %>% scales::percent(accuracy = .1),
           str_c("2pp to ", win_sh_lst$cand_read, "\n", win_sh_lst$party_std), 
           icon = icon("trophy"),
           color = "light-blue")

}

valbox_win_marg <- function(p_year = "2018") {
  # Create valuebox with winners share
  
  win_sh_lst <-
    expand_two_pp_sum(p_year)[1,] %>% 
    flatten()
  
  valueBox(win_sh_lst$vote_margin %>% scales::comma(),
           "margin over 2nd place", 
           icon = icon("ruler-horizontal"),
           color = "light-blue")
  
}

valbox_win_from_3rd <- 
  function(p_year = "2018") {
    # Create value box with the distribution of preferences from the 3rd placegetter to first 2 placegetters
    
    pref_3rd <-
      pref_w_party %>% 
      filter(year == p_year) %>% 
      group_by(year, from_cand) %>% 
      mutate(rem_cands = n()) %>% 
      ungroup() %>% 
      filter(rem_cands == 2) %>% 
      inner_join(expand_two_pp_sum(p_year), by = c("year", "to_cand" = "candidate"), suffix = c(".pref", ".all")) %>% 
      arrange(-votes.all)
    
    if (nrow(pref_3rd) >= 2) {
      valueBox(pref_3rd[1,] %>% pull("votes.pref") %>% scales::comma(),
               str_c("of ", pref_3rd[1,] %>% pull("votes_sum") %>% scales::comma(),
                     " preferences distribution from 3rd placed candidate to ", 
                     readable_nm(pref_3rd[1, "to_cand"])),
               icon = icon("code-branch"),
               color = "aqua")      
    } else {
      valueBox("",
               str_c("Preference distribution not published for ", p_year),
               icon = icon("question"),
               color = "yellow")
    }
  }

valbox_didnt_vote <- function(p_year = "2018") {
  # Generates valuebox for number of enrolled voters who didn't vote
  didnt_vote <-
    merge_tot_votes_enrolled() %>% 
    filter(year == p_year) %>% 
    flatten()
  
  valueBox(didnt_vote$votes %>% scales::comma(),
           "enrolled voters who didn't vote",
           icon = icon("kiss"),
           color = "maroon")
}

valbox_2pp_last_total <- function() {
  # Latest 2pp total to ALP
  two_pp_alp <-
    two_pp_all_booth %>% 
    filter(party_std == "Australian Labor Party") %>% 
    arrange(date %>% desc())
  
  valueBox(two_pp_alp$votes[1] %>% scales::comma(),
        str_c("votes to ALP in ", year(two_pp_alp$date[1]), "\n"),
        color = "red",
        icon = icon("signal"))
}

valbox_2pp_last_sh <- function() {
  # Latest 2pp total to ALP
  two_pp_alp <-
    two_pp_all_booth %>% 
    filter(party_std == "Australian Labor Party") %>% 
    arrange(date %>% desc())
  
  valueBox(two_pp_alp$votes_sh[1] %>% scales::percent(accuracy = 0.1), 
        str_c("2pp share to ALP in ", year(two_pp_alp$date[1]), "\n"),
        color = "aqua",
        icon = icon("balance-scale"))
}

valbox_2pp_max_sh <- function() {
  # Latest 2pp total to ALP
  two_pp_alp <-
    two_pp_all_booth %>% 
    filter(party_std == "Australian Labor Party") %>% 
    arrange(votes_sh %>% desc())
  
  valueBox(two_pp_alp$votes_sh[1] %>% scales::percent(accuracy = 0.1), 
           str_c("maximum 2pp share to ALP, in ", year(two_pp_alp$date[1])),
           color = "aqua",
           icon = icon("balance-scale"))
}

valbox_2pp_max_total <- function() {
  # Latest 2pp total to ALP
  two_pp_alp <-
    two_pp_all_booth %>% 
    filter(party_std == "Australian Labor Party") %>% 
    arrange(votes %>% desc())
  
  valueBox(two_pp_alp$votes[1] %>% scales::comma(),
           str_c("maximum 2pp votes to ALP in ", year(two_pp_alp$date[1])),
           color = "red",
           icon = icon("signal"))
}

valbox_fpref_last_votes <- 
  function(p_votes_rnk = 1L) {
    # Value box for the number of first preference votes to the leading party in last election
    pref1_votes_party <-
      party_votes_by_elec %>% 
      arrange(date %>% desc, -votes) %>% 
      mutate(year = year(date) %>% as.character()) %>% 
      dplyr::slice(p_votes_rnk)
    
    valueBox(pref1_votes_party$votes[1] %>% scales::comma(),
          str_c("first pref votes to ", pref1_votes_party$party_std[1], " in ", pref1_votes_party$year[1]),
          color = if_else(p_votes_rnk == 1, "red", "yellow"),
          icon = if_else(p_votes_rnk == 1, "trophy", "arrow-right") %>% icon(),
          width = 2)
  }

valbox_local_rem <- function(p_year = "2018", p_stat_typ = "Local") {
  # Print valuebox with the voter share between polling station types
  
  poll_stn_summ <-
    votes_by_booth_all %>% 
    filter(year == p_year) %>% 
    filter(booth != "All") %>% 
    mutate(phys_stn = match(booth, booth_addr_lst$booth),
           phys_stn = if_else(is.na(phys_stn), "Remote", "Local")) %>% 
    group_by(phys_stn, year) %>% 
    summarise(votes_sum = sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(votes_sh = votes_sum / sum(votes_sum)) %>% 
    filter(phys_stn == p_stat_typ)
  
  valueBox(value = poll_stn_summ$votes_sh[1] %>% scales::percent(accuracy = 0.1),
           str_c("or ", scales::comma(poll_stn_summ$votes_sum[1]), " voters attending a ",
              str_to_lower(p_stat_typ), " polling station"),
           color = "green",
           icon = if_else(p_stat_typ == "Local", "map-marker-alt", "compass") %>% icon(),
           width = 2)
  
}

valbox_poll_stn_votes <- function(p_booth = "Northcote South", p_max_last = c("last", "max"), 
                                  p_dim = c("votes_sum", "votes_sh_elec", "votes_2pp_sh"),
                                  p_color = "cyan") {
  # Generate valuebox with specified dimensions
  
  votes_booth_sum <-
    votes_by_booth_all %>% 
    filter(booth != "All") %>% 
    group_by(date) %>% 
    mutate(votes_sh = votes / sum(votes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(booth == p_booth) %>% 
    select(booth, year, votes_sum = votes, party_std, votes_sh_elec = votes_sh)
  
  votes_booth_2pp <-
    two_pp_by_booth_nondom %>% 
    filter(votes > 0, booth == p_booth) %>% 
    mutate(year = format(date, "%Y")) %>% 
    select(booth, year, votes_2pp_sum = votes, party_std, votes_2pp_sh = votes_sh)
  
  sort_dim <- "year"
  sort_dim <- if (p_max_last[1] != "last") {p_dim[1]} else {"year"}
  
  votes_booth_sum <-
    votes_booth_sum %>% 
    full_join(votes_booth_2pp, by = c("booth", "year", "party_std")) %>% 
    arrange(.data[[sort_dim]] %>% desc())
  
  result <-
    if (p_dim[1] %in% c("votes_2pp_sh", "votes_sh_elec")) {
      votes_booth_sum %>% 
        pull(.data[[p_dim[1]]]) %>% 
        head(1) %>% 
        scales::percent(accuracy = 0.1)
    } else {
      votes_booth_sum %>% 
        pull(.data[[p_dim[1]]]) %>% 
        head(1) %>% 
        scales::comma()
    }
  
  measure <-
    case_when(
      p_dim[1] == "votes_sum" ~ "Total votes lodged at ",
      p_dim[1] == "votes_sh_elec" ~ "Share of electorate who voted at ",
      p_dim[1] == "votes_2pp_sh" ~ str_c("Two-party-preferred for ", votes_booth_sum$party_std[1], " at "),
      TRUE ~ "NA"
    )
  
  valueBox(result,
        str_c(measure, p_booth, " polling station in ", votes_booth_sum$year[1]),
        color = p_color,
        # icon = icon()
        width = 2)
  
}

conv_html_to_table <- function(.html_raw, .tbl_num) {
  # Convert html text to a table
  
  tbl_raw <-
    .html_raw %>% 
    html_nodes("table") %>% 
    purrr::pluck(.tbl_num) %>% 
    html_table(fill = TRUE)
  
  tbl_raw_nms <-
    tbl_raw %>% 
    names() %>% 
    keep(~!is.na(.))
  
  tbl_raw <-
    tbl_raw %>% 
    fix_df(unique = TRUE) %>% 
    select_at(vars(-starts_with("NA.")))
  
  tbl_head <-
    tbl_raw %>% 
    slice(1) %>% 
    unlist() %>% 
    enframe("head_raw", "row1_raw") %>% 
    mutate(raw_nms = tbl_raw_nms,
           head_final = if_else(str_detect(head_raw, "^X\\.*\\d*"), row1_raw, raw_nms),
           party = if_else(str_detect(head_raw, "^X\\.*\\d*"), NA_character_, 
                           if_else(row1_raw == "", "INDEP", row1_raw)))
  
  head_ren <-
    tbl_head %>% 
    select(head_final, head_raw) %>% 
    deframe()
  
  tbl_raw %>% 
    rename(!!!head_ren) %>% 
    tail(-1) %>% 
    head(-1) %>% 
    map_df(parse_guess) %>% 
    filter_all(all_vars(!is.na(.))) 
  
  # This is the tidied table. Can also return the table details
  
}

