
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
