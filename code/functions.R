
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
