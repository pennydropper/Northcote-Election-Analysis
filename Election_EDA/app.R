#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(gridExtra)
library(leaflet)
library(networkD3)

source("./functions.R")

# data_dir <- ".././data"
# getwd()

# Add line to retrieve key data

retrieve_dfs(transf_df, "./data")

ui <-
   dashboardPage(
      dashboardHeader(title = "Northcote Elections' Results"),
      
      dashboardSidebar(
         sidebarMenu(
            menuItem("Home", tabName = "home"),
            menuItem("First and Final Votes", tabName = "first_and_final_votes"),
            menuItem("First Pref Votes", tabName = "first_pref_votes"),
            menuItem("Two Party Preferred", tabName = "two_pp_share"),
            # menuItem("2 Party Preferred Votes", tabName = "two_pp_votes"),
            menuItem("Votes distribution FROM party", tabName = "votes_distn_from_party"),
            menuItem("Votes distribution TO party", tabName = "votes_distn_to_party"),
            # menuItem("Votes distribution FROM candidate", tabName = "votes_distn_from_cand"),
            menuItem("Polling stations", tabName = "poll_map"),
            # menuItem("Polling station sizes", tabName = "poll_station_sizes"),
            menuItem("Votes by polling station", tabName = "two_pp_by_poll_stn")
            # menuItem("Total votes by polling station", tabName = "votes_by_poll_stn")
         )
      ),
      
      dashboardBody(
         # theme = "bootstrap.css",
         
         # Set of tabs with reporting options
         
         tabItems(
            tabItem( "home",
                     # Home page
                     
                     fluidPage(
                        column(
                           width = 1
                        ),
                        column(
                           width = 10,
                           h2("Northcote Election Results"),
                           
                           h3("Home Page"),
                           p("Having lived in Northcote for nearly 30 years and having voted in 8 state elections (including a by-election) 
                             and 9 federal elections, it occurred to me that there were many unanswered questions about my electorate 
                             and neighbourhood."),

                           h4("Background"),
                           p("My neighbourhood has a strong working class history.  The Australian Labor Party (ALP) has held the 
                             Federal Division of Cooper (formerly Batman) for all but 10 years since Federation in 1901.  
                             Similarly, the ALP has held the State District of Northcote for all but 1 year since its creation in 1927.  
                             Federal representatives include an ALP Deputy Prime Minister and State representatives include an ALP Premier."),
                           
                           h4("Trending from ALP to Greens "),
                           p("The demographics of Northcote has swung from the traditional blue collar ALP-supporter to the more 
                             progressive, typically educated Greens' supporter.  The trend was sufficiently strong for the 
                             Australian Greens to snatch the District of Northcote away from the ALP in a by-election in 2017, 
                             though the ALP regained the seat in the statewide election in 2018, despite the Australian Greens 
                             increasing their first-preference votes from the 2017 by-election. "),
                           
                           h4("Curiosity-driven Analysis "),
                           p("This analysis started with some basic questions, which, when answered, led to more questions:"),
                           tags$li("Which polling stations in Northcote most supported the Australian Greens?"),
                           tags$li("Where are the different polling stations?"),
                           tags$li("How many people tend to vote at the different stations?"),
                           tags$li("How many enrolled voters failed to vote in the 2017 by-election?"),
                           tags$li("How have Liberal voters divided their preferences between the ALP and the Greens over time?
                              The Liberal party chose not to field a candidate in the 2017 by-election, which the 
                              media assumed was an attempt to make mischief for the governing ALP."),
                           
                           h4("Data sources"),
                           tags$li("Voting data was sourced from the ", a("Victorian Electoral Commission (VEC) website", 
                                                                   href="https://www.vec.vic.gov.au/Results/results-state.html", target="_blank")
                           ),
                           tags$li("Polling station locations was sourced from my own local knowledge, 
                           VEC advertisements in the back-issues of The Age and with some help from a friend."),
                           tags$li("The electoral boundaries map shows the current (2018) boundaries, as per ",
                           a("Spatial Datamart Victoria", href = "http://services.land.vic.gov.au/SpatialDatamart/index.jsp", target = "_blank"),
                           ".  There have been some minor adjustments since the 1999 election along the northern border."),
                           
                           h4("Feedback"),
                           p("Any feedback will be much appreciated:", a("pennydropper@gmail.com", target="_blank")),
                           p("James Northrop")
                        )
                     )
                     
            ),
            
            tabItem( "first_and_final_votes",
                     # Total first and final votes for given year
                     
                     
                     fluidRow(
                        box(
                           selectInput("first_final_votes_sel_year",
                                       "Select election:",
                                       choices = elec_dates$elec_ID %>% sort(decreasing = TRUE)),
                           width = 2
                        ),
                        
                        valueBoxOutput("valbox_win_sh_out", width = 2),
                        valueBoxOutput("valbox_win_marg_out", width = 2),
                        valueBoxOutput("valbox_win_from_3rd_out", width = 2),
                        valueBoxOutput("valbox_didnt_vote_out", width = 2)

                     ),
                     
                        fluidRow(
                           box(
                              plotlyOutput("votes_by_cand_plot"),
                              p(),
                              p("Two-party preferred and distributed preference votes data only available for ALP and Liberal candidates prior to 2006 election.  
                        From the 2006 election, distributed preference votes available for the 1st and 2nd placegetters.  
                        From the 2010 election, distributed preference votes available for all candidates.")
                           ),
                           box(
                              p(strong("Flow of preferences of minor candidates to top 2")),
                              sankeyNetworkOutput("votes_distn_sankey"),
                              p("Use mouseover for more information on number of candidates. Click and drag grey nodes if congested.")
                           )
                        )
                     
            ),
            
            tabItem( "first_pref_votes",
                     # Total first preference votes by party
                     fluidRow(
                        valbox_fpref_last_votes(1L),
                        valbox_fpref_last_votes(2L)
                     ),
                     
                     fluidRow(
                        plotlyOutput("party_votes_by_elec_plot")
                     )
            ),
            
            tabItem( "two_pp_share",
                     # Two party preferred
                     
                     fluidRow(
                        valueBoxOutput("valbox_2pp_last_total_out", width = 2),
                        valueBoxOutput("valbox_2pp_max_total_out", width = 2),
                        valueBoxOutput("valbox_2pp_last_sh_out", width = 2),
                        valueBoxOutput("valbox_2pp_max_sh_out", width = 2)
                     ),
                     
                     fluidRow(
                        # h2(textOutput("plot_booth_votes_bar_heading")),  # Something wacky about these plots
                        box(
                           plotlyOutput("two_pp_all_booth_plot"),
                           width = 12
                        )
                        
                     ),
                     fluidRow(
                        box(
                           p("Two-party preferred and distributed preference votes data only available for ALP and Liberal candidates prior to 2006 election.  
                        From the 2006 election, distributed preference votes available for the 1st and 2nd placegetters."),
                           width = 12
                           
                        )
                        
                     )
                     
            ),
            
            tabItem( "two_pp_votes",
                     # Two party preferred votes
                     
                     fluidRow(
                        plotlyOutput("two_pp_vote_ts_plot", height = 700),
                        p(),
                        p("Two-party preferred and distributed preference votes data only available for ALP and Liberal candidates prior to 2006 election.  
                        From the 2006 election, distributed preference votes available for the 1st and 2nd placegetters.")
                     )
            ),
            
            tabItem( "votes_distn_from_party",
                     # Votes distribution by party
                     
                     sidebarLayout(
                        sidebarPanel(
                           selectInput("votes_distn_party_sel",
                                       "Select party to display for chart:",
                                       choices = pref_w_party$party_std.from %>% unique() %>% sort()),
                           width = 2
                        ),
                        
                        mainPanel(
                           plotlyOutput("distn_party_prefs_plot", height = "auto", width = "auto")
                        )
                     )
            ),
            
            tabItem( "votes_distn_to_party",
                     # Votes distribution to party
                     
                     sidebarLayout(
                        sidebarPanel(
                           checkboxGroupInput("votes_distn_party_to_sel",
                                              "Select party to display for chart:",
                                              choices = pref_w_party$party_std.to %>% unique() %>% sort(),
                                              selected = c("Australian Labor Party", "Australian Greens")
                           ),
                           width = 2
                           
                        ),
                        
                        mainPanel(
                           plotlyOutput("distn_party_prefs_to_plot", height = 1000, width = "auto")
                        )
                     )
            ),
            
            tabItem( "votes_distn_from_cand",
                     # Votes distribution by candidate
                     
                     sidebarLayout(
                        sidebarPanel(
                           selectInput("votes_distn_cand_sel",
                                       "Select candidate to display for chart:",
                                       choices = pref_w_party$from_cand %>% unique() %>% sort()),
                           width = 2
                        ),
                        
                        mainPanel(
                           plotlyOutput("pref_distn_sel_cand_plot", height = 700)
                        )
                     )
            ),
            
            tabItem( "poll_map",
                     # Northcote map with year selector
                     
                     fluidRow(
                        box(
                           selectInput("map_year",
                                       "Select year to display on map:",
                                       choices = elec_dates$elec_ID),
                           width = 2
                        ),
                        valueBoxOutput("valbox_local_rem_out", width = 2)
                     ),
                     
                     fluidRow(
                        h4(textOutput("poll_map_heading"),
                          style = "margin-left: 25px;")
                     ),
                     
                     fluidRow(
                        box(
                           leafletOutput("booth_map", height = 450)
                           
                        ),
                        box(
                           plotlyOutput("plot_booth_votes_bar_plot2", height = 450)
                        )
                     ),
                     
                     fluidRow(
                        h5("Two-party preferred and distributed preference votes data only available for ALP and Liberal candidates prior to 2006 election.  
                        From the 2006 election, distributed preference votes available for the 1st and 2nd placegetters.",
                           style = "margin-left: 25px;")
                     )
            ),
            
            tabItem( "two_pp_by_poll_stn",
                     # Two party preferred share by booth
                     
                     sidebarLayout(
                        sidebarPanel(
                           selectInput("two_pp_by_booth_nondom_plot_booth",
                                       "Select polling station to highlight:",
                                       choices = two_pp_by_booth_nondom %>% filter(votes > 0) %>%
                                          pull(booth) %>% unique() %>% sort(),
                                       selected = "Northcote South"),
                           width = 2
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                           width = 10,
                           fluidRow(
                              valueBoxOutput("valbox_poll_stn_votes_out", width = 2),
                              #valbox_poll_stn_elec_sh_last_out, valbox_poll_stn_2pp_last_out
                              valueBoxOutput("valbox_poll_stn_elec_sh_last_out", width = 2),
                              valueBoxOutput("valbox_poll_stn_2pp_last_out", width = 2)
                           ),
                           
                           fluidRow(
                              plotlyOutput("poll_stn_ts_out", height = 450)
                              
                           ),
                           
                           fluidRow(
                              h5("Two-party preferred and distributed preference votes data only available for ALP and Liberal candidates prior to 2006 election.  
                        From the 2006 election, distributed preference votes available for the 1st and 2nd placegetters.",
                                 style = "margin-left: 25px;")
                              
                           )
                        )
                     )
            )
         )
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$booth_map <- renderLeaflet({
      # Plot of electorate with booths (polling stations)
      
      print_booth_map(votes_by_phys_booth, input$map_year)
   })
   
   output$poll_map_heading <- renderText({
      str_c("Where people voted and how neighbourhoods voted in ", input$map_year)
   })
   
   output$plot_booth_votes_bar_plot <- renderPlotly({
      plot_booth_votes_bar(input$plot_booth_votes_bar_year, votes_by_booth_all)
   })
   
   output$plot_booth_votes_bar_plot2 <- renderPlotly({
      plot_booth_votes_bar(input$map_year, votes_by_booth_all)
   })
   
   output$plot_booth_votes_bar_heading <- renderText({
      str_c("Number of voters at each polling station in ", input$plot_booth_votes_bar_year)
   })
   
   output$two_pp_all_booth_plot <- renderPlotly({
      two_pp_tot <-
         plot_two_pp_all_booth()
      two_pp_sh <-
         plot_2pp_vote_ts()
      subplot(two_pp_tot, two_pp_sh)
   })

   output$two_pp_vote_ts_plot <- renderPlotly({
      plot_2pp_vote_ts()
   })
   
   output$two_pp_by_booth_nondom_plot <- renderPlotly({
      plot_two_pp_by_booth_nondom(p_booth = input$two_pp_by_booth_nondom_plot_booth)
   })
   
   output$votes_by_booth_all_plot <- renderPlotly({
      plot_votes_by_booth_all(votes_by_booth_all, input$two_pp_by_booth_nondom_plot_booth) 
   })
   
   output$poll_stn_ts_out <- renderPlotly({
      subplot(
         plot_two_pp_by_booth_nondom(p_booth = input$two_pp_by_booth_nondom_plot_booth),
         plot_votes_by_booth_all(votes_by_booth_all, input$two_pp_by_booth_nondom_plot_booth),
         titleY = TRUE
      )
   })
   
      output$party_votes_by_elec_plot <- renderPlotly({
      plot_party_votes_by_elec(party_votes_by_elec)
   })
   
   output$votes_by_cand_plot <- renderPlotly({
      plot_votes_by_cand(input$first_final_votes_sel_year, distn)
   })
   
   output$distn_party_prefs_plot <- renderPlotly({
      plot_distn_party_prefs(input$votes_distn_party_sel, pref_w_party, elec_dates)  
   })
   
   output$distn_party_prefs_to_plot <- renderPlotly({
      plot_distn_party_prefs_rec(input$votes_distn_party_to_sel, pref_w_party, elec_dates)  
   })
   
   output$pref_distn_sel_cand_plot <- renderPlotly({
      plot_pref_distn_sel_cand(input$votes_distn_cand_sel, pref_w_party)  
   })
   
   output$votes_distn_cand_val <- renderPrint({
      input$votes_distn_cand_sel
   })
   
   output$votes_distn_sankey <- renderSankeyNetwork({
      plot_distn_sankey(input$first_final_votes_sel_year, distn)
   })
   
   output$valbox_win_sh_out <- renderInfoBox({
      valbox_win_sh(input$first_final_votes_sel_year)
   })
   
   output$valbox_win_marg_out <- renderInfoBox({
      valbox_win_marg(input$first_final_votes_sel_year)
   })

   output$valbox_win_from_3rd_out <- renderInfoBox({
      valbox_win_from_3rd(input$first_final_votes_sel_year)
   })
      
   output$valbox_didnt_vote_out <- renderInfoBox({
      valbox_didnt_vote(input$first_final_votes_sel_year)
   })
   
   output$valbox_2pp_last_total_out <- renderInfoBox({
      valbox_2pp_last_total()
   })
   
   output$valbox_2pp_max_total_out <- renderInfoBox({
      valbox_2pp_max_total()
   })
   
   output$valbox_2pp_last_sh_out <- renderInfoBox({
      valbox_2pp_last_sh()
   })
   
   output$valbox_2pp_max_sh_out <- renderInfoBox({
      valbox_2pp_max_sh()
   })
   
   output$valbox_local_rem_out <- renderInfoBox({
      valbox_local_rem(input$map_year, "Local")
   })
   
   output$valbox_poll_stn_votes_out <- renderInfoBox({
      valbox_poll_stn_votes(input$two_pp_by_booth_nondom_plot_booth,
                            p_dim = "votes_sum",
                            p_max_last = "last",
                            p_color = "aqua")
   })
   
   output$valbox_poll_stn_2pp_last_out <- renderInfoBox({
      valbox_poll_stn_votes(input$two_pp_by_booth_nondom_plot_booth,
                            p_dim = "votes_2pp_sh",
                            p_max_last = "last",
                            p_color = "yellow")
   })
   
   output$valbox_poll_stn_elec_sh_last_out <- renderInfoBox({
      valbox_poll_stn_votes(input$two_pp_by_booth_nondom_plot_booth,
                            p_dim = "votes_sh_elec",
                            p_max_last = "last",
                            p_color = "aqua")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

