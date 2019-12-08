#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(gridExtra)
library(leaflet)

source("./functions.R")

# data_dir <- ".././data"
# getwd()

# Add line to retrieve key data

retrieve_dfs(transf_df, "./data")


# Define UI for application that draws a histogram
ui <- fluidPage(
   # theme = "bootstrap.css",
   # Application title
   titlePanel("Northcote Election Results - Explore the data!"),
   
   # Set of tabs with reporting options
   navlistPanel(
      widths = c(2, 10),
      # Northcote map with year selector
      # Bar chart of polling stations for a single year
      # Two party preferred share by booth
      # Two party preferred share
      # Two party preferred votes
      # Total votes by booth
      # Total first preference votes by party
      # Votes distribution by party
      # Votes distribution by candidate
      
      tabPanel( "First and Final Votes",
                # Total first and final votes for given year
                
                sidebarLayout(
                   sidebarPanel(
                      selectInput("first_final_votes_sel_year",
                                  "Select year to display for chart:",
                                  choices = elec_dates$elec_ID %>% keep(~. >= "2010") %>% sort(decreasing = TRUE)),
                      width = 2
                   ),
                   
                   mainPanel(
                      plotlyOutput("votes_by_cand_plot", height = 700)
                   )
                )
      ),
      
      tabPanel( "First Pref Votes",
                # Total first preference votes by party
                
                fluidRow(
                   plotlyOutput("party_votes_by_elec_plot", height = 700)
                )
      ),
      
      tabPanel( "2 Party Preferred Share",
                # Two party preferred share
                
                fluidRow(
                   # h2(textOutput("plot_booth_votes_bar_heading")),  # Something wacky about these plots
                   plotlyOutput("two_pp_all_booth_plot", height = 700)
                   # h2("Hello world")
                )
                
      ),
      
      tabPanel( "2 Party Preferred Votes",
                # Two party preferred votes
                
                fluidRow(
                   plotlyOutput("two_pp_vote_ts_plot", height = 700)
                )
      ),
      
      tabPanel( "Votes distribution FROM party",
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
      
      tabPanel( "Votes distribution TO party",
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
      
      tabPanel( "Votes distribution FROM candidate",
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
      
      tabPanel( "Polling station map",
         # Northcote map with year selector
         
         sidebarLayout(
            sidebarPanel(
               selectInput("map_year",
                           "Select year to display on map:",
                           choices = elec_dates$elec_ID),
               width = 2
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
               h2(textOutput("poll_map_heading")),
               p(),
               leafletOutput("booth_map", height = 700)
            )
         )
      ),
      
      tabPanel( "Polling station sizes",
                # Bar chart of polling stations for a single year
                
                sidebarLayout(
                   sidebarPanel(
                      selectInput("plot_booth_votes_bar_year",
                                  "Select year to display for chart:",
                                  choices = elec_dates$elec_ID),
                      width = 2
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      h2(textOutput("plot_booth_votes_bar_heading")),
                      p(),
                      plotlyOutput("plot_booth_votes_bar_plot", height = 700)
                   )
                )
      ),
      
      tabPanel( "2 Party Pref by Polling Station",
                # Two party preferred share by booth
                
                sidebarLayout(
                   sidebarPanel(
                      selectInput("two_pp_by_booth_nondom_plot_booth",
                                  "Select polling station to highlight:",
                                  choices = two_pp_by_booth_nondom %>% filter(votes > 0) %>%
                                     pull(booth) %>% unique() %>% sort()),
                      width = 2
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      plotlyOutput("two_pp_by_booth_nondom_plot", height = 700)
                   )
                )
      ),
      
      tabPanel( "Total votes by polling station",
                # Total votes by booth
                
                sidebarLayout(
                   sidebarPanel(
                      selectInput("votes_by_booth_sel_booth",
                                  "Select polling station to highlight on chart:",
                                  choices = two_pp$booth %>% unique() %>% sort()),
                      width = 2
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      # h2(textOutput("plot_booth_votes_bar_heading")),
                      # p(),
                      plotlyOutput("votes_by_booth_all_plot", height = 700)
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
   
   output$plot_booth_votes_bar_heading <- renderText({
      str_c("Number of voters at each polling station in ", input$plot_booth_votes_bar_year)
   })
   
   output$two_pp_by_booth_nondom_plot <- renderPlotly({
      plot_two_pp_by_booth_nondom(p_booth = input$two_pp_by_booth_nondom_plot_booth)
   })
   
   output$two_pp_all_booth_plot <- renderPlotly({
      plot_two_pp_all_booth()
      # plot_2pp_vote_ts(two_pp_all_booth)
   })

   output$two_pp_vote_ts_plot <- renderPlotly({
      plot_2pp_vote_ts(two_pp_all_booth)
   })
   
   output$votes_by_booth_all_plot <- renderPlotly({
      plot_votes_by_booth_all(votes_by_booth_all, input$votes_by_booth_sel_booth) 
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
   
   }

# Run the application 
shinyApp(ui = ui, server = server)

