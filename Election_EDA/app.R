#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

source(".././code/functions.R")

# Add line to source key functions


# Add line to retrieve key data


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Northcote Election Results - Explore the data!"),
   
   # Set of tabs with reporting options
   tabsetPanel(
      # Northcote map with year selector
      # Bar chart of polling stations for a single year
      # Two party preferred share by booth
      # Two party preferred share
      # Two party preferred votes
      # Total votes by booth
      # Total first preference votes by party
      # Votes distribution by party
      # Votes distribution by candidate
      
      tabPanel( "Booth map",
         # Northcote map with year selector
         
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
            sidebarPanel(
               selectInput("map_year",
                           "Select year to display on map:",
                           choices = elec_dates$elec_ID)
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
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                   sidebarPanel(
                      selectInput("plot_booth_votes_bar_year",
                                  "Select year to display for chart:",
                                  choices = elec_dates$elec_ID)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      h2(textOutput("plot_booth_votes_bar_heading")),
                      p(),
                      plotlyOutput("plot_booth_votes_bar_plot", height = 700)
                   )
                )
      ),
      
      tabPanel( "2 Party Pref by Booth",
                # Two party preferred share by booth
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                   sidebarPanel(
                      selectInput("two_pp_by_booth_nondom_plot_booth",
                                  "Select booth to highlight:",
                                  choices = two_pp$booth %>% unique())
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      plotlyOutput("two_pp_by_booth_nondom_plot", height = 700)
                   )
                )
      ),
      
      tabPanel( "2 Party Pref Share"
                # Two party preferred share
                
                # Sidebar with a slider input for number of bins 
                # sidebarLayout(
                #    sidebarPanel(
                #       # selectInput("two_pp_by_booth_nondom_plot_booth",
                #       #             "Select booth to highlight:",
                #       #             choices = two_pp$booth %>% unique())
                #    ),
                #    
                # splitLayout(
                #       plotlyOutput("two_pp_all_booth_plot", height = 700),
                #       plotlyOutput("two_pp_vote_ts_plot", height = 700)
                #    )
                
      ),
      
      tabPanel( "2 Party Pref Votes",
                # Two party preferred share
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                   sidebarPanel(
                      # selectInput("two_pp_by_booth_nondom_plot_booth",
                      #             "Select booth to highlight:",
                      #             choices = two_pp$booth %>% unique())
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                      plotlyOutput("two_pp_vote_ts_plot", height = 700)
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
      str_c("Plot of Polling Stations in ", input$map_year)
   })
   
   output$plot_booth_votes_bar_plot <- renderPlotly({
      plot_booth_votes_bar(input$plot_booth_votes_bar_year, votes_by_booth_all)
   })
   
   output$plot_booth_votes_bar_heading <- renderText({
      str_c("Relative sizes of polling stations in ", input$plot_booth_votes_bar_year)
   })
   
   output$two_pp_by_booth_nondom_plot <- renderPlotly({
      plot_two_pp_by_booth_nondom(p_booth = input$two_pp_by_booth_nondom_plot_booth)
   })
   
   output$two_pp_all_booth_plot <- renderPlotly({
      plot_two_pp_all_booth()
   })

   output$two_pp_vote_ts_plot <- renderPlotly({
      plot_2pp_vote_ts(two_pp_all_booth)
   })
   
   }

# Run the application 
shinyApp(ui = ui, server = server)

