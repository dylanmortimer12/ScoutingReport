#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library (shiny)
require(baseballr)
library(tidyverse)
library(shinydashboard)

#player and team data
pitcher.data <- daily_pitcher_bref("2017-04-06","2017-10-15")

hitter.data <- daily_batter_bref("2018-06-01","2018-06-04")

# get team list
team.list <- c('Arizona','Boston','Atlanta','Baltimore','Chicago','Cincinnati','Cleveland','Colorado','Detroit','Houston','Kansas City','Los Angeles','Miami','Milwaukee','Minnesota','New York','Oakland','Philadelphia','Pittsburgh','San Diego','San Francisco','Seattle','St. Louis','Tampa Bay','Texas','Toronto','Washington')

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Choose Pitcher"),
      
      ######################## Pitcher Selection #############################
      
      # Choose League
      selectInput(inputId = 'p.league', label = 'Choose League', choices = c('MLB-AL','MLB-NL'), selected = NULL, multiple = FALSE),
      
      # Choose Team
      selectInput(inputId = 'p.team', label = 'Choose Team', choices = c('Pick a League'), selected = NULL, multiple = FALSE),
      
      # Choose Pitcher
      selectInput(inputId = 'pitcher', label = 'Choose Pitcher', choices = c('Pick a Team'), selected = NULL, multiple = FALSE),
      
      ######################## Hitter Selection ##########################
      titlePanel("Choose Hitter"),
      
      # Choose League
      selectInput(inputId = 'h.league', label = 'Choose League', choices = c('MLB-AL','MLB-NL'), selected = NULL, multiple = FALSE),
      
      # Choose Team
      selectInput(inputId = 'h.team', label = 'Choose Team', choices = c('Pick a League'), selected = NULL, multiple = FALSE),
      
      # Choose Hitter
      selectInput(inputId = 'hitter', label = 'Choose Hitter', choices = c('Pick a Team'), selected = NULL, multiple = FALSE),
      
      actionButton("submitButton", "submit"),
      br(),
      p('App utilizes the baseballR package which scrapes data from baseballsavant.com and baseball-reference.com and provides code for spray chart displayed')
    ),
    mainPanel(
      sidebarPanel(
        titlePanel("Pitch Usages"),
        tableOutput('usage'),
        titlePanel("Whiff Rates"),
        tableOutput('whiffs')
      ),
      mainPanel(
      titlePanel("Spray Chart"),
      plotOutput('spray'),
      titlePanel('Zone Profile'),
      plotOutput('zone.profile')
      )
      
    )
  )
   
   
)

pitcher.league <- NULL
hitter.league <- NULL
chosen.pitcher <- NULL
chosen.hitter <- NULL
submitted <- -1

data <- NULL


server <- function(input, output,session) {
  
  ########################## Pitcher Selection ##############################

   observe({
     # Get List of Teams in Correct League
     
      chosen.league <- input$p.league
      pitcher.league <<- chosen.league

      team.data <- pitcher.data %>% 
         filter(Level == chosen.league) %>% 
         filter(grepl(',',Team) == FALSE)
      team.list <- unique(team.data$Team)
       
      # Update Team Choices
      updateSelectInput(session, "p.team",
                         label = "Choose Team",
                         choices = team.list)
      
   })
   observe({
     # Get list of Pitchers on Correct Team
     chosen.team <- input$p.team
     
     player.data <- pitcher.data %>% 
       filter(Level == pitcher.league) %>% 
       filter(grepl(chosen.team,Team) == TRUE)
     pitcher.list <- unique(player.data$Name)
     
     # Update Player Choices
     updateSelectInput(session, "pitcher",
                       label = "Choose Pitcher",
                       choices = pitcher.list)
  })
  observe({
    chosen.pitcher <<- input$pitcher
  })
   #################### Hitter Selection ############################
   
   observe({
     # Get List of Teams in Correct League
     
     chosen.league <- input$h.league
     hitter.league <<- chosen.league
     
     team.data <- hitter.data %>% 
       filter(Level == chosen.league) %>% 
       filter(grepl(',',Team) == FALSE)
     team.list <- unique(team.data$Team)
     
     
     # Update Team Choices
     updateSelectInput(session, "h.team",
                       label = "Choose Team",
                       choices = team.list)
     
   })
   observe({
     # Get list of Hitters on Correct Team
     chosen.team <- input$h.team
     
     player.data <- hitter.data %>% 
       filter(Level == hitter.league) %>% 
       filter(grepl(chosen.team,Team) == TRUE)
     hitter.list <- unique(player.data$Name)
     
     # Update Player Choices
     updateSelectInput(session, "hitter",
                       label = "Choose Hitter",
                       choices = hitter.list)
   })
   observe({
     chosen.hitter <<- input$hitter
   })
   
   # On Submit
   observe({
     local.submit <- input$submitButton
     print('something is happening')
     if(submitted == -1){
       submitted <<- 0
       print('this ish')
     }
     else{
       
       # Get data for Pitcher/Hitter Combo
       submitted <<- input$submitButton
       
       # Get Names
       pitcher.name <- strsplit(chosen.pitcher," ")
       pitcher.first <- pitcher.name[[1]][1]
       pitcher.last <- pitcher.name[[1]][2]
       hitter.name <- strsplit(chosen.hitter," ")
       hitter.first <- hitter.name[[1]][1]
       hitter.last <- hitter.name[[1]][2]
       
       # Get IDs
       pitcher.id <- playerid_lookup(pitcher.last,pitcher.first)$mlbam_id
       hitter.id <- playerid_lookup(hitter.last,hitter.first)$mlbam_id
       print("ids are")
       print(pitcher.id)
       print(hitter.id)
       
       # Get Data
       data <<- data.frame(scrape_statcast_savant(start_date = "2017-04-06", end_date = "2017-09-15", playerid = pitcher.id, player_type = 'pitcher'))

       # Filter for Hitter
       filtered.data <- data %>% 
         filter(batter == hitter.id)
       
       validate(
         need(nrow(filtered.data) != 0, "This Matchup Contains No Data")
       )
       
       # Output Usages
       usage <- data.frame(sort(table(droplevels(filtered.data$pitch_type)),decreasing=TRUE))
       colnames(usage)[1] <- 'Pitch'
       output$usage <- renderTable(usage)

       # Output Whiffs
       whiffs <- NULL
       pitches <- data.frame(unique(filtered.data$pitch_type))
       colnames(pitches)[1] <- 'Pitch'
       
       for(i in 1:nrow(pitches)){
         temp.data <- filtered.data %>% 
           filter(pitch_type == pitches[i,])
         whiffs[i] <- mean(temp.data$description == 'swinging_strike') * 100
       }
       whiffs <- na.omit(data.frame(cbind(pitches,whiffs)))
       colnames(whiffs)[2] <- 'Whiff %'
       output$whiffs <- renderTable(whiffs)
       
       # Output Spray Chart
       spray.data <- filtered.data %>% 
         filter(is.na(events) != TRUE) %>% 
         filter(events != 'strikeout')
       if(nrow(spray.data) > 0){
         for(i in 1:nrow(spray.data)){
           spray.data$hit_type[i] <- switch(as.character(spray.data$events[i]),'single' = '1','double'='2','triple'='3','homerun'='4','field_out')
         }
         output$spray <- renderPlot(ggspraychart(spray.data, point_alpha = .6, fill_legend_title = "Hit Type", fill_value = "hit_type", 
            fill_palette = c("1"="#A2C8EC", "2"="#006BA4", "3"="#FF940E",
            "field_out"="#595959", "4"="#C85200")))
       }
       
       # Output Zone Profile
       avg.bot <- mean(filtered.data$sz_bot,na.rm = TRUE)
       avg.top <- mean(filtered.data$sz_top,na.rm = TRUE)
       print(avg.bot)
       
       output$zone.profile <- renderPlot(ggplot(filtered.data,aes(x=plate_x,y=plate_z,color=pitch_type),size=2) + geom_point() +
         geom_rect(aes(xmin=-1,xmax=1,ymin=avg.bot,ymax=avg.top),size=1,color = "blue",alpha = .001) +
         scale_x_continuous(limits = c(-2, 2)) + scale_y_continuous(limits = c(0,3.8))
       )
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

