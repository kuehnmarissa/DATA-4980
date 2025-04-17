library(shiny)
library(readxl)
library(tidyverse)
library(DT)

ui <- fluidPage(
    tabsetPanel(
      tabPanel(title = "View Data", 
               dataTableOutput(outputId="view_data_dataTable")
      ),
      tabPanel(title = "Streams",
               numericInput(inputId = "streams_in_num",label="How many artists to display?",value=3),
               plotOutput(outputId = "streams_out_plot1"),
               plotOutput(outputId = "streams_out_plot2")
      ),
      tabPanel(title = "Songs",
               numericInput(inputId = "songs_in_num",label="How many artists to display?",value=3),
               plotOutput(outputId = "songs_out_plot1"),
               dataTableOutput(outputId = "songs_out_dataTable")
      ),
      tabPanel(title = "Musical Qualities",
               plotOutput(outputId = "mq_out_plot1"),
               plotOutput(outputId = "mq_out_plot2"),
               plotOutput(outputId = "mq_out_plot3")
      )
      
    )
)

server <- function(input, output){
  
  # Read in spotify data
  data <- read_xlsx("spotify-2023.xlsx")  
  
  # Output for "View data"
  output$view_data_dataTable <- renderDT(data)
  
  # Create new dataframe with the artists and their number of songs and total streams, sorted by descending streams
  df <- data %>% 
    separate_rows(Artist,sep=",")%>% 
    group_by(Artist) %>% 
    summarize(songs=n(), total_streams=sum(Streams)) %>% 
    arrange(desc(total_streams))
  
  # Create new dataframe with the artists and their shares of the songs and total streams
  df_pct <- df %>% 
    summarize(Artist=Artist,songs_pct=songs/sum(songs),streams_pct=total_streams/sum(total_streams)) %>% 
    arrange(desc(songs_pct))
  
  # Create new dataframe with songs per year
  df_year <- data %>% group_by(`Year Released`) %>% summarize(songs=n())
  
  
  # Output for "Streams"
  output$streams_out_plot1 <- renderPlot(
    ggplot(df %>% slice(1:input$streams_in_num))+
      geom_col(aes(x=fct_reorder(Artist,total_streams),y=total_streams))+
      labs(title="Total Streams",x="Artist",y="Total Streams in Millions") +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      coord_flip()+
      theme_minimal()
  )
  output$streams_out_plot2 <- renderPlot(
    ggplot(df)+
      geom_histogram(aes(x=total_streams))+
      labs(title="Distribution of Streams",x="Total Streams", y="Number of Songs")+
      scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))+
      theme_minimal()
  )
  

  # Output for "Songs"
  output$songs_out_plot1 <- renderPlot(
    ggplot(df_pct %>% slice(1:input$songs_in_num))+
      geom_col(aes(x=fct_reorder(Artist,songs_pct),y=songs_pct))+
      labs(title="Percent of All Songs",x="Artist",y="Percent of All Songs") +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()+
      theme_minimal()
  )
  output$songs_out_dataTable <- renderDT(df_year)
  
  # Create new dataframes for musical qualities
  df_mode <- data %>% group_by(Mode) %>% summarize(songs=n(), total_streams=sum(Streams)) 
  df_key <- data %>% group_by(Key) %>% summarize(songs=n(), total_streams=sum(Streams)) 
  
  # Output for "Musical Qualities"
  output$mq_out_plot1 <- renderPlot(
    ggplot(df_mode) +
      geom_col(aes(x=Mode,y=songs))+
      labs(title="Spotify Songs By Musical Mode",x="Mode",y="Number of Songs")+
      theme_minimal()
  )
  output$mq_out_plot2 <- renderPlot(
    ggplot(df_key) +
      geom_col(aes(x=fct_reorder(Key,desc(songs)),y=songs))+
      labs(title="Spotify Songs By Musical Key",x="Key",y="Number of Songs")+
      theme_minimal()
  )
  output$mq_out_plot3 <- renderPlot(
    ggplot(data) +
      geom_histogram(aes(x=BPM))+
      scale_x_continuous(breaks = seq(min(data$BPM), max(data$BPM), by = 15))+
      labs(title="Distribution of Spotify Songs By Beats Per Minute (BPM)",x="BPM",y="Number of Songs")+
      theme_minimal()
  )
  
}


shinyApp(ui=ui, server=server)
