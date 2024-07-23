
source("libraries.R")
source("FUN.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "Shiny App" ),
  dashboardSidebar(disable = TRUE),

  dashboardBody(
    fluidRow(
      box(width = 8,   
          
          selectInput( inputId = "plot_choice",
                       label = "Graph type",
                       choices = c("")),
          
          plotlyOutput(outputId = "SelectedPlot")
          
      ),
      
      infoBoxOutput("TargetScoreBox"),
      
      infoBoxOutput("RatioScoreBox"),
      
      
      tabBox(width = 12, 
             tabPanel("Table",
                      
                      DT::dataTableOutput(outputId = "your_table")
             ),
             tabPanel("Raw Data",
                      
                      DT::dataTableOutput(outputId = "raw_data_day"),
                      
             )
      )
    )
  )
  
)
