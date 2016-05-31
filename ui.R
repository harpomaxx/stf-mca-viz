library(shiny)

shinyUI(fluidPage(
  
  titlePanel("2D Behavioral Models Representation (beta)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", 
                  label = "Choose a dataset to use",
  choices = list("dataset22_raw", "dataset45_raw","dataset87_raw", "DGA"),
                  selected = "dataset87_raw"),
     # helpText("Botnet vs Normal Traffic"),
      sliderInput("size", label = h5("Select the number of flows in STF connection"),
                  min = 1, max = 100, value = c(6,16)), 
      textInput("labeltext1", label = h5("Select traffic of Interest"), 
                value = '[a-zA-Z0-9]'),
      helpText("Default regexp machts only STF connections with valid labels"),
      
      textInput("labeltext2", label = h5("First Group [blue]"), 
                value = 'Normal'),
      textInput("labeltext3", label = h5("Second Group [orange]"), 
                value = 'Botnet'),
      helpText("Warning: Previous selections should be valid regexps"),
      htmlOutput("basic_info")
      
    
      
    ),
    mainPanel(
      actionButton("zoom", label = "Reset Zoom"),
      helpText("Select an area of insterest and double lick for zooming it"),
      plotOutput("barplot",click = "plot1_click", dblclick = "plot1_dblclick",
                 brush = brushOpts(
        id = "plot1_brush"
      )),
     
     tableOutput('brush_info')
      #verbatimTextOutput("brush_info")
        
    )
    
    
  )
  
)
)

