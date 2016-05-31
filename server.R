library(shiny)
library(MASS)
#global variales 
data = read.csv(file = "data/dataset87_raw", sep = '|', header = T)

get_states <- function(start, size,labeled_data) {
  states <-
    strsplit(substring(as.character(labeled_data[, 4]), start), "")
  f_states = mapply(function(x, y) {
    #fix length
    length(x) <- y # Roland's better suggestion
    return(x)
  }, states, size)
  f_states = as.data.frame(t(f_states)) # to data frame
  f_states[1, ] = lapply(f_states[1, ], as.factor) #to factor
  return (f_states)
}

get_mca <- function(start, size,labeled_data) {
  m <- mca(get_states(start, size,labeled_data), nf = 2)
  rs <- cbind(as.data.frame(m$rs), labeled_data[, 2])
  colnames(rs) <- c("X", "Y", "Labels")
  return (rs)
}

shinyServer(function(input, output) {
 
  get_data <- reactive({
    data=read.csv(file = paste("data/",input$dataset,sep="")
    , sep = '|', header = T)
})  
  
  get_labeled_data<-reactive({
    data<-get_data()
    data[grep(input$labeltext1, data$Label), ]
  })

  get_rs<-reactive({
    labeled_data<-get_labeled_data() 
    rs <- get_mca(input$size[1], input$size[2],labeled_data)
  })

  output$brush_info <- renderTable({
    rs <-get_rs()
    bpoints<- brushedPoints(
      rs,
      input$plot1_brush,
      xvar = "X",
      yvar = "Y",
      allRows = FALSE
    )[,3]
    table(gsub('-[0-9]+',"",bpoints))
    })
#  output$selected_info <- renderPlot({
#    labeled_data<-get_labeled_data()
#    barplot(c(nrow(labeled_data),
#              length(grep(input$labeltext2, labeled_data[, 2])),
#              length(grep(input$labeltext3, labeled_data[, 2]))
#              ),col=c("skyblue"),border=('pink'),names.arg=c(input$labeltext1,input$labeltext2,input$labeltext3),las=2)
#    
#  }, width=200 , height = 200)
  
   output$basic_info <- renderUI({
   labeled_data<-get_labeled_data()
   HTML(paste("<HR><b>Labels Information : </b>","<br/> ",input$labeltext1," : ",nrow(labeled_data),"<br/>",
               input$labeltext2," : ",length(grep(input$labeltext2, labeled_data[, 2])), "<br/>",
             input$labeltext3," : ",length(grep(input$labeltext3, labeled_data[, 2])),"<br/><br/>", sep=""))
    
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$zoom,{
    ranges$x <- NULL
    ranges$y <- NULL
  }
               ) 
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$barplot <- renderPlot({
    labeled_data<-get_labeled_data()
    data.color <- 'black'
    data.color[grep(input$labeltext2, labeled_data[, 2])] <- 'skyblue'
    data.color[grep(input$labeltext3, labeled_data[, 2])] <- 'orange'
    rs <-get_rs()
    colnames(rs) <- c("X", "Y", "Labels")
    
    plot(rs$X,
         rs$Y,
         col = data.color,
         pch = 21,
         cex = 1.1,
         ylim=ranges$y,
         xlim=ranges$x
         )
    legend("topright",c(input$labeltext2,input$labeltext3),cex=0.8,pch=1,col=c('skyblue','orange'))
    grid()
  })
  
  
})
