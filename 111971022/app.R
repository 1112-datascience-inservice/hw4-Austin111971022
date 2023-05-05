library(shiny)
library(ggbiplot)  
library(factoextra)
library(FactoMineR)

ui <- fluidPage(
  tags$img(src = "https://www.nccu.edu.tw/var/file/0/1000/img/logo_en.png", width = "300", height = "auto"),
  headerPanel("Data Science HW4"),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
         'input.dataset === "PCA"',
         tags$img(src = "https://www.cs.nccu.edu.tw/web/images/logo.png",
                  style = "display: block; margin: auto; background-color: #191970; max-width: 100%; height: auto;"),
          div(style = "height: 20px;"), 
          tags$style('.font0 { font-size: 20px; color: #000000; font-weight: bold;}'),
          helpText("Principal Component Analysis (PCA)", class = "font0"), 
          div(style = "height: 10px;"), 
          radioButtons('x', 'X Variable', c("pc1"="1", "pc3"="3", "pc4"="4"), inline = TRUE),
          radioButtons('y', 'Y Variable', c("pc2"="2", "pc3"="3", "pc4"="4"), inline = TRUE),
         ),
        
        conditionalPanel(
         'input.dataset === "CA"',
         tags$img(src = "https://www.cs.nccu.edu.tw/web/images/logo.png",
                  style = "display: block; margin: auto; background-color: #191970; max-width: 100%; height: auto;"),
         div(style = "height: 20px;"), 
         helpText("Correspondence Analysis (CA)", class = "font0"),
         tags$img(src = "https://www.integratedots.com/wp-content/uploads/2019/06/iris_petal-sepal-e1560211020463.png",
                  style = "display: block; margin: auto; max-width: 100%; height: auto;")
         ),
         
         conditionalPanel(
           'input.dataset === "Input data(log)"',
           tags$img(src = "https://www.cs.nccu.edu.tw/web/images/logo.png",
                    style = "display: block; margin: auto; background-color: #191970; max-width: 100%; height: auto;"),
           div(style = "height: 20px;"),
           tags$style('.font1 { font-size: 20px; color: #000000; }'),
           helpText("Input Iris Data (log)", class = "font0") 
         ) 
    ),
      
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("PCA", plotOutput('plot')),
        tabPanel("CA", plotOutput('ca')),
        tabPanel("Input data(log)", DT::dataTableOutput("mytable3"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # log transform 
  data(iris)
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  ir.pca <- reactive({
    prcomp(log.ir, center = TRUE, scale. = TRUE)
  })
  
  # Compute CA on the iris dataset
  iris.ca <- CA(iris[,1:4])
  
  # update y variable options based on x variable selection
   observeEvent(input$x, {
    if(input$x == "1") {
      updateRadioButtons(session, 'y', 'Y Variable', c("pc2"="2", "pc3"="3", "pc4"="4"),  selected = input$y, inline = TRUE)}
    else if(input$x == "2") {
      updateRadioButtons(session, 'y', 'Y Variable', c("pc1"="1", "pc3"="3", "pc4"="4"),  selected = input$y, inline = TRUE)}
    else if(input$x == "3") {
      updateRadioButtons(session, 'y', 'Y Variable', c("pc1"="1", "pc2"="2", "pc4"="4"),  selected = input$y, inline = TRUE)}
    else if(input$x == "4") {
      updateRadioButtons(session, 'y', 'Y Variable', c("pc1"="1", "pc2"="2", "pc3"="3"),  selected = input$y, inline = TRUE)}
    }
  )
  # update x variable options based on y variable selection
  observeEvent(input$y, {
    if(input$y == "2") {
      updateRadioButtons(session, 'x', 'X Variable', c("pc1"="1", "pc3"="3", "pc4"="4"), selected = input$x, inline = TRUE)}
    else if(input$y == "1") {
      updateRadioButtons(session, 'x', 'X Variable', c("pc2"="2", "pc3"="3", "pc4"="4"), selected = input$x, inline = TRUE)}
    else if(input$y == "3") {
      updateRadioButtons(session, 'x', 'X Variable', c("pc1"="1", "pc2"="2", "pc4"="4"), selected = input$x, inline = TRUE)}
    else if(input$y == "4") {
      updateRadioButtons(session, 'x', 'X Variable', c("pc1"="1", "pc2"="2", "pc3"="3"), selected = input$x, inline = TRUE)}
    }
  )
  
  # ggbiplot
  output$plot <- renderPlot(
    ggbiplot(ir.pca(), choices = c(as.numeric(input$x),as.numeric(input$y)), obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE) +
      scale_color_discrete(name = '') +
      theme(legend.direction = 'horizontal', legend.position = 'top')
  )
  
  # customize the length drop-down menu
  output$mytable3 <- DT::renderDataTable(
    DT::datatable(log.ir, options = list(lengthMenu = c(10, 20, 30), pageLength = 10))
  )
  
  output$ca <- renderPlot(
    fviz_ca_biplot(iris.ca, 
                   arrow = c(FALSE, TRUE),
                   repel = FALSE,
                   geom = c("point", "text"),
                   pointsize = 1,
                   labelsize = 4)
  )
}

shinyApp(ui, server)