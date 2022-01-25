suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(bs4Dash)
  library(waiter)
})

ui <- dashboardPage(
  dashboardHeader(
    title = "Basic dashboard"
  ),
  dashboardSidebar(
    sidebarUserPanel(
      name = "Hello"
    ),
    sidebarMenu(
      id = "sidebarmenu",
      sidebarHeader("Header 1"),
      menuItem(
        "Item 1",
        tabName = "item1",
        icon = icon("sliders-h")
      ),
      menuItem(
        "Item 2",
        tabName = "item2",
        icon = icon("id-card")
      ),
      menuItem(
        text = "Item List 1",
        icon = icon("bars"),
        startExpanded = TRUE,
        menuSubItem(
          text = "Item 3",
          tabName = "tab3",
          icon = icon("circle")
        ),
        menuSubItem(
          text = "Item 4",
          tabName = "tab4",
          icon = icon("circle")
        )
      ),
      compact = TRUE
      #flat = TRUE
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      ),
      box(
        title = "Debug Output",
        htmlOutput("debug")
      )
    )
  ),
  controlbar = dashboardControlbar(),
  dark = TRUE,
  preloader = list(html = tagList(spin_1(), "Loading..."), color = "#235972")
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$debug <- renderText({
    paste(
      input$sidebarmenu,
      input$sidebarItemExpanded,
      sep = "<br>"
    )
  })
}

shinyApp(ui, server)