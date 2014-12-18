
library(markdown)
vars <- reactiveValues(page=NULL, users=NULL) # shared between all users!

vars$page <- 1

# shinyUI no longer required, apparently...
ui <- shinyUI(fluidPage(
  # display the appropriate page for the user. how do I know what that is?
  # dynamic ui example: http://shiny.rstudio.com/gallery/dynamic-ui.html
  # the trick is to call renderUI() on the server side
  # then uiOutput here...
  uiOutput("page")
))

# "shinyServer is called once when each client loads the shiny url..."
server <- function(input, output) {
  # pages could easily be in separate files...
  currentPage <- reactive({vars$page})
  pages <- list()
  pages[[1]] <- {
    titlePanel("Instructions")
    HTML(markdownToHTML(text="
### Instructions

You will be in groups of four...

etc.
      ", fragment.only=TRUE))
    
  }
  pages[[2]] <- {
    titlePanel("Instructions")
    HTML("Some more HTML")
  }
  pages[[3]] <- {
    
  }
  output$page <- renderUI(pages[[currentPage()]])
}

shinyApp(ui = ui, server = server)