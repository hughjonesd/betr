
# demo:

# take a number of html fragments and push them to some clients
# update the clients with a number of button presses from them both
frag <- character(0)
frag <- sapply(1:5, function(x) paste("<b>stage",x,"</b>"))
pressed <- -1
now <- 0
shinyServer(func=function(input, output) {
#   mypressed <- 0
#   mypressctr <- 0
#   was_i_pressed <- reactive({
#     if (input$pressme > mypressctr) {
#       mypressed <<- mypressed + 1
#       pressed <<- pressed + 1
#     }
#     mypressctr <<- input$pressme
#   })
#   count_my_presses <- reactive({
#     was_i_pressed()
#     mypressed
#     })
#   all_presses <- reactive({
#     was_i_pressed()
#     pressed
#   })
#   output$pressed <- renderText({
#     sprintf("I have been pressed %s times by you, and %s times in total",
#         count_my_presses(), all_presses())
#   })
  observe({
    input$pressme
    pressed <<- pressed + 1
    output$pressed <- renderText({
      sprintf("I have been pressed %s times in total", pressed)
    })
  })
  observe({
    input$next_stage
    output$frag <- renderText({
        now <<- now + 1
        frag[now]
    })
  })
  
})