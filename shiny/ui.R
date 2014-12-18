shinyUI(fluidPage(
  sidebarLayout(
    titlePanel("My first multiuser app"),
    mainPanel(
      actionButton("pressme", "Press me"),
      p(textOutput("pressed")),
      actionButton("next_stage", "Next stage"),
      h2(htmlOutput("frag"))
    )
  )  
))

  
  