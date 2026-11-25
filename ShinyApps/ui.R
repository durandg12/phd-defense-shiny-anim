m <- 1000
xi <- rnorm(m)

choose_width <- "100px"

fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Target confidence level:", 0.1, min = 0, max = 1,
                   width = choose_width),
      numericInput("delta", "Signal strengh:", 3, min = 1, max = 10,
                   width = choose_width),
      numericInput("s", "Region size:", 10, min = 1, max = m / 10,
                   width = choose_width),
      numericInput("nb_reg_signal", "Number of regions with signal:", 3,
                   min = 1, max = 10, width = choose_width),
      width = 2
    ),
    mainPanel(
      h3(textOutput("bound")),
      plotlyOutput("plot")
    )
  )
)
