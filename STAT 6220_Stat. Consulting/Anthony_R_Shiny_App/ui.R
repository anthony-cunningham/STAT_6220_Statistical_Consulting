library(shiny)

ui <- fluidPage(
    titlePanel("Burroughs Visualizations and Tests"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                  label = "Choose a Variable:",
                  choices = c("Labs", "Within90")),
            br(),
            p(strong("Labs"), "determines whether or not a given patient completed the full set of 4 lab exams."),
            br(),
            p(strong("Within90"), "determines whether or not a given patient was seen by a specialist within 90 days of initial ADT treatment."),
            br(),
            checkboxInput(inputId = "midp",
                          label = strong("Use Mid-p Value"),
                          value = FALSE)),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("2-by-2 Contingency Tables", verbatimTextOutput("cont_table")),
                      tabPanel("Fisher's Exact Tests", verbatimTextOutput("fisher_test")),
                      tabPanel("Bar Chart: Counts", plotOutput("plot_count")),
                      tabPanel("Bar Chart: Proportions", plotOutput("plot_prop")),
                      tabPanel("Donut Charts", plotOutput("plot_donut")))
          )
      )
)