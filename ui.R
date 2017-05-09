# Define UI for application that draws a histogram
suppressMessages(library(plotly))

shinyUI(fluidPage(
  titlePanel("Shiny Judging App"),
    sidebarLayout(position = "left",
    sidebarPanel(
      tabsetPanel(
        tabPanel("Judge Votes",
         fluidRow(
           h2("Judge Scoring", align = "center"),
             column(12,
               column(12,
                 textInput("judgeid", label = h3("Judge Id"), value = "", placeholder = "Judge Name ie: Bob")
               ),
               column(12,
                 textInput("posterid", label = h3("Poster Id"), value = "", placeholder = "Poster Name ie: G12")
               ),
               column(8,
                 numericInput("total", label = h3("Total"), value = "", min = 1, max = 12, step = 1, width = "100%")
               ),
               column(4,
                 h4("Best One?"),
                 checkboxInput("best", label = NULL, value = FALSE)
               )
            ),
            actionButton("submitvote", label = "Submit Votes", icon = NULL, width = "100%")
          ),
          hr(),
          fluidRow(
            h2("GSM Suggestions", align = "center"),
            textInput("gsm1", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            textInput("gsm2", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            textInput("gsm3", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            actionButton("submitgsm", label = "Submit GSM Vote", icon = NULL, width = "100%")
          )
        ),
        tabPanel("People's Choice",
          fluidRow(
            textInput("pep1", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            textInput("pep2", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            textInput("pep3", label = "Poster #: ", value = "", placeholder = "ie: GF1", width = "100%"),
            actionButton("submitpepc", label = "Submit People's Choice Vote", icon = NULL, width = "100%")
          )
        )
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Running Total",
           fluidRow(
             column(12,
               column(4,
                 h3("Graduate Posters", align = "center"),
                 # plotOutput("plotG"),
                 plotlyOutput("plotlyG")
               ),
               column(4,
                 h3("Undergraduate Posters", align = "center"),
                 # plotOutput("plotU"),
                 plotlyOutput("plotlyU")
               ),
               column(4,
                 h3("Student Posters", align = "center"),
                 # plotOutput("plotS")
                 plotlyOutput("plotlyS")
               )
             )
           ),
           hr(),
           fluidRow(
             column(12,
               column(4,
                 h3("GSM Recommendations", align = "center"),
                 # plotOutput("plotGSM"),
                 plotlyOutput("plotlyGSM")
               ),
               column(4,
                 h3("People's Choice", align = "center"),
                 # plotOutput("plotPEPC"),
                 plotlyOutput("plotlyPEPC")
               ),
               column(4,
                 downloadButton('judgeResponses', 'Download Judge Scoring'),
                 br(),
                 br(),
                 br(),
                 downloadButton('GSMVotes', 'Download GSM Votes'),
                 br(),
                 br(),
                 br(),
                 downloadButton('peoplesChoice', 'Download Peoples Choice')
               )
             )
           ),
           hr()
           # DT::dataTableOutput("responses", width = 300), tags$hr()
        ),
        tabPanel("Graduate",
                 h1("Graduate Posters"),
                 uiOutput("winnersG")
        ),
        tabPanel("Undergradute",
                 h1("Undergraduate Posters"),
                 uiOutput("winnersU")
        ),
        tabPanel("Student",
                 h1("Class Project Posters"),
                 uiOutput("winnersS")
        ),
        tabPanel("People's Choice",
                 h1("People's Choice Awards"),
                 uiOutput("winersPep")
        )
      )
    )
  )
))
