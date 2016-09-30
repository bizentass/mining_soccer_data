if (!require("pacman")) install.packages("pacman")
pacman::p_load(shinydashboard,plotly)

header <- dashboardHeader(
  title = "SoccerMiner"
)

sideBar <- dashboardSidebar(
                  textInput("text", label="Enter Team"),
                  column(width = 12,
                    submitButton("Submit"),
                    p()
                  ),
                  column(width = 12,
                    box(width = NULL, status = "warning",title="Query made",
                      verbatimTextOutput("text")
                    ),
                    box( width = NULL, status = "info", title = "Tweets",
                      verbatimTextOutput("tweet_text")
                    )
                  )
              )

body <- dashboardBody(
         fluidRow(
           column(width = 9,
              box( width = NULL,
                  plotlyOutput("plot1")
              )
           ),
           column(width = 3,
              box( width = NULL,
                   infoBoxOutput("rating")
              )
           ),
           column(width = 12,
                  box( width = NULL,
                       plotlyOutput("kmeans")
                  )
           ),
           column(width = 12,
              box( width = NULL,
                   leafletOutput("soccerMap")
              )
           )
         )
)

dashboardPage(
  header,
  sideBar,
  body
)