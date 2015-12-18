library(shiny)
library(rCharts)
options(RCHART_LIB = 'nvd3')

shinyUI(
  navbarPage(
    theme = "bootstrap3.min.css",
    "Exploring The Titanic Dataset",
    tabPanel(p(icon("star"), "Welcome"),
             h2("Welcome to Exploring The Titanic Dataset"),
             includeMarkdown("welcome.md")
             ),
    tabPanel(p(icon("bar-chart"),"Explore"),
             sidebarLayout(
               sidebarPanel(
                 h4("Select Attributes To Change the Chart Parameters"),
                 checkboxGroupInput(
                   "sexInput", "Sex",
                   c("Male" = "male", "Female" =
                       "female"),
                   selected = c("male", "female")
                 ),
                 sliderInput("ageInput", "Age", 0, 80, c(0, 80)),
                 checkboxGroupInput(
                   "classInput", "Class", c(
                     "First" = 1,
                     "Second" = 2,
                     "Third" = 3
                   ),
                   selected = c(1,2,3)
                 ),
                 p("See About | Help for more information on using this App.")
               ),
               mainPanel(
                 h4("Passenger Outcome by Sex", align = "center"),
                 showOutput("plot1", "nvd3"),
                 h4("Passenger Outcome by Class", align = "center"),
                 showOutput("plot2", "nvd3"),
                 h4("Passenger Ages Histogram", align = "center"),
                 showOutput("plot3", "nvd3"))
               
             )),
    
    tabPanel(p(icon("cog"),"Predict"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("sexInput2", "Sex",
                              c("Male" = "male", "Female" =
                                  "female")),
                 numericInput(
                   "ageInput2", "Age", value = 14, min = 1, max = 80
                 ),
                 conditionalPanel(condition = "input.sexInput2 == 'male'",
                                  selectInput(
                                    "titleInput2", "Title", c("Master",
                                                              "Mr",
                                                              "Sir",
                                                              "Dr",
                                                              "Capt",
                                                              "Don",
                                                              "Major",
                                                              "Rev",
                                                              "Col"),
                                    selected = "Master"
                                  )),
                 
                 conditionalPanel(condition = "input.sexInput2 == 'female'",
                                  selectInput(
                                    "titleInput3", "Title", c(
                                      "Miss",
                                      "Mme",
                                      "Mlle",
                                      "Mrs",
                                      "Lady",
                                      "Dr",
                                      "Dona",
                                      "the Countess",
                                      "Jonkheer"
                                    ),
                                    selected = "Mrs"
                                  )),
                 
                 radioButtons("classInput2", "Class", c("First", "Second", "Third"), selected = "Second"),
                 radioButtons("familyInput2", "Travelling with your family?", c("No", "Yes")),
                 radioButtons(
                   "embarkedInput2", "Departure port?", c(
                     "Southhampton" = "S",
                     "Cherbourg" = "C",
                     "Queenstown " = "Q"
                   )
                 ),
                 p("See About | Help for more information on using this App.")
               ),
               mainPanel(
                 h2("Would you have survived the Titanic disaster?"),
                 p(
                   "Enter your details on the left hand panel to
                   see if you would have survived the sinking of the
                   Titanic"
                 ),
                 hr(),
                 textOutput("predictionText"),
                 br(),
                 textOutput("predictionProb")
                 )
               )),
    
    tabPanel(p(icon("table"),"Data"),
             dataTableOutput(outputId = "table")),
    
    navbarMenu(
      p("About"),
      tabPanel("Model",
               mainPanel(includeMarkdown("model.md"))),
      tabPanel("Help",
               mainPanel(includeMarkdown("help.md")))
    )
    , class = "span12")
)
