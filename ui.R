##### inputs and outputs #####

#workingdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#setwd(workingdirectory)
#outputdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#install.packages("shiny")
library(shiny)
library(shinyjs)

##### define UI #####
shinyUI(fluidPage( #fluidpage based on different browser
  useShinyjs(),
  #Application title
  titlePanel("Statistical Characteristics Calculator"),
  #sidebarlayout
  sidebarLayout(
    #sidebarpanel
    sidebarPanel(
      titlePanel("Enter Information:"),
      tabsetPanel(selected = "General Information", id = 'mynavlist',
        tabPanel("General Information",
                 br(),
                 radioButtons("de-escalation", "With or Without Dose De-escalation", c(With = '1',Without = '0'),selected = '1'),
                 textInput("doseNumber","Please Enter Number of Doses in the Scenario:","6",placeholder = "Enter an integer"),
                 actionButton("Done1","Done")
        ),
        tabPanel("Probabilities",
                 br(),
                 uiOutput("dynamicInputs"),
                 fileInput("probabilities", "Select Input File for Probabilities", multiple = FALSE, accept = NULL, width = NULL),
                 radioButtons("sep1",label = "Separator of choice",choices = c(Comma = ',',Semocolon = ';',Tab = '\t',Space = ' '),selected = ','),
                 actionButton("Done2","Done")
        ),
        tabPanel("Parameters",
                 br(),
                 fluidRow(
                   column(6,textInput("a","Enter A: ", "3",width = "200px",placeholder = "Enter an integer")),
                   column(6,textInput("b","Enter B: ", "3",width = "200px",placeholder = "Enter an integer")) 
                 ),                 
                 fluidRow(
                   column(6,textInput("c","Enter C: ", "1",width = "200px",placeholder = "Enter an integer")),
                   column(6,textInput("d","Enter D: ", "1",width = "200px",placeholder = "Enter an integer")) 
                 ),
                 fluidRow(
                   column(6,textInput("e","Enter E: ", "1",width = "200px",placeholder = "Enter an integer"))
                 ),
                 actionButton("calculate","Calculate")
        )
      )
    ),
    #mainpanel
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Input Information",
          h1("Probability of toxicity"),
          tableOutput("dataProbabilities")
        ),
        tabPanel(
          title = "Data output",
          br(),
          downloadButton("downFile","Download all files"),
          br(),
          tableOutput("deescalation"),
          tableOutput("otherstats")
        ),
        tabPanel(
          title = "Graphic Output",
          br(),
          downloadButton("down","Download all files"),
          br(),
          plotOutput("MTDbarplot"),
          downloadButton("downMTDbarplot","Download barplot"),
          br(),
          plotOutput("MTDscatterplot"),
          downloadButton("downMTDscatterplot","Download scatterplot"),
          br(),
          plotOutput("ptNumbarplot"),
          downloadButton("downptNumbarplot","Download barplot"),
          br(),
          plotOutput("ptNumscatterplot"),
          downloadButton("downptNumscatterplot","Download scatterplot"),
          br(),
          plotOutput("toxincbarplot"),
          downloadButton("downtoxincbarplot","Download barplot"),
          br(),
          plotOutput("toxinccatterplot"),
          downloadButton("downtoxinccatterplot","Download scatterplot"),
          br()
        ),
        tabPanel(
          title = "About File",
          h1("Probability of toxicity file path"),
          tableOutput("fileProbabilities")
        )
      )
    )
  )
))
