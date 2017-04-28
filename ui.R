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
      hidden(
        lapply(seq(4), function(i) {
          div(
            class = "page",
            id = paste0("step", i),
            "Step", i,
            if(i == 1){
              uiOutput("page1")
              
            }
            else if (i == 2){
              uiOutput("page2")
            }
            else if (i == 3){
              uiOutput("page3")
            }
            else if (i == 4){
              uiOutput("page4")
            }
          )
        })
      ),
      br(),
      actionButton("prevBtn", "< Previous"),
      actionButton("nextBtn", "Next >")
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
          title = "About File"
        )
      )
    )
  )
))
