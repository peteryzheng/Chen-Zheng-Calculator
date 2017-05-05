##### inputs and outputs #####

#workingdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#setwd(workingdirectory)
#outputdirectory = "D:/docs/Spring 2017/499R/Shiny app"
#install.packages("shiny")
library(shiny)
library(shinyjs)

##### define UI #####
shinyUI(fillPage( #fluidpage based on different browser
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  useShinyjs(),
  #Application title
  #sidebarlayout
  tags$body(
    tags$div(
      class = "container",
      titlePanel(
        tags$div(
          class="title",
          tags$img(src='logo.png',class = "title"),
          tags$name("Calculator for Operating Characteristics of 3+3 Designs by Zhengjia Chen & Youyun Zheng", class = "title")
        )
      ),
      tags$div(
        class = "panel",
        sidebarLayout(
          #sidebarpanel
          tags$div(class="side",
                   sidebarPanel(
                     actionButton("prevBtn", "< Previous",style="color: #004990"),
                     actionButton("nextBtn", "Next >",style="color: #004990"),
                     hidden(
                       lapply(seq(2), function(i) {
                         div(
                           class = "page",
                           id = paste0("step", i),
                           if(i == 1){
                             uiOutput("page1")
                             
                           }
                           else if (i == 2){
                             uiOutput("page2")
                           }
                           #else if (i == 3){
                           #   uiOutput("page3")
                           # }
                         )
                       })
                     ),
                     br()
                   )
          ),
          #mainpanel
          mainPanel(
            tags$div(
              class = "main",
              tabsetPanel(
                #tabPanel(
                #  title = h1("Input Information",class = "main"),
                #  tableOutput("dataProbabilities"),
                #  tableOutput("dataDosage")
                #),
                tabPanel(
                  title = h1("Table Output",class = "main"),
                  br(),
                  downloadButton("downFile","Download all files"),
                  br(),
                  tableOutput("deescalation"),
                  tableOutput("otherstats")
                ),
                tabPanel(
                  title = h1("Plot Output", class = "main"),
                  br(),
                  tags$div(class = "widgets",downloadButton("down","Download all files")),
                  br(),
                  plotOutput("MTDbarplot"),
                  tags$div(class = "widgets",downloadButton("downMTDbarplot","Download barplot")),
                  br(),
                  plotOutput("MTDscatterplot"),
                  tags$div(class = "widgets",downloadButton("downMTDscatterplot","Download scatterplot")),
                  br(),
                  plotOutput("ptNumbarplot"),
                  tags$div(class = "widgets",downloadButton("downptNumbarplot","Download barplot"),
                           br(),
                           plotOutput("ptNumscatterplot"),
                           tags$div(class = "widgets",downloadButton("downptNumscatterplot","Download scatterplot")),
                           br(),
                           plotOutput("toxincbarplot"),
                           tags$div(class = "widgets",downloadButton("downtoxincbarplot","Download barplot")),
                           br(),
                           plotOutput("toxinccatterplot"),
                           tags$div(class = "widgets",downloadButton("downtoxinccatterplot","Download scatterplot")),
                           br()
                  )
                ),
                tabPanel(
                  title = h1("User Manual", class = "main"),
                  tags$div(class = "UserManual",
                   tags$h1("User Manual", class = "UserManual"),
                   tags$h2("1. How to use this calculator", class = "UserManual"),
                   tags$h3(
                     class = "UserManual",
                     HTML(
                       "To use this calculator, please finish inputting information on two separate pages:<br><br>"
                     )
                   ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='step1.png',class = "figure2"),
                       tags$h4("Figure 1. User Interface of page 1 of the calculator."
                               , class = "legend")
                     ),
                     tags$h1(HTML("On page 1, you will be able to let the calculator know:<br><br>
                                  <p class = 'tab1'>1. Whether your clinical trial is with or without dose deescalation <br><br></p>
                                  <p class = 'tab2'>a. The radio button gives you two options: without dose de-escalation or with dose de-escalation<br><br></p>
                                  <p class = 'tab1'>2. How many dose levels there will be in your clinical trial<br><br></p> 
                                  <p class = 'tab2'>a. The text Input window lets you input an integer number bigger than 0<br><br></p>
                                  "
                     ), class = "piccontainer2")
                     ),
                   tags$div(
                     class = "piccontainer2",
                     tags$figure(
                       class = "piccontainer2",
                       tags$img(src='step2.png',class = "figure2"),
                       tags$h4("Figure 2. User Interface of page 2 of the calculator."
                               , class = "legend")
                     ),
                     tags$h1(HTML("On page 1, you will be able to let the calculator know:<br><br>
                                  <p class = 'tab1'>1. What the probabilities of dose limiting toxicity are for each dose level <br><br></p>
                                  <p class = 'tab2'>a. The number of text inputs for probabilities of dose limiting toxicity corresponds to however many dose levels were indicated<br><br></p>
                                  <p class = 'tab1'>2. What the dosages are for each dose level (optional)<br><br></p> 
                                  <p class = 'tab2'>a. The number of text inputs for dosages corresponds to however many dose levels were indicated<br><br></p>
                                  <p class = 'tab2'>b. If nothing is entered, 'na' will appear in the corresponding location<br><br></p>
                                  Click the 'Calculate' button to obtain results"
                     ), class = "piccontainer2")
                     ),
                   tags$h2("2. How clinical trial designs work", class = "UserManual"),
                   tags$div(
                     class = "piccontainer",
                     tags$figure(
                       class = "piccontainer",
                       tags$img(src='Without.png',class = "figure"),
                       tags$h4("Figure 3. The flow chart of algorithmic-based clinical trial designs without dose de-escalation"
                               , class = "legend")
                     ),
                     tags$h1(HTML("Starting from dose 1, a cohort of A patients was entered in trial for each dose i.<br><br>
                                  <p class = 'tab1'>1. If less than C out of the cohort of A patients experience DLT, dose escalation is indicated. <br><br></p>
                                  <p class = 'tab1'>2. If more than or equal to C, but less than or equal to D out of the cohort of A patients experience DLT, an additional cohort of B patients will be entered at dose i.<br><br></p> 
                                  <p class = 'tab2'>a.	If less than or equal to E out of the cohort of a total A+B patients experience DLT, dose escalation is indicated. <br><br></p>
                                  <p class = 'tab2'>b.	If more than E out of the cohort of a total A+B patients experience DLT, dose i-1 will be deemed MTD.<br><br></p>
                                  <p class = 'tab1'>3.	If more than D out of the cohort of A patients experience DLT, than dose i-1 will be deemed MTD.<br><br></p>
                                  If dose 0 was deemed MTD, or if dose escalation is still indicated at dose n, the trial is unable to determine MTD.<br> <br>
                                  "
                     ), class = "piccontainer")
                     ),
                   tags$div(
                     class = "piccontainer",
                     tags$figure(
                       class = "piccontainer",
                       tags$img(src='With.png',class = "figure"),
                       tags$h4("Figure 4. The flow chart of algorithmic-based clinical trial designs with dose de-escalation"
                               , class = "legend")
                     ),
                     tags$h1(
                       class = "piccontainer",
                       HTML(
                         "Starting from dose 1, a cohort of A patients was entered in trial for each dose i. <br><br>
                         <p class = 'tab1'>1.	If less than C out of the cohort of A patients experience DLT, dose escalation is indicated. <br><br></p>
                         <p class = 'tab1'>2.	If more than or equal to C, but less than or equal to D out of the cohort of A patients experience DLT, an additional cohort of B patients will be entered at dose i. <br><br></p>
                         <p class = 'tab2'>a.	If less than or equal to E out of the cohort of a total A+B patients experience DLT, dose escalation is indicated. <br><br></p>
                         <p class = 'tab2'>b.	If more than E out of the cohort of a total A+B patients experience DLT, dose de-escalation is indicated. <br><br></p>
                         <p class = 'tab1'>3.	If more than D out of the cohort of A patients experience DLT, than dose de-escalation is indicated. <br><br></p>
                         When dose de-escalation is indicated:<br><br>
                         <p class = 'tab1'>1.	If A+B patient has already been treated at dose i-1, which means less than or equal to E out of the cohort of a total A+B patients experience DLT at dose i-1, dose i-1 will be deemed MTD.<br><br></p>
                         <p class = 'tab1'>2.	If not, an additional cohort of B patients will be entered at dose i-1. <br><br></p>
                         <p class = 'tab2'>a.	If less than or equal to E out of the cohort of a total A+B patients experience DLT, dose i-1 will be deemed MTD. <br><br></p>
                         <p class = 'tab2'>b.	If more than E out of the cohort of a total A+B patients experience DLT, further dose de-escalation is indicated. <br><br></p>
                         <p class = 'tab1'>3.	Dose de-escalation will be continued to dose i-2 or so on if indicated until dose 1.<br><br></p>
                         "
                       )
                     )
                   )
                 )
               )                
              )
            )
          )
        )
      )
    )
  )
))
