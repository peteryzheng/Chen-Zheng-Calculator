library(shiny)
library(ggplot2)
library(shinyjs)
onSessionEnded = function(callback) {
  "Registers the given callback to be invoked when the session is closed
  (i.e. the connection to the client has been severed). The return value
  is a function which unregisters the callback. If multiple callbacks are
  registered, the order in which they are invoked is not guaranteed."
  return(.closedCallbacks$register(callback))
}
`%then%` <- shiny:::`%OR%`

shinyServer(
  function(input,output){
    #session$onSessionEnded(function() {
    #  stopApp()
    #})
    doseNumber <- reactive({
      validate(
        need(try(input$doseNumber != ""), "Dose Number Invalid") %then%
        need(try(!is.na(as.numeric(input$doseNumber))), "Dose Number NOT a Number") %then%
        need(try(!as.numeric(input$doseNumber) == 0), "Dose Number CANNOT Equal to 0")
      )
      return(as.numeric(input$doseNumber))
    })
    #################### Dynamic Input pages #################### 
    useShinyjs()
    rv <- reactiveValues(page = 1)
    observe({
      toggleState(id = "prevBtn", condition = rv$page > 1)
      toggleState(id = "nextBtn", condition = rv$page < 2)
      hide(selector = ".page")
      show(sprintf("step%s", rv$page))
    })
    observe({
      shinyjs::toggleState("nextBtn", condition = !is.null(input$doseNumber) && input$doseNumber != "" && as.numeric(input$doseNumber) >= 1 && !is.na(as.numeric(input$doseNumber)))
    })
    observe({
      shinyjs::toggleState("calculate",condition = checkProb())
    })
    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }
    observeEvent(input$prevBtn, navPage(-1))
    observeEvent(input$nextBtn, navPage(1))
    
    output$page1 <- renderUI({
      tagList(
        h2("Step 1"),
        h3("There are two subtypes of standard 3+3 designs: with dose de-escalation vs. without dose de-escalation."),
        br(),
        tags$div(class = "widgets",radioButtons("de-escalation", "Please check one of the subtypes below you want to use:", c(Without = '0',With = '1'),selected = '0')),
        tags$div(class = "widgets",textInput("doseNumber","Please enter the total number of dose levels to be tested in the trial:","6",placeholder = "Enter an integer")),
        textOutput("errorMessage")
      )
    })
    output$page2 <- renderUI({
      tagList(
        h2("Step 2"),
        h3("Please Enter the True Probability of Dose Limiting Toxicity and dosages (optional) for each Dose Level Below:"),
        br(),
        uiOutput("dynamicInputs"),
        h4("Please note: All Doses have to be entered for calculation"),
        actionButton("calculate","Calculate", style="color: #004990")
      )
    })
    #output$page4 <- renderUI({
    #  tagList(
    #    h2(sprintf("Step %s", rv$page)),
    #    "Parameters",
    #    br(),
    #    fluidRow(
    #      column(6,textInput("a","Enter A: ", "3",width = "200px",placeholder = "Enter an integer")),
    #      column(6,textInput("b","Enter B: ", "3",width = "200px",placeholder = "Enter an integer")) 
    #    ),
    #    fluidRow(
    #      column(6,textInput("c","Enter C: ", "1",width = "200px",placeholder = "Enter an integer")),
    #      column(6,textInput("d","Enter D: ", "1",width = "200px",placeholder = "Enter an integer")) 
    #    ),
    #    fluidRow(
    #      column(6,textInput("e","Enter E: ", "1",width = "200px",placeholder = "Enter an integer"))
    #    ),
    #    actionButton("calculate","Calculate")
    #  )
    #})
    output$dynamicInputs <- renderUI({
      temp <- as.numeric(doseNumber())
      lapply(1:temp, function(i) {
          fluidRow(
            tags$div(class = "widgets",column(6,textInput(paste0("Dose",i),paste(paste0("Dose ",i),":", sep = " "),placeholder = "Enter data: "))),
            tags$div(class = "widgets",column(6,textInput(paste0("Dosage",i),paste(paste0("Dosage ",i),":", sep = " "),placeholder = "Enter data: ")))
            
          ) 
      })
    })

    checkProb <- reactive({
      
      temp <- TRUE
      count = doseNumber()
      for(i in 1:count){
        if(is.null(input[[paste0("Dose",i)]])){
          temp = TRUE
        }
        else{
          if (input[[paste0("Dose",i)]] == "") {
            temp = FALSE
          }
          else if(as.numeric(input[[paste0("Dose",i)]]) <= 0){
            temp = FALSE
          }
        }
      }
      return(temp)
    })
    output$errorMessage <- renderText({
      if (checkProb() == TRUE){
        return("")
      }
    })
    

    #################### INPUT FOR PROBABILITY OF TOXICITY #################### 
    dataProbabilities <- reactive({
      temp <- doseNumber()
      print(
        data.frame(lapply(1:temp, function(i) {
          as.numeric(input[[paste0("Dose",i)]])
        }))
      )
      data.frame(lapply(1:temp, function(i) {
        as.numeric(input[[paste0("Dose",i)]])
      }))
    }) #reactive function for reading table
    
    output$dataProbabilities <- renderTable({
      if (is.null(input$calculate) ){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(dataProbabilities())){return()}
            else{
              tempTable <- data.frame(t(dataProbabilities()))
              tempRowNames <- c(1:length(tempTable))
              for (i in 1:length(tempTable[,1])){
                tempRowNames[i] <- paste0("Dose",i)
              }
              rownames(tempTable) <- tempRowNames
              colnames(tempTable) <- "Probabilities of Dose Limiting Toxicity"
              return(tempTable)
            }
          )
      }
    },
    rownames = TRUE
    ) #isolated processing of file data
    
    dataDosage <- reactive({
      temp <- doseNumber()
      data.frame(lapply(1:temp, function(i) {
        as.numeric(input[[paste0("Dosage",i)]])
      }))
    })
    
    output$dataDosage <- renderTable({
      if (is.null(input$calculate) ){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(dataProbabilities())){return()}
            else{
              tempTable <- data.frame(t(dataDosage()))
              tempRowNames <- c(1:length(tempTable))
              for (i in 1:length(tempTable[,1])){
                tempRowNames[i] <- paste0("Dosage",i)
              }
              rownames(tempTable) <- tempRowNames
              colnames(tempTable) <- "Dosage"
              return(tempTable)
            }
            
            
          )
      }
    },
    rownames = TRUE
    ) #isolated processing of file data
    

    #################### DATA OUTPUT #################### 
    deescalation <- reactive({
      parameters <- c(3,3,1,1,1)
      probabilities <- data.frame(dataProbabilities())
      doseConc <- data.frame(dataDosage())
      result <- calculateMTDandPtNum(probability = probabilities,parameter = parameters,dosedeesc = as.numeric(input$`de-escalation`))
      MTD <- result[1,]
      ptNumber <- result[2,]
      toxicity <- calculateIndividualToxicity(ptNum = ptNumber, probabilities = probabilities) #Run method for calculating Individual toxicity
      probabilities <- data.frame(probabilities)
      doseConc <- data.frame(doseConc)
      MTD <- data.frame(MTD)
      ptNumber <- data.frame(ptNumber)
      toxicity <- data.frame(toxicity)
      names(doseConc) <- names(probabilities)
      names(MTD) <- names(probabilities)
      names(ptNumber)<- names(probabilities)
      names(toxicity)<- names(probabilities)
      output <- rbind(probabilities,doseConc,MTD,ptNumber,toxicity)
      output <- data.frame(round(output,digits = 3))
      doseLevel <- as.numeric(1:length(probabilities[1,]))
      colnames(output) <- doseLevel
      row.names(output)<- c("True Probability of Dose Limiting Toxicity","Dosage","Probability of the Dose Level Chosen as MTD","Expected Number of Patients Treated at the Dose Level","Expected Number of Dose Limiting Toxicity at the Dose Level") #Give row names for excel spreadsheet output
      return(output)
    })    
    output$deescalation <- renderTable({
      if(is.null(input$calculate)){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(deescalation())){return()}
            else{
              temp <- deescalation()
              temp <- data.frame(cbind(rownames(temp), temp))
              tempColNames <- c(1:(length(temp[1,])-1))
              ColNames <- c("Dose Level",tempColNames)
              colnames(temp) <- ColNames
              rownames(temp) <- NULL
              return(temp)
            }
          )
      }
    },
    rownames = FALSE
    )
    otherstats <- reactive({
      parameters <- c(3,3,1,1,1)
      probabilities <- data.frame(dataProbabilities())
      doseConc <- data.frame(1:length(probabilities))
      result <- calculateMTDandPtNum(probability = probabilities,parameter = parameters,dosedeesc = as.numeric(input$`de-escalation`))
      MTD <- result[1,]
      ptNumber <- result[2,]
      TTL <- calculateTTL(probMTD = MTD,probabilities = probabilities) #Run method for calculating targeted toxicity level
      toxicity <- calculateIndividualToxicity(ptNum = ptNumber, probabilities = probabilities) #Run method for calculating Individual toxicity
      overallToxicity <- calculateOverallToxicity(ptNum = ptNumber, probabilities = probabilities) #Run method for calculating overall toxicity
      TTLtemp <- data.frame(c(1:(length(probabilities)-1)))
      TTLtemp[1,] <- TTL #Side column 1st item TTL 
      TTLtemp[3,] <- calculateDose0(probability = probabilities,parameter = parameters,dosedeesc = as.numeric(input$`de-escalation`)) # Side column 3rd item dose 0 MTD
      TTLtemp[4,] <- sum(ptNumber) #Side column 4th item total patient number
      TTLtemp[5,] <- overallToxicity #Side column 5th item overall toxicity incidence
      TTLtemp[2,] <- TTLtemp[5,]/TTLtemp[4,] #Side column 2nd item overall toxicity rateTTLtemp[(count - 1)*3+1,1] <- TTL #Side column 1st item TTL
      colnames(TTLtemp) <- "Other Characteristics"#Get rid of ugly column names that don't make sense
      row.names(TTLtemp) <- c("Expected Probability of Dose-limiting Toxicity at MTD","Overall Rate of Dose Limiting Toxicity","Probability of All Dose Levels Being Over Toxic","Expected Overall Number of Patients","Expected Overall Number of Dose Limiting Toxicity")
      TTLtemp <- round(TTLtemp,digits = 3)
      return(TTLtemp)
    })
    output$otherstats <- renderTable({
      if(is.null(input$calculate)){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(otherstats())){return()}
            else
              otherstats()
          )
      }
    },
    rownames = TRUE
    )
    
    #################### GRAPHIC OUTPUT #################### 
    MTDbarplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      appendedtempoutput <- data.frame(TTLtemp[2,],tempoutput[3,])
      samplebarindex <- data.frame(t(c(0:(length(appendedtempoutput)-1))))
      colnames(samplebarindex) <- colnames(appendedtempoutput)
      tempDataFrame <- data.frame(t(rbind(samplebarindex,appendedtempoutput)))
      colnames(tempDataFrame) <- c("Dose_Level", "Probabilities_of_Each_Dose_Chosen_as_MTD")
      return(ggplot(data = tempDataFrame, aes(x = Dose_Level , y = Probabilities_of_Each_Dose_Chosen_as_MTD)) + labs(y = "Probability", x = "Dose Level", title = "Probability of Each Dose Chosen as MTD") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_bar(stat = "identity",fill = "#004990",colour="#004990")+ scale_x_continuous(breaks = c(0:(length(tempoutput)))))
      #png(sprintf( paste(outputdirectory,"barplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Dose_level , y = Probabilities_that_the_dose_is_chosen_as_MTD)) + geom_bar(stat = "identity") + scale_x_continuous(breaks = c(0:(length(probabilities)-1)))
      #print(p)
      #dev.off()
    })
    output$MTDbarplot <- renderPlot({
      if(is.null(input$calculate)){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(MTDbarplot())){return()}
            else
              MTDbarplot()
          )
        
      }
      
    })
    
    MTDscatterplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      tempDataFrame <- data.frame(t(rbind(tempoutput[1,],tempoutput[3,])))
      colnames(tempDataFrame) <- c("Probability_of_Toxicity", "Probability_as_MTD_by_Probability_of_Toxicity")
      return(ggplot(data = tempDataFrame, aes(x = Probability_of_Toxicity , y = Probability_as_MTD_by_Probability_of_Toxicity,group = 1)) + labs(y = "Probability", x = "Probability of Toxicity", title = "Probability as MTD by Probability of Toxicity") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1)))
      #png(sprintf( paste(outputdirectory,"scatterplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Probabilities_that_the_dose_is_chosen_as_MTD,group = 1)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1))
      #print(p)
      #dev.off()
    })
    output$MTDscatterplot <- renderPlot({
      if (is.null(input$calculate)){
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(MTDscatterplot())){return()}
            else
              MTDscatterplot()
          )
      }
      
    })
    
    ptNumbarplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      samplebarindex <- data.frame(t(c(1:length(tempoutput))))
      colnames(samplebarindex) <- colnames(tempoutput)
      tempDataFrame <- data.frame(t(rbind(samplebarindex,tempoutput[4,])))
      colnames(tempDataFrame) <- c("Dose_Level", "Expected_Number_of_Patients_Treated_at_Each_Dose_Level")
      return(ggplot(data = tempDataFrame, aes(x = Dose_Level , y = Expected_Number_of_Patients_Treated_at_Each_Dose_Level,group = 1)) + labs(y = "Number", x = "Dose Level", title = "Expected Number of Patients Treated at Each Dose Level") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_bar(stat = "identity",fill = "#004990",colour="#004990") + scale_x_continuous(breaks = c(1:(length(tempoutput)))))
      #png(sprintf( paste(outputdirectory,"scatterplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Probabilities_that_the_dose_is_chosen_as_MTD,group = 1)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1))
      #print(p)
      #dev.off()
    })
    output$ptNumbarplot <- renderPlot({
      if (is.null(input$calculate)) {
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(ptNumbarplot())){return()}
            else
              ptNumbarplot()
          )
      }
      
    })
    
    ptNumscatterplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      tempDataFrame <- data.frame(t(rbind(tempoutput[1,],tempoutput[4,])))
      colnames(tempDataFrame) <- c("Probability_of_Toxicity", "Expected_Number_of_Patients_by_Probability_of_Toxicity")
      return(ggplot(data = tempDataFrame, aes(x = Probability_of_Toxicity , y = Expected_Number_of_Patients_by_Probability_of_Toxicity,group = 1)) + labs(y = "Number", x = "Probability of Toxicity", title = "Expected Number of Patients by Probability of Toxicity") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1)))
      #png(sprintf( paste(outputdirectory,"scatterplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Probabilities_that_the_dose_is_chosen_as_MTD,group = 1)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1))
      #print(p)
      #dev.off()
    })
    output$ptNumscatterplot <- renderPlot({
      if (is.null(input$calculate)) {
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(ptNumscatterplot())){return()}
            else
              ptNumscatterplot()
          )
      }
      
    })
    
    toxincbarplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      samplebarindex <- data.frame(t(c(1:length(tempoutput))))
      colnames(samplebarindex) <- colnames(tempoutput)
      tempDataFrame <- data.frame(t(rbind(samplebarindex,tempoutput[5,])))
      colnames(tempDataFrame) <- c("Dose_Level", "Expected_Number_of_Dose_Limiting_Toxicity_at_Each_Dose_Level")
      return(ggplot(data = tempDataFrame, aes(x = Dose_Level , y = Expected_Number_of_Dose_Limiting_Toxicity_at_Each_Dose_Level,group = 1)) + labs(y = "Number", x = "Dose Level", title = "Expected Number of Dose Limiting Toxicity at Each Dose Level") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_bar(stat = "identity",fill = "#004990",colour="#004990") + scale_x_continuous(breaks = c(1:(length(tempoutput)))))
      #png(sprintf( paste(outputdirectory,"scatterplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Probabilities_that_the_dose_is_chosen_as_MTD,group = 1)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1))
      #print(p)
      #dev.off()
    })
    output$toxincbarplot <- renderPlot({
      if (is.null(input$calculate)) {
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(toxincbarplot())){return()}
            else
              toxincbarplot()
          )
      }
      
    })
    
    toxinccatterplot <- reactive({
      tempoutput <- deescalation()
      TTLtemp <- otherstats()
      tempDataFrame <- data.frame(t(rbind(tempoutput[1,],tempoutput[5,])))
      colnames(tempDataFrame) <- c("Probability_of_toxicity", "Expected_Number_of_Dose_Limiting_Toxicity_by_Probability_of_Toxicity")
      return(ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Expected_Number_of_Dose_Limiting_Toxicity_by_Probability_of_Toxicity,group = 1)) + labs(y = "Number", x = "Probability of Toxicity", title = "Expected Number of Dose Limiting Toxicity by Probability of Toxicity") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),axis.text = element_text(size = 15),axis.title = element_text(size = 15)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1)))
      #png(sprintf( paste(outputdirectory,"scatterplot ",row.names(tempoutput[(x+2),]),counter,".jpeg",sep = "")))
      #p <- ggplot(data = tempDataFrame, aes(x = Probability_of_toxicity , y = Probabilities_that_the_dose_is_chosen_as_MTD,group = 1)) + geom_point(fill = "#004990",colour="#004990") + geom_line(colour="#004990") + scale_x_continuous(limits = c(0,1))
      #print(p)
      #dev.off()
    })
    output$toxinccatterplot <- renderPlot({
      if (is.null(input$calculate)) {
        return()
      }
      else{
        input$calculate
        if(input$calculate == 0)
          return()
        else
          isolate(
            if(is.null(toxinccatterplot())){return()}
            else
              toxinccatterplot()
          )
      }
      
    })
    
    #################### DOWNLOAD HANDLERS #################### 

    
    output$down <- downloadHandler(
      filename = function(){
        paste("output","pdf",sep = ".")
      },
      content = function(file){
        pdf(file)
        print(MTDbarplot())
        print(MTDscatterplot())
        print(ptNumbarplot())
        print(ptNumscatterplot())
        print(toxincbarplot())
        print(toxinccatterplot())
        dev.off()
      }
    )
    
    output$downMTDbarplot <- downloadHandler(
      filename = function(){
        paste("MTDbarplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(MTDbarplot())
        dev.off()
      }
    )
    output$downMTDscatterplot <- downloadHandler(
      filename = function(){
        paste("MTDscatterplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(MTDscatterplot())
        dev.off()
      }
    )
    output$downptNumbarplot <- downloadHandler(
      filename = function(){
        paste("ptNumbarplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(ptNumbarplot())
        dev.off()
      }
    )
    output$downptNumscatterplot <- downloadHandler(
      filename = function(){
        paste("ptNumscatterplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(ptNumscatterplot())
        dev.off()
      }
    )
    output$downtoxincbarplot <- downloadHandler(
      filename = function(){
        paste("toxincbarplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(toxincbarplot())
        dev.off()
      }
    )
    output$downtoxinccatterplot <- downloadHandler(
      filename = function(){
        paste("toxinccatterplot","png",sep = ".")
      },
      content = function(file){
        png(file)
        print(toxinccatterplot())
        dev.off()
      }
    )
    
    
    output$downFile <- downloadHandler(
      filename = function(){
        paste("output","csv",sep = ".")
      },
      content = function(file){
        temp <- deescalation()
        temp <- data.frame(cbind(rownames(temp), temp))
        tempColNames <- c(1:(length(temp[1,])-1))
        ColNames <- c("Dose Level",tempColNames)
        colnames(temp) <- ColNames
        rownames(temp) <- NULL
        write.csv(temp,file,row.names = FALSE)
        write.table(otherstats(), file, append = TRUE,sep = ',')
      }
    )
  }
)

##### MTD and ptNum calculation #####
calculateMTDandPtNum = function(probability, parameter, dosedeesc){
  lengthOfArrays <- length(probability)
  a <- parameter[1]
  b <- parameter[2]
  c <- parameter[3]
  d <- parameter[4]
  e <- parameter[5]
  ##### Calculate parameters #####
  probMTD <- 1:(lengthOfArrays+1)
  probp0j <- 1:lengthOfArrays
  probp1j <- 1:lengthOfArrays
  probq0j <- 1:lengthOfArrays
  probq1j <- 1:lengthOfArrays
  probq2j <- 1:lengthOfArrays
  for (x in 1:lengthOfArrays){
    ##### calculating p0j #####
    tempcounter = 0
    for (countp0j in 0 : (c-1)){
      tempcounter = tempcounter+((probability[x])^countp0j)*((1-probability[x])^(a-countp0j))*factorial(a)/((factorial(countp0j)*factorial(a-countp0j)))
    }
    probp0j[x] <- tempcounter
    ##### calculating q0j #####
    tempcounter = 0
    firstpart = 0
    secondpart = 0
    for (countq0j in c:d){
      for (innercount in 0:(e-countq0j)){
        firstpart = (probability[x])^countq0j*((1-probability[x])^(a-countq0j))*factorial(a)/(factorial(countq0j)*factorial(a-countq0j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq0j[x] <- tempcounter
    ##### calculating p1j #####
    tempcounter = 0
    for (countp1j in c:d){
      tempcounter = tempcounter + (probability[x])^countp1j*(1-probability[x])^(a-countp1j)*factorial(a)/(factorial(countp1j)*factorial(a-countp1j))
    }
    probp1j[x] <- tempcounter
    ##### Calculate q1j #####
    tempcounter <- 0
    innercount <- 0
    for (countq1j in 0:(c-1)){
      for (innercount in 0:(e-countq1j)){
        firstpart = (probability[x])^countq1j*(1-probability[x])^(a-countq1j)*factorial(a)/(factorial(countq1j)*factorial(a-countq1j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq1j[x] <- tempcounter
    ##### Calculate q2j #####
    tempcounter <- 0
    innercount <- 0
    for (countq2j in 0:(c-1)){
      for (innercount in (e+1-countq2j):b){
        firstpart = (probability[x])^countq2j*(1-probability[x])^(a-countq2j)*factorial(a)/(factorial(countq2j)*factorial(a-countq2j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq2j[x] <- tempcounter
  }
  if(dosedeesc == 1){
    ##### Calculate MTDs #####
    
    for (i in 1:(lengthOfArrays-1)){
      tempprob <- 0
      firstMultiple <- 1
      secondMultiple <- 1
      for (k in (i+1):lengthOfArrays){
        if (i == 1){
          #firstMultiple <- probp0j[1]+probq0j[1]
          firstMultiple <- 1
        }
        else{
          for (j in 1:(i-1)){
            firstMultiple <- firstMultiple * (probp0j[j]+probq0j[j])
          }
        }
        if((k-1) >= (i+1)){
          for (j in (i+1):(k-1)){
            secondMultiple <- secondMultiple * probq2j[j]
          }
        }
        tempprob <- tempprob + (firstMultiple * (probq0j[i] + probq1j[i]) * secondMultiple * (1 - probp0j[k] - probq0j[k]))
      }
      probMTD[i+1] <- tempprob
    }
    ##### dose 1 MTD #####
    dose0counter <- 0
    for (k in 1:lengthOfArrays){
      tempcounter <- 1
      if ((k-1) >= 1){
        for (j in 1 : (k-1)){
          tempcounter <- tempcounter * probq2j[j]
        }
      }
      dose0counter <- dose0counter + tempcounter*(1 - probp0j[k] - probq0j[k])
    }
    probMTD[1] <- dose0counter
    assign("dose0", probMTD[1], envir = .GlobalEnv) #Assign dose 0 value globally
    ##### dose n MTD #####
    tempprob <- 1
    temp <- 1
    for (temp in 1:lengthOfArrays){
      tempprob <- tempprob * (probp0j[temp]+probq0j[temp])
    }
    probMTD[lengthOfArrays+1] <- tempprob
    ##### OFFSET OF REGULAR DOSE BECAUSE DOSE 0 #####
    
    ##### Caculate pt num #####
    ptNum <- 1:lengthOfArrays
    for (j in 1:lengthOfArrays){
      tempNum <- 0
      for (i in 0:(lengthOfArrays-1)){
        for (k in (i+1):lengthOfArrays){
          tempNumDose <- 0
          if (j < i){
            tempNumDose <- (a*probp0j[j]+(a+b)*probq0j[j])/(probp0j[j]+probq0j[j])
          }
          else if ((j >= i) && (j < k)){
            tempNumDose <- a+b
          }
          else if (j == k){
            tempNumDose <- (a*(1-probp0j[j]-probp1j[j])+(a+b)*(probp1j[j]-probq0j[j]))/(1-probp0j[j]-probq0j[j])
          }
          else{
            tempNumDose <- 0
          }
          tempprob <- 0
          firstMultiple <- 1
          secondMultiple <- 1
          if ((i == 1) || (i == 0)){
            firstMultiple <- 1
          }
          else{
            for (firstCount in 1:(i-1)){
              firstMultiple <- firstMultiple * (probp0j[firstCount]+probq0j[firstCount])
            }
          }
          if((k-1) >= (i+1)){
            for (secondCount in (i+1):(k-1)){
              secondMultiple <- secondMultiple * probq2j[secondCount]
            }
          }
          if (i == 0){
            tempprob <- secondMultiple * (1 - probp0j[k] - probq0j[k])
            
          }
          else{
            tempprob <- firstMultiple * (probq0j[i] + probq1j[i]) * secondMultiple * (1 - probp0j[k] - probq0j[k])
          }
          tempNum <- tempNum + tempNumDose * tempprob
        }
      }
      tempNum <- tempNum + ((a*probp0j[j]+(a+b)*probq0j[j])/(probp0j[j]+probq0j[j])) * probMTD[lengthOfArrays+1]
      ptNum[j] <- tempNum
    }
  }
  else{
    ##### Calculate MTDs #####
    
    for (i in 1:(lengthOfArrays-1)){
      tempprob <- 1-probp0j[i+1]-probq0j[i+1]
      temp <- 1
      for (temp in 1:i){
        tempprob <- tempprob * (probp0j[temp] + probq0j[temp])
      }
      probMTD[i+1] <- tempprob
    }
    ##### dose 1 MTD #####
    probMTD[1] <- 1 - probp0j[1] - probq0j[1]
    assign("dose0", probMTD[1], envir = .GlobalEnv) #Assign dose 0 value globally
    ##### dose n MTD #####
    tempprob <- 1
    temp <- 1
    for (temp in 1:lengthOfArrays){
      tempprob <- tempprob * (probp0j[temp]+probq0j[temp])
    }
    probMTD[lengthOfArrays+1] <- tempprob
    ##### OFFSET OF REGULAR DOSE BECAUSE DOSE 0 #####
    
    
    ##### Caculate pt num #####
    ptNum <- 1:lengthOfArrays
    for (j in 1:lengthOfArrays){
      tempNum <- 0
      for (i in 0:lengthOfArrays){
        tempNumDose <- 0
        if (j <= i){
          tempNumDose <- (a*probp0j[[j]]+(a+b)*probq0j[j])/(probp0j[j]+probq0j[j])
        }
        else if (j == i+1){
          tempNumDose <- (a*(1-probp0j[j]-probp1j[[j]])+(a+b)*(probp1j[[j]]-probq0j[j]))/(1-probp0j[j]-probq0j[j])
        }
        else{
          tempNumDose <- 0
        }
        tempNum <- tempNum + tempNumDose * probMTD[i+1]
      }
      ptNum[j] <- tempNum
    }
  }
  ##### Return value #####
  output = data.frame(matrix(nrow = 2,ncol = lengthOfArrays))
  output[1,] <- probMTD[2:(lengthOfArrays+1)]
  output[2,] <- ptNum[1:lengthOfArrays]
  return(output) #  OUTPUT DATA FRAME OF TWO ROWS: FIRST ROW MTD; SECOND ROW PTNUM
  
}

##### Calculate TTL #####
calculateTTL = function (probMTD,probabilities){
  lengthOfArray <- length(probMTD)
  temp1 <- 0 #toxicity of dose i when dose i is MTD
  temp2 <- 0 #when dose i is MTD
  for (i in 1:(lengthOfArray-1)){
    temp1 <- temp1 + probabilities[i]*probMTD[1,i]
    temp2 <- temp2 + probMTD[1,i]
  }
  TTL <- temp1/temp2
  return(TTL)
}

##### Calculate expected individual level toxicity #####
calculateIndividualToxicity = function (probabilities, ptNum){
  lengthOfArray <- length(probabilities)
  toxicity <- 1:lengthOfArray
  for (i in 1:lengthOfArray){
    toxicity[i] <- probabilities[i]*ptNum[i]
  }
  return(toxicity)
}

##### Calculate expected overall toxicity #####
calculateOverallToxicity = function (probabilities, ptNum){
  overallNum <- 0
  lengthOfArray <- length(probabilities)
  for (i in 1:lengthOfArray){
    overallNum <- overallNum + probabilities[i]*ptNum[i]
  }
  return(overallNum)
}

##### Calculate Dose 0 #####
calculateDose0 = function(probability, parameter, dosedeesc){
  lengthOfArrays <- length(probability)
  a <- parameter[1]
  b <- parameter[2]
  c <- parameter[3]
  d <- parameter[4]
  e <- parameter[5]
  ##### Calculate parameters #####
  probMTD <- 1:(lengthOfArrays+1)
  probp0j <- 1:lengthOfArrays
  probp1j <- 1:lengthOfArrays
  probq0j <- 1:lengthOfArrays
  probq1j <- 1:lengthOfArrays
  probq2j <- 1:lengthOfArrays
  for (x in 1:lengthOfArrays){
    ##### calculating p0j #####
    tempcounter = 0
    for (countp0j in 0 : (c-1)){
      tempcounter = tempcounter+((probability[x])^countp0j)*((1-probability[x])^(a-countp0j))*factorial(a)/((factorial(countp0j)*factorial(a-countp0j)))
    }
    probp0j[x] <- tempcounter
    ##### calculating q0j #####
    tempcounter = 0
    firstpart = 0
    secondpart = 0
    for (countq0j in c:d){
      for (innercount in 0:(e-countq0j)){
        firstpart = (probability[x])^countq0j*((1-probability[x])^(a-countq0j))*factorial(a)/(factorial(countq0j)*factorial(a-countq0j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq0j[x] <- tempcounter
    ##### calculating p1j #####
    tempcounter = 0
    for (countp1j in c:d){
      tempcounter = tempcounter + (probability[x])^countp1j*(1-probability[x])^(a-countp1j)*factorial(a)/(factorial(countp1j)*factorial(a-countp1j))
    }
    probp1j[x] <- tempcounter
    ##### Calculate q1j #####
    tempcounter <- 0
    innercount <- 0
    for (countq1j in 0:(c-1)){
      for (innercount in 0:(e-countq1j)){
        firstpart = (probability[x])^countq1j*(1-probability[x])^(a-countq1j)*factorial(a)/(factorial(countq1j)*factorial(a-countq1j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq1j[x] <- tempcounter
    ##### Calculate q2j #####
    tempcounter <- 0
    innercount <- 0
    for (countq2j in 0:(c-1)){
      for (innercount in (e+1-countq2j):b){
        firstpart = (probability[x])^countq2j*(1-probability[x])^(a-countq2j)*factorial(a)/(factorial(countq2j)*factorial(a-countq2j))
        secondpart = (probability[x])^innercount*(1-probability[x])^(b-innercount)*factorial(b)/(factorial(innercount)*factorial(b-innercount))
        tempcounter = tempcounter + firstpart * secondpart
      }
    }
    probq2j[x] <- tempcounter
  }
  if (dosedeesc == 1){
    dose0counter <- 0
    for (k in 1:lengthOfArrays){
      tempcounter <- 1
      if ((k-1) >= 1){
        for (j in 1 : (k-1)){
          tempcounter <- tempcounter * probq2j[j]
        }
      }
      dose0counter <- dose0counter + tempcounter*(1 - probp0j[k] - probq0j[k])
    }
    return(dose0counter)
  }
  else{
    dose0counter <- 1 - probp0j[1] - probq0j[1]
    return(dose0counter)
  }
}