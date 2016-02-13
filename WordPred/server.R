library(shiny)
source("wordPredict.R")

#Shiny server
shinyServer(function(input, output) {
    
    output$text1 <- renderText({
        input$textInput
    })
    
    predictedWords <- reactive({predictThis(input$textInput)})
    
    output$result1 <- renderText({
        #input$textInput
        predictedWords()[1]
    })
    
    #n <- length(predictedWords())
    output$result2 <- renderText({
        #input$textInput
        predictedWords()[-1]
    })
    
})