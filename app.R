#ElemMatrix
#September 4, 2020
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
# library(matlib)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = HTML("Matrix Multiplication in Z<sub>3</sub>"),
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),
    fluidPage(stylesheet,
              
      column(width=12,
        h3("Elementary matrices"),
        uiOutput("mat1"),
      ),
      fluidRow(
        column(width = 4,
             radioButtons("rb", "Choose a conformal matrix:",
                          choiceNames = list(
                            uiOutput("mat_1_o"),
                            uiOutput("mat_2_o"),
                            uiOutput("mat_3_o"),
                            uiOutput("mat_4_o"),
                            uiOutput("mat_5_o"),
                            uiOutput("mat_6_o"),
                            uiOutput("mat_7_o"),
                            uiOutput("mat_8_o"),
                            uiOutput("mat_9_o")
                          ),
                          choiceValues = list(
                            "mat_1_o",
                            "mat_2_o",
                            "mat_3_o",
                            "mat_4_o",
                            "mat_5_o",
                            "mat_6_o",
                            "mat_7_o",
                            "mat_8_o",
                            "mat_9_o"
                          )),
             ),
        column(4,
               awesomeRadio("op",NULL,c("+","-","*","/"), width = "100px"),
               actionBttn("calc", "Calculate Matrix"),
               uiOutput("result")),
        
        column(width = 4,
               
               radioButtons("rb2", "Choose a conformal matrix:",
                            choiceNames = list(
                              uiOutput("mat_1_r_o"),
                              uiOutput("mat_2_r_o"),
                              uiOutput("mat_3_r_o"),
                              uiOutput("mat_4_r_o"),
                              uiOutput("mat_5_r_o"),
                              uiOutput("mat_6_r_o"),
                              uiOutput("mat_7_r_o"),
                              uiOutput("mat_8_r_o"),
                              uiOutput("mat_9_r_o")
                            ),
                            choiceValues = list(
                              "mat_1_r_o",
                              "mat_2_r_o",
                              "mat_3_r_o",
                              "mat_4_r_o",
                              "mat_5_r_o",
                              "mat_6_r_o",
                              "mat_7_r_o",
                              "mat_8_r_o",
                              "mat_9_r_o"
                            )),
               
               # radioButtons("rb2", "Choose another conformal matrix:",
               #              choiceNames = list(
               #                uiOutput("rmatrixA"),
               #                uiOutput("rmatrixB"),
               #                uiOutput("rmatrixC"),
               #                uiOutput("rmatrixD"),
               #                uiOutput("rmatrixE"),
               #                uiOutput("rmatrixF"),
               #                uiOutput("rmatrixG"),
               #                uiOutput("rmatrixH"),
               #                uiOutput("rmatrixI")
               #            ),
               #            choiceValues = list(
               #              "rmatrixA", "rmatrixB", "rmatrixC","rmatrixD","rmatrixE",
               #              "rmatrixF","rmatrixG","rmatrixH","rmatrixI"
               #            )),
      )),
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images or stylesheet must go into a www subfolder

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
    #Variables that are shared among server functions (use <<-)
    mat_1 <<- matrix(c(1,2,1,1), nrow=2,ncol=2,byrow = TRUE)
    mat_2 <<- matrix(c(0,1,2,0), nrow=2,ncol=2,byrow = TRUE)
    mat_3 <<- matrix(c(1,1,2,1), nrow=2,ncol=2,byrow = TRUE)
    mat_4 <<- matrix(c(2,0,0,2), nrow=2,ncol=2,byrow = TRUE)
    mat_5 <<- matrix(c(2,1,2,2), nrow=2,ncol=2,byrow = TRUE)
    mat_6 <<- matrix(c(0,2,1,0), nrow=2,ncol=2,byrow = TRUE)
    mat_7 <<- matrix(c(2,2,1,2), nrow=2,ncol=2,byrow = TRUE)
    mat_8 <<- matrix(c(1,0,0,1), nrow=2,ncol=2,byrow = TRUE)
    mat_9 <<- matrix(c(0,0,0,0), nrow=2,ncol=2,byrow = TRUE)
    
    output$mat_1_o <- renderUI(jax.matrix(mat_1))
    output$mat_2_o <- renderUI(jax.matrix(mat_2))
    output$mat_3_o <- renderUI(jax.matrix(mat_3))
    output$mat_4_o <- renderUI(jax.matrix(mat_4))
    output$mat_5_o <- renderUI(jax.matrix(mat_5))
    output$mat_6_o <- renderUI(jax.matrix(mat_6))
    output$mat_7_o <- renderUI(jax.matrix(mat_7))
    output$mat_8_o <- renderUI(jax.matrix(mat_8))
    output$mat_9_o <- renderUI(jax.matrix(mat_9))
    
    output$mat_1_r_o <- renderUI(jax.matrix(mat_1))
    output$mat_2_r_o <- renderUI(jax.matrix(mat_2))
    output$mat_3_r_o <- renderUI(jax.matrix(mat_3))
    output$mat_4_r_o <- renderUI(jax.matrix(mat_4))
    output$mat_5_r_o <- renderUI(jax.matrix(mat_5))
    output$mat_6_r_o <- renderUI(jax.matrix(mat_6))
    output$mat_7_r_o <- renderUI(jax.matrix(mat_7))
    output$mat_8_r_o <- renderUI(jax.matrix(mat_8))
    output$mat_9_r_o <- renderUI(jax.matrix(mat_9))
    
    #Functions that respond to events in the input
    observeEvent(input$calc,{
      matrix1 <<- input$rb
      matrix2 <<- input$rb2
      
      x <- switch(matrix1,
                  "mat_1_o" = mat_1,
                  "mat_2_o" = mat_2,
                  "mat_3_o" = mat_3,
                  "mat_4_o" = mat_4,
                  "mat_5_o" = mat_5,
                  "mat_6_o" = mat_6,
                  "mat_7_o" = mat_7,
                  "mat_8_o" = mat_8,
                  "mat_9_o" = mat_9
      )
      
      y <- switch(matrix2,
                  "mat_1_r_o" = mat_1,
                  "mat_2_r_o" = mat_2,
                  "mat_3_r_o" = mat_3,
                  "mat_4_r_o" = mat_4,
                  "mat_5_r_o" = mat_5,
                  "mat_6_r_o" = mat_6,
                  "mat_7_r_o" = mat_7,
                  "mat_8_r_o" = mat_8,
                  "mat_9_r_o" = mat_9
      )
      if(input$op == "*") {
        output$result <- renderUI(jax.matrix((x%*%y)%%3))
      }
      if(input$op == "/") {
        if(matrix2 == "rmatrixI") {
          showModal(modalDialog(title = "Can't Divide",id = "div",
                                h3("Can't Divide by zero")))
          return()
        }
        
        inverseM <- (solve(y))%%3
        cat(inverseM)
        output$result <- renderUI(jax.matrix((x%*%inverseM)%%3))
      }
      if(input$op == "+") {
        output$result <- renderUI(jax.matrix((x+y)%%3))
      }
      if(input$op == "-") {
        output$result <- renderUI(jax.matrix((x-y)%%3))
      }
      
    })
}

#Run the app
shinyApp(ui = ui, server = server)