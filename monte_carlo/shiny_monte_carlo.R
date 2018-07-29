#setwd("R:/general/ForMichael/r_wsp/shiny/monte_carlo")
library(shiny)
library(tibble)
library(tidyverse)

source("gain_est_function.R")
source("tx_matching_est_function.R")
source("coupling_est_function.R")


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(  
      
      sliderInput(inputId = "num",
                               label = "Choose the number of horn samples to simulate",
                               value = 10, min = 1, max = 100),
      
      checkboxGroupInput("parameters", "Parameters to show:",
                         c("Gain_0deg" = "Gain_0deg",
                           "Tx_matching" = "Tx_matching",
                           "Coupling" = "Coupling"), 
                         selected = c("Gain_0deg","Tx_matching", "Coupling")
                         ),
      
      
      actionButton(inputId = "update", label = "Generate/Update Table"),
      # checkboxGroupInput("results", label="Choose results to display:",
      #                    choices= c('Gain_0deg'='gain','Tx_matching'='tx_match', 'Coupling' = 'coupling'),
      #                    select = c('gain','tx_match', 'coupling')),
      textInput("name", "Enter the name you wish to save the file as"),
      textInput("dir", "Enter the filepath of the location where you would like to save the output"),
      #downloadButton("export_data", label = "Export Table"),
      actionButton("save_data", label = "Save Data")
      
                   ),
    mainPanel(tableOutput("sim"))
  
  )
  
)

server <- function(input, output) {
  
  sim_Data <- function(){
    
    horn_scaling <- 
      lapply(1:input$num, function(x) {
        rnorm(1, mean = 0, sd = 0.5)
      } )
    
    horn_x_pos <- 
      lapply(1:input$num, function(x) {
        rnorm(1, mean = 0, sd = 0.5)
      } )
    
    horn_y_pos <- 
      lapply(1:input$num, function(x) {
        rnorm(1, mean = 0, sd = 0.5)
      } )
    
    results_sim <- do.call("rbind", Map('c', horn_scaling, horn_x_pos, horn_y_pos)) %>%
      as.data.frame() %>%
      round(digits = 3) %>%
      rename(horn_scaling = V1, horn_x_pos = V2, horn_y_pos = V3)
    
    gain <- mapply(gain_est, results_sim$horn_scaling, results_sim$horn_x_pos, results_sim$horn_y_pos) %>%
      as.data.frame() %>% `colnames<-`(c("Gain_0deg"))
    
    tx_match <- mapply(tx_matching_est, results_sim$horn_scaling, results_sim$horn_x_pos, results_sim$horn_y_pos) %>%
      as.data.frame() %>% `colnames<-`(c("Tx_matching"))
    
    coupling <- mapply(coupling_est, results_sim$horn_scaling, results_sim$horn_x_pos, results_sim$horn_y_pos) %>%
      as.data.frame() %>% `colnames<-`(c("Coupling"))
    
    finaltable <- cbind(results_sim, gain, tx_match, coupling) 
    return(finaltable)
    
  }
  
  rv <- reactiveValues()
  
  observeEvent(input$update, {
    rv$data <- sim_Data()
    rv$data[input$parameters]
    })

  output$sim <- renderTable({rv$data[input$parameters]})
  
  
 
  # output$export_data <- downloadHandler(
  #   filename = function() {
  #     paste("sim_", nrow(simTable()), "_hornsamples", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(simTable(), file)
  #   }
  # )
  
  observeEvent( input$save_data, {
    write.csv(simTable(), file = file.path(input$dir,paste(input$name ,".csv", sep = "")))
  
  })
  

}


shinyApp(ui = ui, server = server)