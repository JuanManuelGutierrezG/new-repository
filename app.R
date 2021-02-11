# Install requiered packages ###################
list_of_packages <- c("shiny","xlsx","deSolve","GA","FME","tidyverse","shinybusy","shinyWidgets")
new_packages <- list_of_packages[!(list_of_packages %in%  installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries ################################
library(shiny)
library(xlsx)
library(deSolve)
library(GA)
library(FME)
library(tidyverse)
library(shinybusy)
library( shinyWidgets)
library(ggplot2)

# Source script with functions ####################
source("functions.R")

# user interface ##################################
ui <- fluidPage(
        add_busy_spinner(spin = "fading-circle"),
        withMathJax(), # For LaTex code
        
        sidebarLayout(
                
                sidebarPanel(
                        
                        fluidRow(
                                
                                column(6,
                                       fileInput(inputId = "file_int_opt",label = "Enter file",accept = c(".xlsx"))
                                       
                                       )
                        ),
                        
                        fluidRow(
                                
                                column(6,
                                       checkboxInput(inputId = "head",label = "Head", value = TRUE)
                                       ),
                                column(6,
                                       selectInput(inputId = "sheet_int_opt",label = "Select sheet",choices = list(1,2,3,4), selected = 1)
                                )
                        ),
                        hr(),
                        
                        selectInput("mod_int","Choose model", choices = list("Model 1 (Monod without inhibition by product)"="model1",
                                                                             "Model 2 (Monod with inhibition by product)" = "model2",
                                                                             "Model 3 (Monod with product partially linked to growth)"="model3",
                                                                             "Model 4 (Monod with cell death)"="model4",
                                                                             "Model 5 (Monod with sustrate consumption for maintenance)"="model5",
                                                                             "Model 6 (Monod with inhibition by product and product partially linked to growth)"="model6"),
                                    selected = "model1"),
                        
                        # Model 1 conditional panel ####################################
                        conditionalPanel(condition = "output.parms_ui_mod1_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod1_name_int_opt",label = "",choices = "Vmax", selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod1_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod1_range_int_opt",label = "",
                                                                            value = c(0, 3))
                                                         )
                                                         
                                                         
                                                 )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod1_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod1_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod1_range_int_opt",label = "",
                                                                        value = c(0, 400))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod1_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "yxs_mod1_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod1_range_int_opt",label = "",
                                                                        value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ypx_mod1_name_int_opt",label = "",choices = "Ypx",selected = "Ypx")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ypx_mod1_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ypx_mod1_range_int_opt",label = "",
                                                                        value = c(0, 20))
                                                     )
                                             ))
                                         
                        ),
                        
                        # Model 2 conditional panel #################################################
                        conditionalPanel(condition = "output.parms_ui_mod2_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod2_name_int_opt",label = "",choices = "Vmax",selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod2_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod2_range_int_opt",label = "",
                                                                            value = c(0, 3))
                                                         )
                                                 )),
                                         
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod2_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod2_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod2_range_int_opt",label = "",
                                                                              value = c(0, 400))
                                                     )
                                             )),
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod2_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "yxs_mod2_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod2_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ypx_mod2_name_int_opt",label = "",choices = "Ypx",selected = "Ypx")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ypx_mod2_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ypx_mod2_range_int_opt",label = "",
                                                                              value = c(0, 20))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "kp_mod2_name_int_opt",label = "",choices = "Kp",selected = "Kp")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "kp_mod2_val_int_opt",label = "",value = 80)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "kp_mod2_range_int_opt",label = "",
                                                                              value = c(0, 200))
                                                     )
                                             ))
                                         
                        ),
                        
                        # Model 3 conditional panel #################################################
                        conditionalPanel(condition = "output.parms_ui_mod3_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod3_name_int_opt",label = "",choices = "Vmax",selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod3_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod3_range_int_opt",label = "",
                                                                                  value = c(0, 3))
                                                         )
                                                 )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod3_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod3_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod3_range_int_opt",label = "",
                                                                              value = c(0, 400))
                                                     )
                                             )),
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod3_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "yxs_mod3_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod3_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "alpha_mod3_name_int_opt",label = "",choices = "alpha",selected = "alpha")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "alpha_mod3_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "alpha_mod3_range_int_opt",label = "",
                                                                              value = c(0, 20))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "beta_mod3_name_int_opt",label = "",choices = "beta",selected = "beta")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "beta_mod3_val_int_opt",label = "",value = 0.01)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "beta_mod3_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             ))
                                         
                        ),
                        
                        # Model 4 conditional panel #################################################
                        conditionalPanel(condition = "output.parms_ui_mod4_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod4_name_int_opt",label = "",choices = "Vmax",selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod4_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod4_range_int_opt",label = "",
                                                                                  value = c(0, 3))
                                                         )
                                                        
                                                 )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod4_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod4_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod4_range_int_opt",label = "",
                                                                              value = c(0, 400))
                                                     )
                                             )),
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod4_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "yxs_mod4_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod4_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ypx_mod4_name_int_opt",label = "",choices = "Ypx",selected = "Ypx")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ypx_mod4_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ypx_mod4_range_int_opt",label = "",
                                                                              value = c(0, 20))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "kd_mod4_name_int_opt",label = "",choices = "Kd",selected = "Kd")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "kd_mod4_val_int_opt",label = "",value = 0.01)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "kd_mod4_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             ))
                                         
                        ),
                        
                        # Model 5 conditional panel #################################################
                        conditionalPanel(condition = "output.parms_ui_mod5_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod5_name_int_opt",label = "",choices = "Vmax",selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod5_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod5_range_int_opt",label = "",
                                                                                  value = c(0, 3))
                                                         )
                                                 )),
                                         
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod5_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod5_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod5_range_int_opt",label = "",
                                                                              value = c(0, 400))
                                                     )
                                             )),
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod5_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "yxs_mod5_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod5_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ypx_mod5_name_int_opt",label = "",choices = "Ypx",selected = "Ypx")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "ypx_mod5_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ypx_mod5_range_int_opt",label = "",
                                                                              value = c(0, 20))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "km_mod5_name_int_opt",label = "",choices = "Km",selected = "Km")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "km_mod5_val_int_opt",label = "",value = 0.01)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "km_mod5_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             ))
                                         
                        ),
                        
                        # Model 6 conditional panel #################################################
                        conditionalPanel(condition = "output.parms_ui_mod6_opt",
                                         
                                         h5("Model Parameters"),
                                         
                                         div(
                                                 fluidRow(
                                                         
                                                         column(2,
                                                                checkboxGroupInput(inputId = "vmax_mod6_name_int_opt",label = "",choices = "Vmax",selected = "Vmax")  
                                                         ),
                                                         column(3, offset = 1,
                                                                numericInput(inputId = "vmax_mod6_val_int_opt",label = "",value = 1.2)
                                                         ),
                                                         column(6,
                                                                numericRangeInput(inputId = "vmax_mod6_range_int_opt",label = "",
                                                                                  value = c(0, 3))
                                                         )
                                                 )),
                                         
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "ks_mod6_name_int_opt",label = "",choices = "Ks",selected = "Ks")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "ks_mod6_val_int_opt",label = "",value = 280)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "ks_mod6_range_int_opt",label = "",
                                                                              value = c(0, 400))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "yxs_mod6_name_int_opt",label = "",choices = "Yxs",selected = "Yxs")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "yxs_mod6_val_int_opt",label = "",value = 0.2)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "yxs_mod6_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "alpha_mod6_name_int_opt",label = "",choices = "alpha",selected = "alpha")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "alpha_mod6_val_int_opt",label = "",value = 4)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "alpha_mod6_range_int_opt",label = "",
                                                                              value = c(0, 20))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "kp_mod6_name_int_opt",label = "",choices = "Kp",selected = "Kp")  
                                                     ),
                                                     column(3, offset = 1,
                                                            numericInput(inputId = "kp_mod6_val_int_opt",label = "",value = 80)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "kp_mod6_range_int_opt",label = "",
                                                                              value = c(0, 200))
                                                     )
                                             )),
                                         
                                         div(style = "margin-top:-2em",
                                             fluidRow(
                                                     
                                                     column(2,
                                                            checkboxGroupInput(inputId = "beta_mod6_name_int_opt",label = "",choices = "beta",selected = "beta")  
                                                     ),
                                                     column(3, offset = 1, 
                                                            numericInput(inputId = "beta_mod6_val_int_opt",label = "",value = 0.01)
                                                     ),
                                                     column(6,
                                                            numericRangeInput(inputId = "beta_mod6_range_int_opt",label = "",
                                                                              value = c(0, 1))
                                                     )
                                             ))
                                         
                        ),
                        
                        # More constant widgets ####
                        actionButton("make_opt_int_opt","Make optimization"),
                        hr(),
                        fluidRow(
                                column(5,
                                       downloadButton(outputId = "down_data_plot_opt",label = "Download data plot"),
                                       hr(),
                                       downloadButton(outputId = "down_op_opt",label = "Download parameters")),
                                column(5, offset = 1,
                                       downloadButton(outputId = "down_comparison_plot_opt",label = "Download comparison"),
                                       hr(),
                                       downloadButton(outputId = "down_ga_opt",label = "Download GA output"))
                        )
                        
                ),
                
                # Outputs #################################################################################
                mainPanel(
                        fluidRow(style = "color:black; font-style:bold; font-size:12px; 
                                                        margin-bottom: -3em; text-align:center",
                                
                                h3("Data"),
                                hr(),
                                
                                column(3, offset = 1,
                                       tableOutput("table_data_out_opt")
                                       ),
                                column(6, offset = 2,
                                       plotOutput("plot_data_out_opt",height = 300)
                                       
                                       )
                        ),
                        fluidRow(
                                
                                column(3, offset = 1,
                                       h3("Optimized parameters"),
                                       hr(),
                                       tableOutput("table_result_out_opt")
                                       ),
                                column(6, offset = 2,
                                       h3("Comparison"),
                                       hr(),
                                plotOutput("comp_out_opt",height = 300)
                                       )
                                
                        ),
                        
                        fluidRow(style = "color:black; font-style:bold; font-size:12px; 
                                                        margin-bottom: -3em; text-align:center",
                                 
                                 h3("Genetic algorithm output"),
                                 hr(),
                                column(5,
                                       verbatimTextOutput("ga_summary_out_opt")),
                                column(6, offset = 1,
                                       plotOutput("ga_plot_out_opt"))
                        )
                        
                )
        )
        
        
)

# server section #################################################
server <- function(input, output, session) {
        
        # Reactive values ########################################
        # Save parameters's values 
        p_val_rec_opt <- reactiveValues(rec_val = numeric())
        
        # Save parameters's names to be optimized
        p_name_rec_opt <- reactiveValues(rec_val = character())
        
        # Save parameters's ranges to be optimized
        p_range_rec_opt <- reactiveValues(rec_val = list())
        
        # Load model ####
        observe({
                
                # Load model 1 ###########################################################
                if (input$mod_int == "model1") { 
                        
                source("model1.R")

                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod1_val_int_opt, Ks =  input$ks_mod1_val_int_opt,
                                                   Yxs = input$yxs_mod1_val_int_opt, Ypx =  input$ypx_mod1_val_int_opt)

                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod1_name_int_opt,input$ks_mod1_name_int_opt,
                                                input$yxs_mod1_name_int_opt,input$ypx_mod1_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod1_range_int_opt, Ks =  input$ks_mod1_range_int_opt,
                                                       Yxs = input$yxs_mod1_range_int_opt, Ypx =  input$ypx_mod1_range_int_opt)
                }
                
                # Load model 2 ###########################################################
                else if (input$mod_int == "model2") { 
                        
                        source("model2.R")
                        
                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod2_val_int_opt, Ks =  input$ks_mod2_val_int_opt,
                                                   Yxs = input$yxs_mod2_val_int_opt, Ypx =  input$ypx_mod2_val_int_opt,
                                                   Kp =  input$kp_mod2_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod2_name_int_opt,input$ks_mod2_name_int_opt,
                                                    input$yxs_mod2_name_int_opt,input$ypx_mod2_name_int_opt,
                                                    input$kp_mod2_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod2_range_int_opt, Ks =  input$ks_mod2_range_int_opt,
                                                        Yxs = input$yxs_mod2_range_int_opt, Ypx =  input$ypx_mod2_range_int_opt,
                                                        Kp =  input$kp_mod2_range_int_opt)
                }
                
                # Load model 3 ###########################################################
                else if (input$mod_int == "model3") { 
                        
                        source("model3.R")
                        
                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod3_val_int_opt, Ks =  input$ks_mod3_val_int_opt,
                                                   Yxs = input$yxs_mod3_val_int_opt, alpha =  input$alpha_mod3_val_int_opt,
                                                   beta =  input$beta_mod3_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod3_name_int_opt,input$ks_mod3_name_int_opt,
                                                    input$yxs_mod3_name_int_opt,input$alpha_mod3_name_int_opt,
                                                    input$beta_mod3_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod3_range_int_opt, Ks =  input$ks_mod3_range_int_opt,
                                                        Yxs = input$yxs_mod3_range_int_opt, alpha =  input$alpha_mod3_range_int_opt,
                                                        beta =  input$beta_mod3_range_int_opt)
                }
                
                # Load model 4 ###########################################################
                else if (input$mod_int == "model4") { 
                        
                        source("model4.R")
                        
                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod4_val_int_opt, Ks =  input$ks_mod4_val_int_opt,
                                                   Yxs = input$yxs_mod4_val_int_opt, Ypx =  input$ypx_mod4_val_int_opt,
                                                   Kd =  input$kd_mod4_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod4_name_int_opt,input$ks_mod4_name_int_opt,
                                                    input$yxs_mod4_name_int_opt,input$ypx_mod4_name_int_opt,
                                                    input$kd_mod4_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod4_range_int_opt, Ks =  input$ks_mod4_range_int_opt,
                                                        Yxs = input$yxs_mod4_range_int_opt, Ypx =  input$ypx_mod4_range_int_opt,
                                                        Kd =  input$kd_mod4_range_int_opt)
                }
                
                # Load model 5 ###########################################################
                else if (input$mod_int == "model5") { 
                        
                        source("model5.R")
                        
                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod5_val_int_opt, Ks =  input$ks_mod5_val_int_opt,
                                                   Yxs = input$yxs_mod5_val_int_opt, Ypx =  input$ypx_mod5_val_int_opt,
                                                   Km =  input$km_mod5_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod5_name_int_opt,input$ks_mod5_name_int_opt,
                                                    input$yxs_mod5_name_int_opt,input$ypx_mod5_name_int_opt,
                                                    input$km_mod5_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod5_range_int_opt, Ks =  input$ks_mod5_range_int_opt,
                                                        Yxs = input$yxs_mod5_range_int_opt, Ypx =  input$ypx_mod5_range_int_opt,
                                                        Km =  input$km_mod5_range_int_opt)
                }
                
                # Load model 6 ################################################ 
                else { 
                        
                        source("model6.R")
                        
                        p_val_rec_opt$rec_val <- c(Vmax = input$vmax_mod6_val_int_opt, Ks =  input$ks_mod6_val_int_opt,
                                                   Yxs = input$yxs_mod6_val_int_opt, alpha =  input$alpha_mod6_val_int_opt,
                                                   Kp =  input$kp_mod6_val_int_opt, beta =  input$beta_mod6_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod6_name_int_opt, input$ks_mod6_name_int_opt,
                                                    input$yxs_mod6_name_int_opt, input$alpha_mod6_name_int_opt,
                                                    input$kp_mod6_name_int_opt, input$beta_mod6_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(Vmax = input$vmax_mod6_range_int_opt, Ks =  input$ks_mod6_range_int_opt,
                                                        Yxs = input$yxs_mod6_range_int_opt, alpha =  input$alpha_mod6_range_int_opt,
                                                        Kp =  input$kp_mod6_range_int_opt, beta =  input$beta_mod6_range_int_opt)
                }
        })
        
        
        # Select panel ##############################################################
        # Show model 1 panel 
        output$parms_ui_mod1_opt <- reactive({
                
                ifelse(input$mod_int == "model1",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod1_opt", suspendWhenHidden = FALSE)
        
        # Show model 2 panel
        output$parms_ui_mod2_opt <- reactive({
                
                ifelse(input$mod_int == "model2",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod2_opt", suspendWhenHidden = FALSE)
        
        # Show model 3 panel
        output$parms_ui_mod3_opt <- reactive({
                
                ifelse(input$mod_int == "model3",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod3_opt", suspendWhenHidden = FALSE)
        
        # Show model 4 panel
        output$parms_ui_mod4_opt <- reactive({
                
                ifelse(input$mod_int == "model4",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod4_opt", suspendWhenHidden = FALSE)
        
        # Show model 5 panel
        output$parms_ui_mod5_opt <- reactive({
                
                ifelse(input$mod_int == "model5",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod5_opt", suspendWhenHidden = FALSE)
        
        # Show model 6 panel
        output$parms_ui_mod6_opt <- reactive({
                
                ifelse(input$mod_int == "model6",TRUE,FALSE)
        })
        
        outputOptions(output, "parms_ui_mod6_opt", suspendWhenHidden = FALSE)
        
        
        # Load file ##############################################################
        df_rec_opt <- reactive({
                
                data <- read.xlsx(file = input$file_int_opt$datapath,
                          sheetIndex = as.numeric(input$sheet_int_opt), header = T) %>% filter(!is.na(time))
        })
        
        # Show data table ##############################################################
        table_data_rec_opt <- reactive({
                
                
                if (input$head == TRUE) {
                        
                        return(head(df_rec_opt()))
                }
                
                else {
                        df_rec_opt()
                }
        })
        
        
        output$table_data_out_opt <- renderTable({
                
                req(input$file_int_opt) #################
                table_data_rec_opt()
        })
        
        # Show scatterplot wiht data ################
        plot_data_rec_opt <- reactive({
                
                
                plot_fun_opt(df_rec_opt())
        })
        
        output$plot_data_out_opt <- renderPlot({
                
                req(input$file_int_opt)
                plot_data_rec_opt()
                
        })
        
        # Make optimization #####################################################
        opt_parms_rec_val_opt <- eventReactive(input$make_opt_int_opt , {
                
                req(input$file_int_opt)
                req(p_name_rec_opt$rec_val)
                get_parms_fun(df_rec_opt(), p_val_rec_opt$rec_val, p_name_rec_opt$rec_val, p_range_rec_opt$rec_val)
                
                
        })
        
        # Show optimized parameters ###############################################
        output$table_result_out_opt <- renderTable({
                
                
                opt_parms_rec_val_opt()[["results_table"]]
                
        })
        
        
        # Show comparation between simulation with optimized parameters and data #################################
        comp_rec_opt <- eventReactive(input$make_opt_int_opt,{
                
                req(input$file_int_opt)
                comp_fun_opt(opt_parms_rec_val_opt()[["values"]], df_rec_opt())
        })
        
        output$comp_out_opt <- renderPlot({

                
                comp_rec_opt()

        })
        
        # ga summary ####
        ga_summary_rec_opt <- eventReactive(input$make_opt_int_opt,{
                
                req(input$file_int_opt)
                summary_ga_fun(opt_parms_rec_val_opt()[["ga"]])
        })
        
        output$ga_summary_out_opt <- renderPrint({
                
                ga_summary_rec_opt()
                
        })
        
        
        # plot ga progress ####
        ga_plot_rec_opt <- eventReactive(input$make_opt_int_opt,{
                
                req(input$file_int_opt)
                plot_ga_fun(opt_parms_rec_val_opt()[["ga"]])
        })
        
        output$ga_plot_out_opt <- renderPlot({
                
                ga_plot_rec_opt()
                
        })
        
}

shinyApp(ui, server)
