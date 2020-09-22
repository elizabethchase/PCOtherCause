#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(survival)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(png)
library(gridGraphics)

load("modobjects.RData")
load("nhanes_data_clean.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Other Cause Mortality Prediction"),
  mainPanel(
    tabsetPanel(
      tabPanel("Predictions",
               sidebarLayout(
                 sidebarPanel(
                    numericInput("pc_age", 
                                "Age (years)", 
                                value = 65),
                    numericInput("pc_height", 
                              "Height (inches)", 
                              value = 70),
                    numericInput("pc_weight", 
                            "Weight (lbs)", 
                            value = 200),
                    checkboxGroupInput("pc_comorbidities", 
                         "Comorbidities", 
                         choices = list("Diabetes" = 1, 
                                        "Hypertension" = 2, 
                                        "Previous stroke" = 3),
                         selected = 1),
                    selectInput("pc_education", "Educational Attainment", 
                    choices = list("Less than 9th grade" = 1, 
                               "9th-11th grade" = 2, 
                               "High school graduate" = 3,
                               "Some college" = 4,
                               "College graduate" = 5), selected = 1),
                    selectInput("pc_marital", 
                            "Marital Status", 
                          choices = list("Married" = 1, 
                             "Widowed, Divorced, or Separated" = 2, 
                             "Never Married" = 3),
                      selected = 1),
                    selectInput("pc_smoking", 
                        "Smoking Status", 
                        choices = list("Never smoker" = 1, 
                           "Current smoker" = 2, 
                           "Former smoker" = 3),
                    selected = 1),
                    submitButton("Update")
                 ),
                 mainPanel(
                   br(),
                   "Our staging model is for patients diagnosed with prostate cancer who have not 
                    yet begun treatment. We predict the long-term chances of dying from other causes.",
                   br(),
                   br(),
                   br(),
                   tabsetPanel(
                     tabPanel(
                       "Summary",
                       br(),
                       textOutput("text1"), 
                       br(),
                       tableOutput("table1"),
                       bsTooltip("table1", "This is a patient's probability of dying of other causes at 5 and 10 years.", 
                                 "right", options = list(container = "body"))
                     ),
                     tabPanel(
                       "Cumulative Incidence",
                       plotOutput("plot1"),
                       fluidRow(
                         column(sliderInput(inputId = "years", label="Years", min = 0, max = 15, value = 10), width = 10),
                         column(submitButton("Update"), width = 2)
                       ),
                       br(),
                       textOutput("info1"),
                       br(),
                       br(),
                       br()
                     )
                   )
                 )
               )
      ), 
      tabPanel("More Information", 
               br(),
               includeMarkdown("ocmapp_methods.Rmd"))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$plot <- renderPlot({ggplot})
  #output$summary <- renderTable({kable})
  model <- reactive({
    pc_dat <- data.frame("age_ctr_40" = input$pc_age - 60.34387, "pc" = "PC")
    pc_bmi <- (input$pc_weight*0.453592)/((input$pc_height*0.0254)^2)
    pc_dat$pc <- factor(pc_dat$pc, levels=c("No PC", "PC"))
    pc_dat$underweight <- ifelse(pc_bmi < 18.5, "Yes", "No")
    pc_dat$overweight2 <- ifelse(pc_bmi >= 25 & pc_bmi < 40, "Yes", "No")
    pc_dat$obese2 <- ifelse(pc_bmi >= 40, "Yes", "No")
    pc_dat$diabetic <- ifelse(1 %in% input$pc_comorbidities, "Yes", "No")
    pc_dat$educ <- ifelse(input$pc_education == 1, "Less than 9th grade", 
                           ifelse(input$pc_education == 2, "9th-11th grade",
                                  ifelse(input$pc_education==3, "HS graduate",
                                         ifelse(input$pc_education==4, "Some college", "College graduate"))))
    pc_dat$hypertension <- ifelse(2 %in% input$pc_comorbidities, "Yes", "No")
    pc_dat$marital2 <- ifelse(input$pc_marital == 1, "Married", 
                               ifelse(input$pc_marital == 2, "Separated",
                                      "Single"))
    pc_dat$smoker <- ifelse(input$pc_smoking == 1, "Never", ifelse(input$pc_smoking== 2, "Current", "Former"))
    pc_dat$stroke <- ifelse(3 %in% input$pc_comorbidities, "Yes", "No")
    
    #mydata <- mydata[mydata$inmodel4 == 1, ]
    pc_prediction <- survfit(cox_40, pc_dat)
    
    incs <- pc_prediction$time[2:length(pc_prediction$time)] - pc_prediction$time[1:(length(pc_prediction$time)-1)]
    rmst <- (incs %*% (pc_prediction$surv[-length(pc_prediction$surv)]))/12
    
    pcdat <- data.frame("Time" = pc_prediction$time, "Risk" = 1-pc_prediction$surv)
    
    riskten <- pcdat$Risk[which.min(ifelse((120-pcdat$Time) < 0, NA, (120-pcdat$Time)))]
    riskfive <- pcdat$Risk[which.min(ifelse((60-pcdat$Time) < 0, NA, (60-pcdat$Time)))]
    
    resultstab <- data.frame("Metric" = c("5-Year Mortality", "10-Year Mortality"), 
                             "Prediction" = c(paste0(round(riskfive*100, digits = 0), "%"),
                                              paste0(round(riskten*100, digits = 0), "%")))
    
    mypred <- round(pcdat$Risk[which.min(ifelse((input$years*12-pcdat$Time) < 0, NA, (input$years*12-pcdat$Time)))]*100, digits=0)
    
    list(alldat = pcdat, riskten = riskten, resultstab = resultstab, mypred = mypred, rmst = rmst)
  
  })
  
  output$text1 <- renderText({
    paste0("This patient is predicted to live ", round(model()$rmst, digits = 0), " of the next 16.75 years. Of 100 men like this patient, ", 
           round(model()$riskten*100, digits = 0), " are expected to die of other causes within the next 10 years.")
  })
  
  output$table1 <- function(){kable(model()$resultstab) %>% column_spec(column = c(1:2), width = "4cm") %>%
      kable_styling()}
  
  output$plot1 <- renderPlot({
    ggplot() + geom_step(data = model()$alldat, aes(x = Time, y = Risk*100), size = 1.5,
                         direction = "hv", alpha = 1) +
      scale_x_continuous("Years", limits = c(0, 168), breaks = seq(0, 168, by=48), labels = c("0", "4", "8", "12")) +
      scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) +
      theme_bw() + 
      geom_hline(yintercept = model()$mypred) + 
      geom_vline(xintercept = input$years*12)
  })
  
  output$info1 <- renderText({
    paste0("Estimated probability of dying of other causes within ", round(input$years, digits = 2), " years: ", model()$mypred, "%.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)