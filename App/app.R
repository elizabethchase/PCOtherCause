#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(survival)
library(ggplot2)

load("ocm_model_final.RData")
load("ocm_nhanes_data.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Other Cause Mortality Prediction"),
  mainPanel(
    tabsetPanel(
      tabPanel("Description", 
               br(),
               includeMarkdown("ocmapp_methods.Rmd")),
      tabPanel("Prostate",
               br(),
               fluidRow(
                 h3("General Characteristics"),
                 br(),
                 column(4,
                        numericInput("pc_age", 
                                     "Age (years)", 
                                     value = 65)),
                 column(8)),
               fluidRow(
                 column(4,
                        numericInput("pc_height", 
                                     "Height (inches)", 
                                     value = 70)),
                 column(4,
                        numericInput("pc_weight", 
                                     "Weight (lbs)", 
                                     value = 200)),
                 column(4,
                        checkboxGroupInput("pc_comorbidities", 
                                           "Comorbidities", 
                                           choices = list("Diabetes" = 1, 
                                                          "Hypertension" = 2, 
                                                          "Previous stroke" = 3),
                                           selected = 1))
               ),
               fluidRow(
                 column(4,
                        selectInput("pc_education", "Educational Attainment", 
                                    choices = list("Less than 9th grade" = 1, 
                                                   "9th-11th grade" = 2, 
                                                   "High school graduate" = 3,
                                                   "Some college" = 4,
                                                   "College graduate" = 5), selected = 1)),
                 column(4,
                        selectInput("pc_marital", 
                                    "Marital Status", 
                                    choices = list("Married" = 1, 
                                                   "Widowed, Divorced, or Separated" = 2, 
                                                   "Never Married" = 3),
                                    selected = 1)),
                 column(4,
                        selectInput("pc_smoking", 
                                    "Smoking Status", 
                                    choices = list("Never smoker" = 1, 
                                                   "Current smoker" = 2, 
                                                   "Former smoker" = 3),
                                    selected = 1))
               ),
               fluidRow(
                 column(4,
                        submitButton("Update"),
                        br(),
                        br()),
                 column(8)
               ),
               fluidRow(
                 br(),
                 h3("Mortality Prediction"),
                 textOutput("pc_text"),
                 plotOutput("pc_plot")
               )
      ),
      tabPanel("Lung",
               br(),
               fluidRow(
                 h3("General Characteristics"),
                 br(),
                 column(4,
                        numericInput("lc_age", 
                                     "Age (years)", 
                                     value = 65)),
                 column(8)),
               fluidRow(
                 column(4,
                        numericInput("lc_height", 
                                     "Height (inches)", 
                                     value = 70)),
                 column(4,
                        numericInput("lc_weight", 
                                     "Weight (lbs)", 
                                     value = 200)),
                 column(4,
                        checkboxGroupInput("lc_comorbidities", 
                                           "Comorbidities", 
                                           choices = list("Diabetes" = 1, 
                                                          "Hypertension" = 2, 
                                                          "Previous stroke" = 3),
                                           selected = 1))
               ),
               fluidRow(
                 column(4,
                        selectInput("lc_education", "Educational Attainment", 
                                    choices = list("Less than 9th grade" = 1, 
                                                   "9th-11th grade" = 2, 
                                                   "High school graduate" = 3,
                                                   "Some college" = 4,
                                                   "College graduate" = 5), selected = 1)),
                 column(4,
                        selectInput("lc_marital", 
                                    "Marital Status", 
                                    choices = list("Married" = 1, 
                                                   "Widowed, Divorced, or Separated" = 2, 
                                                   "Never Married" = 3),
                                    selected = 1)),
                 column(4,
                        selectInput("lc_smoking", 
                                    "Smoking Status", 
                                    choices = list("Never smoker" = 1, 
                                                   "Current smoker" = 2, 
                                                   "Former smoker" = 3),
                                    selected = 1))
               ),
               fluidRow(
                 column(4,
                        submitButton("Update"),
                        br(),
                        br()),
                 column(8)
               ),
               fluidRow(
                 br(),
                 h3("Mortality Prediction"),
                 textOutput("lc_text"),
                 plotOutput("lc_plot")
               )
      ),
      tabPanel("Colorectal",
               br(),
               fluidRow(
                 h3("General Characteristics"),
                 br(),
                 column(4,
                        numericInput("cc_age", 
                                     "Age (years)", 
                                     value = 65)),
                 column(8)),
               fluidRow(
                 column(4,
                        numericInput("cc_height", 
                                     "Height (inches)", 
                                     value = 70)),
                 column(4,
                        numericInput("cc_weight", 
                                     "Weight (lbs)", 
                                     value = 200)),
                 column(4,
                        checkboxGroupInput("cc_comorbidities", 
                                           "Comorbidities", 
                                           choices = list("Diabetes" = 1, 
                                                          "Hypertension" = 2, 
                                                          "Previous stroke" = 3),
                                           selected = 1))
               ),
               fluidRow(
                 column(4,
                        selectInput("cc_education", "Educational Attainment", 
                                    choices = list("Less than 9th grade" = 1, 
                                                   "9th-11th grade" = 2, 
                                                   "High school graduate" = 3,
                                                   "Some college" = 4,
                                                   "College graduate" = 5), selected = 1)),
                 column(4,
                        selectInput("cc_marital", 
                                    "Marital Status", 
                                    choices = list("Married" = 1, 
                                                   "Widowed, Divorced, or Separated" = 2, 
                                                   "Never Married" = 3),
                                    selected = 1)),
                 column(4,
                        selectInput("cc_smoking", 
                                    "Smoking Status", 
                                    choices = list("Never smoker" = 1, 
                                                   "Current smoker" = 2, 
                                                   "Former smoker" = 3),
                                    selected = 1))
               ),
               fluidRow(
                 column(4,
                        submitButton("Update"),
                        br(),
                        br()),
                 column(8)
               ),
               fluidRow(
                 br(),
                 h3("Mortality Prediction"),
                 textOutput("cc_text"),
                 plotOutput("cc_plot")
               )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$plot <- renderPlot({ggplot})
  #output$summary <- renderTable({kable})
  model <- reactive({
    pc_dat <- data.frame("age_ctr" = input$pc_age - 69.5, "pc" = "Yes")
    pc_bmi <- (input$pc_weight*0.453592)/((input$pc_height*0.0254)^2)
    pc_dat$pc <- factor(pc_dat$pc, levels=c("No", "Yes"))
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
    
    pc_prediction <- survfit(cox40, newdata=pc_dat)
    
    pcdat <- data.frame("Time" = c(0, pc_prediction$time[-1]), "Risk" = c(0, pc_prediction$cumhaz[-1]))
    
    lc_dat <- data.frame("age_ctr" = input$lc_age - 69.5, "pc" = "Yes")
    lc_bmi <- (input$lc_weight*0.453592)/((input$lc_height*0.0254)^2)
    lc_dat$pc <- factor(lc_dat$pc, levels=c("No", "Yes"))
    lc_dat$underweight <- ifelse(lc_bmi < 18.5, "Yes", "No")
    lc_dat$overweight2 <- ifelse(lc_bmi >= 25 & lc_bmi < 40, "Yes", "No")
    lc_dat$obese2 <- ifelse(lc_bmi >= 40, "Yes", "No")
    lc_dat$diabetic <- ifelse(1 %in% input$lc_comorbidities, "Yes", "No")
    lc_dat$educ <- ifelse(input$lc_education == 1, "Less than 9th grade", 
                          ifelse(input$lc_education == 2, "9th-11th grade",
                                 ifelse(input$lc_education==3, "HS graduate",
                                        ifelse(input$lc_education==4, "Some college", "College graduate"))))
    lc_dat$hypertension <- ifelse(2 %in% input$lc_comorbidities, "Yes", "No")
    lc_dat$marital2 <- ifelse(input$lc_marital == 1, "Married", 
                              ifelse(input$lc_marital == 2, "Separated",
                                     "Single"))
    lc_dat$smoker <- ifelse(input$lc_smoking == 1, "Never", ifelse(input$lc_smoking== 2, "Current", "Former"))
    lc_dat$stroke <- ifelse(3 %in% input$lc_comorbidities, "Yes", "No")
    
    lc_prediction <- survfit(cox40, newdata=lc_dat)
    
    lcdat <- data.frame("Time" = c(0, lc_prediction$time[-1]), "Risk" = c(0, lc_prediction$cumhaz[-1]))
    
    cc_dat <- data.frame("age_ctr" = input$cc_age - 69.5, "pc" = "Yes")
    cc_bmi <- (input$cc_weight*0.453592)/((input$cc_height*0.0254)^2)
    cc_dat$pc <- factor(cc_dat$pc, levels=c("No", "Yes"))
    cc_dat$underweight <- ifelse(cc_bmi < 18.5, "Yes", "No")
    cc_dat$overweight2 <- ifelse(cc_bmi >= 25 & cc_bmi < 40, "Yes", "No")
    cc_dat$obese2 <- ifelse(cc_bmi >= 40, "Yes", "No")
    cc_dat$diabetic <- ifelse(1 %in% input$cc_comorbidities, "Yes", "No")
    cc_dat$educ <- ifelse(input$cc_education == 1, "Less than 9th grade", 
                          ifelse(input$cc_education == 2, "9th-11th grade",
                                 ifelse(input$cc_education==3, "HS graduate",
                                        ifelse(input$cc_education==4, "Some college", "College graduate"))))
    cc_dat$hypertension <- ifelse(2 %in% input$cc_comorbidities, "Yes", "No")
    cc_dat$marital2 <- ifelse(input$cc_marital == 1, "Married", 
                              ifelse(input$cc_marital == 2, "Separated",
                                     "Single"))
    cc_dat$smoker <- ifelse(input$cc_smoking == 1, "Never", ifelse(input$cc_smoking== 2, "Current", "Former"))
    cc_dat$stroke <- ifelse(3 %in% input$cc_comorbidities, "Yes", "No")
    
    cc_prediction <- survfit(cox40, newdata=cc_dat)
    
    ccdat <- data.frame("Time" = c(0, cc_prediction$time[-1]), "Risk" = c(0, cc_prediction$cumhaz[-1]))
    
    list(pcdat = pcdat, lcdat = lcdat, ccdat = ccdat)
  })
  
  output$pc_plot <- renderPlot({
    ggplot() + geom_step(data = model()$pcdat, aes(x = Time, y = Risk*100), size = 1.5, 
                         direction = "hv", alpha = 1) +
      scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) + 
      scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) + theme_bw()
  })
  output$pc_text <- renderText({
    pc_ten <- model()$pcdat$Risk[model()$pcdat$Time==120]
    
    paste0("At ten years, this patient's cumulative hazard of dying of other causes is ", round(pc_ten*100, digits=2), "%.")
  })
  
  output$lc_plot <- renderPlot({
    ggplot() + geom_step(data = model()$lcdat, aes(x = Time, y = Risk*100), size = 1.5, 
                         direction = "hv", alpha = 1) +
      scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) + 
      scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) + theme_bw()
  })
  output$lc_text <- renderText({
    lc_ten <- model()$lcdat$Risk[model()$lcdat$Time==120]
    
    paste0("At ten years, this patient's cumulative hazard of dying of other causes is ", round(lc_ten*100, digits=2), "%.")
  })
  
  output$cc_plot <- renderPlot({
    ggplot() + geom_step(data = model()$ccdat, aes(x = Time, y = Risk*100), size = 1.5, 
                         direction = "hv", alpha = 1) +
      scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) + 
      scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) + theme_bw()
  })
  output$cc_text <- renderText({
    cc_ten <- model()$ccdat$Risk[model()$ccdat$Time==120]
    
    paste0("At ten years, this patient's cumulative hazard of dying of other causes is ", round(cc_ten*100, digits=2), "%.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)