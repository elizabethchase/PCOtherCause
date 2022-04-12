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
library(pammtools)
library(markdown)

load("modobjects.RData")
load("nhanes_data_clean.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(HTML(
    "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-179472953-2'></script>
                <script>
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                
                gtag('config', 'UA-179472953-2');
                </script>
                "
  )),
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
                               "College graduate" = 5,
                               "Don't use my educational attainment in the model" = 6), selected = 3),
                    selectInput("pc_marital", 
                            "Relationship Status", 
                          choices = list("Married or living with partner" = 1, 
                             "Widowed, divorced, or separated" = 2, 
                             "Never married and no live-in partner" = 3,
                             "Don't use my relationship status in the model" = 4),
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
                   "The presented model is for patients diagnosed with prostate cancer who have not 
                   yet begun treatment. We predict the long-term chances of dying from other causes 
                   for prostate cancer patients.",
                   br(),
                   br(),
                   br(),
                   span(textOutput("marital_warn"),style="color:red"),
                   br(),
                   span(textOutput("education_warn"), style="color:red"),
                   tabsetPanel(
                     tabPanel(
                       "Summary",
                       br(),
                       textOutput("text1"), 
                       br(),
                       tableOutput("table1"),
                       "This is a patient's life expectancy and their probability of dying of other causes at 5 and 10 years (in the absence of prostate cancer)."
                     ),
                     tabPanel(
                       "Survival Curve",
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
               ),
               hr(),
               print("Disclaimer: The content provided does not provide medical advice. 
              By proceeding, you acknowledge that viewing or use of this content does 
              not create a medical professional-patient relationship, and does not 
              constitute an opinion, medical advice, professional service or treatment 
              recommendation of any condition. Content provided is for educational purposes only. 
              The information and Content provided are not substitutes for medical or professional care, 
              and you should not use the information in place of a visit, call, consultation or the 
              advice of your physician or other healthcare provider. You are liable or responsible for 
              any advice, course of treatment, or any other information, services that are based on 
              Content through this site."),
               br(),
               br()), 
      tabPanel("More Information", 
               br(),
               includeMarkdown("ocmapp_methods.Rmd"),
               hr(),
               print("Disclaimer: The content provided does not provide medical advice. 
              By proceeding, you acknowledge that viewing or use of this content does 
              not create a medical professional-patient relationship, and does not 
              constitute an opinion, medical advice, professional service or treatment 
              recommendation of any condition. Content provided is for educational purposes only. 
              The information and Content provided are not substitutes for medical or professional care, 
              and you should not use the information in place of a visit, call, consultation or the 
              advice of your physician or other healthcare provider. You are liable or responsible for 
              any advice, course of treatment, or any other information, services that are based on 
              Content through this site."),
               br(),
               br())
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
                                         ifelse(input$pc_education==4, "Some college", 
                                                ifelse(input$pc_education==5, "College graduate", NA_character_)))))
    if (input$pc_education==6){
      no_educ <- TRUE
    } else{
      no_educ <- FALSE
    }
    
    pc_dat$hypertension <- ifelse(2 %in% input$pc_comorbidities, "Yes", "No")
    pc_dat$marital2 <- ifelse(input$pc_marital == 1, "Married", 
                               ifelse(input$pc_marital == 2, "Separated",
                                      ifelse(input$pc_marital == 3, "Single", NA_character_)))
    
    if (input$pc_marital==4){
      no_mar <- TRUE
    } else{
      no_mar <- FALSE
    } 
    
    pc_dat$smoker <- ifelse(input$pc_smoking == 1, "Never", ifelse(input$pc_smoking== 2, "Current", "Former"))
    pc_dat$stroke <- ifelse(3 %in% input$pc_comorbidities, "Yes", "No")
    
    #mydata <- mydata[mydata$inmodel4 == 1, ]
    
    pc_dat_maxed <- pc_dat
    pc_dat_maxed$educ <- "College graduate"
    pc_dat_maxmar <- pc_dat
    pc_dat_maxmar$marital2 <- "Married"
    
    if (!no_mar & !no_educ){
      pc_prediction <- survfit(cox_40, pc_dat)
      pc_prediction_maxed <- survfit(cox_40, pc_dat_maxed)
      pc_prediction_maxmar <- survfit(cox_40, pc_dat_maxmar)
      
      inds <- which(pc_prediction$surv-0.5 <= 0)
      inds_maxed <- which(pc_prediction_maxed$surv-0.5 <= 0)
      inds_maxmar <- which(pc_prediction_maxmar$surv-0.5 <= 0)
      
      time <- pc_prediction$time
      if (identical(inds, integer(0))){
        mytime <- 202
      } else{
        mytime <- time[min(inds)]
      }
      if (identical(inds_maxed, integer(0))){
        mytime_maxed <- 202
      } else{
        mytime_maxed <- time[min(inds_maxed)]
      }
      if (identical(inds_maxmar, integer(0))){
        mytime_maxmar <- 202
      } else{
        mytime_maxmar <- time[min(inds_maxmar)]
      }
      
      if (mytime < 120 & mytime_maxmar >= 120){
        mar_warn <- "This patient's life expectancy prediction would change from < 10 years
                   to 10+ years based only on a change in relationship status, which could be an
                   unfair prediction. Relationship status is predictive of mortality because of its
                   relationship with caregiving support, nutrition, exercise, and routine medical
                   care. Consider discussing whether the patient wants relationship status to affect
                   their prediction, and possibly use the 'Don't use my relationship status in the model' 
                   option instead."
      } else {
        mar_warn <- ""
      }
      
      if (mytime < 120 & mytime_maxed >= 120){
        educ_warn <- "This patient's life expectancy prediction would change from < 10 years
                   to 10+ years based only on a change in educational attainment, which could be an
                   unfair prediction. Educational attainment is predictive of mortality because of its
                   relationship with socioeconomic status, insurance coverage, and access to quality
                   health care. Consider discussing whether the patient wants their educational
                   attainment to affect their prediction, and possibly use the 'Don't use my educational
                   attainment in the model' option instead."
      } else {
        educ_warn <- ""
      }
    } else if (!no_mar & no_educ){
      pc_prediction <- survfit(cox_noeduc, pc_dat)
      pc_prediction_maxmar <- survfit(cox_noeduc, pc_dat_maxmar)
      
      inds <- which(pc_prediction$surv-0.5 <= 0)
      inds_maxmar <- which(pc_prediction_maxmar$surv-0.5 <= 0)
      
      time <- pc_prediction$time
      if (identical(inds, integer(0))){
        mytime <- 202
      } else{
        mytime <- time[min(inds)]
      }
      if (identical(inds_maxmar, integer(0))){
        mytime_maxmar <- 202
      } else{
        mytime_maxmar <- time[min(inds_maxmar)]
      }
      
      if (mytime < 120 & mytime_maxmar >= 120){
        mar_warn <- "This patient's life expectancy prediction would change from < 10 years
                   to 10+ years based only on a change in relationship status, which could be an
                   unfair prediction. Relationship status is predictive of mortality because of its
                   relationship with caregiving support, nutrition, exercise, and routine medical
                   care. Consider discussing whether the patient wants relationship status to affect
                   their prediction, and possibly use the 'Don't use my relationship status in the model' 
                   option instead."
      } else {
        mar_warn <- ""
      }
      educ_warn <- ""
    } else if (no_mar & !no_educ){
      pc_prediction <- survfit(cox_nomar, pc_dat)
      pc_prediction_maxed <- survfit(cox_nomar, pc_dat_maxed)
      
      inds <- which(pc_prediction$surv-0.5 <= 0)
      inds_maxed <- which(pc_prediction_maxed$surv-0.5 <= 0)
      
      time <- pc_prediction$time
      if (identical(inds, integer(0))){
        mytime <- 202
      } else{
        mytime <- time[min(inds)]
      }
      if (identical(inds_maxed, integer(0))){
        mytime_maxed <- 202
      } else{
        mytime_maxed <- time[min(inds_maxed)]
      }
      
      mar_warn <- ""
    
      if (mytime < 120 & mytime_maxed >= 120){
        educ_warn <- "This patient's life expectancy prediction would change from < 10 years
                   to 10+ years based only on a change in educational attainment, which could be an
                   unfair prediction. Educational attainment is predictive of mortality because of its
                   relationship with socioeconomic status, insurance coverage, and access to quality
                   health care. Consider discussing whether the patient wants their educational
                   attainment to affect their prediction, and possibly use the 'Don't use my educational
                   attainment in the model' option instead."
      } else {
        educ_warn <- ""
      }
    } else if (no_mar & no_educ){
      pc_prediction <- survfit(cox_red, pc_dat)
    
      inds <- which(pc_prediction$surv-0.5 <= 0)
      
      time <- pc_prediction$time
      if (identical(inds, integer(0))){
        mytime <- 202
      } else{
        mytime <- time[min(inds)]
      }
      
      mar_warn <- ""
      educ_warn <- ""
    } 
    
    pc_dat$medsurv <- mytime
    pc_dat$medsurv2 <- ifelse(pc_dat$medsurv>=180, ">15 years", paste0(round(pc_dat$medsurv/12, digits=0), " years"))
    pc_dat$bmi <- pc_bmi
    pc_dat$age <- input$pc_age
    pc_dat$strokestat <- ifelse(pc_dat$stroke=="Yes", "a previous stroke", "no history of stroke")
    pc_dat$diabstat <- ifelse(pc_dat$diabetic=="Yes", "with diabetes", "without diabetes")
    pc_dat$hyperstat <- ifelse(pc_dat$hypertension=="Yes", "hypertensive", "non-hypertensive")
    pc_dat$educstat <- case_when(
      pc_dat$educ=="Less than 9th grade" ~ "of less than 9th grade",
      pc_dat$educ=="9th-11th grade" ~ "of 9th-11th grade",
      pc_dat$educ=="HS graduate" ~ "of high school diploma/GED",
      pc_dat$educ=="Some college" ~ "of some college",
      pc_dat$educ=="College graduate" ~ "of college degree",
      no_educ ~ "not provided"
    )
    pc_dat$marstat <- case_when(
      pc_dat$marital2=="Single" ~ "of never married/in a live-in relationship",
      pc_dat$marital2=="Married" ~ "of currently married/in a live-in relationship",
      pc_dat$marital2=="Separated" ~ "of formerly married/in a live-in relationship",
      no_mar ~ "not provided"
    )
    pc_dat$smokstat <- case_when(
      pc_dat$smoker=="Never" ~ "never",
      pc_dat$smoker=="Current" ~ "current",
      pc_dat$smoker=="Former" ~ "former"
    )
    
    #pcdat <- data.frame("Time" = c(pc_prediction$time, fg_predictions[,1]), "Risk" = c(1-pc_prediction$surv, fg_predictions[,2]),
     #                   "Model" = c(rep("Cause-Specific", length(pc_prediction$time)), rep("Fine and Gray", length(fg_predictions[,1]))))
    pcdat <- data.frame("Time" = pc_prediction$time, "Risk" = 1-pc_prediction$surv, "Lower" = 1-pc_prediction$lower, 
                       "Upper" = 1-pc_prediction$upper)
    
   
    riskten <- pcdat$Risk[which.min(ifelse((120-pcdat$Time) < 0, NA, (120-pcdat$Time)))]
    riskfive <- pcdat$Risk[which.min(ifelse((60-pcdat$Time) < 0, NA, (60-pcdat$Time)))]
    
    resultstab <- data.frame("Metric" = c("Life Expectancy", "5-Year Mortality", "10-Year Mortality"), 
                             "Prediction" = c(paste0(pc_dat$medsurv2),
                                              paste0(round(riskfive*100, digits = 0), "%"),
                                              paste0(round(riskten*100, digits = 0), "%")))
    
    mypred <- round(pcdat$Risk[which.min(ifelse((input$years*12-pcdat$Time) < 0, NA, (input$years*12-pcdat$Time)))]*100, digits=0)
    
    list(alldat = pcdat, riskten = riskten, resultstab = resultstab, mypred = mypred, 
         patient_char = pc_dat, mar_warn = mar_warn, educ_warn = educ_warn)
  
  })
  
  output$marital_warn <- renderText({
    model()$mar_warn
  })
  
  output$education_warn <- renderText({
    model()$educ_warn
  })
  
  output$text1 <- renderText({
    paste0("This is a ", model()$patient_char$age, " year old ", model()$patient_char$hyperstat, " man with BMI ", round(model()$patient_char$bmi, digits = 1), ", ", model()$patient_char$strokestat,
           " and ", model()$patient_char$diabstat, ". He is a ", model()$patient_char$smokstat, " smoker, with relationship status ", model()$patient_char$marstat, 
           " and educational attainment ", model()$patient_char$educstat, ".")
  })

  output$text2 <- renderText({
    paste0("Of 100 men like this patient, ", round(model()$riskten*100, digits = 0), 
           " are expected to die of other causes within the next 10 years.")
  })
  
  output$text3 <- renderText({
    paste0("Estimated median life expectancy in the absence of cancer mortality: ", model()$patient_char$medsurv2)
  })
  
  output$table1 <- function(){kable(model()$resultstab) %>% column_spec(column = c(1:2), width = "4cm") %>%
      kable_styling()}
  
  output$plot1 <- renderPlot({
    #ggplot() + geom_stepribbon(data = model()$alldat2, aes(x = Time, ymin = Lower*100, ymax=Upper*100), fill = "grey70") + 
      ggplot() + geom_step(data = model()$alldat, aes(x = Time, y = Risk*100), size = 1.5,
                         direction = "hv", alpha = 1) +
      scale_x_continuous("Years", limits = c(0, 168), breaks = seq(0, 168, by=48), labels = c("0", "4", "8", "12")) +
      scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) +
      theme_bw() + 
      geom_hline(yintercept = model()$mypred) + 
      geom_vline(xintercept = input$years*12) #+
      #scale_color_manual(values = c("black", "mediumpurple4"))
  })
  
  output$info1 <- renderText({
    paste0("Estimated probability of dying of other causes in the absence of prostate cancer within ", round(input$years, digits = 2), " years: ", model()$mypred, "%.")
  })
  
  output$pict1 <- renderPlot({
  
    img_red <- readPNG("man_red.png")
    img_blue <- readPNG("man_blue.png")
    deadman <- rasterGrob(img_red, interpolate=FALSE)
    liveman <- rasterGrob(img_blue, interpolate=FALSE)
    
    myplot <- qplot(0:10, 0:10, geom="blank") 
    k <- 1
    dead <- round(model()$riskten*100, digits=0)
    vec1 <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    for (j in vec1){
      for (i in 1:10){
        if (k <= dead){
          myplot <- myplot + annotation_custom(deadman, xmin=(i-1), xmax=i, ymin=(j-1), ymax=j)
        } else{
          myplot <- myplot + annotation_custom(liveman, xmin=(i-1), xmax=i, ymin=(j-1), ymax=j)
        }
        k <- k+1
      }
    }
    
    myplot + theme(rect = element_blank(), axis.ticks = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(), 
                   axis.text.y=element_blank()) + xlab("") + ylab("") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)