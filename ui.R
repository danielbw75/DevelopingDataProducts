shinyUI(fluidPage( title="In-Patien children death prediction",
        h1('In-patient children death prediction'),
        hr(),
        fluidRow(
                column(3,h4("Your patient signs and Symptoms:"),
                checkboxInput('vars1', 'age less than 4 months'),
                checkboxInput('vars2', "temp. lower than 35.5C"),
                checkboxInput('vars3', "Did not have fever")),
                column(3,checkboxInput('vars4', "Pallor"),
                checkboxInput('vars5', "Jaundice"),
                checkboxInput('vars6', "Difficult Breathing"),
                checkboxInput('vars7', "Deep Breathing")),
                column(3,checkboxInput('vars8', "Convulsions"),
                checkboxInput('vars9', "Unable to Sit"),
                checkboxInput('vars10', "Unable to Drink"),
                helpText("Select your patient Signs and Symptoms to see his Scores and Risks of Dying")),
                column(3,checkboxInput('vars11', "Altered Consciousness"),
                checkboxInput('vars12', "Unconsciousness"),
                checkboxInput('vars13', "Meningeal Signs"))),
        
        fluidRow(tabsetPanel(type = "tabs", 
                tabPanel("Risk of Dying",
                      column(4,        dataTableOutput("table")),
                      column(6,
                             fluidRow(
                                     verbatimTextOutput("value")),
                             fluidRow(
                                     verbatimTextOutput("value2")),
                             fluidRow(
                                     verbatimTextOutput("value3")),
                             helpText("Select your patient signs and symptoms to check his Risk of Dying, if there is no value showing,
                                      this is because there were no patients in the dataset with this score or logit prob."),
                             helpText("Don't forget to create your custom score in the lower panel of the page"))),
             
                    tabPanel("ROC curve",
                             helpText("Here you can check if your custom Score can beat the Authors Score or the Logit prediction.
                                      check how changing your score wheights and thresholds will change its sensitivity, specificity and AUC"),
                             column(3,plotOutput('plot1')),
                             column(3,plotOutput('plot2')),
                             column(3,plotOutput('plot3')),
                             column(3,plotOutput('plot4'))),
                             
                    tabPanel("Risk Distribution",
                             column(5,dataTableOutput('RiskSummary')),
                             column(4,
                                    h4("Your Patient risk is the Yellow bar"),
                                    plotOutput('plot6')),
                             column(3,
                                    radioButtons("predictor", "Select the predictor", c(
                                            "Author Score",
                                            "Custom Score",
                                            "Logit"),
                                            selected = "Author Score"),
                             helpText("Do not forget to create your Custom Score, in the lower panel of the page.You can check how changing the weights and thresholds will affect your Risk Distribution."),
                             helpText("When you select your patient signs and symptoms the graph will update his risk."))),
                            
                    
                   
                    
                    
                    tabPanel("Help (documentation)",
                             column(3,
                                    p("This Shiny app intent to show 3 different predictors to assess in-patient children death.", br(),
                                    "The dataset was downloaded from", a("http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133950#sec011"),br(), "from the publication:", br(),
                                    strong("Admission Risk Score to Predict Inpatient Pediatric Mortality at Four Public Hospitals in Uganda"),
                                    br(),em("Arthur Mpimbaza , David Sears, Asadu Sserwanga, Ruth Kigozi, Denis Rubahika, Adam Nadler, Adoke Yeka, Grant Dorsey"),
                                    br(),"Published: July 28, 2015DOI: 10.1371/journal.pone.0133950.", strong("journal PLOS One.")),br(),
                                        p("This was a dataset with more than 50,000 observations in 4 Uganda's hospitals assessing for signs and symptoms which would increase in-patient pediatric death.")),
                                
                                column(3,
                                p("In this whole documentation the Authors Score is the Score proposed by the authors of the article.
                                Custom Score is the Score the user will interactively make on-line on the app.
                                Logit Score will be referring to logistic regression used with those variables to predict Death."),br(),
                                
                                p("Your Patient will be the example patient which the user will choose his signs and symptoms in the app's upper panel.",br(),"The app is divided in  3 main panels: Upper, Middle and Lower.",
                                br(), strong("The upper panel"),"will be where you can choose", em("your patient"), "signs and symptoms to check his Risk of Death.",
                                strong("The Middle Panel"),"is composed of the 3 panels which will be explained below.",strong(" The Lower Panel"),"Is where you can create your Custom Score.
                                You can create your Custom Score changing the weight for each sign and symptom and changing the thresholds values for the low risk group and high risk group.
                                Remember that a Score should be used in daily practice by physician and should be easy to apply and interpret.")),
                                                
                                column(3,
                                p(strong("The first Middle panel (risk of Dying):"),
                                "In the first panel, there is a table and 3 text outputs which will show  each predictor's (Custom Score, Author Score and Logit) value, Risk group and Death Risk for your patient.
Clicking in more Signs and Symptoms will change this values. If there is no value showing, this is due to the fact that for this specific Score or Probability there were no observations in the Dataset.
Do not forget to create and change the values of your Custom Score to check how it changes the risk classification of your patient."),br(),
                                p(strong("The Second Middle panel (ROC curve):"),
                        "3 graphs, showing the ROC curve for each predictor and a 4th graph showing all ROC curves together.
                        Check how changing your custom score will change your ROC curve, try to get a good AUC and a high specificty for your Low Risk Threshold and high sensibility for your High Risk Threshold.
                        The dots in the graphs are the 'best' value, best summed sensitivity and specificity, with exception to the Custom Score, which will have the Custom Lower and Higher Threshold marked in the graph.")),
                                        
                                column(3,
                                p(strong("The Third Middle Panel (Risk Distribution):"),
                                "In this panel the user can see a summary for each value of the Score/ Logit Prob. of the data set, showing the mortality rate of this score, the number of observations and number of deaths and the risk group associated with this value.
                                This can be seen both in a table and in a graph form." ,br(),"As the user change the values in the Custom score the risk distribution will change.",br(),
                                "The patient which the user choose his signs and symptoms will have a Score/Prob and this will be highlited in Yellow in the graph. Changing the signs and symptoms will change his Score/Prob and respectevily his classification in the graph."),
                                br(),
                                p("You can choose which predictor risk distribution to view in the right side of the panel with the radial buttons."),br(),
                                    p(em("This app should not be used to assess real risks since the population of the study is very specific with a high mortality rate and a doctor should always be consulted.")))))),
                    
          
        
        hr(),
        h3("Create your score"),
        fluidRow(
                
                column(3,sliderInput("age.less.4.months", "Age less than 4 months", 
                                     min=0, max=10, value=0),
                       sliderInput("temp.less.35.5", "Temperature lower than 35.5C", 
                                   min=0, max=10, value=0),
                       sliderInput("no.subjective.fever", "Did not have fever", 
                                   min=0, max=10, value=0),
                          sliderInput("pallor", "Pallor", 
                            min=0, max=10, value=0),
                sliderInput("jaundice", "Jaundice", 
                           min=0, max=10, value=0)),
                column(3,
                               
                              
                               
                               sliderInput("difficulty.breathing", "Difficulty Breathing", 
                                           min=0, max=10, value=0),
                               sliderInput("deep.breathing", "Deep Breathing", 
                                           min=0, max=10, value=0),
                       sliderInput("convulsions", "Convulsions", 
                                   min=0, max=10, value=0),
                       sliderInput("unable.to.sit", "Unable to Sit", 
                                   min=0, max=10, value=0),
                sliderInput("unable.to.drink", "Unable to Drink", 
                            min=0, max=10, value=0)),
                column(3,
                              
                               sliderInput("altered.consciousness", "Altered Consciousness", 
                                           min=0, max=10, value=0),
                               sliderInput("unconsciousness", "Unconsciousness", 
                                           min=0, max=10, value=0),
                               sliderInput("meningeal.signs", "Meningeal Signs", 
                                           min=0, max=10, value=0),
                helpText("Create your Score mixing the variables weights. Then, select the signs and symptoms your patient present,
                                        to predict his risk of death with your new score.")),
                               
                column(2,
                       h4("Set your Score limits"),
                       helpText("Select your risk thresholds"),
                       numericInput("low.risk", "low risk = Less than:", 
                                   min=0, max=130, value=1,step=1),
                       numericInput("high.risk", "high hisk = More than:", 
                                   min=0, max=130, value=5,step=1),
                
                        actionButton("update", "Create Score"))),
        
        fluidRow( "dataset from:",strong("Admission Risk Score to Predict Inpatient Pediatric Mortality at Four Public Hospitals in Uganda"),
                  br(),em("Arthur Mpimbaza , David Sears, Asadu Sserwanga, Ruth Kigozi, Denis Rubahika, Adam Nadler, Adoke Yeka, Grant Dorsey"),
                  br(),"Published: July 28, 2015DOI: 10.1371/journal.pone.0133950.", strong("journal PLOS One."),a("http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133950#sec011"))
        
                
                
        
        
        
        
        
        )#fluidpage
        )#server