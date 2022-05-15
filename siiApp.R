# Task: Create a data visualization for SII-IIS
# Authors: Suhayl Sayed
# Date: 2022-04-13

# Notes
# Here is a tutorial for RShiny: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
# For examples of selectizeInput, go to https://shiny.rstudio.com/gallery/selectize-examples.html (ui.R tab)
# To create a hierarchy of buttons, use conditional panels (https://shiny.rstudio.com/reference/shiny/1.4.0/conditionalPanel.html)
# For graphing help, refer to: https://plotly.com/r/. Note that traces, values to be plotted x, have to be columns.
# For choropleth/leaflet help, refer to: https://rstudio.github.io/leaflet/json.html

#---------------------------------------------------------------------
# Install packages
#install.packages("plotly")
#install.packages("leaflet")
#install.packages("shinyjs")
#---------------------------------------------------------------------


# Load packages
library(shiny)
library(tidyverse)
library(data.table)
library(readxl)
library(plotly)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(shinyjs)

# Define the js method that resets the page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" 


# Data loading and preprocessing------------------------------------------------
# Read and rename data for Participation, Employment and Unemployment rates for VisMin and Sex Tabs

# Read and rename data for Education data
confidenceDT <- read_csv("4310006201_databaseLoadingData.csv") %>% 
  select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
           "Indicators", "Statistics", "VALUE"))

setnames(confidenceDT, colnames(confidenceDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))

# Read and rename data for Education data
discriminationDT <- read_csv("4310006101_databaseLoadingData (2).csv") %>% 
  select(c("REF_DATE", "GEO", "Visible minority", "Selected sociodemographic characteristics", 
           "Indicators", "Characteristics", "VALUE"))

setnames(discriminationDT, colnames(discriminationDT), c("Year", "Geography", "VisMin", "Characteristic", "Indicator", "Confidence","Value"))

# Read and rename data for Education data
educationDT <- read_csv("4310006701_databaseLoadingData (6).csv") %>% 
  select(c("REF_DATE", "GEO", "Sex", "Age group", 
           "First official language spoken", "Immigrant and generation status", "Visible minority status", "Indicators", 
           "VALUE"))

setnames(educationDT, colnames(educationDT), c("Year", "Geography", "Sex", "Age", "Language", "Immigration", "VisMin", "Indicators", "Value"))




OverQualDT <- read_csv("4310007101_databaseLoadingData.csv") %>% 
  select(c("REF_DATE", "GEO", "Sex", "Age group", 
           "First official language spoken", "Immigrant and generation status", "Visible minority status", "Location of study","Highest certificate, diploma or degree", "Indicators", 
           "VALUE"))

setnames(OverQualDT, colnames(OverQualDT), c("Year", "Geography", "Sex", "Age", "Language", "Immigration", "VisMin", "Location", "Degree", "Indicators", "Value"))






#Load Dataset for Police-reported hate crime
polData <- read.csv("3510006601_databaseLoadingData (2).csv") %>%
select(c("REF_DATE", "GEO",4, "VALUE"))
setnames(polData, colnames(polData), c("Year", "Geography", "Motivation", "Value"))




# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  # Include shinyjs in the UI
  useShinyjs(),
  
  # Title of the app
  titlePanel("Social Inclusion Data Visualization Tool"),
  
  #Changes color of tabpanel from standard blue to black
  tags$style(type = "text/css", "a{color: #000000;}", style = "font-size:70px"),
 

  
  
  
# Create the tab panel
  tabsetPanel(
              
    
    # Commented on only the first tab, but follow this layout to create more
    # Bar Graphs by VisMin Tab
    #Tab for Geography
    tabPanel("Theme & Definition of Indicators", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectizeInput("dim4",
                                label = "Theme",
                                choices = list("Participation in the Labour Market")
                                
                 ),
                 
                 conditionalPanel(
                   condition = "input.dim4 == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM4",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Working-age population in the labour force (participation rate)",
                                    
                                    "Working-age population in employment (employment rate)",
                                    
                                    "Working-age population in unemployment (unemployment rate)",
                                    
                                    "Workers working mainly full-time weeks in the previous year",
                                    
                                    "Self-employed workers in the labour force (unincorporated)",
                                    
                                    "Overqualified workers with a university degree",
                                    
                                    "Youth not in employment, education or training (NEET)",
                                    
                                    "Average employment income of the population",
                                    
                                    "Average weekly wage of paid employees",
                                    
                                    "Currently employed population considering their job related to their education",
                                    
                                    "Paid employees considering their current job good for career advancement",
                                    
                                    "Paid employees receiving at least one employment benefit in their current job",
                                    
                                    "Paid employees having pension plan in their current job",
                                    
                                    "Paid employees having paid sick leave in their current job",
                                    
                                    "Paid employees having paid vacation leave in their current job",
                                    
                                    "Paid employees having disability insurance in their current job",
                                    
                                    "Paid employees having supplemental medical care in their current job",
                                    
                                    "Paid employees having worker's compensation in their current job",
                                    
                                    "Paid employees having maternity, paternity or lay-off benefits in their current job",
                                    
                                    "Paid employees covered by union contract or collective agreement in their current job",
                                    
                                    "Paid employees receiving formal training in their current job",
                                    
                                    "Paid employees receiving informal training in their current job"
                                    
                                    
                                  ),
                                  selected = NULL
                   ),
                   
                   
                
                   
                 ),
                 
                 
                 
                 # Add the js code and button to the page
                 #extendShinyjs(text = jsResetCode, functions = "reset"),
                 actionButton("reset_button", "Reset Page"),
               ),
               
               #Main Panel for displaying graphs
               mainPanel(
                 conditionalPanel(
                   
                   condition = "input.dim4 == 'Participation in the Labour Market' & input.LM4 == 'Overqualified workers with a university degree'",
                   
              br(),
                   h4("Overqualified workers with a university degree"),
                   
                   helpText("Refers to people with a bachelor’s degree or above (at bachelor's level or above) who, during the current year or the year prior the census, held a position usually requiring a high school diploma or equivalency certificate or less.")
                   
                 )
                 
                 
               )
             )
             
    ),
       
   
    tabPanel("Groups Designated as Visible Minorities", fluid = TRUE, font = list(size = 10),
             sidebarLayout(
               
               # Specify your widgets here
               sidebarPanel(
                 
                 selectizeInput("dim",
                                label = "Theme",
                                choices = list("Participation in the Labour Market",
                                               "Representation in leadership and decision-making positions",
                                               "Civic engagement and political participation",
                                               
                                               "Basic needs and housing",
                                               "Health, well-being and outlook on life",
                                               "Income and wealth",
                                               "Social connections and networks",
                                               "Trust, confidence in institutions and sense of belonging",
                                               
                                               "Education, training and skills",
                                               "Local community",
                                               "Access to public services and institutions",
                                               "Discrimination"
                                               
                                               
                                               )
                                
                 ),
                 # Widgets for use with datasets with Labour Market Variables
                 conditionalPanel(
                   condition = "input.dim == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM",
                                  label = "Indicators",
                                  choices = list(
                                                 
                                                 "Working-age population in the labour force (participation rate)",
                                                 
                                                 "Working-age population in employment (employment rate)",
                                                 
                                                 "Working-age population in unemployment (unemployment rate)",
                                                 
                                                 "Workers working mainly full-time weeks in the previous year",
                                                 
                                                 "Self-employed workers in the labour force (unincorporated)",
                                                 
                                                 "Overqualified workers with a university degree",
                                                 
                                                 "Youth not in employment, education or training (NEET)",
                                                 
                                                 "Average employment income of the population",
                                                 
                                                 "Average weekly wage of paid employees",
                                                 
                                                 "Currently employed population considering their job related to their education",
                                                 
                                                 "Paid employees considering their current job good for career advancement",
                                                 
                                                 "Paid employees receiving at least one employment benefit in their current job",
                                                 
                                                 "Paid employees having pension plan in their current job",
                                                 
                                                 "Paid employees having paid sick leave in their current job",
                                                 
                                                 "Paid employees having paid vacation leave in their current job",
                                                 
                                                 "Paid employees having disability insurance in their current job",
                                                 
                                                 "Paid employees having supplemental medical care in their current job",
                                                 
                                                 "Paid employees having worker's compensation in their current job",

                                                  "Paid employees having maternity, paternity or lay-off benefits in their current job",

                                                  "Paid employees covered by union contract or collective agreement in their current job",

                                                  "Paid employees receiving formal training in their current job",

                                                  "Paid employees receiving informal training in their current job"

                                                 
                                                 ),
                                  selected = NULL
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.LM == 'Overqualified workers with a university degree'",
                     
                     
                     
                     
                     selectizeInput("VM20",
                                    label = "Visible Minority Status",
                                    choices = unique(OverQualDT$VisMin),
                                    selected = list("Total - Visible minority", "Not a visible minority"),
                                    multiple = TRUE
                     ),
                     
                   
                     selectizeInput("OverLocation",
                                    label = "Location of Study",
                                    choices = unique(OverQualDT$Location),
                                    
                                    
                     ),
                     
                     selectizeInput("OverDegree",
                                    label = "Highest certificate, diploma or degree",
                                    choices = unique(OverQualDT$Degree),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverGeo",
                                    label = "Geography",
                                    choices = unique(OverQualDT$Geography),
                                    selected = "Canada"
                                    
                     ),
                     
                     selectizeInput("OverImm",
                                    label = "Immigration and Generational Status",
                                    choices = unique(OverQualDT$'Immigration'),
                                    selected = "Canada"
                                    
                     ),
                     
                     
                     selectizeInput("OverYear",
                                    label = "Year",
                                    choices = unique(OverQualDT$Year),
                                    
                     ),
                     
                     selectizeInput("OverAge",
                                    label = "Age Group",
                                    choices = unique(OverQualDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("OverSex",
                                    label = "Sex",
                                    choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
                                    selected = "Total - Sex"
                     ),
                     
                     selectizeInput("OverLang",
                                    label = "Language",
                                    choices = unique(OverQualDT$'Language'),
                                    
                     )
                   ),
   
                 ),
                 
            
                 
                 #Widgets for Representation Variables
                 conditionalPanel(
                   
                   condition = "input.dim == 'Representation in leadership and decision-making positions'",
                   
                   selectizeInput("Rep",
                                  label = "Indicators",
                                  choices = list("Percent of workers in all management occupations",
                                             
                                             "Percent of workers in senior management occupations",
                                             
                                             "Percent of workers in specialized middle management occupations",
                                             
                                             "Percent of workers in other middle management occupations"
                                  )
                   ),
                   
                 ),
                 
                 
                 # Widgets for use with datasets with Discrimination Variables
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination'",
                   
                   
                   selectizeInput("disind",
                                  label = "Discrimination Indicators",
                                  choices = list(
                                    "Experience(s) of discrimination",
                                    
                                    "Experience(s) of discrimination based on ethnicity or culture",
                                    
                                    "Experience(s) of discrimination based on race or colour",
                                    
                                    "Experience(s) of discrimination based on religion",
                                    
                                    "Experience(s) of discrimination based on language",

                                    "Discrimination at work or when applying for a job or promotion",
                                    
                                    "Discrimination when dealing with the police",
                                    
                                    "Discrimination when in a store, bank or restaurant",
                                    
                                    "Discrimination when attending school or classes",
                          
                               
                                    
                                    "Hate Crime"
                                    
                                    
                                    
                                    
                                  )
                   ),
                   
                   conditionalPanel(
                     
                     condition = "input.disind == 'Experience(s) of discrimination based on ethnicity or culture' || input.disind == 'Experience(s) of discrimination'",
                   
                     selectizeInput("covYear",
                                    label = "Year",
                                    choices = unique(discriminationDT$'Year')
                     ),
                     
                     selectizeInput("covGeo",
                                    label = "Geography",
                                    choices = unique(discriminationDT$'Geography')
                     ),
                     
                     
                     
                     
                     selectizeInput("VM30",
                                    label = "Visible Minority Status",
                                    choices = unique(discriminationDT$VisMin),
                                    selected = list("Black"),
                                    multiple = TRUE
                     ),
                     
                     selectizeInput("covCharacteristics",
                                    label = "Selected sociodemographic characteristics",
                                    choices = list('Age', 'Sex', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                    
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Age'",
                       
                       selectizeInput("covCharSpecAge",
                                      label = "Age",
                                      choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                      
                       ),
                       
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Sex'",
                       
                       selectizeInput("covCharSpecSex",
                                      label = "Sex",
                                      choices = list('Man', 'Woman'),
                                      
                       ),
                       
                     ),
                     
                     
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Immigration Status'",
                       
                       selectizeInput("covCharSpecImm",
                                      label = "Immigration Status",
                                      choices = list('Immigrants', 'Non-Immigrants'),
                                      
                       ),
                       
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Generation Status'",
                       
                       selectizeInput("covCharSpecGen",
                                      label = "Immigration Status",
                                      choices = list( 'First generation', 'Second generation', 'Third-plus generation'),
                                      
                       ),
                       
                     ),
                     
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Language Spoken'",
                       
                       selectizeInput("covCharSpecLang",
                                      label = "Language Spoken",
                                      choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                      
                       ),
                       
                     ),
                     
                     
                     conditionalPanel(
                       
                       condition = "input.covCharacteristics == 'Education Status'",
                       
                       selectizeInput("covCharSpecEdu",
                                      label = "Language Spoken",
                                      choices = list('High school diploma or a high school equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                      
                       ),
                       
                     ),
                     
                     selectizeInput("covConfidence",
                                    label = "Confidence Interval",
                                    choices = unique(discriminationDT$'Confidence')
                     ),
                     
                     
                     
                    
                       
                   ),
                   
                   
                   conditionalPanel(
                     
                     condition = "input.disind == 'Hate Crime'",
                   selectizeInput("disYear",
                                  label = "Year",
                                  choices = unique(polData$'Year')
                   ),
                   
                   selectizeInput("motivation",
                                  label = "Motivation",
                                  choices = list('Race or ethnicity', 'Total police-reported hate crime'),
                                  selected = "Other",
                                  
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.motivation == 'Total police-reported hate crime'",
                     
                     selectizeInput("VM8",
                                    label = "Race or ethnicity and other characteristics",
                                    choices = list('Race or ethnicity', 'Religion', 'Sexual orientation', 'Language', 'Disabilitiy', 'Sex', 'Age', 'Unknown motivation'),
                                    selected = "Religion",
                                    multiple = TRUE
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.motivation == 'Race or ethnicity'",
                     
                     selectizeInput("VM7",
                                    label = "Groups designated as Visible Minority",
                                    choices = list('Black', 'South Asian', 'East or Southeast Asian', 'Arab or West Asian', 'White', 'Indigenous', 'Multiple races or ethnicities', 'Other Race or ethnicity', 'Unknown Race or ethnicity' ),
                                    selected = "Black",
                                    multiple = TRUE
                     ),
                     
                                   ),
                   
            
                 
                   
                   
                   
                   selectizeInput("geo_dis",
                                  label = "Geography",
                                  choices = list("Canada")
                   )
                   
                 ),
                 
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Civic engagement and political participation'",
                   
                   selectizeInput("dimCivilEngagement",
                                  label = "Indicators",
                                  choices = list(
                                    "Percent of the population members of at least one civic group or organization",
                                    
                                    "Percent of the population members in a sports or recreational organization",
                                    
                                    "Percent of the population members in a cultural, educational or hobby organization",
                                    
                                    "Percent of the population members in union or professional association",
                                    
                                    "Percent of the population members in a political party or group",
                                    
                                    "Percent of the population members in a religious-affiliated group",
                                    
                                    "Percent of the population members in a school group, neighbourhood, civic or community association",
                                    
                                    "Percent of the population members in a humanitarian or charitable organization or service club",
                                    
                                    "Percent of the population members in a seniors' group",
                                    
                                    "Percent of the population members in a youth organization",
                                    
                                    "Percent of the population members in an immigrant or ethnic association or club",
                                    
                                    "Percent of the population members in an environmental group ",
                                    
                                    "Percent of the population engaged in political activities",
                                    
                                    "Percent of the population voting in the last federal election",
                                    
                                    "Percent of the population voting in the last provincial election",
                                    
                                    "Percent of the population voting in the last municipal election"
                                    
                                  )
                   )
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Basic needs and housing'",
                   
                   selectizeInput("dimBasicNeeds",
                                  label = "Indicators",
                                  choices = list(
                                    "Percent of the population living in a dwelling owned by one member of the household ",
                                    
                                    "Percent of the population living in core need household",
                                    
                                    "Percent of the population living in suitable housing",
                                    
                                    "Percent of the population living in an affordable housing",
                                    
                                    "Percent of the population living in a food-secure household",
                                    
                                   " Percent of the population living in a household with marginal food security",
                                    
                                    "Percent of the population living in a food-insecure household, moderate or severe",
                                    
                                    "Percent of the population living in a household with moderate food insecurity",
                                    
                                   " Percent of the population living in a household with severe food insecurity"
                                    
                                    
                                  )
                   )
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Health, well-being and outlook on life'",
                   
                   selectizeInput("dimHealth",
                                  label = "Indicators",
                                  choices = list(
                                   " Percent of the population reporting very good or excellent general health",
                                    
                                    "Percent of the population reporting fair or poor general health",
                                    
                                    "Percent of the population reporting very good or excellent mental health",
                                    
                                    "Percent of the population reporting fair or poor mental health",
                                    
                                    "Percent of the population reporting their life stressful",
                                    
                                    "Percent of the population satisfied with life as a whole",
                                    
                                    "Percent of the population predicting their life opportunities will improve in the next 5 years"
                                    
                                    
                                    
                                  )
                   )
                   
                 ),
                 
                 
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Income and wealth'",
                   
                   selectizeInput("dimIncome",
                                  label = "Indicators",
                                  choices = list(
                                    "Average total household income, adjusted for the number of persons",
                                    
                                    "Percent of the population living in poverty (low-income MBM)",
                                    
                                    "Percent of the population living in low income situation (before-tax)",
                                    
                                    "Percent of the population living in low income situation (after-tax)",
                                    
                                    "Percent of the population reporting difficulty in meeting financial needs of their household",
                                    
                                    "Percent of the population reporting ease in meeting financial needs of their household"
                                    
                                  )
                   )
                   
                 ),
              
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Social connections and networks'",
                   
                   selectizeInput("dimSocial",
                                  label = "Indicators",
                                  choices = list(
                                    "Percent of the population living alone",
                                    
                                    "Median size of a personal local network with close ties", 
                                    
                                    "Average size of a local personal network with close ties",
                                    
                                    "Percent of the population with a personal close-ties network of 10 or more people",
                                    
                                    "Percent of the population with a personal close-ties network of 5 or more relatives",
                                    
                                    "Percent of the population with a personal close-ties network of 5 or more friends",
                                    
                                    "Percent of the population with no personal network with weak ties",
                                    
                                    "Percent of the population with a personal weak-ties network of 1 to 19 people",
                                    
                                    "Percent of the population with a personal weak-ties network of 20 or more people ",
                                    
                                    "Percent of the population with a personal ethnically-diverse network"
                                    
                                    
                                  )
                   )
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging'",
                   
                   selectizeInput("dimTrust",
                                  label = "Indicators",
                                  choices = list(
                                   " Population reporting that most people can be trusted",
                                    
                                    "Population expressing confidence in Federal Parliament",
                                    
                                    "Population expressing confidence in the Canadian media",
                                    
                                    "Population expressing confidence in the school system", 
                                    
                                    "Population expressing confidence in the justice system, courts",
                                    
                                    "Population expressing confidence in the police",
                                    
                                    "Population expressing confidence in major corporations",
                                    
                                    "Population expressing confidence in merchants and business people",
                                    
                                    "Population reporting strong sense of belonging to their local community",
                                    
                                    "Population reporting strong sense of belonging to their town or city",
                                    
                                    "Population reporting strong sense of belonging to their province",
                                    
                                    "Population reporting strong sense of belonging to Canada",
                                    
                                    "Population expressing confidence in banks"
                                    
                                    
                                    
                                  )
                   ),
                   
                   
                  
                     
                     selectizeInput("confYear",
                                    label = "Year",
                                    choices = unique(confidenceDT$'Year')
                     ),
                     
                     selectizeInput("confGeo",
                                    label = "Geography",
                                    choices = unique(confidenceDT$'Geography')
                     ),
                     
                     
                     
                     
                     selectizeInput("VM40",
                                    label = "Visible Minority Status",
                                    choices = unique(confidenceDT$VisMin),
                                    selected = list("Black"),
                                    multiple = TRUE
                     ),
                     
                     selectizeInput("confCharacteristics",
                                    label = "Selected sociodemographic characteristics",
                                    choices = list('Age', 'Sex', 'Immigration Status', 'Generation Status', 'Language Spoken', 'Education Status'),
                                    
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Age'",
                       
                       selectizeInput("confCharSpecAge",
                                      label = "Age",
                                      choices = list('Total, 15 years and over', '15 to 24 years', '25 to 64 years', '65 years and over'),
                                      
                       ),
                       
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Sex'",
                       
                       selectizeInput("confCharSpecSex",
                                      label = "Sex",
                                      choices = list('Men', 'Women'),
                                      
                       ),
                       
                     ),
                     
                     
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Immigration Status'",
                       
                       selectizeInput("confCharSpecImm",
                                      label = "Immigration Status",
                                      choices = list('Immigrants', 'Non-Immigrants'),
                                      
                       ),
                       
                     ),
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Generation Status'",
                       
                       selectizeInput("confCharSpecGen",
                                      label = "Immigration Status",
                                      choices = list( 'First generation', 'Second generation', 'Third-plus generation'),
                                      
                       ),
                       
                     ),
                     
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Language Spoken'",
                       
                       selectizeInput("confCharSpecLang",
                                      label = "Language Spoken",
                                      choices = list('First official language spoken, English only', 'First official language spoken, French only'),
                                      
                       ),
                       
                     ),
                     
                     
                     conditionalPanel(
                       
                       condition = "input.confCharacteristics == 'Education Status'",
                       
                       selectizeInput("confCharSpecEdu",
                                      label = "Language Spoken",
                                      choices = list('High school diploma or a high school equivalency certificate or less', 'Postsecondary certicate or diploma (non-university)', 'University certificate or diploma'),
                                      
                       ),
                       
                     ),
                     
                     selectizeInput("confConfidence",
                                    label = "Confidence Interval",
                                    choices = unique(confidenceDT$'Confidence')
                     ),
                     
                     
                     
                     
                
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills'",
                   
                   selectizeInput("dimEducation4",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Population with no certificate, diploma or degree",
                                    
                                    "Population with high school diploma or equivalency certificate",
                                    
                                    "Population with postsecondary certificate or diploma below bachelor level",
                                    
                                    "Population with bachelor’s degree or above",
                                    
                                    "Population with bachelor’s degree",
                                    
                                    "Population with master’s degree or earned doctorate",
                                    
                                    "Knowledge of official languages, English only",
                                    
                                    "Knowledge of official languages, French only",
                                    
                                    "Knowledge of official languages, English and French",
                                    
                                    "Knowledge of official languages, neither English nor French",
                                    
                                    "Received a formal training paid by the employer in the past 12 months",
                                    
                                    "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"

                                    
                                  )
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.dimEducation4 == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                     
                     
                     
                     
                     selectizeInput("VM10",
                                    label = "Visible Minority Group",
                                    choices = unique(educationDT$VisMin),
                                    selected = list("Total - Visible minority", "Not a visible minority"),
                                    multiple = TRUE
                     ),
                     
                     
                     
                     selectizeInput("eduGeo2",
                                    label = "Geography",
                                    choices = unique(educationDT$Geography),
                                    selected = "Canada"
                                    
                     ),
                     
                     selectizeInput("eduVisMin2",
                                    label = "Immigration and Generational Status",
                                    choices = unique(educationDT$'Immigration'),
                                    selected = "Canada"
                                    
                     ),
                     
                     
                     selectizeInput("eduYear2",
                                    label = "Year",
                                    choices = unique(educationDT$Year),
                                    
                     ),
                     
                     selectizeInput("eduAge2",
                                    label = "Age Group",
                                    choices = unique(educationDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("eduSex2",
                                    label = "Sex",
                                    choices = sort(unique(educationDT$Sex), decreasing = TRUE),
                                    selected = "Total - Sex"
                     ),
                     
                     selectizeInput("eduLang2",
                                    label = "Language",
                                    choices = unique(educationDT$'Language'),
                                    
                     )
                   ),
                   
                 ),
                 
                
                 
                 
                 
                 
                 
                 
              
                 
                 
                 
                 
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Local community'",
                   
                   selectizeInput("dimCommunity",
                                  label = "Indicators",
                                  choices = list(
                                    "Percent of the population satisfied with feeling part of their community",
                                    
                                    "Percent of the population satisfied with their neighbourhood",
                                    
                                    "Percent of the population satisfied with quality of local environment",
                                    
                                    "Percent of the population reporting feeling safe in their neighbourhood",
                                    
                                    "Percent of the population satisfied with personal safety from crime",
                                    
                                    "Violent victimization rate per 1,000 population",
                                    
                                    "Percent of the population reporting incident against them was a hate crime", 
                                    
                                    "Percent of police reported hate crimes motivated by Race or ethnicity",
                                    
                                    "Percent of police reported hate crimes motivated by religion",
                                    
                                    "Percent of the population perceiving local police good in enforcing laws",
                                    
                                    "Percent of the population perceiving local police good in responding to calls",
                                    
                                    "Percent of the population perceiving local police good in being approachable",
                                    
                                    "Percent of the population perceiving local police good in supplying information",
                                    
                                    "Percent of the population perceiving local police good in ensuring safety in the area",
                                    
                                    "Percent of the population perceiving local police as treating people fairly"
                                    
                                    
                                    
                                    
                                    
                                  )
                   )
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Access to public services and institutions'",
                   
                   selectizeInput("dimPublicServices",
                                  label = "Indicators",
                                  choices = list(
                                    "Percent of the population reporting having regular health care provider",
                                    
                                    "Percent of the population reporting unmet healthcare needs",
                                    
                                    "Percent of the population reporting no need for mental health care",
                                    
                                    "Percent of the population reporting needs for mental health care not met or partially met",
                                    
                                    "Percent of the population reporting needs for mental health care not met",
                                    
                                    "Percent of the population reporting needs for mental health partially met",
                                    
                                    "Percent of the population reporting all needs for mental health care met"
                                    
                                    
                                    
                                  )
                   )
                   
                 ),
                 
             
                 
                 
                 
                 # Add the js code and button to the page
                # extendShinyjs(text = jsResetCode, functions = "reset"),
                 
                 actionButton("reset_button", "Reset Page"),
               ),
               # This is the main panel
               # Visuals will be displayed here
               # You can also add text, images, etc. here
               mainPanel(
                 
                 h2("Groups Designated as Visible Minorities"),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Immigration Status'" ,
                   br(),
        
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
             
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCov", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCov2", inline = TRUE, width = 400, height = 500))
                   ),

                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Age'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovAge", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovAge2", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Sex'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovSex", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovSex2", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Generation Status'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovGen", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovGen2", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Language Spoken'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovLang", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovLang2", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination based on ethnicity or culture' &  input.covCharacteristics == 'Education Status'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination based on ethnicity or culture"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovEdu", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovEdu2", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 ###
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Immigration Status'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCov1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCov21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Age'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovAge1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovAge21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Sex'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovSex1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovSex21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Generation Status'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovGen1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovGen21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Language Spoken'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovLang1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovLang21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Experience(s) of discrimination' &  input.covCharacteristics == 'Education Status'" ,
                   br(),
                   
                   h3("Experience(s) of discrimination"),
                   br(),
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sBarCovEdu1", inline = TRUE, width = 400, height = 500), plotlyOutput("sBarCovEdu21", inline = TRUE, width = 400, height = 500))
                   ),
                   
                   helpText("Source: General Social Survey (GSS), cycle 35, Social Identity. Years available: 2020")
                   
                 ),
                 
                 
                 ###
                 
        
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Hate Crime' & input.motivation == 'Race or ethnicity'",
                   
                   br(),
                   br(),
                   plotlyOutput("immdisPlot", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Note: Uniform Crime Reporting Survey (UCR) data are collected 
                            directly from survey respondents (Police Services) and extracted 
                            from administrative files. The categories that appear in this chart 
                            are those used by police services when reporting on hate crime incidences.")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the police' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 
                 
                 ##
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in Federal Parliament' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 
                 ##
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the Canadian media' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 ##
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the school system' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 ##
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in the justice system, courts' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 ##
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in major corporations' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 ##
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Immigration Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConf6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Age'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfAge6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Sex'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfSex6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Generation Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfGen6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Language Spoken'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfLang6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 conditionalPanel(
                   
                   condition = "input.dim == 'Trust, confidence in institutions and sense of belonging' & input.dimTrust == 'Population expressing confidence in merchants and business people' & input.confCharacteristics == 'Education Status'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarConfEdu6", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, General Social Survey, Social Identity, 2020")
                   
                 ),
                 
                 ##
                
                 
                 
             
              
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Participation in the Labour Market' & input.LM == 'Overqualified workers with a university degree'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarOver", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                   
                 ),
                 
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarEduVM", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                conditionalPanel(
                   
                  condition = "input.dim == 'Education, training and skills' && input.dimEducation4 == 'Population with no certificate, diploma or degree'",
                   
                   br(),
                  br(),
                   plotlyOutput("sBarEduVM1", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with high school diploma or equivalency certificate'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarEduVM2", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with postsecondary certificate or diploma below bachelor level'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarEduVM3", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with bachelor’s degree or above'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarEduVM4", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 
                 conditionalPanel(
                   
                   condition = "input.dim == 'Education, training and skills' & input.dimEducation4 == 'Population with master’s degree or earned doctorate'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarEduVM5", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 
                 
                 
                
                 
                 #Visuals for Discrimination Variables
                 conditionalPanel(
                   
                   condition = "input.dim == 'Discrimination' & input.disind == 'Hate Crime' && input.motivation == 'Total police-reported hate crime'",
                   
                   br(),
                   br(),
                   plotlyOutput("immdis2Plot", inline = TRUE, width = 700, height = 500),
                   helpText("Note: Uniform Crime Reporting Survey (UCR) data are collected 
                            directly from survey respondents (Police Services) and extracted 
                            from administrative files. The categories that appear in this chart 
                            are those used by police services when reporting on hate crime incidences.")
                 )
               )
             )
    ),
    
    
    
    #Tab for Immigrant Status Analysis
    tabPanel("Immigration Status", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
          
                 
                 selectizeInput("dim2",
                                label = "Theme",
                                choices = list(
                                               
                                               "Education, training and skills",
                                               "Participation in the Labour Market"
                                              
                                               
                                               
                                )
                                
                 ),
                 
                 
                 # Widgets for use with datasets with Labour Market Variables
                 conditionalPanel(
                   condition = "input.dim2 == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM2",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Working-age population in the labour force (participation rate)",
                                    
                                    "Working-age population in employment (employment rate)",
                                    
                                    "Working-age population in unemployment (unemployment rate)",
                                    
                                    "Workers working mainly full-time weeks in the previous year",
                                    
                                    "Self-employed workers in the labour force (unincorporated)",
                                    
                                    "Overqualified workers with a university degree",
                                    
                                    "Youth not in employment, education or training (NEET)",
                                    
                                    "Average employment income of the population",
                                    
                                    "Average weekly wage of paid employees",
                                    
                                    "Currently employed population considering their job related to their education",
                                    
                                    "Paid employees considering their current job good for career advancement",
                                    
                                    "Paid employees receiving at least one employment benefit in their current job",
                                    
                                    "Paid employees having pension plan in their current job",
                                    
                                    "Paid employees having paid sick leave in their current job",
                                    
                                    "Paid employees having paid vacation leave in their current job",
                                    
                                    "Paid employees having disability insurance in their current job",
                                    
                                    "Paid employees having supplemental medical care in their current job",
                                    
                                    "Paid employees having worker's compensation in their current job",
                                    
                                    "Paid employees having maternity, paternity or lay-off benefits in their current job",
                                    
                                    "Paid employees covered by union contract or collective agreement in their current job",
                                    
                                    "Paid employees receiving formal training in their current job",
                                    
                                    "Paid employees receiving informal training in their current job"
                                    
                                    
                                  ),
                                  selected = NULL
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.LM2 == 'Overqualified workers with a university degree'",
                     
                     
                     
                     
                     selectizeInput("VM21",
                                    label = "Immigration and Generational Status",
                                    choices = unique(OverQualDT$Immigration),
                                    selected = list("Black"),
                                    multiple = TRUE
                     ),
                     
                     
                     selectizeInput("OverLocationIS",
                                    label = "Location of Study",
                                    choices = unique(OverQualDT$Location),
                                    
                                    
                     ),
                     
                     selectizeInput("OverDegreeIS",
                                    label = "Highest certificate, diploma or degree",
                                    choices = unique(OverQualDT$Degree),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverGeoIS",
                                    label = "Geography",
                                    choices = unique(OverQualDT$Geography),
                                    selected = "Canada"
                                    
                     ),
                     
                     selectizeInput("OverVMIS",
                                    label = "Visible Minority Status",
                                    choices = unique(OverQualDT$VisMin),
                                   
                                    
                     ),
                     
                     
                     selectizeInput("OverYearIS",
                                    label = "Year",
                                    choices = unique(OverQualDT$Year),
                                    
                     ),
                     
                     selectizeInput("OverAgeIS",
                                    label = "Age Group",
                                    choices = unique(OverQualDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("OverSexIS",
                                    label = "Sex",
                                    choices = sort(unique(OverQualDT$Sex), decreasing = TRUE),
                                    selected = "Total - Sex"
                     ),
                     
                     selectizeInput("OverLangIS",
                                    label = "Language",
                                    choices = unique(OverQualDT$'Language'),
                                    
                     )
                   ),
                   
                 ),
                 
                 # Widgets for use with datasets with Labour Market Variables
                 conditionalPanel(
                   condition = "input.dim2 == 'Education, training and skills'",
                   
                   
                   selectizeInput("Edu",
                                  label = "Education Indicators",
                                  choices = list( "Population with no certificate, diploma or degree",
                                                  
                                                  "Population with high school diploma or equivalency certificate",
                                                  
                                                  "Population with postsecondary certificate or diploma below bachelor level",
                                                  
                                                  "Population with bachelor’s degree or above",
                                                  
                                                  "Population with bachelor’s degree",
                                                  
                                                  "Population with master’s degree or earned doctorate",
                                                  
                                                  "Knowledge of official languages, English only",
                                                  
                                                  "Knowledge of official languages, French only",
                                                  
                                                  "Knowledge of official languages, English and French",
                                                  
                                                  "Knowledge of official languages, neither English nor French",
                                                  
                                                  "Received a formal training paid by the employer in the past 12 months",
                                                  
                                                  "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"
                                                  
                                  ),
                                  selected = NULL
                   ),
                   
                   
               
                   conditionalPanel(
                     condition = "input.Edu == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                     
                     
                   
                     
                     selectizeInput("VM9",
                                    label = "Immigration and Generational Status",
                                    choices = unique(educationDT$'Immigration'),
                                    selected = list("Total - Visible minority", "Not a visible minority"),
                                    multiple = TRUE
                     ),
                     
                   
                     
                     selectizeInput("eduGeo",
                                    label = "Geography",
                                    choices = unique(educationDT$Geography),
                                    selected = "Canada"
                                    
                     ),
                     
                     selectizeInput("eduVisMin",
                                    label = "Visible Minority Group",
                                    choices = unique(educationDT$VisMin),
                                    selected = "Canada"
                                    
                     ),
                     
                     
                     selectizeInput("eduYear",
                                    label = "Year",
                                    choices = unique(educationDT$Year),
                                    
                     ),
                     
                     selectizeInput("eduAge",
                                    label = "Age Group",
                                    choices = unique(educationDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("eduSex",
                                    label = "Sex",
                                    choices = sort(unique(educationDT$Sex), decreasing = TRUE),
                                    selected = "Total - Sex"
                     ),
                     
                     selectizeInput("eduLang",
                                    label = "Language",
                                    choices = unique(educationDT$'Language'),
                                   
                     )
                   )
                   
                   
                   
                   
                   
                    
                ),
              
                 
               
                     
      
                   
                   
            
          
              
                
                   
                   
                   
                   
           
                   
                  
                
                 
                
            
                 
               
                 
                 
                 
              
                 
               ),
               mainPanel(
                 
                 h2("Immigration Status"),
                 
                 #Visuals related to Labour Market
                 conditionalPanel(
                   
                   condition = "input.dim2 == 'Education, training and skills'",
                   
             
                   conditionalPanel(
                     condition = "input.Edu == 'Population with bachelor’s degree'",
                     
                     br(),
                     br(),
                     plotlyOutput("sBarEdu", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.Edu == 'Population with no certificate, diploma or degree'",
                     br(),
                     br(),
                     plotlyOutput("sBarEdu1", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu == 'Population with high school diploma or equivalency certificate'",
                     br(),
                     br(),
                     plotlyOutput("sBarEdu2", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu == 'Population with postsecondary certificate or diploma below bachelor level'",
                     br(),
                     br(),
                     plotlyOutput("sBarEdu3", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu == 'Population with bachelor’s degree or above'",
                     br(),
                     br(),
                     plotlyOutput("sBarEdu4", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu == 'Population with master’s degree or earned doctorate'",
                     br(),
                     br(),
                     plotlyOutput("sBarEdu5", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   )
                   
                   
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim2 == 'Participation in the Labour Market' & input.LM2 == 'Overqualified workers with a university degree'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarOverIS", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                   
                 )
               )
               
             )
            ),
    
    #Bar graphs by Sex Tab
    tabPanel("Sex/Gender", fluid = TRUE,
             sidebarLayout(
               
               sidebarPanel(
                 
                 selectizeInput("dim3",
                                label = "Theme",
                                choices = list("Education, training and skills",
                                               "Participation in the Labour Market"
                                               
                                               )
                                
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.dim3 == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM3",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Working-age population in the labour force (participation rate)",
                                    
                                    "Working-age population in employment (employment rate)",
                                    
                                    "Working-age population in unemployment (unemployment rate)",
                                    
                                    "Workers working mainly full-time weeks in the previous year",
                                    
                                    "Self-employed workers in the labour force (unincorporated)",
                                    
                                    "Overqualified workers with a university degree",
                                    
                                    "Youth not in employment, education or training (NEET)",
                                    
                                    "Average employment income of the population",
                                    
                                    "Average weekly wage of paid employees",
                                    
                                    "Currently employed population considering their job related to their education",
                                    
                                    "Paid employees considering their current job good for career advancement",
                                    
                                    "Paid employees receiving at least one employment benefit in their current job",
                                    
                                    "Paid employees having pension plan in their current job",
                                    
                                    "Paid employees having paid sick leave in their current job",
                                    
                                    "Paid employees having paid vacation leave in their current job",
                                    
                                    "Paid employees having disability insurance in their current job",
                                    
                                    "Paid employees having supplemental medical care in their current job",
                                    
                                    "Paid employees having worker's compensation in their current job",
                                    
                                    "Paid employees having maternity, paternity or lay-off benefits in their current job",
                                    
                                    "Paid employees covered by union contract or collective agreement in their current job",
                                    
                                    "Paid employees receiving formal training in their current job",
                                    
                                    "Paid employees receiving informal training in their current job"
                                    
                                    
                                  ),
                                  selected = NULL
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.LM3 == 'Overqualified workers with a university degree'",
                     
                     
                     
                     
                     selectizeInput("VM22",
                                    label = "Sex",
                                    choices = unique(OverQualDT$Sex),
                                    selected = list("Male"),
                                    multiple = TRUE
                     ),
                     
                     
                     selectizeInput("OverLocationSX",
                                    label = "Location of Study",
                                    choices = unique(OverQualDT$Location),
                                    
                                    
                     ),
                     
                     selectizeInput("OverDegreeSX",
                                    label = "Highest certificate, diploma or degree",
                                    choices = unique(OverQualDT$Degree),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverGeoSX",
                                    label = "Geography",
                                    choices = unique(OverQualDT$Geography),
                                    selected = "Canada"
                                    
                     ),
                     
                     selectizeInput("OverVMSX",
                                    label = "Visible Minority Status",
                                    choices = unique(OverQualDT$VisMin),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverYearSX",
                                    label = "Year",
                                    choices = unique(OverQualDT$Year),
                                    
                     ),
                     
                     selectizeInput("OverAgeSX",
                                    label = "Age Group",
                                    choices = unique(OverQualDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("OverImmSX",
                                    label = "Immigration and Generational Status",
                                    choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                    
                     ),
                     
                     selectizeInput("OverLangSX",
                                    label = "Language",
                                    choices = unique(OverQualDT$'Language'),
                                    
                     )
                   ),
                   
                 ),
              
                 conditionalPanel(
                   condition = "input.dim3 == 'Education, training and skills'",
                   
                   # These are the drop down menus
                   
                   
                   selectizeInput("Edu3",
                                  label = "Education, training and skills Indicators",
                                  choices = list("Population with no certificate, diploma or degree",
                                                 
                                                 "Population with high school diploma or equivalency certificate",
                                                 
                                                 "Population with postsecondary certificate or diploma below bachelor level",
                                                 
                                                 "Population with bachelor’s degree or above",
                                                 
                                                 "Population with bachelor’s degree",
                                                 
                                                 "Population with master’s degree or earned doctorate",
                                                 
                                                 "Knowledge of official languages, English only",
                                                 
                                                 "Knowledge of official languages, French only",
                                                 
                                                 "Knowledge of official languages, English and French",
                                                 
                                                 "Knowledge of official languages, neither English nor French",
                                                 
                                                 "Received a formal training paid by the employer in the past 12 months",
                                                 
                                                 "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months"),
                                  selected = NULL
                   ),
                   
                 
                   
             
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.dim3 == 'Education, training and skills' && input.Edu3 == 'Population with bachelor’s degree' || 'Population with no certificate, diploma or degree' || 'Population with high school diploma or equivalency certificate' || 'Population with postsecondary certificate or diploma below bachelor level' || 'Population with bachelor’s degree or above' || 'Population with master’s degree or earned doctorate' ",
                   
                   
                   
                   
                   selectizeInput("VM11",
                                  label = "Sex",
                                  choices = unique(educationDT$Sex),
                                  selected = list(),
                                  multiple = TRUE
                   ),
                   
                   
                   
                   selectizeInput("eduGeo3",
                                  label = "Geography",
                                  choices = unique(educationDT$Geography),
                                  selected = "Canada"
                                  
                   ),
                   
                   selectizeInput("eduVisMin3",
                                  label = "Visible Minority Group",
                                  choices = unique(educationDT$VisMin),
                                  selected = "Canada"
                                  
                   ),
                   
                   
                   selectizeInput("eduYear3",
                                  label = "Year",
                                  choices = unique(educationDT$Year),
                                  
                   ),
                   
                   selectizeInput("eduAge3",
                                  label = "Age Group",
                                  choices = unique(educationDT$Age),
                                  selected = "Total - Age"
                   ),
                   
                   selectizeInput("eduImm3",
                                  label = "Immigration and Generational Status",
                                  choices = sort(unique(educationDT$'Immigration'), decreasing = TRUE),
                                  
                   ),
                   
                   selectizeInput("eduLang3",
                                  label = "Language",
                                  choices = unique(educationDT$'Language'),
                                  
                   )
                 ),
                 
      
                 # Add the js code and button to the page
                 #extendShinyjs(text = jsResetCode, functions = "reset"),
                 actionButton("reset_button", "Reset Page"),
                 
               ),
               mainPanel(
                 
                 
                 h2("Sex/Gender"),
                 
                 #Visuals related to Labour Market
                 conditionalPanel(
                   
                   condition = "input.dim3 == 'Education, training and skills'",
                   
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with bachelor’s degree'",
                     
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   ),
                   
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with no certificate, diploma or degree'",
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX1", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with high school diploma or equivalency certificate'",
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX2", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with postsecondary certificate or diploma below bachelor level'",
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX3", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with bachelor’s degree or above'",
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX4", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.Edu3 == 'Population with master’s degree or earned doctorate'",
                     br(),
                     br(),
                     plotlyOutput("sBarEduSX5", inline = TRUE, width = 700, height = 500),
                     br(),
                     helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                     
                     
                     
                   )
                   
                   
                   
                 ),
                 
                 conditionalPanel(
                   
                   condition = "input.dim3 == 'Participation in the Labour Market' & input.LM3 == 'Overqualified workers with a university degree'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarOverSX", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                   
                 )
                 
                 
                 
                
               )
             )
    ),
    #Tab for Geography
    tabPanel("Geography", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectizeInput("dim4",
                                label = "Theme",
                                choices = list("Participation in the Labour Market")
                                
                 ),
                 
                 conditionalPanel(
                   condition = "input.dim4 == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM4",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Working-age population in the labour force (participation rate)",
                                    
                                    "Working-age population in employment (employment rate)",
                                    
                                    "Working-age population in unemployment (unemployment rate)",
                                    
                                    "Workers working mainly full-time weeks in the previous year",
                                    
                                    "Self-employed workers in the labour force (unincorporated)",
                                    
                                    "Overqualified workers with a university degree",
                                    
                                    "Youth not in employment, education or training (NEET)",
                                    
                                    "Average employment income of the population",
                                    
                                    "Average weekly wage of paid employees",
                                    
                                    "Currently employed population considering their job related to their education",
                                    
                                    "Paid employees considering their current job good for career advancement",
                                    
                                    "Paid employees receiving at least one employment benefit in their current job",
                                    
                                    "Paid employees having pension plan in their current job",
                                    
                                    "Paid employees having paid sick leave in their current job",
                                    
                                    "Paid employees having paid vacation leave in their current job",
                                    
                                    "Paid employees having disability insurance in their current job",
                                    
                                    "Paid employees having supplemental medical care in their current job",
                                    
                                    "Paid employees having worker's compensation in their current job",
                                    
                                    "Paid employees having maternity, paternity or lay-off benefits in their current job",
                                    
                                    "Paid employees covered by union contract or collective agreement in their current job",
                                    
                                    "Paid employees receiving formal training in their current job",
                                    
                                    "Paid employees receiving informal training in their current job"
                                    
                                    
                                  ),
                                  selected = NULL
                   ),
                   
                   
                   
                     
                     
                     
                     
                     selectizeInput("VM23",
                                    label = "Geography",
                                    choices = unique(OverQualDT$Geography),
                                    selected = list("Male"),
                                    multiple = TRUE
                     ),
                     
                     
                     selectizeInput("OverLocationGEO",
                                    label = "Location of Study",
                                    choices = unique(OverQualDT$Location),
                                    
                                    
                     ),
                     
                     selectizeInput("OverDegreeGEO",
                                    label = "Highest certificate, diploma or degree",
                                    choices = unique(OverQualDT$Degree),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverSexGEO",
                                    label = "Sex",
                                    choices = unique(OverQualDT$Sex),
                                    selected = "Males"
                                    
                     ),
                     
                     selectizeInput("OverVMGEO",
                                    label = "Visible Minority Status",
                                    choices = unique(OverQualDT$VisMin),
                                    
                                    
                     ),
                     
                     
                     selectizeInput("OverYearGEO",
                                    label = "Year",
                                    choices = unique(OverQualDT$Year),
                                    
                     ),
                     
                     selectizeInput("OverAgeGEO",
                                    label = "Age Group",
                                    choices = unique(OverQualDT$Age),
                                    selected = "Total - Age"
                     ),
                     
                     selectizeInput("OverImmGEO",
                                    label = "Immigration and Generational Status",
                                    choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                    
                     ),
                     
                     selectizeInput("OverLangGEO",
                                    label = "Language",
                                    choices = unique(OverQualDT$'Language'),
                                    
                     )
                   
                   
                 ),
               
                 
                 
                 # Add the js code and button to the page
                 #extendShinyjs(text = jsResetCode, functions = "reset"),
                 actionButton("reset_button", "Reset Page"),
               ),
               
               #Main Panel for displaying graphs
               mainPanel(
                 conditionalPanel(
                   
                   condition = "input.dim4 == 'Participation in the Labour Market'",
                   
                   br(),
                   br(),
                   plotlyOutput("sBarOverGEO", inline = TRUE, width = 700, height = 500),
                   br(),
                   helpText("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011.")
                   
                 )
                 
                
               )
             )
             
    ),
    
    # Scatter plot and Line Graph Tab
    tabPanel("Time Series Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
              
                 
                 selectizeInput("spdim",
                                label = "Theme",
                                choices = list("Participation in the Labour Market", "Discrimination"),
                                selected = "Discrimination",
                 ),
                 
                 
              
                 conditionalPanel(
                   condition = "input.spdim == 'Participation in the Labour Market'",
                   
                   
                   selectizeInput("LM6",
                                  label = "Indicators",
                                  choices = list(
                                    
                                    "Working-age population in the labour force (participation rate)",
                                    
                                    "Working-age population in employment (employment rate)",
                                    
                                    "Working-age population in unemployment (unemployment rate)",
                                    
                                    "Workers working mainly full-time weeks in the previous year",
                                    
                                    "Self-employed workers in the labour force (unincorporated)",
                                    
                                    "Overqualified workers with a university degree",
                                    
                                    "Youth not in employment, education or training (NEET)",
                                    
                                    "Average employment income of the population",
                                    
                                    "Average weekly wage of paid employees",
                                    
                                    "Currently employed population considering their job related to their education",
                                    
                                    "Paid employees considering their current job good for career advancement",
                                    
                                    "Paid employees receiving at least one employment benefit in their current job",
                                    
                                    "Paid employees having pension plan in their current job",
                                    
                                    "Paid employees having paid sick leave in their current job",
                                    
                                    "Paid employees having paid vacation leave in their current job",
                                    
                                    "Paid employees having disability insurance in their current job",
                                    
                                    "Paid employees having supplemental medical care in their current job",
                                    
                                    "Paid employees having worker's compensation in their current job",
                                    
                                    "Paid employees having maternity, paternity or lay-off benefits in their current job",
                                    
                                    "Paid employees covered by union contract or collective agreement in their current job",
                                    
                                    "Paid employees receiving formal training in their current job",
                                    
                                    "Paid employees receiving informal training in their current job"
                                    
                                    
                                  ),
                                  selected = NULL
                   ),
                   
                   
                   
                   
                   
                   
                   
                   selectizeInput("OverGeoLINE",
                                  label = "Geography",
                                  choices = unique(OverQualDT$Geography),
                                 
                   ),
                   
                   
                   selectizeInput("OverLocationLINE",
                                  label = "Location of Study",
                                  choices = unique(OverQualDT$Location),
                                  
                                  
                   ),
                   
                   selectizeInput("OverDegreeLINE",
                                  label = "Highest certificate, diploma or degree",
                                  choices = unique(OverQualDT$Degree),
                                  
                                  
                   ),
                   
                   
                   selectizeInput("OverSexLINE",
                                  label = "Sex",
                                  choices = unique(OverQualDT$Sex),
                                  selected = "Males"
                                  
                   ),
                   
                   selectizeInput("OverVMLINE",
                                  label = "Visible Minority Status",
                                  choices = unique(OverQualDT$VisMin),
                                  
                                  
                   ),
                   
                   
          
                   
                   selectizeInput("OverAgeLINE",
                                  label = "Age Group",
                                  choices = unique(OverQualDT$Age),
                                  selected = "Total - Age"
                   ),
                   
                   selectizeInput("OverImmLINE",
                                  label = "Immigration and Generational Status",
                                  choices = sort(unique(OverQualDT$Immigration), decreasing = TRUE),
                                  
                   ),
                   
                   selectizeInput("OverLangLINE",
                                  label = "Language",
                                  choices = unique(OverQualDT$'Language'),
                                  
                   )
                   
                   
                 ),
                 
                
                 
                
                 conditionalPanel(
                   condition = "input.spdim == 'Discrimination'",
                   selectizeInput("LM6",
                                  label = "Indicators",
                                  choices = list("Hate Crime"),
                                  selected = "Hate Crime",
                   ),
                   
                   
                   selectizeInput("motivation2",
                                  label = "Motivation",
                                  choices = list('Race or ethnicity', 'Total police-reported hate crime'),
                                  selected = "Race or ethnicity",
                                
                   ),
                   
                  
                   
                   
                   conditionalPanel(
                     condition = "input.motivation2 == 'Race or ethnicity'",
                     
                     selectizeInput("VisMi2",
                                    label = "Race or ethnicity and other characteristics",
                                    choices = list('Black', 'South Asian', 'East or Southeast Asian', 'Arab or West Asian', 'White', 'Indigenous', 'Multiple races or ethnicities', 'Other Race or ethnicity', 'Unknown Race or ethnicity' ),
                                    selected = "Black",
                                    multiple = TRUE
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.motivation2 == 'Total police-reported hate crime'",
                     
                     selectizeInput("VisMi",
                                    label = "Police Reported Hate Crimes",
                                    choices = list('Race or ethnicity', 'Religion', 'Sexual orientation', 'Language', 'Disabilitiy', 'Sex', 'Age', 'Unknown motivation'),
                                    selected = "Religion",
                                    multiple = TRUE
                     ),
                     
                     
                   ),
           
          
                 
                  ),
                 
                
                 
                 
                 
                 # Add the js code and button to the page
                # extendShinyjs(text = jsResetCode, functions = "reset"),
                 actionButton("reset_button", "Reset Page"),
               ),
               
               mainPanel(
                 
                 conditionalPanel(
                   condition = "input.spdim == 'Discrimination' & input.motivation2 == 'Total police-reported hate crime'",
                 h2("Time Series Analysis"),
                 br(),
                 br(),
                 plotlyOutput("ltwograph", inline = TRUE, width = 700, height = 500),
                 p("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                 
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.spdim == 'Discrimination' & input.motivation2 == 'Race or ethnicity'",
                   h2("Time Series Analysis"),
                   br(),
                   br(),
                   plotlyOutput("lgraph", inline = TRUE, width = 700, height = 500),
                   p("Source: Statistics Canada, Census of Population, 2016. Statistics Canada, National Household Survey, 2011. Statistics Canada, Census of Population, 2006")
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.spdim == 'Participation in the Labour Market'",
                   h2("Time Series Analysis"),
                   br(),
                   br(),
                   plotlyOutput("lthreegraph", inline = TRUE, width = 700, height = 500),
                   p("Source: ----")
                   
                 ),
                 
                 
                 
               )
             )
    )
  )
)

# Define server logic ---------------------------------------------------------------
server <- function(input, output) {
  
  # Method to call the reset function
  observeEvent(input$reset_button, {js$reset()})
  
  # Reactive values ----------------------------------------------------------
  # This reactive filters for sex to build the first column graph on the first tab
 
  filtered_Conf <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in the police' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu1 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in Federal Parliament' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu2 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in the canadian media' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu3 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in the school system' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  filtered_Conf4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu4 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in the justice system, courts' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  filtered_Conf5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu5 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in major corporations' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  filtered_Conf6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecImm,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  filtered_ConfAge6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecAge,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  filtered_ConfSex6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecSex,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  filtered_ConfGen6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecGen,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  
  filtered_ConfLang6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecLang,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  filtered_ConfEdu6 <- reactive({
    
    newDT <- confidenceDT %>%
      
      filter(`VisMin` %in% input$VM40, `Year` %in% input$confYear, `Geography` %in% input$confGeo, `Confidence` %in% input$confConfidence,`Characteristic` %in% input$confCharSpecEdu,`Indicator` == 'Confidence in merchants and business people' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  
  filtered_Cov <- reactive({
    
    newDT <- discriminationDT %>%
     
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_Cov2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  
  filtered_CovAge <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovAge2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecSex,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecSex, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu2 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(s) of discrimination based on ethnicity or culture, since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  ##
  
  
  
  
  
  filtered_Cov1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_Cov21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecImm, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic'  )
    
    
    return(newDT)
  })
  
  
  
  filtered_CovAge1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovAge21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecAge, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecSex,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovSex21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecSex, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovGen21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecGen, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovLang21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecLang, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu1 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu,`Indicator` == 'Experience(s) of discrimination, 5 years before Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  filtered_CovEdu21 <- reactive({
    
    newDT <- discriminationDT %>%
      
      filter(`VisMin` %in% input$VM30, `Year` %in% input$covYear, `Geography` %in% input$covGeo, `Confidence` %in% input$covConfidence,`Characteristic` %in% input$covCharSpecEdu, `Indicator` == 'Experience(es) of discrimination since the beginning of Covid-19 pandemic' )
    
    
    return(newDT)
  })
  
  
  ###
  

   filtered_OverVM <- reactive({
    
    newDT <- OverQualDT %>%
      filter(`Immigration` %in% input$OverImm,`Degree` %in% input$OverDegree, `Language` %in% input$OverLang,`Location` %in% input$OverLocation, `VisMin` %in% input$VM20, `Geography` %in% input$OverGeo,`Year` %in% input$OverYear,`Age` %in% input$OverAge,`Sex` %in% input$OverSex)
    
    
    return(newDT)
  })
  
  
  filtered_OverGEO <- reactive({
    
    newDT <- OverQualDT %>%
      filter(`Immigration` %in% input$OverImmGEO,`Degree` %in% input$OverDegreeGEO, `Language` %in% input$OverLangGEO,`Location` %in% input$OverLocationGEO, `VisMin` %in% input$OverVMGEO, `Geography` %in% input$VM23,`Year` %in% input$OverYearGEO,`Age` %in% input$OverAgeGEO,`Sex` %in% input$OverSexGEO)
    
    
    return(newDT)
  })
  
  filtered_OverIS <- reactive({
    
    newDT <- OverQualDT %>%
      filter(`Immigration` %in% input$VM21,`Degree` %in% input$OverDegreeIS, `Language` %in% input$OverLangIS,`Location` %in% input$OverLocationIS, `VisMin` %in% input$OverVMIS, `Geography` %in% input$OverGeoIS,`Year` %in% input$OverYearIS,`Age` %in% input$OverAgeIS,`Sex` %in% input$OverSexIS)
    
    
    return(newDT)
  })
  
  
  filtered_OverSX <- reactive({
    
    newDT <- OverQualDT %>%
      filter(`Immigration` %in% input$OverImmSX,`Degree` %in% input$OverDegreeSX, `Language` %in% input$OverLangSX,`Location` %in% input$OverLocationSX, `VisMin` %in% input$OverVMSX, `Geography` %in% input$OverGeoSX,`Year` %in% input$OverYearSX,`Age` %in% input$OverAgeSX,`Sex` %in% input$VM22)
    
    
    return(newDT)
  })
  
  
  filtered_educationSX1 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with bachelor’s degree")
    
    
    return(newDT)
  })
  
  filtered_educationSX2 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with no certificate, diploma or degree")
    
    
    return(newDT)
  })
  
  filtered_educationSX3 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11,`Indicators` == "Population with high school diploma or equivalency certificate")
    
    
    return(newDT)
  })
  
  
  filtered_educationSX4 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
    
    
    return(newDT)
  })
  
  
  filtered_educationSX5 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11, `Indicators` == "Population with bachelor’s degree or above")
    
    
    return(newDT)
  })
  
  
  filtered_educationSX6 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduImm3, `Language` %in% input$eduLang3, `VisMin` %in% input$eduVisMin3, `Geography` %in% input$eduGeo3,`Year` %in% input$eduYear3,`Age` %in% input$eduAge3,`Sex` %in% input$VM11,`Indicators` == "Population with master’s degree or earned doctorate")
    
    
    return(newDT)
  })
  filtered_educationVM1 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with bachelor’s degree")
    
    
    return(newDT)
  })
  
  filtered_educationVM2 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with no certificate, diploma or degree")
    
    
    return(newDT)
  })
  
  filtered_educationVM3 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with high school diploma or equivalency certificate")
    
    
    return(newDT)
  })
  
  
  filtered_educationVM4 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
    
    
    return(newDT)
  })
  
  
  filtered_educationVM5 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with bachelor’s degree or above")
    
    
    return(newDT)
  })
  
  
  filtered_educationVM6 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$eduVisMin2, `Language` %in% input$eduLang2, `VisMin` %in% input$VM10, `Geography` %in% input$eduGeo2,`Year` %in% input$eduYear2,`Age` %in% input$eduAge2,`Sex` %in% input$eduSex2, `Indicators` == "Population with master’s degree or earned doctorate")
    
    
    return(newDT)
  })
  
  filtered_education1 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with bachelor’s degree")
    
    
    return(newDT)
  })
  
  filtered_education2 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with no certificate, diploma or degree")
    
    
    return(newDT)
  })
  
  
  filtered_education3 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with high school diploma or equivalency certificate")
    
    
    return(newDT)
  })
  
  filtered_education4 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with postsecondary certificate or diploma below bachelor level")
    
    
    return(newDT)
  })
  filtered_education5 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with bachelor’s degree or above")
    
    
    return(newDT)
  })
  filtered_education6 <- reactive({
    
    newDT <- educationDT %>%
      filter(`Immigration` %in% input$VM9, `Language` %in% input$eduLang, `VisMin` %in% input$eduVisMin, `Geography` %in% input$eduGeo,`Year` %in% input$eduYear,`Age` %in% input$eduAge,`Sex` %in% input$eduSex, `Indicators` == "Population with master’s degree or earned doctorate")
    
    
    return(newDT)
  })
  
  
  #This is a reactive for  police reported hate crime by VisMin
  filtered_immdisc <- reactive({
    
    req(input$VM7)
    
    newDT <- polData %>%
      #pivot_wider(names_from = "Year", names_prefix = "yr", values_from = "Value") %>% 
     
      filter(Motivation %in% input$VM7, Year %in% input$disYear)
    
    return(newDT)
    
  })
  
  
  filtered_imm2disc <- reactive({
    
    req(input$VM8)
    
    newDT <- polData %>%
      #pivot_wider(names_from = "Year", names_prefix = "yr", values_from = "Value") %>% 
      
      filter(Motivation %in% input$VM8, Year %in% input$disYear)
    
    return(newDT)
    
  })
  

  

  



  
  
  # This reactive filters the line plot data
  filtered_lineData <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin
    req(input$VisMi2)
    
    # Filter values
    newDT <- polData %>%
      filter(
        `Motivation` %in% input$VisMi2)
    
    
    # Pivot by VisMin
    newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  filtered_linethreeData <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin
    req(input$VisMi2)
    
    # Filter values
    newDT <- polData %>%
      filter(`Immigration` %in% input$OverImmLINE,`Degree` %in% input$OverDegreeLINE, `Language` %in% input$OverLangLINE,`Location` %in% input$OverLocationLINE, `VisMin` %in% input$OverVMLINE, `Geography` %in% input$OverGeoLINE,`Age` %in% input$OverAgeLINE,`Sex` %in% input$OverSexLINE)
    
    
    
    # Pivot by VisMin
    newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  
  
  # This reactive filters the line plot data
  filtered_linetwoData <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin
    req(input$VisMi)
    
    # Filter values
    newDT <- polData %>%
      filter(
             `Motivation` %in% input$VisMi)
  
    
    # Pivot by VisMin
    newDT <- pivot_wider(newDT, names_from = `Motivation`, values_from = `Value`)
    
    return(newDT)
  })
  
  
  
  # Output ---------------------------------------------------
  output$sBarConf <- renderPlotly({
    
    req(filtered_Conf())
    
    
    fig <- plot_ly(filtered_Conf(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge <- renderPlotly({
    
    req(filtered_ConfAge())
    
    
    fig <- plot_ly(filtered_ConfAge(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex <- renderPlotly({
    
    req(filtered_ConfSex())
    
    
    fig <- plot_ly(filtered_ConfSex(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen <- renderPlotly({
    
    req(filtered_ConfGen())
    
    
    fig <- plot_ly(filtered_ConfGen(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang <- renderPlotly({
    
    req(filtered_ConfLang())
    
    
    fig <- plot_ly(filtered_ConfLang(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu <- renderPlotly({
    
    req(filtered_ConfEdu())
    
    
    fig <- plot_ly(filtered_ConfEdu(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConf1 <- renderPlotly({
    
    req(filtered_Conf1())
    
    
    fig <- plot_ly(filtered_Conf1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge1 <- renderPlotly({
    
    req(filtered_ConfAge1())
    
    
    fig <- plot_ly(filtered_ConfAge1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex1 <- renderPlotly({
    
    req(filtered_ConfSex1())
    
    
    fig <- plot_ly(filtered_ConfSex1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen1 <- renderPlotly({
    
    req(filtered_ConfGen1())
    
    
    fig <- plot_ly(filtered_ConfGen1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang1 <- renderPlotly({
    
    req(filtered_ConfLang1())
    
    
    fig <- plot_ly(filtered_ConfLang1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu1 <- renderPlotly({
    
    req(filtered_ConfEdu1())
    
    
    fig <- plot_ly(filtered_ConfEdu1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the police",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
##
  
  output$sBarConf2 <- renderPlotly({
    
    req(filtered_Conf2())
    
    
    fig <- plot_ly(filtered_Conf2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge2 <- renderPlotly({
    
    req(filtered_ConfAge2())
    
    
    fig <- plot_ly(filtered_ConfAge2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex2 <- renderPlotly({
    
    req(filtered_ConfSex2())
    
    
    fig <- plot_ly(filtered_ConfSex2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen2 <- renderPlotly({
    
    req(filtered_ConfGen2())
    
    
    fig <- plot_ly(filtered_ConfGen2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang2 <- renderPlotly({
    
    req(filtered_ConfLang2())
    
    
    fig <- plot_ly(filtered_ConfLang2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu2 <- renderPlotly({
    
    req(filtered_ConfEdu2())
    
    
    fig <- plot_ly(filtered_ConfEdu2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the canadian media",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
##
  
  output$sBarConf3 <- renderPlotly({
    
    req(filtered_Conf3())
    
    
    fig <- plot_ly(filtered_Conf3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge3 <- renderPlotly({
    
    req(filtered_ConfAge3())
    
    
    fig <- plot_ly(filtered_ConfAge3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex3 <- renderPlotly({
    
    req(filtered_ConfSex3())
    
    
    fig <- plot_ly(filtered_ConfSex3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen3 <- renderPlotly({
    
    req(filtered_ConfGen3())
    
    
    fig <- plot_ly(filtered_ConfGen3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang3 <- renderPlotly({
    
    req(filtered_ConfLang3())
    
    
    fig <- plot_ly(filtered_ConfLang3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu3 <- renderPlotly({
    
    req(filtered_ConfEdu3())
    
    
    fig <- plot_ly(filtered_ConfEdu3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the school system",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  ##
  
  
  output$sBarConf4 <- renderPlotly({
    
    req(filtered_Conf4())
    
    
    fig <- plot_ly(filtered_Conf4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge4 <- renderPlotly({
    
    req(filtered_ConfAge4())
    
    
    fig <- plot_ly(filtered_ConfAge4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex4 <- renderPlotly({
    
    req(filtered_ConfSex4())
    
    
    fig <- plot_ly(filtered_ConfSex4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen4 <- renderPlotly({
    
    req(filtered_ConfGen4())
    
    
    fig <- plot_ly(filtered_ConfGen4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang4 <- renderPlotly({
    
    req(filtered_ConfLang4())
    
    
    fig <- plot_ly(filtered_ConfLang4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu4 <- renderPlotly({
    
    req(filtered_ConfEdu4())
    
    
    fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in the justice system, courts",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  ##
  
  
  output$sBarConf5 <- renderPlotly({
    
    req(filtered_Conf5())
    
    
    fig <- plot_ly(filtered_Conf5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge5 <- renderPlotly({
    
    req(filtered_ConfAge5())
    
    
    fig <- plot_ly(filtered_ConfAge5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex5 <- renderPlotly({
    
    req(filtered_ConfSex5())
    
    
    fig <- plot_ly(filtered_ConfSex5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen5 <- renderPlotly({
    
    req(filtered_ConfGen5())
    
    
    fig <- plot_ly(filtered_ConfGen5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang5 <- renderPlotly({
    
    req(filtered_ConfLang5())
    
    
    fig <- plot_ly(filtered_ConfLang5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu5 <- renderPlotly({
    
    req(filtered_ConfEdu5())
    
    
    fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  ##
  
  output$sBarConf5 <- renderPlotly({
    
    req(filtered_Conf5())
    
    
    fig <- plot_ly(filtered_Conf5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge5 <- renderPlotly({
    
    req(filtered_ConfAge5())
    
    
    fig <- plot_ly(filtered_ConfAge5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex5 <- renderPlotly({
    
    req(filtered_ConfSex5())
    
    
    fig <- plot_ly(filtered_ConfSex5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen5 <- renderPlotly({
    
    req(filtered_ConfGen5())
    
    
    fig <- plot_ly(filtered_ConfGen5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang5 <- renderPlotly({
    
    req(filtered_ConfLang5())
    
    
    fig <- plot_ly(filtered_ConfLang5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu5 <- renderPlotly({
    
    req(filtered_ConfEdu5())
    
    
    fig <- plot_ly(filtered_ConfEdu4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in major corporations",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  ##
  
  output$sBarConf6 <- renderPlotly({
    
    req(filtered_Conf6())
    
    
    fig <- plot_ly(filtered_Conf6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfAge6 <- renderPlotly({
    
    req(filtered_ConfAge6())
    
    
    fig <- plot_ly(filtered_ConfAge6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfSex6 <- renderPlotly({
    
    req(filtered_ConfSex6())
    
    
    fig <- plot_ly(filtered_ConfSex6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfGen6 <- renderPlotly({
    
    req(filtered_ConfGen6())
    
    
    fig <- plot_ly(filtered_ConfGen6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarConfLang6 <- renderPlotly({
    
    req(filtered_ConfLang6())
    
    
    fig <- plot_ly(filtered_ConfLang6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarConfEdu6 <- renderPlotly({
    
    req(filtered_ConfEdu6())
    
    
    fig <- plot_ly(filtered_ConfEdu6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "Confidence in merchants and business people",font = list(size = 18)),  yaxis = list( title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  ##
  
  
  
  
  output$sBarCov <- renderPlotly({
    
    req(filtered_Cov())
  
    
    fig <- plot_ly(filtered_Cov(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCov2 <- renderPlotly({
    
    req(filtered_Cov2())
    
    
    fig <- plot_ly(filtered_Cov2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'yyy'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovAge <- renderPlotly({
    
    req(filtered_CovAge())
    
    
    fig <- plot_ly(filtered_CovAge(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovAge2 <- renderPlotly({
    
    req(filtered_CovAge2())
    
    
    fig <- plot_ly(filtered_CovAge2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovSex <- renderPlotly({
    
    req(filtered_CovSex())
    
    
    fig <- plot_ly(filtered_CovSex(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovSex2 <- renderPlotly({
    
    req(filtered_CovSex2())
    
    
    fig <- plot_ly(filtered_CovSex2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovGen <- renderPlotly({
    
    req(filtered_CovGen())
    
    
    fig <- plot_ly(filtered_CovGen(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovGen2 <- renderPlotly({
    
    req(filtered_CovGen2())
    
    
    fig <- plot_ly(filtered_CovGen2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  output$sBarCovLang <- renderPlotly({
    
    req(filtered_CovLang())
    
    
    fig <- plot_ly(filtered_CovLang(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovLang2 <- renderPlotly({
    
    req(filtered_CovLang2())
    
    
    fig <- plot_ly(filtered_CovLang2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  output$sBarCovEdu <- renderPlotly({
    
    req(filtered_CovEdu())
    
    
    fig <- plot_ly(filtered_CovEdu(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovEdu2 <- renderPlotly({
    
    req(filtered_CovEdu2())
    
    
    fig <- plot_ly(filtered_CovEdu2(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  ####
  
  output$sBarCov1 <- renderPlotly({
    
    req(filtered_Cov1())
    
    
    fig <- plot_ly(filtered_Cov1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCov21 <- renderPlotly({
    
    req(filtered_Cov21())
    
    
    fig <- plot_ly(filtered_Cov21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovAge1 <- renderPlotly({
    
    req(filtered_CovAge1())
    
    
    fig <- plot_ly(filtered_CovAge1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovAge21 <- renderPlotly({
    
    req(filtered_CovAge21())
    
    
    fig <- plot_ly(filtered_CovAge21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
              xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovSex1 <- renderPlotly({
    
    req(filtered_CovSex1())
    
    
    fig <- plot_ly(filtered_CovSex1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovSex21 <- renderPlotly({
    
    req(filtered_CovSex21())
    
    
    fig <- plot_ly(filtered_CovSex21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovGen1 <- renderPlotly({
    
    req(filtered_CovGen1())
    
    
    fig <- plot_ly(filtered_CovGen1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovGen21 <- renderPlotly({
    
    req(filtered_CovGen21())
    
    
    fig <- plot_ly(filtered_CovGen21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
            xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  output$sBarCovLang1 <- renderPlotly({
    
    req(filtered_CovLang1())
    
    
    fig <- plot_ly(filtered_CovLang1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovLang21 <- renderPlotly({
    
    req(filtered_CovLang21())
    
    
    fig <- plot_ly(filtered_CovLang21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  output$sBarCovEdu1 <- renderPlotly({
    
    req(filtered_CovEdu1())
    
    
    fig <- plot_ly(filtered_CovEdu1(), x = ~VisMin, y = ~Value, type = 'bar') %>%
      
      
      layout(title = list(text = "5 years before Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'), yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarCovEdu21 <- renderPlotly({
    
    req(filtered_CovEdu21())
    
    
    fig <- plot_ly(filtered_CovEdu21(), x = ~VisMin, y = ~Value,  type = 'bar', color = "red") %>%
      
      
      
      
      layout(title = list(text = "Since the beginning of Covid-19 pandemic",font = list(size = 18)),  yaxis = list(range=c(0,60), title = 'percent (%)'),
             xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  ###
  
  #Graph for Hate Crime by VisMin
  output$immdisPlot <- renderPlotly({
    
    req(filtered_immdisc())
    
    fig <- plot_ly(filtered_immdisc(), x = ~Motivation, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Police Reported Hate Crimes by Groups Designated as Visible Minority",font = list(size = 18)),
             yaxis = list(title = 'number'), xaxis = list(title = 'Race or ethnicity'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$immdis2Plot <- renderPlotly({
    
    req(filtered_imm2disc())
    
    fig <- plot_ly(filtered_imm2disc(), x = ~Motivation, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Police Reported Hate Crimes by Groups Designated as Visible Minority",font = list(size = 18)),
             yaxis = list(title = 'number'), xaxis = list(title = 'Motive'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOver <- renderPlotly({
    
    req(filtered_OverVM())
    
    fig <- plot_ly(filtered_OverVM(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOver <- renderPlotly({
    
    req(filtered_OverVM())
    
    fig <- plot_ly(filtered_OverVM(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOverIS <- renderPlotly({
    
    req(filtered_OverIS())
    
    fig <- plot_ly(filtered_OverIS(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarOverGEO <- renderPlotly({
    
    req(filtered_OverGEO())
    
    fig <- plot_ly(filtered_OverGEO(), x = ~Geography, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Geography'))
    
    fig
    
  })
  
  output$sBarOverSX <- renderPlotly({
    
    req(filtered_OverIS())
    
    fig <- plot_ly(filtered_OverSX(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Overqualified workers with a university degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEduSX <- renderPlotly({
    
    req(filtered_educationSX1())
    
    fig <- plot_ly(filtered_educationSX1(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  output$sBarEduSX1 <- renderPlotly({
    
    req(filtered_educationSX2())
    
    fig <- plot_ly(filtered_educationSX2(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  output$sBarEduSX2 <- renderPlotly({
    
    req(filtered_educationSX3())
    
    fig <- plot_ly(filtered_educationSX3(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  
  output$sBarEduSX3 <- renderPlotly({
    
    req(filtered_educationSX4())
    
    fig <- plot_ly(filtered_educationSX4(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  
  output$sBarEduSX4 <- renderPlotly({
    
    req(filtered_educationSX5())
    
    fig <- plot_ly(filtered_educationSX5(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  
  output$sBarEduSX5 <- renderPlotly({
    
    req(filtered_educationSX6())
    
    fig <- plot_ly(filtered_educationSX6(), x = ~Sex, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Sex'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEduVM <- renderPlotly({
    
    req(filtered_educationVM1())
    
    fig <- plot_ly(filtered_educationVM1(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarEduVM1 <- renderPlotly({
    
    req(filtered_educationVM2())
    
    fig <- plot_ly(filtered_educationVM2(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  output$sBarEduVM2 <- renderPlotly({
    
    req(filtered_educationVM3())
    
    fig <- plot_ly(filtered_educationVM3(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarEduVM3 <- renderPlotly({
    
    req(filtered_educationVM4())
    
    fig <- plot_ly(filtered_educationVM4(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarEduVM4 <- renderPlotly({
    
    req(filtered_educationVM5())
    
    fig <- plot_ly(filtered_educationVM5(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  output$sBarEduVM5 <- renderPlotly({
    
    req(filtered_educationVM6())
    
    fig <- plot_ly(filtered_educationVM6(), x = ~VisMin, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Visible Minority Status'))
    
    fig
    
  })
  
  
  #Graph for Hate Crime by VisMin
  output$sBarEdu <- renderPlotly({
    
    req(filtered_education1())
    
    fig <- plot_ly(filtered_education1(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  #Graph for Hate Crime by VisMin
  output$sBarEdu1 <- renderPlotly({
    
    req(filtered_education2())
    
    fig <- plot_ly(filtered_education2(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with no certificate, diploma or degree",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  output$sBarEdu2 <- renderPlotly({
    
    req(filtered_education3())
    
    fig <- plot_ly(filtered_education3(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with high school diploma or equivalency certificate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  output$sBarEdu3 <- renderPlotly({
    
    req(filtered_education4())
    
    fig <- plot_ly(filtered_education4(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with postsecondary certificate or diploma below bachelor level",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  output$sBarEdu4 <- renderPlotly({
    
    req(filtered_education5())
    
    fig <- plot_ly(filtered_education5(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with bachelor’s degree or above",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  output$sBarEdu5 <- renderPlotly({
    
    req(filtered_education6())
    
    fig <- plot_ly(filtered_education6(), x = ~Immigration, y = ~Value, name = "2014", type = 'bar') %>%
      
      layout(title = list(text = "Population with master’s degree or earned doctorate",font = list(size = 18)),
             yaxis = list(title = 'percent (%)'), xaxis = list(title = 'Immigrant and generation status'))
    
    fig
    
  })
  
  
  


  

  

  
  # Line graph
  output$lgraph <- renderPlotly({
    
    # Require filtered_lineData
    req(filtered_lineData())
    
    # Create the base graph
    lp <- plot_ly(data = filtered_lineData(), x = ~Year)
    
    # Add each series one-by-one as new traces
    for (i in 3:length(colnames(filtered_lineData()))) {
      lp <- lp %>%
        add_trace(x = filtered_lineData()$Year, y = filtered_lineData()[[i]],
                  type = "scatter", mode = "lines+markers",
                  name = colnames(filtered_lineData())[i])
    }
    
    
    
    
    # Note hovermode = "x unified" is not working as it is supposed to
    # Best work-around was used in xaxis with spike layout
    lp <- lp %>%
      layout(title = "Police Reported Hate Crime Time Series Analysis",
             hovermode = "Police Reported Hate Crime Time Series Analysis",
             xaxis = list(title = "Year",
                          showspikes = TRUE,
                          spikecolor = "black",
                          spikethickness = 2,
                          spikemode  = 'toaxis+across',
                          spikesnap = 'data',
                          showline=TRUE),
             yaxis = list(title = "number")
      )
    
    lp
  })
  
  
  
  # Line graph
  output$ltwograph <- renderPlotly({
    
    # Require filtered_lineData
    req(filtered_linetwoData())
    
    # Create the base graph
    lp <- plot_ly(data = filtered_linetwoData(), x = ~Year)
    
    # Add each series one-by-one as new traces
    for (i in 3:length(colnames(filtered_linetwoData()))) {
      lp <- lp %>%
        add_trace(x = filtered_linetwoData()$Year, y = filtered_linetwoData()[[i]],
                  type = "scatter", mode = "lines+markers",
                  name = colnames(filtered_linetwoData())[i])
    }
    
    
    
    
    # Note hovermode = "x unified" is not working as it is supposed to
    # Best work-around was used in xaxis with spike layout
    lp <- lp %>%
      layout(title = "Police Reported Hate Crime Time Series Analysis",
             hovermode = "Police Reported Hate Crime Time Series Analysis",
             xaxis = list(title = "Year",
                          showspikes = TRUE,
                          spikecolor = "black",
                          spikethickness = 2,
                          spikemode  = 'toaxis+across',
                          spikesnap = 'data',
                          showline=TRUE),
             yaxis = list(title = "number")
      )
    
    lp
  })
}
# Run the app -------------------------------------------------------
shinyApp(ui = ui, server = server)