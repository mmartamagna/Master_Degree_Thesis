#################################################################
##  SCRIPT 4: Shiny Dashboard Multidimensional Well-being      ##
##  Author: Marta Magnani                                      ## 
##  Course: Data Science and Economics (University of Milan)   ##                                    
##  Last: Sep 10th, 2023                                       ##
#################################################################

# Install and load necessary libraries
library(leaflet)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(RColorBrewer)
library(fmsb)
library(viridis)
library(tidyr)
library(shiny.router)
library(likert)
library(rhino)
library(ggplot2)
library(ShinyItemAnalysis)
library(moments)
library(shinythemes)
library(formattable)
library(corrplot)
library(heatmaply)
library(ggcorrplot)

data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "BESt Work Life®"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Participants and survey", tabName = "survey"),
      menuItem("Item level statistics", tabName = "stats"),
      menuItem("Composite measures statistics", tabName = "composite"),
      menuItem("Organizations scores", tabName = "scores"),
      menuItem("View data", tabName = "data_view", icon = icon("table")),
      menuItem("Legend", tabName = "legend", icon = icon("info-circle")))
    ),
  dashboardBody(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
/* General Body Styling */
body {
  font-family: 'Trebuchet MS', sans-serif;
  background-color: #f0f7f6;  /* Background color for individual pages */
}

/* Header Styling */
.skin-blue .main-header .navbar, .skin-blue .main-header .logo {
  background-color: #13a19c;  /* Perry color */
  color: white;
}

/* Sidebar Styling */
.skin-blue .main-sidebar {
  background-color: #9cc3c1;  /* Lighter shade of Perry color */
}

/* Active Sidebar Item */
.sidebar-menu .active a {
  border-left-color: #13a19c;  /* Perry color */
  color: #13a19c;  /* Perry color */
}

/* Logo Hover Color */
.skin-blue .main-header .logo:hover {
  background-color: #13a19c;  /* Perry color */
  color: white;
}

/* Hamburger Menu on Hover, Focus, and Active */
.skin-blue .main-header .navbar .navbar-toggle:hover,
.skin-blue .main-header .navbar .navbar-toggle:focus {
  background-color: #ffcc99 !important; /* Your desired light orange color */
}

/* Navbar Items */
.skin-blue .main-header .navbar .nav > li > a {
  color: white;
}

/* Navbar Items on Hover */
.skin-blue .main-header .navbar .nav > li > a:hover {
  background-color: #9cc3c1;  /* Lighter shade of Perry color */
}

/* Active Tab */
.nav-tabs > li > a.active-tab {
  background-color: #0b7261 !important;  /* Darker shade of Perry color */
}


/* Make Tab darker when clicked */
.nav-tabs > li > a:active {
  background-color: #0b7261 !important;  /* Darker shade of Perry color */
}

.nav-tabs > li > a {
  background-color: #13a19c !important;  /* Perry color */
}

/* Sidebar Menu Items */
.skin-blue .main-sidebar .sidebar .nav-sidebar > li > a {
  color: white !important;  /* White */
}

/* Background color for individual pages */
.content-wrapper {
  background-color: #f0f7f6 !important;  
}

/* Tab Panel Color */
.nav-tabs > li > a {
  background-color: #13a19c !important;  /* Perry color */
}

/* Text color inside Tab Panel */
.nav-tabs > li > a {
  color: white !important;
}


/* Text Size */
.big-text {
  font-size: 30px !important;
}

")),
      tags$head(
        tags$script(HTML("
    $(document).ready(function(){
      $('.nav-tabs > li > a').click(function(){
        $('.nav-tabs > li > a').removeClass('active-tab');
        $(this).addClass('active-tab');
      });

      // New code to navigate to the Introduction tab when logo is clicked
      $('.main-header .logo').click(function(){
        $('a[data-value=\"intro\"]').click();
      });
    });
  ")),
        tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML')
      )),
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                column(width = 6, 
                       div(style = "background-color: white; padding: 20px; border-radius: 10px;",
                           h1("Quantification of Workers’ Multidimensional Well-Being and Participation Levels"),
                           h2("New Approaches for Organizational Climate Measurement"),
                           h3("Master's final disseration a.y. 2022-2023"),
                           
                           hr(),
                           h3("Author and university information"),
                           tags$ul(
                             tags$li(strong("Name:"), " Marta Magnani (961071)"),
                             tags$li(strong("University:"), " Università degli Studi di Milano"),
                             tags$li(strong("M.Sc. Degree course:"), " Data Science and Economics (LM-91)"),
                             tags$li(strong("Contact:"), " marta.magnani2@studenti.unimi.it"),
                             tags$li(strong("Presentation date:"), " October 23rd, 2023")),
                           
                           hr(),
                           h3("Objectives"),
                           p("This dashboard serves as an interactive supplement to my thesis. Specifically, it aims to:"),
                           tags$ul(
                             tags$li("Provide descriptive statistics on various metrics related to workers' well-being and participation levels."),
                             tags$li("Enable users to navigate and understand the dataset used in the research.")
                           ),
                           hr(),
                           h3("Acknowledgments"),
                           p("I would like to thank Dr. Lorenzo Semplici and NeXt - Nuova Economia per Tutti APS ETS for their assistance in the development of this research project and the data provision.")
                       )),
                column(width = 6, 
                       div(style = "background-color: lightyellow; padding: 20px; border-radius: 10px;",
                           h1("Case study"),
                           h2("First edition of BESt Work Life® organizational climate survey"),
                           p("The  BESt Work Life® organizational climate survey was created by NeXt Nuova Economia Per Tutti APS ETS in 2022. I personally participated in the collection and analysis of data during an internship based at NeXt."),
                           hr(),
                           h3("Data"),
                           p("The dataset used in this dashboard contains 784 individual-level records obtained from employees of 22 distinct Italian organizations who participated in the BESt Work Life® organizational climate survey. Each organization deliberately participated in the study, inviting all their employees at every level to complete the questionnaire. Data collection started in June 2022 and concluded in November 2022. Since the questionnaire filing was not mandatory, the collected records do not necessarily come from the totality of workers of each organization."),
                           hr(),
                           h3("Survey characteristics"),
                           p("This survey presents the following characteristics:"),
                           tags$ul(
                             tags$li("Measures 127 items of workers' well-being and 21 items of participation levels."),
                             tags$li("From the items it is possible to derive 6 composite measures (Workers' well-being, Participation Level, Organizational Health, Subjective Well-being, Subjective Malaise, Absence of work-related Stress)."),
                             tags$li("Provides organizations of a multidimensional measurment tool for organizational climate.")
                           ))))),
      tabItem(tabName = "survey",
              tabsetPanel(
                tabPanel("Survey participants", 
                         fluidRow(
                           column(width = 11,
                                  h2("Main characteristics of the participating organizations"),  # Title
                                  p("The dataset used in this thesis contains 784 individual-level records obtained from employees of 22 distinct Italian organizations who participated in the BESt Work Life® organizational climate survey. Each organization deliberately participated in the study, inviting all their employees at every level to complete the questionnaire. Data collection started in June 2022 and concluded in November 2022. Since the questionnaire filing was not mandatory, the collected records do not necessarily come from the totality of workers of each organization.")  # Text
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  selectInput("pie_choice_organization", "Organizations characteristic:", choices = c("Dimension" = "dimension", 
                                                                                                                      "Province" = "province", 
                                                                                                                      "Region" = "region",
                                                                                                                      "Sector" = "sector", 
                                                                                                                      "Form" = "juridical_form"))
                           ),
                           column(width = 6,
                                  selectInput("pie_choice_individual", "Respondents characteristic:", choices = c("Role" = "role", 
                                                                                                                  "Gender" = "gender", 
                                                                                                                  "Age" = "age", 
                                                                                                                  "Seniority" = "seniority"))
                           )),
                         fluidRow(
                           column(width = 5,
                                  div(style = "height:300px;", plotlyOutput("pie_chart_total", height = "300px"))
                           ),
                           column(width = 5,
                                  div(style = "height:300px;", plotlyOutput("pie_chart_individual", height = "300px"))
                           )),
                         fluidRow(
                           column(width = 5,
                                  selectInput("var1", "Choose the first variable:", choices = c("Dimension" = "dimension", "Gender" = "gender", "Age" = "age", 
                                                                                                "Seniority" = "seniority", "Role" = "role")),
                                  selectInput("var2", "Choose the second variable:", choices = c("Gender" = "gender", "Age" = "age", 
                                                                                                 "Seniority" = "seniority", "Role" = "role", 
                                                                                                 "Dimension" = "dimension")))),
                          fluidRow(
                            column(width=5,
                                  div(style = "height:500px;", plotlyOutput("contingency_plot", height = "500px"))),
                           column(width = 5,
                                  div(style = "height:500px;", leafletOutput("map", height = "500px"))
                           ))),
                tabPanel("Survey structure",
                         fluidRow(
                           column(width=12,
                                  h2("Survey structure"),
                                  p("The most complex and also interesting aspect of the BESt Work Life survey is the multidimensionality of measures."))
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("donut_plot_bes", height = "400px", width = "90%")),
                           column(width=6,
                                  plotlyOutput("donut_plot_health", height = "400px", width = "90%"))
                          
                         ),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("donut_plot_subjwb", height = "400px", width = "90%")),
                           column(width=6,
                                  plotlyOutput("donut_plot_subjma", height = "400px", width = "90%"))
                           ),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("donut_plot_stress", height = "400px", width = "90%"))
                         )),
                tabPanel("Outliers",
                         fluidRow(
                           column(width=10,
                                  h2("Survey outliers"),
                                  p("A commonly used method for detecting and managing outliers in multi-dimensional data is to employ the Mahalanobis distance. This metric measures how far each data point is from the central hub of the data distribution, while also considering the covariance structure of the data set. Higher values suggest that a particular observation is more distant from the region where the majority of data points are concentrated.")),
                           HTML("Mahalanobis Distance Formula: \\( D^2(\\mathbf{x}) = (\\mathbf{x} - \\mathbf{\\mu})' \\mathbf{\\Sigma}^{-1} (\\mathbf{x} - \\mathbf{\\mu}) \\)")),
                         fluidRow(
                           column(width=10,
                                  plotlyOutput("mahalanobis_plot", height = "600px"))
                         ),
                         fluidRow(
                           column(width=10,
                                  h2(" "))),
                         fluidRow(
                           column(width=10,
                                  plotlyOutput("mahalanobis_plot_part", height = "600px"))
                         ))
                         )),
      tabItem(tabName = "stats",
              h2("Item-level statistics"),
              tabsetPanel(
                tabPanel("Participation Level", 
                         fluidRow(
                           column(width = 12,
                                  h2("Participation Items"),  # This is your title
                                  p("This section provides insights into the level of participation across different categories such as Dimension, Role, Gender, Age, and Seniority. The participation level is obtained as a weighted average of 21 different items which assume a value between 0 and 5. The total participation level assumes a value between 0 and 5.")                           )
                         ),
                         fluidRow(
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable_part')))
                         ),
                         fluidRow(
                           column(width = 6,
                                  selectInput("category_choice", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("interactive_participation_plot", height = "800px", width="100%")),
                           column(width = 6,
                                  plotlyOutput("interactive_plot", height = "800px", width="100%"))),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("multi_boxplot", height = "500px", width="100%")),
                           column(width=6,
                                  plotlyOutput("heatmap_part", height = "600px", width = "100%"))
                         )),
                tabPanel("Workers' well-being", 
                         fluidRow(
                           column(width = 12,
                                  h2("Domains of multidimensional well-being"),  # This is your title
                                  p("This section provides insights into the level of multidimensional well-being across different categories such as Dimension, Role, Gender, Age, and Seniority. The workers' well-being is obtained as a sample average of 12 different well-being domains which assume a value between 1 and 5. The total organizational well-being assumes a value between 1 and 5.")  # This is your content
                           )),
                         fluidRow(
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable_bes')))),
                         fluidRow(
                           column(width = 6,
                                  selectInput("category_choice_bes", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width = 6,
                                  plotlyOutput("interactive_bes_plot", height = "600px", width="100%")),
                           column(width = 6,
                                  plotlyOutput("bes_interactive_plot", height = "600px", width="100%"))),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("multi_boxplot_bes", height = "600px", width="100%")),
                           column(width=6,
                                  plotlyOutput("heatmap_bes", height = "600px", width = "100%"))
                )),
                tabPanel("Organizational health", 
                         fluidRow(
                           column(width = 12,
                                  h2("Organizational health domains"),  # This is your title 
                                  p("This section provides insights into the organizational health across different categories such as Dimension, Role, Gender, Age, and Seniority. The organizational health is obtained as sample average of 15 different domains which assume a value between 1 and 5. The total organizational health measure assumes a value between 1 and 5.")  # This is your content
                           )),
                         fluidRow(
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable_health')))
                         ),
                         fluidRow(
                           column(width = 6,
                                  selectInput("health_category_choice", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width = 6,
                                  plotlyOutput("interactive_health_plot", height = "800px", width="100%")),
                           column(width=5,
                                  plotlyOutput("health_interactive_plot", height = "800px", width="100%"))
                         ),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("multi_boxplot_health", height = "600px", width="100%")
                         ),
                         column(width=6,
                                plotlyOutput("heatmap_health", height = "600px", width = "100%"))
                         )),
                tabPanel("Subjective organizational well-being", 
                         fluidRow(
                           column(width = 12,
                                  h2("Domains of subjective organizational well-being"),  # This is your title
                                  p("This section provides insights into the subjective well-being across different categories such as Dimension, Role, Gender, Age, and Seniority. The subjective well-being is obtained as a sample average of 15 different well-being domains and assumes a value between 1 and 5.")  # This is your content
                           )
                         ),
                         fluidRow(
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable_subjwb')))
                         ),
                         fluidRow(
                           column(width = 6,
                                  selectInput("category_choice_subjwb", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("interactive_subjwb_plot", height = "600px", width="100%")),
                           column(width=6,
                                  plotlyOutput("subjwb_interactive_plot", height = "600px", width="100%"))
                         ),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("multi_boxplot_subjwb", height = "600px", width="100%")),
                           column(width=6,
                                  plotlyOutput("heatmap_subjwb", height = "600px", width = "100%"))
                           )),
                tabPanel("Subjective organizational malaise", 
                         fluidRow(
                           column(width = 12,
                                  h2("Domains of subjective organizational malaise"),
                                  p("This section provides insights into the subjective malaise across different categories such as Dimension, Role, Gender, Age, and Seniority. The subjective malaise is obtained as a sample average of 14 different well-being domains and assumes a value between 1 and 5.")
                           )
                         )
                         ,
                         fluidRow(
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable_subjma')))
                         ),
                         fluidRow(
                           column(width = 6,
                                  selectInput("category_choice_subjma", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("interactive_subjma_plot", height = "600px", width="100%")),
                           column(width=6,
                                  plotlyOutput("subjma_interactive_plot", height = "600px", width="100%"))),
                         fluidRow(
                           column(width=12),
                           h3("  ")
                         ),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("multi_boxplot_subjma", height = "600px", width="100%")),
                           column(width=6,
                                  plotlyOutput("heatmap_subjma", height = "600px", width = "100%"))
                         )),
                tabPanel("Absence of work-related stress", 
                         fluidRow(
                           column(width = 12,
                                  h2("Domains of work-related stress (absence)"),
                                  p("This section provides insights into the absence of work-related stress across different categories such as Dimension, Role, Gender, Age, and Seniority. The work-related stress is obtained as a sample average of 7 different domains and assumes a value between 1 and 5.")
                           )
                           ,
                           fluidRow(
                             column(width=12,
                                    div(style = "background-color: white;",
                                        formattableOutput('summaryTable_stress')))
                           ),
                           fluidRow(
                             column(width = 6,
                                    selectInput("category_choice_stress", "Choose a category:", 
                                                choices = c("Dimension" = "dimension", 
                                                            "Role" = "role", 
                                                            "Gender" = "gender", 
                                                            "Age" = "age", 
                                                            "Seniority" = "seniority")))),
                           fluidRow(
                             column(width = 6,
                                    plotlyOutput("interactive_stress_plot", height = "500px", width="100%")),
                             column(width=6,
                                    plotlyOutput("stress_interactive_plot", height = "500px", width="100%")
                             )),
                           fluidRow(
                             column(width=12),
                             h3("  ")
                           ),
                           fluidRow(
                             column(width=6,
                                    plotlyOutput("multi_boxplot_stress", height = "600px", width="100%")
                           ),
                           column(width=6,
                                  plotlyOutput("heatmap_stress", height = "600px", width = "100%"))
                             ))))),
      tabItem(tabName = "composite",
              h2("Composite measures"),
              tabsetPanel(
                tabPanel("Density distribution",
                         fluidRow(
                           h2("Summary statistics of main composite measures"),
                           column(width=12,
                                  div(style = "background-color: white;",
                                      formattableOutput('summaryTable')))),
                         fluidRow(
                           column(width=6,
                                  selectInput("var_choice_hist", "Choose a variable:", choices = c("Workers' well-being"="bes_wellbeing", "Organizational health"="health_organization", "Subjective well-being"="subjective_wellbeing", "Subjective malaise"="subjective_malaise", "Absence of work-related"="stress")))),
                         fluidRow(
                           column(width=6,
                                  sliderInput("cut_score", "Select Cut-Score:", min = 1, max = 5, value = 5),
                                  plotlyOutput("histPlot_bes", height="500px", width="100%")),
                           column(width=6,
                                  sliderInput("cut_score_part", "Select Cut-Score:", min = 0, max = 5, value = 5),
                                  plotlyOutput("histPlot_part", height = "500px", width = "100%"))
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             h2("")
                           )
                         ),
                         fluidRow(
                           column(width = 6, offset = 6,
                                  selectInput("measure_choice_dotplot", "Choose a measure:", 
                                              choices = c("Organizational well-being" = "bes_wellbeing", 
                                                          "Organizational health" = "health_organization", 
                                                          "Subjective malaise" = "subjective_malaise", 
                                                          "Subjective well-being" = "subjective_wellbeing", 
                                                          "Work-related stress (absence)" = "stress"))
                           )),
                         fluidRow(
                           column(width=6,
                                  plotlyOutput("heatmaply_plot", height = "600px", width = "100%")),
                           column(width=6,
                                  plotlyOutput("dotplot_correlation", height = "600px", width = "100%")))),
                tabPanel("Participation Level",
                                  fluidRow(
                                    column(width=6,
                                           div(style = "background-color: white;",
                                               tableOutput("table_test_participation")))),
                
                         fluidRow(
                           column(width = 10,
                                  h2("Participation composite measure"),  # This is your title
                                  p("This section provides insights into the level of participation across different categories such as Dimension, Role, Gender, Age, and Seniority. The participation level is obtained as a weighted average of 21 different items which assume a value between 0 and 5. The total participation level assumes a value between 0 and 5.")  # This is your content
                           )
                         ),
                         fluidRow(
                           column(width = 5,
                                  selectInput("category_choice_density", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=5,
                                  plotlyOutput("participation_density_plot", height = "400px", width="100%")),
                           column(width = 5,
                                  plotlyOutput("participation_boxplot", height = "400px", width="100%")
                           ))),
                tabPanel("Workers' well-being", 
                         fluidRow(
                           column(width=6,
                                  div(style = "background-color: white;",
                                      tableOutput("table_test_bes")))),
                         fluidRow(
                           column(width = 5, 
                                  selectInput("bes_category_choice", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")),
                                  )),
                         fluidRow(
                           column(width=5,
                                  plotlyOutput("bes_plot", height = "400px", width="100%")),
                           column(width = 5,
                                  plotlyOutput("bes_boxplot", height = "400px", width="100%")
                           )
                         ),
                         fluidRow(
                           column(width=5,
                                  selectInput("x_var", "Choose x variable:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")),
                                  selectInput("fill_var", "Choose fill variable:", 
                                              choices = c("None", "Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")),
                                  box(plotlyOutput("boxplot_prova")))
                         )
                ),
                tabPanel("Organizational health",
                         fluidRow(
                           column(width = 10,
                                  h2("Organizational health composite measure"),
                           )
                         ),
                         fluidRow(
                           column(width=6,
                                  div(style = "background-color: white;",
                                      tableOutput("table_test_health")))),
                         fluidRow(
                           column(width = 5,
                                  selectInput("category_choice_health", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=5,
                                  plotlyOutput("health_density_plot", height = "400px", width="100%")),
                           column(width = 5,
                                  plotlyOutput("health_boxplot", height = "400px", width="100%")
                           ))),
                tabPanel("Subjective organizational well-being", 
                         fluidRow(
                           column(width = 10,
                                  h2("Subjective well-being composite measure")  # This is your title
                           )
                         ),
                         fluidRow(
                           column(width=6,
                                  div(style = "background-color: white;",
                                      tableOutput("table_test_subjwb")))),
                         fluidRow(
                           column(width = 5, 
                                  selectInput("subjwb_category_choice", "Choose a category:", 
                                              choices = c("Dimension" = "dimension", 
                                                          "Role" = "role", 
                                                          "Gender" = "gender", 
                                                          "Age" = "age", 
                                                          "Seniority" = "seniority")))),
                         fluidRow(
                           column(width=5,
                                  plotlyOutput("subjwb_plot", height = "400px", width="100%")),
                           column(width = 5,
                                  plotlyOutput("subjwb_boxplot", height = "400px", width="100%")
                           )
                         )
                ),
                tabPanel("Subjective organizational malaise", 
                        fluidRow(
                         column(width = 10,
                                h2("Subjective malaise composite measure")  # This is your title
                         )
                       ),
                       fluidRow(
                         column(width=6,
                                div(style = "background-color: white;",
                                    tableOutput("table_test_subjma")))),
                       fluidRow(
                         column(width = 5, 
                                selectInput("subjma_category_choice", "Choose a category:", 
                                            choices = c("Dimension" = "dimension", 
                                                        "Role" = "role", 
                                                        "Gender" = "gender", 
                                                        "Age" = "age", 
                                                        "Seniority" = "seniority")))),
                       fluidRow(
                         column(width=5,
                               plotlyOutput("subjma_plot", height = "400px", width="100%")),
                         column(width = 5,
                                plotlyOutput("subjma_boxplot", height = "400px", width="100%")
                         )
                       )
              ),
              tabPanel("Work-related stress absence", 
                       fluidRow(
                         column(width = 10,
                                h2("Absence of work-related stress composite measure")  # This is your title
                         )
                       ),
                       fluidRow(
                         column(width=6,
                                div(style = "background-color: white;",
                                    tableOutput("table_test_stress")))),
                       fluidRow(
                         column(width = 5, 
                                selectInput("stress_category_choice", "Choose a category:", 
                                            choices = c("Dimension" = "dimension", 
                                                        "Role" = "role", 
                                                        "Gender" = "gender", 
                                                        "Age" = "age", 
                                                        "Seniority" = "seniority")))),
                       fluidRow(
                         column(width=5,
                                plotlyOutput("stress_plot", height = "400px", width="100%")),
                         column(width = 5,
                                plotlyOutput("stress_boxplot", height = "400px", width="100%")
                         ))))
              ),
      tabItem(tabName = "scores",
              tabPanel("Organizations scores", 
                       h2("Organizations scores"),
                       fluidRow(
                         column(width = 12,
                                p("This section provides a comparative analysis between Participation Level and the other composite measures such as Organizational well-being, Organizational health, Subjective malaise, Subjective well-being and Work-related stress. You can customize the graphs by selecting a measure from the dropdown list.")  # Text
                         )
                       ),
                       fluidRow(
                         column(width = 6,
                                selectInput("measure_choice", "Choose a measure:", 
                                            choices = c("Organizational well-being" = "bes_wellbeing", "Organizational health" = "health_organization", "Subjective malaise" = "subjective_malaise", "Subjective well-being" = "subjective_wellbeing", "Work-related stress (absence)" = "stress")),
                                selectInput("color_choice", "Color by:", 
                                            choices = c(
                                              "Dimension" = "dimension", 
                                              "Province" = "province", 
                                              "Sector" = "sector", 
                                              "Form" = "juridical_form",
                                              "Code" = "organization_code"))
                                
                         ),
                         column(width = 6,
                                selectInput("color_choice_individual", "Color by:", 
                                            choices = c(
                                              "Dimension" = "dimension", 
                                              "Role" = "role", 
                                              "Gender" = "gender", 
                                              "Age" = "age", 
                                              "Seniority" = "seniority",
                                              "Code" = "organization_code"))
                         )
                       ),
                       fluidRow(
                         column(width = 6,
                                plotlyOutput("composite_plot_total", height = "400px", width = "100%")
                         ),
                         column(width = 6,
                                plotlyOutput("composite_plot_individual", height = "400px", width = "100%")  # Second plot
                         )
                       ),
                       fluidRow(
                         column(width = 12,
                                dataTableOutput("avg_data_output")
                         )))
      ),
      tabItem(tabName = "data_view",
              DTOutput("dataTable")
      ),
      tabItem(tabName = "legend",
              h2("Legend"),
              tabsetPanel(
                id = "legend_tabs",
                tabPanel("ATECO Sectors", 
                         DTOutput("sectors_table")
                ),
                tabPanel("Organizational Health", 
                         p("Source: NeXt Nuova Economia per Tutti ATS EPS"),
                         DTOutput("health_table")
                ),
                tabPanel("Subjective Well-being", 
                         p("Source: NeXt Nuova Economia per Tutti ATS EPS"),
                         DTOutput("wellbeing_table")
                ),
                tabPanel("Subjective Malaise", 
                         p("Source: NeXt Nuova Economia per Tutti ATS EPS"),
                         DTOutput("malaise_table")
                ),
                tabPanel("Work-related Stress", 
                         p("Source: NeXt Nuova Economia per Tutti ATS EPS"),
                         DTOutput("stress_table"))
              )
      )
      
      # If you have more tabItems to add, you can do so here without a trailing comma after the last one
    )
  ),
  p(class = "big-text", "This is some large text.")
)



# Server
server <- function(input, output, session) {

    output$value1 <- renderValueBox({
      valueBox(
        formatC(784, format = "d", big.mark = ","),
        "Completed questionnaires",
        icon = icon("pencil"),  # Changed icon name
        color = "aqua"
      )
    })
    
    output$value2 <- renderValueBox({
      valueBox(
        formatC(22, format = "d", big.mark = ","),
        "Participating organizations",
        icon = icon("users"),  # Changed icon name
        color = "orange"
      )
    })
    
    output$value3 <- renderValueBox({
      valueBox(
        HTML(paste0("<div style='font-size: 24px;'>Jun - Nov 2022</div>")),
        subtitle = "Collection period",
        icon = icon("calendar"),
        color = "blue"
      )
    })
    
  
  
################################ DESCRIPTIVE STATISTICS ####################################
####SAMPLE STATISTICS
  
  output$pie_chart_total <- renderPlotly({
    var_selected <- input$pie_choice_organization
    df <- data_ready %>%
      group_by_at(var_selected) %>%
      summarise(Frequency = n()) %>%
      mutate(Percentage = (Frequency / sum(Frequency)) * 100)
    
    num_categories <- nrow(df)
    
    plot_ly(df, labels = ~get(var_selected), values = ~Percentage, type = 'pie', 
            marker = list(colors = viridis(num_categories))) %>%
      layout(title = paste("Pie chart of", var_selected))
  })


output$pie_chart_individual <- renderPlotly({
  var_selected <- input$pie_choice_individual
  df <- data_ready %>%
    group_by_at(var_selected) %>%
    summarise(Frequency = n()) %>%
    mutate(Percentage = (Frequency / sum(Frequency)) * 100)
  
  num_categories <- nrow(df)
  
  plot_ly(df, labels = ~get(var_selected), values = ~Percentage, type = 'pie', 
          marker = list(colors = viridis(num_categories))) %>%
    layout(title = paste("Pie chart of", var_selected))
})



output$contingency_plot <- renderPlotly({
  
  var1 <- input$var1
  var2 <- input$var2
  
  # Check if the two variables are the same to avoid duplication
  if (var1 == var2) {
    return(NULL)
  }
  
  # Create a local copy of data_ready to avoid side effects
  local_data <- data_ready %>% 
    group_by(!!sym(var1), !!sym(var2)) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  plot <- ggplot(local_data, aes_string(x = var1, y = "count", fill = var2)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = sprintf("%.1f%%", percentage), y = count / 2), position = position_fill(vjust = 0.5), size = 3) +
    labs(
      x = ' ', 
      y = 'Count (%)',
      fill = var2
    ) +
    scale_fill_brewer(palette="Set2") +
    theme_minimal() +
    theme(text = element_text(size = 11)) +
    ggtitle(paste("Frequency of", var2, "in each", var1))
  
  ggplotly(plot)
})

############################## MAP INTRODUCTION ##############################################

data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")

# Logic for the map
output$map <- renderLeaflet({
  
  # Prepare the coordinates data
  coordinates_data <- data.frame(
    Provincia = c('Genova', 'Savona', 'Mantova', 'Milano', 'Benevento', 'Brescia', 
                  'Lecce', 'Torino', 'Alessandria', 'Terni', 'Vicenza', 'Bologna', 
                  'Firenze', 'Ancona', 'Roma'),
    Coordinates = c('44.4471043,8.726019', '44.3347052,8.3412585', '45.1618459,10.7320508', '45.4626154,9.0129249',
                    '41.1230179,14.7314898', '45.5436978,10.1412768', '40.3541888,18.1330946', '45.0736127,7.5107937',
                    '44.9085857,8.556058', '42.5621299,12.545681', '45.5503125,11.4685125', '44.4992289,11.2492835',
                    '43.7800525,11.1585658', '43.5822664,13.4244243', '41.9101108,12.2063328')
  )
  
  # Split the Coordinates into Lat and Long
  coordinates_data$Lat <- as.numeric(sapply(strsplit(coordinates_data$Coordinates, ","), "[[", 1))
  coordinates_data$Long <- as.numeric(sapply(strsplit(coordinates_data$Coordinates, ","), "[[", 2))
  coordinates_data$Coordinates <- NULL  # Removing the combined column
  
  
  # Merge the data
  merged_data <- merge(data_ready, coordinates_data, by.x = "province", by.y = "Provincia")
  
  # Aggregate data
  aggregated_data <- merged_data %>%
    group_by(province, Lat, Long, dimension, sector, organization_code) %>%
    summarise(Num_Respondents = n(), .groups = 'drop')
  
  # Create a color palette for the dimension
  pal <- colorFactor(c("red", "green", "blue", "yellow"), domain = aggregated_data$dimension)
  
  
  # Plot the data using leaflet
  map <- leaflet(data = aggregated_data) %>%
    addTiles() %>%
    addCircleMarkers(~Long, ~Lat, 
                     popup = paste("Registered office:", aggregated_data$province,
                                   "<br>Organization Code:", aggregated_data$organization_code,
                                   "<br>Dimension:", aggregated_data$dimension, 
                                   "<br>Sector:", aggregated_data$sector,
                                   "<br>Number of Respondents:", aggregated_data$Num_Respondents),
                     color = ~pal(dimension),
                     radius = 5, fillOpacity = 0.8) %>%
    addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
    setView(12.4924, 41.8902, zoom = 5)  # Adjusted the zoom level to focus more on Italy
  
  map
})

######DONUT CHART
# Add this to your server function in your Shiny app
output$donut_plot_bes <- renderPlotly({
  
  cronbach_list_bes <- list(
    Politics <- c("Q1", "Q17", "Q19", "Q21", "Q29", "Q35", "Q72", "Q78", "Q79", "Q80", "Q81", "Q83", "Q84", "Q115", "Q117"),
    Security <- c("Q2", "Q3", "Q7", "Q62"),
    Health <- c("Q4", "Q6", "Q8", "Q10", "Q67", "Q68", "Q69", "Q70", "Q76"),
    Socialrelationships <- c("Q5", "Q16", "Q22", "Q36", "Q37", "Q40", "Q42", "Q44", "Q54", "Q56", "Q60", "Q61", "Q85", "Q86", "Q93", "Q94", "Q95", "Q98", "Q99", "Q101", "Q102", "Q114", "Q118", "Q119"),
    Worklifebalance <- c("Q9", "Q11", "Q12", "Q23", "Q25", "Q27", "Q28", "Q30", "Q31", "Q32", "Q33", "Q34", "Q38", "Q39", "Q41", "Q43", "Q45", "Q46", "Q49", "Q52", "Q59", "Q63", "Q64", "Q65", "Q71", "Q75", "Q82","Q87", "Q88","Q92", "Q103", "Q104", "Q105", "Q107", "Q109", "Q110", "Q112", "Q116"),
    Naturalculturalheritage <- c("Q13", "Q14"),
    Qualityofservices <- c("Q15", "Q20", "Q73", "Q120", "Q122"),
    Economicwellbeing <- c("Q24", "Q26", "Q89", "Q123", "Q124", "Q125", "Q96", "Q97"),
    Innovationresearchcreativity <- c("Q47", "Q50", "Q51"),
    Education <- c("Q48", "Q100", "Q121"),
    Environment <- c("Q108", "Q113"),
    Subjectivewellbeing <- c("Q18", "Q53", "Q55", "Q57", "Q58", "Q66", "Q74", "Q77", "Q90", "Q91", "Q106", "Q111", "Q126", "Q127"))
  
  names(cronbach_list_bes) <- c("Politics", "Security", "Health", "Social relationships", "Work-life balance", "Natural cultural heritage", "Quality of services", "Economic well-being", "Innovation, research, creativity", "Education", "Environment", "Subjective well-being")
  
  domain_names <- names(cronbach_list_bes)
  num_questions <- sapply(cronbach_list_bes, length)
  donut_data <- data.frame(Domain = domain_names, Count = num_questions)
  
  total_count <- sum(donut_data$Count)
  percent_count <- (donut_data$Count / total_count) * 100
  
  # Create hover text
  hover_text <- paste("Domain:  " = domain_names, "<br>Number of items: ", donut_data$Count, "<br>Percentage: ", round(percent_count, 2), "%")
  
  # Create the donut chart
  plot_ly(donut_data, ids = ~Domain, values = ~Count, labels = ~Domain, type = 'pie', hole = 0.3, textinfo = "label", hovertext = ~hover_text, hoverinfo = "text") %>%
    layout(title = "Distribution of Items Across 12 Well-Being Domains",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend=FALSE)
})

output$donut_plot_health <- renderPlotly({
  
  cronbach_list_health <- list(
  health_prevention <- c("Q1", "Q2", "Q3", "Q4", "Q5"),
  health_workenvironment <- c("Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15"),
  health_activelistening <- c("Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23"),
  health_equity <- c("Q24", "Q25", "Q26", "Q27", "Q28", "Q29"),
  health_administrativeefficiency <- c("Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37"),
  health_jobsustainability <- c("Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46"),
  health_openinnovation <- c("Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q43"),
  health_conflicts <- c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64"),
  health_stresslevels <- c("Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "Q72", "Q73", "Q74", "Q75", "Q76", "Q77"),
  health_information <- c("Q78", "Q79", "Q80", "Q81"),
  health_clearobjectives <- c("Q82", "Q83", "Q84", "Q85", "Q86", "Q87", "Q88", "Q89", "Q90"),
  health_skillsenhancement <- c("Q90", "Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99", "Q100", "Q101", "Q102"),
  health_meaningfulness <- c("Q102", "Q103", "Q104", "Q105", "Q106", "Q107", "Q108", "Q109", "Q110", "Q111", "Q112", "Q113"),
  health_relationalenvironment <- c("Q114", "Q115", "Q116", "Q117", "Q118", "Q119", "Q126", "Q127"),
  health_organizationalwelfare<- c("Q120", "Q121", "Q122", "Q123", "Q124", "Q125"))

  
  names(cronbach_list_health) <- c("Accidents prevention", "Healthy work environment", "Active listening", "Equity", "Administrative efficiency", "Job sustainability", "Openness to innovation", "Conflicts management", "Stress levels control", 
                                   "Sharing of information", "Clear objectives", "Skills enhancement", "Sense of usefulness", "Relational environment", "Organizational welfare" )
  
  domain_names <- names(cronbach_list_health)
  num_questions <- sapply(cronbach_list_health, length)
  donut_data <- data.frame(Domain = domain_names, Count = num_questions)
  
  total_count <- sum(donut_data$Count)
  percent_count <- (donut_data$Count / total_count) * 100
  
  # Create hover text
  hover_text <- paste("Domain:  " = domain_names, "<br>Number of items: ", donut_data$Count, "<br>Percentage: ", round(percent_count, 2), "%")
  
  # Create the donut chart
  plot_ly(donut_data, ids = ~Domain, values = ~Count, labels = ~Domain, type = 'pie', hole = 0.3, textinfo = "label", hovertext = ~hover_text, hoverinfo = "text") %>%
    layout(title = "Distribution of Items Across 15 Organizational Health Domains",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend=FALSE)
})

output$donut_plot_subjwb <- renderPlotly({
  
  cronbach_list_subjwb <- list(
  managementesteem <- c("Q1", "Q5", "Q20", "Q21", "Q29", "Q59", "Q72"),
  changeability <- c("Q4", "Q32"),
  selfrealization <- c("Q18", "Q48", "Q105", "Q108"),
  interpersonal <- c("Q22", "Q60", "Q61", "Q62", "Q64", "Q94", "Q99", "Q101", "Q118"),
  managementcredibility <- c("Q30", "Q41", "Q42", "Q44", "Q81"),
  team<- c("Q35", "Q36", "Q40", "Q54", "Q57"),
  organizationsuccess <- c("Q37", "Q53"),
  worklifebalance <- c("Q38", "Q39", "Q52", "Q67", "Q69", "Q70", "Q71", "Q75", "Q77"),
  values <- c("Q78", "Q79", "Q80", "Q116"),
  gotowork <- c("Q110"),
  satisfaction <- c("Q103", "Q107"),
  commitment <- c("Q112"))
  
  names(cronbach_list_bes) <- c("Politics", "Security", "Health", "Social relationships", "Work-life balance", "Natural cultural heritage", "Quality of services", "Economic well-being", "Innovation, research, creativity", "Education", "Environment", "Subjective well-being")
  
  names(cronbach_list_subjwb)  <- c(
    "Management esteem", "Changeability", "Self-realization", "Interpersonal relationships","Management credibility","Teamwork", "Organization success",
    "Work-life balance", "Values", "Desire to go to work","Job satisfaction","Commitment to work")
  
  domain_names <- names(cronbach_list_subjwb)
  num_questions <- sapply(cronbach_list_subjwb, length)
  donut_data <- data.frame(Domain = domain_names, Count = num_questions)
  
  total_count <- sum(donut_data$Count)
  percent_count <- (donut_data$Count / total_count) * 100
  
  # Create hover text
  hover_text <- paste("Domain:  " = domain_names, "<br>Number of items: ", donut_data$Count, "<br>Percentage: ", round(percent_count, 2), "%")
  
  # Create the donut chart
  plot_ly(donut_data, ids = ~Domain, values = ~Count, labels = ~Domain, type = 'pie', hole = 0.3, textinfo = "label", hovertext = ~hover_text, hoverinfo = "text") %>%
    layout(title = "Distribution of Items Across 12 Subjective Well-Being Domains",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend=FALSE)
})

output$donut_plot_subjma <- renderPlotly({
  
  cronbach_list_subjma <- list(
  organizationalconfusion <- c("Q27", "Q45", "Q46", "Q87"),
  absenteeism <- c("Q65"),
  changejob <- c("Q104"),
  irrelevance <- c("Q85", "Q86", "Q109"),
  unrecognized <- c("Q24", "Q26", "Q90", "Q91", "Q96", "Q97"),
  unusefulness<- c("Q92", "Q95"),
  rulesadherence <- c("Q88"),
  proactivity <- c("Q17", "Q102"),
  jobinterest <- c("Q23", "Q111"),
  resentment <- c("Q28", "Q63", "Q89"),
  slowness <- c("Q31", "Q34"),
  aggressive <- c("Q55"),
  gossip <- c("Q114", "Q119"))
  
  
  names(cronbach_list_subjma)<- c(
    "Organizational confusion","Absenteeism","Desire to change job","Feeling of irrelevance","Feeling unrecognized","Feeling of unusefulness", "Rules adherence and emotional detachment",
    "Lack of cognitive proactivity", "Lack of interest in job", "Feeling of resentment","Slowness in work","Aggressiveness","Gossiping")

  domain_names <- names(cronbach_list_subjma)
  num_questions <- sapply(cronbach_list_subjma, length)
  donut_data <- data.frame(Domain = domain_names, Count = num_questions)
  
  total_count <- sum(donut_data$Count)
  percent_count <- (donut_data$Count / total_count) * 100
  
  # Create hover text
  hover_text <- paste("Domain:  " = domain_names, "<br>Number of items: ", donut_data$Count, "<br>Percentage: ", round(percent_count, 2), "%")
  
  # Create the donut chart
  plot_ly(donut_data, ids = ~Domain, values = ~Count, labels = ~Domain, type = 'pie', hole = 0.3, textinfo = "label", hovertext = ~hover_text, hoverinfo = "text") %>%
    layout(title = "Distribution of Items Across 7 Stress Absence Domains",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend=FALSE)
})

output$donut_plot_stress <- renderPlotly({
  
  cronbach_list_stress <- list(
    change <- c("Q47", "Q78", "Q79", "Q80"),
    colleagues <- c("Q34", "Q36", "Q40", "Q54", "Q57", "Q58", "Q64", "Q90", "Q98", "Q99"),
    control <- c("Q31", "Q52", "Q75", "Q76", "Q100", "Q116"),
    demand <- c("Q1", "Q38", "Q39", "Q42", "Q43", "Q44", "Q45", "Q46", "Q67", "Q68", "Q71", "Q92", "Q95", "Q115"),
    management <- c("Q2", "Q5", "Q8", "Q9", "Q11", "Q12", "Q15", "Q18", "Q20", "Q30", "Q32", "Q37", "Q49", "Q59", "Q63", "Q72", "Q89", "Q91"),
    relations<- c("Q4", "Q17", "Q22", "Q60", "Q61", "Q62", "Q85", "Q86", "Q94", "Q101", "Q102", "Q114", "Q118", "Q119"),
    role<- c("Q24", "Q25", "Q26", "Q27", "Q82", "Q84", "Q87", "Q88", "Q96", "Q97", "Q105"))
    
  names(cronbach_list_stress) <-c("Change","Colleagues","Control","Demand","Management","Relations","Role")
  
  domain_names <- names(cronbach_list_stress)
  num_questions <- sapply(cronbach_list_stress, length)
  donut_data <- data.frame(Domain = domain_names, Count = num_questions)
  
  total_count <- sum(donut_data$Count)
  percent_count <- (donut_data$Count / total_count) * 100
  
  # Create hover text
  hover_text <- paste("Domain:  " = domain_names, "<br>Number of items: ", donut_data$Count, "<br>Percentage: ", round(percent_count, 2), "%")
  
  # Create the donut chart
  plot_ly(donut_data, ids = ~Domain, values = ~Count, labels = ~Domain, type = 'pie', hole = 0.3, textinfo = "label", hovertext = ~hover_text, hoverinfo = "text") %>%
    layout(title = "Distribution of Items Across 7 Stress Absence Domains",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
           showlegend=FALSE)
})

##################################### COMPOSITE MEASURES SECTION #####################################
##SUMMARY STATISTICS TABLE AND HISTOGRAMS
output$summaryTable <- renderFormattable({
  
  var_list <- c("subjective_wellbeing", "stress",   "subjective_malaise", "bes_wellbeing", "health_organization", "participation_level")
  
  new_var_list <- c("Subjective well-being", "Absence of work-related stress",  "Subjective malaise", "Workers' well-being", "Organizational health", "Participation level")
  
  names_map <- setNames(var_list, new_var_list)
  new_data<- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = measures,
    Mean = sapply(new_var_list, function(x) mean(new_data[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    #Measure = formatter("span", style = x ~ ifelse(x == "Measure", "font-weight:bold;", "")),
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})

output$histPlot_bes <- renderPlotly({
  
  selected_var <- input$var_choice_hist
  cut_score <- input$cut_score
  
  # Categorize data based on cut-score
  data_ready$category <- ifelse(data_ready[[selected_var]] < cut_score, "Below",
                                ifelse(data_ready[[selected_var]] > cut_score, "Above", "Equal"))
  
  fig <- plot_ly(data_ready, x = ~get(selected_var), type = "histogram", histnorm = "count", 
                 color = ~category, colors = c("Below" = "lightblue", "Equal" = "gray", "Above" = "orange"),
                 marker = list(line = list(color = 'rgba(0, 0, 0, 0.2)', width = 0.5))) %>%
    layout(title = paste("Histogram of", selected_var),
           xaxis = list(title = selected_var),
           yaxis = list(title = "Frequency"),
           barmode = 'overlay',
           showlegend = FALSE)
  
  fig
})

### INTERACTIVE HISTOGRAMS FOR COMPOSITE MEASURES
output$histPlot_part <- renderPlotly({
  
  cut_score <- input$cut_score_part
  
  # Categorize data based on cut-score
  data_ready$category <- ifelse(data_ready$participation_level < cut_score, "Below",
                                ifelse(data_ready$participation_level > cut_score, "Above", "Equal"))
  
  fig <- plot_ly(data_ready, x = ~participation_level, type = "histogram", histnorm = "count", 
                 color = ~category, colors = c("Below" = "lightblue", "Equal" = "gray", "Above" = "orange"),
                 marker = list(line = list(color = 'rgba(0, 0, 0, 0.2)', width = 0.5))) %>%
    layout(title = "Participation Level Histogram",
           xaxis = list(title = "Participation Level"),
           yaxis = list(title = "Frequency"),
           barmode = 'overlay',
           showlegend = FALSE)
  
  fig
})


# CORRELATION PLOT
output$dotplot_correlation <- renderPlotly({
  
  viridis_colors <- viridis(22)
  selected_measure <- input$measure_choice_dotplot
  
  # Mapping the internal variable name to its display name for the plot title and x-axis label
  measure_names <- c(
    "bes_wellbeing" = "Organizational well-being",
    "health_organization" = "Organizational health",
    "subjective_malaise" = "Subjective malaise",
    "subjective_wellbeing" = "Subjective well-being",
    "stress" = "Absence of work-related stress"
  )
  
  display_name <- measure_names[selected_measure]
  
  x_vals <- data_ready[, selected_measure]
  y_vals <- data_ready$participation_level
  
  # Fit a linear model
  fit <- lm(y_vals ~ x_vals)
  fitted_vals <- fitted(fit)
  
  # Calculate standard error of the fitted values
  se_fit <- sqrt(sum(resid(fit)^2) / (length(fitted_vals) - 2))
  upper_bound <- fitted_vals + se_fit
  lower_bound <- fitted_vals - se_fit
  
  # Calculate Pearson correlation
  cor_value <- cor(x_vals, y_vals, use = "complete.obs")
  
  plot_ly(data = data_ready, 
          x = ~get(selected_measure), 
          y = ~participation_level, 
          type = "scatter", 
          mode = "markers",
          colors = viridis_colors,
          hoverinfo = "text") %>%
    add_ribbons(x = ~x_vals, ymin = ~lower_bound, ymax = ~upper_bound, fillcolor = 'rgba(255, 0, 0, 0.2)', line = list(color = 'transparent')) %>%  # Adding shaded region
    add_lines(x = ~x_vals, y = ~fitted_vals, line = list(color = 'red')) %>%  # Adding fitted line
    layout(title = paste("Participation Level vs", display_name), 
           xaxis = list(title = paste(display_name, "<br>", "Pearson correlation:", round(cor_value, 3))),  # Adding Pearson correlation below x-axis label
           yaxis = list(title = "Participation Level"), 
           showlegend = FALSE,
           margin = list(b = 60)  # Extra bottom margin for the annotation
    )
})
  



##CORRELATION HEATMAP

output$heatmaply_plot <- renderPlotly({
  
  var_list <- c("participation_level", "bes_wellbeing", "health_organization", "subjective_wellbeing", "subjective_malaise", "stress")
  
  # Define new names for the variables
  new_var_list <- c("Participation level", "Workers' well-being", "Organizational health", "Subjective well-being", "Subjective malaise", "Absence of stress")
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})



##### PARAGONING ALL THE MEASURES ###################################################################
##### TOTAL DOTS PLOT
  output$composite_plot_total <- renderPlotly({
    selected_measure <- input$measure_choice
    color_choice <- input$color_choice
    
    # Mapping the internal variable name to its display name for the plot title and x-axis label
    measure_names <- c(
      "bes_wellbeing" = "Organizational well-being",
      "health_organization" = "Organizational health",
      "subjective_malaise" = "Subjective malaise",
      "subjective_wellbeing" = "Subjective well-being",
      "stress" = "Absence of work-related tress"
    )
    display_name <- measure_names[selected_measure]
    
    # Calculate the averages
    avg_data <- data_ready %>%
      group_by(organization_code, province, dimension, sector, juridical_form) %>%
      summarise(avg_participation = mean(participation_level, na.rm = TRUE),
                avg_measure = mean(get(selected_measure), na.rm = TRUE))
    
    # Normalize the avg_measure values for better representation as sizes
    normalized_size <- scales::rescale(avg_data$avg_measure, to = c(10, 30))
    
    # Determine the number of unique colors needed based on the selected variable
    n_colors <- length(unique(avg_data[[color_choice]]))
    
    # Generate a color palette
    color_palette <- viridis(n_colors)
    
    # Map colors to the data
    mapped_colors <- color_palette[as.factor(avg_data[[color_choice]])]
    
    
    p <- plot_ly() 
    
    unique_categories <- unique(avg_data[[color_choice]])
    n_colors <- length(unique_categories)
    color_palette <- viridis(n_colors)
    
    for(i in seq_along(unique_categories)) {
      category <- unique_categories[i]
      color <- color_palette[i]
      
      filtered_data <- avg_data[avg_data[[color_choice]] == category, ]
      
      p <- add_trace(p, 
                     data = filtered_data, 
                     x = ~avg_measure, 
                     y = ~avg_participation, 
                     type = "scatter", 
                     mode = "markers", 
                     name = as.character(category),  # legend label
                     marker = list(size = normalized_size[avg_data[[color_choice]] == category], 
                                   color = color, 
                                   sizemode = "diameter"))
    }
    
    p <- layout(p, 
                title = paste("Average Participation Level vs", display_name), 
                xaxis = list(title = display_name), 
                yaxis = list(title = "Average Participation Level"), 
                showlegend = TRUE)
    
    return(p)
  })
  
##### INDIVIDUAL DOTS PLOT
  
  output$composite_plot_individual <- renderPlotly({
    viridis_colors <- viridis(22)
    selected_measure <- input$measure_choice
    color_variable <- input$color_choice_individual  # Added this line
    
    # Mapping the internal variable name to its display name for the plot title and x-axis label
    measure_names <- c(
      "bes_wellbeing" = "Organizational well-being",
      "health_organization" = "Organizational health",
      "subjective_malaise" = "Subjective malaise",
      "subjective_wellbeing" = "Subjective well-being",
      "stress" = "Absence of work-related stress"
    )
    
    display_name <- measure_names[selected_measure]
    
    plot_ly(data = data_ready, 
            x = ~get(selected_measure), 
            y = ~participation_level, 
            type = "scatter", 
            mode = "markers",
            colors = viridis_colors,
            color = ~factor(get(color_variable)),  # Added this line
            text = ~paste("Code:", organization_code, 
                          "<br>Province:", province, 
                          "<br>Dimension:", dimension, 
                          "<br>Sector:", sector,
                          "<br>Juridical form:", juridical_form),
            hoverinfo = "text") %>% 
      layout(title = paste("Participation Level vs", display_name), 
             xaxis = list(title = display_name), 
             yaxis = list(title = "Participation Level"), 
             showlegend = TRUE)  # Enabling the legend to show colors by dimension or other category
  })

avg_data_table <- reactive({
  selected_measure <- input$measure_choice
  data_ready %>%
    group_by(organization_code, province, sector, dimension, juridical_form) %>%
    summarise(
      avg_participation = mean(participation_level, na.rm = TRUE),
      avg_measure = mean(get(selected_measure), na.rm = TRUE)
    )
})

# Existing server logic
output$avg_data_output <- renderDT({
  datatable(avg_data_table(), options = list(scrollX = TRUE))
})

  
  
#########PARTICIPATION #####################################################################################
######### 21 PARTICIPATION ITEMS

output$summaryTable_part <- renderFormattable({
  
  var_list <- c(
    "part_joborg",
    "part_area",
    "part_health",
    "part_environment",
    "part_development",
    "part_training",
    "part_intcomm",
    "part_welfare",
    "part_innovation",
    "part_intconflicts",
    "part_hr",
    "part_general",
    "part_remuneration",
    "part_extcomm",
    "part_occupation",
    "part_suppliers",
    "part_stakeholder",
    "part_extconflicts",
    "part_econstatus",
    "part_investments",
    "part_shareholders"
  )
  
  new_var_list <- c(
    "Work organization",
    "Area planning",
    "Health protection",
    "Environmental sustainability",
    "Organization development",
    "Skilss development",
    "Internal communication",
    "Organization welfare",
    "Product-process innovation",
    "Internal conflicts",
    "H&R",
    "General planning",
    "Remuneration",
    "External communication",
    "Employment",
    "Suppliers selection",
    "Stakeholders involvement",
    "External conflicts",
    "Economic-financial status",
    "Investments",
    "Employee share ownership"
  )
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_part <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_part[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_part[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_part[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_part[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_part[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_part[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_part[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_part[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_part[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})

output$interactive_plot <- renderPlotly({
  
  var_list <- c("part_welfare", "part_development", "part_hr", "part_joborg", 
                "part_remuneration", "part_intcomm", "part_extcomm", "part_training", 
                "part_health", "part_econstatus", "part_suppliers", "part_innovation",
                "part_investments", "part_occupation", "part_general", "part_area", 
                "part_shareholders", "part_intconflicts", "part_extconflicts", 
                "part_stakeholder", "part_environment")
  
  # New names you want to give to these variables
  new_var_list <- c("Organization welfare", "Organization development", "H&R", "Work organization",
                    "Remuneration", "Internal communication", "External communication",
                    "Skilss development", "Health protection", "Economic-financial status",
                    "Suppliers selection", "Product-process innovation", "Investments", "Employment", "General planning", "Area planning", "Employee share ownership", "Internal conflicts", "External conflicts", "Stakeholders involvement", "Environmental sustainability")
  
  # Creating a named vector for renaming
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
      
      part_data <- new_data[, new_var_list]

      # Calculate averages for each column
      avg_values <- colMeans(part_data, na.rm = TRUE)
      
      # Convert to data frame for plotting
      plot_data <- data.frame(
        variable = names(avg_values),
        average = avg_values,
        size = scales::rescale(avg_values, to = c(10, 30))
      )
      
      # Create the plot
      plot <- plot_ly(data = plot_data, 
                      x = ~average, 
                      y = ~variable, 
                      type = "scatter", 
                      mode = "markers", 
                      marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
        layout(title = "Participation items", xaxis = list(title = "Average level", range = c(0, 5)),
               yaxis = list(title = FALSE))
      
      plot
    })

    #### DENSITY PARTICIPATION PLOT
    output$participation_density_plot <- renderPlotly({
      selected_category <- input$category_choice_density
      
      # Create an empty plotly object
      plot <- plot_ly()
      
      # Generate density data for each group in the selected category
      for(group in unique(data_ready[[selected_category]])) {
        data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
        density_data <- density(data_sub$participation_level, na.rm = TRUE)
        df_density <- data.frame(x = density_data$x, y = density_data$y)
        
        # Add the density curve for this group
        plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
        
        # Add hover text (optional)
        plot <- plot %>% add_trace(
          x = ~df_density$x,
          y = ~df_density$y,
          text = ~paste("Dimension: ", group,
                        "<br>Mean: ", round(mean(data_sub$participation_level, na.rm = TRUE), 2),
                        "<br>Median: ", round(median(data_sub$participation_level, na.rm = TRUE), 2),
                        "<br>SD: ", round(sd(data_sub$participation_level, na.rm = TRUE), 2)),
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = FALSE,
          marker = list(size = 1, opacity = 0),
          data = df_density
        )
      }
      
      # Add titles and axis labels
      plot <- plot %>% layout(
        title = paste("Probability distribution by", selected_category),
        xaxis = list(title = "Participation level"),
        yaxis = list(title = "Density")
      )
      
      plot
    })
    
    output$participation_boxplot <- renderPlotly({
      selected_category <- input$category_choice_density
      
      # Generate the boxplot using plotly
      plot <- plot_ly(data = data_ready, x = ~participation_level, type = "box", color = ~get(selected_category)) %>%
        layout(title = "Boxplot of participation level by category",
               xaxis = list(title = "Participation level"),
               yaxis = list(title = selected_category))  # Add the viridis colors here
      return(plot)
    })
    
    

output$interactive_participation_plot <- renderPlotly({
  var_list <- c("part_welfare", "part_development", "part_hr", "part_joborg", 
                "part_remuneration", "part_intcomm", "part_extcomm", "part_training", 
                "part_health", "part_econstatus", "part_suppliers", "part_innovation",
                "part_investments", "part_occupation", "part_general", "part_area", 
                "part_shareholders", "part_intconflicts", "part_extconflicts", 
                "part_stakeholder", "part_environment")
  
  # New names you want to give to these variables
  new_var_list <- c("Organization welfare", "Organization development", "H&R", "Work organization",
                    "Remuneration", "Internal communication", "External communication",
                    "Skilss development", "Health protection", "Economic-financial status",
                    "Suppliers selection", "Product-process innovation", "Investments", "Employment", "General planning", "Area planning", "Employee share ownership", "Internal conflicts", "External conflicts", "Stakeholders involvement", "Environmental sustainability")
  
  # Creating a named vector for renaming
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
      
      # Get the selected category from the user input
      selected_category <- input$category_choice
      
      # Calculate the average for each of the 21 variables, grouped by the selected category
      avg_data <- new_data %>%
        group_by(!!sym(selected_category)) %>%
        summarise(across(starts_with(new_var_list), mean, na.rm = TRUE))
      
      # Convert data to long format for plotting
      avg_data_long <- avg_data %>% 
        pivot_longer(cols = starts_with(new_var_list), names_to = "Variable", values_to = "Average")
      
      # Create the plot
      plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
        layout(title = "Average participation level by category", yaxis = list(title = FALSE), xaxis = list(range = c(0, 4)))
    })
    
    
      
####### WELL-BEING ##############################################
#### 12 WELL-BEING DOMAINS

output$summaryTable_bes <- renderFormattable({
  
  var_list <- c(
    "bes_security",
    "bes_socialrelationships",
    "bes_environment",
    "bes_politics",
    "bes_worklifebalance",
    "bes_subjectivewellbeing",
    "bes_health",
    "bes_education",
    "bes_qualityofservices",
    "bes_naturalculturalheritage",
    "bes_economicwellbeing",
    "bes_innovationresearchcreativity"
  )
  

  new_var_list <- c(
    "Security",
    "Social Relationships",
    "Environment",
    "Politics",
    "Work-Life Balance",
    "Subjective Wellbeing",
    "Health",
    "Education",
    "Quality of Services",
    "Natural & Cultural Heritage",
    "Economic Well-being",
    "Innovation, Research & Creativity"
  )
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_bes <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_bes[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_bes[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_bes[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_bes[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_bes[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_bes[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_bes[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})


# List of variables of interest and their new names
 output$bes_interactive_plot <- renderPlotly({
   
   var_list <- c("bes_politics", "bes_security", "bes_health", "bes_socialrelationships",
                 "bes_worklifebalance", "bes_naturalculturalheritage","bes_qualityofservices",
                 "bes_economicwellbeing","bes_innovationresearchcreativity","bes_education",
                 "bes_environment","bes_subjectivewellbeing")
   
   new_var_list <- c("Politics", "Security", "Health", "Social Relationships",
                     "Work-Life Balance", "Natural & Cultural Heritage", "Quality of Services",
                     "Economic Wellbeing", "Innovation, Research & Creativity", "Education",
                     "Environment", "Subjective Wellbeing")
   
   # Rename variables
   names_map <- setNames(var_list, new_var_list)
   new_data <- data_ready %>% rename(!!!names_map)
   
   viridis_colors <- viridis(22)
   
        
        # Filter data to keep only the columns of interest
        bes_data <- new_data[, new_var_list]
        
        # Calculate averages for each column
        avg_values <- colMeans(bes_data, na.rm = TRUE)
        
        # Convert to data frame for plotting
        plot_data <- data.frame(
          variable = names(avg_values),
          average = avg_values,
          size = scales::rescale(avg_values, to = c(10, 30))
        )
        
        # Create the plot
        plot <- plot_ly(data = plot_data, 
                        x = ~average, 
                        y = ~variable, 
                        type = "scatter", 
                        mode = "markers", 
                        marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
          layout(title = "Workers' well-being domains", xaxis = list(title = "Average Level", range = c(1, 5)),
                 yaxis = list(title = FALSE))
        
        plot
      })
    
#### WELL-BEING DENSITY PLOT
      
output$bes_plot <- renderPlotly({
        selected_category <- input$bes_category_choice
        
        # Create an empty plotly object
        plot <- plot_ly()
        
        # Generate density data for each group in the selected category
        for(group in unique(data_ready[[selected_category]])) {
          data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
          density_data <- density(data_sub$bes_wellbeing, na.rm = TRUE)
          df_density <- data.frame(x = density_data$x, y = density_data$y)
          
          # Add the density curve for this group
          plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
          
          # Add hover text (optional)
          plot <- plot %>% add_trace(
            x = ~df_density$x,
            y = ~df_density$y,
            text = ~paste("Dimension: ", group,
                          "<br>Mean: ", round(mean(data_sub$bes_wellbeing, na.rm = TRUE), 2),
                          "<br>Median: ", round(median(data_sub$bes_wellbeing, na.rm = TRUE), 2),
                          "<br>SD: ", round(sd(data_sub$bes_wellbeing, na.rm = TRUE), 2)),
            hoverinfo = "text",
            type = 'scatter',
            mode = 'markers',
            showlegend = FALSE,
            marker = list(size = 1, opacity = 0),
            data = df_density
          )
        }
        
        # Add titles and axis labels
        plot <- plot %>% layout(
          title = paste("Probability distribution by", selected_category),
          xaxis = list(title = "Workers' well-being"),
          yaxis = list(title = "Density")
        )
        
        plot
      })

### WELL-BEING BOX PLOTS
output$bes_boxplot <- renderPlotly({
  selected_category <- input$bes_category_choice
  
  # Generate the boxplot using plotly
  plot <- plot_ly(data = data_ready, x = ~bes_wellbeing, type = "box", color = ~get(selected_category)) %>%
    layout(title = "Boxplot of workers' well-being by category",
           xaxis = list(title = "Workers' well-being", range=c(1.5, 5)),
           yaxis = list(title = selected_category))
  
  return(plot)
})

## WELL BEING BARS

# Server
# Server code
output$interactive_bes_plot <- renderPlotly({
  
  var_list <- c("bes_politics", "bes_security", "bes_health", "bes_socialrelationships",
                "bes_worklifebalance", "bes_naturalculturalheritage","bes_qualityofservices",
                "bes_economicwellbeing","bes_innovationresearchcreativity","bes_education",
                "bes_environment","bes_subjectivewellbeing")
  
  new_var_list <- c("Politics", "Security", "Health", "Social Relationships",
                    "Work-Life Balance", "Natural & Cultural Heritage", "Quality of Services",
                    "Economic Wellbeing", "Innovation, Research & Creativity", "Education",
                    "Environment", "Subjective Wellbeing")
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Get the selected category from the user input
  selected_category <- input$category_choice_bes
  
  # Calculate averages
  avg_data <- new_data %>%
    group_by(!!sym(selected_category)) %>%
    summarise(across(all_of(new_var_list), mean, na.rm = TRUE))  # Note the change here
  
  # Convert to long format
  avg_data_long <- avg_data %>% 
    pivot_longer(cols = all_of(new_var_list), names_to = "Variable", values_to = "Average")  # Note the change here
  
  # Create the plot
  plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
    layout(title = "Average workers' well-being by category", yaxis = list(title = FALSE),  xaxis = list(range = c(1, 5)))
})

######### ORGANIZATIONAL HEALTH #####################################################################################
######### 15 HEALTH ITEMS

output$summaryTable_health <- renderFormattable({

  var_list <- c(
    "health_conflicts",
    "health_meaningfulness",
    "health_clearobjectives",
    "health_prevention",
    "health_activelistening",
    "health_workenvironment",
    "health_information",
    "health_skillsenhancement",
    "health_administrativeefficiency",
    "health_relationalenvironment",
    "health_equity",
    "health_jobsustainability",
    "health_stresslevels",
    "health_openinnovation",
    "health_organizationalwelfare"
  )
  
  new_var_list <- c(
    "Conflicts management",
    "Sense of usefulness",
    "Clear objectives",
    "Accidents prevention",
    "Active listening",
    "Healthy work environment",
    "Sharing of information",
    "Skilss enhancement",
    "Administrative efficiency",
    "Collaborative relational environment",
    "Equity",
    "Job sustainability",
    "Stress levels control",
    "Openness to innovation",
    "Organizational welfare"
  )
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_bes <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_bes[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_bes[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_bes[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_bes[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_bes[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_bes[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_bes[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})

output$health_interactive_plot <- renderPlotly({
  
  # List of variables of interest
  var_list <- c("health_prevention", "health_workenvironment", "health_activelistening",
                "health_equity", "health_administrativeefficiency", "health_jobsustainability",
                "health_openinnovation", "health_conflicts", "health_stresslevels",
                "health_information", "health_clearobjectives", "health_skillsenhancement",
                "health_meaningfulness", "health_relationalenvironment", "health_organizationalwelfare")
  
  
  # New names you want to give to these variables
  new_var_list <- c("Accidents prevention", "Healthy work environment", "Active listening", "Equity",
                    "Administrative efficiency", "Job sustainability", "Openness to innovation",
                    "Conflicts management", "Stress levels control", "Sharing of information",
                    "Clear objectives", "Skilss enhancement", "Sense of usefulness", "Collaborative relational environment", "Organizational welfare")
  
  # Creating a named vector for renaming
  names_map <- setNames(var_list, new_var_list)
  
  # Assuming `data` is your original dataframe
  # Using dplyr to rename variables
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Filter data to keep only the columns of interest
  health_data <- new_data[, new_var_list]
  
  # Calculate averages for each column
  avg_values <- colMeans(health_data, na.rm = TRUE)
  
  # Convert to data frame for plotting
  plot_data <- data.frame(
    variable = names(avg_values),
    average = avg_values,
    size = scales::rescale(avg_values, to = c(10, 30))
  )
  
  # Create the plot
  plot <- plot_ly(data = plot_data, 
                  x = ~average, 
                  y = ~variable, 
                  type = "scatter", 
                  mode = "markers", 
                  marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
    layout(title = "Organizational health dimensions", xaxis = list(title = "Average Level", range = c(1, 5)),
           yaxis = list(title = FALSE))
  
  plot
})

#### DENSITY HEALTH PLOT
output$health_density_plot <- renderPlotly({
  selected_category <- input$category_choice_health
  
  # Create an empty plotly object
  plot <- plot_ly()
  
  # Generate density data for each group in the selected category
  for(group in unique(data_ready[[selected_category]])) {
    data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
    density_data <- density(data_sub$health_organization, na.rm = TRUE)
    df_density <- data.frame(x = density_data$x, y = density_data$y)
    
    # Add the density curve for this group
    plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
    
    # Add hover text (optional)
    plot <- plot %>% add_trace(
      x = ~df_density$x,
      y = ~df_density$y,
      text = ~paste("Dimension: ", group,
                    "<br>Mean: ", round(mean(data_sub$health_organization, na.rm = TRUE), 2),
                    "<br>Median: ", round(median(data_sub$health_organization, na.rm = TRUE), 2),
                    "<br>SD: ", round(sd(data_sub$health_organization, na.rm = TRUE), 2)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE,
      marker = list(size = 1, opacity = 0),
      data = df_density
    )
  }
  
  # Add titles and axis labels
  plot <- plot %>% layout(
    title = paste("Probability distribution by", selected_category),
    xaxis = list(title = "Organizational health"),
    yaxis = list(title = "Density")
  )
  
  plot
})

output$health_boxplot <- renderPlotly({
  selected_category <- input$category_choice_health
  
  # Generate the boxplot using plotly
  plot <- plot_ly(data = data_ready, x = ~health_organization, type = "box", color = ~get(selected_category)) %>%
    layout(title = "Boxplot of organizational health by category",
           xaxis = list(title = "Organization health", range=c(1.5, 5)),
           yaxis = list(title = selected_category))  # Add the viridis colors here
  
  return(plot)
})


output$interactive_health_plot <- renderPlotly({
  
  # List of variables of interest
  var_list <- c("health_prevention", "health_workenvironment", "health_activelistening",
                "health_equity", "health_administrativeefficiency", "health_jobsustainability",
                "health_openinnovation", "health_conflicts", "health_stresslevels",
                "health_information", "health_clearobjectives", "health_skillsenhancement",
                "health_meaningfulness", "health_relationalenvironment", "health_organizationalwelfare")
  
  
  # New names you want to give to these variables
  new_var_list <- c("Accidents prevention", "Healthy work environment", "Active listening", "Equity",
                    "Administrative efficiency", "Job sustainability", "Openness to innovation",
                    "Conflicts management", "Stress levels control", "Sharing of information",
                    "Clear objectives", "Skilss enhancement", "Sense of usefulness", "Collaborative relational environment", "Organizational welfare")
  
  # Creating a named vector for renaming
  names_map <- setNames(var_list, new_var_list)
  
  # Assuming `data` is your original dataframe
  # Using dplyr to rename variables
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Get the selected category from the user input
  selected_category <- input$health_category_choice
  
  # Calculate the average for each of the 21 variables, grouped by the selected category
  avg_data_health <- new_data %>%
    group_by(!!sym(selected_category)) %>%
    summarise(across(starts_with(new_var_list), mean, na.rm = TRUE))
  
  # Convert data to long format for plotting
  avg_data_long <- avg_data_health %>% 
    pivot_longer(cols = starts_with(new_var_list), names_to = "Variable", values_to = "Average")
  
  # Create the plot
  plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
    layout(title = "Average organizational health by category", yaxis = list(title = FALSE), xaxis = list(range = c(1, 5)))
})

####################### SUBJECTIVE WELL-BEING ##############################################

output$summaryTable_subjwb <- renderFormattable({
  
  var_list <- c(
    "subjective_wb_interpersonal",
    "subjective_wb_commitment",
    "subjective_wb_organizationsuccess",
    "subjective_wb_satisfaction",
    "subjective_wb_gotowork",
    "subjective_wb_selfrealization",
    "subjective_wb_values",
    "subjective_wb_managementesteem",
    "subjective_wb_changeability",
    "subjective_wb_team",
    "subjective_wb_managementcredibility",
    "subjective_wb_worklifebalance"
  )
  
  
  new_var_list <- c(
    "Interpersonal relationships",
    "Commitment to work",
    "Organization success",
    "Job satisfaction",
    "Desire to go to work",
    "Self-realization",
    "Values",
    "Management esteem",
    "Changeability",
    "Teamwork",
    "Management credibility",
    "Work-life balance"
  )
  

  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_bes <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_bes[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_bes[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_bes[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_bes[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_bes[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_bes[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_bes[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})

# List of variables of interest and their new names
output$subjwb_interactive_plot <- renderPlotly({
  
  var_list <- c("subjective_wb_managementesteem", 
                   "subjective_wb_changeability", 
                   "subjective_wb_selfrealization", 
                   "subjective_wb_interpersonal", 
                   "subjective_wb_managementcredibility", 
                   "subjective_wb_team", 
                   "subjective_wb_organizationsuccess", 
                   "subjective_wb_worklifebalance", 
                   "subjective_wb_values", 
                   "subjective_wb_gotowork", 
                   "subjective_wb_satisfaction", 
                   "subjective_wb_commitment")
  
  
  new_var_list <- c("Management esteem", 
                    "Changeability", 
                    "Self-realization", 
                    "Interpersonal relationships", 
                    "Management credibility", 
                    "Teamwork", 
                    "Organization success", 
                    "Work-life balance", 
                    "Values", 
                    "Desire to go to work", 
                    "Job satisfaction", 
                    "Commitment to work")
  
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  
  # Filter data to keep only the columns of interest
  subjma_data <- new_data[, new_var_list]
  
  # Calculate averages for each column
  avg_values <- colMeans(subjma_data, na.rm = TRUE)
  
  # Convert to data frame for plotting
  plot_data <- data.frame(
    variable = names(avg_values),
    average = avg_values,
    size = scales::rescale(avg_values, to = c(10, 30))
  )
  
  # Create the plot
  plot <- plot_ly(data = plot_data, 
                  x = ~average, 
                  y = ~variable, 
                  type = "scatter", 
                  mode = "markers", 
                  marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
    layout(title = "Subjective well-being domains", xaxis = list(title = "Average level", range = c(1, 5)),
           yaxis = list(title = FALSE))
  
  plot
})

#### SUBJECTIVE WELL-BEING DENSITY PLOT

output$subjwb_plot <- renderPlotly({
  selected_category <- input$subjwb_category_choice
  
  # Create an empty plotly object
  plot <- plot_ly()
  
  # Generate density data for each group in the selected category
  for(group in unique(data_ready[[selected_category]])) {
    data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
    density_data <- density(data_sub$subjective_wellbeing, na.rm = TRUE)
    df_density <- data.frame(x = density_data$x, y = density_data$y)
    
    # Add the density curve for this group
    plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
    
    # Add hover text (optional)
    plot <- plot %>% add_trace(
      x = ~df_density$x,
      y = ~df_density$y,
      text = ~paste("Dimension: ", group,
                    "<br>Mean: ", round(mean(data_sub$subjective_wellbeing, na.rm = TRUE), 2),
                    "<br>Median: ", round(median(data_sub$subjective_wellbeing, na.rm = TRUE), 2),
                    "<br>SD: ", round(sd(data_sub$subjective_wellbeing, na.rm = TRUE), 2)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE,
      marker = list(size = 1, opacity = 0),
      data = df_density
    )
  }
  
  # Add titles and axis labels
  plot <- plot %>% layout(
    title = paste("Probability distribution by", selected_category),
    xaxis = list(title = "Subjective well-being"),
    yaxis = list(title = "Density")
  )
  
  plot
})

### SUBJECTIVE WELL-BEING BOX PLOTS
output$subjwb_boxplot <- renderPlotly({
  selected_category <- input$subjwb_category_choice
  
  # Generate the boxplot using plotly
  plot <- plot_ly(data = data_ready, x = ~subjective_wellbeing, type = "box", color = ~get(selected_category)) %>%
    layout(title = "Boxplot of subjective well-being by category",
           xaxis = list(title = "Subjective well-being", range=c(1.5, 5)),
           yaxis = list(title = selected_category))
  
  return(plot)
})

## SUBJECTIVE WELL BEING BARS

# Server
# Server code
output$interactive_subjwb_plot <- renderPlotly({
  
  var_list <- c("subjective_wb_managementesteem", 
                "subjective_wb_changeability", 
                "subjective_wb_selfrealization", 
                "subjective_wb_interpersonal", 
                "subjective_wb_managementcredibility", 
                "subjective_wb_team", 
                "subjective_wb_organizationsuccess", 
                "subjective_wb_worklifebalance", 
                "subjective_wb_values", 
                "subjective_wb_gotowork", 
                "subjective_wb_satisfaction", 
                "subjective_wb_commitment")
  
  
  new_var_list <- c("Management esteem", 
                    "Changeability", 
                    "Self-realization", 
                    "Interpersonal relationships", 
                    "Management credibility", 
                    "Teamwork", 
                    "Organization success", 
                    "Work-life balance", 
                    "Values", 
                    "Desire to go to work", 
                    "Job satisfaction", 
                    "Commitment to work")
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Get the selected category from the user input
  selected_category <- input$category_choice_subjwb
  
  # Calculate averages
  avg_data <- new_data %>%
    group_by(!!sym(selected_category)) %>%
    summarise(across(all_of(new_var_list), mean, na.rm = TRUE))  # Note the change here
  
  # Convert to long format
  avg_data_long <- avg_data %>% 
    pivot_longer(cols = all_of(new_var_list), names_to = "Variable", values_to = "Average")  # Note the change here
  
  # Create the plot
  plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
    layout(title = "Average subjective well-being by category", yaxis = list(title = FALSE),  xaxis = list(range = c(1, 5)))
})

####################### SUBJECTIVE MALAISE ##############################################

output$summaryTable_subjma <- renderFormattable({
  
  var_list <- c(
    "subjective_ma_absenteeism",
    "subjective_ma_resentment",
    "subjective_ma_proactivity",
    "subjective_ma_irrelevance",
    "subjective_ma_rulesadherence",
    "subjective_ma_organizationalconfusion",
    "subjective_ma_jobinterest",
    "subjective_ma_gossip",
    "subjective_ma_slowness",
    "subjective_ma_changejob",
    "subjective_ma_unusefulness",
    "subjective_ma_aggressive",
    "subjective_ma_unrecognized"
  )
  
  new_var_list <- c(
    "Absenteeism",
    "Feeling of resentment",
    "Lack of cognitive proactivity",
    "Feeling of irrelevance",
    "Rules adherence and emotional detachment",
    "Organizational confusion",
    "Lack of interest in job",
    "Gossiping",
    "Slowness in work",
    "Desire to change job",
    "Feeling of unusefulness",
    "Aggressiveness",
    "Feeling unrecognized"
  )
  
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_bes <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_bes[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_bes[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_bes[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_bes[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_bes[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_bes[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_bes[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})

# List of variables of interest and their new names
output$subjma_interactive_plot <- renderPlotly({
  
  var_list <- c("subjective_ma_absenteeism", 
                   "subjective_ma_changejob", 
                   "subjective_ma_irrelevance", 
                   "subjective_ma_unrecognized", 
                   "subjective_ma_unusefulness", 
                   "subjective_ma_rulesadherence", 
                   "subjective_ma_proactivity", 
                   "subjective_ma_jobinterest", 
                   "subjective_ma_organizationalconfusion", 
                   "subjective_ma_resentment", 
                   "subjective_ma_aggressive", 
                   "subjective_ma_slowness", 
                   "subjective_ma_gossip")
  
  
  
  new_var_list <- c("Absenteeism", 
                             "Desire to change job", 
                             "Feeling of irrelevance", 
                             "Feeling unrecognized", 
                             "Feeling of unusefulness", 
                             "Rules adherence and 
                              emotional detachment", 
                             "Lack of cognitive proactivity", 
                             "Lack of interest in job", 
                             "Organizational confusion", 
                             "Feeling of resentment", 
                             "Aggressiveness", 
                             "Slowness in work", 
                             "Gossiping")
  
  
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  
  # Filter data to keep only the columns of interest
  subjma_data <- new_data[, new_var_list]
  
  # Calculate averages for each column
  avg_values <- colMeans(subjma_data, na.rm = TRUE)
  
  # Convert to data frame for plotting
  plot_data <- data.frame(
    variable = names(avg_values),
    average = avg_values,
    size = scales::rescale(avg_values, to = c(10, 30))
  )
  
  # Create the plot
  plot <- plot_ly(data = plot_data, 
                  x = ~average, 
                  y = ~variable, 
                  type = "scatter", 
                  mode = "markers", 
                  marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
    layout(title = "Subjective malaise domains", xaxis = list(title = "Average level", range = c(1, 5)),
           yaxis = list(title = FALSE))
  
  plot
})

#### SUBJECTIVE MALAISE DENSITY PLOT

output$subjma_plot <- renderPlotly({
  selected_category <- input$subjma_category_choice
  
  # Create an empty plotly object
  plot <- plot_ly()
  
  # Generate density data for each group in the selected category
  for(group in unique(data_ready[[selected_category]])) {
    data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
    density_data <- density(data_sub$subjective_malaise, na.rm = TRUE)
    df_density <- data.frame(x = density_data$x, y = density_data$y)
    
    # Add the density curve for this group
    plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
    
    # Add hover text (optional)
    plot <- plot %>% add_trace(
      x = ~df_density$x,
      y = ~df_density$y,
      text = ~paste("Dimension: ", group,
                    "<br>Mean: ", round(mean(data_sub$subjective_malaise, na.rm = TRUE), 2),
                    "<br>Median: ", round(median(data_sub$subjective_malaise, na.rm = TRUE), 2),
                    "<br>SD: ", round(sd(data_sub$subjective_malaise, na.rm = TRUE), 2)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE,
      marker = list(size = 1, opacity = 0),
      data = df_density
    )
  }
  
  # Add titles and axis labels
  plot <- plot %>% layout(
    title = paste("Probability distribution by", selected_category),
    xaxis = list(title = "Subjective malaise"),
    yaxis = list(title = "Density")
  )
  
  plot
})

### SUBJECTIVE MALAISE BOX PLOTS
output$subjma_boxplot <- renderPlotly({
  selected_category <- input$subjma_category_choice
  
  # Generate the boxplot using plotly
  plot <- plot_ly(data = data_ready, x = ~subjective_malaise, type = "box", color = ~get(selected_category)) %>%
    layout(title = "Boxplot of subjective malaise by category",
           xaxis = list(title = "Subjective malaise", range=c(1.5, 5)),
           yaxis = list(title = selected_category))
  
  return(plot)
})

## SUBJECTIVE WELL BEING BARS

# Server
# Server code
output$interactive_subjma_plot <- renderPlotly({
  
  var_list <- c("subjective_ma_absenteeism", 
                "subjective_ma_changejob", 
                "subjective_ma_irrelevance", 
                "subjective_ma_unrecognized", 
                "subjective_ma_unusefulness", 
                "subjective_ma_rulesadherence", 
                "subjective_ma_proactivity", 
                "subjective_ma_jobinterest", 
                "subjective_ma_organizationalconfusion", 
                "subjective_ma_resentment", 
                "subjective_ma_aggressive", 
                "subjective_ma_slowness", 
                "subjective_ma_gossip")
  
  new_var_list <- c("Absenteeism", 
                    "Desire to change job", 
                    "Feeling of irrelevance", 
                    "Feeling unrecognized", 
                    "Feeling of unusefulness", 
                    "Rules adherence and 
                     emotional detachment", 
                    "Lack of cognitive proactivity", 
                    "Lack of interest in job", 
                    "Organizational confusion", 
                    "Feeling of resentment", 
                    "Aggressiveness", 
                    "Slowness in work", 
                    "Gossiping")
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Get the selected category from the user input
  selected_category <- input$category_choice_subjma
  
  # Calculate averages
  avg_data <- new_data %>%
    group_by(!!sym(selected_category)) %>%
    summarise(across(all_of(new_var_list), mean, na.rm = TRUE))  # Note the change here
  
  # Convert to long format
  avg_data_long <- avg_data %>% 
    pivot_longer(cols = all_of(new_var_list), names_to = "Variable", values_to = "Average")  # Note the change here
  
  # Create the plot
  plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
    layout(title = "Average subjective malaise by category", yaxis = list(title = FALSE),  xaxis = list(range = c(1, 5)))
})

####################### WORK-RELATED STRESS ##############################################

output$summaryTable_stress <- renderFormattable({

  # Vector containing the variable names
  var_list <- c(
    "stress_relations",
    "stress_management",
    "stress_colleagues",
    "stress_change",
    "stress_control",
    "stress_role",
    "stress_demand"
  )
  
  # Vector containing the definitions of each variable
  new_var_list <- c(
    "Relations",
    "Management",
    "Colleagues",
    "Change",
    "Control",
    "Role",
    "Demand"
  )
  
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data_bes <- data_ready %>% rename(!!!names_map)
  
  stats <- data.frame(
    #Measure = new_var_list,
    Mean = sapply(new_var_list, function(x) mean(new_data_bes[[x]], na.rm = TRUE)),
    Median = sapply(new_var_list, function(x) median(new_data_bes[[x]], na.rm = TRUE)),
    SD = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE)),
    Min = sapply(new_var_list, function(x) min(new_data_bes[[x]], na.rm = TRUE)),
    Max = sapply(new_var_list, function(x) max(new_data_bes[[x]], na.rm = TRUE)),
    SE = sapply(new_var_list, function(x) sd(new_data_bes[[x]], na.rm = TRUE) / sqrt(length(na.omit(new_data_bes[[x]])))),
    Kurtosis = sapply(new_var_list, function(x) kurtosis(new_data_bes[[x]], na.rm = TRUE)),
    Skew = sapply(new_var_list, function(x) skewness(new_data_bes[[x]], na.rm = TRUE))
  )
  
  formattable(stats, list(
    Mean = color_tile("white", "lightblue"),
    SD = color_tile("white", "lightgreen"),
    Median = color_tile("white", "lightpink"),
    Min = color_tile("white", "lightgray"),
    Max = color_tile("white", "lightyellow"),
    SE = color_tile("white", "lightcyan"),
    Kurtosis = color_tile("white", "lightcoral"),
    Skew = color_tile("white", "lightgoldenrodyellow")
  ))
  
})


# List of variables of interest and their new names
output$stress_interactive_plot <- renderPlotly({
  
  # Vector containing the variable names
  var_list <- c("stress_change", "stress_colleagues", "stress_control", "stress_demand", "stress_management", "stress_relations", "stress_role")
  
  # Vector containing the definitions of each variable
  new_var_list <- c(
    "Change", "Colleagues", "Control", "Demand", "Management", "Relations", "Role"
  )
  
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  
  # Filter data to keep only the columns of interest
  stress_data <- new_data[, new_var_list]
  
  # Calculate averages for each column
  avg_values <- colMeans(stress_data, na.rm = TRUE)
  
  # Convert to data frame for plotting
  plot_data <- data.frame(
    variable = names(avg_values),
    average = avg_values,
    size = scales::rescale(avg_values, to = c(10, 30))
  )
  
  # Create the plot
  plot <- plot_ly(data = plot_data, 
                  x = ~average, 
                  y = ~variable, 
                  type = "scatter", 
                  mode = "markers", 
                  marker = list(size = ~size, sizemode = "diameter", color=viridis_colors)) %>%
    layout(title = "Absence of work-related stress domains", xaxis = list(title = "Average level", range = c(1, 5)),
           yaxis = list(title = FALSE))
  
  plot
})

#### STRESS DENSITY PLOT

output$stress_plot <- renderPlotly({
  selected_category <- input$stress_category_choice
  
  # Create an empty plotly object
  plot <- plot_ly()
  
  # Generate density data for each group in the selected category
  for(group in unique(data_ready[[selected_category]])) {
    data_sub <- subset(data_ready, data_ready[[selected_category]] == group)
    density_data <- density(data_sub$stress, na.rm = TRUE)
    df_density <- data.frame(x = density_data$x, y = density_data$y)
    
    # Add the density curve for this group
    plot <- plot %>% add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = group, data = df_density)
    
    # Add hover text (optional)
    plot <- plot %>% add_trace(
      x = ~df_density$x,
      y = ~df_density$y,
      text = ~paste("Dimension: ", group,
                    "<br>Mean: ", round(mean(data_sub$stress, na.rm = TRUE), 2),
                    "<br>Median: ", round(median(data_sub$stress, na.rm = TRUE), 2),
                    "<br>SD: ", round(sd(data_sub$stress, na.rm = TRUE), 2)),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = FALSE,
      marker = list(size = 1, opacity = 0),
      data = df_density
    )
  }
  
  # Add titles and axis labels
  plot <- plot %>% layout(
    title = paste("Probability distribution by", selected_category),
    xaxis = list(title = "Absence of work-related stress"),
    yaxis = list(title = "Density")
  )
  
  plot
})

### STRESS BOX PLOTS
output$stress_boxplot <- renderPlotly({
  selected_category <- input$stress_category_choice
  
  # Generate the boxplot using plotly
  plot <- plot_ly(data = data_ready, x = ~stress, type = "box", color = ~get(selected_category)) %>%
    layout(title = "Boxplot of stress absence by category",
           xaxis = list(title = "Absence of stress", range=c(1, 5)),
           yaxis = list(title = selected_category))
  
  return(plot)
})

## STRESS BARS

# Server
# Server code
output$interactive_stress_plot <- renderPlotly({
  
  # Vector containing the variable names
  var_list <- c("stress_change", "stress_colleagues", "stress_control", "stress_demand", "stress_management", "stress_relations", "stress_role")
  
  # Vector containing the definitions of each variable
  new_var_list <- c(
    "Change", "Colleagues", "Control", "Demand", "Management", "Relations", "Role"
  )
  
  # Rename variables
  names_map <- setNames(var_list, new_var_list)
  new_data <- data_ready %>% rename(!!!names_map)
  
  viridis_colors <- viridis(22)
  
  # Get the selected category from the user input
  selected_category <- input$category_choice_stress
  
  # Calculate averages
  avg_data <- new_data %>%
    group_by(!!sym(selected_category)) %>%
    summarise(across(all_of(new_var_list), mean, na.rm = TRUE))  # Note the change here
  
  # Convert to long format
  avg_data_long <- avg_data %>% 
    pivot_longer(cols = all_of(new_var_list), names_to = "Variable", values_to = "Average")  # Note the change here
  
  # Create the plot
  plot_ly(data = avg_data_long, x = ~Average, y = ~Variable, color = ~get(selected_category), type = 'bar') %>%
    layout(title = "Average work-related stress absence by selected category", yaxis = list(title = FALSE),  xaxis = list(range = c(1, 5)))
})


################################  VIEW DATA SECTION ################################################
  
output$dataTable <- renderDT({
  # Identify the columns that start with 'Q' followed by a number from 1 to 127
  cols_to_remove <- grep("^Q[1-9]$|^Q[1-9][0-9]$|^Q1[0-1][0-9]$|^Q12[0-7]$", names(data_ready), value = TRUE)
  
  # Combine the 'BWL' and identified columns to create a vector of columns to remove
  cols_to_remove <- c("BWL", cols_to_remove)
  
  # Remove the identified columns from data_ready
  data_ready_subset <- data_ready[, !(names(data_ready) %in% cols_to_remove)]
  
  # Create the data table
  datatable(data_ready_subset, options = list(scrollX = TRUE))
})

      
################################ LEGEND SECTION #####################################################
output$sectors_table <- renderDT({
        # Data
        sectors_data <- data.frame(
          Code = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"),
          Description = c(
            "Agriculture, Forestry, and Fishing", 
            "Mining and Quarrying", 
            "Manufacturing", 
            "Electricity, Gas, Steam, and Air Conditioning Supply", 
            "Water Supply; Sewerage, Waste Management, and Remediation Activities", 
            "Construction", 
            "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles", 
            "Transportation and Storage", 
            "Accommodation and Food Service Activities", 
            "Information and Communication Services", 
            "Financial and Insurance Activities", 
            "Real Estate Activities", 
            "Professional, Scientific, and Technical Activities", 
            "Rental and Leasing Activities; Travel Agency, Support Services to Businesses", 
            "Public Administration and Defence; Compulsory Social Security", 
            "Education", 
            "Human Health and Social Work Activities", 
            "Arts, Entertainment, and Recreation", 
            "Other Service Activities", 
            "Activities of Households as Employers of Domestic Personnel; Undifferentiated Goods- and Services-Producing Activities of Households for Own Use", 
            "Activities of Extraterritorial Organizations and Bodies"
          )
        )
        
        # Render table
        datatable(sectors_data, options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE))
      })

output$health_table <- renderDT({
  health_data <- data.frame(
    Dimension = c(
      "Creates a healthy, comfortable, and welcoming work environment",
      "Sets explicit and clear objectives and is consistent between statements and operational practices",
      "Recognizes and values employees' skills and contributions, and stimulates new potentials",
      "Listens to employees' demands",
      "Communication and sharing of information",
      "Conflict management",
      "Fosters an open, communicative, and collaborative relational environment",
      "Ensures operational efficiency, quick decision-making, supports action towards goals",
      "Ensures equity in terms of compensation, assignment of responsibilities, and personnel promotion",
      "Stress level",
      "Instills a sense of social usefulness in employees",
      "Adopts all actions to prevent accidents and occupational risks",
      "Work management and sustainability of commitment",
      "Openness to external environment and technological and cultural innovation",
      "Organizational welfare"
    ),
    Description = c(
      "A healthy environment ensures fundamental hygiene rules; comfortable and welcoming refer to functional aspects - both in relation to work requirements and those of employees and/or clients - aesthetics, and care for the environment's appearance.",
      "This pertains to the strategic direction that must transparently formulate the pursued objectives, and the managerial style that must effectively communicate these objectives to employees through unambiguous means. Furthermore, the direction does not contradict in practice and daily operations what has been decided, regulated, and agreed upon.",
      "This encompasses acknowledging individual characteristics and diversity of contributions. This involves making appropriate requests from individuals based on their roles, competencies, qualifications, and what they do. It facilitates the expression of expertise in all its forms (technical-professional, cross-functional, and social), while acknowledging reciprocity in exchanges - providing compensation for what is received (financial, social visibility, etc.) and promoting the development of expertise (updating, knowledge sharing, circulation, etc.).",
      "The organization considers employees' requests and proposals as elements contributing to the enhancement of organizational processes. Therefore, they are taken into account in major decision-making processes (goal definition, work organization, rule setting, etc.). This includes negotiation processes within the organization, where both parties (management-employees) are acknowledged, interacting based on reciprocal rules. This aspect relates to involvement and participation processes.",
      "Everything done or occurring constitutes information to be made available and known to others, within the scope of work. Everyone is granted access to information, and there are clear tools and rules for disseminating information.",
      "Refers to potential conflict situations, both explicit and implied.",
      "This addresses the quality of communication and working style, both horizontally and vertically.",
      "Ensures operational fluidity in work life; problems are approached with the intention of overcoming them, avoiding unnecessary complications that slow down work, and fostering a sense of progress toward common objectives even from problematic situations.",
      "Clear criteria and paths for responsibilities, careers, rewards, etc. are defined, explicitly communicated, and made public. Equal opportunity for all to access them is ensured.",
      "Refers to the perceived level of physical and mental fatigue and stress.",
      "Recognizes that individual employees' activities are integral and necessary components of a more complex process aimed at achieving common results. The functional relationship between individual activities and corporate goals is consistently upheld over time.",
      "Organizations comply with legal safety obligations while also integrating safety and health protection as fundamental aspects of their identity and corporate culture.",
      "Addresses work content and workload.",
      "The organization displays flexibility, openness to change, adaptability, and views the external environment as a resource for improvement.",
      "Encompasses the totality of benefits and provisions a company extends to its employees with the aim of enhancing their personal and work lives."
    )
  )
  
  # Render table
  datatable(health_data, options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE))
})


output$wellbeing_table <- renderDT({
  wellbeing_data <- data.frame(
    Dimension = c(
      "Organization Satisfaction",
      "Desire to Contribute to the Organization",
      "Sense of Being Part of a Team",
      "Desire to Go to Work",
      "Sense of Self-Fulfillment",
      "Belief in the Ability to Change Current Negatives",
      "Balanced Work-Life Relationship",
      "Positive Interpersonal Relationships",
      "Shared Organizational Values",
      "Credibility of Management",
      "Respect for Management",
      "Perception of Organization's Success"
    ),
    Description = c(
      "Approval of being part of a valued organization.",
      "Willingness to work for the organization, even beyond the required effort.",
      "Perception of working together towards a common goal. Perception of emotional cohesion within the group.",
      "Daily pleasure in going to work.",
      "Feeling that working for the organization also satisfies personal needs.",
      "Confidence in the organization's capability to overcome existing negative aspects.",
      "Perception of a fair balance between work and leisure time.",
      "Satisfaction with interpersonal relationships built in the workplace.",
      "Agreement with the actions and values expressed by the organization.",
      "Trust in the management's leadership and professional abilities.",
      "Appreciation of the humane and moral qualities of the management.",
      "Viewing the organization as esteemed externally."
    )
  )
  
  # Render table
  datatable(wellbeing_data, options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE))
})


output$malaise_table <- renderDT({
  malaise_data <- data.frame(
    Dimension = c(
      "Resentment towards the Organization",
      "Unusual Aggressiveness and Nervousness",
      "Feeling of Uselessness",
      "Feeling of Irrelevance",
      "Feeling of Unacknowledgement",
      "Reluctance to Go to Work",
      "Lack of Interest in Work",
      "Desire to Change Jobs",
      "Gossip",
      "Formal Rule Adherence and Emotional Detachment",
      "Slowness in Performance",
      "Organizational Confusion in Terms of Roles, Tasks",
      "Diminished Cognitive Proactiveness",
      "Absenteeism"
    ),
    Description = c(
      "Feeling anger or bitterness towards the organization, sometimes accompanied by a desire for retaliation.",
      "Expressing aggression, even verbally, beyond one's usual behavior, which might also occur outside of work.",
      "Perceiving one's actions as futile and undervalued.",
      "Seeing oneself as insignificant and replaceable, not essential to the organization's functioning.",
      "Feeling one's capabilities and work are not adequately recognized.",
      "Daily difficulty in going to work.",
      "Low motivation that may manifest through non-compliance with rules, procedures, and reduced work quality.",
      "Desire linked to dissatisfaction with the work and/or professional context.",
      "When gossip reaches excessive levels, it's considered an indicator of distress, even becoming a substitute for work.",
      "While fulfilling tasks and complying with organizational rules, one lacks emotional involvement. Feeling of working mechanically.",
      "Time taken to complete work tasks prolongs, with or without self-awareness of the phenomenon.",
      "Lack of clarity about 'who does what' among organizational members and/or clients, sometimes causing discomfort and a desire for remedy.",
      "Absence of initiative-taking and desire for personal knowledge development.",
      "Prolonged and systematic absences from the workplace."
    )
  )
  
  # Render table
  datatable(malaise_data, options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE))
})
      
output$stress_table <- renderDT({
        stress_data <- data.frame(
          Dimension = c("Demand", "Control", "Management support", "Colleagues support", "Relationships", "Role", "Change"),
          Description = c ("Encompasses aspects such as workload, work organization, and the work context.", "Pertains to workers' autonomy/control over the ways their work activities are carried out.", 
                           "Includes encouragement, support, and resources provided by the company and superiors.", "Relates to encouragement, support, and resources offered by colleagues.",
                           "Involves promoting positive work interactions to prevent conflicts and address unacceptable behaviors.", "Ensures workers' awareness of their position within the organization and prevents conflicts.",
                           "Assesses the extent to which organizational changes, of any scale, are managed and communicated within the company's context."
          ))
        
        # Render table
        datatable(stress_data, options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE))
      })


############àààMULTI BOX PLOT

output$multi_boxplot_bes <- renderPlotly({
  
  data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")

  # Select only the columns that start with 'part_'
  data_ready_filtered <- data_ready %>% select(starts_with("bes_"), -bes_wellbeing)
  
  
  # Convert data to long format
  data_long <- data_ready_filtered %>% 
    gather(key = "Variable", value = "Value")
  
  # Identify outliers
  data_long <- data_long %>%
    group_by(Variable) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
    )
  
  new_var_list <- c("Politics", "Security", "Health", "Social Relationships",
                    "Work-Life Balance", "Natural & Cultural Heritage", "Quality of Services",
                    "Economic Wellbeing", "Innovation, Research & Creativity", "Education",
                    "Environment", "Subjective Wellbeing")
  
  data_long$Variable <- factor(data_long$Variable, levels = unique(data_long$Variable), labels = new_var_list)
  
  # Create the plot
  p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
               text = ~paste0("Variable: ", Variable,
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text",
               showlegend = FALSE  # Hide legend for box plot
  ) %>%
    add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
              marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
              name = "Outliers",  # Legend name
              showlegend = TRUE  # Show legend for scatter plot
    ) %>%
    layout(title = "Side-by-Side Boxplots and Points",
           yaxis = list(title = "Variables"),
           xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
  
  p
  
})
  
  output$multi_boxplot <- renderPlotly({
    
    data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
  
    
    # Select only the columns that start with 'part_'
    data_ready_filtered <- data_ready %>% select(starts_with("part_"))
    
    # Now proceed with the rest of your code
    
    # Convert data to long format
    data_long <- data_ready_filtered %>% 
      gather(key = "Variable", value = "Value")
    
    # Identify outliers
    data_long <- data_long %>%
      group_by(Variable) %>%
      mutate(
        median = round(median(Value), 2),
        mean = round(mean(Value), 2),
        Q1 = round(quantile(Value, 0.25), 2),
        Q3 = round(quantile(Value, 0.75), 2),
        IQR = round(Q3 - Q1, 2),
        Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
      )
    
    p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
                 text = ~paste0("Variable: ", Variable,
                                "<br>Mean: ", mean,
                                "<br>Median: ", median,
                                "<br>Q1: ", Q1,
                                "<br>Q3: ", Q3),
                 hoverinfo = "text",
                 showlegend = FALSE  # Hide legend for box plot
    ) %>%
      add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
                marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
                name = "Outliers",  # Legend name
                showlegend = TRUE  # Show legend for scatter plot
      ) %>%
      layout(title = "Side-by-Side Boxplots and Points",
             yaxis = list(title = "Variables"),
             xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
    
    p
      })
    

output$multi_boxplot_health <- renderPlotly({
  
  data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
  
  
  # Select only the columns that start with 'health_'
  data_ready_filtered <- data_ready %>% select(starts_with("health_"), -health_organization)
  
  
  # Convert data to long format
  data_long <- data_ready_filtered %>% 
    gather(key = "Variable", value = "Value")
  
  # Identify outliers
  data_long <- data_long %>%
    group_by(Variable) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
    )
  
  new_var_list <- c("Accidents prevention", "Healthy work environment", "Active listening", "Equity",
                    "Administrative efficiency", "Job sustainability", "Openness to innovation",
                    "Conflicts management", "Stress levels control", "Sharing of information",
                    "Clear objectives", "Skilss enhancement", "Sense of usefulness", "Collaborative relational environment", "Organizational welfare")
  
  data_long$Variable <- factor(data_long$Variable, levels = unique(data_long$Variable), labels = new_var_list)
  
  p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
               text = ~paste0("Variable: ", Variable,
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text",
               showlegend = FALSE  # Hide legend for box plot
  ) %>%
    add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
              marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
              name = "Outliers",  # Legend name
              showlegend = TRUE  # Show legend for scatter plot
    ) %>%
    layout(title = "Side-by-Side Boxplots and Points",
           yaxis = list(title = "Variables"),
           xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
  
  p
})



###############

output$multi_boxplot_subjwb <- renderPlotly({
  
  data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
  
  
  # Select only the columns that start with 'part_'
  data_ready_filtered <- data_ready %>% select(starts_with("subjective_wb_"))
  
  # Now proceed with the rest of your code
  
  # Convert data to long format
  data_long <- data_ready_filtered %>% 
    gather(key = "Variable", value = "Value")
  
  # Identify outliers
  data_long <- data_long %>%
    group_by(Variable) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
    )
  
  new_var_list <- c("Management esteem", 
                    "Changeability", 
                    "Self-realization", 
                    "Interpersonal relationships", 
                    "Management credibility", 
                    "Teamwork", 
                    "Organization success", 
                    "Work-life balance", 
                    "Values", 
                    "Desire to go to work", 
                    "Job satisfaction", 
                    "Commitment to work")
  
  data_long$Variable <- factor(data_long$Variable, levels = unique(data_long$Variable), labels = new_var_list)
  
  
  p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
               text = ~paste0("Variable: ", Variable,
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text",
               showlegend = FALSE  # Hide legend for box plot
  ) %>%
    add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
              marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
              name = "Outliers",  # Legend name
              showlegend = TRUE  # Show legend for scatter plot
    ) %>%
    layout(title = "Side-by-Side Boxplots and Points",
           yaxis = list(title = "Variables"),
           xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
  
  p
})

output$multi_boxplot_subjma <- renderPlotly({
  
  data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
  
  
  # Select only the columns that start with 'part_'
  data_ready_filtered <- data_ready %>% select(starts_with("subjective_ma_"))
  
  # Now proceed with the rest of your code
  
  # Convert data to long format
  data_long <- data_ready_filtered %>% 
    gather(key = "Variable", value = "Value")
  
  # Identify outliers
  data_long <- data_long %>%
    group_by(Variable) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
    )
  
  new_var_list <- c("Absenteeism", 
                    "Desire to change job", 
                    "Feeling of irrelevance", 
                    "Feeling unrecognized", 
                    "Feeling of unusefulness", 
                    "Rules adherence and 
                     emotional detachment", 
                    "Lack of cognitive proactivity", 
                    "Lack of interest in job", 
                    "Organizational confusion", 
                    "Feeling of resentment", 
                    "Aggressiveness", 
                    "Slowness in work", 
                    "Gossiping")
  
  data_long$Variable <- factor(data_long$Variable, levels = unique(data_long$Variable), labels = new_var_list)
  
  p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
               text = ~paste0("Variable: ", Variable,
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text",
               showlegend = FALSE  # Hide legend for box plot
  ) %>%
    add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
              marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
              name = "Outliers",  # Legend name
              showlegend = TRUE  # Show legend for scatter plot
    ) %>%
    layout(title = "Side-by-Side Boxplots and Points",
           yaxis = list(title = "Variables"),
           xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
  
  p
})

output$multi_boxplot_stress <- renderPlotly({
  
  data_ready <- read.csv("C:/Users/magna/OneDrive/Tesi Magistrale DSE/Analisi Script R/data_ready.csv")
  
  
  # Select only the columns that start with 'part_'
  data_ready_filtered <- data_ready %>% select(starts_with("stress_"))
  
  # Now proceed with the rest of your code
  
  # Convert data to long format
  data_long <- data_ready_filtered %>% 
    gather(key = "Variable", value = "Value")
  
  # Identify outliers
  data_long <- data_long %>%
    group_by(Variable) %>%
    mutate(
      median = round(median(Value), 2),
      mean = round(mean(Value), 2),
      Q1 = round(quantile(Value, 0.25), 2),
      Q3 = round(quantile(Value, 0.75), 2),
      IQR = round(Q3 - Q1, 2),
      Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
    )
  
  
  new_var_list <- c(
    "Change", "Colleagues", "Control", "Demand", "Management", "Relations", "Role"
  )
  
  data_long$Variable <- factor(data_long$Variable, levels = unique(data_long$Variable), labels = new_var_list)
  
  
  p <- plot_ly(data = data_long, y = ~Variable, x = ~Value, type = "box",
               text = ~paste0("Variable: ", Variable,
                              "<br>Mean: ", mean,
                              "<br>Median: ", median,
                              "<br>Q1: ", Q1,
                              "<br>Q3: ", Q3),
               hoverinfo = "text",
               showlegend = FALSE  # Hide legend for box plot
  ) %>%
    add_trace(type = "scatter", mode = "markers", y = ~Variable, x = ~Value,
              marker = list(color = ~ifelse(Outlier, 'red', 'lightblue'), opacity = 0.7),
              name = "Outliers",  # Legend name
              showlegend = TRUE  # Show legend for scatter plot
    ) %>%
    layout(title = "Side-by-Side Boxplots and Points",
           yaxis = list(title = "Variables"),
           xaxis = list(title = "Values", range = c(0.5, 5)))  # Set x-axis range from 0.5 to 5
  
  p
})


###############CORRELATION HEATMAPLY
output$heatmap_part <- renderPlotly({
  
  var_list <- c("part_welfare", "part_development", "part_hr", "part_joborg", 
                "part_remuneration", "part_intcomm", "part_extcomm", "part_training", 
                "part_health", "part_econstatus", "part_suppliers", "part_innovation",
                "part_investments", "part_occupation", "part_general", "part_area", 
                "part_shareholders", "part_intconflicts", "part_extconflicts", 
                "part_stakeholder", "part_environment")
  
  # New names you want to give to these variables
  new_var_list <- c("Organization welfare", "Organization development", "H&R", "Work organization",
                    "Remuneration", "Internal communication", "External communication",
                    "Skilss development", "Health protection", "Economic-financial status",
                    "Suppliers selection", "Product-process innovation", "Investments", "Employment", "General planning", "Area planning", "Employee share ownership", "Internal conflicts", "External conflicts", "Stakeholders involvement", "Environmental sustainability")
  
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})

output$heatmap_bes <- renderPlotly({
  
  var_list <- c("bes_politics", "bes_security", "bes_health", "bes_socialrelationships",
                "bes_worklifebalance", "bes_naturalculturalheritage","bes_qualityofservices",
                "bes_economicwellbeing","bes_innovationresearchcreativity","bes_education",
                "bes_environment","bes_subjectivewellbeing")
  
  new_var_list <- c("Politics", "Security", "Health", "Social relationships",
                    "Work-Life balance", "Nat-Cult Heritage", "Services",
                    "Econ. Wellbeing", "I, R & Creativity", "Education",
                    "Environment", "Subj. wellbeing")
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})

output$heatmap_health <- renderPlotly({
  
  # List of variables of interest
  var_list <- c("health_prevention", "health_workenvironment", "health_activelistening",
                "health_equity", "health_administrativeefficiency", "health_jobsustainability",
                "health_openinnovation", "health_conflicts", "health_stresslevels",
                "health_information", "health_clearobjectives", "health_skillsenhancement",
                "health_meaningfulness", "health_relationalenvironment", "health_organizationalwelfare")
  
  
  # New names you want to give to these variables
  new_var_list <- c("Accidents prevention", "Healthy environment", "Active listening", "Equity",
                    "Adm. efficiency", "Job sustainability", "Innovation",
                    "Conflicts management", "Stress control", "Sharing of information",
                    "Clear objectives", "Skilss enhancement", "Usefulness", "Collab. environment", "Organiz. welfare")
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})

output$heatmap_subjwb <- renderPlotly({
  
  var_list <- c("subjective_wb_managementesteem", 
                "subjective_wb_changeability", 
                "subjective_wb_selfrealization", 
                "subjective_wb_interpersonal", 
                "subjective_wb_managementcredibility", 
                "subjective_wb_team", 
                "subjective_wb_organizationsuccess", 
                "subjective_wb_worklifebalance", 
                "subjective_wb_values", 
                "subjective_wb_gotowork", 
                "subjective_wb_satisfaction", 
                "subjective_wb_commitment")
  
  
  new_var_list <- c("Management esteem", 
                    "Changeability", 
                    "Self-realization", 
                    "Interpersonal relationships", 
                    "Management credibility", 
                    "Teamwork", 
                    "Organization success", 
                    "Work-life balance", 
                    "Values", 
                    "Desire to go to work", 
                    "Job satisfaction", 
                    "Commitment to work")
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})

output$heatmap_subjma <- renderPlotly({
  
  var_list <- c("subjective_ma_absenteeism", 
                "subjective_ma_changejob", 
                "subjective_ma_irrelevance", 
                "subjective_ma_unrecognized", 
                "subjective_ma_unusefulness", 
                "subjective_ma_rulesadherence", 
                "subjective_ma_proactivity", 
                "subjective_ma_jobinterest", 
                "subjective_ma_organizationalconfusion", 
                "subjective_ma_resentment", 
                "subjective_ma_aggressive", 
                "subjective_ma_slowness", 
                "subjective_ma_gossip")
  
  new_var_list <- c("Absenteeism", 
                    "Change job", 
                    "Irrelevance", 
                    "Unrecognized", 
                    "Unusefulness", 
                    "Rules adherence", 
                    "Lack of proactivity", 
                    "Lack of interest", 
                    "Organizational confusion", 
                    "Resentment", 
                    "Aggressiveness", 
                    "Slowness", 
                    "Gossiping")
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})

output$heatmap_stress <- renderPlotly({
  
  # Vector containing the variable names
  var_list <- c("stress_change", "stress_colleagues", "stress_control", "stress_demand", "stress_management", "stress_relations", "stress_role")
  
  # Vector containing the definitions of each variable
  new_var_list <- c(
    "Change", "Colleagues", "Control", "Demand", "Management", "Relations", "Role"
  )
  
  # Filter the data based on the original variable names
  data_filtered <- data_ready[, var_list]
  
  # Rename the columns to the new variable names
  colnames(data_filtered) <- new_var_list
  
  # Compute the correlation matrix
  corr_matrix <- cor(data_filtered, use = "pairwise.complete.obs")
  
  # Generate the heatmap
  heatmaply_cor(
    corr_matrix,
    k_col = 2, 
    k_row = 2
  )
})


###OUTLIERS

output$mahalanobis_plot <- renderPlotly({
  
  # Subset the relevant columns (assuming they are named Q1, Q2, ..., Q127)
  relevant_columns_bes <- grep("^Q[1-9][0-9]*$", names(data_ready), value = TRUE)
  
  
  # Subset the data
  data_subset_bes <- data_ready[, relevant_columns_bes]
  
  mahalanobis_values_bes <- mahalanobis(data_subset_bes, colMeans(data_subset_bes, na.rm = TRUE), solve(var(data_subset_bes, na.rm = TRUE)))
  
  
  # Create a data frame for plotting
  plot_data_bes <- data.frame(Index = 1:length(mahalanobis_values_bes), Mahalanobis = mahalanobis_values_bes)
  
  # Calculate a threshold (e.g., a percentile or a statistical cutoff)
  threshold_bes <- quantile(mahalanobis_values_bes, 0.99)  # 99th percentile
  
  # Calculate outliers
  is_outlier_bes <- plot_data_bes$Mahalanobis > threshold_bes
  
  # Count outliers and non-outliers
  n_outliers_bes <- sum(is_outlier_bes)
  n_non_outliers_bes <- nrow(plot_data_bes) - n_outliers_bes
  
  # Add a 'status' column to the data
  plot_data_bes$status <- ifelse(is_outlier_bes, "Outlier", "Non-Outlier")
  
  # Create the Plotly plot
  p <- plot_ly(data = plot_data_bes, x = ~Index, y = ~Mahalanobis, type = 'scatter', mode = 'markers',
               text = ~paste("Index:", Index, "<br>Mahalanobis Distance:", Mahalanobis),
               marker = list(color = ~ifelse(status == "Outlier", 'red', 'black'))) %>%
    add_lines(y = ~rep(threshold_bes, length(Index)), line = list(color = "red", dash = "dash"), showlegend = FALSE) %>%
    layout(
      title = "Mahalanobis Distance for Well-being Items Multivariate Outliers",
      xaxis = list(title = "Index"),
      yaxis = list(title = "Mahalanobis Distance"),
      legend = list(x = 1, xanchor = "left", y = 1),  # Position legend on the right
      annotations = list(
        x = Inf, y = Inf,
        text = paste("Outliers:", n_outliers_bes, "<br>Non-Outliers:", n_non_outliers_bes),
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        showarrow = FALSE
      )
    )
  
})


output$mahalanobis_plot_part <- renderPlotly({
  
  # Select variables that start with Q or part_
  relevant_columns_part <- grep("^part_", names(data_ready), value = TRUE)

  data_subset_part <- data_ready[, relevant_columns_part]

  
  mahalanobis_values_part <- mahalanobis(data_subset_part, colMeans(data_subset_part, na.rm = TRUE), solve(var(data_subset_part, na.rm = TRUE)))
  
  
  # Create a data frame for plotting
  plot_data_part <- data.frame(Index = 1:length(mahalanobis_values_part), Mahalanobis = mahalanobis_values_part)
  
  # Calculate a threshold (e.g., a percentile or a statistical cutoff)
  threshold_part <- quantile(mahalanobis_values_part, 0.99)  # 99th percentile
  
  # Calculate outliers
  is_outlier_part <- plot_data_part$Mahalanobis > threshold_part
  
  # Count outliers and non-outliers
  n_outliers_part <- sum(is_outlier_part)
  n_non_outliers_part <- nrow(plot_data_part) - n_outliers_part
  
  # Add a 'status' column to the data
  plot_data_part$status <- ifelse(is_outlier_part, "Outlier", "Non-Outlier")
  
  # Create the Plotly plot
  p <- plot_ly(data = plot_data_part, x = ~Index, y = ~Mahalanobis, type = 'scatter', mode = 'markers',
               text = ~paste("Index:", Index, "<br>Mahalanobis Distance:", Mahalanobis),
               marker = list(color = ~ifelse(status == "Outlier", 'red', 'black'))) %>%
    add_lines(y = ~rep(threshold_part, length(Index)), line = list(color = "red", dash = "dash"), showlegend = FALSE) %>%
    layout(
      title = "Mahalanobis Distance for Well-being Items Multivariate Outliers",
      xaxis = list(title = "Index"),
      yaxis = list(title = "Mahalanobis Distance"),
      legend = list(x = 1, xanchor = "left", y = 1),  # Position legend on the right
      annotations = list(
        x = Inf, y = Inf,
        text = paste("Outliers:", n_outliers_part, "<br>Non-Outliers:", n_non_outliers_part),
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        showarrow = FALSE
      )
    )
  
})

########### TABLE FOR TESTS

# Render the table
# The ID "table" here links to tableOutput("table") in the UI
output$table_test_participation <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names


output$table_test_bes <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names


# The ID "table" here links to tableOutput("table") in the UI
output$table_test_subjwb <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names

# The ID "table" here links to tableOutput("table") in the UI
output$table_test_health <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names

# The ID "table" here links to tableOutput("table") in the UI
output$table_test_subjma <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names


# The ID "table" here links to tableOutput("table") in the UI
output$table_test_stress <- renderTable({
  
  # Create a 5x3 matrix filled with NA values
  my_table <- matrix(NA, nrow = 6, ncol = 3)
  
  # Add row names for the categorical variables (no need for an empty first cell)
  rownames(my_table) <- c("   ", "Dimension", "Role", "Gender", "Age", "Seniority")
  
  # Add column names for the criteria we checked
  colnames(my_table) <- c("Normality", "Homogeneity of Variance", "Test")
  
  # Populate the table with 'Yes'/'No' for Normality and Homogeneity, and the name of the test to use
  my_table[1,] <- c("Shapiro-Wilk", "Levene's test", "   ")
  my_table[2,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[3,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[4,] <- c("No", "Yes", "Mann-Whitney U")
  my_table[5,] <- c("No", "Yes", "Kruskal-Wallis")
  my_table[6,] <- c("No", "Yes", "Kruskal-Wallis")
  
  as.data.frame(my_table)
}, rownames = TRUE)  # Display row names

function(input, output) {
  output$boxplot_prova <- renderPlotly({
    p <- plot_ly(data_ready, 
                 x = ~get(input$x_var), 
                 y = ~bes_wellbeing, 
                 color = ~get(if (input$fill_var == "None") input$x_var else input$fill_var), 
                 type = "box") %>%
      layout(title = 'Boxplot of Workers Well-being', 
             yaxis = list(title = 'Workers Well-being'), 
             xaxis = list(title = input$x_var))
    
    p
  })
}


}

# Run the app
shinyApp(ui = ui, server = server)


