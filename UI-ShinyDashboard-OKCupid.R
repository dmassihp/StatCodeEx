## ui.R ##
library(shiny)
library(shinydashboard)

shinyUI(bootstrapPage(
  
  dashboardPage(skin = "purple",
    dashboardHeader(title = "36-315 Final Project"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Religion", tabName = "PartA"),
        menuItem("Age And Kids", tabName = "PartB"),
        menuItem("Smoking And Drinking", tabName = "SmokingAndDrinking"),
        menuItem("Height And Weight", tabName = "HeightAndWeight"),
        menuItem("Body Type", tabName = "BodyType"),
        menuItem("Word Cloud", tabName = "WordCloud"),
        menuItem("Gender", tabName = "Part7"),
        menuItem("Pets", tabName = "Part8")
      )
    ),
    dashboardBody(
      tabItems(
        ##Jun-------------------------------------------------------------------------------------
        # First tab content
        tabItem(tabName = "PartA",
                fluidRow(
                  
                  box(plotOutput(outputId = "religion_plot"))
                  
                )
        ),
        
        # Second tab content
        tabItem(tabName = "PartB",
                fluidRow(
                  
                  box(plotOutput(outputId = "age")),
                  
                  box(radioButtons(inputId = "age_input", 
                                   label = strong("Age Distribution for Vegetarians"),
                                   choices = list("Default" = 1, 
                                                  "Compare based on Offspring" = 2, 
                                                  "Compare with Other Diets" = 3), 
                                   selected = 1))
                  
                )
        ),
        ##Jim--------------------------------------------------------------------------------------
        # 3rd tab content
        tabItem(tabName = "SmokingAndDrinking",
                fluidRow(
                  box(plotOutput(outputId = "smoke_drink_plot_1", height = 600)),
                  box(plotOutput(outputId = "smoke_drink_plot_2", height = 600))
                ),
                fluidRow(
                  box(checkboxInput(inputId = "sqrt_scale",
                                    label = strong("Apply Sqrt Compression to the Scale"),
                                    value = FALSE))
                ),
                fluidRow(
                  box(plotOutput(outputId = "smoke_drink_plot_3", height = 600))
                )
        ),
        
        # 4th Tab
        tabItem(tabName = "HeightAndWeight",
                fluidRow(
                  box(plotOutput(outputId = "height_weight_plot_1", height = 600), width=5),
                  box(plotOutput(outputId = "height_weight_plot_2", height = 600), width=7)
                ),
                fluidRow(
                  box(checkboxInput(inputId = "show_stats",
                                    label = strong("Show Summary Statistics"),
                                    value = FALSE), width = 5),
                  box(checkboxInput(inputId = "prop_veg",
                                    label = strong("By Proportion of Vegetarian"),
                                    value = FALSE), width = 7)
                )
                  
        ),
        ##Jennifer---------------------------------------------------------------------------------
        #5th
        tabItem(tabName = "BodyType",
                fluidRow(
                  box(plotOutput(outputId = "body_type_plot")),
                  
                  box(selectInput(inputId = "diet_type",
                                  label = "Diets",
                                  choices = c("All Diets", "Diets Generalized", 
                                              "Vegetarian Diets and Other"),
                                  selected = "All Diets"))
                  
                )
        ),
        
        #6th
        tabItem(tabName = "WordCloud",
                tabsetPanel(
                  tabPanel("Essay 4: Favorite Books, Movies, Show, Music, and Food", 
                           box(plotOutput(outputId = "word_cloud_4", 
                                          height = "400px")),
                           
                           box(selectInput(inputId = "gender",
                                           label = "Gender",
                                           choices = c("Males","Females"),
                                           selected = "Males"))),
                  tabPanel("Essay 5: The Six Things I Could Never Do Without", 
                           box(plotOutput(outputId = "word_cloud_5", 
                                          height = "400px")),
                           
                           box(selectInput(inputId = "gend",
                                           label = "Gender",
                                           choices = c("Males","Females"),
                                           selected = "Males")))
                )
        ),
        ##Dorsa------------------------------------------------------------------------------------
        #7th
        tabItem(tabName = "Part7",
                fluidRow(
                  box(width=9,
                    plotOutput(outputId = "bar_ageorsex", height = "300px", width = "900px")
                  ),
                  box(width=3,
                    radioButtons(inputId = "sex",
                                 label = "Basic Characteristics: Distribution by Sex or  Age",
                                 choices = c("Sex", "Age"),
                                 selected = "Sex")
                  )
                )
        ),
        
        #8th
        tabItem(tabName = "Part8",
                fluidRow(
                  box(width=8,
                    plotOutput(outputId = "pets_plot", height = "400px", width = "700px")
                  ),
                  box(width=4,
                    radioButtons(inputId = "pets",
                                 label = "Pet Preferences for Vegetarians or Non-vegetarians",
                                 choices = c("Vegetarians", "Non-vegetarians"),
                                 selected = "Vegetarians")
                  )
                )
        )
        
      )
    )
  )
))
