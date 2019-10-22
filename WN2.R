#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(rworldmap)
library(rgeos)

# Water_Nexus <- read_csv('WN2.csv')
# Water_Nexus <- Water_Nexus[1:139,c(2:7, 9:256)]
# write.csv(Water_Nexus, 'WN2_v3.csv', row.names = FALSE)
Water_Nexus <- read_csv('WN2_v4.csv', locale = readr::locale(encoding = "latin1"))
first_country <- which(colnames(Water_Nexus) == 'Benin')
last_country <- which(colnames(Water_Nexus) == 'Canada')
first_sector <- which(colnames(Water_Nexus) == 'Water and agriculture')
last_sector <- which(colnames(Water_Nexus) == 'Finance')

#### ui ####

ui <- dashboardPage(skin = "green",
    # Dashboard header ####
    dashboardHeader(title="Water Nexus dashboard"),
    # Dashboard sidebar #### 
    dashboardSidebar(
        sidebarMenu(id="tabs",
                    menuItem("About", 
                             tabName = "about",
                             icon = icon("info")),
                    menuItem("Country", 
                             tabName = "info",
                             icon = icon("flag")
                             ),
                    menuItem("Actor", 
                             tabName = "activity",
                             icon = icon("landmark")),
                    menuItem("Sector", 
                             tabName = "contact",
                             icon = icon("folder-open")),
                    selectInput(inputId = "nation", label = "Select a country", 
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_country:last_country])),
                    selectInput(inputId = "research", label = "Select a sector",
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_sector:last_sector]))
        )
    ),
    # Dashboard body #### 
    dashboardBody(
        tabItems(
            # About tab content ####
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12, 
                            h2("About the dashboard"),
                            hr(),
                            h3("Belgian Actors in the Water Sector"),
                            br(),
                            h5(
                                "This interactive Belgian Actors dashboard aims to inventory the Belgian actors active in the water sector, their field of activities and the countries where they are active. The actors are multidisciplinary, including public sector,  NGO's, civil society, private sector, teaching/research institutes, and PPPs/ Networks."),
                            br(),
                            h5("If you want your organisation to be added to this inventory, please  provide your information using this ",
                               a("link",
                                 href = "https://www.surveymonkey.com/r/WBG6DN6"),
                               "English version) or this ",
                               a("link",
                                 href = "https://fr.surveymonkey.com/r/WH9T3CM"),
                               "(French version)."),
                            br(),
                            h5("If you want to modify the information about your organisation, please send us an email to: ",
                               a("waternexusbelgium@gmail.com",
                                 href = "mailto: waternexusbelgium@gmail.com"))
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("About the dataset"),
                            hr(),
                            h3("Dataset of Directorate-General for Development Cooperation and Humanitarian Aid"),
                            br(),
                            h4("The dataset of Directorate-General for Development Cooperation and Humanitarian Aid (DGD) contains 12550 projects in total from 1987 to 2018. The dataset mainly focuses on projects in the water sector. As such, projects related to including environment, agriculture, fisheries, forestry, and hydroelectricity are also included. The dataset includes 191 attributes which are characteristics of any projects that have cooperation with DGD. The attributes cover from basic information of the projects, e.g. title, year, period, etc., to specific properties of the projects, i.e. scale of their involvement with respect to Sustainable Development Goals (SDGs), target groups, reached results, etc."),
                            br(),
                            h4("Besides this dataset, a broader database that includes projects funded by other funding organizations, such as VLIR-UOS, ARES, VPWvO, Enabel, etc., is being developed. If you want to add the information about the projects funded/implemented by your organisation, please send us an email to: ",
                               a("waternexusbelgium@gmail.com",
                                 href = "mailto: waternexusbelgium@gmail.com"))
                        )
                    ),
                    fluidRow(
                        column(6,
                               h1("Funded by"),
                               img(style = "max-width:50%",
                                   src = "Logo2.jpg")
                        ),
                        column(6, 
                               img(align = "left|bottom",
                                   style = "max-width:50%",
                                   src = "Logo.jpg") 
                        )
                    )
            ), # end of About tabItem
            # Info tab content ####
            tabItem(tabName = "info",
                    fluidRow(
                        valueBoxOutput("actor"),
                        valueBoxOutput("country"),
                        valueBoxOutput("sector")
                    ),
                    fluidRow(
                        box(title = "Actor info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "Project map", width = 12,
                            leafletOutput("map", width = "100%", height = 400) # Can be changed
                        )
                    )
            ), # end of Info tab content
            # Activity tab content ####
            tabItem(tabName = "activity",
                    fluidRow(
                        valueBoxOutput("actor1"),
                        valueBoxOutput("country1"),
                        valueBoxOutput("sector1")
                    ),
                    fluidRow(
                        box(title = "Active country", width = 12,
                            plotlyOutput("activecountry")
                        )
                    ),
                    fluidRow(
                        box(title = "Active sector", width = 12,
                            plotlyOutput("activesector")
                        )
                    )
            ), # end of country tab content
            # Contact tab content ####
            tabItem(tabName = "contact"

            )
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({Water_Nexus})

    nationname <- reactive({
        nationname <- vector(mode = 'character', length = 0)
        for (i in first_country:last_country){
            if(sum(!is.na(df()[,i] > 0))){
                nationname <- c(nationname, colnames(df()[,i]))
            }
        }
        nationname
    })

    observe({
        updateSelectInput(session, inputId = "nation", label = "Select a country", choices = c("All", nationname()))
    })

    df_country <- reactive({
        input$nation
    })

    sectorname <- reactive({
        if(df_country()!= "All"){
            a <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            sectorname <- vector(mode = "character", length = 0)
            for (i in first_sector:last_sector){
                if(sum(!is.na(a[,i]))>0){
                    sectorname <- c(sectorname, colnames(a[,i]))
                }
            }
            sectorname
        } else {
            colnames(df())[first_sector:last_sector]
        }
    })

    observe({
        updateSelectInput(session, inputId = "research", label = "Select a sector", choices = c("All", sectorname()))
    })

    df_research <- reactive({
        input$research
    })

    # Output valuebox ####
    output$actor <- renderValueBox({

        if (df_country() == "All"){
            if (df_research() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research()]),]
            }
        } else {
            if (df_research() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research()]),]
            }
        }

        valueBox(
            value = nrow(selectedData),
            subtitle = "Total actor",
            icon = icon("landmark"),
            color = "purple"
        )
    })
    output$country <- renderValueBox({

        if (df_country() == "All"){
            if (df_research() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research()]),]
            }
        } else {
            if (df_research() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research()]),]
            }
        }

        selectedData_v2 <- selectedData[,first_country:last_country][, colSums(is.na(selectedData[,first_country:last_country])) < nrow(selectedData[,first_country:last_country])]

        valueBox(
            value = ncol(selectedData_v2),
            subtitle = "Total country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$sector <- renderValueBox({ 

        if (df_country() == "All"){
            if (df_research() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research()]),]
            }
        } else {
            if (df_research() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research()]),]
            }
        }

        selectedData_v2 <- selectedData[,first_sector:last_sector][, colSums(is.na(selectedData[,first_sector:last_sector]))< nrow(selectedData[,first_sector:last_sector])]

        valueBox(
            value = ncol(selectedData_v2),
            subtitle = "Total sector",
            icon = icon("folder-open"),
            color = "purple"
        )
    })

    # Output table ####
    output$table <- DT::renderDataTable(server = FALSE, {
            selectedData <- df()
            if (df_country() == "All"){
                if (df_research() == "All") {
                    selectedData <- df()
                } else {
                    selectedData <- df()[!is.na(df()[, colnames(df()) == df_research()]),]
                }
            } else {
                if (df_research() == "All") {
                    selectedData <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                } else {
                    selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
                    selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research()]),]
                }
            }

            selectedData 

            DT::datatable(selectedData,
                          filter="top",
                          selection="multiple",
                          escape=FALSE,
                          extensions = 'Buttons',
                          options = list(sDom  = '<"top"pB>t<"bottom"i>r', pageLength = 5,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

        })

}

#### Run the application ####
shinyApp(ui = ui, server = server)
