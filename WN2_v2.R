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
library(usethis)

Water_Nexus <- read_csv('WN2_v8.csv', locale = readr::locale(encoding = "latin1"))
first_country <- which(colnames(Water_Nexus) == 'Benin')
last_country <- which(colnames(Water_Nexus) == 'Switzerland')
first_lat <- which(colnames(Water_Nexus) == 'lat_Benin')
last_lat <- which(colnames(Water_Nexus) == 'lat_Switzerland')
first_long <- which(colnames(Water_Nexus) == 'long_Benin')
last_long <- which(colnames(Water_Nexus) == 'long_Switzerland')



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
                    conditionalPanel(condition = "input.tabs == 'info'",
                                     selectInput(inputId = "actors_1", label = "Select an actor", 
                                                 choices = c(All = "All", sort(Water_Nexus$`Name 1`))),
                                     selectInput(inputId = "research_1", label = "Select a sector",
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_sector:last_sector]))
                                     ),
                    conditionalPanel(condition = "input.tabs == 'activity'",
                                     selectInput(inputId = "nation_1", label = "Select a country", 
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_country:last_country])),
                                     selectInput(inputId = "research_2", label = "Select a sector",
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_sector:last_sector]))
                                     ),
                    conditionalPanel(condition = "input.tabs == 'contact'",
                                     selectInput(inputId = "nation_2", label = "Select a country", 
                                                 choices = c(All = "All", colnames(Water_Nexus)[first_country:last_country])),
                                     selectInput(inputId = "actors_2", label = "Select an actor", 
                                                 choices = c(All = "All", Water_Nexus$`Name 1`))
                                     )
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
                            h4(
                                "This interactive Belgian Actors dashboard aims to inventory the Belgian actors active in the water sector, their field of activities and the countries where they are active. The actors are multidisciplinary, including public sector,  NGO's, civil society, private sector, teaching/research institutes, and PPPs/ Networks.")
                        )
                    ),
                    fluidRow(
                        box(width = 12, 
                            h2("About the dataset"),
                            hr(),
                            h3("Dataset of Directorate-General for Development Cooperation and Humanitarian Aid"),
                            br(),
                            h4("Dataset covers 140 organisations from the public, private sector, NGOs and NPOs, that are working in water sector in Belgium. They have been interviewed to provide precise information about the activities of their organizations. In the end of the project, 170 organizations in total are expected to be interviewed."),
                            br(),
                            h4("However, if you want your organisation to be added to this inventory, please  provide your information using this ",
                               a("link",
                                 href = "https://www.surveymonkey.com/r/WBG6DN6"),
                               "(English version) or this ",
                               a("link",
                                 href = "https://fr.surveymonkey.com/r/WH9T3CM"),
                               "(French version) or download the file ",
                               a("here",
                                 href="https://docs.google.com/spreadsheets/d/1njjBHyR9TTJDMQWwO29-TigGWO_lYlEPS-naOFpdpnU/edit#gid=2113514347"),
                                "and send your application to: ",
                               a("waternexusbelgium@gmail.com",
                                 href = "mailto: waternexusbelgium@gmail.com")
                               )
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
                        box(title = "Project info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            ), style = "font-size:70%"
                        )
                    ),
                    fluidRow(
                        box(title = "Project map", width = 12,
                            leafletOutput("map", width = "100%", height = 400) # Can be changed
                        )
                    )
            ), # end of Info tab 
            # Activity tab content ####
            tabItem(tabName = "activity",
                    fluidRow(
                        valueBoxOutput("actor1"),
                        valueBoxOutput("country1"),
                        valueBoxOutput("sector1")
                    ),
                    fluidRow(
                        box(title = "Project info", width = 12, height = 700,
                            DT::dataTableOutput("table1"
                                                ,  width = "100%", height = 700
                            ), style = "font-size:70%" 
                        )
                    ),
                    fluidRow(
                        box(title = "Project map", width = 12, height = 700,
                            leafletOutput("map1", width = "100%", height = 700) # Can be changed
                        )
                    )
            ), # end of Activity tab
            # Contact tab content ####
            tabItem(tabName = "contact",
                    fluidRow(
                        valueBoxOutput("actor2"),
                        valueBoxOutput("country2"),
                        valueBoxOutput("sector2")
                    ),
                    fluidRow(
                        box(title = "Project info", width = 12, height = 700,
                            DT::dataTableOutput("table2"
                                                ,  width = "100%", height = 700
                            ), style = "font-size:70%"
                        )
                    ),
                    fluidRow(
                        box(title = "Project map", width = 12,
                            leafletOutput("map2", width = "100%", height = 400) # Can be changed
                        )
                    )
            ) # end of Contact tab 
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({Water_Nexus})
    
    # Nation 1
    nationname_1 <- reactive({
        nationname_1 <- vector(mode = 'character', length = 0)
        for (i in first_country:last_country){
            if(sum(!is.na(df()[,i] > 0))){
                nationname_1 <- c(nationname_1, colnames(df()[,i]))
            }
        }
        nationname_1
    })
    
    
    observe({
        updateSelectInput(session, inputId = "nation_1", label = "Select a country", choices = c("All", sort(nationname_1())))
    })
    
    df_country_1 <- reactive({
        input$nation_1
    })
    

    # Nation 2
    nationname_2 <- reactive({
        nationname_2 <- vector(mode = 'character', length = 0)
        for (i in first_country:last_country){
            if(sum(!is.na(df()[,i] > 0))){
                nationname_2 <- c(nationname_2, colnames(df()[,i]))
            }
        }
        nationname_2
    })
    
    
    observe({
        updateSelectInput(session, inputId = "nation_2", label = "Select a country", choices = c("All", sort(nationname_2())))
    })
    
    df_country_2 <- reactive({
        input$nation_2
    })
    
    
    # Actor 2

    actorname_2 <- reactive({
        if(df_country_2()!= "All"){
        a_1 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
        actorname_2 <- a_1$`Name 1`
        actorname_2
        } else {
            df()$`Name 1`
        }
    })

    observe({
        updateSelectInput(session, inputId = "actors_2", label = "Select an actor", choices = c("All", sort(actorname_2())))
    })

    df_actor_2 <- reactive({
        input$actors_2
    })

    # Sector 2
    
    sectorname_2 <- reactive({
        if(df_country_1()!= "All"){
            a <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            sectorname_2 <- vector(mode = "character", length = 0)
            for (i in first_sector:last_sector){
                if(sum(!is.na(a[,i]))>0){
                    sectorname_2 <- c(sectorname_2, colnames(a[,i]))
                }
            }
            sectorname_2
        } else {
            colnames(df())[first_sector:last_sector]
        }
    })
    
    
    observe({
        updateSelectInput(session, inputId = "research_2", label = "Select a sector", choices = c("All", sort(sectorname_2())))
    })
    
    df_research_2 <- reactive({
        input$research_2
    })
    
    # Actor 1

    df_actor_1 <- reactive({
        input$actors_1
    })

    # Sector 1

    sectorname_1 <- reactive({
        if(df_actor_1() != "All"){
            a_2 <- df()[df()$`Name 1` == df_actor_1(),]

            sectorname_1 <- vector(mode = "character", length = 0)
            for (i in first_sector:last_sector){
                if(sum(!is.na(a_2[,i]))>0){
                    sectorname_1 <- c(sectorname_1, colnames(a_2[,i]))
                }
            }
            sectorname_1
        } else {
            colnames(df())[first_sector:last_sector]
        }
    })

    observe({
        updateSelectInput(session, inputId = "research_1", label = "Select a sector", choices = c("All", sort(sectorname_1())))
    })

    df_research_1 <- reactive({
        input$research_1
    })

    
    # Output valuebox in Info tab ####
    output$actor <- renderValueBox({

        if (df_actor_1() == "All"){
            if (df_research_1() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_1()]),]
            }
        } else {
            if (df_research_1() == "All") {
                selectedData <- df()[df()$`Name 1` == df_actor_1(),]
            } else {
                selectedData2 <- df()[df()$`Name 1` == df_actor_1(),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_1()]),]
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

        if (df_actor_1() == "All"){
            if (df_research_1() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_1()]),]
            }
        } else {
            if (df_research_1() == "All") {
                selectedData <- df()[df()$`Name 1` == df_actor_1(),]
            } else {
                selectedData2 <- df()[df()$`Name 1` == df_actor_1(),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_1()]),]
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

        if (df_actor_1() == "All"){
            if (df_research_1() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_1()]),]
            }
        } else {
            if (df_research_1() == "All") {
                selectedData <- df()[df()$`Name 1` == df_actor_1(),]
            } else {
                selectedData2 <- df()[df()$`Name 1` == df_actor_1(),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_1()]),]
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

    # Output valuebox1 in Activity tab ####
    output$actor1 <- renderValueBox({
        
        if (df_country_1() == "All"){
            if (df_research_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_2()]),]
            }
        } else {
            if (df_research_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_2()]),]
            }
        }
        
        valueBox(
            value = nrow(selectedData),
            subtitle = "Total actor",
            icon = icon("landmark"),
            color = "purple"
        )
    })
    output$country1 <- renderValueBox({
        
        if (df_country_1() == "All"){
            if (df_research_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_2()]),]
            }
        } else {
            if (df_research_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_2()]),]
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
    output$sector1 <- renderValueBox({ 
        
        if (df_country_1() == "All"){
            if (df_research_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_2()]),]
            }
        } else {
            if (df_research_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_2()]),]
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
    
    # Output valuebox2 in Contact tab ####
    output$actor2 <- renderValueBox({
        
        if (df_country_2() == "All"){
            if (df_actor_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[df()$`Name 1` == df_actor_2(),]
            }
        } else {
            if (df_actor_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
                selectedData <- selectedData2[selectedData2$`Name 1` == df_actor_2(),]
            }
        }
        
        valueBox(
            value = nrow(selectedData),
            subtitle = "Total actor",
            icon = icon("landmark"),
            color = "purple"
        )
    })
    output$country2 <- renderValueBox({
        
        if (df_country_2() == "All"){
            if (df_actor_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[df()$`Name 1` == df_actor_2(),]
            }
        } else {
            if (df_actor_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
                selectedData <- selectedData2[selectedData2$`Name 1` == df_actor_2(),]
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
    output$sector2 <- renderValueBox({ 
        
        if (df_country_2() == "All"){
            if (df_actor_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[df()$`Name 1` == df_actor_2(),]
            }
        } else {
            if (df_actor_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
                selectedData <- selectedData2[selectedData2$`Name 1` == df_actor_2(),]
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
    
    # Output table in Info tab ####
    
    output$table <- DT::renderDataTable(server = FALSE, {
        
        if (df_actor_1() == "All"){
            if (df_research_1() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_1()]),]
            }
        } else {
            if (df_research_1() == "All") {
                selectedData <- df()[df()$`Name 1` == df_actor_1(),]
            } else {
                selectedData2 <- df()[df()$`Name 1` == df_actor_1(),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_1()]),]
            }
        }
        
        first_country <- which(colnames(selectedData) == 'Benin')
        last_country <- which(colnames(selectedData) == 'Switzerland')
        first_sector <- which(colnames(selectedData) == 'Water and agriculture')
        last_sector <- which(colnames(selectedData) == 'Finance')
        
        name <- selectedData[, 1:4] %>% tidyr::unite(Name, remove = TRUE, sep = ", ", na.rm = TRUE)
        address <- selectedData[, 8:11] %>% tidyr::unite(Address, remove = TRUE, sep = ", ", na.rm = TRUE)
        person <- selectedData[, c(12,16)] %>% tidyr::unite(Person, remove = TRUE, sep = " - ", na.rm = TRUE)
        country <- selectedData[,first_country:last_country][,colSums(is.na(selectedData[,first_country:last_country]))<nrow(selectedData[,first_country:last_country])] %>% 
            tidyr::unite(`Active country`, remove = TRUE, sep = ", ", na.rm = TRUE)
        sector <- selectedData[,first_sector:last_sector] %>% tidyr::unite(`Active sector`, remove = TRUE, sep = ", ", na.rm = TRUE)
        link <- selectedData[, 28]
        
        selectedData_v2 <- bind_cols(name, selectedData[,5], address, person, selectedData[,20], selectedData[,24], link, country, sector)
        colnames(selectedData_v2)[5:7] <- c("Email", "Telephone", "Website")
        
        
        DT::datatable({DT::datatable(selectedData_v2)
            selectedData_v2$Website <- paste0("<a href='",selectedData_v2$Website,"' target='_blank'>",selectedData_v2$Website,"</a>") # still have problems!!!
            selectedData_v2$Email <- paste0("<a href='mailto:", selectedData_v2$Email, "'>",selectedData_v2$Email, "</a>")
            selectedData_v2
            }, escape = FALSE,
            extensions = c('Buttons'),
            options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                           pageLength = 5,
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           scrollX = TRUE,
                           autoWidth = FALSE))
    })
    
    # Output table1 in Activity tab ####
    
    output$table1 <- DT::renderDataTable(server = FALSE, {
        
        if (df_country_1() == "All"){
            if (df_research_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_2()]),]
            }
        } else {
            if (df_research_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_2()]),]
            }
        }
        
        first_country <- which(colnames(selectedData) == 'Benin')
        last_country <- which(colnames(selectedData) == 'Switzerland')
        first_sector <- which(colnames(selectedData) == 'Water and agriculture')
        last_sector <- which(colnames(selectedData) == 'Finance')
        
        name <- selectedData[, 1:4] %>% tidyr::unite(Name, remove = TRUE, sep = ", ", na.rm = TRUE)
        address <- selectedData[, 8:11] %>% tidyr::unite(Address, remove = TRUE, sep = ", ", na.rm = TRUE)
        person <- selectedData[, c(12,16)] %>% tidyr::unite(Person, remove = TRUE, sep = " - ", na.rm = TRUE)
        country <- selectedData[,first_country:last_country][,colSums(is.na(selectedData[,first_country:last_country]))<nrow(selectedData[,first_country:last_country])] %>% 
            tidyr::unite(`Active country`, remove = TRUE, sep = ", ", na.rm = TRUE)
        sector <- selectedData[,first_sector:last_sector] %>% tidyr::unite(`Active sector`, remove = TRUE, sep = ", ", na.rm = TRUE)
        link <- selectedData[, 28]
        
        selectedData_v2 <- bind_cols(name, selectedData[,5], address, person, selectedData[,20], selectedData[,24], link, country, sector)
        colnames(selectedData_v2)[5:7] <- c("Email", "Telephone", "Website")
        
        DT::datatable({DT::datatable(selectedData_v2)
            selectedData_v2$Website <- paste0("<a href='",selectedData_v2$Website,"' target='_blank'>",selectedData_v2$Website,"</a>") # still have problems!!!
            selectedData_v2$Email <- paste0("<a href='mailto:", selectedData_v2$Email, "'>",selectedData_v2$Email, "</a>")
            selectedData_v2
        }, escape = FALSE,
        extensions = c('Buttons'),
        options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                       pageLength = 5,
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       scrollX = TRUE,
                       autoWidth = FALSE))
    })
    
    # Output table2 in Contact tab ####
    
    output$table2 <- DT::renderDataTable(server = FALSE, {
        
        if (df_country_2() == "All"){
            if (df_actor_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[df()$`Name 1` == df_actor_2(),]
            }
        } else {
            if (df_actor_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
                selectedData <- selectedData2[selectedData2$`Name 1` == df_actor_2(),]
            }
        }
        
        first_country <- which(colnames(selectedData) == 'Benin')
        last_country <- which(colnames(selectedData) == 'Switzerland')
        first_sector <- which(colnames(selectedData) == 'Water and agriculture')
        last_sector <- which(colnames(selectedData) == 'Finance')
        
        name <- selectedData[, 1:4] %>% tidyr::unite(Name, remove = TRUE, sep = ", ", na.rm = TRUE)
        address <- selectedData[, 8:11] %>% tidyr::unite(Address, remove = TRUE, sep = ", ", na.rm = TRUE)
        person <- selectedData[, c(12,16)] %>% tidyr::unite(Person, remove = TRUE, sep = " - ", na.rm = TRUE)
        country <- selectedData[,first_country:last_country][,colSums(is.na(selectedData[,first_country:last_country]))<nrow(selectedData[,first_country:last_country])] %>% 
            tidyr::unite(`Active country`, remove = TRUE, sep = ", ", na.rm = TRUE)
        sector <- selectedData[,first_sector:last_sector] %>% tidyr::unite(`Active sector`, remove = TRUE, sep = ", ", na.rm = TRUE)
        link <- selectedData[, 28]
        
        selectedData_v2 <- bind_cols(name, selectedData[,5], address, person, selectedData[,20], selectedData[,24], link, country, sector)
        colnames(selectedData_v2)[5:7] <- c("Email", "Telephone", "Website")
        
        DT::datatable({DT::datatable(selectedData_v2)
            selectedData_v2$Website <- paste0("<a href='",selectedData_v2$Website,"' target='_blank'>",selectedData_v2$Website,"</a>") # still have problems!!!
            selectedData_v2$Email <- paste0("<a href='mailto:", selectedData_v2$Email, "'>",selectedData_v2$Email, "</a>")
            selectedData_v2
        }, escape = FALSE,
        extensions = c('Buttons'),
        options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                       pageLength = 5,
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       scrollX = TRUE,
                       autoWidth = FALSE))
    })
    
    # Output map in Ìnfo tab ####
    output$map <- renderLeaflet({
        
        if (df_actor_1() == "All"){
            if (df_research_1() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_1()]),]
            }
        } else {
            if (df_research_1() == "All") {
                selectedData <- df()[df()$`Name 1` == df_actor_1(),]
            } else {
                selectedData2 <- df()[df()$`Name 1` == df_actor_1(),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_1()]),]
            }
        }
        
        selectedData_v2 <- selectedData[,c(first_country:last_country, 6)] %>%
            gather(key = "Country", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v3 <- selectedData[,c(first_lat:last_lat, 6)] %>%
            gather(key = "Lat", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v4 <- selectedData[,c(first_long:last_long, 6)] %>%
            gather(key = "Long", value = "value", -Response, na.rm =TRUE) 
        
        
        selectedData_v2$lat <- selectedData_v3$value
        selectedData_v2$long <- selectedData_v4$value
        
        selectedData_v2 <- selectedData_v2 %>% 
            group_by(Country, lat, long, Response) %>%
            summarise(n=n()) %>%
            spread(key = Response, value = n)
        
        selectedData_v2$Total <- rowSums(subset(selectedData_v2, select = -c(Country, lat, long)), na.rm = TRUE)
        
        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 8, name = "Dark2")
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selectedData_v2$lat, selectedData_v2$long,
                          type = "pie",
                          chartdata = subset(selectedData_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selectedData_v2$Total) / sqrt(max(selectedData_v2$Total)),
                          transitionTime = 0)
    })

    # Output map1 in Activity tab ####
    output$map1 <- renderLeaflet({

        if (df_country_1() == "All"){
            if (df_research_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_research_2()]),]
            }
        } else {
            if (df_research_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_1()]),]
                selectedData <- selectedData2[!is.na(selectedData2[, colnames(selectedData2) == df_research_2()]),]
            }
        }

        selectedData_v2 <- selectedData[,c(first_country:last_country, 6)] %>%
            gather(key = "Country", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v3 <- selectedData[,c(first_lat:last_lat, 6)] %>%
            gather(key = "Lat", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v4 <- selectedData[,c(first_long:last_long, 6)] %>%
            gather(key = "Long", value = "value", -Response, na.rm =TRUE) 
        
        
        selectedData_v2$lat <- selectedData_v3$value
        selectedData_v2$long <- selectedData_v4$value
        
        selectedData_v2 <- selectedData_v2 %>% 
            group_by(Country, lat, long, Response) %>%
            summarise(n=n()) %>%
            spread(key = Response, value = n)
        
        selectedData_v2$Total <- rowSums(subset(selectedData_v2, select = -c(Country, lat, long)), na.rm = TRUE)
        
        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 8, name = "Dark2")
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selectedData_v2$lat, selectedData_v2$long,
                          type = "pie",
                          chartdata = subset(selectedData_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selectedData_v2$Total) / sqrt(max(selectedData_v2$Total)),
                          transitionTime = 0)
    })

    # Output map2 in Contact tab ####
    output$map2 <- renderLeaflet({

        if (df_country_2() == "All"){
            if (df_actor_2() == "All") {
                selectedData <- df()
            } else {
                selectedData <- df()[df()$`Name 1` == df_actor_2(),]
            }
        } else {
            if (df_actor_2() == "All") {
                selectedData <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
            } else {
                selectedData2 <- df()[!is.na(df()[, colnames(df()) == df_country_2()]),]
                selectedData <- selectedData2[selectedData2$`Name 1` == df_actor_2(),]
            }
        }

        selectedData_v2 <- selectedData[,c(first_country:last_country, 6)] %>%
            gather(key = "Country", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v3 <- selectedData[,c(first_lat:last_lat, 6)] %>%
            gather(key = "Lat", value = "value", -Response, na.rm =TRUE) 
        
        selectedData_v4 <- selectedData[,c(first_long:last_long, 6)] %>%
            gather(key = "Long", value = "value", -Response, na.rm =TRUE) 
        
        
        selectedData_v2$lat <- selectedData_v3$value
        selectedData_v2$long <- selectedData_v4$value
        
        selectedData_v2 <- selectedData_v2 %>% 
            group_by(Country, lat, long, Response) %>%
            summarise(n=n()) %>%
            spread(key = Response, value = n)
        
        selectedData_v2$Total <- rowSums(subset(selectedData_v2, select = -c(Country, lat, long)), na.rm = TRUE)
        
        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 8, name = "Dark2")
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selectedData_v2$lat, selectedData_v2$long,
                          type = "pie",
                          chartdata = subset(selectedData_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selectedData_v2$Total) / sqrt(max(selectedData_v2$Total)),
                          transitionTime = 0)
    })

}

#### Run the application ####
shinyApp(ui = ui, server = server)
