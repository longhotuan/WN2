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
library(feather)

water_nexus <- read_feather("WN2_v1.feather")

first_country <- which(colnames(water_nexus) == 'Afghanistan')
last_country <- which(colnames(water_nexus) == 'Zimbabwe')
first_lat <- which(colnames(water_nexus) == 'lat_Afghanistan')
last_lat <- which(colnames(water_nexus) == 'lat_Zimbabwe')
first_long <- which(colnames(water_nexus) == 'long_Afghanistan')
last_long <- which(colnames(water_nexus) == 'long_Zimbabwe')
first_sector <- which(colnames(water_nexus) == 'Water and agriculture')
last_sector <- which(colnames(water_nexus) == 'Finance')
firstkw <- which(colnames(water_nexus) == 'Keyword 1')
lastkw <- which(colnames(water_nexus) == 'Keyword 6')

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
                    menuItem("Actors info", 
                             tabName = "info",
                             icon = icon("landmark")),
                    conditionalPanel(condition = "input.tabs == 'info'",
                                     selectInput(inputId = "nation", label = "Select a country", 
                                                 choices = c(All = "All", "Partner countries", colnames(water_nexus)[first_country:last_country])),
                                     selectInput(inputId = "research", label = "Select a field of expertise",
                                                 choices = c(All = "All", colnames(water_nexus)[first_sector:last_sector])),
                                     selectInput(inputId = "sector", label = "Select a sector", 
                                                 choices = c(All = "All", sort(water_nexus$Sector)))
                                     )
        )
    ),
    # Dashboard body #### 
    dashboardBody(
        tabItems(
            # About tab content ####
            tabItem(tabName = "about",
                    # fluidRow(
                    #     box(width = 12, 
                    #         h2("About the dashboard"),
                    #         hr(),
                    #         h3("Belgian Actors in the Water Sector"),
                    #         br(),
                    #         h4(
                    #             "This interactive Beldgian Actors dashboard aims to inventory, depict and provide a searchable database of the Belgian actors active in the water sector and international cooperation. The actors are multidisciplinary, including public sector,  NGO's, civil society, private sector, teaching/research institutes, and PPPs/ Networks.")
                    #     )
                    # ),
                    fluidRow(
                        box(width = 12, 
                            h2("About the dashboard"),
                            # hr(),
                            # h3("Dataset of Directorate-General for Development Cooperation and Humanitarian Aid"),
                            br(),
                            h5("This interactive Belgian Actors dashboard aims to inventory and depict the Belgian actors active in the water sector and international cooperation, and provide a searchable database . The data have been collected through a survey with the actors. Although we aim the inventory to be as comprehensive as possible, it is likely that actors are still missing."),
                            br(),
                            h5("If you want to add or update the information of your organisation in this inventory, please do so ",
                               a("here",
                                 href="https://www.surveymonkey.com/r/5P87THP"),
                               ". If you want to update your information, please send us an email at: ",
                               a("waternexusbelgium@gmail.com",
                                 href = "mailto: waternexusbelgium@gmail.com"), "and we will provide the instructions to do so."
                               ),
                            br(),
                            h5("In addition to contact data, actors were also invited to provide the following key elements of information about their organisation and activities:"),
                            br(),
                            tags$b("Sector (to be selected among the following list):"),
                            br(),
                            h5("    -     Public agency (includes governments and institutional actors)"),
                            h5("    -     Public utility / entreprise"),
                            h5("    -     Government-recognised NGO"),
                            h5("    -     NPO or 4th pillar organisation"),
                            h5("    -     Research institute or team; Knowledge center"),
                            h5("    -     Educational institution"),
                            h5("    -     Private sector organisation"),
                            h5("    -     Platform / group / center"),
                            br(),
                            tags$b("Fields of expertise (to be selected among the following list):"),
                            br(),
                            h5("    -     Water and agriculture"),
                            h5("    -     Water and sustainable cities"),
                            h5("    -     Hydropower"),
                            h5("    -     Water sanitation and hygiene"),
                            h5("    -     Water supply"),
                            h5("    -     Waste water treatment"),
                            h5("    -     Disaster risk management (floods and droughts)"),
                            h5("    -     Water technology"),
                            h5("    -     Equipment supply"),
                            h5("    -     Water consultancy"),
                            h5("    -     Data and information"),
                            h5("    -     Research and development"),
                            h5("    -     Education and training"),
                            h5("    -     Water policy and governance"),
                            h5("    -     Finance"),
                            br(),
                            tags$b("Countries where the actors have had activities over the course of the past 10 years"),
                            br(),
                            h5("The dashboard contains a set of filters to identify the actors  on the basis of these information elements. Each elements is also searchable by typing in a word in the linked search bar. For example, one can search all the actors that listed “Groundwater” in their keywords by typing in “Groundwater” in the Keyword search bar."),
                            br(),
                            br(),
                            h5("Last updated on 12/12/2019")
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
                    ),
                    fluidRow(
                        column(6,
                               h2("Through"),
                               box(
                                   img(style = "max-width:100%",
                                       src = "Logo3.jpg")
                               ),
                               box(
                                   img(style = "max-width:100%",
                                       src = "Logo4.png")
                               )
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
                        box(title = "Actors info", width = 12, height = 700,
                            DT::dataTableOutput("table"
                                                ,  width = "100%", height = 700
                            ), style = "font-size:70%"
                        )
                    ),
                    fluidRow(
                        box(title = "Countries where actors have had activities over the course of the past 10 years", width = 12, height = 400, 
                            leafletOutput("map", width = "100%", height = 400) # Can be changed
                        )
                    )
            ) # end of Info tab 
        ) # end tabItems
    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({water_nexus})
    
    # Nation
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
        updateSelectInput(session, inputId = "nation", label = "Select a country", choices = c("All", "Partner countries", sort(nationname())))
    })
    
    df_country <- reactive({
        input$nation
    })

    # Sector
    
    sectorname <- reactive({
        if(df_country() == "All"){
            colnames(df())[first_sector:last_sector]
        } else if(df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
            sectorname2 <- df()[rowSums(is.na(m)) != ncol(m), ]
            sectorname <- vector(mode = "character", length = 0)
            for (i in first_sector:last_sector){
                if(sum(!is.na(sectorname2[,i]))>0){
                    sectorname <- c(sectorname, colnames(sectorname2[,i]))
                }
            }
            sectorname 
        } else {
            sectorname2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            sectorname <- vector(mode = "character", length = 0)
            for (i in first_sector:last_sector){
                if(sum(!is.na(sectorname2[,i]))>0){
                    sectorname <- c(sectorname, colnames(sectorname2[,i]))
                }
            }
            sectorname 
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "research", label = "Select a field of expertise", choices = c("All", sort(sectorname())))
    })
    
    df_research <- reactive({
        input$research
    })
    
    # Sector
    
    orgname <- reactive({
        if(df_country() == "All"){
            sectorname <- df()
            if(df_research() == "All"){
                orgname <- sectorname$Sector
            } else {
                orgname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                orgname <- orgname2$Sector
                orgname
            }
        } else if(df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            sectorname <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            sectorname <- sectorname[complete.cases(sectorname[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                orgname <- sectorname$Sector
            } else {
                orgname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                orgname <- orgname2$Sector
            }
            
        } else {
            sectorname <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                orgname <- sectorname$Sector
            } else {
                orgname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                orgname <- orgname2$Sector
                orgname
            }
        }
    })

    observe({
        updateSelectInput(session, inputId = "sector", label = "Select a sector", choices = c("All", sort(orgname())))
    })

    df_sector <- reactive({
        input$sector
    })
    
    # Output valuebox in Info tab ####
    output$actor <- renderValueBox({
        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                } 
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        }
        valueBox(
            value = nrow(selecteddf),
            subtitle = "Total actor",
            icon = icon("landmark"),
            color = "purple"
        )
    })
    output$country <- renderValueBox({
        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                } 
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        }

        selecteddf_v2 <- selecteddf[,first_country:last_country][, colSums(is.na(selecteddf[,first_country:last_country])) < nrow(selecteddf[,first_country:last_country])]

        valueBox(
            value = ncol(selecteddf_v2),
            subtitle = "Total country",
            icon = icon("flag"),
            color = "yellow"
        )
    })
    output$sector <- renderValueBox({ 

        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                } 
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        }

        selecteddf_v2 <- selecteddf[,first_sector:last_sector][, colSums(is.na(selecteddf[,first_sector:last_sector]))< nrow(selecteddf[,first_sector:last_sector])]

        valueBox(
            value = ncol(selecteddf_v2),
            subtitle = "Total field of expertise",
            icon = icon("folder-open"),
            color = "purple"
        )
    })

    # Output table in Info tab ####

    output$table <- DT::renderDataTable(server = FALSE, {
        
        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                } 
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        }
        
        name <- selecteddf[, 1:4] %>% tidyr::unite(Name, remove = TRUE, sep = ", ", na.rm = TRUE)
        address <- selecteddf[, 8:11] %>% tidyr::unite(Address, remove = TRUE, sep = ", ", na.rm = TRUE)
        person <- selecteddf[, c(12,16)] %>% tidyr::unite(Person, remove = TRUE, sep = " - ", na.rm = TRUE)
        country <- selecteddf[,first_country:last_country][,colSums(is.na(selecteddf[,first_country:last_country]))<nrow(selecteddf[,first_country:last_country])] %>%
            tidyr::unite(`Active country`, remove = TRUE, sep = ", ", na.rm = TRUE)
        sector <- selecteddf[,first_sector:last_sector] %>% tidyr::unite(`Active sector`, remove = TRUE, sep = ", ", na.rm = TRUE)
        link <- selecteddf[, 28]
        keywords <- selecteddf[,firstkw:lastkw] %>% tidyr::unite(`Active sector`, remove = TRUE, sep = ", ", na.rm = TRUE)
        # description <- selecteddf[,which(colnames(selecteddf) == 'Description')]
        

        selecteddf_v2 <- bind_cols(name, selecteddf[,5], address, person, selecteddf[,20], selecteddf[,24], link, selecteddf[,6], country, sector, keywords)
        colnames(selecteddf_v2)[5:8] <- c("Email", "Telephone", "Website", "Organisation")
        colnames(selecteddf_v2)[4] <- "Contact person"
        colnames(selecteddf_v2)[10:11] <- c("Active field of expertise", "Keywords")

        DT::datatable({DT::datatable(selecteddf_v2)
            selecteddf_v2$Website <- paste0("<a href='",selecteddf_v2$Website,"' target='_blank'>",selecteddf_v2$Website,"</a>")
            selecteddf_v2$Email <- paste0("<a href='mailto:", selecteddf_v2$Email, "'>",selecteddf_v2$Email, "</a>")
            selecteddf_v2
            }, escape = FALSE,
            rownames = FALSE,
            filter = "top",
            selection="multiple",
            extensions = c('Buttons'),
            options = list(sDom  = '<"top"pB>t<"bottom"i>r',
                           pageLength = 5,
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           dom = 't',
                           scrollX = TRUE,
                           fixedColumns = FALSE
                           ))
    })

    # Output map in Info tab ####
    output$map <- renderLeaflet({

        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                } 
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$Sector == df_sector(),]
                }
            } else {
                if(df_sector() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$Sector == df_sector(),]
                }
            }
        }

        selecteddf_v2 <- selecteddf[,c(first_country:last_country, 6)] %>%
            gather(key = "Country", value = "value", -Sector, na.rm =TRUE)

        selecteddf_v3 <- selecteddf[,c(first_lat:last_lat, 6)] %>%
            gather(key = "Lat", value = "value", -Sector, na.rm =TRUE)

        selecteddf_v4 <- selecteddf[,c(first_long:last_long, 6)] %>%
            gather(key = "Long", value = "value", -Sector, na.rm =TRUE)


        selecteddf_v2$lat <- selecteddf_v3$value
        selecteddf_v2$long <- selecteddf_v4$value

        selecteddf_v2 <- selecteddf_v2 %>%
            group_by(Country, lat, long, Sector) %>%
            summarise(n=n()) %>%
            spread(key = Sector, value = n)

        selecteddf_v2$Total <- rowSums(subset(selecteddf_v2, select = -c(Country, lat, long)), na.rm = TRUE)

        tilesURL <- '//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}'
        colors <- brewer.pal(n = 8, name = "Dark2")
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -141.152344, lat1 = 55.646599, lng2 = 161.542969, lat2 = -52.194140) %>%
            addMinicharts(selecteddf_v2$lat, selecteddf_v2$long,
                          type = "pie",
                          chartdata = subset(selecteddf_v2, select = -c(Country, lat, long, Total)),
                          colorPalette = colors,
                          width = 80 * sqrt(selecteddf_v2$Total) / sqrt(max(selecteddf_v2$Total)),
                          transitionTime = 0)
    })

}

#### Run the application ####
shinyApp(ui = ui, server = server)