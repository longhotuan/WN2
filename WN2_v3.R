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

water_nexus <- read_csv('WN2_v12.csv', locale = readr::locale(encoding = "latin1"))
first_country <- which(colnames(water_nexus) == 'Afghanistan')
last_country <- which(colnames(water_nexus) == 'Zimbabwe')
first_lat <- which(colnames(water_nexus) == 'lat_Afghanistan')
last_lat <- which(colnames(water_nexus) == 'lat_Zimbabwe')
first_long <- which(colnames(water_nexus) == 'long_Afghanistan')
last_long <- which(colnames(water_nexus) == 'long_Zimbabwe')
first_sector <- which(colnames(water_nexus) == 'Water and agriculture')
last_sector <- which(colnames(water_nexus) == 'Finance')

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
                                     selectInput(inputId = "research", label = "Select a sector",
                                                 choices = c(All = "All", colnames(water_nexus)[first_sector:last_sector])),
                                     selectInput(inputId = "actors", label = "Select an actor", 
                                                 choices = c(All = "All", sort(water_nexus$`Name 1`)))
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
                                "This interactive Beldgian Actors dashboard aims to inventory, depict and provide a searchable database of the Belgian actors active in the water sector and international cooperation. The actors are multidisciplinary, including public sector,  NGO's, civil society, private sector, teaching/research institutes, and PPPs/ Networks.")
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
                            h4("If you want to add or update the information of your organisation in this inventory, please fill in an excel file ",
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
                        box(title = "Project map", width = 12, height = 400, 
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
        updateSelectInput(session, inputId = "research", label = "Select a sector", choices = c("All", sort(sectorname())))
    })
    
    df_research <- reactive({
        input$research
    })
    
    # Actor
    
    actorname <- reactive({
        if(df_country() == "All"){
            sectorname <- df()
            if(df_research() == "All"){
                actorname <- sectorname$`Name 1`
            } else {
                actorname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                actorname <- actorname2$`Name 1`
                actorname
            }
        } else if(df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            sectorname <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            sectorname <- sectorname[complete.cases(sectorname[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                actorname <- sectorname$`Name 1`
            } else {
                actorname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                actorname <- actorname2$`Name 1`
            }
            
        } else {
            sectorname <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                actorname <- sectorname$`Name 1`
            } else {
                actorname2 <- sectorname[!is.na(sectorname[, colnames(sectorname) == df_research()]),]
                actorname <- actorname2$`Name 1`
                actorname
            }
        }
    })

    observe({
        updateSelectInput(session, inputId = "actors", label = "Select an actor", choices = c("All", sort(actorname())))
    })

    df_actor <- reactive({
        input$actors
    })
    
    # Output valuebox in Info tab ####
    output$actor <- renderValueBox({
        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                } 
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
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
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                } 
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
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
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                } 
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        }

        selecteddf_v2 <- selecteddf[,first_sector:last_sector][, colSums(is.na(selecteddf[,first_sector:last_sector]))< nrow(selecteddf[,first_sector:last_sector])]

        valueBox(
            value = ncol(selecteddf_v2),
            subtitle = "Total sector",
            icon = icon("folder-open"),
            color = "purple"
        )
    })

    # Output table in Info tab ####

    output$table <- DT::renderDataTable(server = FALSE, {
        
        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                } 
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
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

        selecteddf_v2 <- bind_cols(name, selecteddf[,5], address, person, selecteddf[,20], selecteddf[,24], link, selecteddf[,6], country, sector)
        colnames(selecteddf_v2)[5:8] <- c("Email", "Telephone", "Website", "Organisation")


        DT::datatable({DT::datatable(selecteddf_v2)
            selecteddf_v2$Website <- paste0("<a href='",selecteddf_v2$Website,"' target='_blank'>",selecteddf_v2$Website,"</a>") # still have problems!!!
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
                           scrollX = TRUE,
                           autoWidth = FALSE))
    })

    # Output map in Info tab ####
    output$map <- renderLeaflet({

        if (df_country() == "All"){
            selecteddf2 <- df()
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                } 
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else {
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else if (df_country() == "Partner countries"){
            m <-df()[, which(colnames(df()) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                                   "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))] # choose column having the same names
            selecteddf2 <- df()[rowSums(is.na(m)) != ncol(m), ] # make sure the chosen column not entire NA 
            selecteddf2 <- selecteddf2[complete.cases(selecteddf2[ ,1]),] # remove the one that has NA in all row
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf2$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        } else {
            selecteddf2 <- df()[!is.na(df()[, colnames(df()) == df_country()]),]
            if(df_research() == "All"){
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2
                } else {
                    selecteddf <- selecteddf2[selecteddf3$`Name 1` == df_actor(),]
                }
            } else {
                if(df_actor() == "All"){
                    selecteddf <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                } else{
                    selecteddf3 <- selecteddf2[!is.na(selecteddf2[, colnames(selecteddf2) == df_research()]),]
                    selecteddf <- selecteddf3[selecteddf3$`Name 1` == df_actor(),]
                }
            }
        }

        selecteddf_v2 <- selecteddf[,c(first_country:last_country, 6)] %>%
            gather(key = "Country", value = "value", -Response, na.rm =TRUE)

        selecteddf_v3 <- selecteddf[,c(first_lat:last_lat, 6)] %>%
            gather(key = "Lat", value = "value", -Response, na.rm =TRUE)

        selecteddf_v4 <- selecteddf[,c(first_long:last_long, 6)] %>%
            gather(key = "Long", value = "value", -Response, na.rm =TRUE)


        selecteddf_v2$lat <- selecteddf_v3$value
        selecteddf_v2$long <- selecteddf_v4$value

        selecteddf_v2 <- selecteddf_v2 %>%
            group_by(Country, lat, long, Response) %>%
            summarise(n=n()) %>%
            spread(key = Response, value = n)

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