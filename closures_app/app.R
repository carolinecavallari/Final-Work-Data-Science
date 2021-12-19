#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
########################
## Data Organizations ##
########################

#load packages
pacman::p_load(haven, readxl, tidyverse, lubridate, readr, dplyr, knitr, rvest, tibble, broom, ggplot2, naniar, scales, shinydashboard, shiny, RColorBrewer, ggpubr, plotly, DT, sf, rsconnect)

#1. Enrollment Data
enrollment_df <- read_excel("countries-enrollment-data-uis-sep-21.xlsx")

#2. Covid19 Schools' Closure  information since Febraury 2020
closures_df <- read_csv("covid_impact_education.csv")

#2.a Cumulative Schools Closure data
cumulative_closure_df <- read_excel("School-Closures-Database-Final-1.xlsx", 2)

#3. Total School Connectivity
Total_school_connectivity_df <- read_excel("School-Age-Digital-Connectivity.xlsx", 2)

#4. Teachers' vaccinaton data
teacher_vac_df <- read_excel("covid-19-teacher-vaccination.xlsx", 1)

#5. Updated UNESCO Database
UNESCO_school_closures_df <- read_excel("UNESCO_school_closures_database.xlsx")

####################
## Data Wrangling ##
####################




##Wrangling School Closures DF

sf <- stamp("02-2019", orders = "my") 
closures_df$Date <- lubridate::dmy(closures_df$Date)
closures_df <- closures_df %>%
    mutate(month_year = sf(Date)) %>%
    filter(month_year != "02-2020")

is.factor(closures_df$Status)
table(closures_df$Status)
closures_df$Status <- factor(closures_df$Status, levels = c("Closed due to COVID-19", "Partially open", "Fully open", "Academic break"))
closures_df$test <- as.numeric(closures_df$Status)
nfactor <- length(levels(closures_df$Status))
foo <- brewer.pal(n = nfactor, name = "Set1")
names(foo) <- levels(closures_df$Status)

##Wrangling Teacher vaccination data
names(teacher_vac_df)[names(teacher_vac_df) == "Vaccination data reference date"] <- "Date"
names(teacher_vac_df)[names(teacher_vac_df) == "Teacher prioritization in vaccination plans"] <- "vac_priority_status"
head(teacher_vac_df)

is.factor(teacher_vac_df$vac_priority_status)
table(teacher_vac_df$vac_priority_status)
teacher_vac_df$vac_priority_status<- factor(teacher_vac_df$vac_priority_status, levels = c("Group 1", "Group 2", "Group 3 or lower", "Not prioritized", "Not specified", "No data"))

#Wrangling cumulative_closure_df
head(cumulative_closure_df)

names(cumulative_closure_df)[names( cumulative_closure_df) == "UNICEF Country"] <- "country"
names(cumulative_closure_df)[names( cumulative_closure_df) == "UNICEF Region"] <- "region"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Income Group"] <- "income_group"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Days: Academic break"] <- "academic_break"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Days: Fully closed"] <- "fully_closed"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Days: Fully open"] <- "fully_open"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Days:  Partially closed"] <- "partially_closed"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Instruction Days"] <- "instructions_date"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Pre-primary"] <- "pre_primary"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Lower Secondary"] <- "lower_secondary"
names(cumulative_closure_df)[names( cumulative_closure_df) == "Upper Secondary"] <- "upper_secondary"

##Wrangling connectivity data
head(Total_school_connectivity_df)

names(Total_school_connectivity_df)[names(Total_school_connectivity_df) == "Countries and areas"] <- "country"
names(Total_school_connectivity_df)[names(Total_school_connectivity_df) == "Sub-region"] <- "sub_region"
names(Total_school_connectivity_df)[names(Total_school_connectivity_df) == "Income Group"] <- "income_group"
names(Total_school_connectivity_df)[names(Total_school_connectivity_df) == "...12"] <- "Date"


#Wrangling Vaccination rates
head(teacher_vac_df)
names(teacher_vac_df)[names(teacher_vac_df) == "Country"] <- "country"
names(teacher_vac_df)[names(teacher_vac_df) == "% of teachers with at least one dosis"] <- "first_shot"
names(teacher_vac_df)[names(teacher_vac_df) == "% of teachers fully vaccinated"] <- "second_shot"

#Join data frames and create complete_schools_df
complete_schools_df <- dplyr::left_join(cumulative_closure_df,
                                        Total_school_connectivity_df, 
                                        by = "ISO3" ## Be specific about the joining column
) %>%
    select(ISO3, country.x, region, income_group.x, academic_break, fully_closed, fully_open, partially_closed, instructions_date, pre_primary, Primary, lower_secondary, upper_secondary, Total)

names(complete_schools_df)[names(complete_schools_df) == "income_group.x"] <- "income_group"
names(complete_schools_df)[names(complete_schools_df) == "country.x"] <- "country"
names(complete_schools_df)[names(complete_schools_df) == "Total"] <- "internet_access"
names(complete_schools_df)[names(complete_schools_df) == "Primary"] <- "primary"


complete_schools_df <- dplyr::left_join(complete_schools_df,
                                        teacher_vac_df, 
                                        by = "country" 
) %>%
    select(-c(first_shot, second_shot, Date))

head(complete_schools_df)

complete_schools_df <- complete_schools_df %>%
    mutate(., total_students = pre_primary + primary + lower_secondary + upper_secondary, .keep = "all")
complete_schools_df

complete_schools_df 
is.factor(complete_schools_df$vac_priority_status)
table(complete_schools_df$vac_priority_status)


table(complete_schools_df$region)

#Wrangling UNESCO Database for distance learning 

names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Country ID"] <- "ISO3"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Country"] <- "country"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Region 1"] <- "region"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Status"] <- "status"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Enrolment (Pre-Primary to Tertiary)"] <- "enrollment"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Teachers (Pre-Primary to Upper Secondary)"] <- "teachers_number"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Distance learning modalities (TV)"] <- "tv"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Distance learning modalities (Radio)"] <- "radio"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Distance learning modalities (Online)"] <- "online_learning"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Distance learning modalities (Global)"] <- "distance_learning"
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Weeks partially open"] <- "weeks_partially_closed"  
names(UNESCO_school_closures_df)[names(UNESCO_school_closures_df) == "Weeks fully closed"] <- "weeks_fully_closed"                                   
head(UNESCO_school_closures_df)    


UNESCO_school_closures_df <- UNESCO_school_closures_df %>%
    select(., Date, ISO3, country, region, status, enrollment, teachers_number, tv, radio, online_learning, distance_learning, weeks_partially_closed, weeks_fully_closed)


UNESCO_school_closures_df$Date <- lubridate::ymd(UNESCO_school_closures_df$Date)

typeof(UNESCO_school_closures_df$Date)

#Adding TV and Radio data 
unesco_reduced <- filter(UNESCO_school_closures_df, UNESCO_school_closures_df$Date == "2021-10-31")%>%
    select(., tv, radio, online_learning, distance_learning, ISO3)


complete_schools_df <- dplyr::left_join(complete_schools_df,
                                        unesco_reduced, 
                                        by = "ISO3" 
) 

#creating a table for closures_df

closures_table_df <- select(complete_schools_df, c(country, region, 
                                                   fully_closed, partially_closed, internet_access,
                                                   vac_priority_status, total_students, tv, radio, 
                                                   online_learning, distance_learning))





#functions for graphs

Z_Breaks = function(n){
    CUTS = seq(0,1,length.out=n+1)
    rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}




##Defining shiny dashboard components and publishing

ui <- dashboardPage(
    dashboardHeader(
        title = "School Closures during the Covid-19 Pandemic",
        titleWidth = 450),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("World Map and Scatterplot",
                     tabName = "plots_tab",
                     icon = icon("dashboard")),
            menuItem("By Country Table",
                     tabName = "table_tab",
                     icon = icon("th-list")),
            menuItem("Use Guide",
                     tabName = "guide_tab",
                     icon = icon("road"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "plots_tab",
                    fluidRow(box(span(h2("School Closure Totals"),
                                      textOutput("date"),
                                      textOutput("n_schools_closed"),
                                      textOutput("n_schools_partial"),
                                      textOutput("n_schools_open"),
                                      textOutput("n_schools_break"),
                    ))),
                    fluidRow(box(width = 12, selectInput(
                        "month_select",
                        label = "Select the Month to show on the Map",
                        choices = c(
                            "03-2020",
                            "04-2020",
                            "05-2020",
                            "06-2020",
                            "07-2020",
                            "08-2020",
                            "09-2020",
                            "10-2020",
                            "11-2020",
                            "12-2020",
                            "01-2021",
                            "02-2021",
                            "03-2021",
                            "04-2021",
                            "05-2021",
                            "06-2021",
                            "07-2021",
                            "08-2021",
                            "09-2021",
                            "10-2021",
                            "11-2021"
                        ),
                        selected = "11-2021"
                    ),
                    plotlyOutput("fig", width = "100%"))),
                    fluidRow(box(width = 12,
                                 plotlyOutput("plot2", width = "100%"),
                                 selectInput(
                                     "region",
                                     label = "Select the Region for the ScatterPlot",
                                     choices = c(
                                         "all",
                                         "East Asia and Pacific",
                                         "Eastern and Southern Africa",
                                         "Eastern Europe and Central Asia",
                                         "Latin America and Caribbean",
                                         "Middle East and North Africa",
                                         "North America",
                                         "South Asia",
                                         "West and Central Africa",
                                         "Western Europe"
                                     ),
                                     selected = "all"
                                 ))
                    )),
            tabItem(tabName = "table_tab",
                    DT::dataTableOutput("closures_table")),
            tabItem(tabName = "guide_tab",
                    uiOutput("guide"))
        )
    ))

server <- function(input, output) {
    output$fig <- renderPlotly({
        
        filteredData <- filter(closures_df, month_year == input$month_select)
        
        colorScale <- data.frame(z=Z_Breaks(nfactor),
                                 col=rep(foo,each=2),stringsAsFactors=FALSE)
        
        fig <- plot_ly(filteredData, 
                       type='choropleth', 
                       locations= ~ISO, 
                       z=filteredData$test, 
                       text=filteredData$Country,
                       colorscale=colorScale,
                       colorbar=list(tickvals=1:nfactor, ticktext=names(foo))
        )
        
        fig
    })
    
    output$plot2 <- renderPlotly({
        
        region_data <- if(input$region != "all"){
            filter(complete_schools_df, region == input$region)
        } else {
            complete_schools_df
        }
        
        plot2 <- ggplot(region_data,
                        aes(x = internet_access,
                            y = fully_closed, 
                            size = total_students,
                            color = vac_priority_status)) +
            geom_point(alpha = 0.4) +
            geom_text(aes(label = country), size = 3) +
            xlim(0.0, 1.0) +
            scale_x_reverse() +
            labs(title = "A Brief Overview of Education Inequality during the Covid-19 Pandemic",
                 x = "Connectivity rates per country",
                 y = "Days of school's full closure") +
            theme_minimal() +
            guides(size = FALSE, scale = "none")
        
    })
    
    output$closures_table <- DT::renderDataTable(closures_table_df,
                                                 options = list(scrollX = TRUE),
                                                 rownames = FALSE)
    
    output$date <- renderText({
        paste("On ", input$month_select)
    })
    
    output$n_schools_closed <- renderText({
        filteredClosed <- filter(closures_df, 
                                 month_year == input$month_select & 
                                     Status == "Closed due to COVID-19")
        
        paste("Countries with schools closed: ", length(unique(filteredClosed$Country)))
    })
    
    output$n_schools_partial <- renderText({
        filteredPartial <- filter(closures_df, 
                                  month_year == input$month_select & 
                                      Status == "Partially open")
        
        paste("Countries with schools partially closed: ",
              length(unique(filteredPartial$Country)))
    })
    
    output$n_schools_open <- renderText({
        filteredOpen <- filter(closures_df, 
                               month_year == input$month_select & 
                                   Status == "Fully open")
        
        paste("Countries with schools fully open: ", length(unique(filteredOpen$Country)))
    })
    
    output$n_schools_break <- renderText({
        filteredBreak <- filter(closures_df, 
                                month_year == input$month_select & 
                                    Status == "Academic break")
        
        paste("Countries on academic break: ", length(unique(filteredBreak$Country)))
    })
    
    output$guide <- renderUI({
        paste("Welcome to our dashboard!")
        rawText <- readLines("Guide.txt")
        splitText <- stringi::stri_split(str = rawText, regex = '\n')
        replacedText <- lapply(splitText, p)
        return(replacedText)
    })
    
}
shinyApp(ui, server)

