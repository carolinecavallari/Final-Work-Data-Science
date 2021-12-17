#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

##Defining shiny dashboard components and publishing

ui <- dashboardPage(
    dashboardHeader(title = "School Closures during the Covid-19 Pandemic"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("World Map and Scatterplot",
                     tabName = "plots_tab",
                     icon = icon("dashboard")),
            menuItem("By Country Table",
                     tabName = "table_tab",
                     icon = icon("dashboard"))
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
                                         "North America ",
                                         "South Asia",
                                         "West and Central Africa",
                                         "Western Europe"
                                     ),
                                     selected = "all"
                                 ))
                    )),
            tabItem(tabName = "table_tab",
                    DT::dataTableOutput("closures_table"))
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
    
}
shinyApp(ui, server)

# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
