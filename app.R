library(shiny)
library(tidyverse)
library(readxl)
library(maps)
library(ggplot2)
library(rvest)
library(ggrepel)
library(broom)
library(rsconnect)
options(scipen = 999)

source("outbreak.r")
source("plots.r")

ui <- navbarPage(
    "COVID-19 Analysis",
    tabPanel("Mapping", 
             fluidPage(
                 titlePanel("Mapping the outbreak"),
                 sidebarLayout(
                     sidebarPanel = selectInput(
                         "state_choice",
                         "Choose state",
                         c("All states") %>%
                             append(state.name)
                     ),
                     mainPanel(plotOutput("map_plot"))
                 )
             )),
    tabPanel("Visualisations",
             fluidPage(
                 titlePanel("Examining second order effects of COVID-19"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Choose Second Order Effect",
                             c("Jobless Claims" = "job", "Air Traffic" = "air",
                               "Restaurants" = "res", "Search Interest" = "search")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Model",
             tabsetPanel(
                 tabPanel("Approval", 
                          p("We first plot President Trump's approval ratings
                            since February, when the Coronavirus started spreading
                            in the U.S. As we can see, approval ratings spiked in mid march 
                            and plumetted consistently thereafter.
                            "),
                          img(src = "b1.png", height=500, width=700),
                          p("Now, we plot a linear regression of President Trump's
                            approval ratings against the number of COVID-19 infections
                            in the U.S. The result is a weakly positive correlation, 
                            although this might be attributed to linear regression
                            being an imperfect tool given the disease's exponential spread."),
                          img(src = "b2.png", height=500, width=700),
                          p("Linear regression: disapproval rating vs total infections"),
                          img(src = "b3.png", height=500, width=700)),
                 tabPanel("Coronavirus", 
                          p("We now look at President Trump's approval ratings specifically
                            for his handling of the coronavirus outbreak"),
                          img(src = "a1.png", height=500, width=700),
                          p("Linear regression: Coronavirus approval rating vs total infections"),
                          img(src = "a2.png", height=500, width=700),
                          p("Linear regression: Coronavirus disapproval rating vs total infections"),
                          img(src = "a3.png", height=500, width=700)),
                 tabPanel("Infection", 
                          p("We now look at the level of concern among American residents about getting
                            infected by the coronavirus"),
                          img(src = "d1.png", height=500, width=700),
                          p("Linear regression: percentage of Americans 'very' concerned about infection vs total infections"),
                          img(src = "d2.png", height=500, width=700),
                          p("Linear regression: percentage of Americans 'not at all' concerned about infection vs total infections"),
                          img(src = "d3.png", height=500, width=700)),
                 tabPanel("Economy", 
                          p("We now look at the level of concern among American residents about the 
                            state of the economy due to the pandemic."),
                          img(src = "c1.png", height=500, width=700),
                          p("Linear regression: percentage of Americans 'very' concerned about economy vs total infections"),
                          img(src = "c2.png", height=500, width=700),
                          p("Linear regression: percentage of Americans 'not at all' concerned about economy vs total infections"),
                          img(src = "c3.png", height=500, width=700))
             )),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My project involves the analysis of COVID-19, the disease that 
               has literally taken over all our lives. I was particularly interested
               in three things: mapping the rapid spread of the coronavirus across 
               the U.S, measuring the second order impacts of the disease, due to 
               lockdowns and disruptions, and analysing the change in public opinion
               over the course of the disease."),
             p("Data has been gatheref from multiple sources. Mapping data is from 
               The New York Times, along with some county geographical sourced from 
               Wikipedia. Approval ratings for President Trump are from FiveThirtyEight.
               Restaurant bookins data has been sourced from OpenTable; flight data from 
               FlightRadar24; unemployment insurance data from the Department of Labor, and
               search data from Google Trends. "),
             h3("About Me"),
             p("My name is Shreyvardhan Sharma and I am a sophomore studying Computer Science and Mathematics at Harvard. 
             You can reach me at sharma_s@college.harvard.edu.")))

server <- function(input, output) {
    output$line_plot <- renderPlot({
        
        data <- switch(input$plot_type, 
                       "job" = claims,
                       "air" = flights,
                       "res" = res,
                       "search" = search)
        
        aes <- switch(input$plot_type,
                      "job" = aes(x = date, y = sa),
                      "air" = aes(x = date, y = flights),
                      "res" = aes(x = date, y = bookings, color=country),
                      "search" = aes(x = date, y = search))
        
        # Draw the histogram with the specified number of bins
        
        ggplot(data, aes) + geom_line() 
    })
    
    output$map_plot <- renderPlot({
        
        ifelse(
            input$state_choice == "All states",
            
            selectedData <- state_comb,
            
            selectedData <- comb %>%
                filter(state == input$state_choice)
        )
        
        ifelse(
            input$state_choice == "All states",
            
            mapData <- map_data("state"),
            
            mapData <- map_data("county", input$state_choice)
        )
        
        mapData %>%
            ggplot(aes(x = long, y = lat, group = group)) +
            geom_polygon(fill = "#f1f1f1", color = "#dddddd") + 
            theme_void() + 
            geom_point(aes(x = long, y = lat, size = cases), 
                       data=selectedData, inherit.aes = FALSE, color="red", alpha = 0.5) +
            geom_label_repel(aes(label=cases, group=NULL), 
                             data=selectedData, size=4) + 
            scale_size_continuous(range=c(10, 19))
        
    })
}

shinyApp(ui = ui, server = server)