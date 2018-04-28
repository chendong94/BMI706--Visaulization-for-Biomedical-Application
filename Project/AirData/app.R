library(plotly)
library(tidyverse)
library(maps)
library(ggplot2)
library(shiny)
library(scales)


datfile <- "https://raw.githubusercontent.com/chendong94/BMI706-Visaulization-for-Biomedical-Application/master/Project/airdata.csv"

testdat <- read.csv(datfile,stringsAsFactors = F)


ui <- fluidPage(
  titlePanel("Air Data from 1999 to 2004 from EPA"),
  
  sidebarPanel(
    h2("Welcome"),
    p("You can explore the air data in this Shiny App!
      The data is from ", a("EPA.", href = "https://www.epa.gov/outdoor-air-quality-data") ),
    p("Data preprocess step can be found ", a("here.", href="https://github.com/chendong94/BMI706-Visaulization-for-Biomedical-Application/blob/master/Project/datagen.R")),
    
    h2("How to use"),
    p("First, select the air parameter of insterest. 
      Then, select year to view the parameter quantity across US county in that year.
      If you want to see the trend of the parameter, click Trend tab.
      Then, select the county of interest.
      There is also raw data available to filter and explore under Table tab."),
    
    selectInput("selectpara", 
                label = "Select Parameter:", 
                choices = unique(testdat$Parameter.Name),
                selected = "Ambient Min Temperature"),
    
    selectInput("selectyear", 
                label = "Select Year for Map:", 
                choices = as.character(unique(testdat$Year)),
                selected = "1999"),
    

    selectInput("selectcounty", 
                label = "Select County for Trend:", 
                choices = unique(paste0(testdat$State.Name,",",testdat$County.Name)),
                selected = "Alabama,Clay")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Map", plotlyOutput("map")), 
      tabPanel("Trend", plotlyOutput("trend")),
      tabPanel("Table", dataTableOutput("table"))
    )

  )
);

server <- function(input, output,session) {
  

  

    output$map <- renderPlotly({
      
      geo <- list(
        scope = 'usa',
        showland = TRUE,
        landcolor = toRGB("gray95"),
        countrycolor = toRGB("gray80")
      )      
      
      
      dat <- testdat %>% 
        filter(Year==input$selectyear) %>% 
        filter(Parameter.Name==input$selectpara) %>%
        mutate(loc=paste0(tolower(State.Name),",",tolower(County.Name)))
      
      county_data <- map_data("county") %>% mutate(loc=paste0(region,",",subregion))
      
      para <- dat %>%
        group_by(loc) %>%
        summarise(Mean = mean(Arithmetic.Mean))
      
      county_para <- merge(county_data, para, by.x = "loc", by.y = "loc")
      
      county_para$color <- cut(county_para$Mean,
                               breaks = seq(min(county_para$Mean), 
                                            max(county_para$Mean), 
                                            by = max(county_para$Mean) / 5)
      )
      
      county_para <- county_para %>% arrange(order)
      
      p <- county_para %>%
        group_by(group) %>%
        plot_geo(
          x = ~ long,
          y = ~ lat,
          color = ~ color,
          colors = c('#ffeda0', '#f03b20'),
          text = ~ paste(loc, "<br />", Mean)) %>% 
        add_polygons(line = list(width = 0.4)) %>%
        add_polygons(
          fillcolor = 'transparent',
          line = list(color = 'grey', width = 0.5),
          showlegend = FALSE) %>%
        add_annotations( text = testdat$Units.of.Measure[testdat$Parameter.Name == input$selectpara][1],
                         xref = "paper", yref = "paper",
                         x = 1.02, xanchor = "left",
                         y = 0.8, yanchor = "bottom",    
                         legendtitle = TRUE, showarrow = FALSE ) %>%
        layout(title = paste0(input$selectpara, " by County"),
               legend=list(y=0.8, yanchor="top" ),
               geo = geo)

      
      # ggplotly(p)
      p
    })
    
    
    # Trend Plot
    output$trend <- renderPlotly({
      
      dat <- testdat %>% 
        filter(Parameter.Name==input$selectpara) %>%
        mutate(loc=paste0(tolower(State.Name),",",tolower(County.Name))) %>%
        filter(loc==tolower(input$selectcounty))

      
      base <- dat %>% group_by(Year) %>% summarise(mean=mean(Arithmetic.Mean))

      
      ggplot(base,aes(x = Year, y = mean)) + 
        geom_point(aes(size=0.2)) + 
        geom_smooth(method=lm, se=FALSE) +
        scale_x_continuous(breaks= pretty_breaks(length(base$Year)-1)) +
        labs(title=paste0("Trend of ",input$selectpara)) +
        ylab(paste0("Yearly Mean/",testdat$Units.of.Measure[testdat$Parameter.Name == input$selectpara][1])) +
        theme_bw(base_size = 12)
    })
    

  output$table <- renderDataTable({
    testdat %>% 
      filter(Year==input$selectyear) %>% 
      filter(Parameter.Name==input$selectpara) %>%
      mutate(Location=paste0(tolower(State.Name),",",tolower(County.Name))) %>%
      filter(Location==tolower(input$selectcounty))
  })
};

shinyApp(ui, server, options = list(height=600))