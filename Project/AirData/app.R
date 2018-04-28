library(plotly)
library(tidyverse)
library(maps)
library(sp)
library(rgdal)
library(ggplot2)
library(maptools)
library(mapproj)
library(shiny)

testdat <- read.csv("../airdata.csv",stringsAsFactors = F,nrows = 50000)


ui <- fluidPage(
  titlePanel("Air Data"),
  sidebarPanel(
    selectInput("selectyear", 
                label = "Select Year:", 
                choices = as.character(unique(testdat$Year)),
                selected = "1999"),
    
    selectInput("selectpara", 
                label = "Select Parameter:", 
                choices = unique(testdat$Parameter.Name),
                selected = "Ambient Min Temperature")
  ),
  
  mainPanel(
    plotlyOutput("map"),
    plotlyOutput("trend"),
    
    verbatimTextOutput("click")
  )
);

server <- function(input, output,session) {
  output$map <- renderPlotly({
    
    output$map <- renderPlotly({
      
      geo <- list(
        scope = 'usa',
        showland = TRUE,
        landcolor = toRGB("gray95"),
        countrycolor = toRGB("gray80")
      )      
      
      
      dat <- testdat %>% 
        filter(Year==input$selectyear) %>% 
        filter(Parameter.Name==input$selectpara)
      
      county_data <- map_data("county")
      
      para <- dat %>%
        group_by(County.Name) %>%
        summarise(Mean = mean(Arithmetic.Mean))
      
      para$County.Name <- tolower(para$County.Name)
      
      county_para <- merge(county_data, para, by.x = "subregion", by.y = "County.Name")
      
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
          text = ~ subregion,
          hoverinfo = "text") %>% 
        add_polygons(line = list(width = 0.4)) %>%
        add_polygons(
          fillcolor = 'transparent',
          line = list(color = 'grey', width = 0.5),
          showlegend = FALSE,
          hoverinfo = 'none') %>%
        add_annotations( text = testdat$Units.of.Measure[testdat$Parameter.Name == input$selectpara][1],
                         xref = "paper", yref = "paper",
                         x = 1.02, xanchor = "left",
                         y = 0.8, yanchor = "bottom",    
                         legendtitle = TRUE, showarrow = FALSE ) %>%
        layout(title = paste0(input$selectpara, " by County"),
               legend=list(y=0.8, yanchor="top" ),
               geo = geo)
      
      
      ggplotly(p)
      
    })
    
    
    # Trend Plot
    output$trend <- renderPlotly({
      dat <- testdat %>% filter(Parameter.Name==input$selectpara)
      #p <- ggplot(dat)
    })
    
    output$click <- renderPrint({
      d <- event_data("plotly_click")
      
      if (is.null(d) == T) return (NULL);
      
      if (is.null(d)) "Click events appear here (double-click to clear)" else d
    });
  })
};

shinyApp(ui, server, options = list(height=600))