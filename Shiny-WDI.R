# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(googleCharts)
# library(devtools)
# library(data.table)
# library(bit64)
# library(plyr)
# library(dplyr)
WDI2 = fread("WDI2.csv")
#WDI2 = WDI2 %>% filter(!Year %in% c(2018, 2019))


WDI2$`GDP(CurrentUS$) in billion dollars` = WDI2$`GDP(CurrentUS$)`/1000000000

devtools::install_github("jcheng5/googleCharts")

ui = fluidPage(
  
  sliderInput("year", "Pick a year :",
              min = round(min(WDI2$Year)), max = round(max(WDI2$Year)),
              value = min(WDI2$Year), animate = TRUE),
  googleChartsInit(),
  
  selectInput("Choice1", "Choose the X-axis",
              choices = c("GDP(CurrentUS$) in billion dollars", "GDP(CurrentUS$) in billion dollars",
                          "CO2.Emissions(per.capita)", "CO2.Emissions(per.capita)",
                          "Life.Expectancy", "Life.Expectancy",
                          "GNI.per.capita(CurrentUS$)", "GNI.per.capita(CurrentUS$)",
                          "Primary.school.enrollment(%.of.Gross)", "Primary.school.enrollment(%.of.Gross)")),
  
  selectInput("Choice2", "Choose the Y-axis",
              choices = c("GDP(CurrentUS$) in billion dollars", "GDP(CurrentUS$) in billion dollars",
                          "CO2.Emissions(per.capita)", "CO2.Emissions(per.capita)",
                          "Life.Expectancy", "Life.Expectancy",
                          "GNI.per.capita(CurrentUS$)", "GNI.per.capita(CurrentUS$)",
                          "Primary.school.enrollment(%.of.Gross)", "Primary.school.enrollment(%.of.Gross)")),
  
  googleBubbleChart(
    "chart", width = "100%", height = "475px",
    options = list(
      fontName = "Source Sans Pro",
      fontSize = 13,
      hAxis = list(
        title = "Health expenditure, per capita ($USD)"
      ),
      vAxis = list(
        title = "Life expectancy (years)"
      )
    )
  )
)

server = function(input, output) {
  
  defaultColors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69")
  series2 <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(WDI2$Region)
  )
  
  yearData <- reactive({
    df <- WDI2 %>%
      filter(WDI2$Year == input$year) 
    df = df[,c("Country", input$Choice1, input$Choice2,"Region", "Population")]
      })
  
  output$chart <- reactive({
    
    xaxis = WDI2 %>% select(input$Choice1);
    
    yaxis = WDI2 %>% select(input$Choice2)
    
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = paste(input$Choice1, "Vs. ", input$Choice2, " ",
          input$year),
        series = series2,
        hAxis = list(
          title = input$Choice1,
          viewWindow = list(
            min = min(xaxis) - 500,
            max = max(xaxis) + 500
          )
        ),
        vAxis = list(
          title = input$Choice2,
          viewWindow = list(
            min = min(yaxis) - 40,
            max = max(yaxis) + 40
          )
        )
      )
    )
  })
}



shinyApp(ui, server)


