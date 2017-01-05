library(shiny)
library(shinydashboard)
library(stockPortfolio)
library(PerformanceAnalytics)
library(highcharter)
library(DT)

#### Load Data ####
load("data/etf_data.rda", envir = .GlobalEnv)

#### UI ####
ui <- dashboardPage(title = "ETF Single Index Model",
                    dashboardHeader(title = "ETF Single Index Model", titleWidth = 280),
                    dashboardSidebar(width = 280,
                                     tags$head(tags$script(src = "format_numbers.js")),
                                     sidebarMenu(
                                       menuItem(h4("Dashboard"), tabName = "dashboard"),
                                       selectizeInput("etfs", label = "Select ETFs",
                                                   choices = unique(etfList$ASX_CODE), multiple = TRUE,
                                                   options = list(maxItems = 6,
                                                                  placeholder = 'Select up to 6 ETFs')),
                                       dateRangeInput('dateRange',
                                                      label = "Select date range",
                                                      start = Sys.Date() - 1 - lubridate::years(3), 
                                                      end = Sys.Date() - 1,
                                                      format = "dd/mm/yyyy", weekstart = 1,
                                                      min = "2010-01-04", max = Sys.Date() - 1),
                                       radioButtons("frequency", label = "Select frequency of returns",
                                                    choices = list("Daily" = "day",
                                                                   "Weekly" = "week"), selected = "week"),
                                       numericInput("riskFree", "Risk free rate (Australia Bond 10-Year Yield rate)",
                                                    0.0283, min = 0.0100, max = 0.1000),
                                       numericInput("portfolioValue", "Enter Portfolio Value ($)",
                                                    10000, min = 1000, max = 1000000),
                                       tags$div(HTML("<center>"), actionButton("do", "Run Model")),
                                       menuItem(h4("ETF List"), tabName = "etfList"),
                                       menuItem(h4("Fork on Github"), href = "https://github.com/Maiae/ETF-SIM", newtab = TRUE),
                                       menuItem(h4("About the SIM"), href = "https://en.wikipedia.org/wiki/Single-index_model", newtab = TRUE),
                                       tags$footer(tags$a(href = "http://www.thedatagame.com.au", target = "_blank", HTML("<br><center>"), "© The Data Game"))
                                       )
                                     ),
                                     
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                        fluidRow(
                          box(width = 6, status = "primary", title = "ETF Cumulative Returns", plotOutput("returnsPlot")),
                          box(width = 6, status = "primary", title = "Return and Volatility Plot", highchartOutput("portPlot"))
                          ),
                        fluidRow(
                          box(width = 6, status = "primary", title = "Optimal Portfolio Allocation", DT::dataTableOutput("table"), height = 420),
                          box(width = 6, status = "primary", title = "Optimal Portfolio Weights", highchartOutput("pie"), height = 420)
                          )
                      ),
                      tabItem("etfList", DT::dataTableOutput("etfTable"))
                      )
                    )
                  )

                    
#### SERVER ####
server <- function(input, output) {
  
  # reactive ETF returns table
  etfReturns <- eventReactive(input$do, {
    withProgress(message = "Downloading returns data...", value = 0, {
    returns <- getReturns(ticker = c("^AXJO", sort(input$etfs)), freq = input$frequency,
               start = input$dateRange[1], end = input$dateRange[2])
    return(returns$R)
    })
  })
 
  # cumulative returns chart
  output$returnsPlot <- renderPlot({
    chart.CumReturns(etfReturns(), main = "Cumulative returns including ASX200 index", legend.loc = "topleft")
  })
  
  # run SIM where index is ASX200
  sim <- eventReactive(input$do, {
    withProgress(message = "Running SIM model...", value = 0, {
    riskFree <- ifelse(input$frequency == "week", input$riskFree/52, input$riskFree/250) 
    sim <- stockModel(etfReturns(), Rf = riskFree, shortSelling = "no", model = "SIM", index = 1, freq = input$frequency)
    optSim <- optimalPort(sim)
    })
  })
  
  # Returns and risk plot
  output$portPlot <- renderHighchart({
    optSim <- sim()
    etfData <- t(cbind.data.frame(rbind(round(mean.geometric(etfReturns()), 4), 
                                        round(StdDev(etfReturns()), 4)),
                                        "Optimal Portfolio" = rbind(round(optSim$R, 4), round(optSim$risk, 4))))
    etfData <- cbind.data.frame(Name = row.names(etfData), etfData * 100)
    names(etfData) <- c("Name","Expected Returns","StdDev")
    return(hchart(etfData, "scatter", x = StdDev, y = `Expected Returns`, group = Name, size = 1) %>%
             hc_yAxis(labels = list(format = "{value}%")) %>%
             hc_xAxis(labels = list(format = "{value}%")) %>%
             hc_tooltip(pointFormat = "Volatility: {point.x}% <br> Expected Return: {point.y}%")
          )
  })
  
  # Optimal portfolio weights pie chart
  output$pie <- renderHighchart({
    optSim <- sim()
    portfolioWeights <- data.frame(optSim$X)
    return(highchart() %>%
              hc_add_series_labels_values(row.names(portfolioWeights), (portfolioWeights$optSim.X * 100),
                                          type = "pie", size = 230,
                                          dataLabels = list(enabled = TRUE)) %>% 
              hc_legend(enabled = TRUE) %>% 
              hc_tooltip(valueDecimals = 2, pointFormat = "<b>{point.y}%</b>")
    )
  })
  
  # Optimal Portfolio Allocation table
  portfolioValue <- reactive({as.numeric(gsub(",", "", input$portfolioValue)) })
  
  output$table <- DT::renderDataTable({
    optSim <- sim()
    portfolioAlloc <- cbind.data.frame(names(optSim$X), 
                                       etfList[which(etfList$ASX_CODE %in% names(optSim$X)), 2], 
                                       optSim$X,
                                       optSim$X * portfolioValue())
    names(portfolioAlloc) <- c("ASX code", "ETF name", "Weight", "Value") 
    return(datatable(portfolioAlloc, rownames = FALSE, selection = "none", extensions = "Buttons",
                     options = list(dom = 'tB', buttons = c('copy', 'csv', 'excel'), scrollX = TRUE)) %>% 
                     formatCurrency("Value", '$') %>% 
                     formatPercentage("Weight", 1)
          )
  })
  
  output$etfTable <- DT::renderDataTable({
  	datatable(etfList, selection = "none", options = list(scrollX = TRUE, scrollY = TRUE, pageLength = 25), 
  						caption = 'List of Australian Exchange-Traded Funds (ETFs) traded in the ASX.')
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)