library(shiny)
library(shinydashboard)
library(tidyquant)
library(stockPortfolio)
library(PerformanceAnalytics)
library(highcharter)
library(DT)
options(scipen=999)  # turn off scientific notation

#### Load Data ####
load("data/etf_data.rda", envir = .GlobalEnv)
# load("data/nasdaq_data.rda", envir = .GlobalEnv) do not load NASDAQ stocks


#### UI ####
ui <- dashboardPage(title = "ETF Single Index Model",
                    dashboardHeader(title = "ETF Single Index Model", titleWidth = 280),
                    dashboardSidebar(width = 280,
                                     tags$head(tags$script(src = "format_numbers.js")),
                                     sidebarMenu(
                                       menuItem(h4("Dashboard"), tabName = "dashboard"),
                                       selectizeInput("etfs", label = "Select ETFs",
                                                   choices = unique(etfList$ASX_CODE),
                                                   						multiple = TRUE,
                                                   options = list(maxItems = 6,
                                                                  placeholder = 'Select up to 6 Securities')),
                                       dateRangeInput('dateRange',
                                                      label = "Select date range",
                                                      start = Sys.Date() - 1 - lubridate::years(1), 
                                                      end = Sys.Date() - 1,
                                                      format = "dd/mm/yyyy", weekstart = 1,
                                                      min = "2010-01-04", max = Sys.Date() - 1),
                                       radioButtons("frequency", label = "Select frequency of returns",
                                                    choices = list("Daily" = "daily",
                                                                   "Weekly" = "weekly"), selected = "weekly"),
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
                          box(width = 6, status = "primary", title = "ETF Cumulative Returns", highchartOutput("returnsPlot")),
                          box(width = 6, status = "primary", title = "Return and Volatility Plot", highchartOutput("portPlot"))
                          ),
                        fluidRow(
                          box(width = 6, status = "primary", title = "Optimal Portfolio Allocation", DT::dataTableOutput("table"), height = 420),
                          box(width = 6, status = "primary", title = "Optimal Portfolio Growth", highchartOutput("portfolioGrowth"), height = 420)
                          )
                      ),
                      tabItem("etfList", DT::dataTableOutput("etfTable"))
                      )
                    )
                  )

                    
#### SERVER ####
server <- function(input, output) {
  
  # reactive ETF returns table
  etfData <- eventReactive(input$do, {
    withProgress(message = "Downloading returns data...", value = 0, {
    returns <- c("^AXJO", sort(input$etfs)) %>%
      tq_get(get  = "stock.prices",
             from = as.Date(input$dateRange[1]),
             to   = as.Date(input$dateRange[2])) %>%
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = input$frequency, 
                   col_rename = "R") %>% 
      mutate(ReturnsCumulative = cumsum(R))
    
    returns$symbol <- sub("[[:punct:]]AXJO", "ASX200 Index", returns$symbol)
    returns$symbol <- ordered(returns$symbol, c("ASX200 Index", sort(input$etfs)))
    
    return(returns)
    })
  })
 
  # cumulative returns chart
  output$returnsPlot <- renderHighchart({
    # color palette vector
  	colorPal <- c("#000000", "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#17BECF")
    # Cumulative Returns highcharter
    returnsPlot <- hchart(etfData(), "line", hcaes(x = date, y = (ReturnsCumulative * 100), group = symbol),
                           color = colorPal[1:length(unique(etfData()$symbol))]) %>%
                      hc_yAxis(title = list(text = "Cumulative Returns"),
                               labels = list(format = "{value}%")) %>% 
                      hc_tooltip(pointFormat = "{point.y}%", valueDecimals = 2) %>%
                      hc_exporting(enabled = TRUE, filename = "cumulative_returns")
    
    return(returnsPlot)
  })
  
  # run SIM where index is ASX200
  sim <- eventReactive(input$do, {
    withProgress(message = "Running SIM model...", value = 0, {
    
    riskFree <- ifelse(input$frequency == "week", input$riskFree/52, input$riskFree/250)
    
    returnsModel <- etfData() %>% 
    	select(-ReturnsCumulative) %>% 
    	spread(., symbol, R) %>% 
    	select(-date) %>% 
    	as.matrix()
    
    sim <- stockModel(na.omit(returnsModel), Rf = riskFree, shortSelling = "no", model = "SIM", index = 1, freq = "week")
    optSim <- optimalPort(sim)
    return(optSim)
    })
  })
  
  # Returns and risk plot
  output$portPlot <- renderHighchart({
  	# Risk Return table
  	risk_return <- etfData() %>% 
  		group_by(symbol) %>% 
  		summarise(MeanReturns = mean.geometric(R),
  							Sd = StdDev(R)) %>% 
  		select(symbol, MeanReturns, Sd) %>% 
  		rbind.data.frame(cbind.data.frame(symbol = "SIM Portfolio", 
  																			MeanReturns = as.numeric(sim()$R), 
  																			Sd = as.numeric(sim()$risk))) %>% 
  		mutate(symbol = ordered(symbol, symbol))
  	
  	# color palette vector
  	colorPal <- c("#000000", "#1F77B4", "#FF7F0E", "#2CA02C", "#C21A01", "#9D2053", "#774F38", "#17BECF", "#BCBD22")
  	
  	# Scatter Plot pf Risk and Returns
    return(hchart(risk_return, "scatter",
    							hcaes(x = round((Sd * 100), 3), y = round((MeanReturns * 100), 3), group = symbol, size = 1),
    							color = colorPal[1:nrow(risk_return)]) %>%
    			 	hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Expected Returns")) %>%
    			 	hc_xAxis(labels = list(format = "{value}%"), title = list(text = "Volatility")) %>%
    			 	hc_tooltip(pointFormat = "Volatility: {point.y}% <br> Expected Return: {point.x}%") %>% 
    			 	hc_exporting(enabled = TRUE, filename = "risk_return")
          )
  })
  

  # Portfolio growth line chart
  output$portfolioGrowth <- renderHighchart({
  	# Color palette
  	colorPal <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#17BECF")
  	# Portfolio growth chart
  	etfData() %>%
  		tq_portfolio(assets_col   = symbol, 
  								 returns_col  = R, 
  							   weights      = c(0, sim()$X), 
  							   col_rename   = "investment_growth",
  							   wealth.index = TRUE) %>%
  		mutate(investment_growth = investment_growth * portfolioValue()) %>% 
  		hchart(., "line", hcaes(x = date, y = investment_growth), color = colorPal[length(input$etfs) + 1]) %>%
  		hc_yAxis(title = list(text = "Investment Growth"),
  						 labels = list(format = "${value}")) %>% 
  		hc_tooltip(pointFormat = "${point.y}", valueDecimals = 2) %>%
  		hc_exporting(enabled = TRUE, filename = "Investment_Growth") 
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