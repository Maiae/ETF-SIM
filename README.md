# [ETF-SIM](https://emaia.shinyapps.io/ETF-SIM/)
A Single Index Model Shiny App for Australian ETFs.  
  
### Project description
https://thedatagame.com.au/2016/12/24/a-single-index-model-shiny-app-for-etfs/   
  
### Running the App locally
```r
packages = c("shiny", "shinydashboard", "stockPortfolio", "PerformanceAnalytics", "highcharter", "DT")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("ETF-SIM", "Maiae")
```  
  
  
![alt text](https://github.com/Maiae/ETF-SIM/blob/master/etf-sim-screenshot.png "ETF-SIM")  
