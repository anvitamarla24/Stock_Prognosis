rm(list = ls())
library(shiny)
ulv <<- 1

stockList=read.csv("C:/Users/anvit/OneDrive/Desktop/MyApp/Data/EQUITY_L.csv",header = T)

server <- function(input,output)({
  # to output the structure of the dataset
  
    observeEvent(input$Submit,{
      s = tryCatch(Submit_Form(input$name,input$email,input$mbl,input$usrn,input$rpas),
                    waning = function(w) {print("Registration were not saved ")},
                    error = function(e) {print("Registration were not saved ")},
                   NaN)
      
      output$login <- renderText(
        {ifelse(grepl("login",s[[1]]),print(paste("Registration Details got",s[[1]])),
                print(paste("Registration Details got ",s[[1]])))
          
        })
    })
      
  
    observeEvent(input$Login,{
      
      
      l = tryCatch(Login_Check(as.character(input$usrname),as.character(input$passwd)),
                   waning = function(w) {print("Please verify the User name and password")},
                   error = function(e) {print("Please verify the User name and password")},
                   NaN)
      
      output$login <- renderText(
        {ifelse(grepl("login",l[[1]]),print(paste("your login status is",l[[2]])),
                print(paste("your login status is",l[[1]])))
          
      })
      
      ulv <<- as.numeric(l[[2]])
      
      
    observe(
      {
        if(ulv==1){
              output$Suggestion <- renderText("Your session got expired - Login again to see the results")
            
        }
        
        else {
          
          output$Suggestion <- renderText("You have sucessfully logged in - Select Stock for analysis")
          
          observeEvent(input$btn,suspended = ulv,{
            a=tryCatch(Stock_Pred(input$text,as.character(input$date1),as.character(input$date2),input$date3),
                       waning = function(w) {print("Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")},
                       error = function(e) {print("Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")},
                       NaN)
            
            output$Suggestion <- renderText(
              {ifelse(grepl("Reliable",a[[1]]),print(paste("As per the Timeseries Analysis done on the selected stock",a[[8]],", The investment in this stock for next ",a[[9]], " days is: ",a[[1]])),
                      a[[1]])
              })
            
            output$bt <- renderText("Comparision of the three alogirithm were provide below with backtesting results:")
            
            output$RT_ARI <- renderPlot(
              {ifelse(length(a)==13,print(a[[10]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            
            output$RT_HW <- renderPlot(
              {ifelse(length(a)==13,print(a[[11]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            
            output$RT_ES <- renderPlot(
              {ifelse(length(a)==13,print(a[[12]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            output$comp <- renderDataTable(a[[13]])
            
            output$bt2 <- renderText("By observing above data we observe the predicted results are very much near with ARIMA Modeling")
            
            output$Arima <- renderPlot(
              {ifelse(length(a)==13,plot(a[[2]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            output$Arima2 <- renderText(
              {ifelse(grepl("Reliable",a[[5]]),print(paste("As per the Arima Timeseries Analysis for the stock ",a[[8]],", The investment in this stock for next ",a[[9]], " days is: ",a[[5]])),
                      a[[5]])
              })
            
            output$holt <- renderPlot(
              {ifelse(length(a)==13,plot(a[[3]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            output$holt2 <- renderText(
              {ifelse(grepl("Reliable",a[[6]]),print(paste("As per the Arima Timeseries Analysis for the stock ",a[[8]],", The investment in this stock for Short Term is: ",a[[6]])),
                      a[[6]])
              })
            
            output$ets <- renderPlot(
              {ifelse(length(a)==13,plot(a[[4]]),"Given company name or the time period is not compatible with Quandl data sources. Please try with different options.")
              })
            output$ets2 <- renderText(
              {ifelse(grepl("Reliable",a[[7]]),print(paste("As per the Arima Timeseries Analysis for the stock ",a[[8]],", The investment in this stock for Short Term is: ",a[[7]])),
                      a[[7]])
              })
            
          })
          
        }
      }
    )
    
    })
})

ui <- pageWithSidebar(
  #headerPanel(h4(tags$b("Stock invesetment reliability Prediction"))),
  headerPanel(div(img(src="Stock.jpg",width=1250,height=120),style="text-align: center;")),
  sidebarPanel(
    conditionalPanel(condition="input.tabselected==1",
                     textInput(inputId = "usrname", label = "User Name", placeholder = NULL),
                     passwordInput(inputId = "passwd", label = "Password", value = ""),
                     actionButton("Login","Login"),  
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$h4("New User Registration:"),
                     textInput(inputId = "name", label = "Full Name", placeholder = NULL, width = 400),
                     textInput(inputId = "email", label = "Email Address", placeholder = NULL, width = 400),
                     textInput(inputId = "mbl", label = "Mobile Number", placeholder = NULL, width = 400),
                     textInput(inputId = "usrn", label = "Prefered User Name", placeholder = NULL, width = 400),
                     passwordInput(inputId = "rpas", label = "Enter Password", value = "", width = 400),
                     actionButton("Submit","Submit")
                     ),
    conditionalPanel(condition="input.tabselected==2",h4("
                                                         If you want to build long-term wealth through stock investing and still be able to sleep at night, then consider these points: Invest in stocks of profitable companies that sell goods and services that a growing number of people want. Your stocks will zigzag upward"),
                     tags$br(),
                     tags$b("-- Investement tips for Novice person")),
    
    conditionalPanel(condition="input.tabselected==3",
                     selectInput(inputId="text", label=h4("Select The company"), choices =stockList$NAME.OF.COMPANY, selected = NULL ),
                     tags$h6("Select the Start and End dates for Analysis - Use atleast one year data"),
                     dateInput("date1",h4("Start Date"), value = Sys.Date()-740, max = Sys.Date()),
                     dateInput("date2",h4("End Date"), value = Sys.Date(), max = Sys.Date()),
                     dateInput("date3",h6("Forward forcasting Date"), value = Sys.Date()+10, min = Sys.Date()+2, max = Sys.Date()+50),
                     tags$br(),
                     actionButton("btn","Check for reliability")
                     
    )),
  mainPanel(
    tabsetPanel(
      tabPanel("Login Page", value=1,tags$h5("Enter the login details to get the Strock Market Prediction results, Then proceed to About page"), textOutput("login")),
      tabPanel("About", value=2, 
               helpText(tags$div(tags$b("What is Stock Market?"),
                                 tags$br(),
                                 tags$h4("A stock market, equity market or share market is the aggregation of buyers and sellers (a loose network of economic transactions, not a physical facility or discrete entity) of stocks (also called shares), which represent ownership claims on businesses; these may include securities listed on a public stock exchange as well as those only traded privately."),
                                 tags$br(),
                                 tags$b("What is Stock Prediction?"),
                                 tags$h4("Stock market prediction is the act of trying to determine the future value of a company stock or other financial instrument traded on an exchange. The successful prediction of a stock's future price could yield significant profit."),
                                 tags$br(),
                                 tags$b("How do I predict?"),
                                 tags$h4("Investors are constantly reviewing past pricing history and using it to influence their future investment decisions. Stock market prediction is the act of trying to determine the future value of a company stock. Numerous techniques and methodos are there to help determining this value. Our application deals with technical analysis with some assumptions and suggest short term realiability of stock."),
                                 tags$h4("Navigate to next tab stock Prophecy to get the recommendations"),
                                 tags$br(),tags$br(),
                                 img(src="Question.jpg",width="200",height="100")
               ))),
      tabPanel("Stock Prophecy", value=3,
               tabsetPanel(tabPanel("Suggestion",tags$h5("Initially you have to login in order to see the results"),textOutput("Suggestion"),tags$br(),tags$br(),textOutput("bt"),dataTableOutput("comp"),tags$br(),textOutput("bt2")), 
                           tabPanel("Arima Summary", textOutput("Arima2"), plotOutput("Arima")), 
                           tabPanel("HoltWinters Summary",textOutput("holt2"), plotOutput("holt")),
                           tabPanel("ETS Summary",textOutput("ets2"), plotOutput("ets")))
      ),
      
      id = "tabselected"
    )
  )
  )

#library(gsubfn)
require("highcharter")
require("R2HTML")
require("smooth")
require("Mcomp")
require("Quandl")
require("sweep")
require("tidyverse")
require("tidyquant")
require("forcats")
require("stringr")
require("timetk")
require("ggplot2")
require("reshape2")
require("quantmod")
require("tseries")
require("timeSeries")
require("forecast")
require("xts")
require("qdapTools")

Submit_Form <- function(name, email, phonenum, usrname, passwd){
  
  Reg_data <- list(name, email, phonenum, usrname, passwd)
  write.table(Reg_data,file = "Reg.csv",sep = ",", append = TRUE, col.names = FALSE)
  flag = "Successfully Updated"
  return(list(flag))
}

Login_Check <- function(usrname, passwd){
  
  regd=read.csv("reg.csv",header = T)
  #regd=regd[,c(3,5)]
  
  regd$UserName=tolower(regd$UserName)
  usr=as.character(match(tolower(usrname),regd$UserName))
  passd=as.character(match(tolower(passwd),regd$Password))
  
  
  if( is.na(usr)   &&  is.na(passd)) {
    lf = "Login Un-Successful Try Again"
    x <- 1
  } else {
    lf = "Successfully Logged in"
    x <- 0
  }
  
  return(list(lf,x))
}

Stock_Pred <- function(IND,date1,date2,date3){
  Quandl.api_key("yCypJREUVd6uvZvVqfvC")
  stockList=read.csv("C:/Users/anvit/OneDrive/Desktop/MyApp/Data/EQUITY_L.csv",header = T)
  stockList=stockList[,c(2,1)]
  stockList$NAME.OF.COMPANY=tolower(stockList$NAME.OF.COMPANY)
  sym=as.character(lookup(tolower(IND),stockList))
  IND = Quandl(paste("NSE/",sym,sep=""),collapse="daily",start_date=date1,end_date=date2,type="raw")
  IND$Close.Price <- IND$Close
  IND$Date <- as.Date(IND$Date, "%d-%b-%y")
  IND <- IND[order(IND$Date, decreasing = FALSE),]
  
  Datdif = as.numeric(date3 - Sys.Date())
  
  # Creating a backtesting dataset 
  cnt <-nrow(IND)
  IND2 <- head(IND,cnt-10)
  IND3 <- tail(IND,10)
  
  # Plotting the graphs from the datasets imported
  Close_Price <- IND$Close.Price
  Close_Price2 <- IND2$Close.Price
  Close_price3 <- IND3$Close.Price
  SDate <- IND3$Date
  
  # Removing Data Outliers - Outliers are extreme outcomes (like high stock price) which rarely happens in history, need to remove this for analysis
  stock = ts(Close_Price)
  stock_out <- tsclean(stock)
  outdiff <- stock - stock_out
  stock <- stock_out
  
  # Data for backtesting 
  stock2 = ts(Close_Price2)
  stock_out2 <- tsclean(stock2)
  outdiff2 <- stock2 - stock_out2
  stock2 <- stock_out2
  stock_f2 = ts(na.omit(stock2), frequency=7)
  
  stock_f = ts(na.omit(stock), frequency=7)
  stock_ses.stl = stl(stock_f, s.window="periodic")  
  # Run ARIMA
  Forc <- forecast(stock_ses.stl, method="arima", h=10)
  
  
  #plot_arima <-plot(Forc)    
  forecast <- data.frame(Forc)
  forecast_am <- mean(forecast$Point.Forecast)
  actual_m <- mean(IND$Close.Price[(cnt-10):cnt])
  
  ## BackTesting ARIMA Model
  stock_ses2.stl = stl(stock_f2, s.window="periodic") 
  Forc2 <- forecast(stock_ses2.stl, method="arima", h=10)
  F_ARI <- data.frame(Forc2)
  F_ARI$Actual <- Close_price3
  F_ARI$Date <- SDate
  
  F_ARIG <- melt(F_ARI, id="Date") 
  
  G_ARI <- ggplot(data=F_ARIG, aes(x=Date, y=value,colour=variable),size = 1) +
            geom_line()
  
  G_ARI <- G_ARI + theme(axis.text = element_text(face="bold", size=rel(1))) + ylab("Stock Value")
  
  G_ARI <- G_ARI + labs(title= "ARIMA Backtesting Results", caption = "The above results are compared by considering past 10 days as lag time") +
            theme(plot.title = element_text(colour = "blue", hjust = 0.5))
  
  
  # Run HoltWinters
  forecast_e <- stats::HoltWinters(stock_f)
  forecast_h <- forecast(forecast_e,Datdif)
  #plot_h <-plot(forecast_h)
  f <- data.frame(forecast_h)
  forecast_hm <- mean(f$Point.Forecast)
  
  # Backtesting Holtwinters 
  forecast_e2 <- stats::HoltWinters(stock_f2)
  forecast_h2 <- forecast(forecast_e2,10)
  F_HW <- data.frame(forecast_h2)
  F_HW$Actual <- Close_price3
  F_HW$Date <- SDate

  F_HWG <- melt(F_HW, id="Date") 
  
  G_HW <- ggplot(data=F_HWG, aes(x=Date, y=value,colour=variable),size = 1) +
    geom_line()
  
  G_HW <- G_HW + theme(axis.text = element_text(face="bold", size=rel(1))) + ylab("Stock Value")
  
  G_HW <- G_HW + labs(title= "HOltsWinters Backtesting Results", caption = "The above results are compared by considering past 10 days as lag time") +
    theme(plot.title = element_text(colour = "blue", hjust = 0.5))
  
  
    
  #Run Exponential Smoothing 
  fit <- ets(stock_f)
  forecast_2 <- forecast(fit,Datdif)
  #plot(forecast_2)
  forecast_3=data.frame(forecast_2)
  forecast_ets <- mean(forecast_3$Point.Forecast)
  
  #Backtesting Exponenetial Smoothing
  fit2 <- ets(stock_f2)
  F_ES <- forecast(fit2,10)
  F_ES=data.frame(F_ES)
  F_ES$Actual <- Close_price3
  F_ES$Date <- SDate
  
  F_ESG <- melt(F_ES, id="Date") 
  
  G_ES <- ggplot(data=F_ESG, aes(x=Date, y=value,colour=variable),size = 1) +
          geom_line()
  
  G_ES <- G_ES + theme(axis.text = element_text(face="bold", size=rel(1))) + ylab("Stock Value")
  
  G_ES <- G_ES + labs(title= "Exponential Smoothing Backtesting Results", caption = "The above results are compared by considering past 10 days as lag time") +
    theme(plot.title = element_text(colour = "blue", hjust = 0.5))
  
  
  
  Date <- SDate
  Actual <- Close_price3
  ARIMA_Forecast <- F_ARI$Point.Forecast
  HW_Forecast <- F_HW$Point.Forecast
  ES_Forecast <- F_ES$Point.Forecast
  
  comp <- data.frame(Date,Actual,ARIMA_Forecast,HW_Forecast,ES_Forecast)
  
  
  
  
  if(forecast_am >= actual_m || forecast_hm >= actual_m || forecast_ets >= actual_m){
         flag <- "Reliable"     }
  else { flag <- "Not Reliable" }
  
  if(forecast_am >= actual_m ){ flag_am <- "Reliable"     }
  else { flag_am <- "Not Reliable" }
  
  if(forecast_hm >= actual_m ){ flag_hm <- "Reliable"     }
  else { flag_hm <- "Not Reliable" }
  
  if(forecast_ets >= actual_m){ flag_ets <- "Reliable"     }
  else { flag_ets <- "Not Reliable" }
  
  
  return(list(flag,Forc,forecast_h,forecast_2,flag_am,flag_hm, flag_ets, sym, Datdif, G_ARI, G_HW, G_ES, comp))
}

shinyApp(ui=ui,server = server)
