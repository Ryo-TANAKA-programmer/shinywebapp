
library(httr2)
library(jsonlite)
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(DescTools)
library(plotly)
library(zoo)
library(forecast)
library(randomForest)


# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = h3("My Finance",
                             style = "color: white; font-style: bold;")),
  dashboardSidebar(
    sidebarMenu(
      textInput(inputId = "search_stock",
                label = h4("search stock symbol"),
                value = ""),
      actionButton(inputId = "searchButton",
                   label = h5("Search")),
      selectInput(inputId = "symbol_input",
                  label = h4("Select Symbol"),
                  choices = NULL),
      selectInput(inputId = "metrics", 
                  label = h4("Select Metric"),
                  choices = c("open",
                              "high",
                              "low",
                              "close",
                              "volume")),
      radioButtons(inputId = "movementType", 
                   label = h4("Select Type:"),
                   choices = c("Daily" = "DAILY", "Monthly" = "MONTHLY")),
      actionButton(inputId = "exe", 
                   label = h4("Create a plot")),
      selectInput(inputId = "indic", 
                  label = h4("Select Economic Metric"),
                  choices = c("CPI",
                              "FEDERAL_FUNDS_RATE",
                              "RETAIL_SALES",
                              "DURABLES",
                              "UNEMPLOYMENT",
                              "NONFARM_PAYROLL")),
      actionButton(inputId = "econ", 
                   label = h4("Create Economic Plot")))
  ),
  dashboardBody(
    fluidRow(
      box(title = "Stock Graph",
          background = "black",
          solidHeader = TRUE,
          plotlyOutput(outputId = "plot1",
                       width = "100%",
                       height = "300px")),
      box(title = "Economic Measures",
          background = "black",
          solidHeader = TRUE,
          plotOutput(outputId = "plot4",
                     width = "100%",
                     height = "300px"))
    ),
    tags$hr(),
    tags$p("This stock analysis is powered by R Shiny App and Alpha Vantage API."),
    tags$a("Access API Document", 
           href = "https://www.alphavantage.co/documentation/")
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$searchButton, {
    
    # Perform the API request and update result_new based on the user's input
    stock_tickers_input <- input$search_stock  # Use appropriate input field here
    test_results <- get_test_results(stock_tickers_input)
    
    # Extract stock symbols and update result_new
    stock_symbol_tib <- tibble(stock_symbol = resp_body_json(resp = test_results))
    
    # Call the function by passing the 'stock_symbol_tib' argument
    result <- extract_stock_symbols(stock_symbol_tib = stock_symbol_tib)
    result_new <- print(result[1:length(stock_symbol_tib$stock_symbol$bestMatches)])
    result_new <- unlist(result_new, recursive = FALSE)
    
    updateSelectInput(session = session,
                      inputId = "symbol_input",
                      label = NULL,
                      choices = result_new)
  })
  
  fetched_df <- eventReactive(input$exe, {
    
    
    if (input$exe > 0) {
      fetch_stock_data(movementType = input$movementType,
                       symbol_input = input$symbol_input,
                       metrics = input$metrics)  
    }
    
  })
  
  
  selected_metric <- eventReactive(input$exe, {
    input$metrics
  })
  
  arima_plot_exe <- eventReactive(input$exe, {
    if (input$exe > 0) {
      arima_plot_com(dataframe = fetched_df())
    }
  })
  
  ml_plot_exe <- eventReactive(input$exe, {
    if (input$exe > 0) {
      ml_plot(dataframe = ml_fetched_df())
    }
  })
  
  output$plot1 <- renderPlotly({
    
    req(fetched_df())
    if (!is.null(fetched_df())) {
      p <- ggplot(data = fetched_df(), 
                  mapping = aes(x = date, 
                                y = .data[[selected_metric()]])) +  
        geom_line() +
        labs(x = "Date",
             y = "Price") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  output$plot2 <- renderPlot({
    req(arima_plot_exe())
    
    arima_plot_exe()
  })
  
  ml_fetched_df <- eventReactive(input$exe, {
    
    if (input$exe > 0) {
      ml_fetch_stock (movementType = input$movementType,
                      symbol_input = input$symbol_input)  
    }
  })
  
  output$plot3 <- renderPlot({
    req(ml_plot_exe())
    ml_plot_exe()
  })
  
  # Function to get test results from the API
  get_test_results <- function(stock_tickers_input) {
    
    # user input - users can select tickers and analysis type
    api_key_1 <- Sys.getenv("STOCK_API_KEY_1")
    api_key <- api_key_1
    analysis_type <- "SYMBOL_SEARCH"
    stock_tickers <- stock_tickers_input
    
    # this base url is static
    baseurl <- request("https://www.alphavantage.co/query?")
    
    # feeding input into this code chunk
    test_url <- baseurl %>%
      req_url_query(`function` = analysis_type,
                    keywords = stock_tickers,
                    apikey = api_key_1)
    
    # 200 status check: can be used to validate user input?
    test_results <- req_perform(test_url)
    
    return(test_results)
  }
  
  # Function to extract stock symbols from API response
  extract_stock_symbols <- function(stock_symbol_tib) {
    
    
    # Initialize an empty list to store stock symbols
    stock_symbols <- vector("list", length = length(stock_symbol_tib$stock_symbol$bestMatches))
    
    # Loop through each company and extract the stock symbol
    for (i in seq_along(stock_symbol_tib$stock_symbol$bestMatches)) {
      stock_symbols[[i]] <- stock_symbol_tib$stock_symbol$bestMatches[[i]][['1. symbol']]
    }
    return(stock_symbols)
  }
  
  fetch_stock_data <-  function(movementType,
                                symbol_input,
                                metrics) {
    function_type <- ifelse(movementType == "DAILY", 
                            "TIME_SERIES_DAILY", 
                            "TIME_SERIES_MONTHLY")
    
    baseurl <- request("https://www.alphavantage.co/query?")
    fetch_url <- baseurl %>%
      req_url_query(`function` = function_type,
                    symbol = symbol_input,
                    apikey = api_key_1)
    
    fetch_results <- req_perform(fetch_url)
    
    stock <- tibble(stock_results = resp_body_json(resp = fetch_results)[[2]])
    
    stock_df <- unnest_wider(stock,
                             "stock_results")
    
    stock_df$date <- names(stock$stock_results)
    
    colnames(stock_df) <- c("open","high","low","close","volume","date")
    
    stock_df <- stock_df %>%
      mutate(open = as.numeric(open),
             high = as.numeric(high),
             low = as.numeric(low),
             close = as.numeric(close),
             volume = as.numeric(volume),
             date = as.Date(date))
    
    stock_df <- stock_df %>% 
      select(all_of(metrics),"date")
    
    return(stock_df)
  }
  
  arima_plot_com <- function(dataframe) {
    stock_df_new <- as.data.frame(dataframe)
    stock_df_new <- stock_df_new %>% 
      mutate(logged = log(stock_df_new[1]))
    aur_diff <- diff(stock_df_new$logged,
                     lag = 1)
    aur_diff <- na.locf(object = aur_diff,
                        na.rm = TRUE,
                        fromLast = TRUE)
    set.seed(123)
    arima_model <- auto.arima(stock_df_new$logged)
    forecast_arima <- forecast(arima_model, 
                               h = 10)
    
    forecast_arima$mean <- exp(forecast_arima$mean)
    forecast_arima$lower <- exp(forecast_arima$lower)
    forecast_arima$upper <- exp(forecast_arima$upper)
    
    `stock price` <- ts(stock_df_new$logged)
    arima_plot <- forecast_arima %>% 
      autoplot() + 
      autolayer(exp(`stock price`),
                series = "Past Price") +
      labs(title = "Stock Price Forecast") +
      xlab("Time") +   
      ylab("Stock Price")
    return(arima_plot)
  }
  
  ### ML Function ###
  
  ml_fetch_stock <-  function(movementType,
                              symbol_input) {
    function_type <- ifelse(movementType == "DAILY", 
                            "TIME_SERIES_DAILY", 
                            "TIME_SERIES_MONTHLY")
    
    baseurl <- request("https://www.alphavantage.co/query?")
    fetch_url <- baseurl %>%
      req_url_query(`function` = function_type,
                    symbol = symbol_input,
                    apikey = api_key_1)
    
    fetch_results <- req_perform(fetch_url)
    
    stock <- tibble(stock_results = resp_body_json(resp = fetch_results)[[2]])
    
    stock_df <- unnest_wider(stock,
                             "stock_results")
    
    stock_df$date <- names(stock$stock_results)
    
    colnames(stock_df) <- c("open","high","low","close","volume","date")
    
    stock_df <- stock_df %>%
      mutate(open = as.numeric(open),
             high = as.numeric(high),
             low = as.numeric(low),
             close = as.numeric(close),
             volume = as.numeric(volume),
             date = as.Date(date))
    
    return(stock_df)
  }
  
  ml_plot <- function(dataframe) {
    
    set.seed(123)
    
    samp <- sample(nrow(dataframe), 0.6 * nrow(dataframe))
    train <- dataframe[samp, ]
    test <- dataframe[-samp, ]
    
    model <- randomForest(close ~ .,
                          data = train,
                          ntree = 1000,
                          mtry = 5)
    
    prediction <- predict(model, newdata = test)
    
    results<-cbind(prediction,test$close)
    
    colnames(results) <- c('pred','real')
    
    results <- as.data.frame(results)
    
    diff <- abs(mean(results$pred - results$real))
    
    dist <- quantile(abs(results$pred - results$real))
    
    lowdist <- quantile(abs(results$pred - results$real), 1/3)
    hightdist <- quantile(abs(results$pred - results$real), 2/3)
    
    ml_graph <- ggplot(data = dataframe, 
                       aes(x = date, 
                           y = close)) +
      geom_line() +
      geom_rect(
        xmin = Inf, xmax = max(dataframe$date),  # Right edge of the plot
        ymin = head(dataframe$close, 1) * dist[2], ymax = head(dataframe$close, 1) * dist[4],
        fill = "red",
      ) +
      geom_rect(
        xmin = Inf, xmax = max(dataframe$date),  # Right edge of the plot
        ymin = head(dataframe$close, 1) * lowdist, ymax = head(dataframe$close, 1) * hightdist,
        fill = "blue",
      )
    
    return(ml_graph)
  }
  
  economics <- reactive({
    input$indic
  })
  
  econurl <- ("https://www.alphavantage.co/query?")
  
  output$plot4 <- renderPlot({
    
    req(input$econ)
    
    request(econurl) |>
      req_url_query(
        `function` = economics(),
        interval = "monthly",
        apikey = api_key_1
      ) |>
      req_perform() -> nonfarm
    
    nonfarm
    
    nonfarm1 <- tibble(resp_body_json(nonfarm))
    nonfarm_df <- as.data.frame(nonfarm1[[1]][[4]])
    
    nonfarm_df1 <- gather(nonfarm_df, key = "variable", value = "value", -1)
    nonfarm_df1 <- select(nonfarm_df1, "variable", "value")
    
    nonfarm_df2 <- nonfarm_df1 |>
      separate(variable, into = c("type", "index"), sep = "\\.", extra = "merge") |>
      fill(index)
  
    nonfarm_df3 <- nonfarm_df2 |>
      pivot_wider(names_from = type, 
                  values_from = value) |>
      select(date, value) |>
      mutate(date = if_else(is.na(date), '2023-11-22', date))
    
    nonfarm_df3$date <- as.Date(nonfarm_df3$date)
    nonfarm_df3$value <- as.numeric(nonfarm_df3$value)
    
    nonfarm_df3 %>% 
      ggplot() +
      geom_line(mapping = aes(x = date,
                              y = value)) +
      theme_bw()
  })
}

shinyApp(ui, server)
