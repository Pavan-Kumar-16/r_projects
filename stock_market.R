# Install and load necessary packages
install.packages("shiny")
install.packages("quantmod")
install.packages("forecast")
install.packages("caret")
install.packages("e1071")

library(shiny)
library(quantmod)
library(forecast)
library(caret)
library(e1071)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Stock Market Price Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Choose a Stock Ticker:", choices = c("AAPL", "GOOG", "MSFT")),
      dateRangeInput("date_range", "Date Range:", start = "2010-01-01", end = Sys.Date()),
      actionButton("predict", "Predict Prices")
    ),
    mainPanel(
      plotOutput("pricePlot"),
      plotOutput("arimaPlot"),
      plotOutput("svmPlot"),
      verbatimTextOutput("performanceMetrics")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  stock_data <- reactive({
    req(input$ticker)
    data <- tryCatch({
      getSymbols(input$ticker, src = "yahoo", from = input$date_range[1], to = input$date_range[2], auto.assign = FALSE)
    }, error = function(e) {
      NULL
    })
    if (is.null(data) || nrow(data) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Failed to fetch stock data. Please check the ticker symbol and date range.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    data
  })
  
  predictions <- eventReactive(input$predict, {
    data <- Cl(stock_data())
    if (is.null(data) || anyNA(data)) {
      showModal(modalDialog(
        title = "Error",
        "Insufficient or missing data for prediction.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    data <- na.omit(data)
    train_size <- floor(0.8 * length(data))
    train_data <- data[1:train_size]
    test_data <- data[(train_size + 1):length(data)]
    
    if (length(train_data) < 2 || length(test_data) < 2) {
      showModal(modalDialog(
        title = "Error",
        "Not enough data to train or test the model.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # ARIMA Model
    arima_model <- auto.arima(train_data)
    arima_forecast <- forecast(arima_model, h = length(test_data))
    
    # SVM Model
    train_features <- as.data.frame(as.numeric(index(train_data)))
    test_features <- as.data.frame(as.numeric(index(test_data)))
    train_labels <- as.numeric(train_data)
    test_labels <- as.numeric(test_data)
    
    if (!is.numeric(train_features[,1]) || !is.numeric(test_features[,1])) {
      showModal(modalDialog(
        title = "Error",
        "SVM features must be numeric.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    svm_model <- tryCatch({
      train(train_features, train_labels, method = "svmRadial")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to train SVM model:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
    
    if (is.null(svm_model)) return(NULL)
    svm_predictions <- predict(svm_model, test_features)
    
    list(
      test_data = test_data,
      arima_forecast = arima_forecast,
      svm_predictions = svm_predictions,
      arima_mae = mean(abs(arima_forecast$mean - test_data)),
      arima_rmse = sqrt(mean((arima_forecast$mean - test_data)^2)),
      svm_mae = mean(abs(svm_predictions - test_labels)),
      svm_rmse = sqrt(mean((svm_predictions - test_labels)^2))
    )
  })
  
  output$pricePlot <- renderPlot({
    data <- stock_data()
    if (!is.null(data)) {
      plot(Cl(data), main = paste("Stock Prices for", input$ticker), col = "blue")
    }
  })
  
  output$arimaPlot <- renderPlot({
    preds <- predictions()
    if (!is.null(preds)) {
      plot(preds$arima_forecast, main = "ARIMA Forecast", col = "red")
      lines(preds$test_data, col = "blue")
    }
  })
  
  output$svmPlot <- renderPlot({
    preds <- predictions()
    if (!is.null(preds)) {
      plot(preds$test_data, type = "l", col = "blue", main = "SVM Predictions vs Actual")
      lines(preds$svm_predictions, col = "red")
    }
  })
  
  output$performanceMetrics <- renderPrint({
    preds <- predictions()
    if (!is.null(preds)) {
      cat("ARIMA Model MAE:", preds$arima_mae, "\n")
      cat("ARIMA Model RMSE:", preds$arima_rmse, "\n")
      cat("SVM Model MAE:", preds$svm_mae, "\n")
      cat("SVM Model RMSE:", preds$svm_rmse, "\n")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

