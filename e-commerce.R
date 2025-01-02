install.packages(c("shiny", "shinydashboard", "DT"))
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)

# Sample product data
products <- data.frame(
  ID = 1:5,
  Name = c("Laptop", "Smartphone", "Tablet", "Headphones", "Smartwatch"),
  Price = c(1000, 600, 300, 150, 200),
  Description = c("High-performance laptop", "Latest smartphone", "Portable tablet", "Noise-cancelling headphones", "Stylish smartwatch")
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Simple E-Commerce Store"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Shop", tabName = "shop", icon = icon("shopping-cart")),
      menuItem("Cart", tabName = "cart", icon = icon("shopping-basket"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "shop",
              fluidRow(
                column(12, DTOutput("product_table")),
                actionButton("add_to_cart", "Add Selected to Cart")
              )
      ),
      tabItem(tabName = "cart",
              fluidRow(
                column(12, DTOutput("cart_table")),
                h3("Total Price: $", textOutput("total_price"))
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the shopping cart data
  cart <- reactiveVal(data.frame(
    Product = character(),
    Price = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Render product table
  output$product_table <- renderDT({
    datatable(products, selection = "single", options = list(pageLength = 5))
  })
  
  # Handle adding product to cart
  observeEvent(input$add_to_cart, {
    selected_row <- input$product_table_rows_selected
    if(length(selected_row) > 0) {
      selected_product <- products[selected_row, ]
      current_cart <- cart()
      updated_cart <- rbind(current_cart, data.frame(
        Product = selected_product$Name,
        Price = selected_product$Price
      ))
      cart(updated_cart)
    }
  })
  
  # Render shopping cart table
  output$cart_table <- renderDT({
    datatable(cart(), options = list(pageLength = 5))
  })
  
  # Render total price
  output$total_price <- renderText({
    total <- sum(cart()$Price)
    sprintf("%.2f", total)
  })
}

# Run the application
shinyApp(ui, server)

