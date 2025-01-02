library(shiny)
# Define UI for the application
ui <- fluidPage(
  titlePanel("Restaurant Order and Table Booking System"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Order System"),
      selectInput("item", "Select Item:", 
                  choices = c("Burger", "Pizza", "Pasta", "Salad")),
      numericInput("quantity", "Enter Quantity:", value = 1, min = 1),
      actionButton("add_order", "Add to Order"),
      br(),
      actionButton("calculate", "Calculate Bill"),
      br(), br(),
      h3("Table Booking System"),
      numericInput("table_number", "Enter Table Number:", value = 1, min = 1, max = 10),
      actionButton("book_table", "Book Table")
    ),
    
    mainPanel(
      h3("Your Order"),
      tableOutput("order_table"),
      textOutput("total_bill"),
      h3("Table Availability"),
      tableOutput("table_status")
    )
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  # Define the menu
  menu <- data.frame(
    item = c("Burger", "Pizza", "Pasta", "Salad"),
    price = c(150, 200, 180, 120)
  )
  
  # Define available tables
  tables <- data.frame(
    table_number = 1:10,
    available = rep(TRUE, 10)
  )
  
  # Initialize an empty order list
  order <- reactiveVal(list())
  
  # Function to add items to the order
  observeEvent(input$add_order, {
    current_order <- order()
    item <- input$item
    quantity <- input$quantity
    if (item %in% names(current_order)) {
      current_order[[item]] <- current_order[[item]] + quantity
    } else {
      current_order[[item]] <- quantity
    }
    order(current_order)
  })
  
  # Function to calculate the total bill
  output$total_bill <- renderText({
    input$calculate
    current_order <- order()
    total <- 0
    for (item in names(current_order)) {
      price <- menu$price[menu$item == item]
      total <- total + price * current_order[[item]]
    }
    paste("Total Bill: Rs", total)
  })
  
  # Display the order
  output$order_table <- renderTable({
    current_order <- order()
    order_df <- data.frame(
      Item = names(current_order),
      Quantity = unlist(current_order)
    )
    order_df
  })
  
  # Function to book a table
  observeEvent(input$book_table, {
    table_number <- input$table_number
    if (table_number %in% tables$table_number && tables$available[table_number]) {
      tables$available[table_number] <- FALSE
      showModal(modalDialog(
        title = "Success",
        paste("Table", table_number, "booked successfully")
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        paste("Table", table_number, "is not available")
      ))
    }
    output$table_status <- renderTable({
      tables
    })
  })
  
  # Display table availability
  output$table_status <- renderTable({
    tables
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


