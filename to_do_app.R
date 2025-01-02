install.packages("shiny")
install.packages("DT")
library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("To-Do List Application"),
  sidebarLayout(
    sidebarPanel(
      textInput("task", "Enter new task:"),
      actionButton("addTask", "Add Task"),
      br(),
      br(),
      DTOutput("taskTable")
    ),
    mainPanel(
      h3("Task Details"),
      textOutput("selectedTask"),
      actionButton("markCompleted", "Mark as Completed"),
      textInput("reminder", "Set Reminder (in minutes):"),
      actionButton("setReminder", "Set Reminder")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Create reactive values to store tasks
  tasks <- reactiveVal(data.frame(
    Task = character(),
    Status = character(),
    stringsAsFactors = FALSE
  ))
  
  # Add new task
  observeEvent(input$addTask, {
    new_task <- data.frame(Task = input$task, Status = "Pending", stringsAsFactors = FALSE)
    tasks(rbind(tasks(), new_task))
  })
  
  # Render task table
  output$taskTable <- renderDT({
    datatable(tasks(), selection = "single", rownames = FALSE)
  })
  
  # Display selected task details
  output$selectedTask <- renderText({
    selected_row <- input$taskTable_rows_selected
    if (length(selected_row)) {
      paste("Task:", tasks()[selected_row, "Task"], "\nStatus:", tasks()[selected_row, "Status"])
    } else {
      "Select a task to see details."
    }
  })
  
  # Mark task as completed
  observeEvent(input$markCompleted, {
    selected_row <- input$taskTable_rows_selected
    if (length(selected_row)) {
      task_data <- tasks()
      task_data[selected_row, "Status"] <- "Completed"
      tasks(task_data)
    }
  })
  
  # Set reminder for selected task
  observeEvent(input$setReminder, {
    selected_row <- input$taskTable_rows_selected
    if (length(selected_row)) {
      task_data <- tasks()
      reminder_time <- as.numeric(input$reminder) * 60
      invalidateLater(reminder_time * 1000, session)
      task_data[selected_row, "Status"] <- paste("Reminder set for", input$reminder, "minutes")
      tasks(task_data)
      showNotification(paste("Reminder for task:", task_data[selected_row, "Task"]), duration = reminder_time, type = "message")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
