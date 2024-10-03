library(shiny)
library(data.table)
library(caret)
library(ranger)

load("credit_card_fraud_model.rda")

# Define UI
ui <- fluidPage(
  titlePanel("Credit Card Fraud Detection"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Transaction Data (CSV)",
        accept = c(".csv")
      ),
      actionButton("predict", "Predict Fraud")
    ),
    mainPanel(
      tableOutput("predictions"),
      textOutput("status") # Output to show prediction status
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    req(input$file1) # Ensure a file is uploaded

    # Read the uploaded CSV file
    transaction_data <- fread(input$file1$datapath)

    # Ensure the required columns are present
    required_cols <- c(
      "Time", "V1", "V2", "V3", "V4", "V5", "V6", "V7",
      "V8", "V9", "V10", "V11", "V12", "V13", "V14",
      "V15", "V16", "V17", "V18", "V19", "V20", "V21",
      "V22", "V23", "V24", "V25", "V26", "V27", "V28",
      "Amount"
    )

    if (!all(required_cols %in% colnames(transaction_data))) {
      output$status <- renderText("Error: Uploaded data is missing required columns.")
      return()
    }

    # Pre-process the data if necessary (scaling, etc.)
    transaction_data$Amount <- scale(transaction_data$Amount)

    # Predicting using the loaded model
    predictions <- predict(model_gbm, transaction_data)

    # Define a threshold for classifying fraud
    threshold <- 0.5
    predicted_classes <- ifelse(predictions > threshold, "Fraudulent", "Not Fraudulent")

    # Convert predictions to a data frame for better visibility
    predictions_df <- data.frame(
      Transaction = 1:nrow(transaction_data),
      Fraudulent = predicted_classes
    )

    # Display predictions
    output$predictions <- renderTable({
      predictions_df
    })

    # Show status message
    output$status <- renderText({
      paste("Predictions completed for", nrow(transaction_data), "transactions.")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
