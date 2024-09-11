# Import necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the dataset (assuming it's already downloaded)
# shots_stats_goal.df <- readRDS("~/Hockey_Fall_2023/StudentData/shots_stats_goal.df.Rds")

# Define the user interface of the app
ui <- fluidPage(
  titlePanel("Feature Explore by Shiny App"),  # Title of the app
  
  # Layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting X-axis feature
      selectInput("x_feature", "Select X-axis Feature:", choices = colnames(shots_stats_goal.df)),
      
      # Dropdown for Y-axis feature, shown only for scatter plots
      conditionalPanel(
        condition = "input.plot_type == 'Scatter Plot'",
        selectInput("y_feature", "Select Y-axis Feature:", choices = colnames(shots_stats_goal.df))
      ),
      
      # Dropdown to select the type of plot
      selectInput("plot_type", "Select Plot Type:", choices = c("Histogram", "Density Plot", "Scatter Plot")),
      
      # Dropdown for selecting outcome, shown only for histograms
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        selectInput("selected_outcome", "Select Outcome:", choices = c("All Outcomes", as.character(unique(shots_stats_goal.df$shotOutcome))))
      )
    ),
    
    # Main panel to display the plot
    mainPanel(
      plotlyOutput("dynamicPlot")
    )
  )
)

# Server logic of the Shiny app
server <- function(input, output, session) {
  
  # Output for the dynamic plot
  output$dynamicPlot <- renderPlotly({
    # Return NULL if no X feature is selected
    if (input$x_feature == "") return(NULL)
    
    # Title for the plot
    plot_title <- paste(input$plot_type, "of", input$x_feature)
    
    # Custom colors for different shot outcomes
    custom_colors <- c("Defender Block" = "#54a838", "Miss" = "#15616d", "Save" = "#ff7d00", "Goal" = "#b71515")
    
    # Logic for generating histogram
    if (input$plot_type == "Histogram") {
      data_filtered <- shots_stats_goal.df
      if (input$selected_outcome != "All Outcomes") {
        data_filtered <- data_filtered %>%
          filter(shotOutcome == input$selected_outcome)
      }
      data_filtered$shotOutcome <- as.factor(data_filtered$shotOutcome)
      
      # Creating the histogram
      p <- ggplot(data_filtered, aes_string(x = input$x_feature, fill = "shotOutcome")) +
        geom_histogram(bins = 30, position = "identity") +
        labs(title = plot_title, x = input$x_feature) +
        theme_minimal()
      
      p <- p + scale_fill_manual(values = custom_colors)
      ggplotly(p)  # Convert ggplot to interactive plotly plot
      
      # Logic for generating density plot
    } else if (input$plot_type == "Density Plot") {
      p <- ggplot(shots_stats_goal.df, aes_string(x = input$x_feature)) +
        geom_density(color = "black", size = 1, linetype = "dashed", show.legend = TRUE) +
        labs(title = plot_title, x = input$x_feature)
      
      p <- p + geom_density(aes_string(x = input$x_feature, fill = "shotOutcome"), alpha = 0.7) +
        theme_minimal()
      
      p <- p + scale_fill_manual(values = custom_colors)
      ggplotly(p)  # Convert ggplot to interactive plotly plot
      
      # Logic for generating scatter plot
    } else if (input$plot_type == "Scatter Plot") {
      p <- ggplot(shots_stats_goal.df, aes_string(x = input$x_feature, y = input$y_feature, color = "shotOutcome")) +
        geom_point() +
        labs(title = plot_title, x = input$x_feature, y = input$y_feature) +
        theme_minimal()
      
      p <- p + scale_color_manual(values = custom_colors)
      
      ggplotly(p)  # Convert ggplot to interactive plotly plot
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)