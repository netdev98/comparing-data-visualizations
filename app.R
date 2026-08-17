# Iris Data Explorer - Shiny App
# This app creates an interactive dashboard using the built-in Iris dataset.
# It includes three visualizations and interactive filtering by species.

# Load required libraries
library(shiny)
library(ggplot2)

# ============================================================================
# UI - Defines the layout and appearance of the app
# ============================================================================
ui <- fluidPage(
  
  # App title and styling
  titlePanel("Iris Dataset Explorer Dashboard"),
  
  # Sidebar layout with interactive controls on the left, plots on the right
  sidebarLayout(
    
    # --------------------------------------------------------------------------
    # Sidebar Panel - Contains the interactive controls
    # --------------------------------------------------------------------------
    sidebarPanel(
      width = 3,
      
      # Interactive Function: Species selector
      # Allows users to filter which species are displayed across all three plots
      checkboxGroupInput(
        inputId = "species",
        label = "Select Species to Display:",
        choices = levels(iris$Species),
        selected = levels(iris$Species)
      ),
      
      # Interactive Function: Variable selectors for the scatter plot
      # Users can choose which measurements to compare on the X and Y axes
      selectInput(
        inputId = "x_var",
        label = "Scatter Plot X-Axis:",
        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        selected = "Sepal.Length"
      ),
      
      selectInput(
        inputId = "y_var",
        label = "Scatter Plot Y-Axis:",
        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        selected = "Petal.Length"
      ),
      
      # Interactive Function: Bin count slider for the histogram
      # Users can adjust the number of bins to see different levels of detail
      sliderInput(
        inputId = "bins",
        label = "Histogram Bins:",
        min = 5,
        max = 30,
        value = 15
      ),
      
      hr(),
      
      # Display a summary of the filtered data
      h4("Filtered Data Summary"),
      verbatimTextOutput("dataSummary")
    ),
    
    # --------------------------------------------------------------------------
    # Main Panel - Contains the three visualizations
    # --------------------------------------------------------------------------
    mainPanel(
      width = 9,
      
      # Visualization 1: Scatter plot (top, full width)
      h3("1. Scatter Plot - Measurement Comparison"),
      plotOutput("scatterPlot", height = "350px"),
      
      hr(),
      
      # Visualizations 2 and 3 side by side
      fluidRow(
        # Visualization 2: Histogram (bottom left)
        column(
          6,
          h3("2. Histogram - Sepal Length"),
          plotOutput("histogram", height = "350px")
        ),
        # Visualization 3: Box plot (bottom right)
        column(
          6,
          h3("3. Box Plot - Petal Width by Species"),
          plotOutput("boxPlot", height = "350px")
        )
      )
    )
  )
)

# ============================================================================
# SERVER - Defines the logic that powers the visualizations
# ============================================================================
server <- function(input, output) {
  
  # Reactive expression that filters the iris data based on selected species
  # This runs whenever the user changes the species checkboxes
  # All three plots react to this filtered dataset automatically
  filtered_data <- reactive({
    req(input$species)  # Require at least one species to be selected
    iris[iris$Species %in% input$species, ]
  })
  
  # --------------------------------------------------------------------------
  # Visualization 1: Scatter Plot
  # Compares two user-selected measurements, colored by species
  # --------------------------------------------------------------------------
  output$scatterPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes_string(x = input$x_var, y = input$y_var, color = "Species")) +
      geom_point(size = 3, alpha = 0.7) +
      labs(
        title = paste(input$y_var, "vs", input$x_var),
        x = gsub("\\.", " ", input$x_var),
        y = gsub("\\.", " ", input$y_var)
      ) +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("setosa" = "#E74C3C",
                                     "versicolor" = "#3498DB",
                                     "virginica" = "#2ECC71"))
  })
  
  # --------------------------------------------------------------------------
  # Visualization 2: Histogram
  # Shows the distribution of Sepal Length with user-adjustable bin count
  # --------------------------------------------------------------------------
  output$histogram <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = Sepal.Length, fill = Species)) +
      geom_histogram(bins = input$bins, alpha = 0.7, position = "identity",
                     color = "black") +
      labs(
        title = "Distribution of Sepal Length",
        x = "Sepal Length (cm)",
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("setosa" = "#E74C3C",
                                    "versicolor" = "#3498DB",
                                    "virginica" = "#2ECC71"))
  })
  
  # --------------------------------------------------------------------------
  # Visualization 3: Box Plot
  # Compares the distribution of Petal Width across species
  # --------------------------------------------------------------------------
  output$boxPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = Species, y = Petal.Width, fill = Species)) +
      geom_boxplot(alpha = 0.7, outlier.size = 3) +
      labs(
        title = "Petal Width by Species",
        x = "Species",
        y = "Petal Width (cm)"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("setosa" = "#E74C3C",
                                    "versicolor" = "#3498DB",
                                    "virginica" = "#2ECC71"))
  })
  
  # --------------------------------------------------------------------------
  # Data Summary: Shows key statistics for the filtered dataset
  # --------------------------------------------------------------------------
  output$dataSummary <- renderPrint({
    df <- filtered_data()
    cat("Total observations:", nrow(df), "\n")
    cat("Species shown:", paste(unique(df$Species), collapse = ", "), "\n\n")
    summary(df[, 1:4])
  })
}

# ============================================================================
# Run the application
# ============================================================================
shinyApp(ui = ui, server = server)
