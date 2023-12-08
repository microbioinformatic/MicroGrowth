#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install required packages if not installed
# install.packages(c("shiny", "shinydashboard", "DT"))

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(gcplyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)


# Function to read Measurements file
read_wide_data <- function(file) {
    if (!is.null(file)) {
        # Print file information
        cat("File path:", file$datapath, "\n")
        
        # Attempt to read the data and catch any errors
        tryCatch({
            # Read the data using read_wides
            data_wide <- read_wides(files = file$datapath)
            
            # Check if the object is a data frame
            if (is.data.frame(data_wide)) {
                # Extract file name from the path and replace the 'file' column
                data_wide$file <- basename(file$datapath)
                
                # Print the head of the data
                cat("Head of data_wide:\n")
                print(head(data_wide))
                
                # Update column names if "Time [s]" is present
                if ("Time [s]" %in% colnames(data_wide)) {
                    colnames(data_wide)[colnames(data_wide) == "Time [s]"] <- "Time"
                }
                
                # Remove the original 'file' column
                data_wide <- data_wide[, !(colnames(data_wide) %in% c("file"))]
                
                # Additional processing steps
                colnames(data_wide)[colnames(data_wide) == "Time [s]"] <- "Time"
                data_tidy <- trans_wide_to_tidy(wides = data_wide, id_cols = c("file", "Time"))
                data_tidy$Time <- as.numeric(data_tidy$Time)/3600
                
                return(data_tidy)
            } else {
                cat("Error: The object returned by read_wides is not a data frame.\n")
                return(NULL)
            }
        }, error = function(e) {
            # Print the error message
            cat("Error during reading data:", conditionMessage(e), "\n")
            cat("Traceback:\n")
            print(e)
            return(NULL)
        })
    }
}

# Function to read designs
read_designs <- function(file1, file2) {
    cat("Reading Design File one:", file1$datapath, "\n")
    cat("Reading Design File two:", file2$datapath, "\n")
    
    designs <- import_blockdesigns(files = c(file1$datapath, file2$datapath))
    
    return(designs)
}

# Function to merge data_tidy and designs
merge_dfs <- function(data_tidy, designs) {
    if (!is.null(data_tidy) && !is.null(designs)) {
        print("Inside merge_dfs")
        print("Head of data_tidy:")
        print(head(data_tidy))
        print("Head of designs:")
        print(head(designs))
        
        # Extract the common column "Well" from data_tidy and designs
        well_column <- intersect(colnames(data_tidy), colnames(designs))
        
        if (length(well_column) > 0) {
            merged_data <- gcplyr::merge_dfs(data_tidy, designs, by = well_column)
            
            print("Head of merged_data:")
            print(head(merged_data))
            
            return(merged_data)
        } else {
            cat("Error: No common column 'Well' found for merging.\n")
            return(NULL)
        }
    } else {
        return(NULL)
    }
}

# Function to create a scatter plot
scatter_plot <- function(data) {
    ggplot(data,
           aes(x = Time, y = Measurements, color = Experiment1)) +
        geom_line() + 
        facet_wrap(~ Experiment2) +
        theme_minimal()
}

# UI
ui <- dashboardPage(
    dashboardHeader(title = "MicroGrowth"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plate reader data", tabName = "data", icon = icon("file")),
            fileInput("data_file", "Choose Measurements File (CSV format)"),
            fileInput("design_file1", "Choose Experimental Design File 1 (CSV format)"),
            fileInput("design_file2", "Choose Experimental Design File 2 (CSV format)"),
            textInput("blank_input", "Enter Blank Value", value = ""),
            menuItem("Lag times", tabName = "lag_times", icon = icon("line-chart")),
            menuItem("Initial density", tabName = "init_dens", icon = icon("line-chart")),
            menuItem("Time to reach threshold density", tabName = "dens_thresh", icon = icon("line-chart")),
            menuItem("Area under the curve", tabName = "auc", icon = icon("line-chart"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "data",
                fluidRow(
                    
                        box(
                            width = 10,
                            solidHeader = TRUE,
                            title = "Render Measurements and Designs Tables",
                            status = "info",
                            uiOutput("render_tables_button"),
                            HTML('<p style="font-size: 14px;">Click this button to render the data and designs tables.<br>Note: Please make sure you have uploaded all required files <br>- Measurements file (CSV format)<br>- Experimental Design 1 (CSV format)<br>- Experimental Design 2 (CSV format)</p>')
                        )
                    ),
                
                fluidRow(
                    box(
                        width = 6,
                        title = "Measurements",
                        DTOutput("complete_data_table")
                    ),
                    box(
                        width = 6,
                        title = "Experimental design",
                        DTOutput("designs_table")
                    )
                    
                ),
                fluidRow(
                    box(
                        width = 10,
                        solidHeader = TRUE,
                        title = "Process Data",
                        status = "info",
                        actionButton("Process", "Process"),
                        HTML('<p style="font-size: 14px;">Click this button to process the uploaded data and designs.<br>
    Ensure that you have uploaded all required files:<br>
    - Measurements file (CSV format)<br>
    - Experimental Design 1 (CSV format)<br>
    - Experimental Design 2 (CSV format)<br>
    Enter the blank value in the "Enter Blank Value" field before processing.<br>
    This action will generate processed data for further analysis.</p>')
                    )
                ),
                fluidRow(
                    box(
                        title = "Growth curves",
                        plotOutput("scatter_plot")
                    ),
                    box(
                        title = "Per-capita derivative",
                        plotOutput("second_plot")
                    )
                )
            ),
            tabItem(
                tabName = "lag_times",
                fluidRow(
                    box(
                        title = "Lag Time Examinations",
                        plotOutput("lag_time_summary_plot")
                    ),
                    box(
                        title = "Comparing samples lag time I",
                        plotOutput("lag_time_boxplot")
                    ),
                    box(
                        title = "Comparing samples lag time II",
                        plotOutput("lag_time_average_plot")
                    ),
                    box(
                        title = "Download Lag Time Summary",
                        downloadButton("download_lag_time", "Download Lag Time Summary CSV")
                    )
                )
            )
        )
    )
)
# Server
server <- function(input, output) {
    # Reactive values to store data and designs
    data_tidy <- reactiveVal(NULL)
    designs <- reactiveVal(NULL)
    scatter_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)
    lag_time_summary <- reactiveVal(NULL)  
    
    # Update data_tidy reactive value when data_file is changed
    observe({
        data_tidy(read_wide_data(input$data_file))
    })
    
    
    
    # Update designs reactive value when design_file1 and design_file2 are changed
    observeEvent(c(input$design_file1, input$design_file2), {
        cat("Attempting to read designs...\n")
        
        # Check if both design files are present
        if (!is.null(input$design_file1) && !is.null(input$design_file2)) {
            tryCatch({
                designs <- import_blockdesigns(files = c(input$design_file1$datapath, input$design_file2$datapath))
                
                if (!is.null(designs)) {
                    # Rename columns to match the expected names by column number
                    colnames(designs)[1] <- "Well"
                    colnames(designs)[2] <- "Experiment1"
                    colnames(designs)[3] <- "Experiment2"
                    
                    cat("Designs read successfully.\n")
                    print(head(designs))
                    designs(designs)
                } else {
                    cat("Error: Designs could not be read.\n")
                    designs(NULL)
                }
            }, error = function(e) {
                cat("Error during reading designs:", conditionMessage(e), "\n")
                designs(NULL)
            })
        } else {
            cat("Waiting for both design files to be uploaded.\n")
        }
    })
    
    ####Tables#####
    # Inside the server function
    observe({
        # Render a button to trigger table rendering
        output$render_tables_button <- renderUI({
            actionButton("render_tables", "Render Tables")
        })
        # Check if data and designs are not NULL
        if (!is.null(input$tabName) && input$tabName == "data" && !is.null(data_tidy()) && !is.null(designs())) {
            # Render the complete data summary table
            output$complete_data_table <- renderDataTable({
                req(data_tidy())
                datatable(data_tidy(), options = list(scrollX = TRUE, scrollY = TRUE))
            })
            
            # Render the designs table
            output$designs_table <- renderDataTable({
                req(designs())
                datatable(designs(), options = list(scrollX = TRUE, scrollY = TRUE))
            })
            
           
        }
    })
    
    observeEvent(input$render_tables, {
        # Check if data and designs are not NULL
        if (!is.null(data_tidy()) && !is.null(designs())) {
            # Render the complete data summary table
            output$complete_data_table <- renderDataTable({
                req(data_tidy())
                datatable(data_tidy(), options = list(scrollX = TRUE, scrollY = TRUE))
            })
            
            # Render the designs table
            output$designs_table <- renderDataTable({
                req(designs())
                datatable(designs(), options = list(scrollX = TRUE, scrollY = TRUE))
            })
        }
        # Add this line to show a message when the button is clicked
        #showModal(modalDialog("Tables Rendered!"))
    })
    
    ####END TABles####
    
    observeEvent(input$Process, {
        if (!is.null(data_tidy()) && !is.null(designs())) {
            blank_value <- as.numeric(input$blank_input)
            if (!is.na(blank_value)) {
                processed_data <- merge_dfs(data_tidy(), designs()) %>%
                    group_by(Well, Experiment1, Experiment2) %>%  # Add grouping
                    mutate(
                        deriv = calc_deriv(x = Time, y = Measurements),
                        deriv_percap5 = calc_deriv(x = Time, y = Measurements, percapita = TRUE, blank = blank_value, trans_y = "log"),
                        doub_time = doubling_time(y = deriv_percap5)
                    )
                # Update both reactive values
                processed_data$Experiment1 = trimws(processed_data$Experiment1)
                processed_data(processed_data)
                scatter_data(dplyr::filter(processed_data))
            } else {
                # Handle the case where the input is not a valid numeric value
                cat("Error: Please enter a valid numeric value for the blank.\n")
                processed_data(NULL)
                scatter_data(NULL)
            }
        }
    })
    
    output$scatter_plot <- renderPlot({
        if (!is.null(scatter_data())) {
            
            ggplot(data = scatter_data(),  aes(x = Time, y = Measurements, color = Experiment1)) +
                geom_line() + 
                facet_wrap(~ Experiment2) +
                theme_minimal()
        }
    })
    
    output$second_plot <- renderPlot({
        if (!is.null(processed_data())) {
            ggplot(data = dplyr::filter(processed_data()), aes(x = Time, y = doub_time)) +
                geom_line(na.rm = TRUE) +  # Ignore missing values
                facet_wrap(~Well, scales = "free")
        }
    })
    
   
    
    # Lag Time calculation and visualization
    observeEvent(input$Process, {
        if (!is.null(processed_data())) {
            ex_dat_mrg_sum <- processed_data() %>%
                group_by(Experiment1, Experiment2, Well) %>%
                summarize(
                    lag_time = lag_time(y = Measurements, x = Time, deriv = deriv_percap5),
                    max_percap = max_gc(deriv_percap5),
                    max_percap_time = Time[which_max_gc(deriv_percap5)],
                    max_percap_dens = Measurements[which_max_gc(deriv_percap5)],
                    min_dens = min_gc(Measurements)
                )
            # Save lag_time summary to a CSV file
            write.csv(ex_dat_mrg_sum, file = "lag_time_calculation_derv5.csv")
            
            # Plotting
            output$lag_time_summary_plot <- renderPlot({
                ggplot(data = dplyr::filter(processed_data()), aes(x = Time, y = log(Measurements))) +
                    geom_point() +
                    facet_wrap(~Well) +
                    geom_abline(data = ex_dat_mrg_sum, color = "red",
                                aes(slope = max_percap,
                                    intercept = log(max_percap_dens) - max_percap * max_percap_time)) +
                    geom_vline(data = ex_dat_mrg_sum, aes(xintercept = lag_time), lty = 2) +
                    geom_hline(data = ex_dat_mrg_sum, aes(yintercept = log(min_dens)))
            })
            
            output$lag_time_boxplot <- renderPlot({
                # Plot of lag_time per Design1 per Deseign2
                ex_dat_mrg_sum$Experiment1 <- trimws(ex_dat_mrg_sum$Experiment1)
                summary_data <- ex_dat_mrg_sum %>%
                    group_by(Experiment2, Experiment1) %>%
                    summarize(mean_lag_time = mean(lag_time, na.rm = TRUE),
                              sd_lag_time = sd(lag_time, na.rm = TRUE))
                ggplot(summary_data, aes(x = Experiment1, y = mean_lag_time, color = Experiment2)) +
                    geom_point(position = position_dodge(width = 0.6), size = 3) +
                    geom_errorbar(aes(ymin = mean_lag_time - sd_lag_time, ymax = mean_lag_time + sd_lag_time),
                                  width = 0.2, position = position_dodge(width = 0.6)) +
                    labs(title = "Mean lag_time with SD per Experiments",
                         x = "Media",
                         y = "Lag Time") +
                    theme_minimal() + theme_minimal()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            })
            
            output$lag_time_average_plot <- renderPlot({
                # Plot of design 2 vs design 1
                ex_dat_mrg_sum$Experiment1 <- trimws(ex_dat_mrg_sum$Experiment1)
                summary_data <- ex_dat_mrg_sum %>%
                    group_by(Experiment1, Experiment2) %>%
                    summarize(mean_lag_time = mean(lag_time, na.rm = TRUE),
                              sd_lag_time = sd(lag_time, na.rm = TRUE))
                ggplot(summary_data, aes(x = Experiment2, y = mean_lag_time, color = Experiment1)) +
                    geom_point(position = position_dodge(width = 0.6), size = 3) +
                    geom_errorbar(aes(ymin = mean_lag_time - sd_lag_time, ymax = mean_lag_time + sd_lag_time),
                                  width = 0.2, position = position_dodge(width = 0.6)) +
                    labs(title = "Mean lag_time with SD per Experiments",
                         x = "Media",
                         y = "Lag Time") +
                    theme_minimal()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            })
            
            # Update reactive value for lag time summary
            lag_time_summary(ex_dat_mrg_sum)
        }
    })

            
    output$download_lag_time <- downloadHandler(
        filename = function() {
            "lag_time_calculation_mouse_derv5.csv"
        },
        content = function(file) {
            write.csv(lag_time_summary(), file, row.names = FALSE)
        }
    )
    
  
}

# Run the application
shinyApp(ui, server)




