#
# This is a Shiny web application designed to collate and run
# The three separate apps that have been created by the hockey research team
#


# 1. Please run the code so you can see what it should be without your code additions
# 2. Please add any libraries required below
# 3. Please put your ui code within your assigned tabPanel
#     Please put your serveer code in the server section
#     Please clearly comment what is yours & what it does
#     Make sure your variables do not interfere with other code & everything works as expected
# 4. Comit your changes to GitHub and inform the next person who needs to add thier page

#
# Library loading
#
library(shiny)
library(shinydashboard)
library(png)
library(dplyr)
library(plotly)

#Loading additional libraries for Play Plotter
library(ggnewscale)
library(scales)
library(reshape2)
library(tidyverse)
library(tidymodels)
library(magrittr)
library(kableExtra)
library(DT)
library(dichromat)

#Libraries for Hockey Goal and Shot Explorer
library(ggplot2)
library(jpeg)
library(grid)
library(ggnewscale)
library(dplyr)
library(knitr)

#Sources the functions I use for the rink picture
source("./AnalysisCodeFunc.R")

# loading data frame for feature explore
shots_stats_goal.df <- readRDS("./www/shots_stats_goal.df.Rds")

#Loading stuff for the Play Plotter - Caleb's app
colorPalette <- c("Too Far" = "#A3D237","Mediocre" = "#39D0B0",
                  "Traffic Jam" = "#EA6522","Defender Block" = "#D373C6",
                  "Perfect Shot" = "#008BF8")
#Reads the RDS that has my cluster labels
shots_stats_Play_Plotter.df <- readRDS("./www/shots_stats_goal_clusters2.df.Rds") #now just need to change the labels
xsize <<- 2000
ysize <<- 850
# Coordinates to the goal pipes
pipes_x <<- 1890
lpipe_y <<- 395
rpipe_y <<- 455
# This file path should contain the hockey rink images and all the sequences
filepath <- './FinalGoalShots/'
# Read the rink images and format them to a raster used for graphing
rink_raster <- makeRaster(filepath, 'Rink_Template.jpeg')
half_rink_raster <- makeRaster(filepath, 'Half_Rink_Template.jpeg')

#options for selection as size or filter in the Play Plotter
possibleFeats <- c('Puck Speed' = 'puckSpeed','Shooter Speed' = 'shooterSpeed',
                   'Shooter Posession Time' = 'posTime','Number of Offensive Players' = 'NumOffense',
                   'Number of Defensive Players' = 'NumDefense',
                   'Defender Distance' = 'defDist','Defender Angle' = 'defAngle')

# data frame for Amy's app
shots_stats_cat <- readRDS("www/UpdatedFactoredDiscretizedData.Rds")
shots_stats_cat$NumDefense <- shots_stats_cat$NumDefense %>% as.factor()
shots_stats_cat$NumOffense <- shots_stats_cat$NumOffense %>% as.factor()
names(shots_stats_cat)[names(shots_stats_cat) == 'NumDefense'] <- 'Number of Defensive Players'
names(shots_stats_cat)[names(shots_stats_cat) == 'NumOffense'] <- 'Number of Offensive Players'

#
# ui elements for Ashley's page
#
heatmap_description1 <- box(
  title = strong("What is a heatmap?"), # Use the strong tag
  status = "primary", # Leave as primary
  collapsible = TRUE, # This allows collapsing
  collapsed = TRUE, # Initial value
  width = '100%',
  "The heatmap shows, using the color gradient, relatively high or low values for the different features (columns) and the different shots (rows). It allows us to analyze what features are associated (identified by the top dendrograms) and what shots are similar (row dendrograms) so we can conclude what different types of shots there are."
)
heatmap_description2 <- box(
  title = strong("What is Min/Max Scaling?"), # Use the strong tag
  status = "primary", # Leave as primary
  collapsible = TRUE, # This allows collapsing
  collapsed = TRUE, # Initial value not the main panel so collapse to start
  width = '100%',
  "Min/Max scaling is a data preparation method. Within each column, the highest value becomes 1, and the lowest value becomes 0. Then, each value in between is allotted based on percentage, somewhere between 0 and 1. For example, with the categorized data having low, medium, and high, the values will map to 0, 0.5, and 1 respectively."
)
heatmap_description3 <- box(
  title = strong("Clustering and Dendrograms Explained"), # Use the strong tag
  status = "primary", # Leave as primary
  collapsible = TRUE, # This allows collapsing
  collapsed = TRUE, # Initial value not the main panel so collapse to start
  width = '100%',
  "The dendrograms (lines along the top and bottom) represent a clustering tool utilized by the graph. Rows and columns with similar characteristics are grouped together, the more closely related the more closely they are connected. This allows us to identify clusters, which are distinguished with gaps between the rows. There are 5 shot clusters, discussed more in-depth in 'observations'."  
)
heatmap_description4 <- box(
  title = strong("Clusters"), # Use the strong tag
  status = "primary", # Leave as primary
  collapsible = TRUE, # This allows collapsing
  collapsed = FALSE, # Initial value not the main panel so collapse to start
  width = '100%',
  HTML("The clusters, from top to bottom, are observed as follows: <br> 
  1. Too far, waited too long <br>
  2. Far shots, crowded rink <br>
  3. Edge shots <br>
  4. Goalie face-off <br>
  5. Goalie out of place <br>
  If you have more descriptive names, or other observations, please share with the research team!")
)
heatmap_description5 <- box(
  title = strong("Observations"), # Use the strong tag
  status = "primary", # Leave as primary
  collapsible = TRUE, # This allows collapsing
  collapsed = TRUE, # Initial value not the main panel so collapse to start
  width = '100%',
  HTML("We have found that most goals occur in the 5th cluster, where the goalie is out of place. 
       This follows what many people logially know about hockey, when the goalie isn't between the puck and the goal, scoring is more likely.
       There were also goals present in the 3rd cluster, where edge shots were taken. This refers to shots taken at a ver shallow angle, not facing the goal but from the sides. 
       While this is undoubtedly a harder shot, with less of a target, appears to be the 2nd most often mode of scoring. Why do you think this is?
       The other clusters seemed to define modes of defensive success, where goals were not scored. In essence, when shots were taken from too far, they were blocked or generally unsucessful (clusters 1 and 2). And, a very good goalie means that a single offensive player rarely scored past her.
       <br> What do you think this means in connection to your gameplay? Share with the research team!")
)

heatmap <- tags$img(
  src = "pheatmapMinMaxCat.png",
  alt = "The min/max scaled heatmap visual of the data. For more description see dropdown menus.",
  width = 800
)

heatmap_descriptions <- box(
  width = 4,
  fluidRow(
    column( width = 12,
            heatmap_description1,
            heatmap_description2,
            heatmap_description3,
            heatmap_description4,
            heatmap_description5
    )
  )
)

AshleyTab <- tabPanel("Shot Patterns: Big Picture", 
                      id = "AshleyMain",
                      fluidRow(heatmap, heatmap_descriptions)
)

#
#  Amy's Tab
#

AmyTab <- tabPanel("Shot Outcomes", #tab name, tab content is for your app
                   titlePanel("Hockey Goal and Shot Explorer"), 
                   
                   sidebarLayout(
                     sidebarPanel(
                       # Dropdown for selecting a feature for color
                       selectInput("feature1", "Select Feature for Color:", choices = colnames(shots_stats_cat[c(8, 9, 17, 18, 19, 20, 21, 22, 23, 24, 25)])),
                       
                       # Dropdown for selecting a feature for shape
                       selectInput("feature2", "Select Feature for Shape:", choices = colnames(shots_stats_cat[c(8, 9, 17, 18, 19, 20, 21, 22, 23, 24, 25)])),
                       
                       # Dropdown for selecting a feature for size
                       selectInput("feature3", "Select Feature for Size:", choices = colnames(shots_stats_cat[c(8, 9, 17, 18, 19, 20, 21, 22, 23, 24, 25)])),
                       
                       
                     ),
                     
                     mainPanel(
                       plotOutput("dynamicPlot"),
                       width = 4
                     )
                    )
)


#
# Liebin's Tab
#


LiebinTab <- tabPanel("Shot Features",
                      titlePanel("Feature Explore"),
                      
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
                          plotlyOutput("dynamicPlot_Liebin")
                        )
                      )
)


#
# Caleb's Tab
#
#
# UI elements for Caleb's page
#

cluster_description <- box(
  title = strong("What are these clusters?"),
  status = "primary",
  collapsible = TRUE,
  collapsed = TRUE,
  width = '100%',
  HTML("I named the clusters based on their defining features, which you can see for yourself in the heatmap table. Here is a brief description of each of the clusters: <br>
1. Too far: Pretty self-explanatory, these shots were simply too far away from the goal to have any chance of success <br>
2. Mediocre: Average in pretty much every metric except for the slightly faster speed, which can be seen in the mixed colors on the heatmap <br>
3. Traffic Jam: Has the most players out of any cluster. Most of these shots were blocked by one of the other players involved <br>
4. Defender Block: While there were fewer players involved than traffic jam, the defenders were still nearby and in position to block the shot <br>
5. Perfect shot: Everything went perfectly for the shooter. 
       The notable difference between shots in this cluster and shots in the other clusters is the distance involved. 
       The average distance to the goal is 12 feet closer than the next closest cluster, and almost 30 feet closer than the furthest cluster. 
       They also tended to not have any players other than the goalie involved who could potentially block the shot.
       Even then, the goalie tended to be off the line the puck was traveling on by about 35 degrees,
       so the fact she was still able to block over half the shots is pretty astounding.")
) #For whatever reason \n wasn't working for line breaks, so I had to use HTML instead


cluster_algorithm_explanation <- box(
  title = strong("How are these clusters generated?"),
  status = "primary",
  collapsible = TRUE,
  collapsed = TRUE,
  width = '100%',
  "Clusters are generated from the K-Means algorithm applied to a UMAP projection of the original data. If you don't understand what that means, don't worry!
Essentially, the machine learning algorithm groups similar points together, so that we can look at types of shots rather than having to focus on each individual shots.
I named the clusters based on their defining features, which you can see for yourself in the heatmap table."
)

arrow_explanation <- box(
  title = strong("What's that arrow that appears when I click on a point?"),
  status = "primary",
  collapsible = TRUE,
  collapsed = TRUE,
  width = '100%',
  "The arrow points to the location of the goalie at the time the shot was taken. Unfortunately, I couldn't figure out how to add this information to the legend. If any of you know how to do this in ggplot2 (The R library used to generate the graph), please contact the research team."
)

#
#END UI ELEMENTS
#


#Actual UI code
CalebTab <- tabPanel("Shot Patterns: On Ice", #tab name, tab content is for your app
                     # Application title
                     fluidRow(
                       # Sidebar with a slider input for number of bins 
                       #sidebarLayout(
                       sidebarPanel(#img(src = "FOCI.png", height = 140, width = 400),
                         #selectInput("clust",h3("Select a cluster to plot:"),
                         #            choices = list("Too Far" = 1, "Mediocre" = 2,"Perfect Shot" = 3,
                         #                           "Defender Block" = 4,"Traffic Jam" = 5) ),
                         checkboxGroupInput("clust","Clusters to display:",
                                            c("Too Far" = "Too Far", "Mediocre" = "Mediocre","Traffic Jam" = "Traffic Jam",
                                              "Defender Block" = "Defender Block","Perfect Shot" = "Perfect Shot"),selected = c("Too Far", "Mediocre","Traffic Jam","Defender Block","Perfect Shot")),
                         selectInput("feature", "Select Feature for Size:", choices = possibleFeats),
                         selectInput("filtFeature","Select Feature to Filter by", choices = possibleFeats),
                         sliderInput("feat_range", "Show Shots with Features in Range:", 
                                     min = 0, max = 1000, value = c(0, 1000))
                         
                       ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(
                         h1("Rink View"), 
                         plotOutput("rink", click = "plot_click"),
                         actionButton("rem", "Return to Full Cluster View"),
                         #htmlOutput("tabName"),
                         #dataTableOutput("summaryTable"),
                         width = 4
                       ),
                       #),
                       #sidebarLayout(position = "right",
                       sidebarPanel(
                         h4("Instructions and Explanations:"),
                         div("Use the check boxes on the left to select clusters to plot, 
                             or click on a point to see an individual play in more detail. 
                             You can also use the drop down boxes on the left to select which feature to use for the shot size and which feature to filter by.
                             The slider will let you force only points that have the selected feature within a given range be plotted.
                             For more explanation on the clusters, consult the drop down boxes below."),
                         cluster_description,
                         cluster_algorithm_explanation,
                         arrow_explanation
                       ),
                       # Placeholder for the right main panel
                       mainPanel(width = 4)
                       #)
                     ),
                     
                     
                     
                     fluidRow(
                       htmlOutput("tabName"),
                       dataTableOutput("summaryTable"),
                       width = 4
                       
                     )
                     
)

AboutTab <- tabPanel("About", 
                      id = "AboutMain",
                      fluidRow(
                        column(width = 12,
                               box(title = strong("Hockey Shot Analysis Visualization Explorer"), width = 12),
                                fluidRow(
                                     box( title = strong("Credits"),
                                       width = 6, height = "100%",
                                       HTML("This app was developed by the Hockey SAVE research team, composed of <br>
                                            Amy Enyenihi, Jeff Jung, Caleb Smith, Ashley Woodson, and Lieben Zhang <br>
                                            with the goal of determining ways in which RPI's Women's Hockey Team succeed defensively. 
                                            <br> <br>
                                            We would like to thank Dr. John Erickson and Dr. Kristen Bennett of the Data INCITE Lab for supporting this project. <br>
                                            We thank Dr. Thomas Morgan as well for his expertise and guidance, as well as his coordination with the hockey team in developing this project and providing us this research opportunity.")
                                    ),
                                    box(title = strong("Contacts"),
                                        width = 6, height = "100%",
                                        HTML("If you have any 'INCITE's (haha) you would like to share with the research team's ongoing efforts, or would like to join the research team, please contact Dr. Morgan at morgat5@rpi.edu")
                                        )
                      )
                        )
                      )
)

#
# Define UI for application 
#
ui <- fluidPage(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "Hockey SAVE"),
    dashboardSidebar(collapsed = TRUE),
    dashboardBody(
      fluidRow(
        tabBox(
          # main tabbox for all the pages
          id = "main", width = 12, 
          AmyTab,
          LiebinTab,
          CalebTab,
          AshleyTab,
          AboutTab
        )
      )
    )
  )
)


# Define server logic required to fill in all apps
server <- function(input, output,session) { #I had to add the session part to make my code work - Caleb
  
  
  # Liebin's server part
  output$dynamicPlot_Liebin <- renderPlotly({
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
  
  # Caleb's server part
  shots_stats_Play_Plotter.df <-cbind.data.frame(shots_stats_Play_Plotter.df,data.frame(x = shotStatX(shots_stats_Play_Plotter.df),y = shotStatX(shots_stats_Play_Plotter.df))) 
  #browser()
  #shots <- reactive({
  #  dat <- shots_stats_Play_Plotter.df[shots_stats_Play_Plotter.df$Cluster == input$clust,]
  #print(dat)
  # dat
  #  
  # })
  # xsize <- 2000
  # ysize <- 850
  # # Coordinates to the goal pipes
  # pipes_x <- 1890
  # lpipe_y <- 395
  # rpipe_y <- 455
  global_size_range <- range(shots_stats_Play_Plotter.df$posTime)/6
  # generate bins based on input$bins from ui.R
  values <- reactiveValues()
  shots_stats_Play_Plotter.df$Cluster <- as.factor(shots_stats_Play_Plotter.df$Cluster)
  levels(shots_stats_Play_Plotter.df$Cluster) <- c("Too Far","Mediocre","Traffic Jam","Defender Block","Perfect Shot")
  values$shots <- shots_stats_Play_Plotter.df
  
  
  #Code for filtering borrowed from Lieben
  #Update the slider min and max based on the feature selected
  observeEvent(input$filtFeature, {
    min_val <-  floor(min(shots_stats_Play_Plotter.df[[input$filtFeature]], na.rm = TRUE))
    max_val <- ceiling(max(shots_stats_Play_Plotter.df[[input$filtFeature]], na.rm = TRUE))
    updateSliderInput(session, "feat_range", min = min_val, max = max_val, value = c(min_val, max_val))
  })
  
  #Filter the data based on the slider values
  observeEvent(input$feat_range,{
    values$shots <- shots_stats_Play_Plotter.df %>%
      filter(Cluster %in% input$clust) %>%
      filter(.data[[input$filtFeature]] >= input$feat_range[1], .data[[input$filtFeature]] <= input$feat_range[2])
  })
  
  #Filter the data based on selected cluster
  observeEvent(input$rem, {
    values$shots <- filter(shots_stats_Play_Plotter.df, Cluster %in% input$clust)
  })
  
  #Pull up the single point view if the graph is clicked
  observeEvent(input$plot_click,{
    values$shots <- values$shots[which.min(abs(input$plot_click$x - values$shots$x)),]
  })
  
  #Filter the data based on selected cluster - makes sure that if no clusters are selected nothing is plotted
  observeEvent(input$clust,{
    if(length(input$clust) >= 1){
      values$shots <- filter(shots_stats_Play_Plotter.df, Cluster %in% input$clust)
    }else{
      values$shots <- shots_stats_Play_Plotter.df[shots_stats_Play_Plotter.df$Cluster == -1,]
    }
  },ignoreNULL = FALSE)
  
  #Dynamically updates the heatmap table title based on whether one point is selected or not
  output$tabName <- renderPrint({
    if(nrow(values$shots) > 1){
      HTML(paste0("<h2>","Averages:","</h2>"))
    }else{
      HTML(paste0("<h2>","Shot Statistics:","</h2>"))
    }
  })
  
  #Handles creating the heatmap table
  output$summaryTable <- renderDataTable({
    #Handles the averages plot
    if(nrow(values$shots) > 1){
      #Selects the columns I want in the table
      toSum <- cbind.data.frame(values$shots[,'Cluster'],values$shots[,1:10],values$shots[,12:13])
      #Renames all the columns so it isn't hideous
      names <-c('Cluster','Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (mph)','Shooter Speed (mph)','Distance of Goalie from Goal (ft)',
                'Angle of Goalie (°)','Shooter Posession Time','Number of Offensive Players',
                'Number of Defensive Players','Is Right Handed?',
                'Defender Distance (ft)','Defender Angle (°)')
      colnames(toSum) <- names
      toSum <-cbind.data.frame(toSum[,1] ,apply(toSum[,setdiff(names,c('Cluster'))],2,as.numeric))
      colnames(toSum) <- names
      #print(toSum)
      toSum$Cluster <- as.factor(toSum$Cluster)
      #Grouping by cluster
      levels(toSum$Cluster) <- c("Too Far","Mediocre","Traffic Jam","Defender Block","Perfect Shot")
      tab <- as.data.frame(toSum) %>% group_by(Cluster) %>% summarise_all(.funs = mean)
      
      
      #Converting everything to the right units
      tab[,"Distance to Goal (ft)"] <- tab[,"Distance to Goal (ft)"]/10
      tab[,"Distance of Goalie from Goal (ft)"] <- tab[,"Distance of Goalie from Goal (ft)"]/10
      tab[,"Is Right Handed?"] <- lapply(tab[,"Is Right Handed?"], as.logical)
      tab[,"Puck Speed (mph)"] <- tab[,"Puck Speed (mph)"]/1.467
      tab[,'Shooter Speed (mph)'] <- tab[,'Shooter Speed (mph)']/1.467
      tab[,'Angle of Goalie (°)'] <--1 *  (tab[,'Angle of Goalie (°)'] - 90) #currently positive is left, do I need to multiply by -1 to flip this?
      tab[,'Angle of Shot (°)'] <--1 *  (tab[,'Angle of Shot (°)'] - 90)
      tab[,'Defender Distance (ft)'] <- tab[,'Defender Distance (ft)']/10
      tab[,'Defender Angle (°)'] <- -1 *  (tab[,'Defender Angle (°)'] - 90)
      #Chat GPT wrote the datatable code because this is due tonight
      # Identify numeric columns
      numeric_columns <- sapply(tab, is.numeric)
      numericColnames <- names(tab)[numeric_columns]
      
      heatTable <- datatable(tab,
                             escape = FALSE,
                             options = list(
                               scrollX = TRUE,  # Enable horizontal scrolling
                               autoWidth = TRUE  # Automatically adjust the width
                             ))
      
      #Creates the color scaling
      for(col in numericColnames){
        brks <- quantile(tab[[col]], probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
          {paste0("rgb(255,", ., ",", ., ")")}
        
        # Apply the style to the datatable
        heatTable <- heatTable %>% formatStyle(col, backgroundColor = styleInterval(brks, clrs)) %>% formatRound(columns = col, digits = 1)
      }
      heatTable
      
    }else{#For doing the statistics for a single shot
      #Does the same stuff as the above
      clusts <- as.factor(values$shots$Cluster)
      levels(clusts) <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
      tab <-cbind.data.frame(  values$shots[,1:14],clusts)
      tab[,1] <- tab[,1]/10
      tab[,5] <- tab[,5]/10
      tab[,10] <- as.logical(tab[,10])
      colnames(tab) <- c('Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (ft/s)','Shooter Speed (ft/s)','Distance of Goalie from Goal (ft)',
                         'Angle of Goalie','Shooter Posession Time','Number of Offensive Players',
                         'Number of Defensive Players','Is Right Handed?','Closest Defender',
                         'Defender Distance','Defender Angle','Shot Outcome','Cluster')
      # Identify numeric columns
      numeric_columns <- sapply(tab, is.numeric)
      numericColnames <- names(tab)[numeric_columns]
      
      heatTable <- datatable(tab)
      #This just rounds the numbers off instead of actually recoloring them
      for(col in numericColnames){
        brks <- quantile(tab[[col]], probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
          {paste0("rgb(255,", ., ",", ., ")")}
        
        # Apply the style to the datatable
        heatTable <- heatTable  %>% formatRound(columns = col, digits = 1)
      }
      heatTable
      
    }
  })
  
  #Creates the rink plot
  output$rink <- renderPlot({
    
    #Creates the shot outcome letterings    
    ShotType <- values$shots$shotOutcome
    levels(ShotType) <- c('D','G','S','M')
    
    clusts <- values$shots$Cluster
    
    #ChatGPT wrote this code to make the clusters labelled properly in the legend
    labelMapping <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
    uniqueIntegers <- unique(clusts)
    factorVector <- factor(clusts,levels =  uniqueIntegers,labels = labelMapping[match(uniqueIntegers, labelMapping)] )
    
    #Assigns each cluster to its color
    colorPalette <- c("Too Far" = "#A3D237","Mediocre" = "#39D0B0",
                      "Traffic Jam" = "#EA6522","Defender Block" = "#D373C6",
                      "Perfect Shot" = "#008BF8")
    
    #If not single point view
    if(nrow(values$shots) > 1){
      
      plotData <- values$shots
      colnames(plotData) <-c('Cluster','Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (mph)','Shooter Speed (mph)','Distance of Goalie from Goal (ft)',
                             'Angle of Goalie (°)','Shooter Posession Time','Number of Offensive Players',
                             'Number of Defensive Players','Is Right Handed?',
                             'Defender Distance (ft)','Defender Angle (°)')
      
      #Creates the rink
      ggplot(values$shots) +
        ylim(2000, 1000) +
        xlim(0,ysize) +
        annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
        theme(axis.title.x = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        # Graph players colored by handedness
        geom_text(aes(label = ShotType,color = clusts, x = shotStatX(values$shots), 
                      y = shotStatY(values$shots), size = !!sym(input$feature))) +
        scale_color_manual('Cluster', values = colorPalette) +
        scale_size(name = input$feature, range = global_size_range)#+ colors grabbed from https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf okabe and ito
      
    }else{#Just plot the single point
      defPos <- cbind.data.frame(shotStatX(values$shots) + cos(torad(values$shots$defAngle))*values$shots$defDist,shotStatY(values$shots) + sin(torad(values$shots$defAngle))*values$shots$defDist) 
      colnames(defPos) <- c('x','y')
      
      clusts <- as.factor(values$shots$Cluster)
      levels(clusts) <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
      
      #Creates the arrow points to the goalie
      angle <-  values$shots %>% mutate(xstart = ysize / 2) %>% 
        mutate(ystart = pipes_x) %>% 
        mutate(rad = goalieDist) %>%
        mutate(ang = goalieAngle)
      
      #Rink plot
      ggplot(values$shots) +
        ylim(2000, 1000) +
        xlim(0,ysize) +
        annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
        theme(axis.title.x = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) + 
        geom_text(aes(label = ShotType, x = shotStatX(values$shots), color = clusts,
                      y = shotStatY(values$shots), size = !!sym(input$feature))) +
        scale_color_manual('Cluster', values = colorPalette)+ #why isn't this conserving color
        new_scale_color() +
        scale_size(name = 'Selected Feature', range = global_size_range)+
        geom_point(data = defPos,aes(x = x,y=y,color = "Turquoise",))+
        labs(x = NULL, y = NULL)+
        scale_color_discrete('Nearest Defender',type = c('turquoise'))+ #Creates the blue dot for the defender
        
        
        geom_spoke(data = angle, aes(x = xstart, y = ystart, angle =
                                       torad(ang), radius = rad), 
                   key_glyph = 'abline', linetype = 'solid', arrow = arrow(), linewidth = 1)
    }
  })
  
  
  # Amy's server part
 # server <- function(input, output, session) {
    
    
    # Generate dynamic plots based on user selection
    output$dynamicPlot <- renderPlot({
      
      feature1 <- input$feature1
      feature2 <- input$feature2
      feature3 <- input$feature3
      
      # Base for rink visuals
      rink <- geom_point(aes(x = shotStatX(shots_stats_cat), y = shotStatY(shots_stats_cat), 
                             color = !!sym(feature1), 
                             shape = !!sym(feature2), 
                             size = !!sym(feature3)))
      
      
      # Graph players with features highlighted by color, shape, and size
      ggplot(shots_stats_cat) +
        ylim(2000, 1000) +
        xlim(0,ysize) +
        annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
        theme(axis.title.x = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())  + rink + facet_wrap(~shotOutcome) +
        labs(color = feature1, shape = feature2, size = feature3)
      
      
    })
    
 # }
  
}

# Run the application 
shinyApp(ui = ui, server = server)