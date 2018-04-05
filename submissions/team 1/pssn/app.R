#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(forcats)

Team <- read_csv("data/Teams.csv")
realvars <- c("Kicks", "Handballs", "Disp_eff", "Frees_Agst", "Goals", "Behinds", "Frees_For"
              )
catvars <- c( "Club")
clubs <- unique(Team$Club)

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Women's AFL Statistics Exploration"),
    tabsetPanel(
      # Set up the user interface for the scatterplots tab
      # to select variables to plot, labels and colouring
      tabPanel("Basic plots",
               # Sidebar choosing variables, labels and colour
               sidebarLayout(
                 sidebarPanel(
                   selectInput('x', "X", realvars, realvars[1]),
                   selectInput('y', "Y", realvars, realvars[2]),
                   selectInput('label', "Label", catvars),
                   checkboxGroupInput('clr1', "Colour by club:", clubs, NA)
                 ),

                 # Show the scatterplot, with a fixed height
                 mainPanel(
                   plotlyOutput("scatterplot", height="400px")
                 )
               )
      ),
      # Set up the user interface for the dotplots tab
      # to select labels, allow jitter, and colouring
      tabPanel("Boxplots",
               sidebarLayout(
                 sidebarPanel(
                   selectInput('x_box', "X", realvars, realvars[1])
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(
                   plotlyOutput("dotplot", height="100%")
                 )
               )
      ),
      # Set up the user interface for the multidimensional scaling tab
      # to select variables to combine, and colouring
      tabPanel("Team",
     sidebarLayout(
       sidebarPanel(
         checkboxGroupInput('vars', "Variables to use:", realvars, realvars[1:3])
         ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("mds")
        )
      )
    )
  )
)

server <- function(input, output) {

  # Make the interactive scatterplot
  output$scatterplot <- renderPlotly({
     # Set up the basic plot, using the selected variables,
     # facetted by year
     p <- ggplot(Team,
            aes_string(x = input$x, y = input$y,
                       label = input$label)) +
       facet_wrap(~Year, ncol=2)
     # Check if Clubs are chosen to colour by. If so add colours to
     # points for the selected clubs, fade everything else out
     if (length(input$clr1) > 0) {
       Team_club <- Team %>% filter(Club %in% input$clr1)
       p <- p + geom_point(alpha = 0.1, size=1) +
         geom_point(data=Team_club, mapping=aes(colour=Club),
                    alpha=1, size=2) +
         scale_colour_brewer(palette="Dark2") +
         theme(legend.position = "bottom")
     }
     else # If no clubs chosen make all points black
       p <- p + geom_point(alpha = 0.4)

     ggplotly(p, tooltip=c("label", "x", "y"))
   })

  # Make the interactive dotplot
  output$dotplot <- renderPlotly({
    # Rearrange data to facilitate plots
    Team_sub <- Team %>%
      select(Club, Year, input$x_box)
    Team_long <- Team_sub %>%
      gather(stat, value, -Club, -Year)
    # Set up the basic plot, using the selected variables,
    # facetted by year and statistic. Labeller code needed to make
    # facet headings more readable. Remove axis text, for
    # readability, because we can get the values from mouse over
    p <- ggplot(Team_long,
                aes_string(x = "Club", y = "value",
                           label = input$label_dotplot)) +
      facet_wrap(~stat+Year, scales="free",
                 nrow=2,
                 labeller = labeller(.multi_line = FALSE,
                   Year = label_value, stat=label_value)) +
      xlab("") + ylab("") +
      theme(strip.text.y = element_blank())

    p <- p + geom_boxplot()
    # Make it interactive with labels being the Club or Player,
    # and value
    ggplotly(p, tooltip=c("label", "y"))
  })

  # Make the multidimensional scaling plot
  output$mds <- renderPlotly({
     # Needs to be calculated separately by year
     # Subset the data to the year, and chosen variables
     Team_sub17 <- Team %>%
       filter(Year == "2017") %>%
       select(input$vars)
     Team_sub18 <- Team %>%
       filter(Year == "2018") %>%
       select(input$vars)
     # MDS requires matrix/numeric input only
     Team_sub_mat17 <- as.matrix(Team_sub17)
     Team_sub_mat18 <- as.matrix(Team_sub18)
     # The statistics are all scaled to be centred at 0,
     # with standard deviation 1
     Team_sub_mat17 <- apply(Team_sub_mat17, 2, scale)
     Team_sub_mat18 <- apply(Team_sub_mat18, 2, scale)
     # Do the multidimensional scaling with the selected variables
     # Find a 2D layout of points to best match the distance
     # between Team using all the chosen statistics
     Team_mds17 <- cmdscale(dist(Team_sub_mat17), k=2)
     Team_mds18 <- cmdscale(dist(Team_sub_mat18), k=2)
     # Put the two data sets back together, also adding Player,
     # Year and Club back in
     Team_mds_df17 <- as_tibble(Team_mds17)
     Team_mds_df17$Club <- Team$Club[Team$Year == "2017"]
     Team_mds_df17$Year <- "2017"
     Team_mds_df18 <- as_tibble(Team_mds18)
     Team_mds_df18$Club <- Team$Club[Team$Year == "2018"]
     Team_mds_df18$Year <- "2018"
     Team_mds_df <- bind_rows(Team_mds_df17, Team_mds_df18)
     # Make the plot
     mds <- ggplot(Team_mds_df, aes(x=V1, y=V2, label=Club)) +
       facet_wrap(~Year, scales="free")
     # Colour by Club if chosen
     mds <- mds + geom_point()
     ggplotly(mds, tooltip=c("label"))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

