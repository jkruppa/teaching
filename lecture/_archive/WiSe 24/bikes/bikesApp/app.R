#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readxl)

bikes_tbl <- read_excel("C:/Users/jokruppa/Desktop/bikes/bikesApp/bikes.xlsx")

bikes_tbl %>% 
  filter(tag_im_Jahr >= 260) %>% 
  ggplot(aes(tag_im_Jahr, anzahl_cum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput( 
              "range", "Vorhersagebereich", 
              min = min(bikes_tbl$tag_im_Jahr), 
              max = 366, 
              value = c(min(bikes_tbl$tag_im_Jahr), 
                        max(bikes_tbl$tag_im_Jahr)) 
            ),
            radioButtons( 
              inputId = "rain", 
              label = "Regentage anzeigen", 
              choices = list( 
                "Ja" = 1, 
                "Nein" = 2 
              ),
              selected = 2
            )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      filter_bikes_tbl <- bikes_tbl %>%
        mutate(regen = as_factor(regen)) %>% 
        filter(tag_im_Jahr >= input$range[1] &
                 tag_im_Jahr <= input$range[2]) 
      
      pred <- lm(anzahl_cum ~ tag_im_Jahr, filter_bikes_tbl) %>% 
        predict(tibble(tag_im_Jahr = 366)) %>% #
        round()
      
      filter_bikes_tbl %>% 
        ggplot(aes(tag_im_Jahr, anzahl_cum)) +
        theme_minimal() +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE,
                    fullrange = TRUE) +
        annotate("label", x = 366, y = pred, label = pred) +
        scale_x_continuous(breaks = c(seq(260, 366, 10), 366),
                           limits = c(260, 366)) +
        scale_y_continuous(limits = c(5e5, 1e6)) +
        if(input$rain == "1"){
          geom_point(aes(color = regen))
        }
      
        # generate bins based on input$bins from ui.R
     #   x    <- faithful[, 2]
     #   bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #         xlab = 'Waiting time to next eruption (in mins)',
   #          main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
