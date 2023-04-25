#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(readxl)

dat <- read_excel(here("wgm2018-dataset-crosstabs-all-countries.xlsx"), 
                  sheet = 1,
                  skip = 2)

dat_clean <- dat %>%
  janitor::clean_names() %>%
  select(country:column_n_percent_4) %>%
  fill(question, .direction = "down")

library(rvest)

regions <- read_html("https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification") %>%
  html_table() %>%
  .[[1]]

dat_clean <- dat_clean %>%
  left_join(regions,
            by = c(country = "Country")) %>%
  drop_na(Region) 

question_options <- unique(dat_clean$question)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Survey Responses"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("question",
                        label = "Choose a question.",
                        choices = question_options
            ),
            uiOutput("response_choice"),
            actionButton("go",
                         label = "Click to update plot.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("my_plot"),
            h3("Dataset for plot:"),
           dataTableOutput("dataset")
        ) #mainPanet
    ) #sidebarlayout
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  ## input$question 
  ## string with a value from the `question` column
  ## input$affirmative_choice
  ## string containing value from `response` column that we want percent of
  
  dat_vax <- reactive({ 
  
  dat_q <- dat_clean %>%
    filter(question == input$question)
  
   dat_q %>%
    filter(response == input$affirmative_choice) %>%
    group_by(country, Region) %>% 
    summarize(pct = sum(column_n_percent_4))
   
  }) %>%
    bindEvent(input$go)
    

    output$my_plot <- renderPlot({

      
      plot_title <- str_remove(input$question, "Q[0-9]*")
      
      dat_vax() %>%
        ggplot(aes(x = Region, y = pct, fill = Region)) +
        geom_boxplot(alpha = 0.5) +
        geom_jitter(aes(color = Region), width = 0.1) +
        theme_classic() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45),
              axis.line.x = element_blank())+
        labs(
          x = "",
          y = "",
          title = plot_title
        )
      
    })%>%
      bindEvent(input$go)
    
    output$dataset <- renderDataTable(dat_vax()) %>%
      bindEvent(input$go)
    
    output$response_choice <- renderUI({
      
      these_choices <- dat_clean %>%
        filter(question == input$question) %>%
        pull(response) %>%
        unique()
      
      selectInput("affirmative_choice", 
                  label = "Which is the yes?",
                  choices = these_choices
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
