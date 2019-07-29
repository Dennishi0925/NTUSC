library(shiny)
library(tidyverse)
library(rlang)

data_pornot <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datapor")
college <- data_pornot %>%
  distinct(college) %>%
  unlist()
names(college) <- NULL
dept <- data_pornot %>%
  distinct(dept) %>%
  unlist()
names(dept) <- NULL

# Define UI for application that plots features of movies
ui <- fluidPage(
  #multiple pages
  navbarPage("NTUSC!",
             tabPanel("104-1",
                      sidebarLayout(
                        sidebarPanel(
                          # Select variable for x-axis
                          selectInput(inputId = "a1041", label = "order by",
                                      choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                                      selected = "people"),
                          checkboxGroupInput(inputId = "college1041", label = "select college",
                                             choices = college,
                                             selected = college)
                        ),
                        mainPanel(
                          # Output: Tabset w/ plot, summary, and table ----
                          tabsetPanel(type = "tabs",
                                      tabPanel("Plot", plotOutput(outputId = "b1041")),
                                      #tabPanel("Summary", verbatimTextOutput("summary")),
                                      tabPanel("Table", DT::dataTableOutput(outputId = "movietable"))
                          )
                        )
                      )
             ),
             
             tabPanel("Summary",
                      verbatimTextOutput("summary"))
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create scatterplot object the plotOutput function is expecting
  data_1041 <- reactive({  
    data_pornot %>%
      filter(term == "1041") %>%
      select(college, start, end, name, dept, grade, values) %>%
      group_by(!! rlang::sym(input$a1041), values) %>%
      count() %>% #mutate(n = n()) %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1041)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      #group_by(!! rlang::sym(input$a1041), present) %>%
      #count() %>%
      #ungroup %>%
      #group_by(!! rlang::sym(input$a1041)) %>%
      #mutate(percent = n/sum(n)) %>%
      ungroup
  })
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  #data_1041_final <- reactive({
  #  if (is.null(input$college1041)) {return(NULL)}
  #  # Keep the selected columns
  #  data_1041() %>% filter(college %in% input$college1041)
  #})
  
  output$b1041 <- renderPlot({
    if (is.null(data_1041())) {
      return()
    }
    ggplot(data = data_1041(), aes_string(x = input$a1041, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Print data table if checked
  output$movietable <- DT::renderDataTable({
    DT::datatable(data = data_1041(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)