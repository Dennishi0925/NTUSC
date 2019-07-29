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
grade <- data_pornot %>%
  distinct(grade) %>%
  unlist()
names(grade) <- NULL
# Define UI for application that plots features of movies
ui <- fluidPage(
  #multiple pages
  navbarPage("NTUSC!",
    tabPanel("104-1",
      sidebarLayout(
        sidebarPanel(
          # Select variable for x-axis
          selectInput(inputId = "a1041", label = "group by",
                      choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                      selected = "people"),
          conditionalPanel(
            condition = "input.a1041 == 'college'",
            checkboxGroupInput(inputId = "college1041", label = "select college",
                               choices = college,
                               selected = college)),
          conditionalPanel(
            condition = "input.a1041 == 'grade'",
            checkboxGroupInput(inputId = "grade1041", label = "select grade",
                               choices = grade,
                               selected = grade)),
          conditionalPanel(
            condition = "input.a1041 == 'dept'",
            checkboxGroupInput(inputId = "dept1041", label = "select dept",
                               choices = dept,
                               selected = dept))
          ),
        mainPanel(
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", plotOutput(outputId = "b1041")),
                      tabPanel("Table", DT::dataTableOutput(outputId = "movietable")),
                      tabPanel("section", plotOutput(outputId = "sec1041")),
                      tabPanel("sum", DT::dataTableOutput(outputId = "sum"))
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
  plot_present <- data_pornot %>% filter(term == "1041") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = n/sum(n)) %>%
    ggplot() +
    geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
    geom_line(aes(x = keys, y = percent, group = values)) +
    #guides(fill=guide_legend(reverse=TRUE)) +
    #scale_fill_discrete() +
    coord_flip() +
    guides(fill=guide_legend(title="present")) +
    facet_grid(.~values) +
    scale_fill_brewer(palette="Pastel1")
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  data_1041_final <- reactive({
    if (input$a1041 == "college"){
    data_1041() %>% filter(college %in% input$college1041) %>% arrange(desc(college))
    } else if (input$a1041 == "grade"){
    data_1041() %>% filter(grade %in% input$grade1041) %>% arrange(desc(grade))
    } else if (input$a1041 == "dept"){
      data_1041() %>% filter(dept %in% input$dept1041) %>% arrange(desc(dept))
    }
    else {
    data_1041()
    }
  })
  summary <- data_pornot %>% filter(term == "1041") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = n/sum(n))
  
  output$b1041 <- renderPlot({
    if (is.null(data_1041_final())) {
      return()
    }
    ggplot(data = data_1041_final(), aes_string(x = input$a1041, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
   })
  
  output$sec1041 <- renderPlot({
    plot_present
  })
  
  output$sum <- DT::renderDataTable({
    DT::datatable(data = summary,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  
  #

  # Print data table if checked
  output$movietable <- DT::renderDataTable({
    DT::datatable(data = data_1041_final(),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)