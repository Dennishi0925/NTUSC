library(shiny)
library(tidyverse)
library(rlang)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
getwd()
data_pornot <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datapor") %>% select(-c(X1, ID))
data_lm <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datalm")
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
term <- data_pornot %>%
  distinct(term) %>%
  unlist()
names(term) <- NULL
# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("united"),
  useShinyjs(),
  #multiple pages
  navbarPage("NTUSC!",
    tabPanel("104-1",
      sidebarLayout(position = "right",
        sidebarPanel(div(id ="Sidebar1041",
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
          )),
        mainPanel(
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      id = "term1041",
                      tabPanel("summarizePlot", 
                               plotOutput(outputId = "b1041"),
                               DT::dataTableOutput(outputId = "summarizetable_1041")),
                      tabPanel("percentPlot",
                               plotOutput(outputId = "b1041_2"),
                               DT::dataTableOutput(outputId = "percenttable_1041")),
                      tabPanel("timePlot", 
                               plotOutput(outputId = "b1041_3"),
                               DT::dataTableOutput(outputId = "timetable_1041")),
                      tabPanel("all data", DT::dataTableOutput(outputId = "all_1041"))
                 )
               )
             )
    ),
    
    tabPanel("104-2",
             sidebarLayout(position = "right",
               sidebarPanel(div(id ="Sidebar1042",
                 # Select variable for x-axis
                 selectInput(inputId = "a1042", label = "group by",
                             choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                             selected = "people"),
                 conditionalPanel(
                   condition = "input.a1042 == 'college'",
                   checkboxGroupInput(inputId = "college1042", label = "select college",
                                      choices = college,
                                      selected = college)),
                 conditionalPanel(
                   condition = "input.a1042 == 'grade'",
                   checkboxGroupInput(inputId = "grade1042", label = "select grade",
                                      choices = grade,
                                      selected = grade)),
                 conditionalPanel(
                   condition = "input.a1042 == 'dept'",
                   checkboxGroupInput(inputId = "dept1042", label = "select dept",
                                      choices = dept,
                                      selected = dept))
               )),
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             id = "term1042",
                             tabPanel("summarizePlot", 
                                      plotOutput(outputId = "b1042"),
                                      DT::dataTableOutput(outputId = "summarizetable_1042")),
                             tabPanel("percentPlot",
                                      plotOutput(outputId = "b1042_2"),
                                      DT::dataTableOutput(outputId = "percenttable_1042")),
                             tabPanel("timePlot",
                                      plotOutput(outputId = "b1042_3"),
                                      DT::dataTableOutput(outputId = "timetable_1042")),
                             tabPanel("all data", DT::dataTableOutput(outputId = "all_1042"))
                 )
               )
             )
    ), 
    
    tabPanel("105-1",
             sidebarLayout(position = "right",
               sidebarPanel(div(id ="Sidebar1041",
                 # Select variable for x-axis
                 selectInput(inputId = "a1051", label = "group by",
                             choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                             selected = "people"),
                 conditionalPanel(
                   condition = "input.a1051 == 'college'",
                   checkboxGroupInput(inputId = "college1051", label = "select college",
                                      choices = college,
                                      selected = college)),
                 conditionalPanel(
                   condition = "input.a1051 == 'grade'",
                   checkboxGroupInput(inputId = "grade1051", label = "select grade",
                                      choices = grade,
                                      selected = grade)),
                 conditionalPanel(
                   condition = "input.a1051 == 'dept'",
                   checkboxGroupInput(inputId = "dept1051", label = "select dept",
                                      choices = dept,
                                      selected = dept))
               )),
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             id = "term1051",
                             tabPanel("summarizePlot", 
                                      plotOutput(outputId = "b1051"),
                                      DT::dataTableOutput(outputId = "summarizetable_1051")),
                             tabPanel("percentPlot",
                                      plotOutput(outputId = "b1051_2"),
                                      DT::dataTableOutput(outputId = "percenttable_1051")),
                             tabPanel("timePlot",
                                      plotOutput(outputId = "b1051_3"),
                                      DT::dataTableOutput(outputId = "timetable_1051")),
                             tabPanel("all data", DT::dataTableOutput(outputId = "all_1051"))
                 )
               )
             )
    ),    
    
    tabPanel("105-2",
             sidebarLayout(position = "right",
               sidebarPanel(div(id ="Sidebar1052",
                 # Select variable for x-axis
                 selectInput(inputId = "a1052", label = "group by",
                             choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                             selected = "people"),
                 conditionalPanel(
                   condition = "input.a1052 == 'college'",
                   checkboxGroupInput(inputId = "college1052", label = "select college",
                                      choices = college,
                                      selected = college)),
                 conditionalPanel(
                   condition = "input.a1052 == 'grade'",
                   checkboxGroupInput(inputId = "grade1052", label = "select grade",
                                      choices = grade,
                                      selected = grade)),
                 conditionalPanel(
                   condition = "input.a1052 == 'dept'",
                   checkboxGroupInput(inputId = "dept1052", label = "select dept",
                                      choices = dept,
                                      selected = dept))
               )),
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             id = "term1052",
                             tabPanel("summarizePlot", 
                                      plotOutput(outputId = "b1052"),
                                      DT::dataTableOutput(outputId = "summarizetable_1052")),
                             tabPanel("percentPlot",
                                      plotOutput(outputId = "b1052_2"),
                                      DT::dataTableOutput(outputId = "percenttable_1052")),
                             tabPanel("timePlot",
                                      plotOutput(outputId = "b1052_3"),
                                      DT::dataTableOutput(outputId = "timetable_1052")),
                             tabPanel("all data", DT::dataTableOutput(outputId = "all_1052"))
                 )
               )
             )
    ),  
    
    tabPanel("106-1",
             sidebarLayout(position = "right",
               sidebarPanel(div(id ="Sidebar1061",
                 # Select variable for x-axis
                 selectInput(inputId = "a1061", label = "group by",
                             choices = c("college"="college", "department"="dept", "grade"="grade", "people"="name"),
                             selected = "people"),
                 conditionalPanel(
                   condition = "input.a1061 == 'college'",
                   checkboxGroupInput(inputId = "college1061", label = "select college",
                                      choices = college,
                                      selected = college)),
                 conditionalPanel(
                   condition = "input.a1061 == 'grade'",
                   checkboxGroupInput(inputId = "grade1061", label = "select grade",
                                      choices = grade,
                                      selected = grade)),
                 conditionalPanel(
                   condition = "input.a1061 == 'dept'",
                   checkboxGroupInput(inputId = "dept1061", label = "select dept",
                                      choices = dept,
                                      selected = dept))
               )),
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             id = "term1061",
                             tabPanel("summarizePlot", 
                                      plotOutput(outputId = "b1061"),
                                      DT::dataTableOutput(outputId = "summarizetable_1061")),
                             tabPanel("percentPlot",
                                      plotOutput(outputId = "b1061_2"),
                                      DT::dataTableOutput(outputId = "percenttable_1061")),
                             tabPanel("timePlot",
                                      plotOutput(outputId = "b1061_3"),
                                      DT::dataTableOutput(outputId = "timetable_1061")),
                             tabPanel("all data", DT::dataTableOutput(outputId = "all_1061"))
                 )
               )
             )
    ),
    
    tabPanel("Summary",
      sidebarLayout(position = "right",
        sidebarPanel(div(id ="Sidebarsummary",
                         checkboxGroupInput(inputId = "collegedownload", label = "select college",
                                          choices = college,
                                          selected = college),
                         checkboxGroupInput(inputId = "gradedownload", label = "select grade",
                                          choices = grade,
                                          selected = grade),
                         checkboxGroupInput(inputId = "termdownload", label = "select term",
                                          choices = term,
                                          selected = term),
                         # Button
                         downloadButton("downloadData", "Download")
               )),
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             id = "summary",
                             tabPanel("collegePlot", 
                                      plotOutput(outputId = "b1"),
                                      DT::dataTableOutput(outputId = "summarizetable")),
                             tabPanel("gradePlot",
                                      plotOutput(outputId = "b2"),
                                      DT::dataTableOutput(outputId = "percenttable")),
                             tabPanel("termPlot",
                                      plotOutput(outputId = "b3"),
                                      DT::dataTableOutput(outputId = "timetable")),
                             tabPanel("votePlot",
                                      plotOutput(outputId = "b4"),
                                      plotOutput(outputId = "b5"),
                                      plotOutput(outputId = "b6"),
                                      DT::dataTableOutput(outputId = "votetable")),
                             tabPanel("download data", DT::dataTableOutput(outputId = "downloadtable"))
                 )
               )
             )
    )
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
      count() %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1041)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      ungroup
  })
  data_1041_02 <- reactive({
    data_pornot %>%
      filter(term == "1041") %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(dummyp = if_else(present == "not", 0, 1)) %>%
      group_by(!! rlang::sym(input$a1041), keys) %>%
      mutate(Attnd_Rt = round(sum(dummyp)/n(), 2))
  }) 
  data_1041_03 <- data_pornot %>% filter(term == "1041") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = round(n/sum(n), 2))
  
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
  data_1041_02final <- reactive({
    if (input$a1041 == "college"){
      data_1041_02() %>% filter(college %in% input$college1041) %>% arrange(desc(college))
    } else if (input$a1041 == "grade"){
      data_1041_02() %>% filter(grade %in% input$grade1041) %>% arrange(desc(grade))
    } else if (input$a1041 == "dept"){
      data_1041_02() %>% filter(dept %in% input$dept1041) %>% arrange(desc(dept))
    }
    else {
      data_1041_02()
    }
  })

  output$b1041 <- renderPlot({
    if (is.null(data_1041_final())) {
      return()
    }
    ggplot(data = data_1041_final(), aes_string(x = input$a1041, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
   })
  output$b1041_2 <- renderPlot({
    if (is.null(data_1041_02final())) {
      return()
    } 
    ggplot(data = data_1041_02final(), aes_string(x = 'keys', y = input$a1041, fill = 'Attnd_Rt')) +
      geom_tile() +
      scale_fill_gradient(high = "red", low = "white") +
      guides(fill = guide_legend(title.position="top"))
  })
  
  output$b1041_3 <- renderPlot({
    data_1041_03 %>%
      ggplot() +
      geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
      geom_line(aes(x = keys, y = percent, group = values)) +
      coord_flip() +
      guides(fill=guide_legend(title="present")) +
      facet_grid(.~values) +
      scale_fill_brewer(palette="Pastel1")
  })
  output$all_1041 <- DT::renderDataTable({
    DT::datatable(data = data_pornot,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  # Print data table if checked
  output$summarizetable_1041 <- DT::renderDataTable({
    DT::datatable(data = data_1041_final(),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })
  output$percenttable_1041 <- DT::renderDataTable({
    DT::datatable(data = data_1041_02final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable_1041 <- DT::renderDataTable({
    DT::datatable(data = data_1041_03,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  # Create scatterplot object the plotOutput function is expecting
  data_1042 <- reactive({  
    data_pornot %>%
      filter(term == "1042") %>%
      select(college, start, end, name, dept, grade, values) %>%
      group_by(!! rlang::sym(input$a1042), values) %>%
      count() %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1042)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      ungroup
  })
  data_1042_02 <- reactive({
    data_pornot %>%
      filter(term == "1042") %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(dummyp = if_else(present == "not", 0, 1)) %>%
      group_by(!! rlang::sym(input$a1042), keys) %>%
      mutate(Attnd_Rt = round(sum(dummyp)/n(), 2))
  }) 
  data_1042_03 <- data_pornot %>%
    filter(term == "1042") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = round(n/sum(n), 2))
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  data_1042_final <- reactive({
    if (input$a1042 == "college"){
      data_1042() %>% filter(college %in% input$college1042) %>% arrange(desc(college))
    } else if (input$a1042 == "grade"){
      data_1042() %>% filter(grade %in% input$grade1042) %>% arrange(desc(grade))
    } else if (input$a1042 == "dept"){
      data_1042() %>% filter(dept %in% input$dept1042) %>% arrange(desc(dept))
    }
    else {
      data_1042()
    }
  })
  data_1042_02final <- reactive({
    if (input$a1042 == "college"){
      data_1041_02() %>% filter(college %in% input$college1042) %>% arrange(desc(college))
    } else if (input$a1042 == "grade"){
      data_1041_02() %>% filter(grade %in% input$grade1042) %>% arrange(desc(grade))
    } else if (input$a1042 == "dept"){
      data_1041_02() %>% filter(dept %in% input$dept1042) %>% arrange(desc(dept))
    }
    else {
      data_1042_02()
    }
  })

  output$b1042 <- renderPlot({
    if (is.null(data_1042_final())) {
      return()
    }
    ggplot(data = data_1042_final(), aes_string(x = input$a1042, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$b1042_2 <- renderPlot({
    if (is.null(data_1042_02final())) {
      return()
    } 
    ggplot(data = data_1042_02final(), aes_string(x = 'keys', y = input$a1042, fill = 'Attnd_Rt')) +
      geom_tile() +
      scale_fill_gradient(high = "red", low = "white") +
      guides(fill = guide_legend(title.position="top"))
  })
  
  output$b1042_3 <- renderPlot({
    data_1042_03 %>%
      ggplot() +
      geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
      geom_line(aes(x = keys, y = percent, group = values)) +
      coord_flip() +
      guides(fill=guide_legend(title="present")) +
      facet_grid(.~values) +
      scale_fill_brewer(palette="Pastel1")
  })
  output$all_1042 <- DT::renderDataTable({
    DT::datatable(data = data_pornot,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  # Print data table if checked
  output$summarizetable_1042 <- DT::renderDataTable({
    DT::datatable(data = data_1042_final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$percenttable_1042 <- DT::renderDataTable({
    DT::datatable(data = data_1042_02final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable_1042 <- DT::renderDataTable({
    DT::datatable(data = data_1042_03,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  # Create scatterplot object the plotOutput function is expecting
  data_1051 <- reactive({  
    data_pornot %>%
      filter(term == "1051") %>%
      select(college, start, end, name, dept, grade, values) %>%
      group_by(!! rlang::sym(input$a1051), values) %>%
      count() %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1051)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      ungroup
  })
  data_1051_02 <- reactive({
    data_pornot %>%
      filter(term == "1051") %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(dummyp = if_else(present == "not", 0, 1)) %>%
      group_by(!! rlang::sym(input$a1051), keys) %>%
      mutate(Attnd_Rt = round(sum(dummyp)/n(), 2))
  }) 
  data_1051_03 <- data_pornot %>% filter(term == "1051") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = round(n/sum(n), 2)) %>%
  filter(values %in% c("present", "skip", "leave"))
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  data_1051_final <- reactive({
    if (input$a1051 == "college"){
      data_1051() %>% filter(college %in% input$college1051) %>% arrange(desc(college))
    } else if (input$a1051 == "grade"){
      data_1051() %>% filter(grade %in% input$grade1051) %>% arrange(desc(grade))
    } else if (input$a1051 == "dept"){
      data_1051() %>% filter(dept %in% input$dept1051) %>% arrange(desc(dept))
    }
    else {
      data_1051()
    }
  })
  data_1051_02final <- reactive({
    if (input$a1051 == "college"){
      data_1051_02() %>% filter(college %in% input$college1051) %>% arrange(desc(college))
    } else if (input$a1051 == "grade"){
      data_1051_02() %>% filter(grade %in% input$grade1051) %>% arrange(desc(grade))
    } else if (input$a1051 == "dept"){
      data_1051_02() %>% filter(dept %in% input$dept1051) %>% arrange(desc(dept))
    }
    else {
      data_1051_02()
    }
  })
  
  output$b1051 <- renderPlot({
    if (is.null(data_1051_final())) {
      return()
    }
    ggplot(data = data_1051_final(), aes_string(x = input$a1051, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$b1051_2 <- renderPlot({
    if (is.null(data_1051_02final())) {
      return()
    } 
    ggplot(data = data_1051_02final(), aes_string(x = 'keys', y = input$a1051, fill = 'Attnd_Rt')) +
      geom_tile() +
      scale_fill_gradient(high = "red", low = "white") +
      guides(fill = guide_legend(title.position="top"))
  })
  
  output$b1051_3 <- renderPlot({
    data_1051_03 %>% 
      ggplot() +
      geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
      geom_line(aes(x = keys, y = percent, group = values)) +
      coord_flip() +
      guides(fill=guide_legend(title="present")) +
      facet_grid(.~values) +
      scale_fill_brewer(palette="Pastel1")
  })
  output$all_1051 <- DT::renderDataTable({
    DT::datatable(data = data_pornot,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  # Print data table if checked
  output$summarizetable_1051 <- DT::renderDataTable({
    DT::datatable(data = data_1051_final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$percenttable_1051 <- DT::renderDataTable({
    DT::datatable(data = data_1051_02final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable_1051 <- DT::renderDataTable({
    DT::datatable(data = data_1051_03,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  # Create scatterplot object the plotOutput function is expecting
  data_1052 <- reactive({  
    data_pornot %>%
      filter(term == "1052") %>%
      select(college, start, end, name, dept, grade, values) %>%
      group_by(!! rlang::sym(input$a1052), values) %>%
      count() %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1052)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      ungroup
  })
  data_1052_02 <- reactive({
    data_pornot %>%
      filter(term == "1052") %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(dummyp = if_else(present == "not", 0, 1)) %>%
      group_by(!! rlang::sym(input$a1052), keys) %>%
      mutate(Attnd_Rt = round(sum(dummyp)/n(), 2))
  }) 
  data_1052_03 <- data_pornot %>% filter(term == "1052") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = round(n/sum(n), 2)) %>%
    filter(values %in% c("present", "skip", "leave"))
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  data_1052_final <- reactive({
    if (input$a1052 == "college"){
      data_1052() %>% filter(college %in% input$college1052) %>% arrange(desc(college))
    } else if (input$a1052 == "grade"){
      data_1052() %>% filter(grade %in% input$grade1052) %>% arrange(desc(grade))
    } else if (input$a1052 == "dept"){
      data_1052() %>% filter(dept %in% input$dept1052) %>% arrange(desc(dept))
    }
    else {
      data_1052()
    }
  })
  data_1052_02final <- reactive({
    if (input$a1052 == "college"){
      data_1052_02() %>% filter(college %in% input$college1052) %>% arrange(desc(college))
    } else if (input$a1052 == "grade"){
      data_1052_02() %>% filter(grade %in% input$grade1052) %>% arrange(desc(grade))
    } else if (input$a1052 == "dept"){
      data_1052_02() %>% filter(dept %in% input$dept1052) %>% arrange(desc(dept))
    }
    else {
      data_1052_02()
    }
  })
  
  output$b1052 <- renderPlot({
    if (is.null(data_1052_final())) {
      return()
    }
    ggplot(data = data_1052_final(), aes_string(x = input$a1052, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$b1052_2 <- renderPlot({
    if (is.null(data_1052_02final())) {
      return()
    } 
    ggplot(data = data_1052_02final(), aes_string(x = 'keys', y = input$a1052, fill = 'Attnd_Rt')) +
      geom_tile() +
      scale_fill_gradient(high = "red", low = "white") +
      guides(fill = guide_legend(title.position="top"))
  })
  
  output$b1052_3 <- renderPlot({
    data_1052_03 %>%
      ggplot() +
      geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
      geom_line(aes(x = keys, y = percent, group = values)) +
      coord_flip() +
      guides(fill=guide_legend(title="present")) +
      facet_grid(.~values) +
      scale_fill_brewer(palette="Pastel1")
  })
  output$all_1052 <- DT::renderDataTable({
    DT::datatable(data = data_pornot,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  # Print data table if checked
  output$summarizetable_1052 <- DT::renderDataTable({
    DT::datatable(data = data_1052_final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$percenttable_1052 <- DT::renderDataTable({
    DT::datatable(data = data_1052_02final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable_1052 <- DT::renderDataTable({
    DT::datatable(data = data_1052_03,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  # Create scatterplot object the plotOutput function is expecting
  data_1061 <- reactive({  
    data_pornot %>%
      filter(term == "1061") %>%
      select(college, start, end, name, dept, grade, values) %>%
      group_by(!! rlang::sym(input$a1061), values) %>%
      count() %>%
      ungroup %>%
      group_by(!! rlang::sym(input$a1061)) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      arrange(desc(n)) %>%
      ungroup
  })
  data_1061_02 <- reactive({
    data_pornot %>%
      filter(term == "1061") %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(dummyp = if_else(present == "not", 0, 1)) %>%
      group_by(!! rlang::sym(input$a1061), keys) %>%
      mutate(Attnd_Rt = round(sum(dummyp)/n(), 2))
  }) 
  data_1061_03 <- data_pornot %>% filter(term == "1061") %>%
    group_by(keys, values) %>%
    count() %>%
    ungroup() %>%
    group_by(keys) %>%
    mutate(percent = round(n/sum(n), 2))
  
  # Make sure columns are correct for data set (when data set changes, the
  # columns will initially be for the previous data set)
  data_1061_final <- reactive({
    if (input$a1061 == "college"){
      data_1061() %>% filter(college %in% input$college1061) %>% arrange(desc(college))
    } else if (input$a1061 == "grade"){
      data_1061() %>% filter(grade %in% input$grade1061) %>% arrange(desc(grade))
    } else if (input$a1061 == "dept"){
      data_1061() %>% filter(dept %in% input$dept1061) %>% arrange(desc(dept))
    }
    else {
      data_1061()
    }
  })
  data_1061_02final <- reactive({
    if (input$a1061 == "college"){
      data_1061_02() %>% filter(college %in% input$college1061) %>% arrange(desc(college))
    } else if (input$a1061 == "grade"){
      data_1061_02() %>% filter(grade %in% input$grade1061) %>% arrange(desc(grade))
    } else if (input$a1061 == "dept"){
      data_1061_02() %>% filter(dept %in% input$dept1061) %>% arrange(desc(dept))
    }
    else {
      data_1061_02()
    }
  })
  data_summary <- reactive({
    data_pornot %>% filter(college %in% input$collegedownload,
                           grade %in% input$gradedownload,
                           term %in% input$termdownload) %>%
      arrange(desc(college))
  })
  output$b1061 <- renderPlot({
    if (is.null(data_1061_final())) {
      return()
    }
    ggplot(data = data_1061_final(), aes_string(x = input$a1061, y = 'percent', fill = 'values')) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1")
  })
  output$b1061_2 <- renderPlot({
    if (is.null(data_1061_02final())) {
      return()
    } 
    ggplot(data = data_1061_02final(), aes_string(x = 'keys', y = input$a1061, fill = 'Attnd_Rt')) +
      geom_tile() +
      scale_fill_gradient(high = "red", low = "white") +
      guides(fill = guide_legend(title.position="top"))
  })
  
  output$b1061_3 <- renderPlot({
    data_1061_03 %>%
      ggplot() +
      geom_bar(aes(x = keys, y = percent, fill = factor(values, levels = c("skip","leave","present"))), stat = "identity") +
      geom_line(aes(x = keys, y = percent, group = values)) +
      coord_flip() +
      guides(fill=guide_legend(title="present")) +
      facet_grid(.~values) +
      scale_fill_brewer(palette="Pastel1")
  })
  output$all_1061 <- DT::renderDataTable({
    DT::datatable(data = data_pornot,
                  options = list(pageLength = 10),
                  rownames = FALSE)
    
  })
  # Print data table if checked
  output$summarizetable_1061 <- DT::renderDataTable({
    DT::datatable(data = data_1061_final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$percenttable_1061 <- DT::renderDataTable({
    DT::datatable(data = data_1061_02final(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable_1061 <- DT::renderDataTable({
    DT::datatable(data = data_1061_03,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$b1 <- renderPlot({
    data_pornot %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(college = factor(college), term = factor(term)) %>%
      group_by(college, term, present) %>%
      count() %>%
      ungroup %>%
      group_by(college, term) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      filter(present == "present") %>%
      ungroup %>%
      ggplot(aes(term, percent)) +
      geom_col(width = 0.7, fill="lightblue", colour="black") +
      #geom_line(aes(term, percent, group = college)) +
      facet_wrap(~college, ncol = 4) +
      theme(strip.background = element_rect(fill="grey"))
  })
  output$b2 <- renderPlot({
    data_pornot %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(grade = factor(grade), term = factor(term)) %>%
      group_by(grade, term, present) %>%
      count() %>%
      ungroup %>%
      group_by(grade, term) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      filter(present == "present") %>%
      ungroup %>%
      ggplot(aes(term, percent)) +
      geom_col(width = 0.7, fill="lightblue", colour="black") +
      #geom_line(aes(term, percent, group = college)) +
      facet_wrap(~grade, ncol = 5) +
      theme(strip.background = element_rect(fill="grey"))
  })
  output$b3 <- renderPlot({
    data_pornot %>%
      select(college, grade, dept, name, term, keys, present) %>%
      mutate(college = factor(college), term = factor(term)) %>%
      group_by(keys, term, present) %>%
      count() %>%
      ungroup %>%
      group_by(keys, term) %>%
      mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
      filter(present == "present") %>%
      ungroup %>%
      ggplot(aes(keys, percent)) +
      geom_col(width = 0.7, fill="lightblue", colour="black") +
      #geom_line(aes(term, percent, group = college)) +
      facet_wrap(~term, ncol = 5, scale="free_x") +
      theme(strip.background = element_rect(fill="grey"))
  })
  # Print data table if checked
  output$summarizetable <- DT::renderDataTable({
    DT::datatable(data = data_pornot %>%
                    select(college, grade, dept, name, term, keys, present) %>%
                    mutate(college = factor(college), term = factor(term)) %>%
                    group_by(college, term, present) %>%
                    count() %>%
                    ungroup %>%
                    group_by(college, term) %>%
                    mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
                    filter(present == "present") %>%
                    ungroup,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$percenttable <- DT::renderDataTable({
    DT::datatable(data = data_pornot %>%
                    select(college, grade, dept, name, term, keys, present) %>%
                    mutate(grade = factor(grade), term = factor(term)) %>%
                    group_by(grade, term, present) %>%
                    count() %>%
                    ungroup %>%
                    group_by(grade, term) %>%
                    mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
                    filter(present == "present") %>%
                    ungroup,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$timetable <- DT::renderDataTable({
    DT::datatable(data = data_pornot %>%
                    select(college, grade, dept, name, term, keys, present) %>%
                    mutate(college = factor(college), term = factor(term)) %>%
                    group_by(keys, term, present) %>%
                    count() %>%
                    ungroup %>%
                    group_by(keys, term) %>%
                    mutate(percent = round(n/sum(n), 2), total = sum(n)) %>%
                    filter(present == "present") %>%
                    ungroup,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$b4 <- renderPlot({
    data_lm %>%
      ggplot(aes(void_rate, Attnd_Rate)) +
      geom_point()
  })
  output$b5 <- renderPlot({
    data_lm %>%
      ggplot(aes(support_rate, Attnd_Rate)) +
      geom_point()
  })
  output$b6 <- renderPlot({
    data_lm %>%
      ggplot(aes(vote_rate, Attnd_Rate)) +
      geom_point()
  })
  output$votetable <- DT::renderDataTable({
    DT::datatable(data = data_lm,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  output$downloadtable <- DT::renderDataTable({
    DT::datatable(data = data_summary(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("NTUSC", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_summary(), file) #, row.names = FALSE
    }
  )
  observe({
    if(input$term1041 == "timePlot" | input$term1041 == "all data" ) {
    shinyjs::hide(id = "Sidebar1041")}
    else {shinyjs::show(id = "Sidebar1041")}
  })
  observe({
    if(input$term1041 == "timePlot" | input$term1041 == "all data" ) {
      shinyjs::hide(id = "Sidebar1041")}
    else {shinyjs::show(id = "Sidebar1041")}
  })
  observe({
    if(input$term1042 == "timePlot" | input$term1042 == "all data" ) {
      shinyjs::hide(id = "Sidebar1042")}
    else {shinyjs::show(id = "Sidebar1042")}
  })
  observe({
    if(input$term1051 == "timePlot" | input$term1051 == "all data" ) {
      shinyjs::hide(id = "Sidebar1051")}
    else {shinyjs::show(id = "Sidebar1051")}
  })
  observe({
    if(input$term1052 == "timePlot" | input$term1052 == "all data" ) {
      shinyjs::hide(id = "Sidebar1052")}
    else {shinyjs::show(id = "Sidebar1052")}
  })
  observe({
    if(input$term1061 == "timePlot" | input$term1061 == "all data" ) {
      shinyjs::hide(id = "Sidebar1061")}
    else {shinyjs::show(id = "Sidebar1061")}
  })
  observe({
    if(input$summary == "download data") {
      shinyjs::show(id = "Sidebarsummary")}
    else {shinyjs::hide(id = "Sidebarsummary")}
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)