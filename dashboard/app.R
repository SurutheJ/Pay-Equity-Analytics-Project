# Pay Equity Analytics Dashboard
# Interactive Shiny Dashboard for Gender Pay Gap Analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Load and prepare data
data <- read.csv("../data/glassdoor-gender-pay-gap.csv")
data$Gender <- factor(data$Gender)
data$Dept <- factor(data$Dept)
data$Education <- factor(data$Education, levels = c("High School", "College", "Masters", "PhD"))
data$Salary <- data$BasePay + data$Bonus

# Remove outliers (same as analysis)
data <- data %>%
  filter(between(Salary,
                 quantile(Salary, 0.25) - 1.4 * IQR(Salary),
                 quantile(Salary, 0.75) + 1.4 * IQR(Salary)))

# Define color palette
colors <- c("Female" = "#E76BF3", "Male" = "#00BFC4")

# UI Definition
ui <- dashboardPage(
  skin = "purple",
  
  # Header
  dashboardHeader(title = "Pay Equity Analytics"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Salary Analysis", tabName = "salary", icon = icon("dollar-sign")),
      menuItem("Job & Department", tabName = "job_dept", icon = icon("briefcase")),
      menuItem("Education Impact", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Top Earners", tabName = "top_earners", icon = icon("trophy")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      hr(),
      h4("  Filters", style = "color: white; padding-left: 15px;"),
      selectInput("dept_filter", "Department:",
                  choices = c("All", levels(data$Dept)),
                  selected = "All"),
      selectInput("job_filter", "Job Title:",
                  choices = c("All", unique(data$JobTitle)),
                  selected = "All")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #605ca8; }
        .info-box { min-height: 90px; }
        .small-box { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_employees", width = 3),
          valueBoxOutput("avg_salary", width = 3),
          valueBoxOutput("male_avg", width = 3),
          valueBoxOutput("female_avg", width = 3)
        ),
        fluidRow(
          valueBoxOutput("pay_gap_percent", width = 4),
          valueBoxOutput("female_percent", width = 4),
          valueBoxOutput("top_earner_female", width = 4)
        ),
        fluidRow(
          box(title = "Salary Distribution by Gender", status = "primary", solidHeader = TRUE,
              plotlyOutput("overview_boxplot"), width = 6),
          box(title = "Employee Count by Gender", status = "primary", solidHeader = TRUE,
              plotlyOutput("gender_pie"), width = 6)
        ),
        fluidRow(
          box(title = "Key Findings", status = "warning", solidHeader = TRUE, width = 12,
              HTML("
                <h4>Research Findings Summary:</h4>
                <ul>
                  <li><strong>RQ1:</strong> Job Title significantly affects salary, but Gender alone shows limited direct effect when controlling for other factors.</li>
                  <li><strong>RQ2:</strong> Organizational factors (Job Title, Department) explain more salary variation than demographic factors.</li>
                  <li><strong>RQ3:</strong> Education's return on investment is <strong>equal</strong> across genders - no differential benefit.</li>
                  <li><strong>RQ4:</strong> Structural factors like job role dominate top earner status more than gender.</li>
                  <li><strong>RQ5:</strong> Gender does not significantly predict job title allocation.</li>
                </ul>
              "))
        )
      ),
      
      # Salary Analysis Tab
      tabItem(tabName = "salary",
        fluidRow(
          box(title = "Salary Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("salary_histogram"), width = 6),
          box(title = "Salary by Gender (Box Plot)", status = "primary", solidHeader = TRUE,
              plotlyOutput("salary_boxplot"), width = 6)
        ),
        fluidRow(
          box(title = "Salary vs Age", status = "info", solidHeader = TRUE,
              plotlyOutput("salary_age_scatter"), width = 6),
          box(title = "Salary vs Seniority", status = "info", solidHeader = TRUE,
              plotlyOutput("salary_seniority_scatter"), width = 6)
        )
      ),
      
      # Job & Department Tab
      tabItem(tabName = "job_dept",
        fluidRow(
          box(title = "Distribution of Job Titles by Gender", status = "primary", solidHeader = TRUE,
              plotlyOutput("job_title_bar"), width = 12)
        ),
        fluidRow(
          box(title = "Average Salary by Job Title", status = "info", solidHeader = TRUE,
              plotlyOutput("job_salary_bar"), width = 6),
          box(title = "Average Salary by Department", status = "info", solidHeader = TRUE,
              plotlyOutput("dept_salary_bar"), width = 6)
        )
      ),
      
      # Education Impact Tab
      tabItem(tabName = "education",
        fluidRow(
          box(title = "Salary by Education Level and Gender", status = "primary", solidHeader = TRUE,
              plotlyOutput("education_salary_line"), width = 8),
          box(title = "Interpretation", status = "warning", solidHeader = TRUE, width = 4,
              HTML("
                <h4>Education ROI Analysis</h4>
                <p>The <strong>parallel lines</strong> indicate that education provides 
                <strong>equal return on investment</strong> for both genders.</p>
                <p>Higher education correlates with higher salary regardless of gender, 
                suggesting no differential benefit or penalty.</p>
                <p><em>Statistical finding: No significant interaction between Gender and Education (p > 0.05)</em></p>
              "))
        ),
        fluidRow(
          box(title = "Education Distribution by Gender", status = "info", solidHeader = TRUE,
              plotlyOutput("education_dist"), width = 6),
          box(title = "Performance vs Salary by Gender", status = "info", solidHeader = TRUE,
              plotlyOutput("perf_salary"), width = 6)
        )
      ),
      
      # Top Earners Tab
      tabItem(tabName = "top_earners",
        fluidRow(
          box(title = "Top 10% Earners Analysis", status = "primary", solidHeader = TRUE,
              plotlyOutput("top_earner_plot"), width = 8),
          box(title = "Top Earner Statistics", status = "info", solidHeader = TRUE, width = 4,
              tableOutput("top_earner_stats"))
        ),
        fluidRow(
          box(title = "Factors Affecting Top Earner Status (Odds Ratios)", status = "warning", solidHeader = TRUE,
              plotlyOutput("odds_ratio_plot"), width = 12)
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(title = "Interactive Data Table", status = "primary", solidHeader = TRUE,
              DTOutput("data_table"), width = 12)
        ),
        fluidRow(
          box(title = "Summary Statistics", status = "info", solidHeader = TRUE,
              verbatimTextOutput("data_summary"), width = 12)
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data
    if (input$dept_filter != "All") {
      df <- df %>% filter(Dept == input$dept_filter)
    }
    if (input$job_filter != "All") {
      df <- df %>% filter(JobTitle == input$job_filter)
    }
    df
  })
  
  # Value Boxes
  output$total_employees <- renderValueBox({
    valueBox(
      nrow(filtered_data()), "Total Employees",
      icon = icon("users"), color = "purple"
    )
  })
  
  output$avg_salary <- renderValueBox({
    valueBox(
      paste0("$", format(round(mean(filtered_data()$Salary)), big.mark = ",")),
      "Average Salary",
      icon = icon("dollar-sign"), color = "green"
    )
  })
  
  output$male_avg <- renderValueBox({
    avg <- filtered_data() %>% filter(Gender == "Male") %>% summarise(avg = mean(Salary)) %>% pull(avg)
    valueBox(
      paste0("$", format(round(avg), big.mark = ",")),
      "Male Avg Salary",
      icon = icon("mars"), color = "blue"
    )
  })
  
  output$female_avg <- renderValueBox({
    avg <- filtered_data() %>% filter(Gender == "Female") %>% summarise(avg = mean(Salary)) %>% pull(avg)
    valueBox(
      paste0("$", format(round(avg), big.mark = ",")),
      "Female Avg Salary",
      icon = icon("venus"), color = "fuchsia"
    )
  })
  
  output$pay_gap_percent <- renderValueBox({
    male_avg <- filtered_data() %>% filter(Gender == "Male") %>% summarise(avg = mean(Salary)) %>% pull(avg)
    female_avg <- filtered_data() %>% filter(Gender == "Female") %>% summarise(avg = mean(Salary)) %>% pull(avg)
    gap <- round((male_avg - female_avg) / male_avg * 100, 1)
    valueBox(
      paste0(gap, "%"),
      "Raw Pay Gap",
      icon = icon("percentage"), color = ifelse(gap > 5, "red", "green")
    )
  })
  
  output$female_percent <- renderValueBox({
    pct <- round(sum(filtered_data()$Gender == "Female") / nrow(filtered_data()) * 100, 1)
    valueBox(
      paste0(pct, "%"),
      "Female Workforce",
      icon = icon("venus"), color = "purple"
    )
  })
  
  output$top_earner_female <- renderValueBox({
    top_threshold <- quantile(filtered_data()$Salary, 0.90)
    top_earners <- filtered_data() %>% filter(Salary >= top_threshold)
    pct <- round(sum(top_earners$Gender == "Female") / nrow(top_earners) * 100, 1)
    valueBox(
      paste0(pct, "%"),
      "Female Top 10% Earners",
      icon = icon("trophy"), color = "yellow"
    )
  })
  
  # Overview Plots
  output$overview_boxplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Gender, y = Salary, fill = Gender)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = colors) +
      labs(x = "", y = "Salary ($)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$gender_pie <- renderPlotly({
    df <- filtered_data() %>% count(Gender)
    plot_ly(df, labels = ~Gender, values = ~n, type = 'pie',
            marker = list(colors = c("#E76BF3", "#00BFC4"))) %>%
      layout(showlegend = TRUE)
  })
  
  # Salary Analysis Plots
  output$salary_histogram <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Salary, fill = Gender)) +
      geom_histogram(binwidth = 5000, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = colors) +
      labs(x = "Salary ($)", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$salary_boxplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Gender, y = Salary, fill = Gender)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(alpha = 0.2, width = 0.2) +
      scale_fill_manual(values = colors) +
      labs(x = "", y = "Salary ($)") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$salary_age_scatter <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Age, y = Salary, color = Gender)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = colors) +
      labs(x = "Age", y = "Salary ($)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$salary_seniority_scatter <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Seniority, y = Salary, color = Gender)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = colors) +
      labs(x = "Seniority (Years)", y = "Salary ($)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Job & Department Plots
  output$job_title_bar <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = JobTitle, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(x = "Job Title", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$job_salary_bar <- renderPlotly({
    df <- filtered_data() %>%
      group_by(JobTitle, Gender) %>%
      summarise(AvgSalary = mean(Salary), .groups = "drop")
    
    p <- ggplot(df, aes(x = reorder(JobTitle, AvgSalary), y = AvgSalary, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(x = "Job Title", y = "Average Salary ($)") +
      coord_flip() +
      theme_minimal()
    ggplotly(p)
  })
  
  output$dept_salary_bar <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Dept, Gender) %>%
      summarise(AvgSalary = mean(Salary), .groups = "drop")
    
    p <- ggplot(df, aes(x = reorder(Dept, AvgSalary), y = AvgSalary, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(x = "Department", y = "Average Salary ($)") +
      coord_flip() +
      theme_minimal()
    ggplotly(p)
  })
  
  # Education Plots
  output$education_salary_line <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Education, Gender) %>%
      summarise(AvgSalary = mean(Salary), SE = sd(Salary)/sqrt(n()), .groups = "drop")
    
    p <- ggplot(df, aes(x = Education, y = AvgSalary, color = Gender, group = Gender)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = AvgSalary - SE, ymax = AvgSalary + SE), width = 0.2) +
      scale_color_manual(values = colors) +
      labs(x = "Education Level", y = "Average Salary ($)",
           title = "Does Education Pay Off Differently by Gender?") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$education_dist <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Education, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(x = "Education Level", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$perf_salary <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = factor(PerfEval), y = Salary, fill = Gender)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = colors) +
      labs(x = "Performance Evaluation Score", y = "Salary ($)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Top Earners Plots
  output$top_earner_plot <- renderPlotly({
    df <- filtered_data() %>%
      mutate(TopEarner = ifelse(Salary >= quantile(Salary, 0.90), "Top 10%", "Others"))
    
    p <- ggplot(df, aes(x = TopEarner, fill = Gender)) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "", y = "Proportion", title = "Gender Distribution: Top Earners vs Others") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$top_earner_stats <- renderTable({
    df <- filtered_data() %>%
      mutate(TopEarner = Salary >= quantile(Salary, 0.90))
    
    stats <- df %>%
      group_by(Gender) %>%
      summarise(
        `Total Count` = n(),
        `Top 10% Count` = sum(TopEarner),
        `Top 10% Rate` = paste0(round(sum(TopEarner)/n() * 100, 1), "%")
      )
    stats
  })
  
  output$odds_ratio_plot <- renderPlotly({
    # Pre-computed odds ratios from the analysis
    df_or <- data.frame(
      Factor = c("Manager", "Software Engineer", "Sales Dept", "Seniority", 
                 "PhD", "Masters", "Engineering Dept", "Performance", "Age", "Male Gender"),
      OR = c(469.5, 19.2, 13.3, 6.3, 4.6, 3.6, 7.3, 1.5, 1.2, 0.35),
      Group = c("Job", "Job", "Dept", "Experience", "Education", "Education", 
                "Dept", "Performance", "Demographics", "Demographics")
    )
    df_or$Factor <- factor(df_or$Factor, levels = df_or$Factor[order(df_or$OR)])
    
    p <- ggplot(df_or, aes(x = OR, y = Factor, fill = Group)) +
      geom_col() +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      scale_x_log10() +
      labs(x = "Odds Ratio (log scale)", y = "",
           title = "Factors Affecting Top Earner Status",
           subtitle = "OR > 1 increases odds; OR < 1 decreases odds") +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggplotly(p)
  })
  
  # Data Explorer
  output$data_table <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 15, scrollX = TRUE),
              filter = "top")
  })
  
  output$data_summary <- renderPrint({
    summary(filtered_data() %>% select(Age, Seniority, PerfEval, BasePay, Bonus, Salary))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
