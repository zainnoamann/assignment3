# Set CRAN mirror
options(repos = "https://cloud.r-project.org/")

# Install packages (run once if not installed)
install.packages("http://cran.r-project.org/src/contrib/Archive/curl/curl_6.2.3.tar.gz", repos=NULL, type="source")
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(plotly)) install.packages("plotly")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readxl)) install.packages("readxl")
if (!require(tidyr)) install.packages("tidyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyverse)
library(stringr)

# Load datasets with skip = 1 to bypass title rows
cpi <- read_excel("All groups CPI, Australia, quarterly and annual movement (%).xlsx", skip = 1)
wpi <- read_excel("All sector WPI, quarterly and annual movement (%), seasonally adjusted (a).xlsx", skip = 1)
spending <- read_excel("Household spending, current price, seasonally adjusted estimate.xlsx", skip = 1)
unemployment <- read_excel("Unemployment rate-2.xlsx", skip = 1)

# Clean CPI
cpi <- cpi %>%
  rename(Quarter = 1, CPI_Quarterly = `Change from previous quarter (%)`, CPI_Annual = `Annual change (%)`) %>%
  mutate(Date = as.Date(paste0(Quarter, "-01"), format = "%b-%y-%d")) %>%
  select(Date, CPI_Quarterly, CPI_Annual)

# Clean WPI (filter to start from Mar-2016)
wpi <- wpi %>%
  rename(Quarter = 1, WPI_Quarterly = `Quarterly (%)`, WPI_Annual = `Annual (%)`) %>%
  mutate(Date = as.Date(paste0(Quarter, "-01"), format = "%b-%y-%d")) %>%
  filter(Date >= as.Date("2016-03-01")) %>%  # Filter to start from Mar-2016
  select(Date, WPI_Quarterly, WPI_Annual)

# Clean Spending
spending <- spending %>%
  rename(Month = 1, Spending_Monthly = `Monthly (%)`, Spending_Annual = `Through the year (%)`) %>%
  mutate(Date = as.Date(paste0(Month, "-01"), format = "%b-%Y-%d")) %>%
  select(Date, Spending_Monthly, Spending_Annual)

# Clean Unemployment
unemployment <- unemployment %>%
  rename(Month = 1, Unemployment_Trend = `Trend (%)`, Unemployment_SA = `Seasonally adjusted (%)`) %>%
  mutate(Date = as.Date(paste0(Month, "-01"), format = "%b-%y-%d")) %>%
  select(Date, Unemployment_Trend, Unemployment_SA)

# Load and clean the new dataset
spending_by_category <- read_excel("Household spending, percentage change from previous month, current price, seasonally adjusted.xlsx", skip = 1)

# Inspect raw data to verify categories (add this for debugging)
print("Raw spending_by_category data:")
print(head(spending_by_category))

# Clean the dataset with type conversion and category mapping
spending_by_category <- spending_by_category %>%
  rename(Raw_Category = 1, Feb_2025 = `Feb-2025 (%)`, Mar_2025 = `Mar-2025 (%)`, Apr_2025 = `Apr-2025 (%)`) %>%
  mutate(across(c(Feb_2025, Mar_2025, Apr_2025), as.numeric)) %>%  # Convert to numeric
  # Map the garbled categories to correct names (adjust based on your data)
  mutate(Category = case_when(
    grepl("Food", Raw_Category, ignore.case = TRUE) ~ "Food",
    grepl("Alcoholic|beverages|tobacco", Raw_Category, ignore.case = TRUE) ~ "Alcoholic beverages and tobacco",
    grepl("Clothing|footwear", Raw_Category, ignore.case = TRUE) ~ "Clothing and footwear",
    grepl("Furnishings|household", Raw_Category, ignore.case = TRUE) ~ "Furnishings and household equipment",
    grepl("Health", Raw_Category, ignore.case = TRUE) ~ "Health",
    grepl("Transport", Raw_Category, ignore.case = TRUE) ~ "Transport",
    grepl("Recreation|culture", Raw_Category, ignore.case = TRUE) ~ "Recreation and culture",
    grepl("Hotels|cafes|restaurants", Raw_Category, ignore.case = TRUE) ~ "Hotels, cafes and restaurants",
    grepl("Miscellaneous|goods|services", Raw_Category, ignore.case = TRUE) ~ "Miscellaneous goods and services",
    grepl("Total", Raw_Category, ignore.case = TRUE) ~ "Total",
    TRUE ~ Raw_Category  # Default to raw name if no match
  )) %>%
  pivot_longer(cols = c(Feb_2025, Mar_2025, Apr_2025), 
               names_to = "Month", 
               values_to = "Percentage_Change") %>%
  mutate(Month = factor(Month, levels = c("Feb_2025", "Mar_2025", "Apr_2025"), 
                        labels = c("Feb-2025", "Mar-2025", "Apr-2025"))) %>%
  select(Category, Month, Percentage_Change)

# Reorder categories to place "Total" at the end
spending_by_category$Category <- factor(spending_by_category$Category, 
                                        levels = c("Food", "Alcoholic beverages and tobacco", "Clothing and footwear", 
                                                   "Furnishings and household equipment", "Health", "Transport", 
                                                   "Recreation and culture", "Hotels, cafes and restaurants", 
                                                   "Miscellaneous goods and services", "Total"))
spending_by_category$Category <- str_wrap(spending_by_category$Category, width = 20)

# Verify the cleaned datasets
head(cpi)
head(wpi)
head(spending)
head(unemployment)
head(spending_by_category)

# Create visualizations
# Line chart: CPI vs. WPI
p1 <- ggplot() +
  geom_line(data = cpi, aes(x = Date, y = CPI_Annual, color = "CPI"), size = 1) +
  geom_line(data = wpi, aes(x = Date, y = WPI_Annual, color = "WPI"), size = 1) +
  labs(title = "Inflation vs. Wage Growth (2015–2025)", x = "Year", y = "Annual Change (%)") +
  scale_color_manual(values = c("CPI" = "#1f77b4", "WPI" = "#ff7f0e")) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")  # Improve x-axis readability
p1_interactive <- ggplotly(p1)

# Line chart: Unemployment
p2 <- ggplot(unemployment, aes(x = Date, y = Unemployment_SA)) +
  geom_line(color = "#2ca02c") +
  labs(title = "Unemployment Rate (2015–2025)", x = "Year", y = "Rate (%)") +
  theme_minimal()
p2_interactive <- ggplotly(p2)

# Bar chart: Household Spending
p3 <- ggplot(spending, aes(x = Date, y = Spending_Annual)) +
  geom_bar(stat = "identity", fill = "#d62728") +
  labs(title = "Household Spending Annual Change (2020–2025)", x = "Year", y = "Annual Change (%)") +
  theme_minimal()
p3_interactive <- ggplotly(p3)

# Bar chart: Household Spending by Category
p4 <- ggplot(spending_by_category, aes(x = Category, y = Percentage_Change, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Household Spending Changes by Category (Feb–Apr 2025)", 
       x = "Category", y = "Percentage Change from Previous Month (%)") +
  scale_fill_manual(values = c("Feb-2025" = "#1f77b4", "Mar-2025" = "#ff7f0e", "Apr-2025" = "#2ca02c")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p4_interactive <- ggplotly(p4)

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Cost-of-Living Crisis in Australia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Employment & Spending", tabName = "emp_spend"),
      menuItem("Spending by Category", tabName = "spending_category")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          white-space: normal !important;
          text-align: center;
          font-size: 16px; /* Adjust font size if needed */
          padding: 5px 0;
        }
        .main-header .navbar {
          min-height: 60px; /* Increase header height if needed */
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(plotlyOutput("cpi_wpi_plot"), width = 12),
                box(textOutput("overview_text"), width = 12)
              )),
      tabItem(tabName = "emp_spend",
              fluidRow(
                box(plotlyOutput("unemployment_plot"), width = 6),
                box(plotlyOutput("spending_plot"), width = 6),
                box(textOutput("emp_spend_text"), width = 12)
              )),
      tabItem(tabName = "spending_category",
              fluidRow(
                box(plotlyOutput("spending_category_plot"), width = 12, height = "600px"),
                box(textOutput("category_text"), width = 12)
              ))
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$cpi_wpi_plot <- renderPlotly({ p1_interactive })
  output$overview_text <- renderText({
    "Rising inflation has outpaced wage growth, squeezing Australian households from 2015 to 2025. \nSource: ABS (Australian Bureau of Statistics)"
  })
  output$unemployment_plot <- renderPlotly({ p2_interactive })
  output$spending_plot <- renderPlotly({ p3_interactive })
  output$spending_category_plot <- renderPlotly({ p4_interactive })
  output$category_text <- renderText({
    "Recent changes in household spending by category reflect varying impacts of the cost-of-living crisis in early 2025. \nSource: ABS (Australian Bureau of Statistics)"
  })
  output$emp_spend_text <- renderText({
    "As unemployment rose in 2020, household spending dropped sharply, highlighting how job loss directly reduced consumer spending; as employment recovered, spending gradually increased in response. \nSource: ABS (Australian Bureau of Statistics)"
  })
}

# Run the Shiny app
shinyApp(ui, server)