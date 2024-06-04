library(shiny)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geodata)
source("helpers.R")

nationality_gender <- read.table("data/nationality_gender.csv", skip = 2, 
                                 sep = ";",
                                 col.names = c("year", "state", "German_Male", "German_Female", "German_Total", 
                                               "Foreign_Male", "Foreign_Female", "Foreign_Total", 
                                               "Total_Male", "Total_Female", "Total_Total"), 
                                 colClasses = c("numeric", "character", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

foreigners_data <- nationality_gender %>% 
  select(Foreign_Total, Total_Total, year)

gender_data <- nationality_gender %>% 
  select(Total_Male, Total_Female, year) %>% 
  rename(Male = Total_Male, Female = Total_Female) %>% 
  group_by(year) %>%
  summarize_all(sum)

age_data <- read.table("data/age.csv", skip=6,
                       sep = ";",
                       fill=TRUE,
                       col.names = c("year", "age_group", "Baden-Württemberg","Bayern", "Berlin", "Brandenburg",
                                     "Bremen", "Hamburg", "Hessen",	"Mecklenburg-Vorpommern", "Niedersachsen",
                                     "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt",
                                     "Schleswig-Holstein", "Thüringen"),
                       colClasses = c("character", "character", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# Removing rows with full NAs (last few rows)
age_data <- age_data[complete.cases(age_data), ]
# Keeping only year in date
age_data$year <- format(as.Date(age_data$year, format="%d.%m.%Y"),"%Y")
# Remove rows with total data
age_data<-subset(age_data, age_group!="Insgesamt" & age_group!="Total")
# Sum different states into one column
age_data$num_of_people = rowSums(age_data[,-(1:2)])

# Categorize age groups
for (value in unique(age_data$age_group)) {
  if (value == "90 Jahre und mehr"){
    
    age_data$age_category[age_data$age_group == value] <- "90+"
    
  } else if (value == "unter 1 Jahr") {
    
    age_data$age_category[age_data$age_group == value] <- "0-9"
    
  } else {
    num <- as.numeric(strsplit(value, "-")[[1]][1])
    cat <- dplyr::case_when(
      num < 10             ~ "0-9",
      num >= 10 & num < 20 ~ "10-19",
      num >= 20 & num < 30 ~ "20-29",
      num >= 30 & num < 40 ~ "30-39",
      num >= 40 & num < 50 ~ "40-49",
      num >= 50 & num < 60 ~ "50-59",
      num >= 60 & num < 70 ~ "60-69",
      num >= 70 & num < 80 ~ "70-79",
      num >= 80 & num < 90 ~ "80-89",
    )
    
    age_data$age_category[age_data$age_group == value] <- cat
    
  }}

# Aggregate population by age group
age_data <- aggregate(num_of_people~year+age_category, data=age_data, FUN=sum) 
# Selecting only used columns
age_data <- age_data[,c("year", "num_of_people", "age_category")]

ui <- fluidPage(
    titlePanel("Analysis of the population of Germany", windowTitle = "Population of Germany"),
    titlePanel("1) Percentage of foreigners per state"),

    sidebarLayout(
        sidebarPanel(
          helpText("How did the percentage of foreigners in the population of Germany vary between the states, and were there any noticeable geographic patterns or trends in these differences?"),
          
          selectInput("year", "select the year:", 
                      choices = c("2000", "2001", "2002", "2003", "2004", "2005", 
                      "2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                      "2020", "2021"),
                      selected = "2021"),
          sliderInput("max_percent", "select upper percentage bound:", min = 10, max = 30, value = 16)
        
        ),
        mainPanel(
           plotOutput("map")
        )
    ),
      
    titlePanel("2) Male and Female population over time"),  
    
    sidebarLayout(
        sidebarPanel(
          helpText("How have the male and female population of Germany changed over time, and how do they compare to eachother?"),
          sliderInput("time_period", "select the time period:", min = 2000, 
                      max = 2021, value = c(2000, 2021), sep = "")
        ),
        mainPanel(
          plotOutput("genders")
        )
    ),
    
    titlePanel("3) Distribution of age groups"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("What was the distribution of the population of Germany across different age groups?"),
        selectInput("age_year_select", "select the year:",
                    choices = c("2000", "2001", "2002", "2003", "2004", "2005",
                                "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                "2020", "2021"),
                    selected = "2021"
        ),
      ),
      mainPanel(
        plotOutput("age")
      )
    )
)


# Define the server logic
server <- function(input, output) {
  output$map <- renderPlot({
    # Subset the data for the selected year
    year_data <- subset(foreigners_data, year == input$year)
    
    # Calculate the percentage of foreigners in the population for each state
    year_data$foreign_pct <- year_data$Foreign_Total / year_data$Total_Total * 100
    print(year_data$foreign_pct)
    gadm <- readRDS("data/gadm/gadm41_DEU_1_pk.rds")
    
    # Create a choropleth map with the `percent_map` function
    max = input$max_percent
    percent_map(var = year_data$foreign_pct, "% Foreign", min = 0, max = max)
  })

  # Create the line chart
  output$genders <- renderPlot({
    # Subset the data to only include the selected time period
    years_data <- subset(gender_data, year >= input$time_period[1] & year <= input$time_period[2])
    long_form_data <- years_data %>%
      gather(key = "gender", value = "population", Male:Female)
  
    # line charts
    ggplot(long_form_data, aes(x=year, y=population, color=gender)) + 
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(input$time_period[1], input$time_period[2])) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_color_manual(values = c("Female" = "maroon", "Male" = "steelblue"))

  })
  
  output$age <- renderPlot({
    age_data_by_year <- subset(age_data, year == input$age_year_select)
    ggplot(data=age_data_by_year, aes(x=age_category, y=num_of_people)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x="age group", y = "population") +
      scale_y_continuous(labels = scales::comma)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
