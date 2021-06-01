library(shiny)
library(tidyverse)
library(data.table)
library(lubridate)

setwd("C:/Users/mobug/Desktop/shiny")
getwd()

samsung_elect <- data.table(read.csv("data/samsung_electronics.csv")) %>% 
  filter(Close!="null")
SK_hynix <- data.table(read.csv("data/SK_hynix.csv")) %>% 
  filter(Close!="null")
naver <- data.table(read.csv("data/naver.csv")) %>% 
  filter(Close!="null")
LG_Chem <- data.table(read.csv("data/LG_Chem.csv")) %>% 
  filter(Close!="null")
samsung_electronics_preferred <- data.table(read.csv("data/samsung_electronics_preferred.csv")) %>% 
  filter(Close!="null")
samsung_biologics <- data.table(read.csv("data/samsung_biologics.csv")) %>% 
  filter(Close!="null")
kakao <- data.table(read.csv("data/kakao.csv")) %>% 
  filter(Close!="null")
hyundai_motor <- data.table(read.csv("data/hyundai_motor.csv")) %>% 
  filter(Close!="null")
samsung_sdi <- data.table(read.csv("data/samsung_sdi.csv")) %>% 
  filter(Close!="null")
celltrion <- data.table(read.csv("data/celltrion.csv")) %>% 
  filter(Close!="null")

covid_korea <- data.table(read.csv("data/owid-covid-data.csv")) %>%
  filter(location=="South Korea") %>% 
  select(4:6) %>% 
  filter(!is.na(total_cases))
covid_korea[1,"new_cases"] <- 0
covid_korea

covid_korea_year_month <- str_sub(covid_korea[,date],1,7)
covid_korea_add_ym <- covid_korea %>%
  mutate(year_month = covid_korea_year_month)
covid_korea_group <- group_by(covid_korea_add_ym,year_month)
covid_korea_bymonth <- summarise(covid_korea_group, 
                                 month_cases = sum(new_cases))


ui <- fluidPage(
  titlePanel("Stock Price"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Daily close for each stocks"),
      
      selectInput("var", 
                  label = h4("Choose Stocks"),
                  choices = c("samsung electronics", 
                              "SK hynix",
                              "naver",
                              "LG chemistry",
                              "samsung electronics pref",
                              "samsung biologics",
                              "kakao",
                              "hyundai motor",
                              "samsung SDI",
                              "celltrion"),
                  selected = "samsung electronics"),
      
      checkboxInput("checkbox", "confirmed case", value = FALSE),
      helpText("Check if you want to see confirmed"),
      
      dateRangeInput("dates", h3("Date range"),
                     start = "2017-01-02", end = "2021-04-16",
                     min = "2017-01-02", max = "2021-04-16"),
      helpText("THe blue dashed line indicates the date of the corona outbreak. (2020-01-22)"),
      
      dateInput("date",
                h3("date input"),
                value = "2017-01-02",
                min = "2017-01-02",
                max = "2021-04-16"),
      
      helpText("Set the date you want to know the price")
    
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("text")
    )
    
  )
  
  
  
  
)




# Define server logic ----
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    Data <- switch(input$var,
                   "samsung electronics" = samsung_elect, 
                   "SK hynix" = SK_hynix,
                   "naver" = naver,
                   "LG chemistry" = LG_Chem,
                   "samsung electronics pref" = samsung_electronics_preferred,
                   "samsung biologics" = samsung_biologics,
                   "kakao" =  kakao,
                   "hyundai motor" = hyundai_motor,
                   "samsung SDI" = samsung_sdi,
                   "celltrion" = celltrion
    )
    
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    if(input$checkbox){
      ggplot() +
        theme_bw()+
        geom_vline(xintercept = "2020-01-22", linetype = "dashed", color = "blue", size=1) +
        geom_line(data = covid_korea, mapping = aes(x=date, y=as.numeric(total_cases), group = 1), color = "green", size=1.3) +
        geom_line(data = Data[Date>=start_date&Date<=end_date], mapping = aes(x = Date, y = as.numeric(Close), group = 1)) +
        labs(title = "daily Close versus confirmed") +
        ylab("Close") +
        scale_x_discrete(breaks = c(as.character(as.Date(start_date)), as.character(as.Date(end_date))), expand = c(0.05, 0.05)) +
        scale_y_continuous(sec.axis = sec_axis(~., name="commulative Confirmed")) +
        theme(axis.title = element_text(margin = , size = 15, face = "bold"),
              plot.title = element_text(hjust = 0.5, color = "red", face = "bold", size = rel(1.5))) 
    } else {
      
      ggplot() +
        theme_bw()+
        geom_vline(xintercept = "2020-01-22", linetype = "dashed", color = "blue", size=1) +
        geom_line(data = Data[Date>=start_date&Date<=end_date], mapping = aes(x = Date, y = as.numeric(Close), group = 1)) +
        labs(title = "daily Close") +
        ylab("Close") +
        scale_x_discrete(breaks = c(as.character(as.Date(start_date)), as.character(as.Date(end_date))), expand = c(0.05, 0.05)) +
        theme(axis.title = element_text(margin = , size = 15, face = "bold"),
              plot.title = element_text(hjust = 0.5, color = "red", face = "bold", size = rel(1.5))) 
      
    }
  })
  
  output$text <- renderText({
    
    Data <- switch(input$var,
                   "samsung electronics" = samsung_elect, 
                   "SK hynix" = SK_hynix,
                   "naver" = naver,
                   "LG chemistry" = LG_Chem,
                   "samsung electronics pref" = samsung_electronics_preferred,
                   "samsung biologics" = samsung_biologics,
                   "kakao" =  kakao,
                   "hyundai motor" = hyundai_motor,
                   "samsung SDI" = samsung_sdi,
                   "celltrion" = celltrion
    )
    
    if(dim(Data[Date==input$date, "Close"])[1] != 0){
      paste0(input$var, "\'s Close price of ", input$date, " is ", Data[Date==input$date, "Close"])
    } else {
      print("Since the stock market is closed on holidays and so on, there is no stock price information for the date you set.")
    }
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)