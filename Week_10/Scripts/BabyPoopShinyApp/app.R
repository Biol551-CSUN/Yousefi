# Make a Shiny App using data about baby poop, weight, and feeding

library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)
library(lubridate)
library(hms)
library(lemon)

# Loading Data
BabyData <- read_csv(here("Week_10", "Data", "HatchBabyExport.csv"))

# Cleaning Data
BabyData <- BabyData %>%
    rename("Start_Time" = "Start Time", 
           "End_Time" = "End Time", 
           "Baby" = "Baby Name") %>% 
    separate("Start_Time", 
             into = c("Date_1", "Time_1", "AM_PM_1"), 
             sep = " ") %>%
    separate("End_Time", 
             into = c("Date_2", "Time_2", "AM_PM_2"), 
             sep = " ") %>%
    mutate(Date_1 = mdy(Date_1), 
           Date_2 = mdy(Date_2), 
           name = as.factor(Baby))

# Assigning colors to each baby
Colors <- c("darkorange", "springgreen3")
names(Colors) = c("Blakely", "Micah") 

# Gathering data for each plot
Diaper_Data <- BabyData %>% filter(Activity == "Diaper") %>% 
    rename(Poop = Amount) %>%
    select(Baby, name, Date_1, Time_1, Poop)

Weight_Data <- BabyData %>% filter(Activity == "Weight") %>%
    mutate(Amount = as.numeric(Amount)) %>% 
    rename(Weight = Amount) %>%
    select(Baby, name, Date_1, Weight) %>%
    drop_na()

Feeding_Data <- BabyData %>% filter(Activity == "Feeding", Info == "Bottle") %>%
    mutate(Amount = as.numeric(Amount)) %>% 
    rename(Feeding = Amount) %>%
    select(Baby, name, Date_1, Time_1, Duration, Feeding)

Data <- full_join(Weight_Data, Feeding_Data)

Data <- full_join(Data, Diaper_Data)

Data <- Data %>% select(Baby, name, Date_1, Time_1, Duration, Poop, Weight, Feeding)

# Defining UI for application that allows us to visualize plots
ui <- dashboardPage(
    dashboardHeader(title = "Baby Tracker"),
    dashboardSidebar(
        menuItem("Diapers", tabName = "Poop", icon = icon("poo-storm")),
        menuItem("Weight", tabName = "Weight", icon = icon("weight")),
        menuItem("Feeding", tabName = "Feeding", icon = icon("utensils"))
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML(".main-sidebar { font-size: 20px; }")) # change the font size to 16
        ),
        fluidRow(
            box(title = HTML("<b>Blakely</b>"), background = "orange", HTML(
                "Current Weight:", round(tail(Weight_Data[which(Weight_Data$Baby=="Blakely"), 3:4]$Weight, 1), digits = 2),
                "<br/>Total Poops:", length(Diaper_Data[Diaper_Data$Baby=="Blakely", 3:5]$Poop),
                "<br/>Bottle High Score:", max(Feeding_Data[which(Feeding_Data$Baby=="Blakely"), 3:6]$Feeding)
            )
            
            ),
            box(title =HTML("<b>Micah</b>"), background = "green", HTML(
                "Current Weight:", round(tail(Weight_Data[which(Weight_Data$Baby=="Micah"), 3:4]$Weight, 1), digits = 2),
                "<br/>Total Poops:", length(Diaper_Data[Diaper_Data$Baby=="Micah", 3:5]$Poop),
                "<br/>Bottle High Score:", max(Feeding_Data[which(Feeding_Data$Baby=="Micah"), 3:6]$Feeding)
            )
            
            ) # /infoBox 
        ), # /fluidRow 1
        fixedRow(
            box(checkboxGroupInput("Baby", "Baby", 
                                   choices = c("Blakely", "Micah"),
                                   selected = c("Blakely")))
        ),# /fluidRow 2
        tabItems(
            tabItem(
                "Poop",
                fluidPage(
                    h1("Baby Poops"),
                    box(plotOutput("poop_plot"), width=20)
                )
            ),
            tabItem(
                "Weight",
                fluidPage(
                    h1("Weight (lbs)"),
                    box(plotOutput("weight_plot"), width=10)
                ) 
            ),
            tabItem(
                "Feeding",
                fluidPage(
                    h1("Bottle"),
                    box(plotOutput("bottle_plot"), width=20)
                )  # /fluidPage
            ) # /tabItem
        ) # /tabItems
    )
)

# Defining server logic required to parse and plot data for plot visualization
server <- function(input, output) {
    D <- reactive(Data %>% filter(Baby %in% input$Baby))
    
    output$poop_plot <- renderPlot({
        D() %>% drop_na(Poop) %>%
            ggplot(aes(x=Date_1, group=Poop, fill=Poop)) +
            geom_density(alpha=0.5) +
            scale_x_date(date_labels = "%b %d",
                         date_breaks = "1 week") +
            facet_rep_wrap(.~Baby, nrow = 2, ncol=1, 
                           repeat.tick.labels = TRUE,
                           strip.position="top") +
            theme_classic() +
            theme(strip.background = element_rect(color = "white"), 
                  strip.text = element_text(size=12, face="bold"),
                  legend.position = "right", legend.justification="top",
                  legend.key.size = unit(12, "pt"),
                  legend.text = element_text(size=8)) +
            labs(x="Date", y = "Density", fill="Poop Type") 
    })
    
    output$weight_plot <- renderPlot({
        D() %>% drop_na(Weight) %>%
            ggplot(aes(x=Date_1, y=Weight, color=name)) +
            geom_point(size = 4) +
            scale_x_date(date_labels = "%b %d",
                         date_breaks = "1 week") + 
            scale_color_manual(values = Colors) +
            theme_classic() +
            theme(legend.position = "none") +
            xlab("Date") +
            ylab("Weight")
    })
    
    output$bottle_plot <- renderPlot({
        D() %>% drop_na(Feeding) %>%
            ggplot(aes(x=Date_1, y=Feeding, color=name)) +
            geom_point(shape=20, size = 4) + 
            scale_x_date(date_labels = "%b %d",
                         date_breaks = "1 week") +
            scale_color_manual(values = Colors) +
            facet_rep_wrap(.~Baby, nrow = 2, ncol=1, 
                           repeat.tick.labels = TRUE,
                           strip.position="top") +
            theme_classic() +
            theme(legend.position = "none",
                  strip.background = element_rect(color = "white"),
                  strip.text = element_text(size=12, face="bold")) +
            xlab("Date") +
            ylab("Volume")
    })
}

# Run the application
shinyApp(ui = ui, server = server)


