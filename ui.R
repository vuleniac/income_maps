library(shiny)
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)
library(lattice)
library(RColorBrewer)
library(dplyr)    # for working with data frames
library(gdata)
library(RJSONIO)
library(ggplot2)
library(reshape2)

states <- c(fips.state[1:51,2]," ")
names(states) <- c(fips.state[1:51,3], "I don't know the State")

incomes <-c("Less than $10,000" = "less_10",   
            "$10,000 to $14,999" = "b10_15",
            "$15,000 to $19,999" = "b15_20", 
            "$20,000 to $24,999" = "b20_25",
            "$25,000 to $29,999" = "b25_30",  
            "$30,000 to $34,999" = "b30_35",  
            "$35,000 to $39,999" = "b35_40",  
            "$40,000 to $44,999" = "b40_45",  
            "$45,000 to $49,999" = "b45_50",  
            "$50,000 to $59,999" = "b50_60",  
            "$60,000 to $74,999" = "b60_75",  
            "$75,000 to $99,999" = "b75_100", 
            "$100,000 to $124,999" = "b100_125",
            "$125,000 to $149,999" = "b125_150",
            "$150,000 to $199,999" = "b150_200",
            "$200,000 or more" = "over_200")


ui <- fluidPage(
    titlePanel("Income Distribution - 2012 US Census"),
    sidebarLayout(
        sidebarPanel(
            helpText("Creates the county's income distribution of the address inputed based on information from the 2012 US Census"),
            textInput(inputId="street", label= "Street Address", value="Empire State Building"),
            textInput(inputId="city", label= "City", value= "New York"),
            selectInput(inputId="state", label="Select a State", choices= states, multiple = FALSE,
                        selected="NY", selectize = TRUE, width = NULL, size = NULL),
            textInput(inputId="zip", label= "Zip", width=validateCssUnit(80), value=""),
            submitButton(text ="UPDATE"),
            br(),
            radioButtons(inputId="inc", "Income Range", incomes, selected= "over_200")
            ),
    mainPanel(leafletOutput("map"),
              plotOutput('plot'))
               
    ))

