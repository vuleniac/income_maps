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

geocodeAdddress <- function(address) {
    url <- "http://maps.google.com/maps/api/geocode/json?address="
    url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
    x <- fromJSON(url, simplify = FALSE)
    county_found <- NULL
    if (x$status == "OK") {
        out <- c(x$results[[1]]$geometry$location$lng,
                 x$results[[1]]$geometry$location$lat)
        
        for(i in seq(from= 1, to=length(x$results))){
            for(j in seq(from= 1, to=length(x$results[[i]]$address_components))){
                
                if(x$results[[i]]$address_components[[j]]$types[[1]]=="administrative_area_level_2"){
                    county_found <- x$results[[i]]$address_components[[j]]$long_name[1]
                }
                if(x$results[[i]]$address_components[[j]]$types[[1]]=="administrative_area_level_1"){
                    state_id <- x$results[[i]]$address_components[[j]]$short_name[1]
                    state_id<- fips.state[fips.state$STUSAB==state_id,"STATE"]
                    names(state_id) <- fips.state[fips.state$STATE==state_id,"STUSAB"]
                }
                
            }
        }
        
        if(is.null(county_found)){                            
            county <- fips.county[fips.county$State.ANSI==state_id,c(3)]
            names(county) <- fips.county[fips.county$State.ANSI==state_id,c(4)]
        }else{
            county <- fips.county[fips.county$State.ANSI==state_id & fips.county$County.Name==county_found,c(3)]
            names(county) <- fips.county[fips.county$State.ANSI==state_id & fips.county$County.Name==county_found,c(4)]
        }
        
    } else {
        out <- c(-73.99329,  40.76026)
        county <- 061
        names(county) <- "New York County"
        state_id <- 36
        names(state_id) <-"NY"
    }
    Sys.sleep(0.2)  # API only allows 5 requests per second
    position <- list(out,county,state_id)
    position
}

server <- function (input, output) {

    myDataSet <- reactiveValues(data = NULL)
        
    output$map <- renderLeaflet({
        st <- input$state
        street <- input$street
        city <- input$city
        zip <- input$zip
        
        withProgress(message = 'WAIT', value = 0, {
            
        here <- isolate({paste(street, city, st, zip, "USA", sep=", ")})
        lon_lat <- geocodeAdddress(here)[[1]]
        county  <- geocodeAdddress(here)[[2]]
        st      <- geocodeAdddress(here)[[3]]
        
        incProgress(1/5, detail = 'Finding Location')
        
        ret <- if(length(county)==1 && names(st)!="DC"){names(county)}else{paste(if(names(st)!="DC"){"the State of "}else{"the "},names(states)[states==names(st)], sep="")}
        
        # grab the spatial data (tigris)
        tracts <- tracts(state = st, county = county, cb=TRUE)
        
        # create a geographic set to grab tabular data (acs)
        geo<-geo.make(state=st,
                      county=county, 
                      tract="*",
                      combine = F, 
                      combine.term = "aggregate", 
                      check = F, 
                      key = "52a4080f3d90afc1f1ea3c982ff9f47a2827e246")
        
        # !!!! important note -- the package has not been updated to 2013
        # data so I'm using the five year span that ends in 2012
        
        incProgress(3/5, detail = paste('Retreving Income Information for ',ret, sep=""))
        
        income<-acs.fetch(endyear = 2012, 
                          span = 5, 
                          geography = geo,
                          table.number = "B19001", 
                          col.names = "pretty",
                          key="52a4080f3d90afc1f1ea3c982ff9f47a2827e246")
        
        # use of col.names = "pretty" above gives the full column definitions
        # if you want Census variable IDs use col.names="auto". Here are the
        # variables we want with pretty and auto results.
        #"Household Income: Total:" ("B19001_001")
        #"Household Income: $200,000 or more" ("B19001_017")
        
        
        # the resulting "income" object is not a data.frame it's a list
        # to see what's available
        
        names(attributes(income))
        ##  [1] "endyear"        "span"           "acs.units"      "currency.year" 
        ##  [5] "modified"       "geography"      "acs.colnames"   "estimate"      
        ##  [9] "standard.error" "class"
        attr(income, "acs.colnames")
        ##  [1] "Household Income: Total:"              
        ##  [2] "Household Income: Less than $10,000"   
        ##  [3] "Household Income: $10,000 to $14,999"  
        ##  [4] "Household Income: $15,000 to $19,999"  
        ##  [5] "Household Income: $20,000 to $24,999"  
        ##  [6] "Household Income: $25,000 to $29,999"  
        ##  [7] "Household Income: $30,000 to $34,999"  
        ##  [8] "Household Income: $35,000 to $39,999"  
        ##  [9] "Household Income: $40,000 to $44,999"  
        ## [10] "Household Income: $45,000 to $49,999"  
        ## [11] "Household Income: $50,000 to $59,999"  
        ## [12] "Household Income: $60,000 to $74,999"  
        ## [13] "Household Income: $75,000 to $99,999"  
        ## [14] "Household Income: $100,000 to $124,999"
        ## [15] "Household Income: $125,000 to $149,999"
        ## [16] "Household Income: $150,000 to $199,999"
        ## [17] "Household Income: $200,000 or more"
        
        # convert to a data.frame for merging
        income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                                       str_pad(income@geography$county, 3, "left", pad="0"), 
                                       str_pad(income@geography$tract, 6, "left", pad="0")), 
                                income@estimate[,c("Household Income: Total:",
                                                   "Household Income: Less than $10,000",   
                                                   "Household Income: $10,000 to $14,999",  
                                                   "Household Income: $15,000 to $19,999",  
                                                   "Household Income: $20,000 to $24,999", 
                                                   "Household Income: $25,000 to $29,999",  
                                                   "Household Income: $30,000 to $34,999",  
                                                   "Household Income: $35,000 to $39,999",  
                                                   "Household Income: $40,000 to $44,999",  
                                                   "Household Income: $45,000 to $49,999",  
                                                   "Household Income: $50,000 to $59,999",  
                                                   "Household Income: $60,000 to $74,999",  
                                                   "Household Income: $75,000 to $99,999",  
                                                   "Household Income: $100,000 to $124,999",
                                                   "Household Income: $125,000 to $149,999",
                                                   "Household Income: $150,000 to $199,999",
                                                   "Household Income: $200,000 or more")], 
                                stringsAsFactors = FALSE)
        
        income_df <- select(income_df, 1:18)
        rownames(income_df)<-1:nrow(income_df)
        names(income_df)<-c("GEOID", "total", "less_10","b10_15","b15_20","b20_25","b25_30","b30_35","b35_40","b40_45","b45_50","b50_60","b60_75","b75_100","b100_125","b125_150","b150_200","over_200")
        
        app<-if(length(county)==1 && names(st)!="DC"){names(county)}else{paste(if(names(st)!="DC"){"the State of "}else{"the "},names(states)[states==names(st)], sep="")}
        myDataSet$data <- rbind(income_df, c(app))
        
        income_df$percent <- 100*(income_df[,input$inc]/income_df$total)
        
    income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
    # there are some tracts with no land that we should exclude
    income_merged <- income_merged[income_merged$ALAND>0,]
    label_disp <- paste("Percent of Households <br> with income <br>", names(incomes)[incomes==input$inc], sep="")
    
    popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", paste("Households with income <br>", names(incomes)[incomes==input$inc], "<br>% ", sep=""), round(income_merged$percent,2))
    pal <- colorNumeric(
        palette = "RdYlBu",
        domain = income_merged$percent
    )
    
    incProgress(1/5, detail = 'Rendering Map')
    
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = income_merged, 
                    fillColor = ~pal(percent), 
                    color = "#FFFFFF", # you need to use hex colors 
                    fillOpacity = 0.3, 
                    weight = 1, 
                    smoothFactor = 0.01,
                    popup = popup) %>%
            
        addMarkers(lng=lon_lat[1], lat=lon_lat[2]) %>%
            
        addLegend(pal = pal, 
                  values = income_merged$percent, 
                  position = "bottomright", 
                  title = label_disp,
                  labFormat = labelFormat(suffix = "%")) %>%
        
         fitBounds(lon_lat[1]-0.05, lon_lat[2]-0.05, lon_lat[1]+0.05, lon_lat[2]+0.05)
        }) 
    }) 
    
    
    output$plot <- renderPlot({
        
        income_df <- myDataSet$data
        county <- income_df[nrow(income_df),1]
        income_df <- income_df[1:(nrow(income_df)-1),]
        TT <- paste("Income Distribution in ",county,sep="")
        melted <- melt(income_df[,c(1:18)], id.vars = c("GEOID"))
        melted$value <- as.numeric(melted$value)
        income_dist <- melted[,2:3] %>% group_by(variable) %>% summarise_each(funs = "sum")
        names(income_dist)<- c("Income_Range", "Households")
        ranges <- c("Total",
                    "Less than $10,000",   
                    "$10,000 to $14,999",  
                    "$15,000 to $19,999",  
                    "$20,000 to $24,999", 
                    "$25,000 to $29,999",  
                    "$30,000 to $34,999",  
                    "$35,000 to $39,999",  
                    "$40,000 to $44,999",  
                    "$45,000 to $49,999",  
                    "$50,000 to $59,999",  
                    "$60,000 to $74,999",  
                    "$75,000 to $99,999",  
                    "$100,000 to $124,999",
                    "$125,000 to $149,999",
                    "$150,000 to $199,999",
                    "$200,000 or more")
        income_dist$Income_Range <- factor(ranges, levels=ranges, ordered=TRUE)
        
        c <- ggplot(income_dist[income_dist$Income_Range!="Total",], aes(x = factor(Income_Range), y = Households, fill= Households)) + 
            scale_fill_gradient("Households", low = "green", high = "red") + 
            geom_bar(stat = "identity") +
            labs(x="INOCME RANGES", y="NUMBER of HOUSEHOLDS", title= TT) +
            coord_flip()
            print(c)
  
    })             


         
}