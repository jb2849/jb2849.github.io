#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

########################################
# setwd("C:/Users/bjcfa/Desktop/Shiny/Practice")
library(shinydashboard)
library(raster)
library(htmltools)
library(leaflet)
library(shiny)
library(ggplot2)
library(ggmap)
library(png)
library(imager)
library(dtplyr)
library(gridExtra)
library(car)
library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)
library(DT)
library(maps)
########################################
#FUNCTION
########################################
progress_bar <- function(){
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Loading', value = 0, {
                # Number of times we'll go through the loop
                n <- 10
                
                for (i in 1:n) {
                        # Each time through the loop, add another row of data. This is
                        # a stand-in for a long-running computation.
                        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                        
                        # Increment the progress bar, and update the detail text.
                        incProgress(1/n, detail = paste("Doing part", i))
                        
                        # Pause for 0.1 seconds to simulate a long computation.
                        Sys.sleep(0.1)
                }
        })
}
model <- function(detroit){
        #Data Partition
        groundtruth <- detroit$price
        
        partition <- createDataPartition(y=groundtruth, p=.8 ,list=F)
        train <- detroit[partition,]
        test <- detroit[-partition,]
        
        
        
        
        
        #Model 1 All sigle-family homes
        
        model1 <- lm(price~total_size+total+zip+longitude+latitude, data = train)
        prediction <- predict(model1, test)
        model_output <- cbind(test, prediction)
        
        #RMSE and Error
        model_output$log_prediction <- log(model_output$prediction)
        model_output$log_response <- log(model_output$price)
        error <- abs((model_output$price-model_output$prediction)/(model_output$price))
        RMSE_log <- sqrt(mean((model_output$log_response-model_output$log_prediction)^2, na.rm = TRUE))
        RMSE <- sqrt(mean((model_output$price-model_output$prediction)^2, na.rm = TRUE))
        
        #Evaluation
        quan <- quantile(error,c(0.1,0.25,0.5,0.6,0.7,0.75,0.8,0.9),na.rm = TRUE)
        detroit.sfh.sum <- summary(model1)
        residual.sd <- sd(detroit.sfh.sum$residuals)
        r.squared <- detroit.sfh.sum$adj.r.squared
        df <- detroit.sfh.sum$df[2]
        com <- cbind(residual.sd,r.squared,df,RMSE,RMSE_log,t(quan))
        return(com)
}


plot_Missing <- function(data_in, title = NULL){
        temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
        temp_df <- temp_df[,order(colSums(temp_df))]
        data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
        data_temp$m <- as.vector(as.matrix(temp_df))
        data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
        ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

quant <- function(x,quan){
        if(x < quan[2]){y <- paste(0, "-", quan[2])}
        if(x >= quan[2] & x < quan[3]){y <- paste(quan[2], "-", quan[3])}
        if(x >= quan[3] & x < quan[4]){y <- paste(quan[3], "-", quan[4])}
        if(x >= quan[4] ){y <- paste( ">=", quan[4])}
        return(y)
}

price_group <- function(data){
        data.quan <- quantile(data$price)
        
        data$price_group <- sapply(data$price, quant, quan = data.quan)
        
        data$price_group <- as.factor(data$price_group)
        
        return(data)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
        pp <- list()
        for (i in ii) {
                p <- fun(data_in=data_in, i=i)
                pp <- c(pp, list(p))
        }
        do.call("grid.arrange", c(pp, ncol=ncol))
}

plotCorr <- function(data_in, i){
        data <- data.frame(x = data_in[[i]], SalePrice = data_in$price)
        p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
        return(suppressWarnings(p))
}

feature_select <- function(sample.df){
        
        
        
        ID.VAR <- "X"
        PROPERTY_ID.VAR <- "property_id"
        TARGET.VAR <- "price"
        
        #extract only candidate feature names
        candidate.features <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR,PROPERTY_ID.VAR)) #extract desired feature
        
        data.type <- sapply(candidate.features,function(x){class(sample.df[[x]])}) #categorical summary of character and integer
        
        
        #determine data types
        explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,PROPERTY_ID.VAR,TARGET.VAR))
        data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[[x]])})
        
        #categorize data types in the data set
        unique.classes <- unique(data.classes)
        
        attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])}) #Creating a list to categorize colnames depends on type of data
        names(attr.data.types) <- unique.classes
        
        ####Preparing for Boruta analysis, the missin value will be set to -1 and missing character is set to MISSING
        
        #pull out the response variable
        response <- sample.df$price
        
        #remove identifier and response variables
        sample.df <- sample.df[candidate.features]
        
        #Inserting missing value to -1 for purpose of random forest
        for(x in attr.data.types$integer){
                sample.df[[x]][is.na(sample.df[[x]])] <- -1 
        }
        
        for(x in attr.data.types$numeric){
                sample.df[[x]][is.na(sample.df[[x]])] <- -1
        }
        
        for (x in attr.data.types$character){
                sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"
        }
        
        for(x in attr.data.types$logical){
                sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"
        }
        
        ###Run Borta Analysis###
        ###Boruta function is to select feature with output of variable inmportance measure (VIM)###
        set.seed(13)
        bor.results <- Boruta(sample.df, response, maxRuns =  50,doTrace = 0)
        
        #plot
        # p <- plot(bor.results, xlab = "", xaxt = "n", main = "Feature Importance Graph")
        # lz <- lapply(1:ncol(bor.results$ImpHistory),function(i){bor.results$ImpHistory[is.finite(bor.results$ImpHistory[,i]),i]})
        # names(lz) <- colnames(bor.results$ImpHistory)
        # labels <- sort(sapply(lz,median))
        # axis(side = 1, las=2, labels = names(labels), at = 1:ncol(bor.results$ImpHistory), cex.axis = 0.7)
        # 
        decision <- getSelectedAttributes(bor.results)
        sample.fin <- cbind(sample.df[decision], response)
        colnames(sample.fin)[ncol(sample.fin)] <- "price"
        return(sample.fin)
}

content <- function(dataset){
        paste(seq = "<br/>",
              paste("Address:",dataset$address,",",dataset$city,"," , dataset$state, ",", dataset$zip),
              paste("Year Built:",dataset$year_built),
              paste("House Size",dataset$total_size, "Squared Feet"),
              paste("Price:", dataset$price))
}

########################################
setwd("C:/Users/bjcfa/Desktop/Shiny/Practice")
austin <- read.csv("austin_filtered.csv")[,-c(1:3)]
bakersfield <- read.csv("bakersfield_filter.csv")[,-c(1:4)]
detroit <- read.csv("detroit_filtered.csv")[,-c(1:4)]
houston <- read.csv("houston_filter.csv")[,-c(1:4)]
indianapolis <-read.csv("indianapolis_filter.csv")[,-c(1:4)]

# austin <- read.csv("austin_filtered.csv")[,-c(1:3)]
# bakersfield <- read.csv("bakersfield_filter.csv")[,-c(1:4)]
# detroit <- read.csv("detroit_filtered.csv")[,-c(1:4)]
# houston <- read.csv("houston_filter.csv")[,-c(1:4)]
# indianapolis <-read.csv("indianapolis_filter.csv")[,-c(1:4)]



# Define UI for application that draws a histogram
ui <- shinyUI(dashboardPage(
        dashboardHeader(title = "Data Nerds"),
        dashboardSidebar(
                sidebarMenu(
                        selectInput("location_info", "Choose a city for visualization",
                                    choices = c("Austin", "Bakersfield", "Detroit","Houston","Indianapolis")),
                        # actionButton("go", "submit"),
                        div(style="display:inline-block;width:100%;text-align: center;",actionButton("go", label = "submit")),
                        menuItem("GIS Information", tabName = "GIS"),
                        menuItem("Brief Data Visualization", tabName = "BDI"),
                        menuItem("Model Information", tabName = "MI")
                        
                        
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "GIS",
                                
                                
                                box(title = "Price Distribution On Map", solidHeader = TRUE,status = "primary","The house distrbution based on GIS is shown below:",plotOutput("map")),
                                HTML('<br/>'),
                                box(title = "Interactive Map",solidHeader = TRUE,status = "primary", "You can click the marker to see the address and price of the house:",leafletOutput("i_map"))
                        ),
                        
                        tabItem(tabName = "BDI",
                                
                                box(title = "Price Density Distribution", solidHeader = TRUE,status = "primary",plotOutput(outputId = "price_summary", height = "300px")),
                                
                                box(title = "Visualization For The Missing Data",solidHeader = TRUE,status = "primary",plotOutput(outputId = "missing_visual")),
                                
                                box(title = "Scatter Plot For Analysis",solidHeader = TRUE,status = "primary",plotOutput(outputId = "Scatterplot_analysis"))
                        ),
                        
                        tabItem(tabName = "MI",
                                
                                
                                box(title = "Feature Selection","Feature selection process using Boruta package, the selection result is shown belown",solidHeader = TRUE,status = "primary",DT::dataTableOutput('feature_selection')),
                                
                                
                                box(title = "linear Regression Model", "Applied simple linear regression model, the result of the model is shown below",solidHeader = TRUE,status = "primary",DT::dataTableOutput("lm"))
                                
                        )
                )
                
        )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input,output){
        
        location <- eventReactive(input$go, {
                location.temp()
        })
        
        
        location.temp <- reactive(
                {
                        progress_bar()
                        switch(input$location_info,
                               "Austin" = austin,
                               "Bakersfield" = bakersfield,
                               "Detroit" = detroit,
                               "Houston" = houston,
                               "Indianapolis" = indianapolis)
                        
                }
        )
        
        
        
        #Titile of the Map
        output$map_name <- renderPrint({
                dataset <- location()[1,"city"]
                paste(dataset,"Houses Distribution")
        })
        
        #House Price Mapping
        output$map <- reactivePlot(function(){
                
                # location <- switch(input$location_info,
                #        "Austin" = austin,
                #        "Bakersfield" = bakersfield,
                #        "Detroit" = detroit,
                #        "Houston" = houston,
                #        "Indianapolis" = indianapolis)
                dataset <- location()
                
                
                progress_bar()
                location.map <- get_map(as.character( dataset[1,"city"]), zoom = 11)
                location.map <- ggmap(location.map, extent = "device", legend = "right")
                location.map <- location.map + geom_jitter(aes(x = longitude, y = latitude, color = price),
                                                           size = 2, position = position_jitter(w = 0.0005, h = 0.0005),
                                                           data = dataset) + scale_colour_gradient( low="blue", high="red", space="Lab")
                print(location.map)
                
        })
        
        output$price_summary_name <- renderPrint({
                dataset <- location()[1,"city"]
                paste(dataset,"Price Density Distribution")
        })
        
        output$price_summary <- renderPlot({
                progress_bar()
                dataset <- location()
                hist(as.numeric(dataset$price),
                     probability = TRUE,
                     xlab = "Price",
                     main = "Price Density Distribution")
        })
        
        output$Scatterplot_analysis_name <- renderPrint({
                "Scatter plot for analysis"
        })
        
        output$missing_visual_name <- renderPrint({
                "Visualization for the missing data"
        })
        
        output$missing_visual <- renderPlot({
                progress_bar()
                dataset <- location()
                plot_Missing(dataset[,colSums(is.na(dataset)) > 0])
        })
        
        output$Scatterplot_analysis <- renderPlot({
                progress_bar()
                dataset <- location()[,c("year_built","baths_count","total_size", "zip", "size", "total","longitude","latitude","price")]
                colnames(dataset)[c(3,5,6)] <- c("Building_size","Site_size","Tax_total")
                doPlots(dataset, fun = plotCorr,ii = 1:8)
        })
        
        output$feature_selection_name <- renderPrint({
                "Feature selection process using Boruta package, the selection result is shown belown"
        })
        
        # output$feature_selection <- renderTable({
        #        dataset <- location()
        #        head(feature_select(dataset))
        # })
        
        output$feature_selection <- DT::renderDataTable({
                progress_bar()
                dataset <- location()
                test.table <- head(feature_select(dataset))
                DT::datatable(test.table, 
                              options = list(scrollX = TRUE))
        })
        
        
        output$lm_name <- renderPrint({
                "Applied simple linear regression model, the result of the model is shown below"
        })
        
        output$lm <- DT::renderDataTable({
                progress_bar()
                dataset <- location()
                test.table <- model(dataset)
                DT::datatable(test.table, 
                              options = list(scrollX = TRUE))
        })
        
        output$i_map_name <- renderPrint({
                "Interactive map. You can click the marker to see the address and price of the house"
        })
        
        output$i_map <- renderLeaflet({
                progress_bar()
                
                temp <- location()
                temp <- price_group(temp)
                temp$full_address <- content(temp)
                colorsmap = colors()[1:length(unique(temp$price_group))]
                groupColors = colorFactor(palette = "RdYlBu", domain = temp$price_group)
                leaflet(data = temp) %>% addTiles() %>%
                        addMarkers(~longitude, ~latitude, popup = ~htmlEscape(full_address) 
                                   ,clusterOptions = markerClusterOptions())
        })
        
        
        
})

# Run the application 
shinyApp(ui = ui, server = server)

