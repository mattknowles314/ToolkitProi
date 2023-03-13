#################################################################################################################################################################################
#   Assignment:   EDA Assessed Project
#     Due Date:   March 21st, 2023
#      Members:   Allan Cousins, Matthew Knowles, Oliver Hewitt
#
#                 Inputs:
#                       - Accident data from the UK Department for Transport
#
#                 Outputs:
#                       - Shiny App to aid visual understanding of risk profile
#################################################################################################################################################################################

# This code was written by Allan Cousins, and ported to shinyapps.io by Matthew Knowles

library(shiny)
library(leaflet)
library(tidyverse)
library(stringr)

model1 <- readRDS(file = "app_models_and_data.RDS")[[1]]
model2 <- readRDS(file = "app_models_and_data.RDS")[[2]]
popup_df <- readRDS(file = "app_models_and_data.RDS")[[3]]
region_codes <- readRDS(file = "app_models_and_data.RDS")[[4]]

# custom styles for popup configuration
style <- tags$head(
    tags$style(HTML("
    .popup > .leaflet-popup-content-wrapper {
      /*opacity: 0.75 !important;*/
      background-color: rgba(255, 255, 255, 0.9);
    }
  "))
)

# set holders for data to be transported between functions
input_df_lm <- c()
national_df_lm <- c()

# Define UI
ui <- fluidPage(

    style,

    # TITLE
    titlePanel("UK Vehicular Accident Risk Assessment Tool (2020 Data)"),
    # INPUTS
    sidebarLayout(
        sidebarPanel(


            selectInput("light",
                        "Choose which light condition you would like to investigate",
                        choices = c("Daylight" = 1,
                                    "Darkness - lights lit" = 4,
                                    "Darkness - lights out" = 5,
                                    "Darkness - no lighting" = 6,
                                    "Darkness - lighting unknown" = 7),
                        selected = 1),

            selectInput("surface",
                        "Choose which road surface condition you would like to investigate",
                        choices = c("Dry" = 1,
                                    "Wet or Damp" = 2,
                                    "Snow" = 3,
                                    "Frost or ice" = 4,
                                    "Flood over 3cm deep" = 5,
#                                    "Oil or diesel" = 6,
#                                    "Mud" = 7,
                                    "Other or Unknown" = 9),
                        selected = 1),

            selectInput("weather",
                        "Choose which weather condition you would like to investigate",
                        choices = c("Fine (no high winds)" = 1,
                                    "Raining (no high winds)" = 2,
                                    "Snowing (no high winds)" = 3,
                                    "Fine with high winds" = 4,
                                    "Raining with high winds" = 5,
                                    "Snowing with high winds" = 6,
                                    "Fog or Mist" = 7,
                                    "Other" = 8,
                                    "Unknown" = 9),
                        selected = 1),
            selectInput("day",
                        "Choose which day of the week would you like to investigate",
                        choices = c("Sunday" = 1,"Monday" = 2,"Tuesday" = 3,"Wednesday" = 4,"Thursday" = 5,"Friday" = 6, "Saturday" = 7),
                        selected = 1),

            checkboxInput(inputId = "info", label = HTML("Click me for information about the line plot and to show the trend line! <br> (click again to hide)"), value = FALSE),

            HTML("<br><u>Risk Assessment Tool Navigation</u> <br>On the top left of the page there are several drop down menus that offer a selection of values for explanatory variables included in the statistical model. As you select different values you can see the change in probability manifest in the line graph. Some selections will bring about a slight change whereas others will bring about a rather stark change.
                 <br><br>The line graph displays probabilities for both a specific region in England (red) as well as the national average (blue). You may change which region displays on the line graph by clicking the associated marker on the map (a selected marker will be displayed as red). By selecting the box immediately under the drop down menus (about information) the plot will display trend lines with their associated 95% confidence intervals along with a brief conclusion about what is being shown."),

            HTML("<br><br><u>Risk Assessment Tool Explanation</u> <br> The risk tool displays probabilities of accidents being classified as serious / fatal given the selected inputs. To find the probability of an accident being classified as slight all one needs to do is subtract the displayed probability from 1 (e.g. if the probability of a serious/fatal classification is 0.40 then the probability of a slight classification is 0.60).
                 <br><br>Probabilities were assessed using a logistic regression routine implemented via the <a href='https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm'>'GLM'</a> function from the 'stats' package in the statistical programming language 'R'. It is important to note that the displayed probabilities are both contingent on an accident already occuring, and that the model is appropriately specified."),

            HTML("<br><br><u>Data Source</u> <br> The analysis presented in this application is based on 2020 roadside safety data (RSD) from the UK Department for Transport. The original data are hosted online with the main portal being accessible <a href='https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data'>here.</a> Direct download links for the data can be found <a href='https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-accident-2020.csv'>here</a> (for RSD - Accidents), <a href='https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-vehicle-2020.csv'>here</a> (for RSD - Vehicles), and <a href='https://data.dft.gov.uk/road-accidents-safety-data/dft-road-casualty-statistics-casualty-2020.csv'>here</a> (for RSD - Casualties). Population level data comes from <a href='https://en.wikipedia.org/wiki/Regions_of_England'>Wikipedia.</a>")),



        # outputs
        mainPanel(
            plotOutput("line", height = "60vh"),
            br(),
            textOutput("desc"),
            br(),
            br(),
            br(),
            leafletOutput("map", height = "60vh"))
    ))

##SERVER-----
server <- function(input, output,session) {

    output$line <- renderPlot({
        # create datapoints for selected location
        nvars <- 6
        npoints <- 11
        input_data <- as.data.frame(matrix(c(rep(0, nvars*npoints)), nrow = npoints))
        colnames(input_data) <- c('local_authority_ons_district','day_of_week','light_conditions','weather_conditions','road_surface_conditions','speed_limit')

        if (is.null(input$map_marker_click) == FALSE)
        {
            input_data$local_authority_ons_district <- popup_df$region_names[which(popup_df$region_lat == input$map_marker_click$lat)]
        } else{
            # need base data for london
            input_data$local_authority_ons_district <- 'London'
        }

        input_data$day_of_week <- input$day
        input_data$light_conditions <- input$light
        input_data$weather_conditions <- input$weather
        input_data$road_surface_conditions <- input$surface
        input_data$speed_limit <- seq(from = 20, to = 70, by = 5)

        input_model <- as.data.frame(cbind(predict(model1,type = 'response', newdata = input_data), seq(from = 20, to = 70,by = 5)))
        input_model$Region <- input_data$local_authority_ons_district
        colnames(input_model) <- c('prob','speed','Region')

        # notice the <<- assignment. This allows for this variable to be treated globally.
        input_df_lm <<- input_model

        national_data <- input_data
        national_data$local_authority_ons_district <- 'National'

        national_model <- as.data.frame(cbind(predict(model2,type='response',newdata=national_data),seq(from=20,to=70,by=5)))
        national_model$Region <- national_data$local_authority_ons_district
        colnames(national_model) <- c('prob','speed','Region')

        # notice the <<- assignment. This allows for this variable to be treated globally.
        national_df_lm <<- national_model

        combined_df <- as.data.frame(rbind(national_model, input_model),stringsAsFactors = FALSE)
        colnames(combined_df) <- c('prob','speed','Region')

        # get colours correct way around
        if (input_model$Region == 'East Midlands' || input_model$Region == 'East of England' || input_model$Region == 'London')
        {
            region_cols <- c('#FF0000','#6495ED')
            region_shape <- c(17,16)
        } else
        {
            region_cols <- c('#6495ED','#FF0000')
            region_shape <- c(16,17)
        }

        # get title for graph based on current data
        current_title <- paste("Risk of an Accident Being Serious / Fatal Given the Road Speed Limit \n and Other Factors for Accidents in the",input_data$local_authority_ons_district[1],"Region",sep = " ")

        # set y limits based on data but try to keep consistent as to make it easier to evaluate trends
        if (max(combined_df$prob) > 0.50)
        {
            if (max(combined_df$prob) > 0.70)
            {
                current_ylim <- c(0.05, max(combined_df$prob) + 0.05)
            }else{
                current_ylim <- c(0.05,0.70)
            }
        }else{
            current_ylim <- c(0.05,0.50)
        }
        plot <- ggplot(combined_df, aes(x=speed, y=prob, col=Region, shape = Region)) +
                    scale_colour_continuous(guide = "none")+
                    geom_point(size = 4) +
                    scale_shape_manual(values = region_shape) +
                    scale_color_manual(values = region_cols) +
                    labs(title = current_title,
                        x = "Road Speed Limit (mph)",
                        y = "Probability of a Serious / Fatal Classification")+
                    ylim(current_ylim)+
                    theme(axis.title.x = element_text(size=18,hjust=0.5),
                    axis.title.y = element_text(size = 18,hjust=0.5),
                    plot.title = element_text(size = 22,hjust = 0.5),
                    axis.text = element_text(colour = "black", size = (15)))
        plot2 <- plot + geom_smooth(method=lm)

        if(input$info==TRUE)
        {
            print(plot2)
        }else{
            print(plot)
        }
    })

output$map <- renderLeaflet({
    marker_colors <- rep("blue",nrow(popup_df))
    if(is.null(input$map_marker_click)==FALSE)
    {
        marker_colors[which(popup_df$region_lat==input$map_marker_click$lat)] <- "red"
        popup_location <-  popup_df$region_names[which(popup_df$region_lat==input$map_marker_click$lat)]
    }else{
        marker_colors[which(popup_df$region_names=='London')] <- "red"
        popup_location <- 'London'
    }

    popup_lat <- popup_df$region_lat[which(popup_df$region_names==popup_location)]
    popup_lon <- popup_df$region_lon[which(popup_df$region_names==popup_location)]
    popup_population <- popup_df$region_popDis[which(popup_df$region_names==popup_location)]
    popup_accidents <- popup_df$num_accidents[which(popup_df$region_names==popup_location)]
    popup_accident_rate <- popup_df$accident_rate[which(popup_df$region_names==popup_location)]
    popup_serious <- popup_df$num_fatal[which(popup_df$region_names==popup_location)]
    popup_mild <- popup_df$num_mild[which(popup_df$region_names==popup_location)]
    popup_mild_rate <- popup_df$mild_rate[which(popup_df$region_names==popup_location)]
    popup_serious_rate <- popup_df$serious_rate[which(popup_df$region_names==popup_location)]
    popup_mild_speed <- popup_df$mild_speed[which(popup_df$region_names==popup_location)]
    popup_serious_speed <- popup_df$serious_speed[which(popup_df$region_names==popup_location)]

    popup_df <- cbind(popup_df,marker_colors)
    colnames(popup_df)[ncol(popup_df)] <- "m_col"

    leaflet(popup_df,height=2000) %>%
        setView(lng = -2.3, lat = 53.75, zoom = 6) %>%
        addTiles() %>%
        addPopups(lng = popup_lon,lat = popup_lat,
                  popup = paste("<b><center><font size='+1'>",popup_location," (Region) Accident Statistics</b></center></font>",
                                "<br><u>General</u>",
                                "<br><b>Population:</b> ",popup_population,
                                "<br><b>Number of Accidents:</b> ",popup_accidents,
                                "<br><b>Accident Rate per 100,000:</b> ",popup_accident_rate,
                                "<br><br><u>Fatal/Serious (Severe) Accidents</u>",
                                "<br><b>Number of Severe Accidents:</b> ",popup_serious,
                                "<br><b>Severe Accident Rate (%):</b> ",popup_serious_rate,
                                "<br><b>Average Road Speed Limit (km/h):</b> ",popup_serious_speed,
                                "<br><br><u>Slight Accidents</u>",
                                "<br><b>Number of Slight Accidents:</b> ",popup_mild,
                                "<br><b>Slight Accident Rate (%):</b> ",popup_mild_rate,
                                "<br><b>Average Road Speed Limit (km/h):</b> ",popup_mild_speed),
                  options = popupOptions(closeButton = TRUE, autoClose = TRUE,closeOnClick = FALSE,className = "popup")) %>%
        addCircleMarkers(~region_lon,
                         ~region_lat,
                         label = ~paste("Region:",str_to_title(region_names),sep=" "),
                         labelOptions = labelOptions(noHide = F, textsize = "15px",
                                                     style = list(
                                                         "color" = "black",
                                                         "font-family" = "serif",
                                                         "font-style" = "bold",
                                                         "font-size" = "15px",
                                                         "border-color" = "rgba(0,0,0,0.5)"
                                                     )),
                         color = ~m_col)

})

output$desc <- renderText({
    input_test <- c(input$weather,input$surface,input$light,input$day,input$map_marker_click$lat)

    # perform lm to get coefficients
    fit.input <- lm(prob ~ speed, data=input_df_lm)
    fit.national <- lm(prob ~ speed, data=national_df_lm)

    # find the trend based on coefficients
    if(summary(fit.input)$coefficients[2,1]>0)
    {
        trend.input <- "tends to increase with speed."}
    else if(summary(fit.input)$coefficients[2,1]==0){

        trend.input <- "has little relationship with speed."}

    else{
        trend.input <- "tends to decrease with speed."}

    if(summary(fit.national)$coefficients[2,1]>0)
    {
        trend.national <- "tends to increase with speed."}
    else if(summary(fit.national)$coefficients[2,1]==0){

        trend.national <- "has little relationship with speed."}

    else{
        trend.national <- "tends to decrease with speed."}

    # find the average difference between national and region
    mean.diff <- signif(100*mean(input_df_lm$prob - national_df_lm$prob),digits=3)
    sd.diff <- signif(sd(100*(input_df_lm$prob - national_df_lm$prob)),digits=3)
    if(mean.diff>0)
    {
        mean.desc <- paste("there is a greater probability of a collision being classified as serious or fatal in the ",input_df_lm$Region[1]," region compared to the National average (mean difference in probability (%) = ",mean.diff," and standard deviation = ",sd.diff,").",sep="")
    }else{
        mean.desc <- paste("there is a lesser probability of a collision being classified as serious or fatal in the ",input_df_lm$Region[1]," region compared to the National average (mean difference in probability (%) = ",mean.diff," and standard deviation = ",sd.diff,").",sep="")
    }

    # see if the slopes cross at any point
    if(all(input_df_lm$prob > national_df_lm$prob)){
        slope.desc <- paste(" It's also worth remarking that for the entire range of speed limits the ",input_df_lm$Region[1]," region has a greater chance of having an accident being classified as fatal or serious compared to the National average.",sep="")
    }else{
        if(all(input_df_lm$prob < national_df_lm$prob))
        {
            slope.desc <- paste(" It's also worth remarking that for the entire range of speed limits the ",input_df_lm$Region[1]," region has a lesser chance of having an accident being classified as fatal or serious compared to the National average.",sep="")
        }else{
            cm <- rbind(coef(fit.input),coef(fit.national)) # Coefficient matrix
            cross.values <- signif(c(-solve(cbind(cm[,2],-1)) %*% cm[,1]),digits=3)
            slope.desc <- paste(" It's also worth remarking that the ",input_df_lm$Region[1]," region has a greater or lesser chance of having an accident classified as either serious or fatal compared to the National average depending on the speed limit. This change occurs at a speed limit of ", cross.values[1], " mph.",sep="")
        }
    }

    # description
    if(input$info==TRUE){
        # Duplicated code since without the check below the description doesnt update in real time.
        # This is a hacky solution but a solution it is!
        if(c(input$weather,input$surface,input$light,input$day,input$map_marker_click$lat)==input_test)
        {

            paste("It appears that for a given roadway speed limit ", mean.desc,
                  " The trend line for National accidents indicates that the probability of a collision being classified as serious or fatal ",
                  trend.national,
                  " The trend line for accidents in the ",input_df_lm$Region[1]," region indicates that the probability of a collision being classified as serious or fatal ",
                  trend.input,
                  slope.desc,
                  sep="")
        }else{

            paste("It appears that for a given roadway speed limit ", mean.desc,
                  " The trend line for National accidents indicates that the probability of a collision being classified as serious or fatal ",
                  trend.national,
                  " The trend line for accidents in the ",input_df_lm$Region[1]," (region) indicates that the probability of a collision being classified as serious or fatal ",
                  trend.input,
                  sep="")
        }
    }

})

}

# Run the application
shinyApp(ui = ui, server = server)
