library(dplyr)


shinnyapp<-
#' Title
#'
#' @field server_components list. 
#'
#' @return
#' @export
#'
#' @examples
  setRefClass(
    "shinnyapp",
    fields = list(
      server_components="list"
    ),
    methods=list(
      initialize=function(){
        countries = list(
          "Pakistan",
          "India",
          "Afghanistan",
          "Sweden"
        )
        server_components<<-list()
        apii <- MyShiny::Worldwide_Pollution$new(countries)
        server_components$ui <<- 
          shiny::navbarPage("Air Pollution",
                            shiny::tabPanel("Component 1",
                                            shiny::plotOutput("plot_1")),
                            
                            shiny::tabPanel("Component 2",
                                            shiny::fluidPage(
                                              
                                              shiny::titlePanel("Concentration of PM25"),
                                              
                                              shiny::fluidRow(
                                                shiny::column(2,
                                                              shiny::radioButtons(inputId = "radio", 
                                                                                  label = "Select the country you wish to visualize:", 
                                                                                  choices = c("Pakistan", "India", "Afghanistan", "Sweden"), 
                                                                                  inline = FALSE,
                                                                                  width = NULL)
                                                ),
                                                shiny::hr(),
                                                shiny::column(10, 
                                                              shiny::fluidRow(plotly::plotlyOutput("plot_2", height = "500px")))))))   
        
        server_components$server<<- function(input, output){
          api = apii
          print(api)
          Sys.setenv(
            'MAPBOX_TOKEN' = 
              'pk.eyJ1Ijoic3RldG84MjAiLCJhIjoiY2ptYm1hNGoxMDVzODNxcDh5YWYwdWIyeiJ9.vqmnBQELpRxT2klgrWJvuQ')
          
          output$plot_1 = shiny::renderPlot({
            facets = c(
              "country",
              "value_pm5"
            )
            plot_pm25_means(api$get_facets_all_responses(facets)) 
          })
          
          output$plot_2 = plotly::renderPlotly({
            facet_vector <-c(
              "country",
              "filename",
              "value_pm5",
              "category_pm25",
              "data_location_latitude",
              "data_location_longitude")
            print(facet_vector)
            
            df<-api$get_only_faced_data(api$responses[[input$radio]], facet_vector)
            if(input$radio=="Italy") zoom <- 4.1
            else if(input$radio=="Sweden") zoom <- 3
            else zoom <- 5
            p <- plotly::plot_mapbox(mode = "scattermapbox") %>%
              plotly::add_markers(
                data = df, y = ~df$data_location_latitude, x = ~df$data_location_latitude,
                color=~as.factor(`category_pm25`), text = ~filename, hoverinfo = "text",
                hovertext = paste('</br>Category: ', df$`category_pm25`, "</br>Region: ", df$filename,
                                  "</br>Value: ", df$value_pm5),
                marker=list(size=10), alpha = 0.5,
                colors = rev(RColorBrewer::brewer.pal(length(unique(df$`category_pm25`)),"PiYG"))) %>%
              plotly::layout(
                plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                mapbox = list(style = 'light',
                              scope = "europe",
                              zoom = zoom,
                              center = list(lat = mean(as.numeric(df$data_location_latitude)),
                                            lon = mean(as.numeric(df$data_location_longitude)))),
                legend = list(orientation = 'h',
                              font = list(size = 8)),
                margin = list(l = 0, r = 0,
                              b = 0, t = 0,
                              pad = 0)
              )
            p
            
          })
        }
      },
      run=function(){
        shiny::shinyApp(ui = server_components$ui, server = server_components$server)
      },
      plot_pm25_means=function(all_data){
        mean_table = all_data %>%
          group_by(country) %>%
          summarise(mean=mean(value_pm5))
        g = ggplot2::ggplot(mean_table, ggplot2::aes(x=country, y=mean)) +
          ggplot2::geom_bar(position="dodge", stat="identity") +
          ggplot2::labs(title = "Means of P5 in Countries", x="Countries", y="Mean") +
          ggplot2::scale_y_continuous(breaks=seq(0,70,by=5))
        return(g)
      }
      
    )
  )