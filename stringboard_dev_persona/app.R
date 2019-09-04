#working app

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
  
    
    # Application title
    titlePanel("String Board Data Explorer"),
    
    # Sidebar with inputs for variables
    sidebarLayout(
        sidebarPanel(
           
            h3("Build your graph:"),
            
            #select variable that builds main plot bars
            selectInput(
                inputId = "target_variable",
                label= "Main variable to graph in bars",
                choices = c(
                    "My code editor is" = "code.editor",
                    "I want to learn" = "learn" ,
                    "I specialize in" = "specialize",
                    "I work in" = "work",
                    "My role is" = "role",
                    "I use containers ___ % of the time" = "containers"
                ),
                selected = "code.editor"
            ),
           
            #select how to color in bars
            selectInput(
                inputId = "individual_color",
                label= "Bar fill variable (sub-bars)",
                choices = c(
                    "None" = "none",
                    "My code editor is" = "code.editor",
                    "I want to learn" = "learn" ,
                    "I specialize in" = "specialize",
                    "I work in" = "work",
                    "My role is" = "role",
                    "I use containers ___ % of the time" = "containers",
                    "Location where data was gathered" = "place"
                ),
                selected = "none"
            ),
    
           
            # #filter by location
            # selectInput(
            #     inputId = "place_in",
            #     label= "Data sources",
            #     choices = c(
            #         "Combined" = "all",
            #         "Separated" = "split"
            #     ),
            #     selected = "All"
            # ),
            #optional sizing 
            hr(),
            h4("About these data:"),
            p("These data were collected at Red Hat Summit 2019 and DevConf.US 2019. Developers filled out information about their work and tools on a string board."
            
              
            ),
            hr(),
            h6("Found a bug? @carljpearson on twitter or github")
        ),
        
        # Show the plot
        mainPanel(
           
            plotlyOutput("Plot")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #read data
    data <- isolate({
        
        data <- read_csv("string_data2.csv",col_names = T)
        
        
    })
    
    #visual options show switch
    output$vizout <- reactive({
        input$viz == TRUE
    })
    outputOptions(output, "vizout", suspendWhenHidden = FALSE)
    
    #main plot
    observe({
        
        
        
        output$Plot <- renderPlotly({
            
           if(input$individual_color == input$target_variable){
               
               t <- list(
                   family = "sans serif",
                   size = 12,
                   color = 'red')
               
               
               plotly_empty() %>%
                   layout(title="Please do not choose the same variable twice when building your graph",font=t)
           } else
           {
            
            df_long_spread <- data 
            
            
            if(input$individual_color=="none"){
                p_data <- df_long_spread %>% 
                    group_by(!!rlang::sym(input$target_variable)) %>% 
                    count() %>%
                    na.omit()%>%
                    rename(
                        target=!!rlang::sym(input$target_variable))
            } else if(input$individual_color!="none"){
             p_data <- df_long_spread %>% 
                 group_by(!!rlang::sym(input$target_variable),
                          !!rlang::sym(input$individual_color)) %>% 
                 count() %>%
                na.omit()%>%
                rename(
                        target=!!rlang::sym(input$target_variable),
                       second=!!rlang::sym(input$individual_color))
            }
            
            
            
            #axis names made prettier
            var_name = case_when(
                input$target_variable == "code.editor" ~ "My code editor is" ,
                input$target_variable == "learn"     ~ "I want to learn",
                input$target_variable == "specialize" ~"I specialize in",
                input$target_variable == "work" ~ "I work in",
                input$target_variable == "role" ~ "My role is",
                input$target_variable == "containers" ~ "I use containers ___ % of the time",
                input$target_variable == "place" ~ "Location where data was gathered from"
                
            )
            
                
                
            if(input$individual_color=="none"){
                p <- p_data %>% 
                    plot_ly(
                        x = ~target,
                        y = ~n,
                        color = ~target,
                        type = "bar"
                    ) %>%
                    layout(
                        xaxis = list(title=var_name),
                        yaxis = list(title="Count"),
                        showlegend = FALSE)
            } else if(input$individual_color=="place"){
            p <- p_data %>% 
                plot_ly(
                x = ~target,
                y = ~n,
                color = ~second,
                type = "bar"
            ) %>%
            layout(
                xaxis = list(title=var_name),
                yaxis = list(title="Count"),
                barmode='group')
            } else {
                p <- p_data %>% 
                    plot_ly(
                        x = ~target,
                        y = ~n,
                        color = ~second,
                        type = "bar"
                    ) %>%
                    layout(
                        xaxis = list(title=var_name),
                        yaxis = list(title="Count"),
                        barmode='stack')
            }
            
        
           }
        })
        
        
    }) #end
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)


