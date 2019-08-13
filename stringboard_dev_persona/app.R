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
                label= "Choose variable to graph in bars",
                choices = c(
                    "My code editor is" = "code.editor",
                    "I want to learn" = "learn" ,
                    "I specialize in" = "specialize",
                    "I work in" = "work",
                    "My role is" = "role",
                    "I use containers ___ % of the time" = "containers"
                ),
                selected = "learn"
            ),
            #select variable that builds facets in ggplot
            selectInput(
                inputId = "second_variable",
                label= "Chose faceting variable (sub-graphs)",
                choices = c(
                    "None"="none",
                    "My code editor is" = "code.editor",
                    "I want to learn" = "learn" ,
                    "I specialize in" = "specialize",
                    "I work in" = "work",
                    "My role is" = "role",
                    "I use containers ___ % of the time" = "containers"
                ),
                selected = "none"
            ),
            #select how to color in bars
            selectInput(
                inputId = "individual_color",
                label= "Choose bar fill variable (sub-bars)",
                choices = c(
                     "None" = "none",
                     "My code editor is" = "code.editor",
                     "I want to learn" = "learn" ,
                     "I specialize in" = "specialize",
                     "I work in" = "work",
                     "My role is" = "role",
                     "I use containers ___ % of the time" = "containers"
                ),
                selected = "none"
            ),
            #conditional panels below for selecting each factor in a variable to hide or show
            conditionalPanel(
                condition = "input.individual_color == 'role' ",
                            checkboxGroupInput(inputId = "input_choice_role",
                                        label= "Choose the roles",
                                        choices = c(
                                            "architect",
                                            "manager",
                                            "lead developer",
                                            "senior developer",
                                            "mid-level developer",
                                            "junior developer"
                                        ),
                                        selected =  c(
                                            "architect",
                                            "manager",
                                            "lead developer",
                                            "senior developer",
                                            "mid-level developer",
                                            "junior developer"
                                        )
                            )
                     ),
            conditionalPanel(
                condition = "input.individual_color == 'code.editor' ",
                checkboxGroupInput(inputId = "input_choice_code.editor",
                                   label= "Choose the code editors",
                                   choices = c(
                                       "vc code",
                                       "eclipse",
                                       "eclipse che",
                                       "intellij",
                                       "atom",
                                       "emacs",
                                       "other"
                                   ),
                                   selected =  c(
                                       "vc code",
                                       "eclipse",
                                       "eclipse che",
                                       "intellij",
                                       "atom",
                                       "emacs",
                                       "other"
                                   )
                )
            ),
            conditionalPanel(
                condition = "input.individual_color == 'learn' ",
                checkboxGroupInput(inputId = "input_choice_learn",
                                   label= "Choose the learning area",
                                   choices = c(
                                       "service mesh"   ,
                                       "serverless" ,
                                       "containerization",
                                       "devops",
                                       "container orchestration",
                                       "microservices"
                                   ),
                                   selected =  c(
                                       "service mesh"   ,
                                       "serverless" ,
                                       "containerization",
                                       "devops",
                                       "container orchestration",
                                       "microservices"
                                   )
                )
            ),
            conditionalPanel(
                condition = "input.individual_color == 'specialize' ",
                checkboxGroupInput(inputId = "input_choice_specialize",
                                   label= "Choose the specialization",
                                   choices = c(
                                       "devops" ,
                                       "full stack",
                                       "back end" ,
                                       "front end",
                                       "mobile",
                                       "security" 
                                   ),
                                   selected =  c(
                                       "devops" ,
                                       "full stack",
                                       "back end" ,
                                       "front end",
                                       "mobile",
                                       "security" 
                                   )
                )
            ),
            conditionalPanel(
                condition = "input.individual_color == 'work' ",
                checkboxGroupInput(inputId = "input_choice_work",
                                   label= "Choose the work industry",
                                   choices = c(
                                       "science & tech" ,
                                       "manufacturing",
                                       "government",
                                       "retail & services",
                                       "finance" ,
                                       "security"  ,
                                       "other"
                                   ),
                                   selected =  c(
                                       "science & tech" ,
                                       "manufacturing",
                                       "government",
                                       "retail & services",
                                       "finance" ,
                                       "security"  ,
                                       "other"
                                   )
                )
            ),
            conditionalPanel(
                condition = "input.individual_color=='containers'",
                                    checkboxGroupInput(inputId = "input_choice_conatiners",
                                                       label= "Choose how much they work with containers (%)",
                                                       choices = c(
                                                           "None"="none",
                                                           "1-25",
                                                           "26-50",
                                                           "51-75",
                                                           "76-100"
                                                           
                                                       ),
                                                       selected =  c(
                                                           "None"=="none",
                                                           "1-25",
                                                           "26-50",
                                                           "51-75",
                                                           "76-100"
                                                          
                                                       )
                )
            ),
            #optional sizing 
            hr(),
            checkboxInput(
                inputId = "viz",
                label= "More visual options",
                value=F
            ),
            #change text size
            conditionalPanel(
                condition = "output.vizout",
                numericInput(
                    inputId = "textsize",
                    label = "Choose text size",
                    value=16,
                    min=8,
                    max=24
                    
                    
                ),
                #scale plot
                numericInput(
                    inputId = "plotwidth",
                    label = "Choose plot width scaling",
                    value=800,
                    min=400,
                    max=2000,
                    step=10
                    
                    
                )
                    
                                 
            )
            ),

        # Show the plot
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #read data
    data <- isolate({
        
        data <- read_csv("/Users/carlpearson/Documents/r_github/string_board_data/stringboard_dev_persona/string_data.csv",col_names = T)

        
    })
    
    #visual options show switch
    output$vizout <- reactive({
        input$viz == TRUE
    })
    outputOptions(output, "vizout", suspendWhenHidden = FALSE)
 
    #main plot
    observe({
    
    output$Plot <- renderPlot({
        df_long_spread <- data
        
        
        
        if(input$individual_color=="none" & input$second_variable=="none") { #show with only main variable picked
         p <- df_long_spread %>% 
             group_by(!!rlang::sym(input$target_variable)) %>% 
                count() %>%
                na.omit() %>%
                ggplot(aes(x=!!rlang::sym(input$target_variable),y=n,fill=!!rlang::sym(input$target_variable))) +
                geom_bar(stat="identity") +
                coord_flip() +
                ggthemes::theme_tufte(base_family="sans") + 
                guides(fill=FALSE)
        } else if(input$individual_color=="none" & input$second_variable!="none") { #show with only main variable and faceting variable picked
            p <- df_long_spread %>% 
                group_by(!!rlang::sym(input$target_variable),!!rlang::sym(input$second_variable)) %>% 
                count() %>%
                na.omit() %>%
                ggplot(aes(x=!!rlang::sym(input$target_variable),y=n,fill=!!rlang::sym(input$target_variable))) +
                geom_bar(stat="identity") +
                facet_wrap(~get(input$second_variable),ncol=1)  +
                # scale_y_continuous(breaks=c(1:10)) +
                coord_flip() +
                ggthemes::theme_tufte(base_family="sans") + 
                guides(fill=FALSE)
            
        } else if(input$individual_color!="none" & input$second_variable=="none") { #show with only main variable and bar color variable picked
            p <- df_long_spread %>% 
                filter( if( input$individual_color == "role"){!!rlang::sym(input$individual_color) %in% input$input_choice_role
                } else if( input$individual_color == "work"){!!rlang::sym(input$individual_color) %in% input$input_choice_work
                } else if( input$individual_color == "learn"){!!rlang::sym(input$individual_color) %in% input$input_choice_learn
                } else if( input$individual_color == "code.editor"){!!rlang::sym(input$individual_color) %in% input$input_choice_code.editor
                } else if( input$individual_color == "specialize"){!!rlang::sym(input$individual_color) %in% input$input_choice_specialize
                } else if( input$individual_color == "work"){!!rlang::sym(input$individual_color) %in% input$input_choice_work
                } 
                ) %>%
                group_by(!!rlang::sym(input$target_variable),!!rlang::sym(input$individual_color)) %>% 
                count() %>%
                na.omit() %>%
                ggplot(aes(x=!!rlang::sym(input$target_variable),y=n,fill=!!rlang::sym(input$individual_color))) +
                geom_bar(stat="identity") +
                #facet_wrap(~get(input$second_variable),ncol=1)  +
                # scale_y_continuous(breaks=c(1:10)) +
                coord_flip() +
                ggthemes::theme_tufte(base_family="sans") 
        } else { #show with all variables selected
         p <- df_long_spread %>% 
                filter( if( input$individual_color == "role"){!!rlang::sym(input$individual_color) %in% input$input_choice_role
                    } else if( input$individual_color == "work"){!!rlang::sym(input$individual_color) %in% input$input_choice_work
                    } else if( input$individual_color == "learn"){!!rlang::sym(input$individual_color) %in% input$input_choice_learn
                    } else if( input$individual_color == "code.editor"){!!rlang::sym(input$individual_color) %in% input$input_choice_code.editor
                    } else if( input$individual_color == "specialize"){!!rlang::sym(input$individual_color) %in% input$input_choice_specialize
                    } else if( input$individual_color == "work"){!!rlang::sym(input$individual_color) %in% input$input_choice_work
                    } 
                    ) %>%
               group_by(!!rlang::sym(input$target_variable),!!rlang::sym(input$second_variable),!!rlang::sym(input$individual_color)) %>% 
               count() %>% 
               na.omit() %>%
               ggplot(aes(x=!!rlang::sym(input$target_variable),y=n, fill = !!rlang::sym(input$individual_color))) +
               geom_bar(stat="identity") +
               facet_wrap(~get(input$second_variable),ncol=1)  +
               coord_flip() +
               ggthemes::theme_tufte(base_family="sans") 
             }
        
                #output plot
             p +   theme(text = element_text(size=input$textsize)) + #scale text
                 labs( #plot labels
                     y="Count",
                     caption="Data in bars are not mutually exclusive")
            
            
            #change plot width scaling
         },height = input$plotwidth)
    
    
    }) #end
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
