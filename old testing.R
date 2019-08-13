if(input$second_variable=="specialize") {
  p <- p + facet_wrap(~specialize,ncol=1)  
} else if( input$second_variable=="learn") {
  p <- p + facet_wrap(~learn,ncol=1)  
} else if( input$second_variable=="work") {
  p <- p + facet_wrap(~work,ncol=1)  
} else if( input$second_variable=="role") {
  p <- p + facet_wrap(~role,ncol=1)  
} else if(input$second_variable=="containers") {
  p <- p + facet_wrap(~containers,ncol=1)  
} else{}
p






df_long_spread <- data

if(input$individual_color=="none") {
  df_long_spread %>% 
    #filter(input$individual.color %in% paste0()) %>%
    group_by(learn,specialize) %>% 
    count() %>% na.omit() %>%
    ggplot(aes(x=learn,y=n)) +
    geom_bar(stat="identity") +
    facet_wrap(~specialize,ncol=1) +
    scale_y_continuous(breaks=c(1:10)) +
    coord_flip() +
    ggthemes::theme_tufte(base_family="sans") +
    labs(title="\"I want to learn\" by Specialization Groups",
         subtitle = "Bar clusters not mutually exclusive in data",
         y="Count")
} else if(input$individual_color=="role"){
  
  
  
  df_long_spread %>% 
    filter(role %in% input$input_choice_role) %>%
    group_by(learn,specialize,role) %>% 
    count() %>% na.omit() %>%
    ggplot(aes(x=learn,fill=role,y=n)) +
    geom_bar(stat="identity") +
    facet_wrap(~specialize,ncol=1) +
    scale_y_continuous(breaks=c(1:10)) +
    coord_flip() +
    ggthemes::theme_tufte(base_family="sans") +
    labs(title="\"I want to learn\" by Specialization Groups",
         subtitle = "Bar clusters not mutually exclusive in data",
         y="Count")
  
} else {
  
  df_long_spread %>% 
    filter(containers %in% input$input_choice_container) %>%
    group_by(learn,specialize,containers) %>% 
    count() %>% na.omit() %>%
    ggplot(aes(x=learn,fill=containers,y=n)) +
    geom_bar(stat="identity") +
    facet_wrap(~specialize,ncol=1) +
    scale_y_continuous(breaks=c(1:10)) +
    coord_flip() +
    ggthemes::theme_tufte(base_family="sans") +
    labs(title="\"I want to learn\" by Specialization Groups",
         subtitle = "Bar clusters not mutually exclusive in data",
         y="Count")
  
}