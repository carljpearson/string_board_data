#load packages

library(tidyverse) #all data wrangling/plotting
library(googlesheets) #you will need to allow tidyverse access to your gsuite permissions intially
library(ggthemes)



#get data

gs_ls() #list google sheets
sheets <- gs_title("stringboard_Summit") #raw data from google sheet title
gs_ws_ls(sheets) #list worksheets
data <- gs_read(ss=sheets, #google sheet
                ws="stringboard_Summit") #specific worksheet
df <- data.frame(data) #convert to dataframe
#df <- df[1:100,]

#clearn var names and convert multi select into new vars
dfc <- df %>%
  rename(place=Place,
         day=Day,
         containers=I.deploy.code.to.containers.for,
         code.editor=My.code.editor.is,
         learn=I.want.to.learn,
         work=I.work.in,
         specialize=I.specialize.in,
         role=I.am.a
         ) %>%
  mutate(code.editor=tolower(code.editor)) %>%
  separate(code.editor,
           into=paste0("code.editor",1:4),
           sep=","
           ) %>%
  #mutate_if(contains("code.editor"),str_trim)
  mutate(learn=tolower(learn)) %>%
  separate(learn,
           into=paste0("learn",1:5),
           sep=","
  ) %>%
  mutate(work=tolower(work)) %>%
  separate(work,
           into=paste0("work",1:5),
           sep=","
  ) %>%
  separate(specialize,
            into=paste0("specialize",1:5),
            sep=","
  ) %>%
  separate(role,
           into=paste0("role",1:5),
           sep=","
  ) %>%
  rownames_to_column(var="id")


#view uniques to find what duplicates to consolidate
dfc %>% select(contains("code.editor")) %>% gather(col,name) %>% select(-col) %>% unique()
dfc %>% select(contains("learn")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("work")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("specialize")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("role")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("containers")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()

#elongate data to clean duplicates
dfc2 <- dfc %>% 
  select(-place,-day,-containers) %>%
  gather(col,name,-id) %>%
  mutate(name=str_trim(name),
         name=tolower(name)) %>% 
  mutate(
    name=str_replace(name,"^che","eclipse che"),
    name=str_replace(name,"interlj","intellij"),
    name=str_replace(name,"micro services","microservices"),
    name=str_replace(name,"container orch$","container orchestration"),
    name=str_replace(name,"server less (all)","serverless"),
    name=str_replace(name,"services mesh","service mesh"),
    name=str_replace(name,"devons","devops"),
    name=str_replace(name,"and","&"),
    name=str_replace(name,"retail$","retail & services"),
    name=str_replace(name,"backend","back end"),
    name=str_replace(name,"sr mgr","manager"),
    name=str_replace(name,"development manager","manager"),
    name=str_replace(name,"mid-level$","mid-level developer"),
    name=str_replace(name,"senior$","senior developer"),
    name=str_replace(name,"manager (senior)","manager"),
    name=str_replace(name,"None","none"),
    name=str_replace(name,"51- 75","51-75"),
    name=str_replace(name,"51-76","51-75"),
    
  ) %>%
  filter(name!="") %>%
  spread(col,name)

#verify dups are consolidated
dfc2 %>% select(contains("code.editor")) %>% gather(col,name) %>% select(-col) %>% unique()
dfc2 %>% select(contains("learn")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc2 %>% select(contains("work")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc2 %>% select(contains("specialize")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc2 %>% select(contains("role")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()


#elongate

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df_long <- dfc2 %>%
  gather(category,value,-id) %>%
  na.omit() %>%
  mutate(response_number = substrRight(category,1),
         category = gsub("[[:digit:]]+", "", category)
  )

#make data semi long with level 1 variables repeated
df_long_spread <- dfc2 %>%
  gather(category,value,-id) %>%
  na.omit() %>%
  mutate(response_number = substrRight(category,1),
         category = gsub("[[:digit:]]+", "", category)
         ) %>%
  spread(category,value) 



#add in just one role as role1
df_role1 <- dfc2 %>% select(id,role1)
df_cont <- dfc %>% select(id,containers)
df_place <- dfc %>% select(id,place)
df_long_spread <- df_long_spread %>% 
  select(-role) %>% 
  inner_join(df_role1) %>% 
  rename(role=role1) %>%
  inner_join(df_cont) %>%
  inner_join(df_place)

#write data for app                             
write_csv(df_long_spread,"/Users/carlpearson/Documents/r_github/string_board_data/stringboard_dev_persona/string_data2.csv")

#df_long %>% select
  
#dataviz testing

#i want to learn based on inudstry or specialization
df_long_spread %>% group_by(learn,specialize) %>% count() %>% na.omit() %>%
  ggplot(aes(x=learn,fill=learn,y=n)) +
  geom_bar(stat="identity") +
  facet_wrap(~specialize,ncol=1) +
  scale_y_continuous(breaks=c(1:10)) +
  coord_flip() +
  ggthemes::theme_tufte(base_family="sans") +
  labs(title="\"I want to learn\" by Specialization Groups",subtitle = "Bar clusters not mutually exclusive in data",y="Count")

df_long_spread %>% group_by(learn,specialize,role) %>% count() %>% na.omit() %>%
  ggplot(aes(x=learn,fill=role,y=n)) +
  geom_bar(stat="identity") +
  facet_wrap(~specialize,ncol=1) +
  scale_y_continuous(breaks=c(1:10)) +
  coord_flip() +
  ggthemes::theme_tufte(base_family="sans") +
  labs(title="\"I want to learn\" by Specialization Groups",subtitle = "Bar clusters not mutually exclusive in data",y="Count")

df_long_spread %>% group_by(learn,specialize,work) %>% count() %>% na.omit() %>%
  ggplot(aes(x=learn,fill=work,y=n)) +
  geom_bar(stat="identity") +
  facet_wrap(~specialize,ncol=1) +
  scale_y_continuous(breaks=c(1:10)) +
  coord_flip() +
  ggthemes::theme_tufte(base_family="sans") +
  labs(title="\"I want to learn\" by Specialization Groups",subtitle = "Bar clusters not mutually exclusive in data",y="Count")













#old-----
dfc2 <- dfc %>%
  mutate_all(str_trim) %>%
  mutate_at(vars(contains("code.editor")),
            ~str_replace(.,"^che","eclipse che"),
            ~str_replace(.,"interlj","intellij"),
  ) %>%
  mutate_at(vars(contains("learn")),
            ~str_replace(.,"microservices","microservices"),
            ~str_replace(.,"container orch","container orchestration"),
            ~str_replace(.,"server less (all)","serverless"),
            ~str_replace(.,"devons","devops"),
  ) %>%
  mutate_at(vars(contains("work")),
            ~str_replace(.,"and","&"),
            ~str_replace(.,"retail","retail & services"),
  ) %>%
  mutate_at(vars(contains("specialization")),
            ~str_replace(.,"Backend","Back end")
  ) %>%
  mutate_at(vars(contains("role")),
            ~str_replace(.,"Sr mgr","senior manager"),
            ~str_replace(.,"Mid-level$","Mid-level developer")
            ~str_replace(.,"Senior$","Senior developer")
  ) %>%
  mutate_all(tolower) 


#plotly
library(plotly)
data <- read_csv("/Users/carlpearson/Documents/r_github/string_board_data/stringboard_dev_persona/string_data2.csv",col_names = T)



p <- plot_ly(data, x = ~, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')



data %>%
  group_by(role,containers,code.editor) %>%
  count() %>%
  na.omit() %>%
  ggplot(aes(x=role,y=n)) +
  geom_bar(aes(fill=code.editor),stat="identity",position="dodge") +
  geom_bar(aes(fill=containers),stat="identity",position="stack") +
  #facet_wrap(~get(input$second_variable),ncol=1)  +
  # scale_y_continuous(breaks=c(1:10)) +
  coord_flip() +
  ggthemes::theme_tufte(base_family="sans") 
