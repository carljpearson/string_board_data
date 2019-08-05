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
           into=paste0("code.editor_",1:4),
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
  ) 

dfc %>% select(contains("code.editor")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("learn")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("work")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()
dfc %>% select(contains("specialize")) %>% gather(col,name) %>% select(-col) %>% mutate(name=str_trim(name)) %>% unique()

dfc %>%
  mutate_all(str_trim) %>%
  mutate_at(vars(contains("code.editor")),
            paste(.,"1")
            )
  
  
