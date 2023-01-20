library(tidyverse)
options(digits=4)
options(warn=-1) #generally not recommended, i put that option after making sure everything is running ok.

df=read.csv("chocolate_bars.csv")

summary(df)

df %>% mutate_all(is.na) %>% colSums

df %>% count(company_location) %>% arrange(desc(n))  %>% head()

(df %>% count(company_location) %>% arrange(desc(n))  %>% slice(1))/2

df %>% count(bean_origin) %>% arrange(desc(n))  %>% head(10)

df %>% 
count(bean_origin) %>% arrange(desc(n)) %>% 
mutate(cumsum=cumsum(n))  %>% head(5)

(df %>% 
count(bean_origin) %>% arrange(desc(n)) %>% 
mutate(cumsum=cumsum(n))  %>% head(5)  %>% slice(5)  %>% select(3))/dim(df)[1]

df %>% count(bean_origin) %>% arrange(desc(n))  %>% tail(5)

df %>% count(bean_origin) %>% filter(n>1) %>% arrange(desc(n))  %>% tail(5)

df %>% group_by(bean_origin) %>% 
summarize(average_rating=mean(rating),n=n()) %>% arrange(desc(average_rating)) %>% head()


df %>% group_by(bean_origin) %>% 
summarize(average_rating=mean(rating),n=n()) %>% arrange(desc(average_rating)) %>% tail()

df %>% group_by(bean_origin) %>% 
  summarize(average_rating=mean(rating),n=n()) %>% 
arrange(desc(n),desc(average_rating))  %>% head()

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

c(mean(df$rating),sd(df$rating),Modes(df$rating))

ggplot(data=df,aes(x=rating,y=cocoa_percent))+
           geom_boxplot(aes(group=rating))+
    stat_summary(fun.y="mean",color="red", shape=20,fill="red")+ylab("Percent of Cocoa")+xlab("Rating")

ggplot(data=df,aes(x=cut_interval(rating,5),y=cocoa_percent))+
    geom_boxplot(aes(group=cut_interval(rating,5)))+
    stat_summary(fun.y="mean",color="red", shape=20,fill="red")+ylab("Percent of Cocoa")+xlab("Rating")

dfbins=df %>% mutate(bin=cut_interval(rating,5))

dfbins %>%na.omit %>%  group_by(bin) %>% 
select(bin,cocoa_percent,num_ingredients,rating) %>% 
summarize_all(mean)  

dfbins$toprated=ifelse(df$rating>2.8,1,0)

frequent=df %>% count(bean_origin) %>% arrange(desc(n))  %>% 
head(5) %>% select(bean_origin) %>% 
t %>% as.vector

dfbins$freq=ifelse(dfbins$bean_origin %in% frequent,1,0)

dfbins$sweetener=ifelse(stringr::str_detect(dfbins$ingredients,fixed("S*")),1,0)



model=glm(toprated~cocoa_percent+as.factor(freq)+as.factor(sweetener),
          data=dfbins,family="binomial")

summary(model)

#top 20 ratings
df %>% arrange(desc(rating)) %>% select(ingredients,cocoa_percent) %>% head(20)



#bottom 20 values
df %>%  arrange(rating) %>% select(ingredients,cocoa_percent) %>% head(20)

#bottom 20 ratings , with ommited values
df %>%na.omit %>%  arrange(rating) %>% select(ingredients,cocoa_percent) %>% head(20)



