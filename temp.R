library(ggplot2)

df0 <- data.frame(index=as.numeric(1:10),count = round(runif(10,1,10)))
ggplot(df0,aes(index,count)) + geom_col()

df0$index <- df0$index + 0.5
ggplot(df0,aes(x=index,y=count)) + geom_col(orientation = "x")


ggplot(df0,aes(index,count)) + geom_point()


date <- as.yearqtr(c(2016.00,2016.50, 2016.50, 2016.75, 2017.75, 2019.00, 2016.00, 2016.25,
          2016.50, 2016.75, 2017.00, 2017.25, 2017.50, 2017.75, 2018.00, 2018.25,
          2018.50, 2018.75, 2019.00, 2019.25, 2019.50, 2019.75))

count <-  c(3, 3,  4,  6,  3,  3,  3, 10,  2,  6,  4,
            7,  4,  4,  3,  5,  2,  5,  6,  4,  6,  3)


dance <- c("polka","waltz","polka","polka","waltz","samba","other","other","other",
           "other","other","other","other","other","other","other","other","other",
           "other","other","other","other")

df0 <- tibble(date,count,dance)
# dates are duplicated (but with different events) and not ordered
ggplot(df0,aes(date,count)) + 
  geom_col(orientation = "x") + 
  scale_x_yearqtr(format = "%Yq%q",guide = guide_axis(check.overlap = TRUE)) +
  NULL

df1 %>% 
  uncount(count) %>% 
  mutate(qtr = as.Date(as.yearqtr(date))) %>% 
#  complete(qtr,dance) %>% 
  ggplot(aes(qtr,fill=dance)) + geom_bar() + 
  scale_x_date()
  
  
df0 %>% pivot_wider(id_cols = "date",names_from = "dance",values_from = "count")

df1 <- df0 %>% mutate(date = as.numeric(date))

ggplot(df0,aes(date,count)) + geom_col()

ggplot(df0,aes(factor(as.numeric(date)),count,fill=dance)) + geom_col() + 
  scale_x_discrete(breaks=sort(unique(round(as.numeric(df0$date)))))

df1 <- df %>% 
#  group_by(ShowName) %>% 
  mutate(AirDate = as.numeric(AirDate)) %>% 
  complete(AirDate = seq(min(AirDate),max(AirDate),by=0.25),ShowName,fill=list(Spins=0)) %>% 
  arrange(AirDate,ShowName) %>% 
  mutate(AirDate = as.yearqtr(AirDate)) %>% 
  {.}
  

ggplot(df,aes(AirDate,Spins,fill=ShowName)) + geom_col() + 
  scale_x_yearqtr()


df1 <-  df0
# df1$date <- zoo::as.yearqtr(df1$date)
df1 <- complete(df1,date,fill,fill=list(count=0)) %>% arrange(date)
ggplot(df1,aes(date,count,fill=fill)) + geom_col()

df1 %>%  tidyr::uncount(count) %>% 
  ggplot(aes(date,fill=fill)) + geom_bar()

## scale_x_yearmon with custom discrete breaks
df <-  data.frame(dates = as.yearmon("2018-08") + 0:6/12, values = c(2:6, 0, 1))
ggdf <- ggplot(df, aes(x = dates, y = values)) +
  geom_bar(position = "dodge", stat = "identity") + theme_light() +
  xlab("Month") + ylab("Values")
ggdf ## with default scale_x_yearmon
ggdf + scale_x_yearmon(breaks = df$dates) ## with custom discrete breaks

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
