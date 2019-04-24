library(ggplot2)
library(dplyr)
library(readxl)

ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)
mpg<-as.data.frame(ggplot2::mpg)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

df<- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(data = df, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + geom_col()

mf <- mpg %>% 
  select(class)
ggplot(data = mf, aes(x = class)) + geom_bar()


ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

