library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

exam %>% filter(class == 1)
exam %>% filter(class != 1)

exam %>% filter(math > 40)
exam %>% filter(class == 1, math > 40)

exam %>% filter(class == 1 & math > 40)

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)

mean(class1$math)
mean(class2$math)

library(ggplot2)
mpg <- as.data.frame(mpg)
displ4 <- mpg %>% filter( displ <= 4)
displ5 <- mpg %>% filter( displ >= 5)
mean(displ4$hwy)
mean(displ5$hwy)

audi <- mpg %>% filter(manufacturer == 'audi')
toyota <- mpg %>% filter(manufacturer == 'toyota')

mean(audi$cty)
mean(toyota$cty)

three <- mpg %>% filter(manufacturer == 'chevrolet' | manufacturer =='ford' | manufacturer =='honda')
mean(three$hwy)

exam %>% select(math)
exam %>% select(english)
exam %>% select(-english)

exam %>% 
  filter(class == 1) %>% 
  select(english)

new_data <- mpg %>% select(class, cty)

compact <- new_data %>% filter(class == "compact")
mean(compact$cty) 
suv <- new_data %>% filter(class == "suv")
mean(suv$cty)
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(class, math)

audi %>% arrange(desc(hwy)) %>% head(5)

exam %>% mutate(total = math + english + science) %>% 
  head
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head

exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head
exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head
exam

mpg2 <- mpg
mpg2 <- mpg2 %>%
  mutate(toty = hwy + cty)

mpg2 <- mpg2 %>%
  mutate(avgtot = toty / 2)
mpg2 %>% arrange(desc(avgtot)) %>% head(3)

mpg %>% mutate(toty = hwy + cty,
               avgtot = (hwy + cty) / 2) %>% 
  arrange(desc(avgtot)) %>% 
  head(3)

exam %>% summarise(mean_math = mean(math))
exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math))
exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())
mpg %>%
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

mpg %>% 
  filter(class == "compact") %>% 
  group_by(manufacturer) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt))


test1 <- data.frame(id = c(1,2,3,4,5),
                    miterm = c(70, 80, 70, 90, 85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(60, 50, 70, 80, 95))
total <- left_join(test1, test2, by = "id")                   
total
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "na", "jung"))
exam_new <- left_join(exam, name, by = "class")
exam_new

group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(5,6,7,8,9),
                      test = c(50,20,80,50,95))
group_all <- bind_rows(group_a, group_b)
group_all

group_b <- rename(group_b, aid = id)
group_b <- rename(group_b, id = aid)

fuel <-data.frame(fl = c("c", "d", "e", "p", "r"),
                  price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                  stringsAsFactors = F)
fuel

mpg <- left_join(mpg, fuel, by = "fl")

mpg %>% select(model, fl, price_fl) %>% 
  head(5)

