df <- data.frame(sex = c("M", "F", NA , "M", "F"),
                 score = c(5,4,3,4,NA))
df

is.na(df)
table(is.na(df))

library(dplyr)
df %>% filter(is.na(score))

df %>% filter(!is.na(score) & !is.na(sex))

df_new <- na.omit(df)

mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

exam <- read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA
exam

exam %>% summarise(mean_math = mean(math))

exam %>% summarise(mean_math = mean(math, na.rm = T))

mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))

outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier
table(outlier$sex)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

boxplot(mpg$hwy)$stats
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

