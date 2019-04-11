library(ggplot2)
library(dplyr)
midwest <- as.data.frame(ggplot2::midwest)

str(midwest)

?midwest

summary(midwest)

midwest_new <- midwest
midwest_new

midwest_new <- rename(midwest_new, total = poptotal, asian = popasian)

str(midwest_new)

midwest_new$asiatot <- (midwest_new$asian / midwest_new$total * 100)
str(midwest_new)
hist(midwest_new$asiatot)

mean(midwest_new$asiatot)
midwest_new$asiatotls <- ifelse(midwest_new$asiatot > mean(midwest_new$asiatot), "large", "small")

qplot(midwest_new$asiatotls)
