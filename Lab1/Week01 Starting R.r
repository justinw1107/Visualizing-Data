R.Version()

Sys.which("make")

Sys.getenv("PATH")

library("tidyverse")

install.packages("tidyverse")

library(tidyverse)

data(package = "datasets")

data(package = "ggplot2")

?mtcars

data("mtcars")

head(mtcars)

nrow(mtcars)
ncol(mtcars)

rownames(mtcars)

colnames(mtcars)

summary(mtcars)

str(mtcars)

class(mtcars$hp)

mtcars$hp

class(mtcars["hp"])

mtcars["hp"]

mtcars

mtcars$cyl

unique(mtcars$cyl)

class(mtcars$cyl)

df <- mtcars

df

class(mtcars$gear)
df$gear <- as.factor(df$gear)
class(df$gear)

ggplot(mtcars)



mtcars$cyl <- as.factor(mtcars$cyl)

ggplot(mtcars, aes(cyl)) + geom_bar()


p <- ggplot(mtcars, aes(cyl))
p <- p + geom_bar()
p

p <- ggplot(mtcars, aes(mpg))
p + geom_bar()

df <- mtcars
df$mpg <- as.factor(df$mpg)
p <- ggplot(df, aes(mpg))
p + geom_bar()

df <- mtcars
df$mpg <- as.integer(df$mpg)
p <- ggplot(df, aes(mpg))
p + geom_bar()
ggplot(data = mtcars, aes(x = cyl)) +
  geom_bar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
  geom_bar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
geom_bar() +
  coord_polar(theta = "y")

pie <- ggplot(mtcars, aes(x = "", fill = factor(cyl))) +
 geom_bar() +
coord_polar()
pie

pie <- ggplot(mtcars, aes(x = "", fill = factor(cyl))) +
 geom_bar(width = 1) +
coord_polar()
pie

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = cyl, fill = cyl)) +
coord_polar()

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = cyl, fill = cyl), width = 1) +
coord_polar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
geom_bar() +
  coord_polar(theta = "y")

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = "", fill = cyl), width = 1) +

theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()

) +
coord_polar(theta = "y")

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = "", fill = cyl), width = 1) +

theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),

) +
coord_polar(theta = "y")

ggplot(mtcars, aes(x = "", fill = cyl, )) +

geom_bar(position = "fill") +
  geom_text(
    stat =  "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
theme(
   axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
    )


ggplot(data = mtcars, aes(x = "", fill = cyl)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void()

ggplot(mtcars, aes(x = "", fill = cyl, )) +

geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
theme(
   axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    )


ggplot(mtcars, aes(x = as.integer(cyl))) + geom_histogram(bins = 3)
