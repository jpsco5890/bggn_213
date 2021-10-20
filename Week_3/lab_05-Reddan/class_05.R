# Lab 05: Data Visualization

# Library loading
library(ggplot2)
library(gapminder)
library(dplyr)
library(plotly)

head(cars)

# Display stopping distance (ft) as a function of speed (mph) from the 'cars' 
# dataset using ggplot2

# Option 1
# ggplot(data = cars) +
#   aes(x = speed, y = dist) +
#   geom_point() +
#   title(main = "Stopping Distance of Old Cars") +
#   xlab("Speed (MPH)") +
#   ylab("Stopping Distance (ft)")

# Option 2
ggplot(data = cars) +
  aes(x = speed, y = dist) +
  geom_point() +
  geom_smooth(formula = y ~ x, 
              method = "lm") +
  labs(title = "Stopping Distance of Old Cars",
       x = "Speed (MPH)",
       y = "Stopping Distance (ft)")

# Base graphics translation
plot(x = cars$speed, 
     y = cars$dist, 
     pch = 16,
     main = "Stopping Distance of Old Cars\n[base graphics]",
     xlab = "Speed (MPH)",
     ylab = "Stopping Distance (ft)")
abline(lm(cars$dist ~ cars$speed), 
       col = "blue",
       lwd = 2)

# Loading in the genes data
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

nrow(genes)

colnames(genes)
ncol(genes)

table(genes$State)

round(table(genes$State)["up"]/nrow(genes)*100, 2)

p <- ggplot(data = genes) +
  aes(x = Condition1, y = Condition2, 
      col = State) +
  geom_point()
p

p <- p +
  scale_color_manual(values = c("red", "gray", "blue"))
p  

p <- p +
  labs(title = "Gene Expression Changes Upon Drug Treatment",
       x = "Control (no drug)",
       y = "Drug Treatment")
p

# gapminder dataset
gapminder_2007 <- gapminder %>% filter(year==2007)

# Exploring the data
p <- ggplot(data = gapminder) +
  aes(x = year, y = lifeExp) +
  geom_violin(aes(group = year), draw_quantiles = c(0.5)) +
  geom_jitter(aes(col = continent), alpha = 0.4, width = 0.3)
p

ggplotly(p)

p <- ggplot(data = gapminder_2007) +
  aes(x = gdpPercap, y = lifeExp, col = continent, size = pop) +
  geom_point(alpha=0.4)
p

# Gross example with population as color
q <- ggplot(data = gapminder_2007) +
  aes(x = gdpPercap, y = lifeExp, col = pop) +
  geom_point()
q

# Adjusting point size
p <- ggplot(data = gapminder_2007) +
  aes(x = gdpPercap, y = lifeExp, size = pop) +
  geom_point(alpha=0.4)
p

p <- p + scale_size_area()
p

# Exploring 1957 gapfinder data
gapminder_1957 <- gapminder %>% filter(year==1957)

p <- ggplot(data = gapminder_1957) +
  aes(x = gdpPercap, y = lifeExp, col = continent, size = pop) +
  geom_point(alpha=0.7) +
  scale_size_area(max_size = 15)
p

gapminder_1957_2007 <- gapminder %>% filter(year==1957 | year == 2007)

p <- ggplot(data = gapminder_1957_2007) +
  aes(x = gdpPercap, y = lifeExp, col = continent, size = pop) +
  geom_point(alpha=0.7) +
  scale_size_area(max_size = 15) +
  facet_wrap(~year)
p

# Boxplots
gapminder_2007_top5 <- gapminder %>% 
  filter(year == 2007) %>%
  arrange(desc(pop)) %>%
  top_n(5, pop)

gapminder_2007_top5

ggplot(data = gapminder_2007_top5) +
  aes(x = country, y = pop, fill = continent) +
  geom_col()

ggplot(data = gapminder_2007_top5) +
  aes(x = country, y = pop, fill = lifeExp) +
  geom_col()

ggplot(data = gapminder_2007_top5) +
  aes(x = reorder(country, -pop), y = pop, fill = country) +
  geom_col()

USArrests

ggplot(data = USArrests) +
  aes(x = reorder(rownames(USArrests), Murder), y = Murder) +
  coord_flip() +
  geom_col()

ggplot(data = USArrests) +
  aes(x = reorder(rownames(USArrests), Murder), y = Murder) +
  geom_point() +
  geom_segment(x = rownames(USArrests),
               xend = rownames(USArrests),
               y = 0,
               yend = USArrests$Murder) +
  coord_flip()
