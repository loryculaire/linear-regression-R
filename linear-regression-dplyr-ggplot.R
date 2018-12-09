install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)
x <- mean(gapminder$gdpPercap)

attach(gapminder)
median(pop)
hist(lifeExp)
hist(pop)
hist(log(pop))

boxplot(lifeExp ~ continent) 
plot(lifeExp ~ log(gdpPercap))


library(dplyr)

gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" | 
           country == "Ireland") %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" | 
           country == "Ireland")
t.test(data = df1, lifeExp ~ country)

library(ggplot2)

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=year, size=pop)) +
  geom_point(alpha=0.3) + 
  geom_smooth(method = lm) + 
  facet_wrap(~ continent)


summary(lm(lifeExp ~ log(gdpPercap) + log(pop)))

        