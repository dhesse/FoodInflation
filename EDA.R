library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
price.data <- read_csv("ObservationData_vhltxod.csv.gz")
price.data$Date <- mdy_hms(price.data$Date)
summary(price.data)
local.currency.prices <- price.data %>%
  filter(Unit == 'Standard Local Currency/tonne')
in.std.price <- local.currency.prices %>%
  group_by(country, item) %>%
  mutate(stdPrice = Value/sum(Value[Date == ymd("2000-01-01")])) %>%
  select(country, item, Date, stdPrice) %>%
  na.omit() %>%
  ungroup()
summary.prices <- in.std.price %>%
  group_by(item, Date) %>%
  summarise(`Normalized Price` = mean(stdPrice))
clean.prices <- summary.prices %>%
  group_by(item) %>%
  filter(max(`Normalized Price`) < 50) %>%
  ungroup()
low.change <- clean.prices %>%
  group_by(item) %>%
  filter(max(`Normalized Price`) < 3.5) %>%
  ungroup()
high.change <- clean.prices %>%
  group_by(item) %>%
  filter(max(`Normalized Price`) >= 3.5) %>%
  ungroup()
ggplot(high.change, aes(x=Date, y=`Normalized Price`, colour=item, group=item)) +
  geom_line()
ggsave("high_price_change.png")
ggplot(low.change, aes(x=Date, y=`Normalized Price`, colour=item, group=item)) +
  geom_line()
ggsave("low_price_change.png")
model <- lm(`Normalized Price`~Date, low.change)
print(summary(model))
intercept <- summary(model)$coefficients[1]
slope <- summary(model)$coefficients[2]
ggplot(low.change, aes(x=Date, y=`Normalized Price`, colour=item, group=item)) +
  geom_line() +
  geom_abline(slope=slope, intercept=intercept, aes(colour='best fit'))
ggsave("low_price_change_with_fit.png")
print(slope*60*60*365*24)
