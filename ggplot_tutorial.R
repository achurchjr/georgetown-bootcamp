
ppg_priv$dod <- ppg_priv$value/100000000


View(ppg_priv)

ppg_priv <- ppg_priv %>%
  mutate(indicator=as.factor(indicator), country=as.factor(country),
         value=as.numeric(value), dod=as.numeric(dod))

summary(ppg_priv)

library(tidyverse)

ggplot(data = ppg_priv, mapping = aes(x=date, y=dod, color=country, group=1)) +
  geom_smooth(se=FALSE) +
  geom_point(alpha=1/2)

ghana <- subset(ppg_priv, country == "Ghana")

ggplot(data = ghana) +
  geom_line(mapping = aes(x=date, y=dod, color=country, group=1)) +
  geom_point(mapping = aes(x=date, y=dod, color=country))


ggplot(data=maturity_private)