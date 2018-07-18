library(data.table)
library(reshape2)
library(ggplot2)

tab <- readRDS("tab_chm_num.rds")

scale_tab <- scale(tab[,c(5:15,17,19,20,22)])
dta <- data.frame(origin = tab$origin, individual = make.names(tab$origin, unique = T), scale_tab)

tab_reshape <- data.table(melt(data = dta, id = c("origin", "individual")))
tab_reshape[, min:=min(value, na.rm = T), by = .(origin, variable)]
tab_reshape[, max:=max(value, na.rm = T), by = .(origin, variable)]

ggplot(tab_reshape[origin == "Simeulue"]) +
  geom_line(aes(x = variable, y = value, colour = origin, group = interaction(origin, individual))) +
  scale_colour_manual(values = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#000000", "#a65628", "#4daf4a")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(tab_reshape) +
  geom_ribbon(aes(ymin = min, ymax = max, x = variable, fill = origin, group = interaction(origin, individual)), alpha = 0.1) +
  scale_fill_manual(values = c("#000000", "#a65628", "#2E9FDF", "#00AFBB", "#4daf4a",  "#FC4E07", "#E7B800" )) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

stat_tab <- tab_reshape[ , {SD = sd(value, na.rm = T); VAR = var(value, na.rm = T); list(SD = SD, VAR = VAR)}, by = .(origin, variable)]
stat_tab_nan <- stat_tab[complete.cases(stat_tab), ]

ggplot(stat_tab_nan) +
  geom_bar(aes(x = variable, y = SD, group = origin, fill = origin), stat = "identity") +
  scale_fill_manual(values = c("#2E9FDF", "#E7B800", "#00AFBB", "#FC4E07", "#000000", "#a65628", "#4daf4a")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
