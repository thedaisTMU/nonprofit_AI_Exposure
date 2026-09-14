library(tidyverse)
library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(DaisTheme)
library(plyr)
library(readr)
library(ggrepel)

graph_fields <- fread("graph_fields_nonprofit.csv")
graph_attributes <- fread("graph_attributes_nonprofit.csv")

figure_1_data <- fread("figure_1_nonprofit.csv")

figure_1_nonprofit<-
  ggplot(figure_1_data, aes(x = aioe, y = groupmean, size = TOTAL_WITH_EMP_INC)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(4.7, 6.95), ylim = c(0.406,0.8), clip="off")+
  geom_point(alpha = 0.75,data = figure_1_data[is.na(label),],color = "#eb0072", shape = 16) + 
  geom_point(alpha = 0.75,data = figure_1_data[!is.na(label),], color = "black",shape = 16) +
  scale_size_continuous(range = c(2.5, 12), 
                        name = graph_attributes[Figure_number=="Figure 1",Legends],
                        labels = comma, 
                        limits = c(min(figure_1_data[,TOTAL_WITH_EMP_INC]), NA),
                        breaks = c(1000, 5000, 10000,25000)) +
  # geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = -0.12, nudge_y = -0.01, na.rm = TRUE, size = 6) +
  geom_text_repel(aes(label = label), 
                  color = "black",
                  family="Replica-Regular",
                  size = 2.5, 
                  box.padding = 0.5, 
                  max.overlaps = 10,
                  segment.color = "transparent")+
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_vline(xintercept = median_AIOE, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  annotate("rect",
           xmax=6.96,
           xmin=6.25,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HEHC",English], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=6.96,
           xmin=6.25,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HELC",English], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.467,
           xmin=4.777,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LELC",English], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.467,
           xmin=4.777,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LEHC",English], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  labs(x = graph_attributes[Figure_number=="Figure 1",X_Axis],
       y = graph_attributes[Figure_number=="Figure 1",Y_Axis],
       title = "Figure 1",
       subtitle = "Nonprofit Occupations by Quadrant and Employment Size",
       caption = "Figure 1") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


figure_1_data_fr <- fread("figure_1_nonprofit_fr.csv")

figure_1_nonprofit_fr<-
  ggplot(figure_1_data_fr, aes(x = aioe, y = groupmean, size = TOTAL_WITH_EMP_INC)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(4.7, 6.95), ylim = c(0.406,0.8), clip="off")+
  geom_point(alpha = 0.75,data = figure_1_data_fr[is.na(label),],color = "#eb0072", shape = 16) + 
  geom_point(alpha = 0.75,data = figure_1_data_fr[!is.na(label),], color = "black",shape = 16) +
  scale_size_continuous(range = c(2.5, 12), 
                        name = graph_attributes[Figure_number=="Figure 1",Legends],
                        labels = c("1 000", "5 000","10 000", "25 000"), 
                        limits = c(min(figure_1_data_fr[,TOTAL_WITH_EMP_INC]), NA),
                        breaks = c(1000, 5000, 10000,25000)) +
  # geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = -0.12, nudge_y = -0.01, na.rm = TRUE, size = 6) +
  geom_text_repel(aes(label = label), 
                  color = "black",
                  family="Replica-Regular",
                  size = 2.5, 
                  box.padding = 0.5, 
                  max.overlaps = 10,
                  segment.color = "transparent")+
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_vline(xintercept = median_AIOE, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  annotate("rect",
           xmax=7.00,
           xmin=6.21,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HEHC",French], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=7.00,
           xmin=6.21,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="HELC",French], 
           nudge_x = 0.2, 
           x = 6.605,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.509,
           xmin=4.735,
           ymax=0.417,
           ymin=0.401,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LELC",French], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.41,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  annotate("rect",
           xmax=5.509,
           xmin=4.735,
           ymax=0.805,
           ymin=0.79,
           fill="lightblue") +
  annotate("text", label=graph_fields[Figure=="Common" & R_label=="LEHC",French], 
           nudge_x = 0.2, 
           x = 5.122,
           y = 0.7978,
           family="Replica-Regular",
           color = "black",    # Text color
           size = 3)+
  labs(x = graph_attributes[Figure_number=="Figure 1",X_Axis_FR],
       y = graph_attributes[Figure_number=="Figure 1",Y_Axis_FR],
       title = "Figure 1",
       subtitle = "Professions du secteur sans but lucratif par quadrant et taille de l'emploi",
       caption = "Source: Calculs de l'auteur") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


figure_2_data <- fread("figure_2_data.csv")

dwa_graph_community <- plot.scatter.dais(figure_2_data,AIOE,V1,p.size=pct,group.by=Nonprofit,
                                         plot.title = "Task Specific Automation Exposure and Error Tolerance, Nonprofit Sector",
                                         x.axis = "Automation Exposure",
                                         y.axis = "Consequence of Error",
                                         alpha = 0.5,
                                         colours = set.colours(2,categorical.choice=c("hot.pink","black")),
                                         plot.fig.num = "Figure 2") +
  scale_y_continuous(limits=c(2.7,4.7)) +
  scale_x_continuous(limits=c(5.2,6.8)) +
  theme(axis.line.x = element_line(linewidth = 0.5,colour="#999999"),axis.line.y = element_line(linewidth=0.5,colour="#999999"))


figure_2_data_fr <- fread("figure_2_data_fr.csv")

dwa_graph_community_fr <- plot.scatter.dais(figure_2_data_fr,AIOE,V1,p.size=pct,group.by=Nonprofit,
                                            plot.title = "Exposition à l’automatisation spécifique aux tâches et tolérance à l’erreur, secteur sans but lucratif",
                                            x.axis = "Exposition à l’automatisation",
                                            y.axis = "Conséquence des erreurs",
                                            alpha = 0.5,
                                            colours = set.colours(2,categorical.choice=c("black","hot.pink")),
                                            plot.fig.num = "Figure 2") +
  scale_y_continuous(limits=c(2.7,4.7)) +
  scale_x_continuous(limits=c(5.2,6.8)) +
  theme(axis.line.x = element_line(linewidth = 0.5,colour="#999999"),axis.line.y = element_line(linewidth=0.5,colour="#999999"))

