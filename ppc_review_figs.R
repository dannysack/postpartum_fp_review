library(tidyverse)
library(readxl)
library(ggpubr)
library(extrafont)
library(cowplot)
library(forestplot)
library(grid)
loadfonts()
# setwd
setwd("/Users/sackd/Documents/GitHub/postpartum_fp_review/")

# figure 2

# read in forest sheet
for_tib <- read_excel("20210824redcap_export.xlsx", sheet = "forest")

# create a function to calculate difference to calculate confidence interval
ci_rd <- function(p_i, p_c, n_i, n_c) {
  num1 <- p_i * (1 - p_i)
  num2 <- p_c * (1 - p_c)
  frac1 <- num1 / n_i
  frac2 <- num2 / n_c
  return(1.96*sqrt(frac1 + frac2))
}

# now calculate lb and ub
for_tib <- for_tib %>%
  mutate(Study = factor(Study, levels = for_tib$Study),
         RD_lb = RD - ci_rd(Prop_i, Prop_c, N_i, N_c),
         RD_ub = RD + ci_rd(Prop_i, Prop_c, N_i, N_c),
         `RD (95% CI)` = paste0(round(RD, 2), " (", round(RD_lb, 2),
                                ", ", round(RD_ub, 2), ")"))

# fix one cell
for_tib$`RD (95% CI)`[2] <- "0.15 (-)"

# now make forest plot with accompanying text
fig2 <- ggplot(for_tib) +
  geom_point(aes(y = fct_rev(Study), x = RD, color = Overall.RoB),
             shape = 15, size = 2) +
  geom_errorbarh(aes(y = fct_rev(Study), 
                     xmin = RD_lb, xmax = RD_ub,
                     color = Overall.RoB),
                 height = 0.2, size = 0.5) +
  geom_text(aes(y = Study, x = -0.75, 
                label = Study, 
                hjust = 0), size = 3) +
  annotate(geom = "text", x = -0.75, y = 17, 
           label = "Study", hjust = 0,
           fontface = "bold", size = 3) +
  geom_text(aes(y = Study, x = -0.4, 
                label = `RD (95% CI)`, 
                hjust = 0), size = 3) +
  annotate(geom = "text", x = -0.4, y = 17, 
           label = "RD (95% CI)", hjust = 0,
           fontface = "bold", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.25) +
  scale_color_manual(name = "Risk of Bias",
                     values = c("High" = "#fe9f6d",
                                "Some concerns" = "#de4968",
                                "Low" = "#8c2981")) +
  xlab("Unadjusted Risk Difference") + ylab("") +
  scale_x_continuous(breaks = c(-0.25, 0, 0.25, 0.5),
                     limits = c(-0.75, 0.6)) +
  coord_cartesian(clip = "off") +
  theme_pubr() +
  theme(text = element_text(family = "Arial", size = 10),
        legend.position = c(0.9, 0.1),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(size = 0.25),
        axis.ticks.x = element_line(size = 0.25),
        legend.background = element_rect(fill='transparent'),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.25, "cm"))
fig2
ggsave("../fig2.pdf", width = 8, height = 4)
embed_fonts("../fig2.pdf")

# figure 3

# read in RoB sheet
widerob <- read_excel("20210824redcap_export.xlsx", sheet = "RoB")

# break into two dataframes
widerob_scores <- widerob %>%
  select(Study, ends_with(".RoB"))

widerob_questions <- widerob %>%
  select(Study, !ends_with(".RoB"))

# make long format
longrob_scores <- widerob_scores %>% 
  gather(Question, Answer, Rand.RoB:Overall.RoB) %>%
  mutate(Question = factor(Question, levels = names(widerob_scores)[-1]),
         Study = factor(Study, levels = widerob$Study),
         Answer = factor(Answer, levels = c("Low", "Some concerns", "High")))

longrob_questions <- widerob_questions %>% 
  gather(Question, Answer, Rand.1:SelRes.3) %>%
  mutate(Question = factor(Question, levels = names(widerob_questions)[-1]),
         Study = factor(Study, levels = widerob$Study),
         Answer = ifelse(is.na(Answer), "Not Applicable", Answer),
         Answer = factor(Answer, levels = c("Not Applicable", "No Information",
                                            "No", "Probably No", "Probably Yes", "Yes")))


themeheat <- theme(text = element_text(family = "Arial"),
                   axis.text.x = element_text(angle = 45, family = "Arial", size = 8, hjust = 1),
                   axis.text.y = element_text(family = "Arial", size = 8),
                   axis.line = element_blank(),
                   legend.position = "none",
                   legend.key.size = unit(0.5, "cm"),
                   plot.margin = unit(c(0, 5.5, 0, 0), "pt"))

themehist <- theme(text = element_text(family = "Arial", size = 8),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(family = "Arial", size = 8),
                  axis.line = element_blank(),
                  legend.position = "left",
                  legend.key.size = unit(0.5, "cm"),
                  plot.margin = unit(c(0, 5.5, 0, 0), "pt"))

# make heatmaps
heatscores <- longrob_scores %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("High" = "#fe9f6d",
                               "Some concerns" = "#de4968",
                               "Low" = "#8c2981")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat

heatquestions <- longrob_questions %>%
  ggplot() +
  geom_tile(aes(x = Question, y = fct_rev(Study), fill = Answer), width = 1) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat 

# now make stacked bar plots
countscores <- longrob_scores %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("High" = "#fe9f6d",
                               "Some concerns" = "#de4968",
                               "Low" = "#8c2981")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist

countquestions <- longrob_questions %>%
  ggplot(aes(x = Question, fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist

# combine plots
scores_plot <- plot_grid(countscores, heatscores,
                         ncol = 1, rel_heights = c(2, 5),
                         align = "v", axis = "l")
questions_plot <- plot_grid(countquestions, heatquestions, 
                         ncol = 1, rel_heights = c(2, 5),
                         align = "v", axis = "l")

plot_grid(questions_plot, scores_plot, nrow = 1,
          labels = "auto", label_size = 10,
          label_fontfamily = "Arial",
          rel_widths = c(1.5, 1))
ggsave("../Drafts/fig3.pdf", width = 9, height = 6)
embed_fonts("../Drafts/fig3.pdf")

# try figure three with a different orientation
themeheat1 <- theme(text = element_text(family = "Arial"),
                   axis.text.x = element_text(angle = 45, family = "Arial", size = 8, hjust = 1),
                   axis.text.y = element_text(family = "Arial", size = 8),
                   axis.line = element_blank(),
                   legend.position = "none",
                   legend.key.size = unit(0.5, "cm"),
                   plot.margin = unit(c(0, 0, 0, 0), "pt"))

heatscores_1 <- longrob_scores %>%
  ggplot() +
  geom_tile(aes(y = fct_rev(Question), x = Study, fill = Answer), width = 1) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("High" = "#fe9f6d",
                               "Some concerns" = "#de4968",
                               "Low" = "#8c2981")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat1

heatquestions_1 <- longrob_questions %>%
  ggplot() +
  geom_tile(aes(y = fct_rev(Question), x = Study, fill = Answer), width = 1) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("") +
  theme_pubr() +
  themeheat1

themehist1 <- theme(text = element_text(family = "Arial", size = 8),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(family = "Arial", size = 8),
                  axis.line = element_blank(),
                  legend.position = "bottom",
                  legend.key.size = unit(0.5, "cm"),
                  legend.direction = "vertical",
                  plot.margin = unit(c(0, 0, 0, 0), "pt"))

countscores_1 <- longrob_scores %>%
  ggplot(aes(x = fct_rev(Question), fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Risk of Bias",
                    values = c("High" = "#fe9f6d",
                               "Some concerns" = "#de4968",
                               "Low" = "#8c2981")) +
  xlab("") + ylab("Count") +
  theme_pubr() +
  themehist1 + coord_flip()

countquestions_1 <- longrob_questions %>%
  ggplot(aes(x = fct_rev(Question), fill = Answer, label = ..count..)) +
  geom_histogram(stat = "count", position = "stack", width = 1) +
  geom_text(stat = "count", size = 2, fontface = "bold",
            position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Signaling Answer",
                    values = c("Not Applicable" = "grey",
                               "No Information" = "#fbd524",
                               "No" = "#f89540",
                               "Probably No" = "#de5f65",
                               "Probably Yes" = "#b52f8c",
                               "Yes" = "#7e03a8")) +
  xlab("") + ylab("Count") +
  guides(fill = guide_legend(ncol = 2)) +
  theme_pubr() +
  themehist1 + coord_flip()

# combine plots
scores_plot_1 <- plot_grid(heatscores_1, countscores_1,
                         nrow = 1, rel_widths = c(4, 2),
                         align = "h", axis = "b")
questions_plot_1 <- plot_grid(heatquestions_1, countquestions_1, 
                            nrow = 1, rel_widths = c(4, 2),
                            align = "h", axis = "b")

plot_grid(questions_plot_1, scores_plot_1, ncol = 1,
          labels = "auto", label_size = 10,
          label_fontfamily = "Arial",
          rel_heights = c(1.5, 1))
ggsave("../fig3_alt.pdf", width = 9, height = 7)
embed_fonts("../fig3_alt.pdf")