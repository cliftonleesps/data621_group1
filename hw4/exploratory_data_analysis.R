##source("data_prep.R")

## install.packages("devtools")
## devtools::install_github("haleyjeppson/ggmosaic")
## devtools::install_github("thomasp85/patchwork")

library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(patchwork)

m_df <- train_df %>% dplyr::select(TARGET_FLAG,KIDSDRIV,AGE,HOMEKIDS,YOJ,INCOME,HOME_VAL,TRAVTIME,BLUEBOOK,TIF,OLDCLAIM,CLM_FREQ,MVR_PTS,CAR_AGE) %>% tidyr::pivot_longer(!TARGET_FLAG, names_to='variable' , values_to = 'value')

## density plots
m_df %>% ggplot(aes(x=value, group=TARGET_FLAG, fill=TARGET_FLAG)) + geom_density(color='#023020') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()

m_df %>% ggplot(aes(x= value)) + geom_density(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) + theme_bw()


## box plots
m_df %>% ggplot(aes(x=TARGET_FLAG, y=value, group=TARGET_FLAG)) +
geom_boxplot(color='#023020', fill='gray') + facet_wrap(~variable, scales = 'free',  ncol = 4) +
  stat_summary(fun = "mean",  geom = "point", shape = 8, size = 2, color = "steelblue") +
    stat_summary(fun = "median",  geom = "point", shape = 8, size = 2, color = "red") + theme_bw()


## Violin plot
m_df %>% ggplot(aes(x=TARGET_FLAG, y=value, group=TARGET_FLAG)) +
    geom_violin(color='#023020', fill='gray', trim=FALSE) + facet_wrap(~variable, scales = 'free',  ncol = 4) +
    geom_boxplot(color='#023020', fill='white', width=0.1) + theme_bw();



## MOSAIC PLOTS FOR:
## "PARENT1","MSTATUS","SEX","EDUCATION","JOB","CAR_USE","CAR_TYPE","RED_CAR","REVOKED","URBANICITY"

for (c in colnames(train_df)) {
    if (class(train_df[[c]]) == "factor") {
        if (length(levels(train_df[[c]])) == 2) {
            ##print(summary(train_df[c]))
            print(paste0(c, '2'))
        } else {
            print(paste0(c, ' more'))
        }
    }
}

## PARENT1
plot_parent1 <- ggplot(data = train_df) +
  geom_mosaic(aes(x = PARENT1, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Single Parent", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## Sex
plot_sex <- ggplot(data = train_df) +
  geom_mosaic(aes(x = SEX, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Sex", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## Marriage status
plot_mstatus <- ggplot(data = train_df) +
  geom_mosaic(aes(x = MSTATUS, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Marriage Status", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## Car use
plot_car_use <- ggplot(data = train_df) +
  geom_mosaic(aes(x = CAR_USE, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Car Use", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## Red car
plot_red_car <- ggplot(data = train_df) +
  geom_mosaic(aes(x = RED_CAR, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Red Car?", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## license revoked
plot_revoked <- ggplot(data = train_df) +
  geom_mosaic(aes(x = REVOKED, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="License Revoked (Past 7 Years)", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

## urban or rural area
plot_urbanicity <- ggplot(data = train_df) +
  geom_mosaic(aes(x = URBANICITY, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Home / Work Area", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 0, margin = margin(t = -6, unit="mm")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

plot_parent1 + plot_sex + plot_mstatus + plot_car_use + plot_red_car + plot_revoked + plot_urbanicity + plot_layout(nrow = 4)


plot_education <- ggplot(data = train_df) +
  geom_mosaic(aes(x = EDUCATION, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Education", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 90, margin = margin(t = -0.5, unit="pt")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

plot_job <- ggplot(data = train_df) +
  geom_mosaic(aes(x = JOB, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Job Category", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 90, margin = margin(t = -0.5, unit="pt")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")

plot_car_type <- ggplot(data = train_df) +
  geom_mosaic(aes(x = CAR_TYPE, fill=CAR_CRASH), offset = 0.005) +
  labs(y="", x="", title="Car Type", margin=margin(b=10,unit="pt")) +
    theme(panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          axis.ticks.x= element_blank(),
          axis.text.x = element_text(angle = 90, margin = margin(t = -0.5, unit="pt")),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "right")


plot_education + plot_job + plot_car_type + plot_layout(nrow = 2)
