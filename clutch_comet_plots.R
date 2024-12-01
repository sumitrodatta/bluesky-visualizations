library(tidyverse)
library(ggdark)
library(magick)
library(ggforce)
library(patchwork)

pbp_curr_yr=read_csv(paste0("Data/2024-25_pbp.csv")) %>% 
  group_by(GAME_ID) %>% 
  fill(c(SCORE,SCOREMARGIN)) %>%
  replace_na(list(SCORE="0 - 0",SCOREMARGIN="TIE")) %>% ungroup() %>% 
  mutate(SCOREMARGIN=if_else(SCOREMARGIN=="TIE",0,as.numeric(SCOREMARGIN))) %>%
  separate(SCORE,sep=" - ",into=c("visitor_score","home_score"),convert = TRUE) %>%
  mutate(clutch=(PERIOD==4 & abs(SCOREMARGIN)<=5) & PCTIMESTRING<= hms::hms(hours=5)|(PERIOD>4)) %>% ungroup()

free_throws=pbp_curr_yr %>% 
  filter(if_any(c(HOMEDESCRIPTION,VISITORDESCRIPTION),
                ~str_detect(.,"Free Throw")))

clutch_free_throws=free_throws %>% filter(clutch) %>% 
  group_by(PLAYER1_NAME) %>%
  summarize(clutch_fts_attempted=n(),
            clutch_fts_missed=sum(str_detect(VISITORDESCRIPTION,"MISS")|
                                    str_detect(HOMEDESCRIPTION,"MISS"),
                                  na.rm=TRUE),
            clutch_ft_percent=1-clutch_fts_missed/clutch_fts_attempted) %>% 
  ungroup()

non_clutch_free_throws=free_throws %>% filter(!clutch) %>% 
  group_by(PLAYER1_NAME) %>%
  summarize(non_clutch_fts_attempted=n(),
            non_clutch_fts_missed=sum(str_detect(VISITORDESCRIPTION,"MISS")|
                                        str_detect(HOMEDESCRIPTION,"MISS"),
                                      na.rm=TRUE),
            non_clutch_ft_percent=1-non_clutch_fts_missed/non_clutch_fts_attempted) %>% ungroup()

clutch_differential=inner_join(clutch_free_throws,non_clutch_free_throws) %>% 
  mutate(differential=clutch_ft_percent-non_clutch_ft_percent) %>% 
  filter(clutch_fts_attempted>=10)

# https://thef5.substack.com/p/how-to-comet-plot
clutch_ft_comet_plotter<-function(type="clutch_fts_attempted",sort="best"){
  df_for_plot=clutch_differential %>%
    mutate(player=paste0(PLAYER1_NAME,"\n(",clutch_fts_attempted," Clutch FTAs)"))
  pos_neg_colors=`if`(type=="differential" & sort=="worst",c("#E64B35FF", "#00A087FF"),c("#00A087FF", "#E64B35FF"))
  subtitle=case_when(type=="clutch_fts_attempted"~
                       "Top 10 in Clutch Free Throws Attempted;",
                     (type=="differential" && sort=="best")~
                       "Top 10 in Differential between Clutch & Non-Clutch FT% (min 10 Clutch FTA);",
                     TRUE~
                       "Bottom 10 in Differential between Clutch & Non-Clutch FT% (min 10 Clutch FTA);")
  title=case_when(type=="clutch_fts_attempted"~
                       "Differential between Clutch & Non-Clutch FT%",
                     (type=="differential" && sort=="best")~
                       'The "Ice, Ice, Baby" Award (sponsored by Hisense)',
                     TRUE~"The Clutch Line Syndrome Award (presented by Diar DeRozan)")
  
  type<-sym(type)
  if (type=="clutch_fts_attempted"){
    df_for_plot=df_for_plot %>% slice_max(!!type,n=10)
  }
  else if (type=="differential" & sort=="best"){
    df_for_plot=df_for_plot %>% slice_max(!!type,n=10)
  }
  else if (type=="differential" & sort=="worst"){
    df_for_plot=df_for_plot %>% slice_min(!!type,n=10)
  }
  comet_plot=df_for_plot %>%
    ggplot() +
    geom_link(aes(x=non_clutch_ft_percent,
                  y=fct_reorder(player,!!type),
                  xend=clutch_ft_percent,yend=player,
                  size=stat(index),color=differential<0)) +
    scale_color_manual(values = pos_neg_colors) +
    scale_size(range = c(.01, 4))+
    scale_x_continuous(labels = scales::percent,limits=c(0.4,1)) +
    geom_point(
      data = df_for_plot %>% 
        filter(differential<0),
      aes(clutch_ft_percent, y=fct_reorder(player,!!type), 
          color = differential<0),
      shape = 21,
      fill = "white",
      size = 3.5
    )  +
    geom_point(
      data = df_for_plot %>% 
        filter(differential>0),
      aes(clutch_ft_percent, y=fct_reorder(player,!!type), 
          color = differential<0),
      shape = 21,
      fill = "white",
      size = 3.5
    )+
    dark_theme_gray()+
    theme(legend.position = 'none',plot.title.position = 'plot')+
    labs(title=title,
         subtitle=str_wrap(paste(subtitle,
                                 "Clutch defined as game within five points in the last 5 minutes of the 4th quarter, 
                                 or any score margin in overtime periods."),width = 90),
         y="",
         x="Free Throw Percentage")+
    plot_annotation(
      caption="Inspiration taken from Owen Phillips' tutorial: https://thef5.substack.com/p/how-to-comet-plot")
  return(comet_plot)
}

best_differential=clutch_ft_comet_plotter(type="differential",sort="best")
best_differential

worst_differential=clutch_ft_comet_plotter(type="differential",sort="worst")
worst_differential

most_clutch_ftas=clutch_ft_comet_plotter(type="clutch_fts_attempted",sort="best")
most_clutch_ftas


ggsave("Best_Diff.png", best_differential, w = 7, h = 6, dpi = 300)
ggsave("Worst_Diff.png", worst_differential, w = 7, h = 6, dpi = 300)
ggsave("Most_Clutch_FTAs.png", most_clutch_ftas, w = 7, h = 6, dpi = 300)


# Make an inset plot that we'll use for our key
inset_plot=clutch_differential %>% 
  ggplot() + 
  geom_link(aes(x = 0, y = 1, xend = 1, yend = 1, size = stat(index)), color = "#00A087FF")  + 
  scale_size(range = c(.00001, 3)) + 
  scale_y_continuous(limits = c(.95, 1.05)) +
  scale_x_continuous(limits = c(-.2, 1.2)) +
  labs(title = "KEY") +
  coord_cartesian(clip = 'off') +
  geom_point(aes(x = 1, y = 1), color = "#00A087FF",shape = 21,fill = "white", size = 2.5) + 
  dark_theme_gray()+
  theme(legend.position = 'none', 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        #plot.background = element_rect(fill = 'white', color = "black"),
        plot.title = element_text(hjust = .5, face = 'bold')) +
  annotate(geom = 'text', x = 0.5, y = 1.025, label = "Free Throw\nPercentage Differential", hjust = .5, lineheight = 1,size=2.5)  + 
  annotate(geom = 'text', x = 0, y = .985, label = "Non-Clutch\nFT%", hjust = .5, lineheight = 1,size=1.85) + 
  annotate(geom = 'text', x = 1, y = .985, label = "Clutch\nFT%", hjust = .5, lineheight = 1,size=1.85)

inset_plot

ggsave("Inset.png",  inset_plot, w = 1.5, h = 1.5, dpi = 300)

# Read in Inset plot
inset <- image_read("Inset.png")

# Read in Comet plot
graf1 <- image_read("Best_Diff.png")
graf2 <- image_read("Worst_Diff.png")
graf3 <- image_read("Most_Clutch_FTAs.png")

# Layer Inset plot on top of Comet plot
image_composite(graf1, inset, offset = "+500+900") %>% image_write("Best_Diff.png")

image_composite(graf2, inset, offset = "+500+500") %>% image_write("Worst_Diff.png")

image_composite(graf3, inset, offset = "+600+700") %>% image_write("Most_Clutch_FTAs.png")
