library(tidyverse)
# webscraping
library(rvest)
library(polite)
# extract tables from PDF
library(rJava)
library(tabulapdf)
# clean names
library(janitor)
# team color palettes
library(teamcolors)
# ggplot enhancements
library(ggrepel)
library(ggdark)
library(patchwork)
# fancy tables
library(gt)

current_year=2025
team_info=read_csv("Data/Team Summaries.csv") %>% 
  filter(season==current_year,!is.na(abbreviation)) %>% 
  select(team,abbreviation)

nba_bow=bow("https://official.nba.com/",user_agent = "Sumitro Datta",force=TRUE,delay=10)
session=nod(nba_bow,path="2023-24-nba-coachs-challenge-reviews/")

coaches_challenge_by_day=scrape(session) %>% 
  html_nodes("a") %>% html_attr("href") %>% tibble() %>%
  rename(links=".") %>%
  distinct(links) %>%
  filter(str_detect(links,"Coachs-Challenges")) %>% slice(1) %>% pull()

challenge_tables=tabulapdf::extract_tables(file=coaches_challenge_by_day)

all_challenges=challenge_tables[[1]] %>% 
  as_tibble(.name_repair = "minimal") %>%
  clean_names()

for (x in 2:length(challenge_tables)){
  new_table=challenge_tables[[x]] %>% as_tibble(.name_repair = "minimal") %>% clean_names()
  all_challenges=bind_rows(all_challenges,new_table)
}

challenges_team_summary=all_challenges %>% 
  filter(date>="10/22/2024") %>%
  group_by(team_challenged) %>% 
  summarize(num_challenges=n(),
            successful_challenges=sum(str_detect(challenge_outcome,"Successful"),
                                      na.rm = TRUE),
            success_rate=successful_challenges/num_challenges)

challenges_team_summary %>% 
  arrange(desc(success_rate),desc(num_challenges)) %>% 
  slice_max(success_rate,n=5) %>% gt() %>%
  fmt_percent(columns=success_rate)

challenges_team_summary_colors=left_join(
  team_info,
  enframe(league_pal("nba"),name="team",value = "team_color")) %>%
  mutate(abbreviation=case_when(abbreviation=="PHO"~"PHX",
                                abbreviation=="BRK"~"BKN",
                                abbreviation=="CHO"~"CHA",
                                TRUE~abbreviation)) %>%
  left_join(.,challenges_team_summary,by=join_by(abbreviation==team_challenged))

league_challenge_metrics=challenges_team_summary %>% 
  summarize(avg_challenges=mean(num_challenges),
            tot_challenges=sum(num_challenges),
            tot_success=sum(successful_challenges),
            success_rate=tot_success/tot_challenges)

challenges_team_summary_colors %>% 
  ggplot(aes(x=num_challenges,
             y=success_rate,
             color=team,
             label=abbreviation))+
  geom_vline(xintercept=league_challenge_metrics$avg_challenges,color="black",
             linetype="dotted")+
  annotate("text",x=league_challenge_metrics$avg_challenges,
           y=max(challenges_team_summary_colors$success_rate),
           label = round(league_challenge_metrics$avg_challenges,2),
           hjust = -0.5,vjust=0.5,color="black")+
  geom_hline(yintercept=league_challenge_metrics$success_rate,
             color="black",linetype="dotted")+
  annotate("text",y=league_challenge_metrics$success_rate,
           x=min(challenges_team_summary_colors$num_challenges),
           label = scales::percent(league_challenge_metrics$success_rate),
           vjust = -1,color="black")+
  scale_color_teams(guide="none")+
  geom_point()+
  geom_label_repel(fill="white",min.segment.length = 0,max.overlaps = Inf)+
  scale_y_continuous(labels = scales::percent)+
  labs(title=paste0("NBA Team Challenge Usage vs Success Rate as of ",
                    max(all_challenges$date)),
       subtitle=paste0("Average number of challenges per team: ",
                       round(league_challenge_metrics$avg_challenges,2),
                       ", with leaguewide success rate of ",
                       scales::percent(league_challenge_metrics$success_rate)))+
  plot_annotation(
    caption=paste("Data from",coaches_challenge_by_day)
  )

ggsave("Challenges Plot/NBA Challenges by Team Plot.png",height=8,width=10)

all_challenges_by_min_sec=
  all_challenges %>% 
  filter(date>="10/22/2024",!is.na(challenge_outcome)) %>%
  mutate(minute_of_quarter=as.numeric(word(game_clock_review_is_triggered,sep=":")),
         second_of_quarter=as.numeric(word(game_clock_review_is_triggered,start=2,sep=":")),
         seconds_elapsed=(period-1)*12*60+
           (if_else(period>4,4,11)-minute_of_quarter)*60+
           60-second_of_quarter,
         seconds_elapsed_in_quarter=(if_else(period>4,4,11)-minute_of_quarter)*60+
           60-second_of_quarter,
         minutes_elapsed=seconds_elapsed/60,
         minutes_elapsed_in_quarter=seconds_elapsed_in_quarter/60,
         quarter_text=factor(if_else(period>4,"Overtime",paste("Quarter",period)),
                             levels=c("Quarter 1",
                                      "Quarter 2",
                                      "Quarter 3",
                                      "Quarter 4",
                                      "Overtime")))

all_challenges_by_min_sec %>%
  ggplot(aes(x=minutes_elapsed_in_quarter,color=challenge_outcome))+
  geom_freqpoly(binwidth=1)+
  geom_point(stat="bin", aes(y=..count..), binwidth=1)+
  scale_color_manual(values = c("Unsuccessful" = "#D81A60", "Successful" = "#3BFF00"))+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  dark_theme_gray()+
  facet_wrap(~quarter_text)+
  labs(title=paste("NBA Challenge Outcome by Quarter as of",max(all_challenges$date)))+
  plot_annotation(
    caption=paste("Data from",coaches_challenge_by_day))

ggsave("Challenges Plot/NBA Challenge Outcome by Quarter.png",height=5,width=11)

all_challenges_by_min_sec %>%
  ggplot(aes(x=minutes_elapsed,color=challenge_outcome))+
  geom_freqpoly(binwidth=1)+
  geom_point(stat="bin", aes(y=..count..), binwidth=1)+
  geom_vline(xintercept=seq(12,
                            max(all_challenges_by_min_sec$minutes_elapsed),
                            by=12),
             color="white",
             linetype="dotted")+
  scale_color_manual(values = c("Unsuccessful" = "#D81A60", "Successful" = "#3BFF00"))+
  scale_x_continuous(breaks=seq(0,max(all_challenges_by_min_sec$minutes_elapsed),by=2))+
  dark_theme_gray()+
  labs(title=paste("NBA Challenge Outcome by Game Minutes Elapsed as of",
                   max(all_challenges$date)))+
  plot_annotation(
    caption=paste("Data from",coaches_challenge_by_day))

ggsave("Challenges Plot/NBA Challenge Outcome by Game Minute.png",height=5,width=11)

all_challenges %>% 
  filter(date>="10/22/2024") %>% mutate(full_trigger=paste(trigger,initial_call)) %>% 
  group_by(full_trigger,team_challenged) %>%
  summarize(n=n(),
            success=sum(str_detect(challenge_outcome,"Successful"),na.rm = TRUE),
            success_rate=success/n) %>%
  ungroup() %>%
  ggplot(aes(y=full_trigger,x=success_rate,color=team_challenged,size=n))+geom_jitter()
