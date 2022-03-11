#load libraries
library(formr)
library(janitor)
library(Amelia)
library(Hmisc)
library(lavaan)
library(psych)
library(corrplot)
library(clValid)
library(clustertend)
library(factoextra)
library(caret)
library(plotly)
library(skimr)
library(tidyverse)
library(infer)
library(rstatix)
library(readxl)
library(ROSE)
library(ppsr)
library(superheat)
library(visdat)

#Scoring functions
likertNum <- function(x){
  case_when(
    x == "Strongly_Disagree" ~ 1,         #Likert Scale 1 begin
    x == "Disagree" ~ 2,
    x == "Neutral" ~ 3,
    x == "Uncertain" ~ 3,
    x == "Agree" ~ 4,
    x == "Strongly_Agree" ~ 5,  
    x == "None" ~ 1, #Likert Scale 2 begin
    x == "Little" ~ 2,
    x == "Moderately" ~ 3,
    x == "Much" ~ 4,
    x == "Extremely" ~ 5,
    x == "Never" ~ 1,                     #Likert Scale 3 begin
    x == "Rarely" ~ 2,
    x == "Sometime" ~ 3,
    x == "Often" ~ 4,
    x == "Very_Often" ~ 5,
    x == "Always" ~5,
    x == "Uncharacteristic" ~ 1,           #Likert Scale 3 begin
    x == "Somewhat_Uncharacteristic" ~ 2,
    x == "Neither" ~ 3,
    x == "Somewhat_Characteristic" ~ 4,
    x == "Characteristic" ~ 5,
    x == "Never" ~ 1,                      #Likert Scale 4 begin
    x == "Yearly" ~ 2,
    x == "Monthly" ~ 3,
    x == "Weekly" ~ 4,
    x == "Daily" ~ 5
  )
}

#load and pre-process raw data
list.files(path = "C:/Users/Administrator.BENNNBX56000004/Documents/Matt DeLoia Files/SRLN_Survey(formR)/Results_Files")

path <- "C:/Users/Administrator.BENNNBX56000004/Documents/Matt DeLoia Files/SRLN_Survey(formR)/Results_Files_BehavioralScales"
files <- dir(path, patter= "*.xlsx")

df_rawdata <- files %>% 
  map(~ read_xlsx(file.path(path, .))) %>% 
  reduce(bind_rows) %>% 
  clean_names() %>%
  drop_na(id)

write_rds(df_rawdata, "df_rawdata.rds")

df_preprocess <- df_rawdata %>% 
  select(id, test, class, group, version, rank, experience:run_time_seconds, everything()) %>% 
  rename(nerves = h1, selftalk = h2, controllable = h3, WIN = h4, breathing = h5) %>% 
  mutate(class = factor(class, levels = c("Mar", "May", "Jul", "Aug", "Nov", "Feb22")))  %>% 
  mutate_at(vars(rank, version, test, group), as.factor) %>% 
  mutate_at(vars(a1:breathing), likertNum) %>% #score items
  mutate_at(vars( a7,a8,a9,a10,  #reverse score items
                  b1,b3, b4, b6,
                  c2, c3, c4, c6, c7, c9, c10, c11, c16, c19, c22, c23,c27, c31,
                  e2, e4, e6, e7, e8, e11, e13, e15, e18, e20,
                  f3, f4, f7, f8, f9, f13, f14,
                  g1, g2, g3, g4, g6, g7, g8, g9, g10), ~(6-.x)) %>% 
  arrange(class, id)
  
vis_dat(df_preprocess)

write_rds(df_preprocess, "df_preprocess.rds") #write pre_process to file

# experience key:  1= beginner, 2= intermediate, 3= advanced

#item statistics of difficulty (mean) and standard deviation
item_stats <- df_preprocess %>%
  select(id, a1:g10) %>%
  gather(a1:g10, key=item, value=indiv_score) %>%
  group_by(item) %>% 
  summarise(score = mean(indiv_score, na.rm=TRUE), sd = sd(indiv_score, na.rm=TRUE)) %>% 
  arrange(score) %>%
  select(item, score, sd) %>% 
  mutate(score = round(score, 1), sd=round(sd, 1)) %>% 
  mutate(rank = rank (-score, ties.method = "random" ) ) %>% 
  rename("mean" = "score") %>%
  mutate(scale = substr(item, 1,1)) %>% 
  mutate(scale = if_else(scale=="a", "mental_tough", 
         if_else(scale=="b", "LocusControl",
         if_else(scale=="c", "ShortSR", 
         if_else(scale=="d", "Phys_EmotionSR", 
         if_else(scale=="e", "PANAS", 
         if_else(scale=="f", "mindfulness", 
         if_else(scale=="g", "test_anxiety", 
         if_else(scale=="h", "TTP_assessment", scale)))))))))#Rank order questions from easiest to hardest

write.csv(item_stats, "ScaleQuestions.csv") #write item stats to file

df_scored <- df_preprocess %>% 
  #left_join(df_infreq, by="id") %>% 
  group_by(id, test) %>% 
  mutate(mt_Confidence = mean(c(a1, a2, a3, a4, a5, a6 ), na.rm = TRUE),
         mt_Control = mean(c(a7, a8, a9, a10 ), na.rm = TRUE),
         mt_Positivity = mean(c(a11, a12, a13), na.rm = TRUE),
         LocusControl = mean (c(b1, b2, b3, b4, b5, b6), na.rm = TRUE),
         # LC_1 = mean(c(b4), na.rm=TRUE),
         # LC_2 = mean(c(b2, b5), na.rm=TRUE),
         # LC_3 = mean(c(b1, b3, b6), na.rm=TRUE),
         # ShortSR = mean(c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31), na.rm = TRUE),
         ShortSR_GS = mean(c(c1, c2, c6, c9, c16, c19, c21, c25, c3, c4, c7, c8, c10, c17, c26), na.rm=TRUE), #goal setting
         ShortSR_LM = mean(c(c11, c12, c14, c15, c22, c28, c29, c5, c13, c23, c24, c27, c30, c31), na.rm = TRUE), #learning from mistakes
         PhysEmotionSR = mean(c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11), na.rm = TRUE),
         PANAS_P = mean(c(e1, e3, e5, e9, e10, e12, e14, e16, e17, e19), na.rm = TRUE), 
         PANAS_N = mean(c( e2, e4, e6, e7, e8, e11, e13, e15, e18, e20), na.rm = TRUE),
         m_Observing = mean(c(f1, f6, f11), na.rm = TRUE),
         m_Describe = mean(c(f2, f7, f12), na.rm = TRUE),
         m_Awareness = mean(c(f3, f8, f13), na.rm = TRUE),
         m_Nonjudging = mean(c(f4, f9, f14), na.rm = TRUE),
         m_Nonreactivity = mean(c(f5, f10, f15), na.rm = TRUE),
         TestAnxiety = mean(c(g1, g2, g3, g4,  g6, g7, g8, g9, g10), na.rm = TRUE),
         # ta_Anxiety = mean(c(g1, g2, g3, g4, g5), na.rm=TRUE),
         # ta_Distractability = mean(c(g6), na.rm=TRUE),
         # ta_Worry = mean(c(g7, g8), na.rm = TRUE),
         # ta_Rumination = mean(c(g6, g9, g10), na.rm = TRUE),
        TTPassessment = mean(c(nerves, selftalk,controllable,WIN, breathing), na.rm=TRUE)) %>% 
  select(-(a1:breathing))

write_rds(df_scored, "df_scored.rds") #write scored data frame to file
