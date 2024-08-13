## Visualizing the data
library(tidyverse)
library(GGally)
library(scales)
library(reshape2)
library(directlabels)
library(ggpubr)

setwd("/Users/Thomas/Documents/Preasp_Misa/GitHub/QE2_Preasp")
aggregated <- read.csv("aggregated_data.csv")

#### GETTING TOKEN COUNTS BEFORE EXCLUSIONS #####

summary(aggregated)
summary(as.factor(aggregated$vowel))
table(aggregated$vowel)
table(aggregated$Year)

# t <- with(aggregated, table(vowel,Year))
# write.table(t, file = "count_vowel_year.txt", sep = ",", quote = FALSE, row.names = T)
# 
# t <- with(aggregated, table(vowel))
# write.table(t, file = "count_vowel.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(Year))
# write.table(t, file = "count_year.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(coda_cons))
# write.table(t, file = "count_following.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(foot))
# write.table(t, file = "count_footing.txt", sep = ",", quote = FALSE, row.names = F)

### DOING EXCLUSIONS ####

## Filter out the one token of ɔɪ

aggregated <- aggregated %>% filter(vowel!="ɔɪ")

## Filter out the few that are initial footing

aggregated <- aggregated %>% filter(foot!="initial")

#### GETTING TOKEN COUNTS AFTER EXCLUSIONS #####

# summary(aggregated)
# 
# p <- aggregated %>% 
#   ungroup() %>% 
#   group_by(Year) %>%
#   summarise(total_tokens=n(),
#             pre_tokens=sum(pre),
#             pre_amb_tokens=sum(pre_amb),
#             pre_percent= round(100*pre_tokens/total_tokens, 1),
#             pre_amb_percent= round(100*pre_amb_tokens/total_tokens, 1))
# write.table(p, file = "pre_year.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(vowel,Year))
# write.table(t, file = "count_vowel_year.txt", sep = ",", quote = FALSE, row.names = T)
# 
# t <- with(aggregated, table(vowel))
# write.table(t, file = "count_vowel.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(Year))
# write.table(t, file = "count_year.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(coda_cons))
# write.table(t, file = "count_following.txt", sep = ",", quote = FALSE, row.names = F)
# 
# t <- with(aggregated, table(foot))
# write.table(t, file = "count_footing.txt", sep = ",", quote = FALSE, row.names = F)


###### Preasp x consonant #######

## Set colours

colors <- c("definitely present" = "red", "including ambiguous" ="orange")

## pre by following consonant (medial only)

df.plot <- aggregated %>%
  filter(foot == "medial") %>%
  group_by(coda_cons) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n())

df.plot %>% 
  ggplot (aes(x=reorder(coda_cons, `definitely present`))) +
  geom_segment(aes(y=`definitely present`,xend=coda_cons,yend=0))+
  geom_point(aes(y=`definitely present`, color="definitely present"), size=4)+
  geom_segment(aes(y=`including ambiguous`,xend=coda_cons,yend=0),linetype="dotdash")+
  geom_point(aes(y=`including ambiguous`,color="including ambiguous"), size=4)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=colors) +
  xlab("Following consonant")+
  ylab("Proportion of tokens") +
  ylim(0,0.32) +
  labs(color='Pre-aspiration', title='Proportion of tokens with pre-aspiration, medial footing') 



## pre by following consonant (final only)

df.plot <- aggregated %>%
  filter(foot == "final") %>%
  group_by(coda_cons) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n())

df.plot %>% 
  ggplot (aes(x=reorder(coda_cons, `definitely present`))) +
  geom_segment(aes(y=`definitely present`,xend=coda_cons,yend=0))+
  geom_point(aes(y=`definitely present`, color="definitely present"), size=4)+
  geom_segment(aes(y=`including ambiguous`,xend=coda_cons,yend=0),linetype="dotdash")+
  geom_point(aes(y=`including ambiguous`,color="including ambiguous"), size=4)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=colors) +
  xlab("Following consonant")+
  ylab("Proportion of tokens") +
  ylim(0,0.32) +
  labs(color='Pre-aspiration', title='Proportion of tokens with pre-aspiration, final footing') 




###### Preasp x vowel #######

## pre by vowel, medial only 

df.plot <- aggregated %>%
  filter(foot == "medial") %>%
  group_by(vowel) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n())

df.plot %>% 
  ggplot (aes(x=reorder(vowel, `definitely present`))) +
  geom_segment(aes(y=`definitely present`,xend=vowel,yend=0))+
  geom_point(aes(y=`definitely present`, color="definitely present"), size=4)+
  geom_segment(aes(y=`including ambiguous`,xend=vowel,yend=0),linetype="dotdash")+
  geom_point(aes(y=`including ambiguous`,color="including ambiguous"), size=4)+
  theme_bw()+
  scale_color_manual(values=colors) +
  xlab("Preceding vowel")+
  ylab("Proportion of tokens with pre-aspiration present, medial footing") +
  labs(color='Pre-aspiration') 

## pre by vowel, final only 

df.plot <- aggregated %>%
  filter(foot == "final") %>%
  group_by(vowel) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n())

df.plot %>% 
  ggplot (aes(x=reorder(vowel, `definitely present`))) +
  geom_segment(aes(y=`definitely present`,xend=vowel,yend=0))+
  geom_point(aes(y=`definitely present`, color="definitely present"), size=4)+
  geom_segment(aes(y=`including ambiguous`,xend=vowel,yend=0),linetype="dotdash")+
  geom_point(aes(y=`including ambiguous`,color="including ambiguous"), size=4)+
  theme_bw()+
  scale_color_manual(values=colors) +
  xlab("Preceding vowel")+
  ylab("Proportion of tokens with pre-aspiration present, final footing") +
  labs(color='Pre-aspiration') 



###### Preasp x year #######

## pre by year + best fit line

aggregated$Year <- as.numeric(aggregated$Year)
df.plot <- aggregated %>%
  group_by(Year) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n()) %>%
  gather(`Pre-aspiration`, proportion, -Year) %>%
  mutate(`Pre-aspiration` = factor(`Pre-aspiration`, levels=c("including ambiguous","definitely present")))

p <- df.plot %>%
  ggplot (aes(x=Year, y=proportion, color=`Pre-aspiration`)) +
  geom_point(size=4)+
  scale_x_continuous() +
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of tokens") +
  labs(title='Proportion of tokens with pre-aspiration present by year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(fill=`Pre-aspiration`, color=`Pre-aspiration`), method="lm",se=T) +
  stat_regline_equation(aes(label=paste(..adj.rr.label..)), label.x = 2005)

ggpar(p, palette = c("orange","red"))


## pre by year + best fit line, excluding 1964

aggregated$Year <- as.numeric(aggregated$Year)
df.plot <- aggregated %>%
  filter(Year != 1964) %>% 
  group_by(Year) %>%
  summarise(`definitely present` = sum(pre)/n(),
            `including ambiguous` = sum(pre_amb)/n()) %>%
  gather(`Pre-aspiration`, proportion, -Year) %>%
  mutate(`Pre-aspiration` = factor(`Pre-aspiration`, levels=c("including ambiguous","definitely present")))

p <- df.plot %>%
  ggplot (aes(x=Year, y=proportion, color=`Pre-aspiration`)) +
  geom_point(size=4)+
  scale_x_continuous() +
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of tokens") +
  labs(title='Proportion of tokens with pre-aspiration present by year, excluding 1964') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(fill=`Pre-aspiration`, color=`Pre-aspiration`), method="lm",se=T) +
  stat_regline_equation(aes(label=paste(..adj.rr.label..)), label.x = 2005)

ggpar(p, palette = c("orange","red"))


####### Breathiness x year ######

## breathiness by year + best fit line

aggregated$Year <- as.numeric(aggregated$Year)
df.plot <- aggregated %>%
  group_by(Year) %>%
  summarise(`definitely present` = sum(br)/n(),
            `including ambiguous` = sum(br_amb)/n()) %>%
  gather(Breathiness, proportion, -Year) %>%
  mutate(Breathiness = factor(Breathiness, levels=c("including ambiguous","definitely present")))

p <- df.plot %>%
  ggplot (aes(x=Year, y=proportion, color=Breathiness)) +
  geom_point(size=4)+
  scale_x_continuous() +
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of tokens") +
  labs(title='Proportion of tokens with breathiness present by year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(fill=Breathiness, color=Breathiness), method="lm",se=T) +
  stat_regline_equation(aes(label=paste(..adj.rr.label..)), label.x = 2005)

ggpar(p, palette = c("orange","red"))


## breathiness by year + best fit line, excluding 1964

aggregated$Year <- as.numeric(aggregated$Year)
df.plot <- aggregated %>%
  filter(Year != 1964) %>%
  group_by(Year) %>%
  summarise(`definitely present` = sum(br)/n(),
            `including ambiguous` = sum(br_amb)/n()) %>%
  gather(Breathiness, proportion, -Year) %>%
  mutate(Breathiness = factor(Breathiness, levels=c("including ambiguous","definitely present")))

p <- df.plot %>%
  ggplot (aes(x=Year, y=proportion, color=Breathiness)) +
  geom_point(size=4)+
  scale_x_continuous() +
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of tokens") +
  labs(title='Proportion of tokens with breathiness present by year, excluding 1964') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(fill=Breathiness, color=Breathiness), method="lm",se=T) +
  stat_regline_equation(aes(label=paste(..adj.rr.label..)), label.x = 2005)

ggpar(p, palette = c("orange","red"))


####### Creakiness x year ########

## creakiness by year + best fit line

aggregated$Year <- as.numeric(aggregated$Year)
df.plot <- aggregated %>%
  group_by(Year) %>%
  summarise(`definitely present` = sum(cr)/n()) %>%
  gather(Glottalisation, proportion, -Year)

p <- df.plot %>%
  ggplot (aes(x=Year, y=proportion)) +
  geom_point(size=4, color="red")+
  scale_x_continuous() +
  theme_bw()+
  xlab("Year")+
  ylab("Proportion of tokens") +
  labs(title='Proportion of tokens with glottalisation present by year') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(fill=Glottalisation, color=Glottalisation), method="lm",se=T) +
  stat_regline_equation(aes(label=paste(..adj.rr.label..)), label.x = 1957, color="red")

ggpar(p, palette = c("red"))




####### Co-occurrence ########

mycolors <- c("lightcyan2","darkseagreen3","darkseagreen2","cornsilk1")

## Preaspiration and breathiness co-occurrence, separated by foot ##

##
pre.br <- aggregated %>%
  select(1:7,pre,br)
pre.br$pre_br <- ifelse(pre.br$pre==1 & pre.br$br==1,1,0)
pre.br$pre_alone <- ifelse(pre.br$pre==1 & pre.br$br==0,1,0)
pre.br$br_alone <- ifelse(pre.br$pre==0 & pre.br$br==1,1,0)

## Get tables ##

## percentages

t <- pre.br %>% 
  ungroup() %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `breathiness with pre-aspiration`=sum(pre_br)/n(),
            `breathiness alone`=sum(br_alone)/n(),
            `neither`=1-(`pre-aspiration alone`+`breathiness with pre-aspiration`+`breathiness alone`))

write.table(t, file = "cooc_pre_br_pct.txt", sep = ",", quote = FALSE, row.names = F)

## numbers

t <- pre.br %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone),
            `breathiness with pre-aspiration`=sum(pre_br),
            `breathiness alone`=sum(br_alone),
            `neither`=n()-(`pre-aspiration alone`+`breathiness with pre-aspiration`+`breathiness alone`),
            `total` = n())

write.table(t, file = "cooc_pre_br_n.txt", sep = ",", quote = FALSE, row.names = F)


## Make it into a plot ##

df.plot <- pre.br %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `breathiness with pre-aspiration`=sum(pre_br)/n(),
            `breathiness alone`=sum(br_alone)/n(),
            `neither`=1-(`pre-aspiration alone`+`breathiness with pre-aspiration`+`breathiness alone`)) %>%
  melt()

df.plot$foot <- as.character(df.plot$foot)

df.plot %>%
  ggplot(aes(fill=variable, y=value, x=foot, label=round(value,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and breathiness within tokens") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="", reverse=T)) 



## Preaspiration and creakiness co-occurrence, separated by foot ##

mycolors <- c("lightcyan2","indianred3","indianred1","cornsilk1")

pre.cr <- aggregated %>%
  select(1:6,pre,cr)
pre.cr$pre_cr <- ifelse(pre.cr$pre==1 & pre.cr$cr==1,1,0)
pre.cr$pre_alone <- ifelse(pre.cr$pre==1 & pre.cr$cr==0,1,0)
pre.cr$cr_alone <- ifelse(pre.cr$pre==0 & pre.cr$cr==1,1,0)

## Get tables ##

## percentages

t <- pre.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `glottalisation with pre-aspiration`=sum(pre_cr)/n(),
            `glottalisation alone`=sum(cr_alone)/n(),
            `neither`=1-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone`))

write.table(t, file = "cooc_pre_cr_pct.txt", sep = ",", quote = FALSE, row.names = F)

## numbers

t <- pre.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone),
            `glottalisation with pre-aspiration`=sum(pre_cr),
            `glottalisation alone`=sum(cr_alone),
            `neither`=n()-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone`),
            `total` = n())

write.table(t, file = "cooc_pre_cr_n.txt", sep = ",", quote = FALSE, row.names = F)


## Make it into a plot ##

df.plot <- pre.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `glottalisation with pre-aspiration`=sum(pre_cr)/n(),
            `glottalisation alone`=sum(cr_alone)/n(),
            `neither`=1-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone`)) %>%
  melt()

df.plot$foot <- as.character(df.plot$foot)

df.plot %>%
  ggplot(aes(fill=variable, y=value, x=foot, label=round(value,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and glottalisation within tokens") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="", reverse=T)) 


## Preasp, breathiness, and creakiness co-occurrence


mycolors <- c("lightcyan2","indianred1","indianred3","darkorchid2","darkseagreen3","darkseagreen2","burlywood2","cornsilk1")

pre.br.cr <- aggregated %>%
  select(1:6,pre,br,cr)

pre.br.cr$pre_alone <- ifelse(pre.br.cr$pre==1 & pre.br.cr$br==0 & pre.br.cr$cr==0 ,1,0)
# take care of none with none function in next step
pre.br.cr$pre_br <- ifelse(pre.br.cr$pre==1 & pre.br.cr$br==1 & pre.br.cr$cr==0 ,1,0)
pre.br.cr$br_alone <- ifelse(pre.br.cr$pre==0 & pre.br.cr$br==1 & pre.br.cr$cr==0 ,1,0)
pre.br.cr$pre_cr <- ifelse(pre.br.cr$pre==1 & pre.br.cr$br==0 & pre.br.cr$cr==1 ,1,0)
pre.br.cr$cr_alone <- ifelse(pre.br.cr$pre==0 & pre.br.cr$br==0 & pre.br.cr$cr==1 ,1,0)
pre.br.cr$pre_br_cr <- ifelse(pre.br.cr$pre==1 & pre.br.cr$br==1 & pre.br.cr$cr==1 ,1,0)
pre.br.cr$br_cr <- ifelse(pre.br.cr$pre==0 & pre.br.cr$br==1 & pre.br.cr$cr==1 ,1,0)

## Get tables ##

## percentages

t <- pre.br.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `glottalisation alone`=sum(cr_alone)/n(),
            `glottalisation with pre-aspiration`=sum(pre_cr)/n(),
            `glottalisation, breathiness, and pre-aspiration` = sum(pre_br_cr)/n(),
            `breathiness with pre-aspiration` = sum(pre_br)/n(),
            `breathiness alone` = sum(br_alone)/n(),
            `glottalisation and breathiness, no pre-aspiration` = sum(br_cr)/n(),
            `none`=1-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone` 
                      + `glottalisation, breathiness, and pre-aspiration` + `breathiness with pre-aspiration` + `breathiness alone`))

write.table(t, file = "cooc_pre_br_cr_pct.txt", sep = ",", quote = FALSE, row.names = F)

## numbers

t <- pre.br.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone),
            `glottalisation alone`=sum(cr_alone),
            `glottalisation with pre-aspiration`=sum(pre_cr),
            `glottalisation, breathiness, and pre-aspiration` = sum(pre_br_cr),
            `breathiness with pre-aspiration` = sum(pre_br),
            `breathiness alone` = sum(br_alone),
            `glottalisation and breathiness, no pre-aspiration` = sum(br_cr),
            `none`=n()-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone` 
                      + `glottalisation, breathiness, and pre-aspiration` + `breathiness with pre-aspiration` 
                      + `breathiness alone` + `glottalisation and breathiness, no pre-aspiration`),
            `total`=n())



write.table(t, file = "cooc_pre_br_cr_n.txt", sep = ",", quote = FALSE, row.names = F)

## Make it into a plot ##

df.plot <- pre.br.cr %>% 
  ungroup %>%
  group_by(foot) %>%
  summarise(`pre-aspiration alone`=sum(pre_alone)/n(),
            `glottalisation alone`=sum(cr_alone)/n(),
            `glottalisation with pre-aspiration`=sum(pre_cr)/n(),
            `glottalisation, breathiness, and pre-aspiration` = sum(pre_br_cr)/n(),
            `breathiness with pre-aspiration` = sum(pre_br)/n(),
            `breathiness alone` = sum(br_alone)/n(),
            `glottalisation and breathiness, no pre-aspiration` = sum(br_cr)/n(),
            `none`=1-(`pre-aspiration alone`+`glottalisation with pre-aspiration`+`glottalisation alone` 
                      + `glottalisation, breathiness, and pre-aspiration` + `breathiness with pre-aspiration` 
                      + `breathiness alone` + `glottalisation and breathiness, no pre-aspiration`)) %>%
  melt()

df.plot$foot <- as.character(df.plot$foot)

df.plot %>%
  ggplot(aes(fill=variable, y=value, x=foot, label=round(value,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration, breathiness, and glottalisation within tokens") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="", reverse=T)) 







######## RELEASE: Preasp and breathiness vs. release type co-ocurrence  #########

## Preaspiration and release co-occurrence, overall ##

pre.release.summary <- aggregated %>%
                      ungroup() %>% 
                      group_by(pre) %>%
                      summarise(unreleased=sum(unrel)/n(),
                                 spirantised=sum(sp)/n(),
                                 affricated=sum(aff)/n(),
                                `normal release`=1-(unreleased+spirantised+affricated)) %>%
                      gather(release, percentage, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                             "1" = "pre-aspirated",
                             "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=pre, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 


## Preaspiration and release co-occurrence, /kpt/ ##

pre.release.summary <- aggregated %>% filter(coda_cons == "k" | coda_cons == "t" | coda_cons == "p") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel)/n(),
            spirantised=sum(sp)/n(),
            affricated=sum(aff)/n(),
            `normal release`=1-(unreleased+spirantised+affricated)) %>%
  gather(release, percentage, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=pre, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, plosives /kpt/ only") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 



## Preaspiration and release co-occurrence, /t/ ##

pre.release.summary <- aggregated %>% filter(coda_cons == "t") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel)/n(),
            spirantised=sum(sp)/n(),
            affricated=sum(aff)/n(),
            `normal release`=1-(unreleased+spirantised+affricated)) %>%
  gather(release, percentage, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=pre, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, /t/ only") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 



## Preaspiration and release co-occurrence, just /t/, just medial, % ##

pre.release.summary <- aggregated %>% filter(coda_cons == "t", foot == "medial") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel)/n(),
            spirantised=sum(sp)/n(),
            affricated=sum(aff)/n(),
            `normal release`=1-(unreleased+spirantised+affricated)) %>%
  gather(release, percentage, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=pre, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, /t/ only, medial footing") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 


## Preaspiration and release co-occurrence, just /t/, just medial, total n= ##

pre.release.summary <- aggregated %>% filter(coda_cons == "t", foot == "medial") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel),
            spirantised=sum(sp),
            affricated=sum(aff),
            `normal release`=n()-(unreleased+spirantised+affricated)) %>%
  gather(release, count, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=count, x=pre, label=count)) +
  geom_bar(position="stack",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, /t/ only, medial footing") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 



## Preaspiration and release co-occurrence, just /t/, just final, % ##

pre.release.summary <- aggregated %>% filter(coda_cons == "t", foot == "final") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel)/n(),
            spirantised=sum(sp)/n(),
            affricated=sum(aff)/n(),
            `normal release`=1-(unreleased+spirantised+affricated)) %>%
  gather(release, percentage, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=pre, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, /t/ only, final footing") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 



## Preaspiration and release co-occurrence, just /t/, just final, total n= ##

pre.release.summary <- aggregated %>% filter(coda_cons == "t", foot == "final") %>%
  ungroup() %>% 
  group_by(pre) %>%
  summarise(unreleased=sum(unrel),
            spirantised=sum(sp),
            affricated=sum(aff),
            `normal release`=n()-(unreleased+spirantised+affricated)) %>%
  gather(release, count, -pre) 

pre.release.summary$pre <- as.character(pre.release.summary$pre)
pre.release.summary$pre <- recode(pre.release.summary$pre,
                                  "1" = "pre-aspirated",
                                  "0" = "no pre-aspiration")

pre.release.summary %>%
  ggplot(aes(fill=release, y=count, x=pre, label=count)) +
  geom_bar(position="stack",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  labs(y="", x="", title="Co-occurrence of pre-aspiration and release type, /t/ only, final footing") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", reverse=T)) 






## Breathiness and release co-occurrence ##

br.release.summary <- aggregated %>%
  ungroup() %>% 
  group_by(br) %>%
  summarise(unreleased=sum(unrel)/n(),
            spirantised=sum(sp)/n(),
            affricated=sum(aff)/n(),
            `normal release`=1-(unreleased+spirantised+affricated)) %>%
  gather(release, percentage, -br) 

br.release.summary$br <- as.character(br.release.summary$br)
br.release.summary$br <- recode(br.release.summary$br,
                                  "1" = "breathy",
                                  "0" = "non breathy")
br.release.summary %>%
  ggplot(aes(fill=release, y=percentage, x=br, label=round(percentage,3))) +
  geom_bar(position="fill",stat="identity") +
  geom_text(position=position_stack(vjust=0.5), angle = 45) +
  coord_flip() + 
  ylim(0,1) +
  labs(y="", x="", title="Co-occurrence of breathiness and release type") +
  scale_fill_manual(values=mycolors) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Release type", , reverse=T)) 
















# 
# 
# 
# 
# mean_environments <- aggregated %>% 
#   group_by(vowel,coda_cons,foot) %>% 
#   summarise(pre=mean(pre),br=mean(br),cr=mean(cr),unrel=mean(unrel),sp=mean(sp))
# 

## Attempt to create a function, but doesnʻt work
# percent_barplot <- function(group,aspect) {
#   
#   group_var <- enquo(group)
#   aspect_var <- enquo(var)
#   
#   df.plot <- aggregated %>%
#     group_by(!!group_var) %>%
#     summarise(present = sum(!!aspect_var)/n(),
#               absent = 1-present)}
# 
# 
#   df.plot %>%
#     melt() %>%
#     ggplot (aes(x=group_var, y=value, fill=variable)) +
#     geom_bar(stat = "identity", position = "stack")
# 
# 
# percent_barplot(vowel,pre)

