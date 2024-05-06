#SET UP
#clear R 
rm(list=ls())

#packages
library(dplyr)
library(lubridate)
library(lme4)
library(lmerTest)
library(ggplot2)
library(patchwork)

data<- read.csv(file.choose(), stringsAsFactors = TRUE, header = TRUE)

#only first of each calf each day
data <- data %>%
  group_by(Name, Date) %>%
  filter(row_number() == 1)

names(data)[names(data)=="Year"] <- "DeerYear"
names(data)[names(data)=="seen.together"] <- "SeenTogether"
names(data)[names(data)=="PopN"] <- "Density"

#making dates
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data$BirthDate <- as.Date(data$BirthDate, format="%d/%m/%Y")
data$DeathDate <- as.Date(data$DeathDate, format="%d/%m/%Y")

#getting variables
data1 <- data[, c("Date", "DeerYear", "Group", "Sex", "Name", "MumCode", "BirthDate", "BirthYear",
                  "BirthMonth", "DeathDate", "MumGroup", "MumReprodStatusNextYear", "MumReprodStatus",
                  "SeenTogether", "Density")]

#adding birth density by matching it to BirthYear
data1 <- data1 %>% group_by(Name) %>% 
  mutate(BirthDensity=Density[match(BirthYear, DeerYear)])

#sorting NAs 
data1[data1 == ""] <- NA
colSums(is.na(data1))

#removing misssing reprod status for mum 
data1 <- data1[complete.cases(data1$MumReprodStatusNextYear), ]

#deleting rows with no birthdate 
data1 <- data1[complete.cases(data1$BirthDate), ]

#taking out sex = 3 
table(data1$Sex)
data1 <- subset(data1, Sex != 3)

#remove calves recorded as both M and F 
duplicate <- as.data.frame(unique(data1[, c("Name", "Sex")]))
duplicated_names <- duplicate$duplicated_name <- duplicated(duplicate$Name)
duplicated_names_shown <- duplicate[duplicate$duplicated_name, ]
data1 <- data1[!(data1$Name %in% duplicated_names_shown$Name), ]

#making age variable
data1$Age <- as.numeric(difftime(data1$Date, data1$BirthDate, units='days'))
data1 <- subset(data1, Age>=0)

#make sex F and M
data1$Sex <- ifelse(data1$Sex == 1, "F", ifelse(data1$Sex == 2, "M", data1$Sex))

#-------------------------------------------------------------------------------

#ASSOCIATION TRENDS

calves2years<- data1[which(data1$Age>=0 & data1$Age<730), ]
calves2years$LifeYear <- ifelse(calves2years$Age < 365, 1, 2)

calves2years <- calves2years %>% 
  mutate(Month = format(Date, "%m"))
calves2years$Month <- as.numeric(as.character(calves2years$Month))
calves2years$Month <- ifelse(calves2years$LifeYear == 2, calves2years$Month + 12, calves2years$Month)

proportions_calves2years <- calves2years %>% 
  group_by(Name, DeerYear, Month, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total = n()) %>% 
  mutate(prop_seen_together = n_seen_together / total)

combined_proportions_calves2years <- proportions_calves2years %>%
  group_by(Month, Sex) %>%
  summarise(total_prop_seen_together = mean(prop_seen_together))

combined_proportions_calves2years$Month <- factor(combined_proportions_calves2years$Month, 
                                                  levels = c("6", "7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5", 
                                                             "18", "19", "20", "21", "22", "23", "24", "13", "14", "15", "16", "17"))

ggplot(combined_proportions_calves2years, aes(x = Month, y = total_prop_seen_together, colour = Sex, group = Sex)) +
  geom_line(size=0.7) +
  labs(x = "Month", y = "Total observations association proportion", color = "Sex") +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t = 10), size=15),
        axis.title.y = element_text(margin = margin(r = 10), size=15))+
  annotate("text", x = 6, y = 0.97, label = "Calves", size = 5, hjust = 0.5) +  
  annotate("text", x = 18, y = 0.97, label = "Yearlings", size = 5, hjust = 0.5)+
  scale_y_continuous(limits = c(0.40, 1.0))+
  geom_vline(xintercept = 13, linetype = "dotted")+
  scale_x_discrete(labels = c("6", "7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5",
                              "6", "7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5"))+
  scale_colour_manual(values = c("red", "blue"), labels = c("M"="Males", "F"="Females"))

#-------------------------------------------------------------------------------

#CALVES IN FIRST YEAR

calvesfirst <- data1[which(data1$Age < 365 & data1$Age >= 0), ]

calvesfirst <- calvesfirst %>%
  mutate(Pregnant = ifelse(MumReprodStatusNextYear == "True.Yeld", 0, 1))
calvesfirst$Pregnant <- as.factor(calvesfirst$Pregnant)

model <- glmer(SeenTogether ~ Pregnant + Sex + scale(BirthDensity) + (1 | Name) + (1 | MumCode) + (1 | DeerYear), 
               data = calvesfirst, 
               family = binomial)
summary(model)

#seasons

calvesfirst_season <- calvesfirst %>%
  mutate(Month = month(Date))

calvesfirst_season <- subset(calvesfirst_season, !(Month %in% c(6, 8, 10, 12)))

calvesfirst_season$Season <- as.factor(ifelse(calvesfirst_season$Month >= 7 & calvesfirst_season$Month <= 11, 1,
                                              ifelse(calvesfirst_season$Month >= 1 & calvesfirst_season$Month <= 5, 2, NA)))

model2 <- glmer(SeenTogether ~ Season*Pregnant + Sex + (1|DeerYear) + (1|Name) + (1|MumCode), data=calvesfirst_season, family=binomial)

summary(model2)

season_means <- calvesfirst_season %>% group_by(Season, Pregnant) %>% summarise(mean=mean(SeenTogether), 
                                                                                sd=sd(SeenTogether),
                                                                                n=n()) %>% 
  mutate(se=sd/sqrt(n),
         upper=mean+se*1.96,
         lower=mean-se*1.96)

ggplot(data=season_means, aes(x = Pregnant, y = mean, colour=Season))+
  geom_errorbar(aes(ymax=upper, ymin=lower), linewidth=0.8)+
  geom_point(aes(Pregnant, mean), size=2.5)+
  labs(y= "Total observations association proportion")+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))

#seperate seasons 

season1<- subset(calvesfirst_season, Season==1)

model3 <- glmer(SeenTogether ~ Pregnant + Sex + (1|DeerYear) + (1|Name) + (1|MumCode), data=season1, family=binomial)
summary(model3)

season2<- subset(calvesfirst_season, Season==2)

model4 <- glmer(SeenTogether ~ Pregnant + Sex + (1|DeerYear) + (1|Name) + (1|MumCode), data=season2, family=binomial)
summary(model4)

#-------------------------------------------------------------------------------

#YEARLINGS

yearlings <- data1[which(data1$Age >= 365 & data1$Age <= 730),]
colSums(is.na(yearlings))

yearlings <- yearlings[complete.cases(yearlings$Density), ]

model5 <- glmer(SeenTogether ~ Sex + MumReprodStatus + scale(Density) + (1 | Name) + (1 | MumCode) + (1 | DeerYear),
               data=yearlings, family=binomial)
summary(model5)

status_means <- yearlings %>% group_by(MumReprodStatus) %>% summarise(mean=mean(SeenTogether), 
                                                                      sd=sd(SeenTogether),
                                                                      n=n()) %>% 
  mutate(se=sd/sqrt(n),
         upper=mean+se*1.96,
         lower=mean-se*1.96)

status_sex_means <- yearlings %>% group_by(MumReprodStatus, Sex) %>% summarise(mean=mean(SeenTogether), 
                                                                               sd=sd(SeenTogether),
                                                                               n=n()) %>% 
  mutate(se=sd/sqrt(n),
         upper=mean+se*1.96,
         lower=mean-se*1.96)


ggplot(data = status_sex_means, aes(x = MumReprodStatus, y = mean)) +
  geom_errorbar(aes(ymax = upper, ymin = lower, colour = Sex), linewidth = 0.5, alpha = 0.75) +
  geom_point(aes(y = mean, colour = Sex), size = 2) +
  geom_point(data=status_means, aes(y=mean, x=MumReprodStatus, colour ="Total means"), size = 2.5)+
  labs(y = "Total observations proportion association", x = "Mother's Reproductive Status") +
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  scale_color_manual(values = c("M" = "blue", "F" = "red", "Total means" = "black"))

#-------------------------------------------------------------------------------

#OFFSPRING BENEFITS
#SURVIVAL

data2 <- data1[!is.na(data1$DeathDate), ]

data2 <- data2 %>%
  mutate(Lifespan = as.numeric(DeathDate - BirthDate))

death_type<- read.csv(file.choose(), stringsAsFactors = TRUE, header = TRUE)

death_type[death_type == ""] <- NA
death_type <- death_type[complete.cases(death_type$Name), ]

data2<- unique(left_join(data2, death_type, by="Name"))
data2 <- data2[complete.cases(data2$DeathType), ]
data2 <- data2[data2$DeathType != "S", ]

calvesfirst2 <- data2[which(data2$Age <365 & data2$Age >= 0 & data2$Lifespan >= 139), ]
yearlings2 <- data2[which(data2$Age >= 365 & data2$Age <= 730 & data2$Lifespan > 365), ]
calves2years2 <- data2[which(data2$Age >= 0 & data2$Age <= 730 & data2$Lifespan >= 139), ]

#first year survival 

died <- subset(calvesfirst2, format(DeathDate, "%m") %in% c("01", "02", "03", "04")  &  Lifespan < 365 )
survived <- subset(calvesfirst2, Lifespan > 334)

died_months <- subset(died, format(Date, "%m") %in% c("07", "09", "11"))
survived_months <- subset(survived, format(Date, "%m") %in% c("07", "09", "11"))

survival <- rbind(died_months, survived_months)
survival$Survival <- ifelse(survival$Lifespan <= 334, 0, 1)

proportions <- survival %>% 
  group_by(Name, Survival, Sex, DeerYear, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)
proportions <- proportions %>%
  group_by(Name) %>%
  filter(row_number() == 1)

model6 <- glmer(Survival ~ prop_seen_together * Sex + (1|MumCode) + (1|DeerYear), data = proportions, family = binomial)
summary(model6)

model7 <- glmer(Survival ~ prop_seen_together + Sex + (1|MumCode) + (1|DeerYear), data = proportions, family = binomial)
summary(model7)

#survival into adulthood (first-year calves)

adulthood <- calvesfirst2
adulthood$Survival <- ifelse(adulthood$Lifespan >= 852, 1, 0)

years <- adulthood %>% 
  group_by(Name, Lifespan, Survival, Sex, MumCode, DeerYear) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

years <- years %>%
  group_by(Name) %>%
  filter(row_number() == 1)
years <- years %>% select(Name, DeerYear)

proportions_adulthood <- adulthood %>% 
  group_by(Name, Lifespan, Survival, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

proportions_adulthood <- left_join(proportions_adulthood, years, by = "Name")

model8<- glmer(Survival ~ prop_seen_together + Sex+ (1|MumCode) + (1|DeerYear), data = proportions_adulthood, family = binomial)
summary(model8)

adulthood_means <- proportions_adulthood %>% group_by(Survival) %>% summarise(mean=mean(prop_seen_together), 
                                                                              sd=sd(prop_seen_together),
                                                                              n=n()) %>% 
  mutate(se=sd/sqrt(n),
         upper=mean+se*1.96,
         lower=mean-se*1.96)

first<- ggplot(data=adulthood_means, aes(x = factor(Survival), y = mean, linetype=factor(Survival)))+
  geom_errorbar(aes(ymax=upper, ymin=lower), linewidth=0.75)+
  geom_point(aes(y=mean), size=2)+
  labs(y= "Average proportion association per calf", x="Survival to adulthood")+
  scale_x_discrete(labels=c("Died", "Survived"))+
  guides(linetype="none")+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  scale_y_continuous(limits=c(0.55, 0.9))

#survival into adulthood (yearlings)

adulthood2 <- yearlings2

adulthood2$Survival <- ifelse(adulthood2$Lifespan >= 852, 1, 0)

years2 <- adulthood2 %>% 
  group_by(Name, Lifespan, Survival, Sex, MumCode, DeerYear) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

years2 <- years2 %>%
  group_by(Name) %>%
  filter(row_number() == 1)
years2 <- years2 %>% select(Name, DeerYear)

proportions_adulthood2 <- adulthood2 %>% 
  group_by(Name, Lifespan, Survival, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

proportions_adulthood2 <- left_join(proportions_adulthood2, years2, by = "Name")

model9<- glmer(Survival ~ prop_seen_together + Sex + (1|DeerYear), data = proportions_adulthood2, family = binomial)
summary(model9)

adulthood_means2 <- proportions_adulthood2 %>% group_by(Survival) %>% summarise(mean=mean(prop_seen_together), 
                                                                                sd=sd(prop_seen_together),
                                                                                n=n()) %>% 
  mutate(se=sd/sqrt(n),
         upper=mean+se*1.96,
         lower=mean-se*1.96)

second<- ggplot(data=adulthood_means2, aes(x = factor(Survival), y = mean, linetype=factor(Survival)))+
  geom_errorbar(aes(ymax=upper, ymin=lower), linewidth=0.75)+
  geom_point(aes(y=mean), size=2)+
  labs(y= "Average proportion association per yearling", x="Survival to adulthood")+
  scale_x_discrete(labels=c("Died", "Survived"))+
  guides(linetype="none")+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  scale_y_continuous(limits=c(0.55, 0.9))

survivalplots<- first + second + plot_layout(ncol=2)
survivalplots

#LIFESPAN 

#calves in first year

proportions_calvesfirst <- calvesfirst2 %>% 
  group_by(Name, Lifespan, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

model10 <- lmer(sqrt(Lifespan) ~ prop_seen_together + Sex + (1|MumCode), data = proportions_calvesfirst)
summary(model10)

span1<- ggplot(proportions_calvesfirst, aes(x = prop_seen_together, y= Lifespan))+
  geom_point(alpha = 0.3, size=1.2)+
  geom_smooth(method="lm", se=TRUE, colour="Red")+
  labs(x = "Proportion association per calf",
       y = "Lifespan (days)")+
  theme_minimal()+
  coord_cartesian(xlim = c(0, max(proportions_calvesfirst$prop_seen_together)),
                  ylim = c(0, max(proportions_calvesfirst$Lifespan)))

#yearlings

proportions_yearlings <- yearlings2 %>% 
  group_by(Name, Lifespan, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

model11 <- lmer(sqrt(Lifespan) ~ prop_seen_together + Sex + (1|MumCode) , data = proportions_yearlings)
summary(model11)

span2<- ggplot(proportions_yearlings, aes(x = prop_seen_together, y= Lifespan))+
  geom_point(alpha = 0.5, size=1.2)+
  geom_smooth(method="lm", se=TRUE, colour="blue")+
  labs(x = "Proportion associated per yearling",
       y = "Lifespan (days)")+
  theme_minimal()+
  coord_cartesian(xlim = c(0, max(proportions_yearlings$prop_seen_together)),
                  ylim = c(0, max(proportions_yearlings$Lifespan)))


spanplots<- span1 + span2 + plot_layout(ncol=1)
spanplots

#removing early mortality

adultlifespan <- subset(calvesfirst2, Lifespan>=852)

proportions_adults <- adultlifespan %>% 
  group_by(Name, Lifespan, Sex, MumCode) %>%
  summarise(n_seen_together = sum(SeenTogether),
            total=n()) %>% 
  mutate(prop_seen_together = n_seen_together/total)

model12 <- glm(sqrt(Lifespan) ~ prop_seen_together + Sex, data = proportions_adults)
summary(model12)
