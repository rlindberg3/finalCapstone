


suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(caTools))
suppressMessages(library(caret))
suppressMessages(library(GGally))

nfl_data <- read.csv("NFL_offense.csv")

#make all null temperatures at game time "room" temperature
nfl_data$temp[nfl_data$temp == "NULL"] <- 70
nfl_data$temp <- as.integer(nfl_data$temp)

#highlight temp extremes
nfl_data <- mutate(nfl_data, cold_weather= ifelse(temp < 45, 1,0))
nfl_data <- mutate(nfl_data, hot_weather=  ifelse(temp > 85, 1,0))

#weather factors
nfl_data <- mutate(nfl_data, grass_1 = ifelse(surf == "DD GrassMaster" | surf == "Grass",
                                              1,0))
nfl_data <- mutate(nfl_data, bad_weather_1 = ifelse(cond == "Light Rain" | 
                                                      cond == "Rain" |
                                                      cond == "Flurries" |
                                                      cond == "Snow" |
                                                      cond == "Foggy" |
                                                      cond == "Windy" |
                                                      cond == "Hazy" |
                                                      cond == "Thunderstorms"|
                                                      cond == "Light Snow" |
                                                      cond == "Light Showers" ,1,0))


#identify home team
nfl_data$h <- as.character(nfl_data$h)
nfl_data$team <- as.character(nfl_data$team)
nfl_data <- mutate(nfl_data, home_team_1=  ifelse(h == team, 1,0))

#identify position
nfl_data <- mutate(nfl_data, is_WR =  ifelse(pos1 == "WR", 1,0))
nfl_data <- mutate(nfl_data, is_TE =  ifelse(pos1 == "TE", 1,0))
nfl_data <- mutate(nfl_data, is_RB =  ifelse(pos1 == "RB", 1,0))
nfl_data <- mutate(nfl_data, is_QB =  ifelse(pos1 == "QB", 1,0))
#age
nfl_data <- mutate(nfl_data, age = year - yob)
#replace 0 forty with avg for position
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(forty1 = ifelse(forty == 0, mean(forty[forty>0]), forty))
#replace 0 vertical with average for position
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(vertical1 = ifelse(vertical == 0, mean(vertical[vertical>0]), vertical))
#replace 0 arm length with formula for 40% of height is arm
nfl_data$arm <- ifelse(nfl_data$arm == 0, nfl_data$height*0.4, nfl_data$arm)

nfl_data$broad <- as.numeric(nfl_data$broad)

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(broad1 = ifelse(broad == 0, mean(broad[broad>0]), broad))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(shuttle1 = ifelse(shuttle == 0, mean(shuttle[shuttle>0]), shuttle))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(cone1 = ifelse(cone == 0, mean(cone[cone>0]), cone))
#clean teams and give each team a field
nfl_data <- mutate(nfl_data, Teams =  ifelse(team == "STL" | team == "LA", "STL/LA",team))
nfl_data <- mutate(nfl_data, ARI = ifelse(Teams == "ARI",1,0))
nfl_data <- mutate(nfl_data, ATL = ifelse(Teams == "ATL",1,0))
nfl_data <- mutate(nfl_data, BAL = ifelse(Teams == "BAL",1,0))
nfl_data <- mutate(nfl_data, BUF = ifelse(Teams == "BUF",1,0))
nfl_data <- mutate(nfl_data, CAR = ifelse(Teams == "CAR",1,0))
nfl_data <- mutate(nfl_data, CHI = ifelse(Teams == "CHI",1,0))
nfl_data <- mutate(nfl_data, CIN = ifelse(Teams == "CIN",1,0))
nfl_data <- mutate(nfl_data, CLE = ifelse(Teams == "CLE",1,0))
nfl_data <- mutate(nfl_data, DAL = ifelse(Teams == "DAL",1,0))
nfl_data <- mutate(nfl_data, DEN = ifelse(Teams == "DEN",1,0))
nfl_data <- mutate(nfl_data, DET = ifelse(Teams == "DET",1,0))
nfl_data <- mutate(nfl_data, GB = ifelse(Teams == "GB",1,0))
nfl_data <- mutate(nfl_data, HOU = ifelse(Teams == "HOU",1,0))
nfl_data <- mutate(nfl_data, IND = ifelse(Teams == "IND",1,0))
nfl_data <- mutate(nfl_data, JAC = ifelse(Teams == "JAC",1,0))
nfl_data <- mutate(nfl_data, KC = ifelse(Teams == "KC",1,0))
nfl_data <- mutate(nfl_data, MIA = ifelse(Teams == "MIA",1,0))
nfl_data <- mutate(nfl_data, MINN = ifelse(Teams == "MIN",1,0))
nfl_data <- mutate(nfl_data, NE = ifelse(Teams == "NE",1,0))
nfl_data <- mutate(nfl_data, NOR = ifelse(Teams == "NO",1,0))
nfl_data <- mutate(nfl_data, NYG = ifelse(Teams == "NYG",1,0))
nfl_data <- mutate(nfl_data, NYJ = ifelse(Teams == "NYJ",1,0))
nfl_data <- mutate(nfl_data, OAK = ifelse(Teams == "OAK",1,0))
nfl_data <- mutate(nfl_data, PHI = ifelse(Teams == "PHI",1,0))
nfl_data <- mutate(nfl_data, PIT = ifelse(Teams == "PIT",1,0))
nfl_data <- mutate(nfl_data, SD = ifelse(Teams == "SD",1,0))
nfl_data <- mutate(nfl_data, SEA = ifelse(Teams == "SEA",1,0))
nfl_data <- mutate(nfl_data, SF = ifelse(Teams == "SF",1,0))
nfl_data <- mutate(nfl_data, STL = ifelse(Teams == "STL/LA",1,0))
nfl_data <- mutate(nfl_data, TB = ifelse(Teams == "TB",1,0))
nfl_data <- mutate(nfl_data, TEN = ifelse(Teams == "TEN",1,0))
nfl_data <- mutate(nfl_data, WAS = ifelse(Teams == "WAS",1,0))



#calculate the averages by player, position, and team
#receiving
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_recy_plyr = mean(recy))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_recy_pos = mean(recy))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_recy_team = mean(recy))
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rec_plyr = mean(rec))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rec_pos = mean(rec))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rec_team = mean(rec))
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_trg_plyr = mean(trg))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_trg_pos = mean(trg))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_trg_team = mean(trg))
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rectd_plyr = mean(tdrec))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rectd_pos = mean(tdrec))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rectd_team = mean(tdrec))



#running
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rbra_plyr = mean(ra))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rbra_team = mean(ra))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rbra_pos = mean(ra))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_rbry_plyr = mean(ry))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_rbry_team = mean(ry))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_rbry_pos = mean(ry))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_fuml_plyr = mean(fuml))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_fuml_team = mean(fuml))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_fuml_pos = mean(fuml))
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_tdr_plyr = mean(tdr))
nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_tdr_pos = mean(tdr))
nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_tdr_team = mean(tdr))



#passing
nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpy_plyr = mean(py))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpy_team = mean(py))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpy_pos = mean(py))


nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpc_plyr = mean(pc))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpc_team = mean(pc))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpc_pos = mean(pc))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbints_plyr = mean(ints))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbints_team = mean(ints))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbints_pos = mean(ints))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbtdp_plyr = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbtdp_team = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbtdp_pos = mean(tdp))

nfl_data <- nfl_data %>%
  group_by(player.1)%>%
  mutate(avg_qbpa_plyr = mean(pa))

nfl_data <- nfl_data %>%
  group_by(Teams)%>%
  mutate(avg_qbpa_team = mean(pa))

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(avg_qbpa_pos = mean(pa))

#age

nfl_data <- nfl_data %>%
  group_by(age, pos1)%>%
  mutate(avg_recy_age = mean(recy))

nfl_data <- nfl_data %>%
  group_by(age, pos1)%>%
  mutate(age_count = n())


nfl_data_fields<- subset(nfl_data, select = c("height", "weight", "cold_weather", "hot_weather",
                                              "home_team_1", "temp", "is_WR", "is_QB", "is_RB", "is_TE", "age",
                                              "forty1", "vertical1", "ARI", "ATL", "BAL", "BUF",
                                              "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND",
                                              "JAC", "KC", "MIA", "MINN", "NE", "NOR", "NYG", "NYJ", "OAK", "PHI", "PIT",
                                              "SD", "SEA", "STL", "TB", "TEN", "WAS",
                                              "avg_recy_plyr","avg_recy_pos","avg_recy_team","avg_rec_plyr","avg_rec_pos",
                                              "avg_rec_team", "avg_trg_plyr","avg_trg_pos","avg_trg_team","avg_rectd_plyr",
                                              "avg_rectd_pos","avg_rectd_team","avg_tdr_plyr","avg_tdr_pos","avg_tdr_team",
                                              "avg_rbra_plyr","avg_rbra_pos", "avg_rbra_team","avg_rbry_plyr","avg_rbry_pos",
                                              "avg_rbry_team","avg_fuml_plyr","avg_fuml_pos", "avg_fuml_team","avg_qbpy_plyr",
                                              "avg_qbpy_pos", "avg_qbpy_team","avg_qbpa_plyr","avg_qbpa_pos","avg_qbpa_team",
                                              "avg_qbpc_plyr","avg_qbpc_pos", "avg_qbpc_team","avg_qbints_plyr", "avg_qbints_pos",
                                              "avg_qbints_team","avg_qbtdp_plyr","avg_qbtdp_pos","avg_qbtdp_team","grass_1",
                                              "bad_weather_1"))

write.csv(nfl_data_fields,"C:/Users/Rich Lindberg/Documents/R/NFLCapstoneTest/nfl_data_fields.csv")

cor_nfl <- cor(nfl_data_fields)
image(cor_nfl)

qplot(x=Var1, y=Var2, data=melt(cor(nfl_data_fields)), fill=value, geom="tile")+
  scale_fill_gradient2(limits=c(-1, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5))

highlyCor_nfl_data_fields1 <- findCorrelation(cor_nfl, cutoff = .8)
highlyCor_nfl_data_fields1

highlyCor_nfl_data_fields2 <- findCorrelation(cor_nfl, cutoff = .85)
highlyCor_nfl_data_fields2

highlyCor_nfl_data_fields3 <- findCorrelation(cor_nfl, cutoff = .9)
highlyCor_nfl_data_fields3

filtered_nfl_data_fields <-nfl_data_fields
filtered_nfl_data_fields <- filtered_nfl_data_fields[,-highlyCor_nfl_data_fields1]
filtered_nfl_data_fields


qplot(x=Var1, y=Var2, data=melt(cor(filtered_nfl_data_fields)), fill=value, geom="tile")+
  scale_fill_gradient2(limits=c(-1, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5))


filtered_nfl_data_fields2 <- nfl_data_fields
filtered_nfl_data_fields2 <- filtered_nfl_data_fields2[,-highlyCor_nfl_data_fields2]

qplot(x=Var1, y=Var2, data=melt(cor(filtered_nfl_data_fields2)), fill=value, geom="tile")+
  scale_fill_gradient2(limits=c(-1, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5))


filtered_nfl_data_fields3 <- nfl_data_fields
filtered_nfl_data_fields3 <- filtered_nfl_data_fields3[,-highlyCor_nfl_data_fields3]

qplot(x=Var1, y=Var2, data=melt(cor(filtered_nfl_data_fields3)), fill=value, geom="tile")+
  scale_fill_gradient2(limits=c(-1, 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5))


#splitting the data into training and testing sets
set.seed(123)
split <- sample.split(nfl_data$recy, SplitRatio = 0.7)
TrainRecy <- subset(nfl_data, split == TRUE)
TestRecy <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(TrainRecy, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, TrainRecy)
testTransformed <- predict(preProcValues, TestRecy)

#ggpairs for recy
########
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("recy",colnames(filtered_nfl_data_fields[46:54]))])

#recy regression
#formula for not having to write everything out

wrrecyregform <- formula(paste("recy ~ ", 
                               paste(colnames(filtered_nfl_data_fields), collapse="+")))

#first run of the recy regression
linRegrecy <- lm(wrrecyregform, data = trainTransformed)
summary(linRegrecy)

#updating formula to take out insignigicant values
linRegrecy2 <- update(linRegrecy, ~. -height- hot_weather - home_team_1-vertical1-BUF-DAL-DEN-GB
                      -NE-NOR-NYJ-SEA
                      -avg_trg_team-avg_tdr_team-avg_rbra_team-avg_fuml_plyr-avg_fuml_team
                      -avg_qbints_plyr
                      -avg_qbints_team-avg_qbtdp_team-bad_weather1)
summary(linRegrecy2)

#updating formulas to remove any insignificant values
linRegrecy3 <- update(linRegrecy2, ~. -JAC)
summary(linRegrecy3)


#testing the results on the test results
RecyPredicted <- predict(linRegrecy3, newdata = testTransformed)

SSErecy <- sum((RecyPredicted - testTransformed$recy)^2)
SSTrecy <- sum((mean(nfl_data$recy)-testTransformed$recy)^2)
r2_recy <- 1 - SSErecy/SSTrecy 
r2_recy
rmse_recy <- sqrt(SSErecy/nrow(testTransformed))
rmse_recy

#plotting the regression 
par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegrecy3, which = c(1,2,3,5))

confint(linRegrecy3)

coef(summary(linRegrecy3))

anova(linRegrecy3)

#aic 
aic_recy <- step(lm(wrrecyregform, data = trainTransformed), direction = "backward")

######################################################
set.seed(123)
splitrec <- sample.split(nfl_data$rec, SplitRatio = 0.7)
TrainRec <- subset(nfl_data, split == TRUE)
TestRec <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(TrainRec, method = c("center", "scale"))
trainTransformedrec <- predict(preProcValues, TrainRec)
testTransformedrec <- predict(preProcValues, TestRec)


#ggpairs for rec
########
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("rec",colnames(filtered_nfl_data_fields[46:54]))])


recregform <- formula(paste("rec ~ ", 
                            paste(colnames(filtered_nfl_data_fields), collapse="+")))



linRegrec <- lm(recregform, data = trainTransformedrec)
summary(linRegrec)

linRegrec2 <- update(linRegrec, ~. -hot_weather -forty1 -GB - SEA -avg_trg_team -avg_tdr_team
                     -avg_rbra_team -avg_fuml_team -avg_qbtdp_team - avg_qbints_team)
summary(linRegrec2)

RecPredicted <- predict(linRegrec2, newdata = testTransformedrec)

SSErec <- sum((RecPredicted - testTransformedrec$rec)^2)
SSTrec <- sum((mean(nfl_data$rec)-testTransformedrec$rec)^2)
r2_rec <- 1 - SSErec/SSTrec 
r2_rec
rmse_rec <- sqrt(SSErec/nrow(testTransformedrec))
rmse_rec

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegrec2, which = c(1,2,3,5))

confint(linRegrec2)

coef(summary(linRegrec2))

anova(linRegrec2)
#aic 
aic_rec <- step(lm(recregform, data = TrainRecy), direction = "backward")

############################################################
set.seed(123)
splittrg <- sample.split(nfl_data$trg, SplitRatio = 0.7)
Traintrg <- subset(nfl_data, split == TRUE)
Testtrg <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Traintrg, method = c("center", "scale"))
trainTransformedtrg <- predict(preProcValues, Traintrg)
testTransformedtrg <- predict(preProcValues, Testtrg)


#ggpairs for trg
########
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("trg",colnames(filtered_nfl_data_fields[46:54]))])




trgregform <- formula(paste("trg ~ ", 
                            paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegtrg <- lm(trgregform, data = trainTransformedtrg)

summary(linRegtrg)

linRegtrg2 <- update(linRegtrg, ~. -height-cold_weather-hot_weather-forty1-vertical1-DAL-DEN-GB-NE-NOR-SEA
                     -avg_trg_team -avg_tdr_team-avg_rbra_team -avg_fuml_team -avg_qbtdp_team 
                     -avg_qbints_team - grass_1-bad_weather_1)

summary(linRegtrg2)

TrgPredicted <- predict(linRegtrg2, newdata = testTransformedtrg)

SSEtrg <- sum((TrgPredicted - testTransformedtrg$trg)^2)
SSTtrg <- sum((mean(nfl_data$trg)-testTransformedtrg$trg)^2)
r2_trg <- 1 - SSEtrg/SSTtrg 
r2_trg
rmse_trg <- sqrt(SSEtrg/nrow(testTransformedtrg))
rmse_trg

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegrec2, which = c(1:3,5))

confint(linRegtrg2)

coef(summary(linRegtrg2))

anova(linRegtrg2)
#aic 
aic_trg <- step(lm(trgregform, data = trainTransformedtrg), direction = "backward")


#################################################
set.seed(123)
splittdrec <- sample.split(nfl_data$tdrec, SplitRatio = 0.7)
Traintdrec <- subset(nfl_data, split == TRUE)
Testtdrec <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Traintdrec, method = c("center", "scale"))
trainTransformedtdrec <- predict(preProcValues, Traintdrec)
testTransformedtdrec <- predict(preProcValues, Testtdrec)


#ggpairs for tdrec
########
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("tdrec",colnames(filtered_nfl_data_fields[46:54]))])


tdrecregform <- formula(paste("tdrec ~ ", 
                              paste(colnames(filtered_nfl_data_fields), collapse="+")))


linRegRecTD <- lm(tdrecregform, data = trainTransformedtdrec)

summary(linRegRecTD)


linRegRecTD2 <- update(linRegRecTD, ~. -height-weight-cold_weather-hot_weather-home_team_1
                       -forty1-is_WR-is_TE-age-vertical1-ARI-BAL-BUF-CAR-CIN-CLE-DAL-DEN-DET-GB
                       -HOU-IND-JAC-KC-MIA-MINN-NE-NYG-NYJ-OAK-PHI-PIT-SEA-STL-TB-TEN-WAS
                       -avg_trg_team -avg_tdr_team-avg_rbra_team-avg_rbry_plyr
                       -avg_rbry_pos-avg_fuml_plyr-avg_fuml_team-avg_qbints_plyr_avg_qbtdp_team 
                       -avg_qbints_team - grass_1-bad_weather_1)

summary(linRegRecTD2)

linRegRecTD3 <- update(linRegRecTD2, ~. -ATL-CHI-NOR-SD-avg_qbints_plyr-avg_qbints_team)

summary(linRegRecTD3)


RectdPredicted <- predict(linRegRecTD3, newdata = testTransformedtdrec)

SSErectd <- sum((RectdPredicted - testTransformedtdrec$tdrec)^2)
SSTrectd <- sum((mean(nfl_data$tdrec)-testTransformedtdrec$tdrec)^2)
r2_rectd <- 1 - SSErectd/SSTrectd 
r2_rectd
rmse_rectd <- sqrt(SSEtrg/nrow(testTransformedtdrec))
rmse_rectd

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegRecTD3, which = c(1:3,5))

confint(linRegRecTD3)

coef(summary(linRegRecTD3))

anova(linRegRecTD3)


#aic 
aic_tdrec <- step(lm(tdrecregform, data = trainTransformedtdrec), direction = "backward")


#############################################################
set.seed(123)
splitpy <- sample.split(nfl_data$py, SplitRatio = 0.7)
Trainpy <- subset(nfl_data, split == TRUE)
Testpy <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainpy, method = c("center", "scale"))
trainTransformedpy <- predict(preProcValues, Trainpy)
testTransformedpy <- predict(preProcValues, Testpy)


#ggpairs for py
########
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("py",colnames(filtered_nfl_data_fields[46:54]))])



pyregform <- formula(paste("py ~ ", 
                           paste(colnames(filtered_nfl_data_fields), collapse="+")))


linRegQBpyds <- lm(pyregform, data = trainTransformedpy)

summary(linRegQBpyds)

linRegQBpyds2 <- update(linRegQBpyds, ~.-hot_weather-home_team_1-vertical1-ATL-BAL-DAL-DEN-DET-KC
                        -NOR-PHI-PIT-SEA-SD-WAS-avg_trg_team-avg_tdr_team-avg_rbra_team-avg_rbry_plyr
                        -avg_fuml_team-avg_qbints_team-avg_qbtdp_team-grass_1)

summary(linRegQBpyds2)

PydsdPredicted <- predict(linRegQBpyds2, newdata = testTransformedpy)

SSEpyds <- sum((PydsdPredicted - testTransformedpy$py)^2)
SSTpyds <- sum((mean(nfl_data$py)-testTransformedpy$py)^2)
r2_pyds <- 1 - SSEpyds/SSTpyds 
r2_pyds
rmse_pyds <- sqrt(SSEpyds/nrow(testTransformedpy))
rmse_pyds

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegQBpyds2, which = c(1:3,5))

confint(linRegQBpyds2)

coef(summary(linRegQBpyds2))

anova(linRegQBpyds2)

#aic 
aic_py <- step(lm(pyregform, data = trainTransformedpy), direction = "backward")


######################################################################
set.seed(123)
splitpc <- sample.split(nfl_data$pc, SplitRatio = 0.7)
Trainpc <- subset(nfl_data, split == TRUE)
Testpc <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainpc, method = c("center", "scale"))
trainTransformedpc <- predict(preProcValues, Trainpc)
testTransformedpc <- predict(preProcValues, Testpc)


#ggpairs for pc
########
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("pc",colnames(filtered_nfl_data_fields[46:54]))])


pcregform <- formula(paste("pc ~ ", 
                           paste(colnames(filtered_nfl_data_fields), collapse="+")))


linRegQBpc <- lm(pcregform, data = trainTransformedpc)
summary(linRegQBpc)

linRegQBpc2 <- update(linRegQBpc, ~.-hot_weather-home_team_1-vertical1-ATL-BAL-CIN-DAL-DEN-DET-KC
                      -MIA-PHI-PIT-SEA-STL-WAS-avg_trg_team-avg_tdr_team-avg_rbra_team
                      -avg_rbry_plyr-avg_fuml_team-avg_qbints_team-avg_qbtdp_team-grass_1 )
summary(linRegQBpc2)

PcPredicted <- predict(linRegQBpc2, newdata = testTransformedpc)

SSEpc <- sum((PcPredicted - testTransformedpc$pc)^2)
SSTpc <- sum((mean(nfl_data$pc)-testTransformedpc$pc)^2)
r2_pc <- 1 - SSEpc/SSTpc 
r2_pc
rmse_pc <- sqrt(SSEpc/nrow(testTransformedpc))
rmse_pc

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegQBpc2, which = c(1:3,5))

confint(linRegQBpc2)

coef(summary(linRegQBpc2))

anova(linRegQBpc2)

#aic 
aic_pc <- step(lm(pcregform, data = trainTransformedpc), direction = "backward")

##########################################################
set.seed(123)
splitints <- sample.split(nfl_data$ints, SplitRatio = 0.7)
Trainints <- subset(nfl_data, split == TRUE)
Testints <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainints, method = c("center", "scale"))
trainTransformedints <- predict(preProcValues, Trainints)
testTransformedints <- predict(preProcValues, Testints)


#ggpairs for ints
########
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("ints",colnames(filtered_nfl_data_fields[46:54]))])


intsregform <- formula(paste("ints ~ ", 
                             paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegQBInts <- lm(intsregform, data = trainTransformedints)
summary(linRegQBInts) 

linRegQBInts2 <- lm(ints ~ avg_qbints_plyr, data = trainTransformedints)

summary(linRegQBInts2)

PintPredicted <- predict(linRegQBInts2, newdata = testTransformedints)

SSEint <- sum((PintPredicted - testTransformedints$ints)^2)
SSTint <- sum((mean(nfl_data$ints)-testTransformedints$ints)^2)
r2_int <- 1 - SSEint/SSTint 
r2_int
rmse_int <- sqrt(SSEint/nrow(testTransformedints))
rmse_int

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegQBInts2, which = c(1:3,5))

confint(linRegQBInts2)

coef(summary(linRegQBInts2))

anova(linRegQBInts2)

#aic 
aic_ints <- step(lm(intsregform, data = trainTransformedints), direction = "backward")


####################################################################
set.seed(123)
splitpa <- sample.split(nfl_data$pa, SplitRatio = 0.7)
Trainpa <- subset(nfl_data, split == TRUE)
Testpa <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainpa, method = c("center", "scale"))
trainTransformedpa <- predict(preProcValues, Trainpa)
testTransformedpa <- predict(preProcValues, Testpa)


#ggpairs for pa
########
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("pa",colnames(filtered_nfl_data_fields[46:54]))])


paregform <- formula(paste("pa ~ ", 
                           paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegQBpa <- lm(paregform, data = trainTransformedpa)
summary(linRegQBpa)

linRegQBpa2 <- update(linRegQBpa, ~.-hot_weather-home_team_1-ATL-BAL-CLE-DAL-DEN-DET-KC
                      -NOR-OAK-SEA-STL-WAS-avg_trg_team-avg_tdr_team-avg_rbra_team
                      -avg_rbry_plyr-avg_fuml_team-avg_qbints_team-avg_qbtdp_team
                      -grass_1-bad_weather_1 )
summary(linRegQBpa2)

linRegQBpa3 <- update(linRegQBpa2, ~.-vertical1-IND-PHI-PIT )
summary(linRegQBpa3)


PaPredicted <- predict(linRegQBpa3, newdata = testTransformedpa)

SSEpa <- sum((PaPredicted - testTransformedpa$pa)^2)
SSTpa <- sum((mean(nfl_data$pa)-testTransformedpa$pa)^2)
r2_pa <- 1 - SSEpa/SSTpa 
r2_pa
rmse_pa <- sqrt(SSEpa/nrow(testTransformedpa))
rmse_pa

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegQBpa3, which = c(1:3,5))

confint(linRegQBpa3)

coef(summary(linRegQBpa3))

anova(linRegQBpa3)

#aic 
aic_pa <- step(lm(paregform, data = trainTransformedpa), direction = "backward")

#################################################################
set.seed(123)
splitry <- sample.split(nfl_data$ry, SplitRatio = 0.7)
Trainry <- subset(nfl_data, split == TRUE)
Testry <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainry, method = c("center", "scale"))
trainTransformedry <- predict(preProcValues, Trainry)
testTransformedry <- predict(preProcValues, Testry)


#ggpairs for ry
########
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("ry",colnames(filtered_nfl_data_fields[46:54]))])


ryregform <- formula(paste("ry ~ ", 
                           paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegRushYd <- lm(ryregform, data = trainTransformedry)
summary(linRegRushYd)

linRegRushYd2 <- update(linRegRushYd,~.-height-weight-hot_weather-home_team_1-is_WR-is_TE-forty1
                        -vertical1-ARI-ATL-BAL - BUF - CAR - CHI
                        -CIN - CLE - DAL - DEN - DET - GB - HOU - IND - JAC - KC - MIA 
                        -MINN - NE - NOR - NYG-NYJ - OAK - PHI - PIT -SD - SEA - STL 
                        -TB - TEN - WAS
                        -avg_rectd_plyr-avg_trg_team-avg_tdr_team-avg_rbra_team-avg_rbry_pos
                        -avg_fuml_team-avg_fuml_plyr-avg_qbints_team-avg_qbtdp_team-avg_qbints_plyr
                        -bad_weather_1-grass_1)

summary(linRegRushYd2)

RushydsPredicted <- predict(linRegRushYd2, newdata = testTransformedry)

SSEruyd <- sum((RushydsPredicted - testTransformedry$ry)^2)
SSTruyd <- sum((mean(nfl_data$ry)-testTransformedry$ry)^2)
r2_ruyd <- 1 - SSEruyd/SSTruyd 
r2_ruyd
rmse_ruyd <- sqrt(SSEruyd/nrow(testTransformedry))
rmse_ruyd

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegRushYd2, which = c(1:3,5))

confint(linRegRushYd2)

coef(summary(linRegRushYd2))

anova(linRegRushYd2)


#aic 
aic_ry <- step(lm(ryregform, data = trainTransformedry), direction = "backward")


#############################################################################
set.seed(123)
splitra <- sample.split(nfl_data$ra, SplitRatio = 0.7)
Trainra <- subset(nfl_data, split == TRUE)
Testra <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainra, method = c("center", "scale"))
trainTransformedra <- predict(preProcValues, Trainra)
testTransformedra <- predict(preProcValues, Testra)


#ggpairs for ra
########
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("ra",colnames(filtered_nfl_data_fields[46:54]))])


raregform <- formula(paste("ra ~ ", 
                           paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegRushAtt <- lm(raregform, data = trainTransformedra)
summary(linRegRushAtt)

linRegRushAtt2 <- update(linRegRushYd,~.-height-weight-hot_weather-home_team_1-age-is_TE
                         -vertical1-ARI-ATL-BAL - BUF - CAR - CHI
                         -CIN - CLE - DAL - GB - IND - JAC - KC - MIA 
                         -MINN - NE - NOR -NYJ - OAK - PHI - PIT -SD - SEA 
                         -TB - TEN - WAS
                         -avg_rectd_plyr-avg_trg_team-avg_tdr_team-avg_rbra_team
                         -avg_fuml_team-avg_fuml_plyr-avg_qbints_team-avg_qbtdp_team-avg_qbints_plyr
                         -bad_weather_1-grass_1)

summary(linRegRushAtt2)

linRegRushAtt3 <- update(linRegRushYd,~.-is_WR-forty1-is_TE-DET-HOU-NYG-STL-avg_rbry_pos)
summary(linRegRushAtt3)


RushattPredicted <- predict(linRegRushAtt, newdata = testTransformedra)

SSEruatt <- sum((RushattPredicted - testTransformedra$ra)^2)
SSTruatt <- sum((mean(nfl_data$ra)-testTransformedra$ra)^2)
r2_ruatt <- 1 - SSEruatt/SSTruatt 
r2_ruatt
rmse_ruatt <- sqrt(SSEruatt/nrow(testTransformedra))
rmse_ruatt

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegRushAtt3, which = c(1:3,2))

confint(linRegRushAtt3)

coef(summary(linRegRushAtt3))

anova(linRegRushAtt3)

#aic 
aic_ra <- step(lm(raregform, data = trainTransformedra), direction = "backward")

############################################################
set.seed(123)
splitdr <- sample.split(nfl_data$tdr, SplitRatio = 0.7)
Traintdr <- subset(nfl_data, split == TRUE)
Testtdr <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Traintdr, method = c("center", "scale"))
trainTransformedtdr <- predict(preProcValues, Traintdr)
testTransformedtdr <- predict(preProcValues, Testtdr)


#ggpairs for tdr
########
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("tdr",colnames(filtered_nfl_data_fields[46:54]))])


tdrregform <- formula(paste("ra ~ ", 
                            paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegtdr <- lm(tdrregform, data = trainTransformedtdr)
summary(linRegRushAtt)

linRegtdr2 <- update(linRegRushYd,~.-height-weight-hot_weather
                     -home_team_1-age-is_TE-vertical1-ARI-ATL-BAL - BUF - CAR - CHI
                     -CIN - CLE - DAL - GB - JAC - KC - MIA 
                     -MINN - NE - NOR -NYJ - OAK - PHI - PIT -SD - SEA 
                     -TB - TEN - WAS
                     -avg_rectd_plyr-avg_trg_team-avg_tdr_team-avg_rbra_team
                     -avg_fuml_team-avg_fuml_plyr-avg_qbints_team-avg_qbtdp_team-avg_qbints_plyr
                     -bad_weather_1-grass_1)

summary(linRegtdr2)

linRegtdr3 <- update(linRegtdr2,~.-is_WR-is_TE-forty1-DET-HOU-NYG-STL-avg_rbry_pos)
summary(linRegtdr3)


RushTdrPredicted <- predict(linRegtdr3, newdata = testTransformedtdr)

SSErutdr <- sum((RushTdrPredicted - testTransformedtdr$tdr)^2)
SSTrutdr <- sum((mean(nfl_data$tdr)-testTransformedtdr$tdr)^2)
r2_rutdr <- 1 - SSErutdr/SSTrutdr 
r2_rutdr
rmse_rutdr <- sqrt(SSErutdr/nrow(testTransformedtdr))
rmse_rutdr

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegtdr3, which = c(1:3,2))

confint(linRegtdr3)

coef(summary(linRegtdr3))

anova(linRegtdr3)

#aic 
aic_ra <- step(lm(tdrregform, data = trainTransformedtdr), direction = "backward")

############################################################


set.seed(123)
splitfuml <- sample.split(nfl_data$fuml, SplitRatio = 0.7)
Trainfuml <- subset(nfl_data, split == TRUE)
Testfuml <- subset(nfl_data, split == FALSE)
preProcValues <- preProcess(Trainfuml, method = c("center", "scale"))
trainTransformedfuml <- predict(preProcValues, Trainfuml)
testTransformedfuml <- predict(preProcValues, Testfuml)


#ggpairs for fuml
########
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[1:9]))])
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[10:18]))])
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[19:27]))])
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[28:36]))])
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[37:45]))])
ggpairs(nfl_data[,c("fuml",colnames(filtered_nfl_data_fields[46:54]))])


fumlregform <- formula(paste("fuml ~ ", 
                             paste(colnames(filtered_nfl_data_fields), collapse="+")))

linRegFumble <- lm(fumlregform, data = trainTransformedfuml)
summary(linRegFumble)

linRegFumble2 <- lm(fuml ~ avg_fuml_plyr, data = trainTransformedfuml)

summary(linRegFumble2)

FumblePredicted <- predict(linRegFumble2, newdata = testTransformedfuml)

SSEfum <- sum((FumblePredicted - testTransformedfuml$fuml)^2)
SSTfum <- sum((mean(nfl_data$fuml)-testTransformedfuml$fuml)^2)
r2_fum <- 1 - SSEfum/SSTfum 
r2_fum
rmse_fum <- sqrt(SSEfum/nrow(testTransformedfuml))
rmse_fum

par(mar = c(4, 4, 2, 2), mfrow = c(2, 2))
plot(linRegFumble2, which = c(1:3,5))

confint(linRegFumble2)

coef(summary(linRegFumble2))

anova(linRegFumble2)

#aic 
aic_fuml <- step(lm(fumlregform, data = trainTransformedfuml), direction = "backward")

##########################################################################

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point()+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 75), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "TE"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 50 & pos1 == "TE"), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "RB"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 30 & pos1 == "RB"), aes(label = pname), size = 2.5)

ggplot(data = nfl_data, aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, pos1 == "WR"))+
  geom_text(data = subset(nfl_data, avg_recy_plyr > 70 & pos1 == "WR"), aes(label = pname), size = 2.5)


ggplot(data = subset(nfl_data, pos1 =="WR"), aes(x = age, y = avg_recy_age/age_count, col = Teams))+
  geom_bar(data = subset(nfl_data, pos1 == "WR"), stat = "identity")

nfl_data$pos_we_care <- c(nfl_data$pos1["WR"], nfl_data$pos1["TE"], nfl_data$pos1["WR"])


glimpse(nfl_data)

nfl_data <- nfl_data %>%
  group_by(pos1)%>%
  mutate(rank_pos = rank(avg_recy_plyr, ties.method = "max"))


ggplot(subset(nfl_data, rank_pos >10 & pos1 %in% c("RB", "WR", "TE")), aes(x = avg_recy_plyr, y = avg_trg_plyr, col = Teams ))+
  geom_point(data = subset(nfl_data, rank_pos >10 & pos1 %in% c("RB", "WR", "TE")))+
  geom_text(data = subset(nfl_data, rank_pos >10 &pos1 %in% c("RB", "WR", "TE")), aes(label = pname), size = 2.5)+
  facet_wrap( ~ pos1, ncol = 3)



write.csv(nfl_data, "C:/Users/Rich Lindberg/Documents/R/NFLCapstoneTest/testfile.csv")


