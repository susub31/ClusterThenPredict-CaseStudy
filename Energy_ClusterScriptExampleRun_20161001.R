library(ggplot2)
library(dplyr)
library(data.table)
library(ggmap)
library(caret)
library(corrplot)
library(flexclust)
library(ggdendro)
library(GGally)
library(cluster)

#setwd("~/Sudha/Analytics/Datasets")
energy = read.csv("energy.csv")

#EnergyData = energy[, c("YEAR", "GenTotal", "GenHydro", "GenTotalRenewable", "GenSolar", "EPriceResidential", "EPriceCommercial", "EPriceIndustrial", "CumlFinancial", "CumlRegulatory", "Total.salary", "Import", "presidential.results", "GenSolarBinary")]
EnergyData = energy[, c("GenTotal", "GenHydro", "AllSourcesCO2", "EPriceTotal", "EsalesTotal", "CumlRegulatory", "Total.salary", "Import", "presidential.results", "GenSolarBinary")]
EnergyData = filter(EnergyData, !is.na(AllSourcesCO2))

corenergy = cor(EnergyData[1:10])
diag(corenergy) <- 0
corrplot(corenergy)

corrplot(corenergy, method="circle", col=colorRampPalette(c("red","white","black"))(200), tl.col="black")

statenames= read.csv("StateNames.csv")
statenames$StateName=tolower(statenames$StateName)

#statesMap=map_data("state")
#USMap = merge(statesMap, statenames, by.x="region", by.y="StateName")

energy2 = filter(energy, !(STATE=="AK" | STATE=="HI"))
statenames=filter(statenames, !(State=="AK" | State=="HI"))

energy2 = energy2 %>%
  group_by(STATE) %>%
  summarise(AvgGenTotal=mean(GenTotal), AvgPriceTotal=mean(EPriceTotal)) %>%
  arrange(STATE)

us.dat <- map_data("state")
USMap=merge(us.dat, statenames, by.x="region", by.y="StateName")

USMap = USMap %>%
  arrange(region, order)

#energy2 = merge(energy2, statenames, by.x="STATE", by.y="State")
EnergyMap = merge(USMap, energy2, by.x="State", by.y="STATE")

#write.csv(EnergyMap, "EnergyMap.csv")

ggplot(EnergyMap, aes(x=long, y=lat, group=group, order=order)) +
  geom_polygon(aes(fill=AvgGenTotal), show.legend = TRUE, colour=alpha("white", 1/2), size=0.2) +
  theme_bw() +
  theme(legend.position="right", line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")

ggplot(EnergyMap, aes(x=long, y=lat, group=group, order=order)) +
  geom_polygon(aes(fill=AvgPriceTotal), show.legend = TRUE, colour=alpha("white", 1/2), size=0.2) +
  labs(x="Average Price", y="") +
  theme_grey() +
  theme(legend.position="right", line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")

ggplot(EnergyMap, aes(x=long, y=lat, group=group, order=order)) +
  geom_polygon(aes(fill=AvgGenTotal), show.legend = TRUE, colour=alpha("white", 1/2), size=0.2) +
  theme_bw() +
  theme(legend.position="right", line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")

ggplot(EnergyMap) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=AvgGenTotal), show.legend = TRUE, colour=alpha("white", 1/2), size=0.2) +
  theme_bw() +
  theme(legend.position="right", line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")


ggplot(EnergyMap, aes(x=long, y=lat, group=group, order=order)) +
  geom_polygon(aes(fill=AvgGenTotal), show.legend = TRUE, colour=alpha("white", 1/2), size=0.2) +
  theme_bw() +
  theme(legend.position="right", text=element_blank(), line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")

ggplot(EnergyMap, aes(x=long, y=lat, group=group, order=order)) +
  geom_polygon(aes(fill=AvgGenTotal), colour=alpha("white", 1/2), size=0.2) +
  theme_bw() +
  theme(legend.position="none", text=element_blank(), line=element_blank()) +
  scale_fill_continuous(low="beige", high="hotpink")

boxplot(EPriceTotal ~ STATE, data=energy )
energy$Party=as.factor(ifelse(energy$presidential.results==1, "Republican", "Democrat"))

p = ggplot(energy, aes(STATE, EPriceTotal))
p + geom_boxplot(outlier.color = "red", outlier.shape = 1, fill="white", colour="#3366FF") +
  theme_bw() +
  theme(line=element_blank())

p + geom_boxplot(outlier.color = "red", outlier.shape = 1, aes(colour=energy$STATE)) +
  theme_bw() +
  theme(legend.position="none", line=element_blank())

p + geom_boxplot(data=energy, outlier.color = "red", outlier.shape = 1, aes(colour=Party)) +
  theme_bw() +
  theme(legend.position="none", line=element_blank())

p + geom_boxplot(data=energy, aes(fill=Party)) +
  theme_grey() +
  theme(legend.position="none", line=element_blank())

SelectStates=filter(energy, (STATE %in% c("AK", "HI", "AL", "CT", "DE", "AZ", "KY", "MA", "MD", "MN", "MS", "ND", "NJ", "SC", "TX", "VT", "WY")))
p + geom_boxplot(data=SelectStates, aes(fill=Party)) +
  theme_bw() +
  theme(legend.position="none", line=element_blank())

avgbyState=as.data.frame(State=energy$STATE, AvgPrice=sort(tapply(energy$EPriceTotal, energy$STATE, mean)))
avgbyState = energy %>%
  group_by(STATE) %>%
  summarise(AvgPrice=mean(EPriceTotal))

ggplot(data=avgbyState) + 
  geom_histogram(aes(x=AvgPrice), color="black", fill="grey", alpha=1.0) +
  labs(x="Average Price") +
  scale_x_continuous(breaks=1:23) +
  scale_y_continuous(name="Avg Total Price by State") +
  theme_bw() +
  theme(line=element_blank())


#K-Means
set.seed(144)
spl=sample(1:nrow(energy), size=0.7*nrow(energy))
train=energy[spl,]
test=energy[-spl,]

energyVec = as.vector(energy)
distance = dist(energyVec, method="euclidean")

clusterIntensity = hclust(distance, method="ward.D")
#plot(clusterIntensity)
ggdendrogram(clusterIntensity, rotate=FALSE, size=2)
dhc = as.dendrogram(clusterIntensity)

ddata = dendro_data(dhc, type="rectangle")
p = ggplot(segment(ddata)) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
  coord_flip() + 
  scale_y_reverse(expand=c(0.2,0))

p +
  coord_flip() +
  theme_dendro()



mod = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=train, family="binomial")
summary(mod)
predmod = predict(mod, newdata=test, type="response")

table(test$GenSolarBinary, predmod>=0.5)
tfull=table(test$GenSolarBinary, predmod>=0.5)

energyClusters=cutree(clusterIntensity, k=3)
tapply(energyVec, energyClusters, mean)

tapply(energy$EPriceTotal, energyClusters, mean)
tapply(energy$CumlFinancial, energyClusters, mean)
tapply(energy$CumlRegulatory, energyClusters, mean)
tapply(energy$GenSolarBinary, energyClusters, mean)

cluster1=subset(energy, energyClusters==1)
cluster2=subset(energy, energyClusters==2)
cluster3=subset(energy, energyClusters==3)

table(cluster1$GenSolarBinary)
table(cluster2$GenSolarBinary)
table(cluster3$GenSolarBinary)

tapply(cluster1$CumlFinancial, cluster1$GenSolarBinary, mean)
tapply(cluster2$CumlFinancial, cluster2$GenSolarBinary, mean)
tapply(cluster3$CumlFinancial, cluster3$GenSolarBinary, mean)

tapply(cluster1$EPriceTotal, cluster1$GenSolarBinary, mean)
tapply(cluster2$EPriceTotal, cluster2$GenSolarBinary, mean)
tapply(cluster3$EPriceTotal, cluster3$GenSolarBinary, mean)

#k-means clustering
train.limited = train[, c(23,24,25,26,27)]
test.limited=test[, c(23,24,25,26,27)]
#train.limited=train
#test.limited=test

preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)

set.seed(144)
km = kmeans(train.norm, centers=3, iter.max = 1000)
km$size
clusplot(train.norm, km$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

km2=kmeans(test.norm, centers=3, iter.max=1000)
clusplot(test.norm, km2$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

km3=kmeans(test.norm, centers=1, iter.max=1000)
clusplot(test.norm, km3$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

km.kcca = as.kcca(km, train.norm)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=test.norm)
table(clusterTrain)
table(clusterTest)
train1 = subset(train, clusterTrain==1)
train2 = subset(train, clusterTrain==2)
train3 = subset(train, clusterTrain==3)

test1 = subset(test, clusterTest==1)
test2 = subset(test, clusterTest==2)
test3 = subset(test, clusterTest==3)

EnergyVec2=c(tapply(train$EPriceTotal, km$cluster, mean),
             tapply(train$CumlFinancial, km$cluster, mean),
             tapply(train$CumlRegulatory, km$cluster, mean),
             tapply(train$Total.salary, km$cluster, mean),
             tapply(train$presidential.results, km$cluster, mean),
             tapply(train$Import, km$cluster, mean))
dim(EnergyVec2) = c(3, 6)
colnames(EnergyVec2) = c("EPriceTotal", "Financial","Regulatory","Salary","Results","Import")
EnergyVec2

#GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import
#--model #1 on cluster 1
mod1 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=train1, family="binomial")
summary(mod1)
predtest1 = predict(mod1, newdata=test1, type="response")
table(test1$GenSolarBinary, predtest1 >= 0.5)

mod2 = glm(GenSolarBinary ~ EPriceTotal + CumlFinancial + CumlRegulatory, data=train2, family="binomial")
#summary(mod2)
predtest2 = predict(mod2, newdata=test2, type="response")
table(test2$GenSolarBinary, predtest2 >= 0.5)


#--model #2 on cluster 2
#mod2 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=train2, family="binomial")
#summary(mod2)
#predtest2 = predict(mod2, newdata=test2, type="response")
#table(test2$GenSolarBinary, predtest2 >= 0.5)

#mod2 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=train2, family="binomial")
#summary(mod2)
#predtest2 = predict(mod2, newdata=test2, type="response")
#table(test2$GenSolarBinary, predtest2 >= 0.5)

#--model #2 on cluster 3
mod3 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=train3, family="binomial")
summary(mod3)
predtest3 = predict(mod3, newdata=test3, type="response")
table(test3$GenSolarBinary, predtest3 >= 0.5)

#--Combine all predictions / outcomes to get overall accuracy
AllPredictions = c(predtest1, predtest2, predtest3)
AllOutcomes = c(test1$GenSolarBinary, test2$GenSolarBinary, test3$GenSolarBinary)
table(AllOutcomes, AllPredictions >= 0.5)


table(test$GenSolarBinary, predmod>=0.5)


#table(test1$GenSolarBinary)
#table(test2$GenSolarBinary)
#table(test3$GenSolarBinary)

# R code to plot the confusion matrix
tclust = table(AllOutcomes, AllPredictions >= 0.5)

# Call fourfoldplot function to plot the confusion matrix for entire population
fourfoldplot(tfull, color=c("#CC6666", "#99CC99"), conf.level=0, margin=1, main="Confusion Matrix - Entire Population")


# Call fourfoldplot function to plot the confusion matrix for entire population
fourfoldplot(tfull, color=c("#CC6666", "#99CC99"), conf.level=0, margin=1, main="Confusion Matrix - Entire Population")


# Call fourfoldplot function to plot the confusion matrix for subpopulation (results combined after clustering)
fourfoldplot(tclust, color=c("#CC6666", "#99CC99"), conf.level=0, margin=1, main="Confusion Matrix - Subpopulation")

library(mclust)
fit=Mclust(data.frame(train.norm, km$cluster))
plot(fit)
# display the best model
summary(fit)
citation("mclust")

library(fpc)
plotcluster(test.norm, km2$cluster)
plotcluster(test.norm, km3$cluster)
plotcluster(train.norm, km4$cluster)
plotcluster(train.norm, km$cluster)



summary(fit)
