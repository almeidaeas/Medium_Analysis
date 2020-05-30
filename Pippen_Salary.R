


options(scipen=999)

library(dplyr)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(NbClust)
library(vcd)


# CHECK A SAMPLE OF THE DATA LOADED
dplyr::sample_n(nba_1996_97, 10)

group_by(new_nba_1996_97, CLUSTER, POSITION) %>%
  summarise(
    count = n(),
    mean = mean(SALARY, na.rm = TRUE),
    sd = sd(SALARY, na.rm = TRUE)
  )

set.seed(123)


# HISTOGRAM SALARY FOR SF

# Creating a new dataframe with the data for SF
SF_1996_97 <- nba_1996_97 %>% filter(POSITION == "Small Forward")

h <-hist(SF_1996_97$SALARY,
         main="Salary distribution for SF during the 1996-97 season",
         prob=TRUE,
         xlab="Salary (US$)",
         col="#006699",
         breaks=7,
        )

box(bty="l")
lines(density(SF_1996_97$SALARY,na.rm=T),col="000033",lwd=1)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="FFFFFF")


# ANOVA ANALYSIS USING THE PERCENTILES
VORP.aov <- aov(SALARY ~ PERC_VORP, data = nba_1996_97)
PER.aov <- aov(SALARY ~ PERC_PER, data = nba_1996_97)
USAG.aov <- aov(SALARY ~ PERC_ADJUSTED_USAGE, data = nba_1996_97)
AGE.aov <- aov(SALARY ~ PERC_AGE, data = nba_1996_97)

summary(VORP.aov)
summary(PER.aov)
summary(USAG.aov)
summary(AGE.aov)



# INTERACTION PLOT
par(mfrow=c(2,1))
interaction.plot(nba_1996_97$PERC_VORP,
                 nba_1996_97$PERC_AGE,
                 nba_1996_97$SALARY, type="b", col=c("#003366","#000000","#CC0000","#336666"),
                 leg.bty="p",  leg.bg="beige", lwd=2, pch=c(1,4,8,18,20),
                 xlab="Percentile VORP",
                 ylab="Salary (US$)",
                 main="Interaction Plot: Value over Replacement Player")


interaction.plot(nba_1996_97$PERC_PER,
                 nba_1996_97$PERC_AGE,
                 nba_1996_97$SALARY, type="b", col=c("#003366","#000000","#CC0000","#336666"),
                 leg.bty="p",  leg.bg="beige", lwd=2, pch=c(1,4,8,18,20),
                 xlab="Percentile PER",
                 ylab="Salary (US$)",
                 main="Interaction Plot: Player Efficiency Rating")



# CLUSTER
CLST_1996_97 <- nba_1996_97[, c("PLAYER", "PERC_VORP", "PERC_PER", "PERC_AGE", "PERC_ADJUSTED_USAGE")]
  #CREATE A NEW DF, REMOVE THE DUPLICATE AND SET THE ROWNAMES AS THE PLAYERS' NAMES


library(cluster)
library(factoextra)
library(NbClust)
# Elbow method
fviz_nbclust(CLST_1996_97, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) 
            + labs(subtitle = "Elbow method")

# Kmeans
set.seed(123)
km.res <- kmeans(CLST_1996_97, 4, nstart = 1, iter.max = 1000)

km.res$centers
km.res$size

# Visualize
fviz_cluster(km.res, data = CLST_1996_97, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())




# BOXPLOT
ggboxplot(new_nba_1996_97, x = "CLUSTER", y = "SALARY", 
          color = "POSITION", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c(1,2,3,4), width = 0.7,
          ylab = "Salary", xlab = "Cluster")



# ANALYSIS OF VARIANCE PER POSITION
res.aov <- aov(SALARY ~ POSITION, data = nba_1996_97)
summary(res.aov)
TukeyHSD(res.aov)

res.lm <- lm(SALARY ~ POSITION, data = nba_1996_97)
summary(res.lm)

plot(res.aov, 1)
plot(res.aov, 2)


CLUSTER.aov <- aov(SALARY ~ CLUSTER, data = new_nba_1996_97)
summary(CLUSTER.aov)



# CHI SQUARED 
nomes <- tbl_qui$POSITION
  tbl_qui$POSITION <- NULL
  rownames(tbl_qui) <- nomes

chi <- chisq.test(tbl_qui)
chi

chi$observed
chi$expected

