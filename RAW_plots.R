#########################################
### Creation of plots for EBOV
#########################################

# load ggplot package
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE) # install ggplot2 if requiered
  library(ggplot2) # load ggplot package
}
# load reshape2 package
if (!require("reshape2")) {
  install.packages("reshape2", dependencies = TRUE) # install reshape2 if requiered
  library(reshape2)
}


### TO-DO
## error-bars
# geom_errorbar()
# geom_linerange()
# geom_pointrange()
# geom_crossbar()
# geom_errorbarh()

## working directory
setwd("/home/clara/tmp")
setwd("/net/elara/scratch/clara/Doktorarbeit/EBOV/Ebola_2017")


### Function
se <- function(x) sd(x)/sqrt(length(x))
statistic <- function(x) c(mn = mean(x), s = se(x))

### read tsv containing data
ebola<-read.table(file = 'Count_EBOLA_2017_RAW.tsv', sep = '\t', header = TRUE)
all<-read.table(file = 'Count_EBOLA_2017_RAW.sum.tsv', sep = '\t', header = TRUE)
sMG<-read.table(file = 'Count_EBOLA_2017_RAW.smallMG.tsv', sep = '\t', header = TRUE)
tMG<-read.table(file = 'Count_EBOLA_2017_RAW.totalMG.tsv', sep = '\t', header = TRUE)
sNHE<-read.table(file = 'Count_EBOLA_2017_RAW.smallNHE.tsv', sep = '\t', header = TRUE)
tNHE<-read.table(file = 'Count_EBOLA_2017_RAW.totalNHE.tsv', sep = '\t', header = TRUE)
sDELTA<-read.table(file = 'Count_EBOLA_2017_RAW.smallDELTA.tsv', sep = '\t', header = TRUE)
tDELTA<-read.table(file = 'Count_EBOLA_2017_RAW.totalDELTA.tsv', sep = '\t', header = TRUE)


### processing table
# summarize: typ  MAPPED  EBOLA LEADER  NP  TRAILER
#           small+VP30  x y z y x

# sDELTAm401842 -> sDELTAm

# set mapping_to  number
# sMGs23423 Leader  50
names(sMG)<-tolower(names(sMG))
msMG<-melt(sMG, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))
names(tMG)<-tolower(names(tMG))
mtMG<-melt(tMG, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))
names(sNHE)<-tolower(names(sNHE))
msNHE<-melt(sNHE, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))
names(tNHE)<-tolower(names(tNHE))
mtNHE<-melt(tNHE, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))
names(sDELTA)<-tolower(names(sDELTA))
msDELTA<-melt(sDELTA, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))
names(tDELTA)<-tolower(names(tDELTA))
mtDELTA<-melt(tDELTA, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))


## summarize data
names(all)<-tolower(names(all))
mall<-melt(all, id=c("abbrev"), measure.vars = c("leader", "np", "trailer"))

# compute mean & standard deviation
means <- aggregate(value ~ abbrev + variable, data = mall, FUN = statistic)

# plotting
svg("Count_EBOLA_2017_ERRORS.svg")
ggplot(means, aes(abbrev, means$value[,1], fill = variable)) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_errorbar(aes(ymin = means$value[,1] - means$value[,2], ymax = means$value[,1] + means$value[,2]), width = .2, position = position_dodge(.9))+
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Means of Ebola readcount")
dev.off()


## creation of plots -> Leader + NP + Trailer for every set
svg("Count_EBOLA_2017_RAW.smallMG.svg")
ggplot(msMG, aes(abbrev, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("small RNA MG")
dev.off()

svg("Count_EBOLA_2017_RAW.totalMG.svg")
ggplot(mtMG, aes(abbrev, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("total RNA MG")
dev.off()

svg("Count_EBOLA_2017_RAW.smallNHE.svg")
ggplot(msNHE, aes(abbrev, value, fill = variable)) +
  geom_bar(stat="identity", position = "dodge") + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("small RNA NHE")
dev.off()

svg("Count_EBOLA_2017_RAW.totalNHE.svg")
ggplot(mtNHE, aes(abbrev, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("total RNA NHE")
dev.off()

svg("Count_EBOLA_2017_RAW.smallDELTA.svg")
ggplot(msDELTA, aes(abbrev, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("small RNA DELTA5")
dev.off()

svg("Count_EBOLA_2017_RAW.totalDELTA.svg")
ggplot(mtDELTA, aes(abbrev, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +  # hide axis lables #rotate x axis legend
  scale_fill_brewer(palette = "Set1") +
  ggtitle("total RNA DELTA5")
dev.off()



## small RNA + MG
svg("Count_EBOLA_2017_RAW.smallMG.mappedEBOLA.svg")
barplot(sMG$EBOLA, main = "small RNA + MG", 
        col = "lavenderblush4",
        names.arg=sMG$ABBREV,
        las=2
        )
dev.off()

## total RNA + MG
svg("Count_EBOLA_2017_RAW.totalMG.mappedEBOLA.svg")
barplot(tMG$EBOLA, main =" total RNA + MG", 
        col = "lavenderblush4",
        names.arg=tMG$ABBREV,
        las=2
)
dev.off()

## small RNA + Nhe
svg("Count_EBOLA_2017_RAW.smallNHE.mappedEBOLA.svg")
barplot(sNHE$EBOLA, main = "small RNA + NHE", 
        col = "lavenderblush4",
        names.arg = sNHE$ABBREV,
        las=2
)
dev.off()


## total RNA + Nhe
svg("Count_EBOLA_2017_RAW.totalNHE.mappedEBOLA.svg")
barplot(tNHE$EBOLA, main = "total RNA + NHE", 
        col = "lavenderblush4",
        names.arg = tNHE$ABBREV,
        las = 2
)
dev.off()


## small RNA + Delta5
svg("Count_EBOLA_2017_RAW.smallDELTA5.mappedEBOLA.svg")
barplot(sDELTA$EBOLA, main = "small RNA + DELTA5", 
        col = "lavenderblush4",
        names.arg = sDELTA$ABBREV,
        las = 2
)
dev.off()


## total RNA + Delta5
svg("Count_EBOLA_2017_RAW.totalDELTA5.mappedEBOLA.svg")
barplot(tDELTA$EBOLA, main = "total RNA + DELTA5", 
        col = "lavenderblush4",
        names.arg = tDELTA$ABBREV,
        las = 2
)
dev.off()