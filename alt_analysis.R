
library("xlsx")
library("reshape2")
library("magrittr")
library("dplyr")
library("ggplot2")
library("robCompositions")
library("ggbiplot")

load("valresultat.Rdata")

election.res <- res$kommun$val2014R
election.res <- election.res[order(election.res$ID),]

kommun <- read.xlsx("kommundata.xlsx",1)
kommun <- kommun[order(kommun$code),]

asyl <- read.xlsx("asylsdochm.xlsx",1)

kommun.code.to.name <- select(kommun, c(code, name))

election.res <- merge(x=election.res, y=kommun.code.to.name, by.x="ID", by.y="code")
rownames(election.res) <- election.res$name

election.res.votes.only <- select(
  election.res, 
  c(name, PROCENT_M, PROCENT_C, PROCENT_FP, PROCENT_KD, PROCENT_S, PROCENT_V, PROCENT_MP, PROCENT_SD, PROCENT_FI))

# --------------------------------------
# Robust pca of election data
# --------------------------------------
election.res.votes.only.without.name <- select(election.res.votes.only, -name)

rob.pca <- 
  pcaCoDa(
    election.res.votes.only.without.name, 
    mult_comp = list(1:length(election.res.votes.only.without.name)))

rob.pca.scores <- data.frame(rob.pca$scores)
rob.pca.scores.with.additional.info <- cbind(rob.pca.scores, election.res)
rob.pca.scores.with.additional.info <- merge(rob.pca.scores.with.additional.info, kommun, by.x = "ID", by.y = "code")


party.base.plot <- 
  ggplot(
    rob.pca.scores.with.additional.info,
    aes(x = rob.pca.scores$Comp.1, y = rob.pca.scores$Comp.2))

party.base.plot +
  geom_point(aes(colour=governing, shape = municipalityTypeBroad), size = 3)

party.base.plot +
  geom_point(aes(colour=largest_party, shape = municipalityTypeBroad), size = 3)


# --------------------------------------
# PCA of municipality data
# --------------------------------------
# Exclude columns which are not of interest for pca
kommun.cols.of.interest <- 
  select(
    kommun, 
    -c(code,
       name,
       satisfactionInfluence, 
       satisfactionGeneral,
       fokusRanking,
       longitude,
       latitude,
       satisfactionElderlyCare,
       municipalityType,
       municipalityTypeBroad,
       governing))
rownames(kommun.cols.of.interest) <- kommun$name


kommun.pca <- 
  prcomp(kommun.cols.of.interest,
       scale.=TRUE,
       center=TRUE,
       cor=TRUE) 

summary(kommun.pca)
text.size <- 5
p <- ggbiplot(kommun.pca, labels.size = text.size, varname.size = text.size) 
ggbiplot.data.view <- merge(p$data, kommun, by.x="row.names", by.y="name", all.x=TRUE)
ggbiplot.data.view <- merge(ggbiplot.data.view, election.res, by.x="Row.names", by.y="name")


p <- p + 
  scale_colour_gradient(low="blue",high="red") +
  theme_grey(base_size = 18) +
  geom_text(data=ggbiplot.data.view, aes(label=ifelse(Row.names == "Uppsala", "Uppsala", "")))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_SD))
  
p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_MP))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_S))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_V))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_M))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_C))

p + 
  geom_point(data=ggbiplot.data.view, aes(x=xvar, y=yvar, colour=PROCENT_KD))



# --------------------------------------
# Look at correlations between variables
# --------------------------------------
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Plot correlation matrix
plot.correlation.matrix <- function(corr.mat) {
  corr.mat.m <- melt(corr.mat)
  ggplot(corr.mat.m, aes(x=Var1, y=Var2)) +
    geom_tile(aes(fill=value)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1))
}

kommun.corr.mat <- cor(scale(kommun.cols.of.interest))

kommun.corr.mat <- reorder_cormat(kommun.corr.mat)
kommun.corr.mat.upper <- get_upper_tri(kommun.corr.mat)

# Checking for correlations in the data
plot.correlation.matrix(kommun.corr.mat.upper)

correlation.election.res <- 
  cor(
    kommun.cols.of.interest,
    select(
      election.res.votes.only, -name))

hc <- hclust(dist(correlation.election.res))
hc.party <- hclust(dist(t(correlation.election.res)))
ordered.correlation.election.res <- correlation.election.res[hc$order, hc.party$order]

plot.correlation.matrix(ordered.correlation.election.res) +
  xlab("Municipality KPI") +
  ylab("Voting percantage")

between.party.correlations <- cor(select(election.res, -name))
plot.correlation.matrix(between.party.correlations) 



asyl.org <- read.xlsx("asylsdochm.xlsx",1)
asyl.org <- asyl.org[3:nrow(asyl.org),]
asyl.org$Röster.på.SD <- as.numeric(asyl.org$Röster.på.SD)
asyl.org$Antal.asylsökande.per <- as.numeric(asyl.org$Antal.asylsökande.per)
str(asyl.org)
cor(
  asyl.org$Röster.på.SD, 
  asyl.org$Antal.asylsökande.per,
  method="pearson")

ggplot(asyl.org, aes(x = Antal.asylsökande.per, y = Röster.på.SD)) +
  geom_point()


asyl.csv <- read.csv("cleaned_up_asyl.csv")
cor(
  asyl.csv$asylum.seekers.per.1000.inh, 
  asyl.csv$votes.for.sd.parlament.2014,
  method="pearson")

ggplot(asyl.csv, aes(x = asylum.seekers.per.1000.inh, y = votes.for.sd.parlament.2014)) +
  geom_point() +
  geom_smooth(method=lm)


hist(asyl.csv$votes.for.sd.parlament.2014)



