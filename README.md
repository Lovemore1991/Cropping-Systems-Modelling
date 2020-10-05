# Cropping-Systems-Modelling
Analysis of the performance of different CA technologies 
---
title: 'New SIMLESA: After corrections (Malawi Lowlands) Across countries analysis'
author: "Lovemore Chipindu [lovemore.datascience@gmail.com], [0778796212]"
date: "15 April 2019"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(readr)
new_sim <- read_csv("~/Dr Nyagumbo/Final data set/new_sim.csv")
View(new_sim)
attach(new_sim)
names(new_sim)
```



```{r}
country=as.factor(country)
site=as.factor(site)
agroecoregion=as.factor(agroecoregion)
system=as.factor(system)
systems=factor(system,levels = c("Conv","CA",order=TRUE))
legume_cultivar=as.factor(legume_cultivar)
cropping_systems=factor(cropping_system,levels = c("Conv","CA_sole","CA_intercrop","CA_rotation"),ordered = TRUE)
season_class=as.factor(season_class)
season_clas=factor(season_class,levels = c("(0-2)","(3-5)","(>5)"),ordered = TRUE)
rainfall_class=as.factor(rainfall_class)
rainfall_clas=factor(rainfall_class,levels =c("(<700)","(700-1300)","(>1300)"),ordered = TRUE)
textureclass=as.factor(textureclass)
#drainage_class=c(drainage_class)
drainage_class=as.factor(drainage_class)

```


```{r}
library(ggplot2)
library(maps)
library(ggalt)
library(extrafontdb)
library(MASS)
library(pscl)
library(psych)
library(gridExtra)
library(repr) ### adjusting the length and width of your plot
library(beanplot)
library("devtools")
library("yarrr")
library(agricolae)
library(easynls)
library(MVN)
library(lme4)
library(ggsignif)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(mplot)
```




```{r}
### grow a tree

fit <- rpart(maize_grain~country+agroecoregion+cropping_systems+rainfall_class+textureclass,method="anova",data = new_sim)
printcp(fit)

plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits


```





```{r}
# plot tree 
plot(fit, uniform=TRUE, 
   main="Regression Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


```



```{r}
library(party)
fit2 <- ctree(maize_grain~country+agroecoregion+cropping_systems+rainfall_class+textureclass)
fit2
plot(fit2,cex=1,size=0.01,node.width = 1, edge.label.color = "black", all=TRUE)
####

summary(fit2)

```


```{r}
###### more simplified trees 

multi.class.model <- rpart(maize_grain~country+agroecoregion+cropping_systems+rainfall_class+textureclass, data = new_sim) 
rpart.plot(multi.class.model)

rpart.plot(multi.class.model, type = 4, extra = 0, branch.lty = 6, box.palette = "RdYlGn")
summary(multi.class.model)
```



```{r}
### node by node presentation
tree1 <- rpart(maize_grain~country+agroecoregion+cropping_systems+rainfall_class+textureclass, data = new_sim) 
par(mfrow = c(4,3)) 
for(iframe in 1:nrow(tree1$frame)) {
  cols <- ifelse(1:nrow(tree1$frame) <= iframe, "black", "gray") 
  prp(tree1, col = cols, branch.col = cols, split.col = cols)
  }



```


```{r}
#### another way of doing it

tree <- rpart(maize_grain~country+agroecoregion+cropping_systems+rainfall_class+textureclass, data = new_sim)
my.snip.fun <- function(tree) { # tree is the trimmed tree 
  # should really use indep test data here 
  cat("fraction predicted correctly: ", sum(predict(tree, type = "class") == ptitanic$survived) / length(ptitanic$survived), "\n") 
  }
prp(tree, snip = TRUE, snip.fun = my.snip.fun)


```



```{r}
##stability analysis 

```




Multiple regression analysis across countries 

```{r}
## full model without possible interactions 
## note that drainade classifications are applicable to Mza ans Mwi
## texture data was properly done in Mza and Mlwi
full<-lm(maize_grain~country+agroecoregion+cropping_systems+season_class+rainfall_class+textureclass,data = new_sim) ## if the texture class is included in the model , season class becomes insignificant.
anova(full)
summary(full)

### computing the regression model and do the mean comparison across countries 

## model
fll<-aov(maize_grain~country+agroecoregion+cropping_systems+season_class+rainfall_class,data = new_sim)

out<-LSD.test(fll,"rainfall_class",p.adj="hommel",console=TRUE)
plot(out,variation="SD")

############################# mean comparison of cropping systems 
x<- LSD.test(fll,"cropping_systems",alpha=0.05,group=FALSE)
diffograph(x,cex.axis=0.8,xlab="Maize Grain Yield [kg/ha]",ylab="Maize Grain Yield [kg/ha]")
###
h<-HSD.test(fll,"cropping_systems",group =TRUE)
h
#####
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")

############# 
############################# mean comparison of country 
x<- LSD.test(fll,"country",alpha=0.05,group=FALSE)
diffograph(x,cex.axis=0.8,xlab="Maize Grain Yield [kg/ha]",ylab="Maize Grain Yield [kg/ha]")
###
h<-HSD.test(fll,"country",group =TRUE)
h
#####
bar.group(h$groups,ylim=c(0,5000),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")

##############

############################# mean comparison of rainfall class
x<- LSD.test(fll,"rainfall_class",alpha=0.05,group=FALSE)
diffograph(x,cex.axis=0.8,xlab="Maize Grain Yield [kg/ha]",ylab="Maize Grain Yield [kg/ha]")
###
h<-HSD.test(fll,"rainfall_class",group =TRUE)
h
#####
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")
###Turkey test
#posthoc <- TukeyHSD(x=fll, 'cropping_systems', conf.level=0.95)
#posthoc  ## differences are not intersting



```


modell with possible interaction


```{r model with possible interactions}
##modell with possible interaction
full<-lm(maize_grain~country+agroecoregion+cropping_systems+season_class+rainfall_class+textureclass+cropping_systems*country+
           cropping_systems*agroecoregion+
           cropping_systems*rainfall_class+
           cropping_systems*textureclass+
           cropping_systems*country*agroecoregion+
           cropping_systems*country*rainfall_class+
           cropping_systems*country*textureclass+
           cropping_systems*rainfall_class*season_class+
           cropping_systems*rainfall_class*agroecoregion+
           cropping_systems*agroecoregion*textureclass+
           cropping_systems*rainfall_class*textureclass+
           cropping_systems*agroecoregion*season_class,data = new_sim) ## if the texture class is included in the model , season class becomes insignificant.
anova(full)
#summary(full)

```

Graphical presentation 

```{r}
### cropping systems and grain yield 
# stats output
crop<-aov( maize_grain~cropping_systems,data = new_sim)
summary(crop)
h<-HSD.test(crop,"cropping_systems",group =TRUE)
h
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")

crop<-lm( maize_grain~cropping_systems+agroecoregion,data = new_sim)
summary(crop)
###################

describeBy(maize_grain,cropping_systems)
#######################

stat_box_data <- function(maize_grain, upper_limit = max(maize_grain)*1.5) {
  return( 
    data.frame(
      y = 11500,
      label = paste('N =', length(maize_grain), '\n',
                    'Mean =', round(mean(maize_grain), 0), '\n')
    )
  )
}

## general box plot
theme_set(theme_gray(base_size = 14)) ###This sets the font sizes of anything writt
m<-ggplot(new_sim, aes(x = cropping_systems, y = maize_grain))+ geom_boxplot(size=0.8,varwidth =FALSE,outlier.colour = "red",outlier.shape = 1, shape=6,fill=c("grey","grey","grey","grey")) + geom_smooth(method=lm)+ ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Systems")+ geom_signif(annotations = c("c", "ab"),y_position = c(2500, 3200), xmin=c(1,2), xmax=c(1,2))+ geom_signif(annotations = c("b", "a"),y_position = c(3200, 3600), xmin=c(3,4), xmax=c(3,4))+stat_summary(fun.y=mean, geom="point", shape=10, size=2, color="blue", fill="red")+
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    size=3
  )+ theme_classic(base_size = 13)+ ylim(100, 11500)


#+ 
#geom_crossbar(stat="summary", fun.y=data_summary, fun.ymax=max, fun.ymin=min)
#+stat_compare_means(method = "anova", label.y = 10500)+stat_compare_means(label = "p.signif", method = "t.test",
                     #ref.group = ".all.") 

m

#############
library(cowplot)



####################
theme_set(theme_gray(base_size = 10)) ###This sets the font sizes of anything writt
ggplot(new_sim, aes(x = cropping_systems, y = maize_grain))+ geom_boxplot(size=0.7,varwidth = TRUE,outlier.colour = "red",outlier.shape = 1, shape=6) + geom_smooth(method=lm)+ ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Systems")+stat_summary(fun.y=mean, geom="point", shape=10, size=2, color="blue", fill="red")+stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    size=2
  )+ theme_classic(base_size = 10)+ ylim(100, 11500)+
facet_wrap(.~agroecoregion)
########


##adding theme to the plots 
### violin + boxplot

theme_set(theme_gray(base_size =12))
m <- ggplot(data=new_sim,aes(x=cropping_systems, y=maize_grain))
m + geom_violin(size=0.9,shape=8) + geom_boxplot(width=.3, outlier.size=0,fill=c("grey","grey","grey","grey"))+ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Systems")+ geom_signif(annotations = c("c", "ab"),y_position = c(3000, 3200), xmin=c(1,2), xmax=c(1,2))+ geom_signif(annotations = c("b", "a"),y_position = c(3200,3200), xmin=c(3,4), xmax=c(3,4))+stat_summary(fun.y=mean, geom="point", shape=10, size=2, color="blue", fill="red")+stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    size=3
  )+ theme_classic(base_size = 13)+ ylim(100, 12000)

######
theme_set(theme_gray(base_size =10))
m <- ggplot(data=new_sim,aes(x=cropping_systems, y=maize_grain))
m + geom_violin(size=0.7,shape=8) + geom_boxplot(width=.3, outlier.size=0,fill=c("grey","grey","grey","grey","grey","grey","grey","grey"))+ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Systems")+facet_wrap(.~agroecoregion)+stat_summary(fun.y=mean, geom="point", shape=10, size=2, color="blue", fill="red")+stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    size=3
  )+ theme_classic(base_size = 11)+ ylim(100, 12000)


#####

##cropping system and agroecological regions 
theme_set(theme_gray(base_size = 12)) ###This sets the font sizes of anything writt
m<-ggplot(new_sim, aes(x = cropping_systems, y = maize_grain, fill= agroecoregion))+geom_boxplot(size=0.8,outlier.size = 0)+ ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Systems")+theme(legend.position = c(0.68, 0.75))+stat_summary(fun.y=mean, geom="point", shape=10, size=2, color="blue", fill="red")+stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    size=3
  )+ theme_classic(base_size = 12)+ ylim(100, 12000)+theme(legend.position = c(0.90, 0.75))

m
```


Rainfall Classes
```{r}
stat_box_data <- function(maize_grain, upper_limit = max(maize_grain)*1.5) {
  return( 
    data.frame(
      y = 12000,
      label = paste('N =', length(maize_grain), '\n',
                    'Mean =', round(mean(maize_grain), 0), '\n')
    )
  )
}



#### model
rnfall<-lm(maize_grain~agroecoregion+rainfall_class+cropping_systems)
anova(rnfall)
summary(rnfall)
#######################
rnfall<-aov(maize_grain~agroecoregion+rainfall_class+cropping_systems)
h<-HSD.test(rnfall,"rainfall_class",group =TRUE)
h
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")

out<-LSD.test(rnfall,"rainfall_class",p.adj="hommel",console=TRUE)
out$statistics
plot(out,variation="SD")

```


```{r LSD vallues for rainfall regimes}



```





```{r}
#### model
season<-lm(maize_grain~agroecoregion+season_clas,data = new_sim)
anova(season)
summary(season)
season<-aov(maize_grain~agroecoregion+season_clas,data = new_sim)
anova(season)
#################################
h<-HSD.test(season,"season_clas",group =TRUE)
h
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")

##################################
x<- LSD.test(season,"season_clas",alpha=0.05,group=FALSE)
diffograph(x,cex.axis=0.8,xlab="Maize Grain Yield [kg/ha]",ylab="Maize Grain Yield [kg/ha]")
######################
theme_set(theme_gray(base_size = 12)) ###This sets the font sizes of anything writt
m<-ggplot(new_sim, aes(x = season_clas, y = maize_grain,fill=systems))+ geom_boxplot(size=1,outlier.colour = "red",outlier.shape = 1, shape=6) + geom_smooth(method=lm)+ ylab("Maize Grain yield [kg/ha]") + xlab("Cropping Season [Yrs]")+theme(legend.position = c(0.85, 0.85))+geom_text(data = new_sim, x = 1, y = 3500, label = "a          b")+geom_text(data = new_sim, x = 2, y = 3500, label = "b          a")+geom_text(data = new_sim, x = 3, y = 3500, label = "a          a")+geom_text(data = new_sim, x = 1, y = 7500, label = "c")+geom_text(data = new_sim, x = 2, y = 7500, label = "a")+geom_text(data = new_sim, x = 3, y = 7500, label = "a")
m
####################################################################
###########violin plot season vs cropping systems

theme_set(theme_gray(base_size =10))
m <- ggplot(data=new_sim,aes(x=season_clas, y=maize_grain,fill=systems))
m + geom_violin(size=0.7,shape=8) + geom_boxplot(width=.3, outlier.size=0)+ylab("Maize Grain Yield [kg/ha]") + xlab("Cropping Season [Yrs]")


#########################################################

theme_set(theme_gray(base_size = 11)) ###This sets the font sizes of anything writt
m<-ggplot(new_sim, aes(x = season_clas, y = maize_grain,fill=cropping_systems))+ geom_boxplot(size=1,outlier.colour = "red",outlier.shape = 1, shape=6) + geom_smooth(method=lm)+ ylab("Maize Grain yield [kg/ha]") + xlab("Cropping Season [Yrs]")+theme(legend.position = c(0.85, 0.75))+geom_text(data = new_sim, x = 1, y = 6000, label = "c")+geom_text(data = new_sim, x = 2, y = 6000, label = "a")+geom_text(data = new_sim, x = 3, y = 6000, label = "a")+geom_text(data = new_sim, x = 1, y = 11000, label = "c      a      b          a")+geom_text(data = new_sim, x = 2, y = 11000, label = "c      b       c          a")+geom_text(data = new_sim, x = 3, y = 11000, label = "b      c      a          b")
m


```



legume grown

```{r}
library(readr)
legume <- read_csv("~/Dr Nyagumbo/Final data set/legume.csv")
View(legume)
attach(legume)
legume_cultivar=as.factor(legume_cultivar)

md<-lm(maize_grain~legume_cultivar,data = legume)
anova(md)
summary(md)
mod<-aov(maize_grain~legume_cultivar,data = legume)

h<-HSD.test(mod,"legume_cultivar",group =TRUE)
h
bar.group(h$groups,ylim=c(0,3500),density=4,border="blue",ylab="Maize Grain Yield [kg/ha]")
```


```{r}
theme_set(theme_gray(base_size =7))
m <- ggplot(data=legume,aes(x=legume_cultivar, y=maize_grain))
m + geom_violin(size=0.8,shape=8) + geom_boxplot(width=.3, outlier.size=0,fill=c("grey","grey","grey","grey","grey","grey","grey","grey"))+ylab("Maize Grain Yield [kg/ha]") + xlab("Legumes Grown")+geom_text(data = new_sim, x = 1, y = 3000, label = "bc")+geom_text(data = new_sim, x = 2, y = 4000, label = "b")+geom_text(data = new_sim, x = 3, y = 3000, label = "bc")+geom_text(data = new_sim, x = 4, y = 3000, label = "a")+geom_text(data = new_sim, x = 5, y = 1500, label = "c")+geom_text(data = new_sim, x = 6, y = 2500, label = "ab")+geom_text(data = new_sim, x = 7, y = 3000, label = "bc")+geom_text(data = new_sim, x = 8, y = 3500, label = "b")

```



Drainage and Textural classification Analysis 

```{r}
library(readr)
drainage <- read_csv("~/Dr Nyagumbo/Final data set/drainage.csv")
View(drainage)
attach(drainage)
```


```{r}
#defining factor variables 

country=as.factor(country)
site=as.factor(site)
agroecoregion=as.factor(agroecoregion)
system=as.factor(system)
systems=factor(system,levels = c("Conv","CA",order=TRUE))
legume_cultivar=as.factor(legume_cultivar)
cropping_systems=as.factor(cropping_systems)
cropping_systems=factor(cropping_system,levels = c("Conv","CA_sole","CA_intercrop","CA_rotation"),ordered = TRUE)
season_class=as.factor(season_class)
season_clas=factor(season_class,levels = c("(0-2)","(3-5)","(>5)"),ordered = TRUE)
rainfall_class=as.factor(rainfall_class)
rainfall_clas=factor(rainfall_class,levels = c("(<700)","(700-1300)","(>1300)"),ordered = TRUE)
textureclass=as.factor(textureclass)
drainage_class=as.factor(drainage_class)
drainage_clas=factor(drainage_class,levels =c("Well drained","Moderately well drained","Somewhat poorly drained","Poorly drained"),ordered=TRUE)
```

