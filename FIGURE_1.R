#Read in packages:
library(ggplot2)
library(ggsignif)
library(gridExtra)
#Read in data:
data<- read.csv("Accuracy_precision_recall_vals_r.csv")
#Subset data by method
OG_RGI <-subset(data, Method == "OG_RGI")
LR <- subset(data, Method == "LR")
RGI_S <- subset(data, Method == "RGI_specific")
RGI_A <- subset(data, Method == "RGI_all")
Egg <- subset(data, Method == "Eggnog")
#Perform paired t-test and save as a unique name to use when making the plot
t.test.results.OLa <- t.test(OG_RGI$Accuracy, LR$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.OSa <- t.test(OG_RGI$Accuracy, RGI_S$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.OAa <- t.test(OG_RGI$Accuracy, RGI_A$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.OEa <- t.test(OG_RGI$Accuracy, Egg$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.LSa <- t.test(LR$Accuracy, RGI_S$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.LAa <- t.test(LR$Accuracy, RGI_A$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.LEa <- t.test(LR$Accuracy, Egg$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.SAa <- t.test(RGI_S$Accuracy, RGI_A$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.SEa <- t.test(RGI_S$Accuracy, Egg$Accuracy, paired = TRUE, alternative = "two.sided")
t.test.results.AEa <- t.test(RGI_A$Accuracy, Egg$Accuracy, paired = TRUE, alternative = "two.sided")
# Make sure data is in the correct order
data$Method <- factor(data$Method,
                      levels = c('OG_RGI', 'LR','RGI_specific', 'RGI_all', 'Eggnog'),ordered = TRUE)
# Assign which level of significance each star gets 
map_signif_level <- function(p.value) {
  if (p.value < 0.0005) return("***")
  if (p.value < 0.005) return("**")
  if (p.value < 0.05) return("*")
  return("ns")
}
# Make a colour pallete
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7", "#000000")
# Make a basic box plot with the points on and the outliers and assign to a value
A <- ggplot(data, aes(x=Method, y=Accuracy, fill=Method)) + 
  geom_boxplot(alpha=0.6, outlier.shape = 17, outlier.size = 2.5, outlier.colour= "black") + 
  geom_point(aes(), position = position_jitter(width = 0.05))
# using ggplot_build(p) this will give info about the boxplot
gg<-ggplot_build(A)
#
gg$data[[1]]
#
xx<-gg$data[[1]][c("group","outliers")]
# Change the group values to the Method values
xx$group<-c("OG_RGI","LR", "RGI_specific", "RGI_all", "Eggnog")
#
data.new<-merge(data,xx,by.x="Method",by.y="group")
#
data.new$out<-apply(data.new,1,function(x) x$Accuracy %in% x$outliers)

# Make the new plot with data.new
Plot1 <- ggplot(data.new, aes(factor(Method), Accuracy, fill=Method)) + 
           geom_boxplot(outlier.shape = NA, alpha=0.6)  + #Note alpha alters the saturation of the colours
           geom_point(aes(shape=out,size=out), position = position_jitter(w=0.15))+
           geom_signif(comparisons = list(c("OG_RGI", "LR")), y_position = 102, tip_length = 0.01, textsize = 2.5, vjust = 0.75, # These following lines add sig. bars and *s 
              test.stat = t.test.results.OLa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("OG_RGI", "RGI_specific")),y_position = 107, tip_length = 0.01, textsize = 2.5, vjust = 0.75,# vjust changes the gap between text and the bar
              test.stat = t.test.results.OSa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("OG_RGI", "RGI_all")), y_position = 112, tip_length = 0.01, textsize = 2.5, vjust = 0.75,
              test.stat = t.test.results.OAa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("OG_RGI", "Eggnog")), y_position = 117, tip_length = 0.01, textsize = 2.5, vjust = 0.75,
              test.stat = t.test.results.OEa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "RGI_specific")), y_position = 122, tip_length = 0.01, textsize = 2.5, vjust = 0.75,
                 test.stat = t.test.results.LSa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "RGI_all")), y_position = 127, tip_length = 0.01, textsize = 2.5, vjust = 0.75,
              test.stat = t.test.results.LAa$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "Eggnog")), y_position = 132, tip_length = 0.01, textsize = 2.5, vjust = 0.75,
              test.stat = t.test.results.LEa$p.value, map_signif_level = map_signif_level) +
          scale_shape_manual(values=c(16,17),guide="none")+ #these numbers relate to the shape numbers (of the points)
          scale_size_manual(values=c(1.5,2.5),guide="none") + #these are the respective sizes
          scale_x_discrete(labels=c('1', '2', '3', '4', '5')) +
          labs(y = "Average accuracy (%)", x = "Method") +
          theme(panel.background = element_rect(fill = "white", colour = "black")) + 
          ggtitle("Accuracy of the methods.") +
          scale_fill_manual(values=cbbPalette, guide="none", labels=c('Original RGI analysis', 'Logistic regression of RGI genes', 
                                                                      'J48 model using RGI specific genes', 'J48 model using RGI all genes', 
                                                                      'J48 model using Eggnog gene families')) +

#Now we will do the same for precision and recall:

# Precision
precision <- subset(data, select = c(Antibiotic, Method, average_precision))

OG_RGI_P <-subset(precision, Method == "OG_RGI")
LR_P <- subset(precision, Method == "LR" )
RGI_S_P <- subset(precision, Method == "RGI_specific")
RGI_A_P <- subset(precision, Method == "RGI_all")
Egg_P <- subset(precision, Method == "Eggnog")

t.test.results.OLp <- t.test(OG_RGI_P$average_precision, LR_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.OSp <- t.test(OG_RGI_P$average_precision, RGI_S_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.OAp <- t.test(OG_RGI_P$average_precision, RGI_A_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.OEp <- t.test(OG_RGI_P$average_precision, Egg_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.LSp <- t.test(LR_P$average_precision, RGI_S_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.LAp <- t.test(LR_P$average_precision, RGI_A_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.LEp <- t.test(LR_P$average_precision, Egg_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.SAp <- t.test(RGI_S_P$average_precision, RGI_A_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.SEp <- t.test(RGI_S_P$average_precision, Egg_P$average_precision, paired = TRUE, alternative = "two.sided")
t.test.results.AEp <- t.test(RGI_A_P$average_precision, Egg_P$average_precision, paired = TRUE, alternative = "two.sided")


P <- ggplot(data, aes(x=Method, y=average_precision, fill=Method)) + 
  geom_boxplot(outlier.shape = 17, outlier.size = 2.5, outlier.colour= "black", alpha=0.6) + 
  geom_point(aes(), position = position_jitter(w = 0.1, h = 0)) 

ggp <- ggplot_build(P)
#
ggp$data[[1]]
#
xxp<-ggp$data[[1]][c("group","outliers")]
# Change the group values to the Method values
xxp$group<-c("OG_RGI","LR", "RGI_specific", "RGI_all", "Eggnog")
#
data.newp <-merge(data,xxp,by.x="Method",by.y="group")
#
data.newp$out<-apply(data.newp,1,function(x) x$Accuracy %in% x$outliers)

Plot2 <- ggplot(data.newp, aes(x=Method, y=average_precision, fill=Method)) + 
            geom_boxplot(outlier.shape = NA, alpha=0.6) + 
            geom_signif(comparisons = list(c("OG_RGI", "RGI_specific")),y_position = 102, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.OSp$p.value, map_signif_level = map_signif_level) +
            geom_signif(comparisons = list(c("OG_RGI", "RGI_all")), y_position = 107, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.OAp$p.value, map_signif_level = map_signif_level) +
            geom_signif(comparisons = list(c("OG_RGI", "Eggnog")), y_position = 112, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.OEp$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "RGI_specific")), y_position = 117, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.LSp$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "RGI_all")), y_position = 122, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.LAp$p.value, map_signif_level = map_signif_level) +
           geom_signif(comparisons = list(c("LR", "Eggnog")), y_position = 127, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
              test.stat = t.test.results.LEp$p.value, map_signif_level = map_signif_level) +
           geom_point(aes(shape=out,size=out), position = position_jitter(w=0.15))+
           scale_shape_manual(values=c(16,17),guide="none")+ #these numbers relate to the shape numbers (of the points)
           scale_size_manual(values=c(1.5,2.5),guide="none") + #these are the respective sizes
           scale_x_discrete(labels=c('1', '2', '3', '4', '5')) +
           labs(y = "Average precision (%)", x = "Method") +
           theme(panel.background = element_rect(fill = "white", colour = "black")) + 
           ggtitle("Average precision of the methods.") +
           scale_fill_manual(values=cbbPalette, guide="none", labels=c('Original RGI analysis', 'Logistic regression of RGI genes', 'J48 model using RGI specific genes', 'J48 model using RGI all genes', 'J48 model using Eggnog gene families'))

# Recall:

recall <- subset(data, select = c(Antibiotic, Method, average_recall))

OG_RGI_R <-subset(recall, Method == "OG_RGI")
LR_R <- subset(recall, Method == "LR" )
RGI_S_R <- subset(recall, Method == "RGI_specific")
RGI_A_R <- subset(recall, Method == "RGI_all")
Egg_R <- subset(recall, Method == "Eggnog")


t.test.results.OLr <- t.test(OG_RGI_R$average_recall, LR_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.OSr <- t.test(OG_RGI_R$average_recall, RGI_S_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.OAr <- t.test(OG_RGI_R$average_recall, RGI_A_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.OEr <- t.test(OG_RGI_R$average_recall, Egg_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.LSr <- t.test(LR_R$average_recall, RGI_S_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.LAr <- t.test(LR_R$average_recall, RGI_A_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.LEr <- t.test(LR_R$average_recall, Egg_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.SAr <- t.test(RGI_S_R$average_recall, RGI_A_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.SEr <- t.test(RGI_S_R$average_recall, Egg_R$average_recall, paired = TRUE, alternative = "two.sided")
t.test.results.AEr <- t.test(RGI_A_R$average_recall, Egg_R$average_recall, paired = TRUE, alternative = "two.sided")

R <- ggplot(data, aes(x=Method, y=average_recall, fill=Method)) + 
        geom_boxplot(outlier.shape = 17, outlier.size = 2.5, outlier.colour= "black", alpha=0.6) + 
        geom_point(aes(), position = position_jitter(w = 0.1, h = 0)) 


ggr <- ggplot_build(R)
#
ggr$data[[1]]
#
xxr<-ggr$data[[1]][c("group","outliers")]
# Change the group values to the Method values
xxr$group<-c("OG_RGI","LR", "RGI_specific", "RGI_all", "Eggnog")
#
data.newr <-merge(data,xxr,by.x="Method",by.y="group")
#
data.newr$out<-apply(data.newr,1,function(x) x$Accuracy %in% x$outliers)

Plot3 <- ggplot(data.newr, aes(x=Method, y=average_recall, fill=Method)) + 
                geom_boxplot(outlier.shape = NA, alpha=0.6) + 
                geom_signif(comparisons = list(c("LR", "RGI_specific")), y_position = 102, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
                  test.stat = t.test.results.LSr$p.value, map_signif_level = map_signif_level) +
                geom_signif(comparisons = list(c("LR", "RGI_all")), y_position = 107, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
                  test.stat = t.test.results.LAr$p.value, map_signif_level = map_signif_level) +
                geom_signif(comparisons = list(c("LR", "Eggnog")), y_position = 112, tip_length = 0.025, textsize = 2.5, vjust = 0.5,
                  test.stat = t.test.results.LEr$p.value, map_signif_level = map_signif_level) +
                geom_point(aes(shape=out,size=out), position = position_jitter(w=0.15))+
                scale_shape_manual(values=c(16,17),guide="none")+ #these numbers relate to the shape numbers (of the points)
                scale_size_manual(values=c(1.5,2.5),guide="none") + #these are the respective sizes
                scale_x_discrete(labels=c('1', '2', '3', '4', '5')) +
                labs(y = "Average recall (%)", x = "Method") +
                theme(panel.background = element_rect(fill = "white", colour = "black")) + 
                ggtitle("Average recall of the methods.") +
                scale_fill_manual(values=cbbPalette, guide="none", labels=c('Original RGI analysis', 'Logistic regression of RGI genes', 
                                                                              'J48 model using RGI specific genes', 'J48 model using RGI all genes', 
                                                                              'J48 model using Eggnog gene families'))

grid.arrange(Plot1,Plot2, Plot3, nrow=2, ncol=2)
