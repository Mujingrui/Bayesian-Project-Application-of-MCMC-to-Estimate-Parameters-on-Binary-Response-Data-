getwd()
setwd("F:/Bayesian_project")
getwd()
Pima.data<-read.csv("F:/Bayesian_project/Pima.csv",sep = ",",header=TRUE)
library(ggplot2)
#npreg
dev.off()
plot_npreg<-ggplot(Pima.data,aes(x=npreg))+geom_histogram(binwidth = 1)+geom_vline(aes(xintercept=mean(npreg, na.rm=T)),color="red",linetype="dashed",size=1)
plot_npreg
# Pima.data1<-data.frame(Pima.data,lognpreg=c(log(Pima.data$npreg)))
# plot_npreg1<-ggplot(Pima.data1,aes(x=lognpreg))+geom_histogram(binwidth = 1)+ggtitle("Histogram of npreg")+geom_vline(aes(xintercept=mean(lognpreg, na.rm=T)),color="red",linetype="dashed",size=1)
# plot_npreg1
# head(Pima.data1)
# npreg
boxplot_npreg<-ggplot(Pima.data,aes(x=type,y=npreg))+geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=4)+geom_jitter(shape=16,position=position_jitter(0.2))
boxplot_npreg
volin_npreg<-ggplot(Pima.data,aes(x=type,y=npreg))+geom_violin(alpha=0.999,width=1)+geom_jitter(colour='black')
volin_npreg
mix_npreg<-ggplot(Pima.data, aes(x=type, y=npreg))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)+geom_jitter(colour='black')
mix_npreg

#glu
plot_glu<-ggplot(Pima.data,aes(x=glu))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(glu, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_glu
glu_histogram<-ggplot(Pima.data, aes(x=glu, fill=type))+geom_histogram(binwidth=3, alpha=.5, position="identity")
glu_histogram
mix_glu<-ggplot(Pima.data, aes(x=type, y=glu))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)+geom_jitter(colour='black')
mix_glu

# bp
plot_bp<-ggplot(Pima.data,aes(x=bp))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(bp, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_bp
# par(mfrow=c(1,2))
# Define a function to display more than one plots in one picture
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

bp_plot<-ggplot(Pima.data, aes(x=type, y=bp))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)+geom_jitter(colour='black')
bp_histogram<-ggplot(Pima.data, aes(x=bp, fill=type))+geom_histogram(binwidth=3, alpha=.5, position="identity")+theme(legend.position = c(.23,.9))
multiplot(bp_plot,bp_histogram,cols=2)

# skin
plot_skin<-ggplot(Pima.data,aes(x=skin))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(skin, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_skin
skin_plot<-ggplot(Pima.data, aes(x=type, y=skin))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)+geom_jitter(colour='black')
skin_histogram<-ggplot(Pima.data, aes(x=skin, fill=type))+geom_histogram(binwidth=3, alpha=.5, position="identity")+theme(legend.position = c(.75,.9))
multiplot(skin_plot,skin_histogram,cols=2)

# bmi
plot_bmi<-ggplot(Pima.data,aes(x=bmi))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(bmi, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_bmi
bmi_plot<-ggplot(Pima.data, aes(x=type, y=bmi))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)+geom_jitter(colour='black')
bmi_histogram<-ggplot(Pima.data, aes(x=bmi, fill=type))+geom_histogram(binwidth=3, alpha=.5, position="identity")+theme(legend.position = c(.75,.9))
multiplot(bmi_plot,bmi_histogram,cols=2)

# ped
plot_ped<-ggplot(Pima.data,aes(x=ped))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(ped, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_ped
ped_plot<-ggplot(Pima.data, aes(x=type, y=ped))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)
ped_histogram<-ggplot(Pima.data, aes(x=ped, fill=type))+geom_density(alpha=.2, position="identity")+theme(legend.position = c(.75,.9))
multiplot(ped_plot,ped_histogram,cols=2)

# age
plot_age<-ggplot(Pima.data,aes(x=age))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(age, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_age
age_plot<-ggplot(Pima.data, aes(x=type, y=age))+geom_violin(fill="lightblue")+geom_boxplot(fill="lightgreen", width=.2)
age_histogram<-ggplot(Pima.data, aes(x=age, fill=type))+geom_density(alpha=.2, position="identity")+theme(legend.position = c(.75,.9))
multiplot(age_plot,age_histogram,cols=2)

# lognpreg
Pima.data2<-data.frame(Pima.data, sqrtnpreg = sqrt(Pima.data$npreg), logage = log(Pima.data$age+50))
# plot_age1<-ggplot(Pima.data2,aes(x=logage))+geom_histogram(aes(y=..density..),binwidth = 2)+geom_vline(aes(xintercept=mean(logage, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
# plot_age1
plot_age1<-ggplot(Pima.data2,aes(x=logage))+geom_histogram(aes(y=..density..),binwidth = 0.05)+geom_vline(aes(xintercept=mean(logage, na.rm=T)),color="red",linetype="dashed",size=1)+geom_density(alpha=.2, fill="#FF6666")
plot_age1

