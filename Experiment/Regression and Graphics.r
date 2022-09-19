##### AJPS REPLICATION ####
### Graphics in DeSante 2013 ###
install.packages("foreign")
install.packages("ggplot2")
install.packages("apsrtable")
setwd("C:/Dropbox/AJPS replication files")
setwd("C:/Users/cdesante/Desktop/AJPS replication files/")
library(foreign)
library(apsrtable)
CCES <- read.dta("table3forR.dta")
m1 <- lm( deficit ~ ideology + pid7 +income +educ +age+ female+ racres01, data=CCES)

m2 <- lm( deficit ~ ideology+ pid7 +income+ educ+ age +female +racres01 +WW +WB+ BB, data=CCES)

m3 <- lm( deficit ~ ideology +pid7+ income+ educ +age+ female+ racres01+ WW+ WB+ BB+ WWrr+ WBrr +BBrr, data=CCES)

apsrtable(m1, m2 , m3, lev=.10)


##Graphics:
attach(CCES)

                     my.col <- c("black")
P <- qplot(racres01, deficit, geom="smooth", method="loess", color=my.col,
main="Bivariate Relationship Between Racial Resentment and Fiscal Conservatism \n" ,
xlab="\n Racial Resentment", ylab="Dollars Allocated to Reduce the Deficit \n", level=0.50, size=I(1.3))
P2<-    P   +  scale_colour_identity() + theme_bw()  +opts(axis.text.y = theme_text(size =10, face="bold"))    +
     opts(axis.text.x = theme_text(size =10, face="bold"))   +opts(axis.title.y = theme_text(size =14, angle=90))    +
     opts(axis.title.x = theme_text(size =14))

     P2       + opts(plot.title = theme_text(size =16))


        #saved as fig 4.
        
#Median Values:
#libcon = 4
#pid6 = 4
#income = 7.0
#educ = 3
#female = 0
#age = 57
 library(ggplot2)
mod.int <- -386 + 40* 4 + 3.17 * 4 + 2.77 * 57 + 11*3
mod.int

sim.rr <- seq(0,1, by=.01)
def.bb <-   (mod.int) + 180 + (587-242)*sim.rr
def.ww <-   (mod.int) +140 + (587-326)*sim.rr
    
 max.alloc <- rep(1500, 101)
    x.ax <- rep(sim.rr,3)
GROUPS <- c( rep( "Maximum Allocation", 101), rep("Two White Applicants", 101), rep("Two Black Applicants", 101))
GROUPSo <- factor(GROUPS, levels=c("Maximum Allocation", "Two White Applicants", "Two Black Applicants"))
DVS <- c(max.alloc, 1500-def.ww, 1500-def.bb)

 f5col <-  c( rep( "black", 101), rep("red", 101), rep("dodgerblue", 101))

fig5 <- qplot(x.ax, DVS, colour=f5col, geom="line",  linetype=factor(GROUPSo),  size=I(.80),
xlab="\n Racial Resentment", ylab="Dollars Awarded to Pair \n",
main="Decomposing Racial Resentment: Principles and Racism \n") +  scale_color_identity()

fig5 + theme_bw()  +opts(axis.text.y = theme_text(size =10, face="bold"))    +
     opts(axis.text.x = theme_text(size =10, face="bold"))   +opts(axis.title.y = theme_text(size =14, angle=90))    +
     opts(axis.title.x = theme_text(size =14))    + labs(linetype="Race of Applicants \n"
     )       + guides(colour = "legend",   size = "none") + guides(colour = guide_legend("Race of Applicants \n"),
linetype = guide_legend("Race of Applicants \n"))   + guides(colour = guide_legend("Race of Applicants \n",
            override.aes = list(size = 6))) 
            
## Begin Figure 6 ##

ad<-read.csv("fig6data.csv", header=T)
attach(ad)
names(ad)
 Target<-factor(Target, levels=c("Lazy Black", "Lazy White", "Lazy", "Black",
 "Baseline", "Hard Worker", "White", "Hard Working Black","Hard Working White (n.s.)"))

pp <- qplot(rr.aa, awarded, geom="linerange", ymin=0,ymax=awarded,colour=Target, ylim=c(0,700),
data=ad, size=I(5), xlab="\n Level of Racial Resentment", ylab="Dollars Allocated \n"
)+facet_wrap(~Target)

pp  + scale_size(legend = FALSE)   +theme_bw() +opts(legend.position="none") + opts(strip.text.x = theme_text(size =11, face="bold"))  +
     opts(axis.text.x = theme_text(size =9, face="bold"))   +opts(axis.title.y = theme_text(size =14, angle=90))    +
     opts(axis.title.x = theme_text(size =14))    + labs(linetype="Race of Applicants \n")


setwd("C:/Users/cdesante/Dropbox/AJPS replication files")
Fig1 <- read.csv("applicants.csv")
library(ggplot2)
head(Fig1)
Fig1
program <- c(rep("SNAP", 11), rep("WIC", 11)   )
COL <- c(rep("dodgerblue", 11), rep("firebrick", 11)   )
line.type <-  c(rep(1, 11), rep(4, 11)   )
Year <- rep(2000:2010, 2 )
Y.var <- c(Fig1$kSNAP/1000, Fig1$kWIC/1000)
Y.var
 Fig1.b <- data.frame(cbind(program , Year, Y.var, COL, line.type))
ggplot(data=Fig1.b, aes(x=Year, y=Y.var, colour=COL )) + geom_line() + scale_colour_identity()

      setwd("C:/Users/cdesante/Dropbox/AJPS replication files")
    Fig1 <- read.csv("Fig1.csv")
    head(Fig1)
    ggplot(data=Fig1, aes(x=Year, y = Millions , size=SIZE*1.5, colour=COL , lty=Types)
    ) + geom_line( color=Program)  + scale_size_identity()  + scale_color_identity() 
    