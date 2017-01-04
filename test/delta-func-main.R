
setwd("~/Documents/deltacomp/test/")

# library(extrafont)
# fonts()[fonts()=="CM Roman"]

add_alpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb)/255
    , 2
    , function(x) rgb(x[1], x[2], x[3], alpha = alpha)
  )
}

stdise<-function(x) return((x-mean(x))/sd(x))



source("../R/req-packages.R") # required packages
source("../R/delta-func.R") # to get get_plus_minus_changes() function



#########################
# create variables here #
#########################

max_mins<-24*60

set.seed(1234)
n<-100
sb<-round(rchisq(n,df=500^(1/1.3))^1.3,0) # sedentary behaviour
summary(sb)
lpa<-round(rchisq(n,df=300^(1/1.3))^1.3,0) # lo physical activity
summary(lpa)
mvpa<-round(rchisq(n,df=75^(1/1.3))^1.3,0) # mod-to-vig physical activity
summary(mvpa)
sl<-max_mins-sb-lpa-mvpa # sleep, the remainder of the composition
summary(sl)


sibs<-pmin(rbinom(n,size=9,p=0.2),4)
table(sibs)
parents<-rbinom(n,size=1,p=0.85)+1
table(parents)
ed<-pmin(rbinom(n,size=4,p=0.4),2)
table(ed)

# outcome variable
fat<-20+2*(
  +0.1*stdise(sl)+0.6*stdise(sb)-0.2*stdise(lpa)-0.6*stdise(mvpa)+
  -0.1*stdise(sibs^2)-0.01*(parents-1)-0.01*stdise(ed^2)
)+rnorm(n,sd=3)
summary(fat)

# make 
sibs<-factor(sibs)
parents<-factor(parents)
ed<-factor(ed)

fat_data<-data.frame(fat,sl,sb,lpa,mvpa,sibs,parents,ed)
summary(fat_data)



###################################### run func ######################################

(changes<-get_plus_minus_changes(
    dataf=fat_data
  , y="fat"
  , comps=c("sl","sb","lpa","mvpa")
  , covars=c("sibs","parents","ed")
  , deltas=seq(-60,60,by=5)/(24*60)
  , alpha=0.05
  , verbose=FALSE
))

changes[,"delta"]<-changes[,"delta"]*max_mins # make minutes
changes


################################ plot results with CIs ################################

comps<-levels(changes[,"comp+"])
n_c<-length(comps)
cols<-c("dodgerblue","gold","indianred1","lightseagreen")


min_delta<-min(changes[,"delta"])
max_delta<-max(changes[,"delta"])
min_pred<-min(changes[,"ci_lo"],changes[,"ci_up"])
max_pred<-max(changes[,"ci_lo"],changes[,"ci_up"])


png("../fig/delta_comps.png",width=6,height=6,units="in",res=300)
# pdf("output/delta_comps_serif.pdf",width=8,height=8,family="CM Roman")

	par(mfrow=c(2,2),lend=2)

	for(i in 1:n_c) {

		which_i<-changes[,"comp+"]==comps[i]
		y<-changes[which_i,"delta_pred"]
		x<-changes[which_i,"delta"]
		ci_x<-c(x,rev(x))
		ci_y<-c(changes[which_i,"ci_lo"],rev(changes[which_i,"ci_up"]))
		plot(x,y,type="n",bty="n",xlim=c(min_delta,max_delta),ylim=c(min_pred,max_pred)
			,xlab="Time change in activity (min)",ylab="Change in fat percentage",las=1
			,cex.axis=0.7,cex.lab=0.85)
		abline(v=0,col=add_alpha("black",0.1),lty=2)
		abline(h=0,col=add_alpha("black",0.1),lty=2)
		lines(x,y,col=cols[i])
		polygon(ci_x,ci_y,col=add_alpha(cols[i],0.3),border=NA)
		legend("topright"
			,legend=c(comps[i],"95% CI")
			,col=c(add_alpha(cols[i],c(1,0.3)))
			,lty=c(1,NA)
			,lwd=c(3,NA)
			,pch=c(NA,15)
			,bty="n"
			,cex=0.9
		)

	}

dev.off()
# embed_fonts("../fig/delta_comps_serif.pdf")





