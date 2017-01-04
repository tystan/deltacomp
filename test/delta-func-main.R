
# library(extrafont)
# fonts()[fonts()=="CM Roman"]

add_alpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb)/255
    , 2
    , function(x) rgb(x[1], x[2], x[3], alpha = alpha)
  )
}



source("../R/delta-func.R") # to get get_plus_minus_changes() function



#########################
# create variables here #
#########################


fat_data<-data.frame(fat,sl,sb,lpa,mvpa,sibs,parents,ed)




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

changes[,"delta"]<-changes[,"delta"]*60*24 # make minutes
changes


################################ plot results with CIs ################################

comps<-levels(changes[,"comp+"])
n_c<-length(comps)
cols<-c("dodgerblue","gold","indianred1","lightseagreen")


min_delta<-min(changes[,"delta"])
max_delta<-max(changes[,"delta"])
min_pred<-min(changes[,"ci_lo"],changes[,"ci_up"])
max_pred<-max(changes[,"ci_lo"],changes[,"ci_up"])


pdf("../fig/delta_comps.pdf",width=8,height=8)
# pdf("output/delta_comps_serif.pdf",width=8,height=8,family="CM Roman")

	par(mfrow=c(2,2),lend=2)

	for(i in 1:n_c) {

		which_i<-changes[,"comp+"]==comps[i]
		y<-changes[which_i,"delta_pred"]
		x<-changes[which_i,"delta"]
		ci_x<-c(x,rev(x))
		ci_y<-c(changes[which_i,"ci_lo"],rev(changes[which_i,"ci_up"]))
		plot(x,y,type="n",bty="n",xlim=c(min_delta,max_delta),ylim=c(min_pred,max_pred)
			,xlab="Time change in activity (min)",ylab="Change in fat percentage",las=1)
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
		)

	}

dev.off()
# embed_fonts("../fig/delta_comps_serif.pdf")





