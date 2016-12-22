charts <- function(content, topic, outcome, counted, timeperiod, goalu, goall, type, theme) {
temp <- content
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
#temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)
temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=4, byrow=TRUE, dimnames = list(NULL, c("period.name", "count", "total","Trial")))',sep = '')
x <- eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
myframe$period.name<-gsub("\'", '', fixed = TRUE, myframe$period.name)
myframe$period.name<-str_trim(as.character(myframe$period.name))
myframe$count<-as.numeric(as.character(str_trim(myframe$count)))
myframe$total<-as.numeric(as.character(str_trim(myframe$total)))
if (length(timeperiod) > 0)
	{timeperiod<-paste("Time period (",timeperiod,")",sep="")}
#myframe$Trial<-as.logical(str_trim(myframe$Trial))
myframe$Trial<-str_trim(as.character(myframe$Trial))
#myframe$Trial<-str_trim(myframe$Trial)
if (goalu < 0){goalu = 0}
if (goall < 0){goall = 0}
goalu <- as.numeric(goalu)
goall <- as.numeric(goall)
extremevalue = 0 #Later used in trials with outcomes of proportions. If = 1, will use quasibinomial

attach(myframe)
# http://www.identity.ku.edu/colors/index.shtml
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue)
qcc.options("cex.stats"=0.9, "cex" = 2, "bg.margin"=SkyBlue,"run.length" = 6, "beyond.limits" = list(pch = 20, col = "black"), "violating.runs" = list(pch = 16, col = "orange")) #Shift. A run
sequential = FALSE
period = 0
trialstart = 0
for(i in 1: length(myframe$period.name))
{
period[i] = i
if(myframe$Trial[i] == '1')
	{
	if (sequential == FALSE){trialstart = period[i]}
	sequential = TRUE
	}
else
	{
	}
#Trial[i] =as.logical(Trial[i])
#Below done to avoid handling strings such as "Jan, 2013"
#myframe$period[i] <- i
}
myframe <- cbind(myframe,period)
attach(myframe)

if (type == "r" || type == "R"){sequential = FALSE}

if (sequential == FALSE)
	{
	if (toupper(type) == "SPC-P")
		{
		myframe$currentvalue <- count/total
		spc <- qcc(count,sizes=total,type="p", xlab="",ylab="",title="",labels=myframe$period.name, ylim=c(0,1), digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
		#mtext(bquote("Proportion "~~bolditalic(.(outcome))), side=2, line=2.5, col=KUBlue , cex=1.5)
		y.label = bquote("Proportion "~~bolditalic(.(outcome)))
		average = paste("Average ",outcome," = ",round(spc$center*100,digits = 1),"%", sep = "")
		if(theme=="KU"){display_logo(x=1.2,y=0.2)}
		}
	else 
		{
		if (toupper(type) == "R") #Run chart
			{
			numerator = 0
			denominator = 0
			for(i in 1:length(period))
				{
				numerator <- numerator + count[i]
				denominator <- denominator + total[i]
				}
			par(fin=c(8.8,6))
			plot (row(myframe)[,1],count/total, ylim=c(0,1), xlab="", ylab="", type="b",xaxs="r",axes=F,,xaxt="n")
			axis(side=1,at=1:length(myframe$period),labels=myframe$period.name)
			axis(2,  xaxp=c(0,1,10))
			box()
			mtext(bquote("Proportion "~~bolditalic(.(outcome))), side=2, line=2.5, col=KUBlue , cex=1.5)
			average = paste("Average ",outcome," = ",round(100*numerator/denominator,digits = 1),"%", collapse = NULL)
			if(theme=="KU"){display_logo(x=1.15,y=0.06)}
			}
		else
			{
			if (toupper(type) == "B") #Box plot
				{
				#This does not make sense. Cannot do box chart outside of a trial
				plot.new()
				topic= "Ooops"
				y.label = "oops"
				average=""
				}
			else #c-chart
				{
				if (counted == "events") 
					{
					currentvalue <- count
					spc <- qcc(count,type="c", xlab="",ylab="",title="",labels=myframe$period.name, digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
					y.label = bquote("Count "~~bolditalic(.(outcome)))
					}
				if (counted == "total")  
					{
					currentvalue <- total
					spc <- qcc(total,type="c", xlab="",ylab="",title="",labels=myframe$period.name, digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
					y.label = bquote("Count of "~~bolditalic(total)~~"encounters")
					}
				average = paste("Average ",outcome," = ",round(spc$center,digits = 1),"", sep = "")
				plot(spc, add.stats = TRUE, chart.all = TRUE, label.limits = c("LCL ", "UCL"), title = "", xlab="",ylab="", axes.las = 0, digits = 2)
				}
			}
		}
	par(new=TRUE,xpd=NA)
	plot.new()
	if (toupper(type)=="R")
		{
		mtext(timeperiod, side=1, line=3, col=KUBlue , cex=1.5)
		mtext(average, side=1, line=4.5, col=KUBlue , cex=1)
		}
	else
		{
		#mtext(subtitle, side=3, line=0.5, col=KUBlue , cex=1.2)
		mtext(y.label, side=2, line=3.5, col=KUBlue , cex=1.5)
		mtext(timeperiod, side=1, line=-1.0, col=KUBlue , cex=1.5)
		mtext(average, side=1, line=-0.2, col=KUBlue , cex=1)
		
		}
	mtext(topic, side=3,line=2,col=KUBlue,font=2, cex=3)
	#Goals or targets
	par(new=TRUE,xpd=FALSE)
	if (goalu >= 0 && goall >= 0)
		{
		regionx = c(-1,-1,length(period) + 1,length(period) + 1)
		regiony = c(goall,goalu,goalu,goall)
		polygon(regionx,regiony,col=rgb(0,1,0,alpha=0.05),border = NA)
		axis(4,at=c(goall,goalu),labels=c(goall,goalu),col.ticks="green")
		}
	if (grepl("flu", topic, ignore.case = TRUE) > 0 && grepl("vacc", topic, ignore.case = TRUE) > 0)
		{ #http://www.cdc.gov/flu/fluvaxview/reports/reporti1213/reportii/index.htm
		  # http://www.healthypeople.gov/2020/topicsobjectives2020/objectiveslist.aspx?topicId=23
			if (outcome == "nonconforming")
				{
				abline(a = 0.585, b = 0, col="red", lty = 2, lwd = 2)
				abline(a = 0.2, b = 0, col="green", lty = 2, lwd = 2)
				legend("topright", legend=c("National rate 2012-2013 (41.5%)","Healthy People 2020 goal (80%)"),col=c("red","green"),lty=2, lwd = 2, inset=0.05)
				}
			if (outcome == "conforming")
				{
				abline(a = 0.415, b = 0, col="red", lty = 2, lwd = 2)
				abline(a = 0.8, b = 0, col="green", lty = 2, lwd = 2)
				legend("topleft", legend=c("Healthy People 2020 goal (80%)","National rate 2012-2013 (41.5%)"),col=c("green","red"),lty=2, lwd = 2,  inset=0.05)
				}
		}
	if (grepl("re-admission", topic, ignore.case = TRUE) > 0 | grepl("readmission", topic, ignore.case = TRUE) > 0)
		{
		abline(a = 0.178, b = 0, col="red", lty = 2)
		abline(a = 0.122, b = 0, col="green", lty = 2)
		legend("topright", legend=c("Medicare (17.8%)","Kaiser (12.2%)"),col=c("red","green"),lty=2, inset=0.05)
		}
	}
else #sequential == TRUE
	{
	if (toupper(type) == "B")
		{
		cella = 0
		cellb = 0
		cellc = 0
		celld = 0
		for(i in 1: length (period))
			{
			if (Trial[i] == 0)
				{
				cellc = cellc + count[i]
				celld = celld + (total[i] - count[i])
				}
			else
				{
				cella = cella + count[i]
				cellb = cellb + (total[i] - count[i])
				}
			}
		twobytwo=rbind(c(cella,cellb),c(cellc,celld))
		print("###########################################")
		print(twobytwo)
		names(Trial) <- c("Baseline","Trial")
		par(cex.main = 3)
		boxplot(count/total ~ Trial,ylab=bquote("Mean proportion "~~bolditalic(.(outcome))),main=topic, ylim=c(0,1),names.arg=c("Baseline","Trial"))
		axis(1, at= 1:2, lab=c("Baseline","Trial"),tick=FALSE, cex = 2)
		mtext(side=3,line=0.2,"(before and after analysis)", font=1)
		mm<-tapply(count/total,Trial, median,na.rm=TRUE)
		text(1-0.4,mm[1],round(mm[1],2),pos=2)
		text(2-0.4,mm[2],round(mm[2],2),pos=2)
		#median<-wilcox.test(count/total ~ Trial, alternative="two.sided")
		#text(2,0.95,paste("p=", round(median$p.value[1],3),"\n(non-parametric)"),adj=0,font=1,col='black')
		mean<-fisher.test(twobytwo)
		text(2,0.95,paste("p=", sprintf("%.3f",mean$p.value[1]),"\n(Fisher's Exact)"),adj=0,font=1,col='black')
		print (paste("p=",sprintf("%.3f",mean$p.value[1])))
		if(theme=="KU"){display_logo(x=1.2,y=0.05)}
		}
	else 
		{
		if (toupper(type) == "SPC-P")
			{
			myframe$currentvalue <- count/total
			spc <- qcc(data=count[Trial=='0'],sizes=total[Trial=='0'],newdata=count[Trial=='1'], newsizes=total[Trial=='1'],type="p", xlab="",ylab="",title="",labels=period.name[Trial=='0'],newlabels=period.name[Trial=='1'],ylim=c(0,1), digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
			subtitle = "p chart: before-after trial"
			average = paste("Average (pretrial) = ",round(spc$center*100,digits = 1),"%", sep = "")
			y.label = bquote("Proportion "~~bolditalic(.(outcome)))
			ylim=c(0,1.15)
			if (max(myframe$currentvalue) == 1 || min(myframe$currentvalue == 0)){extremevalue=1}
			if (extremevalue==1)
				{
				mydistribution = quasibinomial(logit)
				}
			else
				{
				mydistribution = binomial(logit)
				}
			weights.type = total
			}
		if (toupper(type) == "SPC-C")
			{
			if (counted == "events") 
				{
				myframe$currentvalue <- count
				spc <- qcc(count[Trial=="0"],newdata=count[Trial=="1"],type="c", xlab="",ylab="",title="",labels=period.name[Trial=="0"],newlabels=period.name[Trial=="1"], digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
				}
			if (counted == "total") 
				{
				myframe$currentvalue <- total
				spc <- qcc(total[Trial=="0"],newdata=total[Trial=="1"],type="c", xlab="",ylab="",title="",labels=period.name[Trial=="0"],newlabels=period.name[Trial=="1"], digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
				}
			mydistribution = poisson(log)
			#Should this regression have weights = count ? Seems to reduce standard error
			#Per GLM: "For a binomial GLM prior weights are used to give the number of trials when the response is the proportion of successes: they would rarely be used for a Poisson GLM"
			weights.type= NULL
			subtitle = "c chart: before-after trial"
			average = paste("Average (pretrial) = ",round(spc$center,digits = 1),"", sep = "")
			y.label = bquote("Count "~~bolditalic(.(outcome)))
			ylim= NULL #c(0,max(currentvalue)+1)
			}
		if (grepl("SPC", type, ignore.case = TRUE))
			{
			plot(spc, add.stats = TRUE, chart.all = TRUE, label.limits = c("LCL ", "UCL"), title = "", xlab="",ylab="", ylim=ylim,axes.las = 0, digits = 2)
			par(new=TRUE,xpd=NA)
			plot.new()
			mtext(timeperiod, side=1, line=-1.1, col=KUBlue , cex=1.3, outer = FALSE)
			mtext(average, side=1, line=-0.1, col=KUBlue , cex=1, outer = FALSE)
			glm.out=glm(currentvalue ~ as.numeric(Trial) + as.numeric(period), family=mydistribution,weights = weights.type, data=myframe)
			sum.sig <- summary(glm.out)
			significance = paste("P-value for secular change (linear regression) = ",format(round(coef(sum.sig)["as.numeric(period)",4],digits = 3), nsmall = 3), sep = "")
			mtext(significance, side=1, line=0, col=KUBlue , cex=1,adj = 1)
			significance = paste("P-value for trial (linear regression) = ",format(round(coef(sum.sig)["as.numeric(Trial)",4],digits = 3), nsmall = 3), sep = "")
			mtext(significance, side=1, line=1, col=KUBlue , cex=1,adj = 1)
			#mtext(myframe$currentvalue, side=1, line=2, col=KUBlue , cex=1,adj = 1) #troubleshooting
			}
		if (grepl("SR", type, ignore.case = TRUE))
			{
			if (length(myframe$period[Trial=='0']) < 3){stop('Insufficent number of values before trial do conduct a segmented regression analysis')}
			if (length(myframe$period[Trial=='1']) < 3){stop('Insufficent number of values after trial do conduct a segmented regression analysis')}
			subtitle = "segmented regression analysis"
			par(mar=c(8.5, 5, 4, 2) + 0.1)
			if (toupper(type) == "SR-P")
				{
				myframe$currentvalue <- count/total
				if (max(myframe$currentvalue) == 1 || min(myframe$currentvalue == 0)){extremevalue=1}
				if (extremevalue==1)
					{
					mydistribution = quasibinomial(logit)
					}
				else
					{
					mydistribution = binomial(logit)
					}
				weights.type=total
				if (max(myframe$currentvalue) == 1 || min(myframe$currentvalue == 0)){mydistribution=quasibinomial(logit)}
				ylim=c(0,1.15)
				y.label = bquote("Proportion "~~bolditalic(.(outcome)))
				}
			if (toupper(type) == "SR-C")
				{
				if (counted == "events")
					{
					myframe$currentvalue <- count
					y.label = bquote("Number of events"~~bolditalic(.(outcome)))
					}
				if (counted == "total")
					{
					myframe$currentvalue <- total
					y.label = bquote("Number "~~bolditalic(.(outcome)))
					}
				mydistribution = poisson(log)
				#Should this regression have weights = count ? Seems to reduce standard error
				#Per GLM: "For a binomial GLM prior weights are used to give the number of trials when the response is the proportion of successes: they would rarely be used for a Poisson GLM"
				weights.type=NULL
				ylim=c(0,max(myframe$currentvalue)+1)
				}
			plot(myframe$period,myframe$currentvalue,type="p", xaxt='n', xlab="",xlim=c(min(myframe$period),max(myframe$period)), ylim=ylim,pch=16,ylab="", main="")
			axis(1, at=myframe$period, labels=myframe$period.name)
			mtext(timeperiod, side=1, line=2, col=KUBlue , cex=1.3, outer = FALSE)
			##Regression calibration data
			glm.out<-glm(currentvalue[Trial=='0'] ~ as.numeric(period[Trial=='0']), family=mydistribution,weights = weights.type[Trial=='0'], data=myframe)
			y.slope <- glm.out$coef[2]
			y.slope.se <- coef(summary(glm.out))[2, "Std. Error"]
			y <- predict(glm.out,data.frame(period=c(seq(1, length(myframe$period[Trial=='0'])-1, by = 1),length(myframe$period[Trial=='0'])+1)),se.fit=TRUE,type="response")
			lines(c(seq(1, length(myframe$period[Trial=='0'])-1, by = 1),length(myframe$period[Trial=='0'])+1),y$fit,col=SkyBlue, lwd=2)
			##Regression trial data
			glm.out<-glm(currentvalue[Trial=='1'] ~ as.numeric(period[Trial=='1']), family=mydistribution,weights = weights.type[Trial=='1'], data=myframe)
			yy.slope <- glm.out$coef[2]
			yy.slope.se <- coef(summary(glm.out))[2, "Std. Error"]
			yy <- predict(glm.out,se.fit=TRUE,type="response")
			lines(na.omit(myframe$period[Trial=='1']), yy$fit,col=SkyBlue ,lwd=2)
			##Sig testing
			##Segmented regression
			### Level change?
			segments(length(myframe$period[Trial=='0'])+1, y$fit[length(myframe$period[Trial=='0'])],length(myframe$period[Trial=='0'])+1,yy$fit[1], col=KUBlue ,lwd=2)
			points(length(myframe$period[Trial=='0'])+1,y$fit[length(myframe$period[Trial=='0'])],col=KUBlue ,bg="white",cex=1.5,pch=21)
			level.change<-abs(y$fit[length(myframe$period[Trial=='0'])]-yy$fit[1])
			t <- (level.change)/sqrt(y$se.fit[length(myframe$period[Trial=='0'])]^2/length(myframe$period[Trial=='0']) + yy$se.fit[1]^2/length(myframe$period[Trial=='1']))
			p.level <- 1- pt(t,length(myframe$period[Trial=='0']) + length(myframe$period[Trial=='1']) - 2);
			if(p.level < 0.05) {color="red"; line.width=2}else{color="black"}
			#text (4+strwidth("A"),y$fit[3],adj=c(0,0.5),paste(round(y$fit[3],2),"(",round(y$fit[3]-y$se.fit[3]*1.96,2)," to ",round(y$fit[3]+y$se.fit[3]*1.96,2),")", sep=""))
			#text (4+strwidth("A"),yy$fit[1]+0.5*level.change,adj=c(0,0.5),paste(round(level.change,2)," (p = ",paste(format.pval(p.level,eps=0.001,digits = 3),")"), sep=""))
			#text (4-strwidth("A"),yy$fit[1],adj=c(1,0.5),paste(round(yy$fit[1],2),"(",round(yy$fit[1]-yy$se.fit[1]*1.96,2)," to ",round(yy$fit[1]+yy$se.fit[1]*1.96,2),")", sep=""))
			significance = paste("Level change at breakpoint (vertical dark line): ",round(level.change,2)," ; p = ",format.pval(p.level,eps=0.001,digits = 3), sep = "")
			mtext(significance, side=1, line=3, col=color , cex=1,adj = 0)

			mtext(paste("Enduring change detected after ",length(myframe$period)," observations?"), side=1, line=4, col="black" , cex=1,adj = 0)
			
			### Slope difference?
			slope.change<-y.slope-yy.slope;
			t <- (slope.change)/sqrt(y.slope.se + yy.slope.se)
			p.slope = 1- pt(t,length(myframe$period[Trial=='0']) + length(myframe$period[Trial=='1']) - 2);
			if(p.slope < 0.05) {color="red"; line.width=2}else{color="black"}
			#text (par("usr")[2],par("usr")[4]-strheight("A"),adj=c(1,1),paste("Slope before: ",round(y.slope,2),"(",round(y.slope+abs(y.slope.se) *1.96,2)," to ",round(y.slope-abs(y.slope.se) *1.96,2),")", sep=""))
			#text (par("usr")[2],par("usr")[4]-2.3*strheight("A"),adj=c(1,1),paste("Slope after: ",round(yy.slope,2),"(",round(yy.slope+abs(yy.slope.se) *1.96,2)," to ",round(yy.slope-abs(yy.slope.se) *1.96,2),")", sep=""))
			#text (par("usr")[2],par("usr")[4]-3.5*strheight("A"),adj=c(1,1),paste("P for difference: ",format.pval(p.slope,eps=0.001,digits = 3)))
			significance = paste("    Slope change at breakpoint (segmented regression): p = ",format(round(p.slope,digits = 3), nsmall = 3), sep = "")
			mtext(significance, side=1, line=5, col=color , cex=1,adj = 0)
			## Linear or logistic regression
			glm.out1     = glm(currentvalue ~ as.numeric(Trial) + as.numeric(period), family=mydistribution, weights=weights.type, data=myframe)
			sum.sig <- summary(glm.out1)
			##Trial by linear regression
			significance = paste("    Mean rates, pre/post (linear regression controlling for secular change): p = ",format(round(coef(sum.sig)[2,4],digits = 3), nsmall = 3), sep = "")
			if(coef(sum.sig)[2,4] < 0.05) {color="red"; line.width=2}else{color="black"}
			mtext(significance, side=1, line=6, col=color , cex=1,adj = 0)
			#Secular by linear regression
			significance = paste("Secular change (linear regression controlling for intervention): p = ",format(round(coef(sum.sig)[3,4],digits = 3), nsmall = 3), sep = "")
			# equivalent: significance = paste("P-value for secular change (linear regression) = ",format(round(coef(sum.sig)["as.numeric(period)",4],digits = 3), nsmall = 3), sep = "")
			if(coef(sum.sig)[3,4] < 0.05) {color="red"; line.width=2}else{color="black"}
			mtext(significance, side=1, line=7, col=color , cex=1,adj = 0)
			#mtext(myframe$currentvalue, side=1, line=8, col=color , cex=1,adj = 0) #troubleshooting
			#Segmented regression with Davies - don't use as does not model a level change at the breakpoint
			#davies.out<-davies.test(lm(currentvalue ~ as.numeric(period),weights = total),~ period, k=length(myframe$period)*100)
			#lm.out<-lm(currentvalue ~ period, weights = total, data=myframe)
			##THIS SEEMS BEST< BUT NOT WORKING 2015-11-11
			##out.seg<-segmented(lm.out,seg.Z=~period,k=10,psi=list(period=trialstart-1))
			#significance = paste("P-value (segmented regression at time period of ",format(round(davies.out$statistic,digits = 1)),") = ",format(round(davies.out$p.value,digits = 3), nsmall = 3), sep = "")
			#mtext(significance, side=1, line=2, col=KUBlue , cex=1,adj = 1)
			#Cosmetics
			if (coef(sum.sig)[3,1]<0){legend.location="topright"}else{legend.location="topleft"}
			legend(legend.location, legend="Projected rate if no intervention     ",lty=0, lwd = 1, col=KUBlue ,pt.bg=KUBlue, cex=0.75,pt.cex=1.25,pch=1, inset=0.05)
			abline(v=length(myframe$period[Trial=='0'])+0.5,col="gray", lty="dotted")
			text(length(myframe$period[Trial=='0'])+0.5+0.5*strwidth("A"),par("usr")[3]+0.5*strheight("A"),cex=0.8,adj=c(0,0),"Implementation started", font=1,col="gray")
			}

		par(new=TRUE,xpd=NA)
		plot.new()
		#mtext(average, side=1, line=-0.1, col=KUBlue , cex=1,  outer = FALSE)
		mtext(y.label, side=2, line=3.5, col=KUBlue , cex=1.5)
		mtext(topic, side=3, line=2, col=KUBlue, font = 2, cex=3)
		mtext(subtitle, side=3, line=0.8, col=KUBlue , cex=1.2)

		#Logo
		if(theme=="KU"){display_logo(x=1.2,y=0.2)}
		#Goals or targets
		if (goalu >= 0 && goall >= 0)
			{
			regionx = c(0,0,length(period) + 1,length(period) + 1)
			regiony = c(goall,goalu,goalu,goall)
			polygon(regionx,regiony,col=rgb(0,1,0,alpha=0.05),border = NA)
			axis(4,at=c(goall,goalu),labels=c(goall,goalu),col.ticks="green")
			}
		if (grepl("flu", topic, ignore.case = TRUE) > 0 && grepl("vaccin", topic, ignore.case = TRUE) > 0)
			{ #http://www.cdc.gov/flu/fluvaxview/reports/reporti1213/reportii/index.htm
			  # http://www.healthypeople.gov/2020/topicsobjectives2020/objectiveslist.aspx?topicId=23
			if (outcome == "nonconforming")
				{
				abline(a = 0.585, b = 0, col="red", lty = 2, lwd = 2)
				abline(a = 0.2, b = 0, col="green", lty = 2, lwd = 2)
				legend("topright", legend=c("National rate 2012-2013 (41.5%)","Healthy People 2020 goal (80%)"),col=c("red","green"),lty=2, lwd = 2, inset=0.05)
				}
			if (outcome == "conforming")
				{
				abline(a = 0.415, b = 0, col="red", lty = 2, lwd = 2)
				abline(a = 0.8, b = 0, col="green", lty = 2, lwd = 2)
				legend("topleft", legend=c("Healthy People 2020 goal (80%)","National rate 2012-2013 (41.5%)"),col=c("green","red"),lty=2, lwd = 2,  inset=0.05)
				}
			}
		if (grepl("re-admission", topic, ignore.case = TRUE) | grepl("readmission", topic, ignore.case = TRUE))
			{
			abline(a = 0.178, b = 0, col="red", lty = 2)
			abline(a = 0.122, b = 0, col="green", lty = 2)
			legend("topright", legend=c("Medicare (17.8%)","Kaiser (12.2%)"),col=c("red","green"),lty=2, inset=0.05)
			}
		}
	}
	#Shewhart rules start
	if (grepl("SPC", type, ignore.case = TRUE))
		{
		lastvalue = 0
		Trend = 0
		Trend.items <- numeric()
		#myframe$currentvalue <- numeric()
		for(i in 1: length (period))
		{
		# $IHI rules http://www.ihi.org/knowledge/Pages/Tools/RunChart.aspx
		# IHI1: Trend of 5 or more consecutively changing in the same direction
		# if (type=="p" || type=="P") {currentvalue[i] <- myframe$d[i]/myframe$total[i]}
		# if (type=="c" || type=="C") {currentvalue[i] <- myframe$d[i]}
		#HERE 12/2016
	  #stop (paste("i=",i,sep="")
		if(myframe$currentvalue[i] > lastvalue)
			{
			if (Trend < 0) {Trend = 2}
			else {Trend = Trend + 1}
			}
		if(myframe$currentvalue[i] < lastvalue)
			{
			if (Trend > 0) {Trend = - 2}
			else{Trend = Trend - 1}
			}
		if (Trend > 4 || Trend < -4)
			{
			Trend.items <- c(Trend.items, i)
			}
		# IHI2: Run of 6 or more on same size of median
		# Built in so not coded here
		lastvalue = myframe$currentvalue[i]
		}
		#Trend.items

		shewhart <- shewhart.rules(spc, run.length = 6)
			
		#For debugging
		#text (0.5,1,paste("Length of period: ", length (myframe$period)))
		for(i in 1: length (myframe$period))
			{
			#2014-04-01: This causes the last point to be missplaced
			# Cannot easily be fixed with new QCC library
			#points ((i-1)/(length(myframe$period) - 1),myframe$currentvalue[i], col="black",pch=16, cex = 1)
			#For debugging
			#text (row(myframe)[i,1],myframe$currentvalue[i],i,adj = c(0,-1))
			}

		# IHI1: Trend of 5 or more consecutively changing in the same direction
		# Trend.items
		for(i in 1: length (myframe$period))
		{
		if (any(Trend.items == i+1) == TRUE)
			{
			Trend.items <- c(i-3,i-2,i-1,i,Trend.items)
			}
		}

		Trend.items <-   sort(Trend.items)
		Trend.items <- unique(Trend.items)
		for(i in 1: length (Trend.items))
			{
			#points (Trend.items[i],myframe$currentvalue[Trend.items[i]], col="blue",pch=21, cex = 1.5)
			#lines ((Trend.items[i]-1)/(length (myframe$period) - 1),myframe$currentvalue[Trend.items[i]],col="blue",lwd=1.5)
			}
		#Below will work if can adjust for margins top and bottom
		#lines ((Trend.items-1)/(length (myframe$period) - 1),myframe$currentvalue[Trend.items],type="l",col="blue",lwd=1.5)
		#Below empirically derived 2014-09-02
		#Below then removed 2016-04-12
		#lines ((Trend.items-1)/(length (myframe$period) - 1),(myframe$currentvalue[Trend.items] + 0.12) * 0.9,type="l",col="blue",lwd=1.5)

		# IHI2: Run of 6 or more on same size of median
		# shewhart$violating.runs
		vr <- violating.runs(spc, run.length = 6)
		for(i in 1: length (vr))
			{
			#Below will work better if can adjust for margins top and bottom
			#points ((vr[i] - 1)/(length (myframe$period) - 1),myframe$currentvalue[vr[i]], col="orange",pch=16, cex = 1)
			}			

		# IHI4:astronomical points
		for(i in 1: length (shewhart$beyond.limits))
			{
			#Not needed with new library
			#points ((shewhart$beyond.limits[i] - 1)/(length (myframe$period) - 1),myframe$currentvalue[shewhart$beyond.limits[i]], col="red",pch=21, cex = 2)
			}
		#Shewhart rules end
		}
}
