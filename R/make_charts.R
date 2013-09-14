make_charts <- function(content, topic, outcome, timeperiod, type, theme) {
temp <- content
#temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub(',', '","', fixed = TRUE, temp, perl = TRUE)
temp <- paste('"',temp,'"')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=4, byrow=TRUE, dimnames = list(NULL, c("period", "count", "total","Trial")))')
x<-eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
myframe$period<-gsub("\'", '', fixed = TRUE, myframe$period, perl = TRUE)
myframe$period<-str_trim(myframe$period)
myframe$count<-as.numeric(as.character(str_trim(myframe$count)))
myframe$total<-as.numeric(as.character(str_trim(myframe$total)))
#myframe$Trial<-as.logical(str_trim(myframe$Trial))
myframe$Trial<-str_trim(myframe$Trial)

attach(myframe)
# http://www.identity.ku.edu/colors/index.shtml
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue)
qcc.options(cex.stats=1, cex.stats=0.9, bg.margin=SkyBlue)
sequential = FALSE
for(i in 1: length(myframe$period))
{
if(Trial[i]>0 || Trial[i] == TRUE)
	{
	sequential = TRUE
	}
else
	{
	}
Trial[i] =as.logical(Trial[i])
#Below done to avoid handling strings such as "Jan, 2013"
#myframe$period[i] <- i
}

attach(myframe)

if (type == "r" || type == "R"){sequential = FALSE}

if (sequential == FALSE)
	{
	if (type == "p" || type == "P")
		{
		baseline <- qcc(count,sizes=total,type="p", xlab="",ylab="",title="",labels=myframe$period,ylim=c(0,1), digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
		mtext(paste("Proportion of encounters ", outcome), side=2, line=2.5, col=KUBlue , cex=1.5)
		average = paste("Average ",outcome," = ",round(baseline$center*100,digits = 1),"%", sep = "")
		if(theme=="KU"){display_logo(x=1.2,y=0.2)}
		}
	else 
		{
		if (type == "r" || type == "R") #Run chart
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
			axis(side=1,at=1:length(myframe$period),labels=myframe$period)
			axis(2,  xaxp=c(0,1,10))
			box()
			mtext(paste("Proportion of encounters ", outcome), side=2, line=2.5, col=KUBlue , cex=1.5)
			average = paste("Average ",outcome," = ",round(100*numerator/denominator,digits = 1),"%", collapse = NULL)
			if(theme=="KU"){display_logo(x=1.15,y=0.06)}
			}
		else #c-chart
			{
			baseline <- qcc(count,type="c", xlab="",ylab="",title="",labels=myframe$period, digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
			mtext(paste("Count of encounters ", outcome), side=2, line=2.5, col=KUBlue , cex=1.5)
			average = paste("Average ",outcome," = ",round(baseline$center,digits = 1),"", sep = "")
			}
		}
	if (type=="R" || type=="r")
		{
		mtext(timeperiod, side=1, line=3, col=KUBlue , cex=1.5)
		mtext(average, side=1, line=4.5, col=KUBlue , cex=1)
		}
	else
		{
		mtext(timeperiod, side=1, line=-1.0, col=KUBlue , cex=1.5)
		mtext(average, side=1, line=-0.2, col=KUBlue , cex=1)
		}
	mtext(topic, side=3,line=2.5,col=KUBlue,font=2, cex=1.3)
	}
else #sequential == TRUE
	{
	if (type == "b" || type == "B")
		{
		cella = 0
		cellb = 0
		cellc = 0
		celld = 0
		for(i in 1: length (period))
			{
			if (!Trial[i])
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
		boxplot(count/total ~ Trial,ylab=paste("Mean proportion of encounters ", outcome),main=topic, ylim=c(0,1),names.arg=c("Baseline","Trial"))
		axis(1, at= 1:2, lab=c("Baseline","Trial"),tick=FALSE)
		mtext(side=3,line=0.5,"(before and after analysis)", font=1)
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
		if (type == "p" || type == "P")
			{
			sequential <- qcc(count[Trial==0],sizes=total[Trial==0],newdata=count[Trial==1], newsizes=total[Trial==1],type="p", xlab="",ylab="",title="",labels=period[Trial==0],newlabels=period[Trial==1],ylim=c(0,1), digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
			mtext(paste("Proportion of encounters ",outcome), side=2, line=2.5, col=KUBlue , cex=1.5)
			mtext(side=3,line=1,paste("proportion of encounters ", outcome, " (p chart): before-after trial"), font=2)
			#mtext(side=3,line=1,"count "~italic(outcome)~" of encounters (p chart): before-after trial", font=2)
			average = paste("Average (pretrial) = ",round(sequential$center*100,digits = 1),"%", sep = "")
			##Sig testing
			#Linear regression
			glm.out1=glm(count/total ~ Trial + period, family=binomial(logit),weights = total)
			}
		if (type == "c" || type == "C")
			{
			sequential <- qcc(count[Trial==0],newdata=count[Trial==1],type="c", xlab="",ylab="",title="",labels=period[Trial==0],newlabels=period[Trial==1], digits=2,nsigmas=3,chart.all=TRUE,add.stats=TRUE)
			mtext("Count of encounters non-conforming", side=2, line=2.5, col=KUBlue , cex=1.5)
			mtext(side=3,line=1,paste("Count of encounters ", outcome, " (c chart): before-after trial"), font=2)
			#mtext(side=3,line=1,"count "~italic(outcome)~" of encounters (c chart): before-after trial", font=2)
			average = paste("Average (pretrial) = ",round(sequential$center,digits = 1),"", sep = "")
			##Sig testing
			#Linear regression
			glm.out1=glm(count ~ Trial + period, family=poisson(log))
			}
		mtext(topic, side=3,line=2.5,col=KUBlue,font=2, cex=1.3)
		mtext(timeperiod, side=1, line=-1.0, col=KUBlue , cex=1.5)
		mtext(average, side=1, line=-0.2, col=KUBlue , cex=1)

		sum.sig <- summary(glm.out1)
		#coef(sum.sig)["Trial",4] or coef(sum.sig)[2,4]
		#plot(period, count/total,xlab="", ylim=c(0,1))
		#mtext(period, side=1, line=2, col=KUBlue , cex=1)
		#lines(period, glm.out1$fitted,type="l",col="red")
		significance = paste("P-value for trial (linear regression) = ",format(round(coef(sum.sig)["Trial",4],digits = 3), nsmall = 3), sep = "")
		mtext(significance, side=1, line=0.5, col=KUBlue , cex=1,adj = 1)
		significance = paste("P-value for period (linear regression) = ",format(round(coef(sum.sig)["period",4],digits = 3), nsmall = 3), sep = "")
		mtext(significance, side=1, line=1.5, col=KUBlue , cex=1,adj = 1)
		if(theme=="KU"){display_logo(x=1.2,y=0.2)}
		}
	}
}
