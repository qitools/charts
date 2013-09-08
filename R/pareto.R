pareto <-
function(content,type,plotbgcolor) {
temp <- gsub('\n', '', fixed = TRUE, content, perl = TRUE)
temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=2, byrow=TRUE,dimnames = list(NULL, c("Reason","count")))')
x<-eval(parse(file = "", n = NULL, text = temp))
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue) #bg=SkyBlue)
if (type=="p" || type=="P")
	{
	count <- as.numeric(x [,2])
	names(count) <- x [,1]
	pareto.chart(count , ylab = "Frequency", col=SkyBlue, cumperc = seq(0, 100, by = 10), xlab="", border=KUBlue,main="Pareto chart for non-conformity")
	mtext("Reasons", side=1, line=3, col=KUBlue , cex=1.5)
	}
else
	{
	Myframe <- as.data.frame(x)
	Myframe$count<-as.numeric(as.character(Myframe$count))
	ggplot(Myframe, aes(x = reorder(Reason, -count), y = count)) + 
		geom_bar(fill = KUBlue,stat="identity") + xlab("Reason")+ ylab("Count") +
		theme(plot.background = element_rect(fill = SkyBlue)) + 
		labs(title = "Frequency of barriers")+ theme(plot.title = element_text(size = rel(2))) + theme(plot.title = element_text(colour = KUBlue))  +
		 theme(axis.text = element_text(colour = KUBlue))+
		 theme(axis.title = element_text(colour = KUBlue))
	}
}
