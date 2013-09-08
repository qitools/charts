ishikawa <-
function(content,theme) {
KUBlue = "#0022B4"
#SkyBlue = "#6DC6E7"
temp <- gsub('\n', '', fixed = TRUE, content, perl = TRUE)
temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- paste('list(',temp,')')
causes<-eval(parse(file = "", n = NULL, text = temp))
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue,bg="#FFFFFF",xaxt="s",yaxt="s",mai=c(.956,.769,.769,.394),pin=c(14,14))
cause.and.effect(cause=causes,title="",effect="Nonconformity",cex = c(1, 1, 0.9), font = c(2, 3, 2))
mtext("Cause and effect diagram",        side=3,line=1,col=KUBlue,font=2, cex=1.3)
mtext("(Ishikawa or fish-bone diagram)", side=3,line=0,col=KUBlue,font=2, cex=1)
if(theme=="KU"){display_logo(x=1.2,y=0.05)}
}
