ishikawa <-
function(content) {
library(qcc)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue,bg=SkyBlue,xaxt="s",yaxt="s",mai=c(.956,.769,.769,.394),pin=c(14,14))
cause.and.effect(cause=content,title="",effect="Nonconformity",cex = c(1, 1, 0.9), font = c(2, 3, 2))
mtext("Cause and effect diagram", side=3,line=2,col=KUBlue,font=2, cex=1.3)
mtext("(Ishikawa or fish-bone diagram)", side=3,line=1,col=KUBlue,font=2, cex=1)
}
