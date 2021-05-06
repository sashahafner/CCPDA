
# logaxis
# Function for adding tickmarks and labels to logrithmic plot axes.
# Author: S. Hafner
# Created: 2007 NOV 16
# Last modified: 2012 JULY 17
logaxis<-function(side=1,amin=10^par('usr')[2*(2-(side%%2))-1],amax=10^par('usr')[2*(2-(side%%2))],
                 labels=TRUE,num.form='e',tclj=0.5,tclm=0.2,box=TRUE,mgp=c(3,1,0),mult=1,las=1,cex=1.0) {
   amin<-mult*amin
   amax<-mult*amax
   if(amin<=0) stop('Error, expect amin>0, got',amin)
   llmin<-ceiling(log10(amin)) #The log of the labled minimum--the smallest labeled tickmark
   llmax<-floor(log10(amax))   #The log of the labled maximum--the largest labeled tickmark
   axis(side,at=10^c(llmin:llmax)/mult,tcl=tclj,labels=FALSE,las=las,mgp=mgp) #First add major tickmarks, one per decade
   if (llmin != llmax & labels == TRUE) {
      for(i in llmin:llmax) {
         #Add major tickmark labels
         if(num.form == 'e') {
           axis(side,at=10^i/mult,labels=as.expression(substitute(10^j,list(j=i))),tcl=tclj,cex.axis=cex,las=las,mgp=mgp)
        } else axis(side,at=10^i/mult,labels=10^i,tcl=tclj,cex.axis=cex,las=las,mgp=mgp)
      }
   }
   if (llmin != llmax) {
      for (i in llmin:(llmax-1)) {
         axis(side,at=10^i*c(2:9)/mult,labels=FALSE,tcl=tclm,las=las,mgp=mgp) #Add minor tickmarks
      }
   }
   addl<-floor(10*(10^llmin - amin)/10^llmin) #The number of minor tickmarks needed below the smallest labeled tickmark
   axis(side,at=10^llmin*(1+c(-addl:0)/10)/mult,labels=FALSE,tcl=tclm,las=las,mgp=mgp) #Add these extra tick marks below the smallest labeled tickmark
   addu<-floor((amax-10^llmax)/10^llmax) #The number of major tickmarks needed above the largest labeled tickmark
   axis(side,at=10^llmax*(1+c(0:addu))/mult,labels=FALSE,tcl=tclm,las=las,mgp=mgp) #Add these extra tickmarks above the largest labeled tickmark
   if (box==TRUE) box() #Add a box around plot
}


