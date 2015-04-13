algaeplot<-function(excelfile,method="prop",unit="",interval=0.25){     
        library(xlsx)
        library(plyr)
        library(devtools)
        library(RCurl)
        install_github("Rakari/SirKR")
        library(SirKR)
        data=read.xlsx(excelfile,1,header=T,as.data.frame=T)
        end=ncol(data)
        #Extract duplicates of same elements in first column
        height=unique(data[,1])  
        data=ddply(data,names(data)[1],colwise(meanNA))
        data=clean(data)
        height=data[,1]
        maxdata=max(data[,2:end])
        
        if(method == "prop"){
                data=cbind(height,data[,2:end]/100)
        } else{
                data=cbind(height,data[,2:end]/maxdata)
                
        }
        
        translation=seq(1,1+3*(end-2),3)
        graph=t(t(data[,2:end])+translation)
        graphmirror=t(translation-t(data[,2:end]))
        graphdata=cbind(graph,graphmirror)
        ylim=interval*max(height)+1
        cex=0.7
        pdf("algaeplot.pdf",height=4,width=end+1)
        matplot(graphdata,interval*height,type="l",col=rep("black",ncol(graphdata)), ylim=c(0,ylim),lty=rep(1,ncol(graphdata)),xlab="",ylab="height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex)
        mtext(names(data[2:end]),side=1,at=translation,cex=cex)
        legendline=c(tail(translation,1)-2,tail(translation,1))
        lines(legendline,c(ylim-0.375,ylim-0.375))
        
        if(method == "prop"){
                text(x=tail(translation,1)-1,y=ylim,"100%",cex=cex)
        }
        if(method == "individ"){
                text(x=tail(translation,1)-1,y=ylim,paste(maxdata," individuals"),cex=cex)   
        }
        if(method == "biomass"){
                legend = ""
                if(unit == "" | unit == "1")  {
                        legend = bquote(.(paste(maxdata," g/","m", sep=""))^2)
                } else {
                        legend = bquote(.(paste(maxdata," g/",unit, "m", sep=""))^2)
                }
                
                unit=unit
                text(x=tail(translation,1)-1,y=ylim,bquote(.(legend)),cex=cex)     
        }
        dev.off()
}
#segments(x0, y0, x1 = x0, y1 = y0,
#polygon(x, y)
#snow and parallel packages
#knitr
#RStudio server
#xlab=paste(names(data[2:end]),collapse="         ")
#axis(1, at = translation, labels = names(data[,2:end]), cex.axis = 0.7)
#xlim=c(0,max(translation)+3)