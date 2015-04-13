library(shiny)
library(xlsx)
library(plyr)
library(devtools)
#library(RCurl)
#install_github("Rakari/SirKR")
library(SirKR)

shinyServer(function(input, output) {
        output$algaeplot <- renderPlot({
                inFile <- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                data=read.xlsx(inFile$datapath,1,header=T,as.data.frame=T) 
                end=ncol(data)
                #Extract duplicates of same elements in first column
                height=unique(data[,1])
                data=ddply(data,names(data)[1],colwise(meanNA))
                data=clean(data)
                height=data[,1]
                maxdata=max(data[,2:end])
                
                if(input$method == "individ") {
                        data=cbind(height,data[,2:end]/maxdata)
                }
                
                translation=seq(1,1+3*(end-2),3)
                graph=t(t(data[,2:end])+translation)
                graphmirror=t(translation-t(data[,2:end]))
                graphdata=cbind(graph,graphmirror)
                ylim=0.125*max(height)+1
                cex=0.7 
                
                matplot(graphdata,0.125*height,type="l",col=rep("black",ncol(graphdata)), ylim=c(0,ylim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex,main = input$text_field)
                mtext(names(data[2:end]),side=1,at=translation,cex=cex)
                legendline=c(tail(translation,1)-2,tail(translation,1))                        
                lines(legendline,c(ylim-0.25,ylim-0.25))
                if(input$method == "prop") {
                        text(x=tail(translation,1)-1,y=ylim,"100%",cex=cex)
                } else {
                        text(x=tail(translation,1)-1,y=ylim,paste(maxdata," individuals"),cex=cex)   
                }
        }) 
        
        output$downloadPlot <- downloadHandler(
                filename=paste(input$file1, ".pdf", sep=""),
                content=function(file=NULL) {
                        pdf(file=file,height=5,width=7+1)  
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        
                        data=read.xlsx(inFile$datapath,1,header=T,as.data.frame=T) 
                        end=ncol(data)
                        #Extract duplicates of same elements in first column
                        height=unique(data[,1])
                        data=ddply(data,names(data)[1],colwise(meanNA))
                        maxdata=max(data[,2:end])
                        
                        if(input$method == "individ") {
                                data=cbind(height,data[,2:end]/maxdata)
                        }
                        
                        translation=seq(1,1+3*(end-2),3)
                        graph=t(t(data[,2:end])+translation)
                        graphmirror=t(translation-t(data[,2:end]))
                        graphdata=cbind(graph,graphmirror)
                        ylim=0.25*max(height)+1
                        cex=0.7 
                        matplot(graphdata,0.25*height,type="l",col=rep("black",ncol(graphdata)), ylim=c(0,ylim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex,main = input$text_field)
                        mtext(names(data[2:end]),side=1,at=translation,cex=cex)
                        legendline=c(tail(translation,1)-2,tail(translation,1))                        
                        lines(legendline,c(ylim-0.25,ylim-0.25))
                        if(input$method == "prop") {
                                text(x=tail(translation,1)-1,y=ylim,"100%",cex=cex)
                        } else {
                                text(x=tail(translation,1)-1,y=ylim,paste(maxdata," individuals"),cex=cex)   
                        }
                        dev.off()
                }
        )

})
