#### installation packages ####
install.packages("XML")
install.packages("wordcloud")
install.packages("tm")
install.packages("rvest")
install.packages("stringr")
install.packages("SnowballC")
install.packages("data.table")
install.packages("TTR")
install.packages("zoo")

library("wordcloud")
library("tm")
library(XML)
library("stringr")
library("SnowballC")
library(RCurl)
library(plotrix)
library("zoo")
library("TTR")
library(data.table)
library(httr)


#### Fonctions ####

getrating=function(entreprise,nb){
  notes=NULL
  for (i in 1:nb){
    doc.html <- htmlTreeParse(getURL(paste0("https://www.opinion-assurances.fr/assureur-",
                                            entreprise,"-page",i,".html") ),useInternal = TRUE)
    #doc.html <- htmlTreeParse(xData,useInternal = TRUE)
    #doc.text = unlist(xpathApply(doc.html, '//h2', xmlValue))
    doc.note = unlist(xpathApply(doc.html, "//span[@itemprop='ratingvalue']", xmlValue))
    doc.dateheure = unlist(xpathApply(doc.html, "//span", xmlGetAttr,"datetime"))
    notes=rbind(as.data.frame(notes),cbind(doc.dateheure,doc.note))
  }
  notes=data.table(notes)
  notes[,annee:=substr(doc.dateheure,1,4)]
  notes[,mois:=substr(doc.dateheure,6,7)]
  notes[,heure:=substr(doc.dateheure,12,13)]
  notes[,note:=as.numeric(as.character(doc.note))]
  evol=notes[,list(.N,moyenne=mean(note),vol=sd(note)),by=list(annee,mois)]
  setorder(evol,annee,mois)
  #nb=notes[,list(.N),by=list(annee,mois)]
  #setorder(nb,annee,mois)
  
  dates=data.table(seq(as.Date(paste0(evol[1,annee],"-",evol[1,mois],"-15")),
                       as.Date(paste0(evol[nrow(evol),annee],"-",evol[nrow(evol),mois],"-15")),by='months'))
  dates[,annee:=substr(V1,1,4)]
  dates[,mois:=substr(V1,6,7)]
  
  evol=merge(evol,dates[,list(annee,mois)],by=c("annee","mois"),all=TRUE)
  return (list(notes=notes,evol=evol))
}

ma=function(evol){
  for (i in 1:nrow(evol)){
    if(is.na(evol[i,N])==TRUE){
      evol[i,N:=0]
    }
  }
  c=1
  while (is.na(evol[c,moyenne])==TRUE){
    c=c+1
  }
  for (i in c:nrow(evol)){
    if(is.na(evol[i,moyenne])==TRUE){
      evol[i,moyenne:=evol[i-1,moyenne]]
    }
  }
  m=c(rep(NA,c-1),VWMA(evol[,moyenne][c:nrow(evol)],evol[,N][c:nrow(evol)],6))
  return (m)
}

plotevol=function(evol,titre){
  
  for (i in 1:nrow(evol)){
    if(is.na(evol[i,vol])==TRUE){
      if(i==1){
        evol[i,vol:=evol[i+1,vol]]
      }else{
        evol[i,vol:=evol[i-1,vol]]
      }
    }
  }
  
  #ma=c(rep(NA,c),VWMA(evol[,moyenne][c:nrow(evol)],evol[,N],6))
  ma=ma(evol)
  ## add extra space to right margin of plot within frame
  par(mar=c(4, 4, 4, 4)+0.1)
  ## Plot first set of data and draw its axis
  plot(1:nrow(evol),evol[,N], pch=16, axes=FALSE, xlab="", ylab="", 
       ylim=c(0,max(evol[,N])),type="b",col="black",
       main=paste("Evolution des notes",titre))
  axis(2,ylim=c(0,max(evol[,N])),col="black",las=1)  ## las=1 makes horizontal labels
  mtext("Nombre de notes moyen par mois",side=2,line=2.5)
  box()
  
  ## Allow a second plot on the same graph
  par(new=TRUE)
  
  ## Plot the second plot and put axis scale on right
  plot(1:nrow(evol),evol[,moyenne], pch=15,  xlab="", ylab="", ylim=c(0,5), 
       axes=FALSE, type="b", col="darksalmon")
  ## a little farther out (line=4) to make room for labels
  mtext("Note moyenne par mois",side=4,col="red",line=2) 
  axis(4, ylim=c(0,5), col="red",col.axis="red",las=1)
  points(1:nrow(evol),ma,col=2,type="l",lwd=3)
  
  ## Add Legend
  #legend("topleft",legend=c("Beta Gal","Cell Density"),
  #      text.col=c("black","red"),pch=c(16,15),col=c("black","red"))
  xat=seq(1,nrow(evol),trunc(nrow(evol)/30)+1)
  axis(1, at=xat, labels=paste(evol[,annee],evol[,mois])[xat],las=2)
}

plotidc=function(evol,titre){
  #ma(evol[,moyenne],5)
  for (i in 1:nrow(evol)){
    if(is.na(evol[i,moyenne])==TRUE){
      if(i==1){
        evol[i,moyenne:=evol[i+1,moyenne]]
      }else{
        evol[i,moyenne:=evol[i-1,moyenne]]
      }
    }
  }
  
  for (i in 1:nrow(evol)){
    if(is.na(evol[i,N])==TRUE){
      evol[i,N:=1]
    }
  }
  
  for (i in 1:nrow(evol)){
    if(is.na(evol[i,vol])==TRUE){
      if(i==1){
        evol[i,vol:=evol[i+1,vol]]
      }else{
        evol[i,vol:=evol[i-1,vol]]
      }
    }
  }
  
  ma=EVWMA(evol[,moyenne],evol[,N],5)
  plot(1:nrow(evol),ma,ylim=c(0,5),xaxt="n",xlab=" ",
       ylab="Notes par mois",type="b",
       main=paste("Evolution des notes",titre))
  sup=EVWMA(ma+1.96*EVWMA(evol[,vol],evol[,N],5)/evol[,N],evol[,N],5)
  inf=EVWMA(ma-1.96*EVWMA(evol[,vol],evol[,N],5)/evol[,N],evol[,N],5)
  points(1:nrow(evol),sup,col=2,type="l")
  points(1:nrow(evol),inf,col=3,type="l")
  
  xat=seq(1,nrow(evol),trunc(nrow(evol)/30)+1)
  axis(1, at=xat, labels=paste(evol[,annee],evol[,mois])[xat],las=2)
}

comparaison=function(p1,p2,e1,e2){
  t=merge(p1,p2,by=c("annee","mois"),all=TRUE)
  evol=t
  ma.x=ma(data.table(moyenne=t[,moyenne.x],N=t[,N.x]))
  ma.y=ma(data.table(moyenne=t[,moyenne.y],N=t[,N.y]))
  
  plot(t[,moyenne.x],type="b",ylim=c(0,5),xaxt="n",xlab=" ",ylab="Notes par mois",
       main="Comparaison de 2 produits")
  points(1:nrow(t),t[,moyenne.y],col=2,type="b")
  
  points(1:nrow(t),ma.x,col=2,type="l",lwd=2)
  points(1:nrow(t),ma.y,col=3,type="l",lwd=2)
  
  xat=seq(1,nrow(t),trunc(nrow(t)/30)+1)
  axis(1, at=xat, labels=paste(t[,annee],t[,mois])[xat],las=2)
  legend(x=1,y=5,c(e1,e2),col=c(1,2),pch=1)
}


#### calcul moyenne simple test ####
creditm.auto=getrating("credit-mutuel-assurance-auto",13)

evol=creditm.auto$evol
notes=creditm.auto$notes

notes=gmf.auto$notes
notes=macif.auto$notes
table.n=as.data.frame(table(notes[,doc.note]))
sum(as.numeric(as.character(table.n[,1]))*table.n[,2])/sum(table.n[,2])
sum(evol[,N]*evol[,moyenne])/sum(evol[,N])


#### résultats différentes entreprises ####
# sortie graphique
creditm.auto=getrating("credit-mutuel-assurance-auto",13)
png("cm.auto.png")
plotevol(creditm.auto$evol,"Crédit Mutuel auto")
dev.off()

gmf.auto=getrating("gmf-assurance-auto",30)
# sortie graphique
png("gmf.auto.png")
plotevol(gmf.auto$evol,"GMF auto")
dev.off()

matmut.auto=getrating("matmut-assurance-auto",22)
# sortie graphique
png("matmut.auto.png")
plotevol(matmut.auto$evol,"MatMut auto")
dev.off()

matmut=getrating("matmut",32)
# sortie graphique
png("matmut.png")
plotevol(matmut$evol,"MatMut")
dev.off()

macif.auto=getrating("macif-assurance-auto",42)
# sortie graphique
png("macif_auto.png")
plotevol(macif.auto$evol,"Macif Auto")
dev.off()

olivier.auto=getrating("olivier-assurances-assurance-auto",11)
# sortie graphique
png("olivier_auto.png")
plotevol(olivier.auto$evol,"L'Olivier Auto")
dev.off()

da.auto=getrating("direct-assurance-assurance-auto",52)
# sortie graphique
png("da_auto.png")
plotevol(da.auto$evol,"DA Auto")
dev.off()


#### Comparaison des entreprises ####
macif.auto$evol
olivier.auto$evol
da.auto$evol


# sortie graphique
png(filename = "macifvolivier_complet.png")
comparaison(macif.auto$evol,olivier.auto$evol,"MACIF Auto","L'Olivier Auto")
dev.off()

# sortie graphique
png(filename = "macif_vs_da_complet.png")
comparaison(macif.auto$evol,da.auto$evol,"MACIF Auto","DA Auto")
dev.off()

