rm(list=ls());gc()
setwd('C:/Users/FengC/R')
lapply(c('openxlsx','data.table','glmnet','Cairo','ggplot2','patchwork','ggrastr'), require, character.only = TRUE)
dat.sheets<-sheets(loadWorkbook('Supplementary Data II.xlsx'))
m.list<-structure(lapply(1:length(dat.sheets),function(i) read.xlsx('Supplementary Data II.xlsx',i)),names=dat.sheets)
m.list<-m.list[unlist(lapply(m.list,ncol))>20]
lapply(m.list,function(x) print(x[1:4,1:4]))
m.list<-structure(lapply(m.list,function(x){isotopomers<-x[1,-1];transitions<-x[-1,1];x<-as.matrix(x[-1,][,-1]);colnames(x)<-as.character(isotopomers);rownames(x)<-transitions;class(x)<-'numeric';x}),
                  names=unlist(lapply(m.list,function(x) paste(ifelse(grepl('glutamate',colnames(x)[1]),'glutamate','aspartate'),ifelse(grepl('without',colnames(x)[1]),'without correction','with correction')))))
err.list<-list()
g.list<-list()
for(D.name in names(m.list)){
  print(D.name)
  D<-m.list[[D.name]]
  
  set.seed(12345)
  input.mat<-do.call(cbind,lapply(1:5000, function(i){
    z<-rbeta(16,2,5)
    y<- runif(2, 0.0001, 0.02)
    e<-y[1]
    f<-y[2]
    x <- c (z [1:8], z [9:16]*e, z [9:16]*f, z [9:16])
    x/sum(x)
  }))  
  glmnet.err<-apply(input.mat, 2, function(test){
    x <- D%*%test
    cal.isotopomer <-coef(glmnet(D, x, lambda = 0, lower.limits = 0, intercept = FALSE, thresh = 1e-30,upper.limits = 1))[-1]
    
    cal.isotopomer-test
  })
  
  test.plot<-function(test.err){
    ggplot(data.table(isotopomer=colnames(D),value=as.numeric(as.matrix(test.err))),aes(x=value,y=isotopomer))+
      geom_point_rast(alpha=.1)+geom_boxplot(outlier.shape = NA,alpha=.5)+theme_bw()
  }
  g.list[[D.name]]<-test.plot(glmnet.err)+ggtitle(D.name)
  rownames(glmnet.err)<-colnames(D)
  err.list[[D.name]]<-round(t(apply(glmnet.err,1,function(x) quantile(x,c(.025,.5,.975)))),3)
  err.list[[D.name]]<-data.table(isotopomer=rownames(err.list[[D.name]]),err.list[[D.name]])
}
CairoPNG('err_estimation_beta.png',w=930,h=1200,res = 150)
wrap_plots(g.list)
dev.off()
wb<-createWorkbook()
for(n in names(err.list)){addWorksheet(wb,n);writeDataTable(wb,n,as.data.frame(err.list[[n]]))}
saveWorkbook(wb,'glmnet_err_beta.xlsx',overwrite = T)

