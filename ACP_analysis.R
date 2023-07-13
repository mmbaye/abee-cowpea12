suppressPackageStartupMessages({
        library(factoextra)
        library(FactoMineR)
        library(FactoInvestigate)
        library(FactoInvestigate)
        library(ggpubr)
        library(tidyverse)
        library(readxl)
})


-------------------------------------------------------------
colors=c("green","blue","red")



df_pca <- PCA(df[,-5], graph = T)
p<-fviz_eig(df_pca)
p
res<-get_eig(df_pca)[c(1:3),]

res<-as.data.frame(res)
write.csv(res, 'Results/res_acp_variance.csv')


myplots_ind<-function(compx=1, compy=2){
        a<-fviz_pca_ind(df_pca, 
                        axes = c(compx,compy),
                        geom.ind = c('point'),
                        fill.ind = df$types,
                        col.ind = df$types,
                        palette = colors,
                        addEllipses = T, 
                        legend.title = "Genotype",
                        repel=T)+
                theme_minimal()  + 
                theme(panel.grid = element_blank())+ 
                scale_shape_manual(values=seq(15,19))
        return(a)
}
myplots_var<-function(compx=1, compy=2){
        b<-fviz_pca_var(df_pca,
                        geom.var = c("arrow"),
                        axes=c(compx,compy),
                        col.var = "black",
                        labelsize = 5,
                        alpha.var=0.2,
                        repel=T)+
                theme_minimal()
        return(b)
}


p1<-myplots_ind()
p2<-myplots_ind(compx = 1,compy = 3)

p3<-myplots_var()

p1
p2
p3
ggarrange(p1,p2,p3,p)


ggsave('Figures/fig1.tiff',width = 7,height = 5)
