############
#1. Bacteria from soil vs Ralstonia in vitro GFP
############
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}

#####this is a excel with a spreadsheet for each time point h0, h2, ....etc the col names are Rep col1 col2 ...

######file with GFP measurements
setwd("..")
table=read_excel_allsheets("GFP.xlsx")
table<-as.list(table)

new_table<-list()

for(i in names(table))  {print (i);
new_table[[i]]=table[[i]]
##new_table=as.data.frame(new_table);
for(j in colnames(table[[i]][,c(3:13)])) {print (j); 
blank=table[[i]]$col12
  new_table[[i]][[j]]=table[[i]][[j]]-blank;
print(new_table[[i]][[j]])
new_table[[i]]$Rep<-table[[i]]$Rep}}

#####as above 4 first rows is one treatment and 4 last rows other
library(data.table)

subsetting<-list()
for(i in names(new_table))  {print (i);
for(j in colnames(table[[i]])){
subsetting[[i]][[j]]=cbind(new_table[[i]][[j]][c(1:4)])}}
library(reshape)
data <- merge_recurse(subsetting)
times=names(new_table)
data$times=rep(times,each=4)
repetition=c(1,2,3,4)
data$replicate=rep(repetition)
write.table(data,"data_formatted_GFP.txt",sep="\t")
data$time_rep=paste(data$times,data$replicate,sep="_")


subsetting<-list()
for(i in names(new_table))  {print (i);
for(j in colnames(table[[i]])){
subsetting[[i]][[j]]=cbind(new_table[[i]][[j]][c(5:8)])}}
library(reshape)
data2 <- merge_recurse(subsetting)
times=names(new_table)
data2$times=rep(times,each=4)
repetition=c(1,2,3,4)
data2$replicate=rep(repetition)
write.table(data,"data_formatted_2_GFP.txt",sep="\t")
colnames(data2)=paste(colnames(data2),"Rs",sep = '_')
data2$time_rep=paste(data$times,data$replicate,sep="_")

####

library(tidyverse)
data_final=left_join(data,data2,by="time_rep")

list_final<-list()
for(i in colnames (data_final)){
list_final[[i]]<- melt(setDT(data_final), id.vars = c("times","replicate"),variable.name =  i)}
list_final_data <- merge_recurse(list_final)
list_final_data2=list_final_data[grep("col", list_final_data$variable),] 
 dim(list_final_data)

 dim(list_final_data2)

 write.table(list_final_data2,"long_data_formatted_GFP.txt",sep="\t")

list_final_data3=list_final_data2
list_final_data3$times=str_replace_all(list_final_data3$times,"h","")
list_final_data3$times=as.numeric(list_final_data3$times)
list_final_data3$value=as.numeric(list_final_data3$value)

list_final_data4=list_final_data3
list_final_data4$times=sub("h","",list_final_data3$times)
list_final_data4$variable  <- case_when(list_final_data4$variable  == "col2" ~ "Rs.UW551",
                                    list_final_data4$variable  == "col3" ~ "Bacillus.B12",
                                    list_final_data4$variable == "col4" ~ "Pseudomonas.P19",
                                    list_final_data4$variable  == "col5" ~ "Pseudomonas.P91",
                                    list_final_data4$variable  == "col6" ~ "Rhodanobacter.R55",
                                    list_final_data4$variable  == "col7" ~ "Rhodanobacter.R68",
                                    list_final_data4$variable  == "col8" ~ "Burkholderia.B12",
                                    list_final_data4$variable  == "col9" ~ "Pandoraea.B14",
                                    list_final_data4$variable  == "col10" ~ "Burkholderia.PB10",
                                    list_final_data4$variable  == "col11" ~ "Burkholderia.PB18",
                               
                                    list_final_data4$variable  == "col2_Rs" ~ "Rs.UW551_Rs.gfp",
                                    list_final_data4$variable  == "col3_Rs" ~ "Bacillus.B12_Rs.gfp",
                                    list_final_data4$variable  == "col4_Rs" ~ "Pseudomonas.P19_Rs.gfp",
                                    list_final_data4$variable  == "col5_Rs" ~ "Pseudomonas.P91_Rs.gfp",
                                    list_final_data4$variable  == "col6_Rs" ~ "Rhodanobacter.R55_Rs.gfp",
                                    list_final_data4$variable == "col7_Rs" ~ "Rhodanobacter.R68_Rs.gfp",
                                    list_final_data4$variable  == "col8_Rs" ~ "Burkholderia.B12_Rs.gfp",
                                    list_final_data4$variable  == "col9_Rs" ~ "Pandoraea.B14_Rs.gfp",
                                    list_final_data4$variable  == "col10_Rs" ~ "Burkholderia.PB10_Rs.gfp",
                                    list_final_data4$variable == "col11_Rs" ~ "Burkholderia.PB18_Rs.gfp")

list_final_data4$times=as.numeric(list_final_data4$times)
colnames(list_final_data4)=sub("variable","Treatment",colnames(list_final_data4))
tiff("curves1_GFP.tiff",res=300,he=200,wi=200,units="mm")

 ggplot(data = list_final_data4, aes(x = times, y = value, group = Treatment, col = Treatment)) +
   geom_line(linewidth=2) +
   labs(x = "Time",
          y = "GFP") +
   facet_wrap(.~Treatment) +
  ggtitle("Growth 48h")+
   theme_bw()+
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank()) +
theme(strip.text.x = element_text(size = 6))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 6))
dev.off()


tiff("curves_2_GPF.tiff",res=300,he=200,wi=200,units="mm")
 ggplot(data = list_final_data4, aes(x = times, y = value, group = Treatment, col = Treatment))  +
   geom_line(linewidth=1) +
   labs(x = "Time",
          y = "GFP") +
  ggtitle("Growth 48h")+
    theme_bw()+
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank()) +
theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 20))
dev.off()
library(pROC)
library(DescTools)
# this uses a loop. Each interation of the loop will make a subset of the data for 1 curve, then calculates the AUC from it.

list_final_data5=subset(list_final_data4,list_final_data4$replicate==1)
data_list <- split(list_final_data5, f = list_final_data5$Treatment)
data_list2 = data_list[which(lapply(data_list, nrow) != 0)]
data_AUC_1<-list()
for(i in names(data_list2)){print(i);
  data_AUC_1[[i]] <- AUC(data_list2[[i]]$times, data_list2[[i]]$value)
}
 list_final_data5=subset(list_final_data4,list_final_data4$replicate==2)
data_list <- split(list_final_data5, f = list_final_data5$Treatment)
data_list2 = data_list[which(lapply(data_list, nrow) != 0)]
data_AUC_2<-list()
for(i in names(data_list2)){print(i);
  data_AUC_2[[i]] <- AUC(data_list2[[i]]$times, data_list2[[i]]$value)
} 
list_final_data5=subset(list_final_data4,list_final_data4$replicate==3)
data_list <- split(list_final_data5, f = list_final_data5$Treatment)
data_list2 = data_list[which(lapply(data_list, nrow) != 0)]
data_AUC_3<-list()
for(i in names(data_list2)){print(i);
  data_AUC_3[[i]] <- AUC(data_list2[[i]]$times, data_list2[[i]]$value)
}
list_final_data5=subset(list_final_data4,list_final_data4$replicate==4)
data_list <- split(list_final_data5, f = list_final_data5$Treatment)
data_list2 = data_list[which(lapply(data_list, nrow) != 0)]
data_AUC_4<-list()
for(i in names(data_list2)){print(i);
  data_AUC_4[[i]] <- AUC(data_list2[[i]]$times, data_list2[[i]]$value)
}
data_AUC_1_df <- as.data.frame(data_AUC_1)
data_AUC_2_df <- as.data.frame(data_AUC_2)
data_AUC_3_df <- as.data.frame(data_AUC_3)
data_AUC_4_df <- as.data.frame(data_AUC_4)
colnames(data_AUC_1_df)=paste(colnames(data_AUC_1_df),"rep1",sep = '_')
colnames(data_AUC_2_df)=paste(colnames(data_AUC_2_df),"rep2",sep = '_')
colnames(data_AUC_3_df)=paste(colnames(data_AUC_3_df),"rep3",sep = '_')
colnames(data_AUC_4_df)=paste(colnames(data_AUC_4_df),"rep4",sep = '_')
data_AUC_final=cbind(data_AUC_1_df,data_AUC_2_df,data_AUC_3_df,data_AUC_4_df)
 write.table(data_AUC_final,"data_AUC_final_GFP.txt",sep="\t")
data_AUC_final=read.table("data_AUC_final_GFP.txt",sep="\t")

data_AUC_final_t=as.data.frame(t(data_AUC_final))
data_AUC_final_t$rep=rownames(data_AUC_final_t)
data_AUC_final_t$treatment=rownames(data_AUC_final_t)
data_AUC_final_t$rep=str_replace_all(data_AUC_final_t$rep,"col[0-9]_","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")

data_AUC_final_t2=data_AUC_final_t[complete.cases(data_AUC_final_t$treatment2),]
write.table(data_AUC_final_t2,"data_AUC_final_to_plot_GFP.txt",sep="\t")
##data_AUC_final_t2=read.table("data_AUC_final_to_plot_ANC_EV.txt",sep="\t")

###########Plots
####subset only GFP
library(FSA)
library(rcompanion)
library(rstatix)
library(ggpubr)
data_AUC_final_t3=data_AUC_final_t2[grep("Rs.gfp", data_AUC_final_t2$treatment2),] 
colnames(data_AUC_final_t3)=sub("1","V1",colnames(data_AUC_final_t3))
model  <- lm(V1 ~ treatment2, data = data_AUC_final_t3)
shapiro_test(residuals(model)) ####non normally distributed 
####no ANOVA use KRUSKALL WALLIs
res.kruskal <- data_AUC_final_t3 %>% kruskal_test(V1 ~ treatment2)
res.kruskal

#  V1       40      37.0     9 0.0000263 Kruskal-Wallis
pwc2 <- dunnTest(V1 ~ treatment2,data=data_AUC_final_t3,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"category" # change the name of grouping factor according to the dataset (df)
cld

cld$category=c("Bacillus.B12_Rs.gfp","Burkholderia.B12_Rs.gfp","Burkholderia.PB10_Rs.gfp","Burkholderia.PB18_Rs.gfp","Pandoraea.B14_Rs.gfp",
"Pseudomonas.P19_Rs.gfp",
"Pseudomonas.P91_Rs.gfp",
"Rhodanobacter.R55_Rs.gfp",
"Rhodanobacter.R68_Rs.gfp",
"Rs.UW551_Rs.gfp")

cld$category<-factor(cld$category,levels=c("Bacillus.B12_Rs.gfp",
"Pseudomonas.P19_Rs.gfp",
"Pseudomonas.P91_Rs.gfp",
"Rhodanobacter.R55_Rs.gfp",
"Rhodanobacter.R68_Rs.gfp",
"Burkholderia.B12_Rs.gfp",
"Pandoraea.B14_Rs.gfp",
"Burkholderia.PB10_Rs.gfp",
"Burkholderia.PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"))

color_table <-  tibble(
 Group_Name = c("Bacillus.B12_Rs.gfp",
"Pseudomonas.P19_Rs.gfp",
"Pseudomonas.P91_Rs.gfp",
"Rhodanobacter.R55_Rs.gfp",
"Rhodanobacter.R68_Rs.gfp",
"Burkholderia.B12_Rs.gfp",
"Pandoraea.B14_Rs.gfp",
"Burkholderia.PB10_Rs.gfp",
"Burkholderia.PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"),
  Color = c("#332288","#0072B2","56B4E9","#6699CC","#88CCEE","#117733","#009E73","#44AA99","#999933","black") ) 
ylim1=max(data_AUC_final_t3$V1)*1.2
ylim2=max(data_AUC_final_t3$V1)*1.05
ylim3=min(data_AUC_final_t3$V1)*1.2
subset_Rs=subset(data_AUC_final_t3,data_AUC_final_t3$treatment2=="Rs.UW551_Rs.gfp")
mean_Rs=median(subset_Rs[c(1,2,3),1])
data_AUC_final_t3$V1[data_AUC_final_t3$V1<0]<-0

data_AUC_final_t3$treatment2<-factor(data_AUC_final_t3$treatment2,levels=c("Bacillus.B12_Rs.gfp",
"Pseudomonas.P19_Rs.gfp",
"Pseudomonas.P91_Rs.gfp",
"Rhodanobacter.R55_Rs.gfp",
"Rhodanobacter.R68_Rs.gfp",
"Burkholderia.B12_Rs.gfp",
"Pandoraea.B14_Rs.gfp",
"Burkholderia.PB10_Rs.gfp",
"Burkholderia.PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"))


tiff("GFP_AUC_letters_final_Rs_PGPR.tiff",res=300,he=120,wi=250,units="mm")
 ggboxplot(data=data_AUC_final_t3, x = "treatment2", y = "V1",
  fill = "treatment2")+ theme_bw() + ylim(ylim3,ylim1)+
  geom_text(data = cld, aes(label = cld$Letter, y = ylim2,x = category), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=5.0,
            check_overlap = F)+   xlab("Treatments")+   ylab("GFP fluorescence area under the curve at 48hpi")+scale_fill_manual(values = color_table$Color)+
  theme(strip.text.x = element_text(size = 40))+theme(strip.text.y = element_text(size = 40))+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0.95))+theme(strip.text.y = element_text(size = 80))+
geom_hline(yintercept=mean_Rs, linetype="dashed", 
                color = "black", size=2)
dev.off()
###


############
#2.Bacteria from soil vs Rasltonia in planta
############

setwd("R:/rsrch/ah1309/lab/Sara/Ralstonia_project/experiments/FERA experiments/competition_assays/plants")
data <- read.table("scores_PGPR.txt", header = T, sep = '\t')

######Barplot

data3=data %>% group_by(Treatment, Groups)  %>%     dplyr::summarise(count=n())
data3=data3%>%    mutate(per =  100 *count/sum(count))

data3=as.data.frame(data3)
data3$Treatment=sub("B12", "Burkholderia.B12", data3$Treatment)
data3$Treatment=sub("P19", "Pseudomonas.P19", data3$Treatment)
data3$Treatment=sub("PB18", "Burkholderia.PB18", data3$Treatment)
data3$Treatment=sub("R55", "Rhodanobacter.R55", data3$Treatment)

data3$Treatment=factor(data3$Treatment,levels=c("Negative_control","Pseudomonas.P19","Pseudomonas.P19.RsUW551","Rhodanobacter.R55","Rhodanobacter.R55.RsUW551","Burkholderia.B12","Burkholderia.B12.RsUW551","Burkholderia.PB18","Burkholderia.PB18.RsUW551","Rs.UW551"))
data3$Groups=factor(data3$Groups,levels=c("Negative_control","Healthy","Diseased"))

data3$per=round(data3$per, digits=2)
data3$percentage=paste(data3$per, "%")
data3=data3[grep("Negative_control|Pseudomonas.P19.Rs.UW551|Rhodanobacter.R55.Rs.UW551|Burkholderia.B12.Rs.UW551|Burkholderia.PB18.Rs.UW551|Rs.UW551", data3$Treatment), ]

tiff("barplot_DI.tiff",res=300,he=120,wi=150,units="mm")
ggplot(data=data3, aes(x=Treatment, y=count,fill=Groups)) +
geom_bar(stat="identity", position="stack") +
geom_text(aes(label = percentage),
            position = position_stack(vjust = 0.5), size = 4) +
    scale_fill_manual(values=c("#777777","#44AA99","black")) +
           labs(x = "Treatment",
         y = "Counts")+
  theme(strip.text.x = element_text(size = 40))+theme(strip.text.y = element_text(size = 40))+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0.95))+theme(strip.text.y = element_text(size = 80))
dev.off()

############
##3.Plates
############
setwd("R:/rsrch/ah1309/lab/Sara/Ralstonia_project/experiments/FERA experiments/competition_assays/selected_9")
plates=read.table("selected_9_plates.txt",sep="\t", header=T)
plates$mm=as.numeric(plates$mm)

model  <- lm(mm ~ Treatment, data = plates)
shapiro_test(residuals(model)) ####non normally distributed 
####no ANOVA use KRUSKALL WALLIs
res.kruskal <- plates %>% kruskal_test(mm ~ Treatment)
res.kruskal

# mm       36      30.0     8 0.000212 Kruskal-Wallis
pwc2 <- dunnTest(mm ~ Treatment,data=plates,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"category" # change the name of grouping factor according to the dataset (df)
cld

cld$category=c("Bacillus.B12_Rs.UW551","Burkholderia.B12_Rs.UW551","Burkholderia.PB10_Rs.UW551","Burkholderia.PB18_Rs.UW551","Pandoraea.B14_Rs.UW551",
"Pseudomonas.P19_Rs.UW551",
"Pseudomonas.P91_Rs.UW551",
"Rhodanobacter.R55_Rs.UW551",
"Rhodanobacter.R68_Rs.UW551")

cld$category<-factor(cld$category,levels=c("Bacillus.B12_Rs.UW551",
"Pseudomonas.P19_Rs.UW551",
"Pseudomonas.P91_Rs.UW551",
"Rhodanobacter.R55_Rs.UW551",
"Rhodanobacter.R68_Rs.UW551",
"Burkholderia.B12_Rs.UW551",
"Pandoraea.B14_Rs.UW551",
"Burkholderia.PB10_Rs.UW551",
"Burkholderia.PB18_Rs.UW551"))

color_table <-  tibble(
 Group_Name = c("Bacillus.B12_Rs.UW551",
"Pseudomonas.P19_Rs.UW551",
"Pseudomonas.P91_Rs.UW551",
"Rhodanobacter.R55_Rs.UW551",
"Rhodanobacter.R68_Rs.UW551",
"Burkholderia.B12_Rs.UW551",
"Pandoraea.B14_Rs.UW551",
"Burkholderia.PB10_Rs.UW551",
"Burkholderia.PB18_Rs.UW551"),
  Color = c("#332288","#0072B2","56B4E9","#6699CC","#88CCEE","#117733","#009E73","#44AA99","#999933") ) 


ylim1=max(plates$mm, na.rm=T)*1.2
ylim2=max(plates$mm, na.rm=T)*1.05
ylim3=min(plates$mm, na.rm=T)*1.2

plates$Treatment<-factor(plates$Treatment,levels=c("Bacillus.B12_Rs.UW551",
"Pseudomonas.P19_Rs.UW551",
"Pseudomonas.P91_Rs.UW551",
"Rhodanobacter.R55_Rs.UW551",
"Rhodanobacter.R68_Rs.UW551",
"Burkholderia.B12_Rs.UW551",
"Pandoraea.B14_Rs.UW551",
"Burkholderia.PB10_Rs.UW551",
"Burkholderia.PB18_Rs.UW551"))

tiff("AUC_letters_final_Rs_PGPR_plates_mm.tiff",res=300,he=100,wi=250,units="mm")
 ggboxplot(data=plates, x = "Treatment", y = "mm",
  fill = "Treatment")+ theme_bw() + ylim(ylim3,ylim1)+
  geom_text(data = cld, aes(label = cld$Letter, y = ylim2,x = category), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=5.0,
            check_overlap = F)+   xlab("Treatments")+   ylab("Inhibitory halo (mm)")+scale_fill_manual(values = color_table$Color)+
 theme(strip.text.x = element_text(size = 40))+theme(strip.text.y = element_text(size = 40))+ theme(axis.text.x = element_text(size=7,angle = 30, vjust = 1, hjust=0.95))+theme(strip.text.y = element_text(size = 80))

dev.off()




############
##4.plot ASVs
############

##########AFTER BLAST
OTUs=as.data.frame(data_phylo_filt_rar@otu_table)
OTUs_t=as.data.frame(t(OTUs))
OTUs_t$SampleName<-rownames(OTUs_t)
NMDS_ASV=left_join(OTUs_t, data.scores.sites, by="SampleName")
write.table(NMDS_ASV,"NMDS_and_samples_position_ASVs.txt",sep="\t",col.names=T)
taxa_species=c("SampleName","NMDS1","NMDS2","ASV1852")
NMDS_ASV_filt=NMDS_ASV[, taxa_species]


data.scores.sites=read.table("NMDS_coordinates_samples.txt",sep="\t",header=T)
table1=data.scores.sites
groups=read.table("groups_no_49.txt",header=T,sep="\t") ####not having 49
table2=left_join(NMDS_ASV_filt,groups,by="SampleName")
table2$NMD1_position=with(table2, ifelse(table2$NMDS1 >0, "scattered","centered"))
table2$Phage_NMDS1=paste(table2$Phages,"_",table2$NMD1_position)
table2$Phage_NMDS1=sub(" ","", table2$Phage_NMDS1)
table2$Phage_NMDS1=sub(" ","", table2$Phage_NMDS1)


#table2[,c(4:11)] <- sapply(table[,c(4:11)], as.numeric)
table2$Phages=factor(table2$Phages,levels=c("Negative_control","Individual","Double","Rs.UW551"))
table2$category3=factor(table2$category3,levels=c("Negative_control","Healthy","Diseased"))
table2$category_phages=factor(table2$category_phages,levels=c("Negative_control","Individual_Healthy","Individual_Diseased","Double_Healthy","Double_Diseased","Rs.UW551_Healthy","Rs.UW551_Diseased"))
table2$Phage_NMDS1=factor(table2$Phage_NMDS1,levels=c("Negative_control_centered","Individual_centered","Individual_scattered","Double_centered","Double_scattered", "Rs.UW551_centered"))



library(tidyverse)
library(ggpubr)
library(rstatix)
library(multcomp)
library(FSA)
library(ggpubr)
library(rcompanion)
library(flextable)

color_table <-  tibble(
  Group_Name = c("Negative_control_centered","Individual_centered","Individual_scattered","Double_centered","Double_scattered", "Rs.UW551_centered"),
  Color = c("#666666","#E69F00","#D55E00","#0072B2","#332288","#882255"))
names=colnames(table2)
names=c("ASV1852","ASV22", "ASV62", "ASV115", "ASV142", "ASV48","ASV157", "ASV50")


anova=list()
model=list()
shapiro=list()
res.kruskal=list()

for (i in names) {

table$Phage_NMDS1=sub("1-Phage","1.Phage",table$Phage_NMDS1)
table$Phage_NMDS1=sub("2-Phages","2.Phages",table$Phage_NMDS1)

model[[i]]  <- lm(table2[[i]] ~ Phage_NMDS1, data = table2)
shapiro[[i]]=shapiro_test(residuals(model[[i]])) ###all are non-normally distributed
res.kruskal[[i]] <- table2 %>% kruskal_test(table2[[i]] ~ Phage_NMDS1)
res.kruskal [[i]]


pwc2 <- dunnTest(table2[[i]]  ~ Phage_NMDS1,data=table2,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"Phage_NMDS1" # change the name of grouping factor according to the dataset (df)
cld

cld$Phage_NMDS1=sub("1.Phage","1-Phage",cld$Phage_NMDS1)
cld$Phage_NMDS1=sub("2.Phages","2-Phages",cld$Phage_NMDS1)


cld$Phage_NMDS1=factor(cld$Phage_NMDS1,levels=c("Negative_control_centered","1-Phage_centered","1-Phage_shifted","2-Phages_centered","2-Phages_shifted", "Rs.UW551_centered"))
ylabel = substitute(expr = paste(italic(i), " abundance (counts)"),
 env = list(i=i))

ylim=1.1*max(table[[i]])
ylim2=1.05*max(table[[i]])
ylim3=min(table[[i]])

table$Phage_NMDS1=sub("1.Phage","1-Phage",table$Phage_NMDS1)
table$Phage_NMDS1=sub("2.Phages","2-Phages",table$Phage_NMDS1)

table$Phage_NMDS1=factor(table$Phage_NMDS1,levels=c("Negative_control_centered","1-Phage_centered","1-Phage_shifted","2-Phages_centered","2-Phages_shifted", "Rs.UW551_centered"))

tiff(filename = paste(i, "Phages_NMDS1_position.tiff",sep = ""),res=300,he=200,wi=200,units="mm")
p=ggboxplot(data=table2, x = "Phage_NMDS1", y=i,
  fill = "Phage_NMDS1")+ theme_bw() +ylim(ylim3,ylim)+
  geom_text(data = cld, aes(label = cld$Letter, y = ylim2, x = Phage_NMDS1), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=8.0,
             check_overlap = F)+scale_fill_manual(values = color_table$Color)+
 ylab(ylabel)+xlab("Treatment according to NMDS1")+
 theme(strip.text.x = element_text(size = 20))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 22))
print(p)
dev.off()

}

