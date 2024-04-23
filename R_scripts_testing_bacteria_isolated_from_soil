############
##1. Bacteria from soil vs Ralstonia in vitro - plot the OD over time and calculate the area under the growth curve and do stats comparing groups
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

#####this is a excel with a spreadsheet for each time point h0, h2, ....etc the col names are Rep col1 col2 ....
table=read_excel_allsheets("ODs.xlsx")
table<-as.list(table)

new_table<-list()

for(i in names(table))  {print (i);
new_table[[i]]=table[[i]]
##new_table=as.data.frame(new_table);
for(j in colnames(table[[i]][,c(2:6)])) {print (j); 
blank=table[[i]]$col12
  new_table[[i]][[j]]=table[[i]][[j]]-blank;
print(new_table[[i]][[j]])
new_table[[i]]$Rep<-table[[i]]$Rep}}

#### 4 first row is one treatment and 4 last rows other
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
write.table(data,"data_formatted_ODs.txt",sep="\t")
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
write.table(data,"data_formatted_2_ODs.txt",sep="\t")
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

 write.table(list_final_data2,"long_data_formatted_ODs.txt",sep="\t")

list_final_data3=list_final_data2
list_final_data3$times=str_replace_all(list_final_data3$times,"h","")
list_final_data3$times=as.numeric(list_final_data3$times)
list_final_data3$value=as.numeric(list_final_data3$value)

list_final_data4=list_final_data3
list_final_data4$times=sub("h","",list_final_data3$times)
list_final_data4$variable  <- case_when(list_final_data4$variable  == "col2" ~ "Rs.UW551",
                                    list_final_data4$variable  == "col3" ~ "Bacterium_12",
                                    list_final_data4$variable == "col4" ~ "Bacterium_19",
                                    list_final_data4$variable  == "col5" ~ "Bacterium_91",
                                    list_final_data4$variable  == "col6" ~ "Bacterium_55",
                                    list_final_data4$variable  == "col7" ~ "Bacterium_68",
                                    list_final_data4$variable  == "col8" ~ "Bacterium_B12",
                                    list_final_data4$variable  == "col9" ~ "Bacterium_B14",
                                    list_final_data4$variable  == "col10" ~ "Bacterium_PB10",
                                    list_final_data4$variable  == "col11" ~ "Bacterium_PB18",
                               
                                    list_final_data4$variable  == "col2_Rs" ~ "Rs.UW551_Rs.gfp",
                                    list_final_data4$variable  == "col3_Rs" ~ "Bacterium_12_Rs.gfp",
                                    list_final_data4$variable  == "col4_Rs" ~ "Bacterium_19_Rs.gfp",
                                    list_final_data4$variable  == "col5_Rs" ~ "Bacterium_91_Rs.gfp",
                                    list_final_data4$variable  == "col6_Rs" ~ "Bacterium_55_Rs.gfp",
                                    list_final_data4$variable == "col7_Rs" ~ "Bacterium_68_Rs.gfp",
                                    list_final_data4$variable  == "col8_Rs" ~ "Bacterium_B12_Rs.gfp",
                                    list_final_data4$variable  == "col9_Rs" ~ "Bacterium_B14_Rs.gfp",
                                    list_final_data4$variable  == "col10_Rs" ~ "Bacterium_PB10_Rs.gfp",
                                    list_final_data4$variable == "col11_Rs" ~ "Bacterium_PB18_Rs.gfp")


list_final_data4$times=as.numeric(list_final_data4$times)

####calculate the area under the growth curve
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
 write.table(data_AUC_final,"data_AUC_final_OD.txt",sep="\t")
data_AUC_final=read.table("data_AUC_final_OD.txt",sep="\t")

data_AUC_final_t=as.data.frame(t(data_AUC_final))
data_AUC_final_t$rep=rownames(data_AUC_final_t)
data_AUC_final_t$treatment=rownames(data_AUC_final_t)
data_AUC_final_t$rep=str_replace_all(data_AUC_final_t$rep,"col[0-9]_","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")
data_AUC_final_t2=data_AUC_final_t[complete.cases(data_AUC_final_t$treatment2),]

write.table(data_AUC_final_t2,"data_AUC_final_to_plot_ANC_EV.txt",sep="\t")
data_AUC_final_t2=read.table("data_AUC_final_to_plot_ANC_EV.txt",sep="\t")

###########Plots and stats comparing the groups 
#######subset only GFP combinations
data_AUC_final_t3=data_AUC_final_t2[grep("Rs.gfp", data_AUC_final_t2$treatment2),] 


colnames(data_AUC_final_t3)=sub("X1","V1",colnames(data_AUC_final_t3))

model  <- lm(V1 ~ treatment2, data = data_AUC_final_t3)
shapiro_test(residuals(model)) ####non normally distributed 
####no ANOVA use KRUSKALL WALLIs
res.kruskal <- data_AUC_final_t3 %>% kruskal_test(V1 ~ treatment2)
res.kruskal

#V1       40      35.7     9 0.0000457 Kruskal-Wallis

pwc2 <- dunnTest(V1 ~ treatment2,data=data_AUC_final_t3,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"category" # change the name of grouping factor according to the dataset (df)
cld
cld
cld$category<-c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp")

cld$category<-factor(cld$category,levels=c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"))

color_table <-  tibble(
 Group_Name = c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"),
  Color = c("#332288","#0072B2","56B4E9","#6699CC","#88CCEE","#117733","#009E73","#44AA99","#999933","black") ) 

data_AUC_final_t3$V1[data_AUC_final_t3$V1<0]<-0
tiff("OD_AUC_letters_final_Rs_PGPR.tiff",res=300,he=200,wi=300,units="mm")
 ggboxplot(data=data_AUC_final_t3, x = "treatment2", y = "V1",
  fill = "treatment2")+ theme_bw() + ylim(0,250)+
  geom_text(data = cld, aes(label = cld$Letter, y = 200, x = category), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=5.5,
            check_overlap = F)+
  xlab("Treatments")+   ylab("AUC")+scale_fill_manual(values = color_table$Color)+
  ggtitle("AUC 48h (OD 600nm)")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 20))
dev.off()


############
##2. Bacteria from soil vs Ralstonia in vitro GFP - as above but using the GFP measurements of the Rs-GFP tagged strain
############

library(Growthcurver)
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

######file as above with GFP measurements
table=read_excel_allsheets("GFP.xlsx")
table<-as.list(table)

new_table<-list()

for(i in names(table))  {print (i);
new_table[[i]]=table[[i]]
##new_table=as.data.frame(new_table);
for(j in colnames(table[[i]][,c(2:6)])) {print (j); 
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
                                    list_final_data4$variable  == "col3" ~ "Bacterium_12",
                                    list_final_data4$variable == "col4" ~ "Bacterium_19",
                                    list_final_data4$variable  == "col5" ~ "Bacterium_91",
                                    list_final_data4$variable  == "col6" ~ "Bacterium_55",
                                    list_final_data4$variable  == "col7" ~ "Bacterium_68",
                                    list_final_data4$variable  == "col8" ~ "Bacterium_B12",
                                    list_final_data4$variable  == "col9" ~ "Bacterium_B14",
                                    list_final_data4$variable  == "col10" ~ "Bacterium_PB10",
                                    list_final_data4$variable  == "col11" ~ "Bacterium_PB18",
                               
                                    list_final_data4$variable  == "col2_Rs" ~ "Rs.UW551_Rs.gfp",
                                    list_final_data4$variable  == "col3_Rs" ~ "Bacterium_12_Rs.gfp",
                                    list_final_data4$variable  == "col4_Rs" ~ "Bacterium_19_Rs.gfp",
                                    list_final_data4$variable  == "col5_Rs" ~ "Bacterium_91_Rs.gfp",
                                    list_final_data4$variable  == "col6_Rs" ~ "Bacterium_55_Rs.gfp",
                                    list_final_data4$variable == "col7_Rs" ~ "Bacterium_68_Rs.gfp",
                                    list_final_data4$variable  == "col8_Rs" ~ "Bacterium_B12_Rs.gfp",
                                    list_final_data4$variable  == "col9_Rs" ~ "Bacterium_B14_Rs.gfp",
                                    list_final_data4$variable  == "col10_Rs" ~ "Bacterium_PB10_Rs.gfp",
                                    list_final_data4$variable == "col11_Rs" ~ "Bacterium_PB18_Rs.gfp")


list_final_data4$times=as.numeric(list_final_data4$times)

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
data_AUC_final_t2=read.table("data_AUC_final_to_plot_ANC_EV.txt",sep="\t")

###########Plots
####subset only GFP
data_AUC_final_t3=data_AUC_final_t2[grep("Rs.gfp", data_AUC_final_t2$treatment2),] 
colnames(data_AUC_final_t3)=sub("X1","V1",colnames(data_AUC_final_t3))
model  <- lm(V1 ~ treatment2, data = data_AUC_final_t3)
shapiro_test(residuals(model)) ####non normally distributed 
####no ANOVA use KRUSKALL WALLIs
res.kruskal <- data_AUC_final_t3 %>% kruskal_test(V1 ~ treatment2)
res.kruskal

#V1       40      34.2     9 0.0000836 Kruskal-Wallis

pwc2 <- dunnTest(V1 ~ treatment2,data=data_AUC_final_t3,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"category" # change the name of grouping factor according to the dataset (df)
cld
cld
cld$category<-c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp")

cld$category<-factor(cld$category,levels=c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"))

color_table <-  tibble(
 Group_Name = c("Bacterium_12_Rs.gfp",
"Bacterium_19_Rs.gfp",
"Bacterium_91_Rs.gfp",
"Bacterium_55_Rs.gfp",
"Bacterium_68_Rs.gfp",
"Bacterium_B12_Rs.gfp",
"Bacterium_B14_Rs.gfp",
"Bacterium_PB10_Rs.gfp",
"Bacterium_PB18_Rs.gfp",
"Rs.UW551_Rs.gfp"),
  Color = c("#332288","#0072B2","56B4E9","#6699CC","#88CCEE","#117733","#009E73","#44AA99","#999933","black") ) 

data_AUC_final_t3$V1[data_AUC_final_t3$V1<0]<-0
tiff("GFP_AUC_letters_final_Rs_PGPR.tiff",res=300,he=200,wi=300,units="mm")
 ggboxplot(data=data_AUC_final_t3, x = "treatment2", y = "V1",
  fill = "treatment2")+ theme_bw() + ylim(0,1000000)+
  geom_text(data = cld, aes(label = cld$Letter, y = 900000, x = category), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=5.5,
            check_overlap = F)+
  xlab("Treatments")+   ylab("AUC")+scale_fill_manual(values = color_table$Color)+
  ggtitle("AUC 48h (GFP)")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 20))
dev.off()




############
##3.Bacteria from soil vs Rasltonia in planta - comparing the disease score of tomato plants inoculated with bacteria and Ralstonia 
############
data <- read.table("scores_PGPR.txt", header = T, sep = '\t') ###Table containing the disease scores of the experiment 

######Barplot

data3=data %>% group_by(Treatment, Groups)  %>%     dplyr::summarise(count=n())
data3=as.data.frame(data3)
data3$Treatment=factor(data3$Treatment,levels=c("Control","P19","P19.RsUW551","R55","R55.RsUW551","B12","B12.RsUW551","PB18","PB18.RsUW551","Rs.UW551"))
data3[1,2]=c("Control")
data3$Groups=factor(data3$Groups,levels=c("Control","Healthy","Diseased"))

tiff("barplot_DI.tiff",res=300,he=200,wi=210,units="mm")
ggplot(data=data3, aes(x=Treatment, y=count,fill=Groups)) +
geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=c("#777777","#DDCC77","#332288")) +
    ggtitle("Number of Diseased and Healthy plants") +
       labs(x = "Treatment",
         y = "Counts")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 25))
dev.off()




############
##4.B12 PGPR in tomatoes - checking differences in dry and wet weight in tomatoes inoculated with B12
############

library(FSA)
library(rcompanion)
library(ggpubr)
library(tidyverse)
library(ggpubr)
library(rstatix)
data <- read.table("Weight.txt", header = T, sep = '\t') ###table containing dry and wet weight of the B12 treated tomatoes


######Fresh weight

ggboxplot(data$Weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())


shapiro.test(data$Weight) # => p-value = 0.6993
stat.test <- data %>% 
  t_test(Weight ~ Treatment) %>%
  add_significance()
stat.test
stat.test <- stat.test %>% add_xy_position(x = "group")

color_table2 <-  tibble(
  Treatment = c("B12_dead","B12_alive"),
  Color = c("#777777","#CC6677"))

ylim1=max(data$Weight)*1.5
ylim2=max(data$Weight)*1.2
tiff("Weight.tiff",res=300,he=200,wi=200,units="mm")
 ggboxplot(data=data, x = "Treatment", y = "Weight",
  fill = "Treatment")+ theme_bw() +ylim(0,ylim1)+ 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))+scale_fill_manual(values = color_table2$Color)+
  xlab("Treatments")+   ylab("Fresh weight (g)")+
  ggtitle("Fresh weight")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 25))
dev.off()


######Dry weight

ggboxplot(data$Dry_Weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())


shapiro.test(data$Dry_Weight) # => p-value = 0.6993
stat.test <- data %>% 
  t_test(Dry_Weight ~ Treatment) %>%
  add_significance()
stat.test
stat.test <- stat.test %>% add_xy_position(x = "group")

color_table2 <-  tibble(
  Treatment = c("B12_dead","B12_alive"),
  Color = c("#777777","#CC6677"))

ylim1=max(data$Dry_Weight)*1.5
ylim2=max(data$Dry_Weight)*1.2
tiff("Dry_Weight.tiff",res=300,he=200,wi=200,units="mm")
 ggboxplot(data=data, x = "Treatment", y = "Dry_Weight",
  fill = "Treatment")+ theme_bw() +ylim(0,ylim1)+ 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))+scale_fill_manual(values = color_table2$Color)+
  xlab("Treatments")+   ylab("Dry weight (g)")+
  ggtitle("Dry weight")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 25))
dev.off()


