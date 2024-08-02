############
1. Growth curves -OD600nm - testing Ralstonia solanacearum growth inhibition when growing with phages
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
for(j in colnames(table[[i]][,c(9:12)])) {print (j); 
blank=mean(table[[i]]$col1,na.rm =T)
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
write.table(data,"data_formatted_ODs_UW551_phages.txt",sep="\t")
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
write.table(data,"data_formatted_2_ODs_UW551_phages.txt",sep="\t")
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

 write.table(list_final_data2,"long_data_formatted_ODs_UW551_phages.txt",sep="\t")

list_final_data3=list_final_data2
list_final_data3$times=str_replace_all(list_final_data3$times,"h","")
list_final_data3$times=as.numeric(list_final_data3$times)
list_final_data3$value=as.numeric(list_final_data3$value)

###################PLOT CURVES

list_final_data4=list_final_data3

list_final_data4$times=sub("h","",list_final_data3$times)


list_final_data4$variable  <- case_when(
                                    list_final_data4$variable  == "col9" ~ "Rs.UW551",
                                    list_final_data4$variable == "col9_Rs" ~ "Rs.UW551.PYO4",
                                    list_final_data4$variable  == "col10_Rs" ~ "Rs.UW551.PYO59",
                                    list_final_data4$variable  == "col10" ~ "Rs.UW551.PYO45",
				    list_final_data4$variable  == "col11" ~ "Rs.UW551.PYO65",
				   )


list_final_data4$times=as.numeric(list_final_data4$times)
##
list_final_data4=list_final_data4[grep("Rs.UW551", list_final_data4$variable),] 

color_table <-  tibble(
 Group_Name = c("Rs.UW551","Rs.UW551.PYO4",
"Rs.UW551.PYO45",
"Rs.UW551.PYO59","Rs.UW551.PYO65"),
Color = c("black","#CC6677","#661100","#0072B2","#009E73")  )

colnames(list_final_data4)=sub("variable","Treatment",colnames(list_final_data4))

data_summary <- summarySE(data = list_final_data4, measurevar = "value", 
                          groupvars = c("times", "Treatment"),
                          na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)



tiff("curves_3_OD_UW551_phages.tiff",res=300,he=200,wi=200,units="mm")
 ggplot(data = data_summary, aes(x = times, y = value, group = Treatment, col = Treatment))  +
   geom_line(linewidth=2) +
   labs(x = "Time",
          y = "Bacterial density (OD 600nm)") +
    theme_bw()+
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank())+scale_colour_manual(values = color_table$Color)+
theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 25))
dev.off()


##########AUC - calculate the area under the growth curve

library(pROC)
library(DescTools)
# this uses a loop. Each interation of the loop will make a subset of the data for 1 curve, then calculates the AUC from it.
# use the subsetted data to calculate auc (flux package)####check
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
 write.table(data_AUC_final,"data_AUC_final_OD_UW551_phages.txt",sep="\t")
data_AUC_final=read.table("data_AUC_final_OD_UW551_phages.txt",sep="\t")

data_AUC_final_t=as.data.frame(t(data_AUC_final))
data_AUC_final_t$rep=rownames(data_AUC_final_t)
data_AUC_final_t$treatment=rownames(data_AUC_final_t)
data_AUC_final_t$rep=str_replace_all(data_AUC_final_t$rep,"col[0-9]_","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")
data_AUC_final_t$treatment2=str_replace_all(data_AUC_final_t$treatment,"_rep[0-9]","")
data_AUC_final_t2=data_AUC_final_t[complete.cases(data_AUC_final_t$treatment2),]
write.table(data_AUC_final_t2,"data_AUC_final_to_plot_UW551_phages.txt",sep="\t")


###########PLOT and stats comparing each of the groups (R.solanacearum with/without phages)

library(tidyverse)
library(ggpubr)
library(rstatix)
data_AUC_final_t3=data_AUC_final_t2
colnames(data_AUC_final_t3)=sub("1","V1",colnames(data_AUC_final_t3))

model  <- lm(V1 ~ treatment2, data = data_AUC_final_t3)
shapiro_test(residuals(model)) ####non normally distributed 
####no ANOVA use KRUSKALL WALLIs
res.kruskal <- data_AUC_final_t3 %>% kruskal_test(V1 ~ treatment2)
res.kruskal


###Result
#####     V1       20      16.0     4 0.00296 Kruskal-Wallis

pwc2 <- dunnTest(V1 ~ treatment2,data=data_AUC_final_t3,method="bonferroni")
pwc2_res <- pwc2$res
cld <- cldList(comparison = pwc2_res$Comparison,
        p.value    = pwc2_res$P.adj,
        threshold  = 0.05)[1:2]
names(cld)[1]<-"category" # change the name of grouping factor according to the dataset (df)
cld
cld$category=factor(cld$category,levels=c(
"Rs.UW551","Rs.UW551.PYO4",
"Rs.UW551.PYO45",
"Rs.UW551.PYO59","Rs.UW551.PYO65"))

data_AUC_final_t3$treatment2=factor(data_AUC_final_t3$treatment2,levels=c(
"Rs.UW551","Rs.UW551.PYO4",
"Rs.UW551.PYO45",
"Rs.UW551.PYO59","Rs.UW551.PYO65"))

color_table <-  tibble(
 Group_Name = c("Rs.UW551","Rs.UW551.PYO4",
"Rs.UW551.PYO45",
"Rs.UW551.PYO59","Rs.UW551.PYO65"),
Color = c("black","#CC6677","#661100","#0072B2","#009E73")  )


data_AUC_final_t3$V1[data_AUC_final_t3$V1<0]<-0
y1=max(data_AUC_final_t3$V1)*1.4
y2=max(data_AUC_final_t3$V1)*1.2
tiff("OD_AUC_UW551_phages_letters.tiff",res=300,he=200,wi=200,units="mm")
 ggboxplot(data=data_AUC_final_t3, x = "treatment2", y = "V1",
  fill = "treatment2")+ theme_bw() + ylim(0,y1)+
  geom_text(data = cld, aes(label = cld$Letter, y = y2, x = category), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=8.0,
            check_overlap = F)+
  xlab("Treatments")+   ylab("Area under the growth curve (AUC)")+scale_fill_manual(values = color_table$Color)+
  ggtitle("AUC 75h (OD 600nm)")+ theme(strip.text.x = element_text(size = 30))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme( text = element_text(size = 25))
dev.off()


