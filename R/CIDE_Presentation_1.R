# Results at the national level
rm(list=ls(all=TRUE))
setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories
# devtools::install_github('hadley/ggplot2')

#### Grouping
#### AMS: 7+10+8+9+12, Diabetes, IHD, LUNG CANCER, CIRRHOSIS, HOMICIDES, TRAFFIC ACCIDENTS,
#### REST: 14+21+15+20
library(ggthemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)
library(reshape2)

source('R/Version including infant mortality/2_3_Reshape_Results.R')
source('R/Functions.R')

# Get some nice colors
#display.brewer.all()
base2 <- c(1:15)
base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')

# Figures for life expectancy at birth ------------------------------------


#First with 2000-2005
State   <- 'National'
Initial <- 2000
Final   <- 2005
sex     <- 'Males'

Data.fig        <- getData.function.g(Data2 = DT.Decomp.ex,state = State,initial = Initial,final = Final)
Data.fig$Period <- paste0(Initial,'-',Final)
Data.fig        <- Data.fig[Data.fig$Sex == sex,]

# Get life expectancies
e01     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Initial & DT.LTmx$sex == 'Males']$ex[1],2)
e02     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Final & DT.LTmx$sex == 'Males']$ex[1],2)
dife0   <- e02-e01

Total.cause <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Cause)]
sum(Total.cause$Contribution)

Total.Age <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Age)]
sum(Total.Age$Contribution)
Total.Age$Contribution <- round(Total.Age$Contribution,2)
ylim1 <- min(Data.fig$Contribution)


cause.name.vec      <- levels(Data.fig$Cause)
cause.code          <- unique(Data.fig$Cause)

cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$Contribution,2)),')')


# Better to do separately to get the subtotal by causes of death

ex.males1 <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle(paste0('A ', Initial,'-',Final), subtitle = bquote(e[.(Initial)] == .(e01)~',' ~ e[.(Final)] == .(e02) ~'. Difference in life expectancy ='~ .(dife0) ) )+
  ylim(-.2, .4)+
  scale_fill_manual(name= ' Cause of death (Contribution)',values=base2,labels = cause.lab)+
  geom_bar(aes(group = Cause), stat = "identity",position = "stack")+
  theme_light()+
  #geom_label(aes(Age, -.17, label = Contribution,fill=NULL),size=3,show.legend = FALSE, data = Total.Age)+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(legend.position = c(.75,.75))+
  geom_hline(yintercept = 0)

ex.males1



#Now 2005-2010
State   <- 'National'
Initial <- 2005
Final   <- 2010
sex     <- 'Males'

Data.fig        <- getData.function.g(Data2 = DT.Decomp.ex,state = State,initial = Initial,final = Final)
Data.fig$Period <- paste0(Initial,'-',Final)
Data.fig        <- Data.fig[Data.fig$Sex == sex,]

# Get life expectancies
e01     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Initial & DT.LTmx$sex == 'Males']$ex[1],2)
e02     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Final & DT.LTmx$sex == 'Males']$ex[1],2)
dife0   <- e02-e01

Total.cause <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Cause)]
sum(Total.cause$Contribution)

Total.Age <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Age)]
sum(Total.Age$Contribution)
Total.Age$Contribution <- round(Total.Age$Contribution,2)
ylim1 <- min(Data.fig$Contribution)


cause.name.vec      <- levels(Data.fig$Cause)
cause.code          <- unique(Data.fig$Cause)

cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$Contribution,2)),')')
text.1 <- paste('Numbers in boxes are age-specific contributions')

# Better to do separately to get the subtotal by causes of death

ex.males2 <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle(paste0('A ', Initial,'-',Final), subtitle = bquote(e[.(Initial)] == .(e01)~',' ~ e[.(Final)] == .(e02) ~'. Difference in life expectancy ='~ .(dife0) ) )+
  ylim(-.2, .4)+
  scale_fill_manual(name= ' Cause of death (Contribution)',values=base2,labels = cause.lab)+
  geom_bar(aes(group = Cause), stat = "identity",position = "stack")+
  theme_light()+
  #geom_label(aes(Age, -.17, label = Contribution,fill=NULL),size=3,show.legend = FALSE, data = Total.Age)+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(legend.position = c(.75,.75))+
  geom_hline(yintercept = 0)

ex.males2

require(gridExtra)
pdf(file = 'C:/Users/jmaburto/Documents/GitHub/CIDE-presentation-2018/Latex/Figures/Fig2.pdf',width=16,height=7,useDingbats = F)
grid.arrange(ex.males1,ex.males2,ncol=2)
dev.off()

# Figures for lifespan variation at birth ------------------------------------
source('R/2_3_Reshape_Results.R')
source('R/Functions.R')


base2 <- c("#D53E4F","lightgrey")

# Figures for life expectancy at birth ------------------------------------

#First with 2000-2005
States <- unique(DT.Decomp.ed$state.name)
States <- c(States[-13],'Guerrero')
i <- States[1]

pdf(file = 'C:/Users/jmaburto/Documents/GitHub/CIDE-presentation-2018/Latex/Figures/Cause_ed_decomp_Males_states.pdf',width=9,height=7,useDingbats = F)
for (i in States){
State   <- i
Initial <- 2005
Final   <- 2015
sex     <- 'Males'
print(i)
Data.fig        <- getData.function.g(Data2 = DT.Decomp.ed,state = State,initial = Initial,final = Final)
Data.fig$Period <- paste0(Initial,'-',Final)
Data.fig        <- Data.fig[Data.fig$Sex == sex,]

Data.fig$Causa <- 1
Data.fig[Data.fig$Cause != 'Homicide',]$Causa <- 2
Data.fig$Causa <- as.factor(Data.fig$Causa)
levels(Data.fig$Causa) <- c('Homicidio','Resto')

Data.fig <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Age,Period,Causa)]
Data.fig <- Data.fig[Data.fig$Age %in% unique(Data.fig$Age)[4:20],]


# Get life expectancies
e01     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Initial & DT.LTmx$sex == 'Males']$ex[1],2)
e02     <- round(DT.LTmx[DT.LTmx$state.name == State & DT.LTmx$year ==  Final & DT.LTmx$sex == 'Males']$ex[1],2)
dife0   <- e02-e01

Total.cause <- Data.fig[,list(Contribution = sum(Contribution)), by = list(Causa)]
sum(Total.cause$Contribution)


cause.name.vec      <- levels(Data.fig$Causa)
cause.code          <- unique(Data.fig$Causa)

#cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$Contribution,2)),')')

ed.males1 <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Causa)) +
  ggtitle(paste0(i, ' 2005-15') )+
  scale_fill_manual(name= ' Causa de muerte',values=base2)+
  ylim(-.25, .25)+
  geom_bar(aes(group = Causa), stat = "identity",position = "stack",show.legend = F)+
  theme_light()+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Edad", y = "Contribucion",size=12)+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(legend.position = c(.75,.35))+
  geom_hline(yintercept = 0)
  #scale_x_discrete(position = "top") 

print(ed.males1)

}
dev.off()


