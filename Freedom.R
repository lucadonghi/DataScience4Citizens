library(readxl)
library(dplyr)

# PER NON PERDERE DATI DURANTE IL MERGE E' NECESSARIO CONTROLLARE CHE GLI STESSI PAESI 
# ABBIANO LO STESSO NOME NEI DUE DATASET
# DAL DATASET ECONOMIC FREEDOM INDEX SOSTITUIAMO I SEGUENTI NOMI
# Brunei Darussalam = Brunei 
# Burma = Myanmar
# Congo, Democratic Republic of the Congo = Congo (Kinshasa)
# Congo, Republic of = Congo (Brazzaville)
# Cote d'Ivoire 
# Gambia = The Gambia
# Korea, North = North Korea
# Korea, South = South Korea
# Kyrgyz Republic = Kyrgyzstan
# Saint Lucia = St. Lucia
# Saint Vincent and the Grenadines = St. Vincent and the Grenadines
# Sao Tome and Principe

# E' INOLTRE IMPORTANTE CHE I DATI MANCANTI SIANO CATEGORIZZATI IN UN MODO SOLO
# CONTROLLARE IL DATASET E CATEGORIZZARE I DATI MANCANTI CON UNA CELLA VUOTA

#IMPORTARE DATASET FREEDOMHOUSE
All_data_FIW_2013_2021 <- read_excel("C:/Users/lucad/OneDrive/Desktop/Internship/Dataset freedom/All_data_FIW_2013-2021.xlsx",sheet = "FIW13-21")
#FILTRARE SOLO DATI DEL 2021
freedom_house<- filter(All_data_FIW_2013_2021, Edition=="2021")
#SELEZIONARE SOLO LE COLONNE A CUI SIAMO INTERESSATI
freedom_house<- data.frame(freedom_house[,1],freedom_house[,8:10],freedom_house[,12:15],freedom_house[,17:19],freedom_house[,24:27],freedom_house[,29:31],freedom_house[,33:36],freedom_house[,38:41],freedom_house[,6:7])
#I NOMI DELLE VARIABILI ATTUALI NON CI DICONO MOLTO
#SOSTITUIRLE CON DEI NOMI CHE CI DICONO QUALCOSA RENDERA' TUTTO PIU' INTUITIVO
colnames(freedom_house)<- c("Country_Name","Gov_Head_Election","Leg_Repres_Election",             
                            "Electoral_laws",                   "Parties_Organization",            
                            "Opposition_Opportunity",           "Free_From_Domination" ,           
                            "Minorities_Electoral_Opport",      "Policy_Determination",            
                            "Corruption_Safeguards",            "Government_Transparency",         
                            "Media_Freedom",                    "Religious_Expression",            
                            "Accademic_Freedom" ,               "Political_Expression",            
                            "Assembly_Freedom",                 "NGO_Freedom",                     
                            "Trade_Unions_Freedom",             "Judiciary_Independence",          
                            "Due_Process",                      "Protection_Illegittimate_Force",  
                            "Treatment_Equality",               "Mobility_Freedom",                
                            "Interference_In_Private_business", "Personal_Social_Freedom",         
                            "Equality_Of_Opportunities", "Political_Score", "Civil_Score")

#IMPORTARE DATASET ECONOMIC FREEDOM INDEX
index2021_data <- read_excel("C:/Users/lucad/OneDrive/Desktop/Internship/Dataset freedom/index2021_data.xlsx")
#SELEZIONARE SOLO LE COLONNE A CUI SIAMO INTERESSATI
economic_freedom<-data.frame(index2021_data[,2],index2021_data[,8:19],index2021_data[,7])
#FAR SI CHE LA VARIABILE CHE UTILIZZEREMO PER IL MERGE ABBIA LO STESSO NOME (NON E' INDISPENSABILE)
colnames(economic_freedom)[1]<-"Country_Name"
colnames(economic_freedom)[14]<-"Economical_Score"

#UNIAMO I DUE DATASET
dataset <- merge(freedom_house, economic_freedom, by="Country_Name")
#DIAMO UN ORDINE ALLE VARIABILI
dataset <- data.frame(dataset[,1:26],dataset[,29:41],dataset[,27:28])
#QUESTO DATASET CONTIENE I NOMI DEGLI STATI, 37 VARIABILI DI LIBERTA' (10 POLITICHE, 15 CIVILI E 12 ECONOMICHE)
#PIU' I 3 SCORE FINALI FORNITI DA FREEDOOM HOUSE E ECONOMIC INDEX FREEDOM PER OGNUNA DELLE 3 CATEGORIE 
#ATTENZIONE: GLI SCORE FINALI POLITICI E CIVILI VANNO DA 7 A 1 MENTRE QUELLO ECONOMICO DA 0 A 100


#BOXPLOT
library(ggpubr)

ggboxplot(dataset, y = "Political_Score", color='blue') +
  geom_hline(yintercept = mean(dataset$Political_Score), linetype = 2)+ 
  ylim(0, 7)

ggboxplot(dataset, y = "Civil_Score", color='red') +
  geom_hline(yintercept = mean(dataset$Civil_Score), linetype = 2)+ 
  ylim(0, 7)

ggboxplot(dataset, y = "Economical_Score", color='green') +
  geom_hline(yintercept = mean(dataset$Economical_Score), linetype = 2)+ 
  ylim(0, 100)

#PERCEZIONI SULL'ITALIA DA SONDAGGIO SUGLI STUDENTI
dataset_percezioni <- read_excel("C:/Users/lucad/OneDrive/Desktop/Internship/Shiny per Stage/dataset.xlsx")
ggplot( dataset_percezioni, aes(x=freedom_index_IT_0_10)) +
  geom_histogram( binwidth=1, fill="blue", color="light blue", alpha=0.9) +
  ggtitle("LIBERTA' PERCEPITA") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  xlim(0,10)+
  theme(
    plot.title = element_text(size=15)
  ) 
#CONTROLLIAMO DOVE SI COLLOCA L'ITALIA
summary(dataset[,39:41])
dataset[dataset$Country_Name=="Italy",39:41]
#DATE LE 37 VARIABILI INIZIALI SONO DAVVERO COSì INFORMATIVI I 3 SCORE FORNITI?


#ISTOGRAMMI
library(ggplot2)

ggplot( dataset, aes(x=Political_Score)) +
  geom_histogram( binwidth=1, fill="blue", color="light blue", alpha=0.9) +
  ggtitle("Libertà Politiche") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  theme(
    plot.title = element_text(size=15)
  ) 

ggplot( dataset, aes(x=Civil_Score)) +
  geom_histogram( binwidth=1, fill="Red", color="orange", alpha=0.9) +
  ggtitle("Libertà Civili") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  theme(
    plot.title = element_text(size=15)
  ) 

ggplot( dataset, aes(x=Economical_Score)) +
  geom_histogram( binwidth=1, fill="green", color="light green", alpha=0.9) +
  ggtitle("Libertà Economiche") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  theme(
    plot.title = element_text(size=15)
  ) 


#ELIMINIAMO I DATI MANCANTI
dataset_completo <- na.omit(dataset)
#QUESTO DATASET NON CONTIENE I PAESI IN CUI ABBIAMO DEI VALORI MANCANTI. PASSIAMO DA 184 PAESI A 178.
#NORMALIZZIAMO IL DATASET
dataset_completo[,2:38] <- scale(dataset_completo[,2:38])
dataset_completo_norm<- dataset_completo[,1:38]
#QUESTO DATASET E' NORMALIZZATO. CIO' CI PERMETTE DI AVERE DEI VALORI COMPARABILI PER TUTTE LE 37 VARIABILI.
#CI PERMETTE INOLTRE IN SEGUITO DI APPLICARE LA PRINCIPAL COMPONENT ANALYSIS CORRETTAMENTE
summary(dataset_completo_norm)


#CONTROLLARE LA DISTRIBUZIONE
#PRIMA DELLA NORMALIZZAZIONE
ggplot( dataset, aes(x=Labor.Freedom)) +
  geom_histogram( binwidth=8, fill="green", color="light green", alpha=0.9) +
  ggtitle("Mobility_Freedom") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  theme(
    plot.title = element_text(size=15)
  ) 

#DOPO LA NORMALIZZAZIONE
ggplot( dataset_completo, aes(x=Labor.Freedom)) +
  geom_histogram( binwidth=0.5, fill="green", color="light green", alpha=0.9) +
  ggtitle("Mobility_Freedom") + 
  xlab("Grado di libertà")+
  ylab("Frequenza")+
  theme(
    plot.title = element_text(size=15)
  ) 

#CORRELOGRAMMA
#CREIAMO DEI NOMI ABBRAVIATI PER LE NOSTRE VARIABILI IN MODO CHE POSSANO ESSERE USATI
#PER LA RAPPRESENTAZIONE
variabili_abbreviate<-c("Country_Name","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e11","e12")
dataset_completo_norm_abbr<-dataset_completo_norm
#SOSTITUIAMO I NOMI
colnames(dataset_completo_norm_abbr)<-variabili_abbreviate
#CREIAMO LA MATRICE DI CORRELAZIONI
library(corrplot)
dataset_corr = cor(dataset_completo_norm_abbr[,2:38])
#CORRELOGRAM PLOT
corrplot(dataset_corr)

#POLITICAL COMPASS
#CREIAMO DEI NOSTRI INDICATORI CHE RIASSUMANO CON UN VALORE OGNUNO
#DEI TRE TIPI DI LIBERTA'
#LA MEDIA E' IL MODO PIU' NATURALE
Political_Mean<-rowMeans(dataset_completo_norm_abbr[,2:11])
Civil_Mean<-rowMeans(dataset_completo_norm_abbr[,12:26])
Economical_Mean<-rowMeans(dataset_completo_norm_abbr[,27:38])
#CREIAMO INOLTRE UN INDICATORE CHE INDICHI INSIEME LE LIBERTA' POLITICHE E CIVILI
Political_Civil_Mean<-rowMeans(dataset_completo_norm_abbr[,2:26])
#UNIAMO I 4 INDICATORI IN UN UNICO DATAFRAME CHE UTILIZZEREMO IN MODO PIU' AGILE PER COSTRUIRE IL POLITICAL COMPASS
political_compass<-data.frame(Political_Mean,Civil_Mean,Economical_Mean,Political_Civil_Mean)
rownames(political_compass)<- dataset_completo_norm_abbr$Country_Name

#PERSONALIZZIAMO IL FONT, IL COLORE E LE DIMENSIONI DELLE SCRITTE CHE VOGLIAMO NEL GRAFICO
library(plotly)
t<- list(
  family = "sans serif",
  size = 14,
  color = toRGB("grey50"))

#GRAFICO 3D POLITICAL COMPASS
political_compass_plot3d <- plot_ly(x=political_compass[,1], y=political_compass[,2], z=political_compass[,3], type="scatter3d", mode="markers", text = rownames(political_compass))
political_compass_plot3d <- political_compass_plot3d %>% add_markers(size=2)
political_compass_plot3d <- political_compass_plot3d %>% add_text(textfont = t, textposition = "top right")
political_compass_plot3d

#GRAFICO 2D POLITICAL COMPASS
political_compass_plot2d<-plot_ly(x=(political_compass[,3]), y=(-political_compass[,4]), mode="markers", text = rownames(political_compass))
political_compass_plot2d <- political_compass_plot2d %>% add_markers()
political_compass_plot2d <- political_compass_plot2d %>% add_text(textfont = t, textposition = "top right")
political_compass_plot2d <- political_compass_plot2d %>% layout(title = 'Political Compass',
                                                                xaxis = list(title = 'Economic Left-Right'
                                                                ),
                                                                yaxis = list(title = 'Libertarian-Authouritarian'
                                                                ))
political_compass_plot2d



#PRINCIPAL COMPONENT ANALYSIS
library(factoextra)
library(REdaS)
#PER ESEGUIRE LA PRINCIPAL COMPONENT ANALISIS USIAMO dataset_completo_norm MA METTIAMO 
#I NOMI DEI PAESI NEL NOME DELLE RIGHE INVECE CHE IN UNA VARIABILE
dataset_pca<-dataset_completo_norm[,2:38]
rownames(dataset_pca)<-dataset_completo_norm[,1]
#CONTROLLIAMO CHE IL DATASET SIA CORRETTAMENTE NORMALIZZATO
means<-colMeans(dataset_pca)
sds<-apply(dataset_pca,2,sd)
descr<-round(cbind(means,sds),2)
descr

#TESTIAMO L'ADEGUATEZZA DEL DATASET PER UNA PRINCIPAL COMPONENT ANALYSIS
#Kaiser-Meyer-Olkin (KMO) criterium
KMOS(dataset_pca)
#Bartlett's test of sphericity
bart_spher(dataset_pca)

#CREIAMO UNA VARIABILE n = numero di osservazioni (paesi) E UNA VARIABILE p = numero di variabili

n<-nrow(dataset_pca)
p<-ncol(dataset_pca)

#EIGENVECTORS AND EIGENVALUES
cor_PCA<-cor(dataset_pca)
eigenval<-eigen(cor_PCA)$values
eigenvec<-eigen(cor_PCA)$vectors

#DECIDIAMO QUANTE COMPONENTI TENERE: NOI NE VOGLIAMO 2 O 3, MA SARANNO SUFFICIENTI?
#SCREEDIAGRAM 
eigen_dataframe<-data.frame(c(1:p),eigenval)
eigen_plot <- ggplot(data = eigen_dataframe, aes(x=eigen_dataframe[,1], y = eigen_dataframe[,2]))+
  geom_point(size=2)+
  geom_hline(yintercept = 1, color = "green", size= 1)+
  labs(x="Number of Components",y="Eigenvalue")+ ggtitle("SCREE PLOT")
eigen_plot

#PERCENTUALE CUMULATA DI VARIANZA SPIEGATA DA OGNI COMPONENTE
eigen_power<- eigenval/p
eigenpower_dataframe <- data.frame(c(1:p),cumsum(eigen_power))
power_plot <- ggplot(data = eigenpower_dataframe, aes(x=eigenpower_dataframe[,1], y = eigenpower_dataframe[,2]))+
  geom_point(size=2)+
  geom_hline(yintercept = 0.8, color = "green", size= 1)+
  labs(x="Number of Components",y="% of Variance Explained")+ ggtitle("VARIANCE EXPLAINED")
power_plot

#DESCRIZIONE RIASSUNTIVA DELLE TRE COMPONENTI PRINCIPALI IN CORRISPONDENZA DELLE VARIABILI ORIGINALI USANDO
#EIGENVECTORS AND EIGENVALUES. 
eigenvecdf <- data.frame(round(eigenvec[,1:3],3), row.names=colnames(dataset_pca))
colnames(eigenvecdf)<-c("Comp1","Comp2","Comp3")
eigendf<-rbind(eigenvecdf,eigenval[1:3])
rownames(eigendf)[38]<-"EIGENVALUE"
eigendf

#DESCRIZIONE RIASSUNTIVA DELLE TRE COMPONENTI PRINCIPALI IN CORRISPONDENZA DELLE VARIABILI ORIGINALI USANDO
#LOADINGS.
comp<-round(cbind(eigenvec[,1]*sqrt(eigenval[1]),eigenvec[,2]*sqrt(eigenval[2]),eigenvec[,3]*sqrt(eigenval[3])),3)
rownames(comp)<-row.names(descr)
colnames(comp)<-c("Comp1","Comp2","Comp3")
comp_var<-rbind(comp,eigen_power[1:3])
rownames(comp_var)[38]<-"% of VAR explained"
comp_var

#CCOMMUNALITIES
communality<-comp[1:37,1]^2+comp[1:37,2]^2+comp[1:37,3]^2
comp_comu<-cbind(comp[1:37,],communality)
comp_comu

#RAPPRESENTAZIONE GRAFICA DEGLI STATI NELL SPAZIO DELLE COMPONENTI PRINCIPALI
scores_groups <- as.matrix(dataset_pca)%*%as.matrix(eigenvec[,1:3])
scores2_groups<-cbind(scores_groups[,1]/sqrt(eigenval[1]),scores_groups[,2]/sqrt(eigenval[2]),scores_groups[,3]/sqrt(eigenval[3]))


a<- list(
  family = "sans serif",
  size = 14,
  color = toRGB("black"))

#RAPPRESENTAZIONE BIDIMENSONALE UTILIZZANDO LE PRIME DUE COMPONENTI PRINCIPALI
PCAplot_2d<-plot_ly(x=scores2_groups[,1], y=scores2_groups[,2], mode="markers", text = rownames(scores2_groups))
PCAplot_2d <- PCAplot_2d %>% add_markers(size=1)
PCAplot_2d <- PCAplot_2d %>% add_text(textfont = a, textposition = "top right")
PCAplot_2d

#RAPPRESENTAZIONE TRIDIMENSIONALE UTILIZZANDO LE PRIME TRE COMPONENTI PRINCIPALI
PCAplot_3d<-plot_ly(x=scores2_groups[,1], y=scores2_groups[,2], z=scores2_groups[,3], type="scatter3d", mode="markers", text = rownames(scores2_groups))
PCAplot_3d <- PCAplot_3d %>% add_markers(size=1)
PCAplot_3d <- PCAplot_3d %>% add_text(textfont = a, textposition = "top right")
PCAplot_3d

#RANKING FINALE DEI PAESI 
PCAplot_1d<-plot_ly(x=0, y=(-scores2_groups[,1]), mode="markers", text = rownames(scores2_groups))
PCAplot_1d <- PCAplot_1d %>% add_markers(size=1)
PCAplot_1d <- PCAplot_1d %>% add_text(textfont = a, textposition = "top right")
PCAplot_1d


#RAPPRESENTAZIONE GRAFICA DELLE VARIABILI ORIGINALI NELLO SPAZIO DELLE COMPONENTI PRINCIPALI: LOADINGS

b<- list(
  family = "sans serif",
  size = 30,
  color = toRGB("grey50"))

loadings<-plot_ly(x=comp[,1], y=(comp[,2]), mode="markers", text = colnames(dataset_completo_norm_abbr[2:38]))
loadings <- loadings %>% add_markers(size=4)
loadings <- loadings %>% add_text(textfont = b, textposition = "top right")
loadings <- loadings %>% layout(title = 'Loadings',
                                xaxis = list(title = 'First Component'
                                ),
                                yaxis = list(title = 'Second Component'
                                ))
loadings

loadings3d <-plot_ly(x=comp[,1], y=(comp[,2]), z=(comp[,3]), mode="markers", text = colnames(dataset_completo_norm_abbr[2:38]))
loadings3d <- loadings3d %>% add_markers()
loadings3d <- loadings3d %>% add_text(textfont = b, textposition = "top right")
loadings3d



#PRINCIPAL COMPONENT ANLYSIS CON FUNZIONI GIA' PRONTE
dataset_pca<-dataset_completo_norm[,2:38]
rownames(dataset_pca)<-dataset_completo_norm[,1]

res.pca <- prcomp(dataset_pca, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord[,1:3]          # Coordinates

# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord[,1:3]          # Coordinates
