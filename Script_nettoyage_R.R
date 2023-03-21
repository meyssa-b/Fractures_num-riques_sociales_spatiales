#Packages requis
library(openxlsx)
library(tidyverse)
library(sf)
library(ggplot2)
library(ggthemes)

#####################################################################Importation des données##########################################################################################################################################

#####################################################################CVL Eligibilité
setwd("~/Perso/Ecole/Master/Carto/Projet/Centre-Val_de_Loire_Base_Eligibilite_20210115/data/outputs")
cvl_elig=read.csv("Centre-Val_de_Loire_Base_Eligibilite_20210115.csv")
#####################################################################Immeuble avec info IRIS
setwd("~/Perso/Ecole/Master/Carto/Projet/MeDAS_Centre_Val_de_Loire")
fichier_immeuble <- read_csv(file = "input/Centre-Val_de_Loire_Base_Immeuble_20210115/data/outputs/Centre-Val_de_Loire_Base_Immeuble_20210115.csv")
fichier_iris <- st_read(dsn = "input/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-12-00282/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020/CONTOURS-IRIS.shp",
                        as_tibble = TRUE, crs = 2154)

immeubles_avec_localisation <- fichier_immeuble %>%
  filter(!is.na(imb_longitude)) %>%
  filter(!is.na(imb_latitude))
# De 1 190 000 immeubles, on passe à 1 189 902, soit 98 lignes sans localisation

immeubles_spatial <- st_as_sf(immeubles_avec_localisation,
                              coords = c("imb_longitude", "imb_latitude"),
                              crs = 3857 # EPSG 3857, WGS84 / Pseudo-Mercator, indiqué dans la doc
)
immeubles_Lambert93 <- immeubles_spatial %>%
  st_transform(2154)

immeubles_iris <- immeubles_Lambert93 %>%
  st_join(fichier_iris)

##################################################################### Fusion Données CVL
cvl=left_join(cvl_elig,immeubles_iris,by="imb_id")

rm(immeubles_Lambert93,immeubles_avec_localisation) #Suppression de datas frames inutilisables

#################################Selection des variables à retirer dans CVL et supression des NA
cvl=cvl %>% select(-c("addr_code.x","addr_code.y","imb_code.x","imb_code.y","imb_source","imb_num","addr_numero","addr_rep","addr_nom_voie","addr_nom_ld",
                      "addr_id_fantoir","addr_source")) %>% drop_na("imb_code_insee") 

################################# Transformation des variables en facteurs
cvl$imb_id=as.factor(cvl$imb_id)
cvl$code_techno=as.factor(cvl$code_techno)
cvl$classe_debit_montant=as.factor(cvl$classe_debit_montant)
cvl$classe_debit_descendant=as.factor(cvl$classe_debit_descendant)
cvl$saturation=as.factor(cvl$saturation)
cvl$imb_code_insee=as.factor(cvl$imb_code_insee)
cvl$addr_code_insee=as.factor(cvl$addr_code_insee)
cvl$addr_nom_commune=as.factor(cvl$addr_nom_commune)
cvl$CODE_IRIS=as.factor(cvl$CODE_IRIS)
cvl$INSEE_COM=as.factor(cvl$INSEE_COM)
cvl$NOM_COM=as.factor(cvl$NOM_COM)
cvl$IRIS=as.factor(cvl$IRIS)
cvl$imb_type=as.factor(cvl$imb_type)
cvl$NOM_IRIS=as.factor(cvl$NOM_IRIS)
cvl$TYP_IRIS=as.factor(cvl$TYP_IRIS)
################################# Regroupement des modalités pour la variable code_techno
cvl$code_techno_fibre <- as.character(cvl$code_techno)
cvl$code_techno_fibre[cvl$code_techno_fibre == "FO"] <- "Fibre"
cvl$code_techno_fibre[cvl$code_techno_fibre == "4GF" |cvl$code_techno_fibre == "COAX" |cvl$code_techno_fibre == "CU" |cvl$code_techno_fibre == "SAT" |
                        cvl$code_techno_fibre == "THDR"|cvl$code_techno_fibre == "WMX"] <- "Non Fibre"
cvl$code_techno_fibre <- as.factor(cvl$code_techno_fibre)

cvl$code_dep=substr(cvl$addr_code_insee,1,2)
cvl=cvl %>% filter(code_dep ==45 | code_dep ==18 | code_dep ==28 | code_dep ==37 | code_dep == 36 | code_dep ==41)
##################################################################### Données Logement IRIS
setwd("~/Perso/Ecole/Master/Carto/Projet/INSEE_LOGEMENTS_IRIS-COMMUNES_2015")
logement_iris=read.xlsx("INSEE_LOGEMENTS_IRIS-COMMUNES_2015.xlsx")

#################################Remplacement des noms des colonnes apr la 1er ligne
colnames(logement_iris)=logement_iris[1,]
logement_iris=logement_iris[-1,]

#################################Selection des variables à utiliser et tranformation en var numériques
logement_iris=logement_iris %>% select(c("IRIS","REG","DEP","COM","LIBCOM","TRIRIS","TYP_IRIS","P15_RP","P15_RSECOCC","P15_LOGVAC","P15_MAISON","P15_APPART",
                                         "P15_RP_ACHTOT","P15_RP_ACH19","P15_RP_ACH45","P15_RP_ACH70","P15_RP_ACH90","P15_RP_ACH05","P15_RP_ACH12",
                                         "P15_MEN","P15_PMEN","P15_PMEN_ANEM0002","P15_PMEN_ANEM0204","P15_PMEN_ANEM0509","P15_PMEN_ANEM10P","P15_RP_PROP",
                                         "P15_RP_LOC","P15_NPER_RP","P15_ANEM_RP")) 

#Tri des départements de CVL
logement_iris = logement_iris %>%
  filter(logement_iris$DEP ==45 | logement_iris$DEP ==18 | logement_iris$DEP ==28 | logement_iris$DEP ==37 | logement_iris$DEP == 36 | logement_iris$DEP ==41)


var=colnames(logement_iris[,8:29])
for(i in var){
  logement_iris[,i]=as.numeric(logement_iris[,i])
}

#Création du data frame logement au niveau communal
logement_comm=logement_iris %>% distinct(COM)
logement_comm$REG=logement_iris$REG[match(logement_comm$COM,logement_iris$COM)]
logement_comm$DEP=logement_iris$DEP[match(logement_comm$COM,logement_iris$COM)]
logement_comm$LIBCOM=logement_iris$LIBCOM[match(logement_comm$COM,logement_iris$COM)]

for(i in var){
  logement_comm[,i]=tapply(logement_iris[,i], logement_iris[,4],sum)
}
#Création du data frame logement au niveau départemental
logement_dep=logement_iris %>% distinct(DEP)
logement_dep$REG=logement_iris$REG[match(logement_dep$DEP,logement_iris$DEP)]

for(i in var){
  logement_dep[,i]=tapply(logement_iris[,i], logement_iris[,3],sum)
}
##################################################################Import données revenu
setwd("~/Perso/Ecole/Master/Carto/Projet/INSEE_REVENUS_DISPONIBLES_2016")
revenu_comm=read.xlsx("INSEE_REVENUS_DISPONIBLES_COMMUNES_2016.xlsx",sheet = "ENSEMBLE")
revenu_iris=read.xlsx("INSEE_REVENUS_DISPONIBLES_IRIS_2016.xlsx",startRow = 6)

#################################Selection des variables de revenu à utiliser
revenu_iris=revenu_iris %>% select(-c(9:32))
revenu_comm=revenu_comm %>% select(-c(9:31))
revenu_iris=revenu_iris %>% drop_na(c("DISP_Q116","DISP_TP6016")) 
revenu_comm=revenu_comm %>% select(-c("Q116","Q316"))

#Création du code département
revenu_comm$code_dep=substr(revenu_comm$CODGEO,1,2)

#Création du revenu au niveau départemental
revenu_dep=revenu_comm %>% distinct(code_dep)
var_rev=colnames(revenu_comm[,3:5])
for(i in var_rev){
  revenu_dep[,i]=tapply(revenu_comm[,i], revenu_comm[,7],sum)
}
revenu_dep$Q216=tapply(revenu_comm$Q216, revenu_comm[,7],mean)


interm=revenu_iris %>% distinct(COM)
interm$tx_pauvre_60=tapply(revenu_iris$DISP_TP6016, revenu_iris[,3],mean)
cvl$tx_pauvre=interm$tx_pauvre_60[match(cvl$addr_code_insee,interm$COM)]

##################################################################Import données complémentaires
setwd("~/Perso/Ecole/Master/Carto/Projet/Datas_complementaires_communes_CVL")
educ=read.csv("Niveau_diplome_CVL.csv",sep=";",encoding = "UTF-8")
age=read.csv("Tranches_age_CVL.csv",sep=";",encoding = "UTF-8")
emploi=read.csv("CSP_emploi_CVL.csv",sep=";",encoding = "UTF-8")

var_age=colnames(age[5:11])
for(i in var_age){
  assign(i,age %>% select(Code.géographique,i) %>% drop_na())
}

age=unique(age[1])
age$X0.14_ans=X0.14.ans[,2]
age$X15.29.ans=X15.29.ans[,2]
age$X30.44.ans=X30.44.ans[,2]
age$X45.59.ans=X0.14.ans[,2]
age$X60.74.ans=X60.74.ans[,2]
age$X75.89.ans=X75.89.ans[,2]
age$X90.ans.ou.plus=X90.ans.ou.plus[,2]
age$Code.géographique=as.character(age$Code.géographique)
age$code_dep=substr(age$Code.géographique,1,2)
rm(X0.14.ans,X15.29.ans,X30.44.ans,X45.59.ans,X60.74.ans,X75.89.ans,X90.ans.ou.plus)

age_dep=as.data.frame(unique(age$code_dep))
names(age_dep)[1]="DEP"
var_age=colnames(age[2:8])
for(i in var_age){
  age_dep[,i]=tapply(age[,i],age$code_dep,sum)
}

var_emploi=colnames(emploi[5:10])
for(i in var_emploi){
  assign(i,emploi %>% select(Code.géographique,i) %>% drop_na())
}

emploi=unique(emploi[1])
emploi$Ouvriers=Ouvriers[,2]
emploi$Artisans..Commerçants..Chefs.entreprise=Artisans..Commerçants..Chefs.entreprise[,2]
emploi$Prof..intermédiaires=Prof..intermédiaires[,2]
emploi$Agriculteurs.exploitants=Agriculteurs.exploitants[,2]
emploi$Cadres.Prof..intel..sup.=Cadres.Prof..intel..sup.[,2]
emploi$Code.géographique=as.character(emploi$Code.géographique)
emploi$code_dep=substr(emploi$Code.géographique,1,2)
rm(Ouvriers,Artisans..Commerçants..Chefs.entreprise,Prof..intermédiaires,Agriculteurs.exploitants,Cadres.Prof..intel..sup.)

emploi_dep=as.data.frame(unique(emploi$code_dep))
names(emploi_dep)[1]="DEP"
var_emploi=colnames(emploi[2:6])
for(i in var_emploi){
  emploi_dep[,i]=tapply(emploi[,i],emploi$code_dep,sum)
}

educ=as.data.frame(tapply(educ$Population.non.scolarisée.15.ans.ou.plus,educ[,c("Code.géographique","Niveau.de.diplôme.obtenu")],sum))
educ$code_geo=rownames(educ)
educ$code_dep=substr(educ$code_geo,1,2)

educ_dep=as.data.frame(unique(educ$code_dep))
names(educ_dep)[1]="DEP"
var_educ=colnames(educ[1:5])
for(i in var_educ){
  educ_dep[,i]=tapply(educ[,i],educ$code_dep,sum)
}

#####################################################################################################################################
#####################################################################GROS FICHIER####################################################################
################################################################################################################################
################################# Fusion table CVL avec Revenu iris et Iris logement: Obtenir table d'info à l'iris
cvl_iris=left_join(cvl,logement_iris,by=c("CODE_IRIS"="IRIS"))
cvl_iris$taux_pauvre_60=revenu_iris$DISP_TP6016[match(cvl_iris$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris$Premier_quart=revenu_iris$DISP_Q116[match(cvl_iris$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris$Mediane=revenu_iris$DISP_MED16[match(cvl_iris$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris$Trois_quart=revenu_iris$DISP_MED16[match(cvl_iris$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris=left_join(cvl_iris,age,by=c("COM"="Code.géographique"))
cvl_iris=left_join(cvl_iris,emploi,by=c("COM"="Code.géographique"))
cvl_iris=left_join(cvl_iris,educ,by=c("COM"="code_geo"))

cvl_iris=cvl_iris %>% drop_na("P15_RP")

cvl_iris=cvl_iris %>% relocate("imb_id","REG","DEP","COM","LIBCOM","addr_code_insee","imb_code_insee","addr_nom_commune","CODE_IRIS","IRIS","NOM_IRIS","TRIRIS")
cvl_iris=cvl_iris %>% relocate("code_techno_fibre",.after="code_techno")

cvl_iris=cvl_iris %>% select(-c("imb_code_insee","addr_code_insee","addr_nom_commune","NOM_COM","INSEE_COM","TYP_IRIS.x","TYP_IRIS.y"))


head(cvl_iris)
summary(cvl_iris)
#####################################################################################################################################
#####################################################################PETIT FICHIER####################################################################
################################################################################################################################
################################# Fusion table CVL avec Revenu iris et Iris logement: Obtenir table d'info à l'iris
Freq_techno=as.data.frame(table(cvl$code_techno,cvl$CODE_IRIS))
iris=cvl %>% distinct(sort(CODE_IRIS))
iris$fo=Freq_techno$Freq[which(Freq_techno=="FO")]
iris$co_4gf=Freq_techno$Freq[which(Freq_techno=="4GF")]
iris$coax=Freq_techno$Freq[which(Freq_techno=="COAX")]
iris$cu=Freq_techno$Freq[which(Freq_techno=="CU")]
iris$sat=Freq_techno$Freq[which(Freq_techno=="SAT")]
iris$thdr=Freq_techno$Freq[which(Freq_techno=="THDR")]
iris$wmx=Freq_techno$Freq[which(Freq_techno=="WMX")]
iris$tx_fo=(iris$fo/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_4gf=(iris$co_4gf/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_coax=(iris$coax/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_cu=(iris$cu/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_sat=(iris$sat/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_thdr=(iris$thdr/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
iris$tx_wmx=(iris$wmx/(iris$fo+iris$co_4gf+iris$coax+iris$cu+iris$sat+iris$thdr+iris$wmx))*100
names(iris)[1]="CODE_IRIS"
iris$IRIS=cvl$IRIS[match(iris$CODE_IRIS,cvl$CODE_IRIS)]
iris$NOM_IRIS=cvl$NOM_IRIS[match(iris$CODE_IRIS,cvl$CODE_IRIS)]
iris$tx_pauvre=cvl$tx_pauvre[match(iris$CODE_IRIS,cvl$CODE_IRIS)]

cvl_iris_agrege=left_join(iris,logement_iris,by=c("CODE_IRIS"="IRIS"))
cvl_iris_agrege$taux_pauvre_60=revenu_iris$DISP_TP6016[match(cvl_iris_agrege$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris_agrege$Premier_quart=revenu_iris$DISP_Q116[match(cvl_iris_agrege$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris_agrege$Mediane=revenu_iris$DISP_MED16[match(cvl_iris_agrege$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris_agrege$Trois_quart=revenu_iris$DISP_MED16[match(cvl_iris_agrege$CODE_IRIS,revenu_iris$IRIS)]
cvl_iris_agrege=left_join(cvl_iris_agrege,age,by=c("COM"="Code.géographique"))
cvl_iris_agrege=left_join(cvl_iris_agrege,emploi,by=c("COM"="Code.géographique"))
cvl_iris_agrege=left_join(cvl_iris_agrege,educ,by=c("COM"="code_geo"))

cvl_iris_agrege=cvl_iris_agrege %>% drop_na("P15_RP")

cvl_iris_agrege=cvl_iris_agrege %>% relocate("REG","DEP","COM","LIBCOM","CODE_IRIS","IRIS","NOM_IRIS","TRIRIS")
cvl_iris_agrege=cvl_iris_agrege %>% relocate("fibre","non_fibre","tx_fibre","tx_non_fibre",.after="TYP_IRIS")

cvl_iris_agrege=cvl_iris_agrege %>% select(-c("code_dep.x","code_dep.y","code_dep"))

head(cvl_iris_agrege)
summary(cvl_iris_agrege)

setwd("~/Perso/Ecole/Master/Carto/Projet")
write.csv(cvl_iris_agrege,"cvl_iris_agrege.csv")
#####################################################################################################################################
#####################################################################GROS FICHIER####################################################################
################################################################################################################################
################################ Fusion table CVL avec Revenu comm et Iris logement: Obtenir table d'info à la commune
cvl_comm=left_join(cvl,logement_comm,by=c("addr_code_insee"="COM"))
cvl_comm$nb_men=revenu_comm$NBMEN16[match(cvl_comm$addr_code_insee,revenu_comm$CODGEO)]
cvl_comm$nb_pers=revenu_comm$NBPERS16[match(cvl_comm$addr_code_insee,revenu_comm$CODGEO)]
cvl_comm$nb_uc=revenu_comm$NBUC16[match(cvl_comm$addr_code_insee,revenu_comm$CODGEO)]
cvl_comm$Mediane=revenu_comm$Q216[match(cvl_comm$addr_code_insee,revenu_comm$CODGEO)]
cvl_comm=left_join(cvl_comm,age,by=c("INSEE_COM"="Code.géographique"))
cvl_comm=left_join(cvl_comm,emploi,by=c("INSEE_COM"="Code.géographique"))
cvl_comm=left_join(cvl_comm,educ,by=c("INSEE_COM"="code_geo"))

cvl_comm=cvl_comm %>% drop_na("P15_RP","X0.14_ans")

cvl_comm=cvl_comm %>% relocate("imb_id","REG","DEP","INSEE_COM","LIBCOM","addr_code_insee","imb_code_insee","addr_nom_commune","CODE_IRIS","IRIS","NOM_IRIS")
cvl_comm=cvl_comm %>% relocate("code_techno_fibre",.after="code_techno")

cvl_comm=cvl_comm %>% select(-c("imb_code_insee","addr_code_insee","addr_nom_commune","CODE_IRIS","IRIS","NOM_IRIS","NOM_COM","INSEE_COM","TYP_IRIS","code_dep"))

head(cvl_comm)
summary(cvl_comm)
#####################################################################################################################################
#####################################################################PETIT FICHIER####################################################################
################################################################################################################################
################################ Fusion table CVL avec Revenu comm et Iris logement: Obtenir table d'info à la commune
Freq_techno=as.data.frame(table(cvl$code_techno_fibre,cvl$addr_code_insee))
comm=Freq_techno %>% distinct(sort(Var2))
comm$fibre=Freq_techno$Freq[which(Freq_techno=="Fibre")]
comm$non_fibre=Freq_techno$Freq[which(Freq_techno=="Non Fibre")]
comm$tx_fibre=(comm$fibre/(comm$fibre+comm$non_fibre))*100
comm$tx_non_fibre=(comm$non_fibre/(comm$fibre+comm$non_fibre))*100
names(comm)[1]="COM"
comm$NOM_COM=cvl$NOM_COM[match(comm$COM,cvl$INSEE_COM)]
comm$tx_pauvre=cvl$tx_pauvre[match(comm$COM,cvl$INSEE_COM)]

cvl_comm_agrege=left_join(comm,logement_comm,by=c("COM"="COM"))
cvl_comm_agrege$nb_men=revenu_comm$NBMEN16[match(cvl_comm_agrege$COM,revenu_comm$CODGEO)]
cvl_comm_agrege$nb_pers=revenu_comm$NBPERS16[match(cvl_comm_agrege$COM,revenu_comm$CODGEO)]
cvl_comm_agrege$nb_uc=revenu_comm$NBUC16[match(cvl_comm_agrege$COM,revenu_comm$CODGEO)]
cvl_comm_agrege$Mediane=revenu_comm$Q216[match(cvl_comm_agrege$COM,revenu_comm$CODGEO)]
cvl_comm_agrege=left_join(cvl_comm_agrege,age,by=c("COM"="Code.géographique"))
cvl_comm_agrege=left_join(cvl_comm_agrege,emploi,by=c("COM"="Code.géographique"))
cvl_comm_agrege=left_join(cvl_comm_agrege,educ,by=c("COM"="code_geo"))

cvl_comm_agrege=cvl_comm_agrege %>% drop_na("P15_RP","X0.14_ans")
cvl_comm_agrege=cvl_comm_agrege %>% relocate("REG","DEP","COM","LIBCOM")
cvl_comm_agrege=cvl_comm_agrege %>% relocate("fibre","non_fibre","tx_fibre","tx_non_fibre",.after="LIBCOM")
cvl_comm_agrege=cvl_comm_agrege %>% select(-c("NOM_COM","code_dep"))

head(cvl_comm_agrege)
summary(cvl_comm_agrege)

setwd("~/Perso/Ecole/Master/Carto/Projet")
write.csv(cvl_comm_agrege,"cvl_comm_agrege.csv")
#####################################################################################################################################
#####################################################################GROS FICHIER####################################################################
################################################################################################################################
################################# Fusion table CVL avec Revenu comm et Iris logement: Obtenir table d'info au departement
age$code_dep=substr(age$Code.géographique,1,2)
emploi$code_dep=substr(emploi$Code.géographique,1,2)
educ$code_dep=substr(educ$code_geo,1,2)

cvl_dep=left_join(cvl,logement_dep,by=c("code_dep"="DEP"))
cvl_dep$nb_men=revenu_dep$NBMEN16[match(cvl_dep$code_dep,revenu_dep$code_dep)]
cvl_dep$nb_pers=revenu_dep$NBPERS16[match(cvl_dep$code_dep,revenu_dep$code_dep)]
cvl_dep$nb_uc=revenu_dep$NBUC16[match(cvl_dep$code_dep,revenu_dep$code_dep)]
cvl_dep$Mediane_revenu=revenu_dep$Q216[match(cvl_dep$code_dep,revenu_dep$code_dep)]
cvl_dep=left_join(cvl_dep,age_dep,by=c("code_dep"="DEP"))
cvl_dep=left_join(cvl_dep,emploi_dep,by=c("code_dep"="DEP"))
cvl_dep=left_join(cvl_dep,educ_dep,by=c("code_dep"="DEP"))

cvl_dep=cvl_dep %>% drop_na("P15_RP")

cvl_dep=cvl_dep %>% relocate("imb_id","REG","code_dep")
cvl_dep=cvl_dep %>% relocate("code_techno_fibre",.after="code_techno")  %>% drop_na("nb_men")

cvl_dep$code_dep=as.factor(cvl_dep$code_dep)

head(cvl_dep)
summary(cvl_dep)
####################################################################################################################################
####################################################################PETIT FICHIER####################################################################
###############################################################################################################################
################################ Fusion table CVL avec Revenu comm et Iris logement: Obtenir table d'info au departement
age$code_dep=substr(age$Code.géographique,1,2)
emploi$code_dep=substr(emploi$Code.géographique,1,2)
educ$code_dep=substr(educ$code_geo,1,2)

Freq_techno=as.data.frame(table(cvl$code_techno_fibre,cvl$code_dep))
dep=cvl %>% distinct(sort(code_dep))
dep$fibre=Freq_techno$Freq[which(Freq_techno=="Fibre")]
dep$non_fibre=Freq_techno$Freq[which(Freq_techno=="Non Fibre")]
dep$tx_fibre=(dep$fibre/(dep$fibre+dep$non_fibre))*100
dep$tx_non_fibre=(dep$non_fibre/(dep$fibre+dep$non_fibre))*100
names(dep)[1]="DEP"
dep$tx_pauvre=cvl$tx_pauvre[match(dep$DEP,cvl$code_dep)]

cvl_dep_agrege=left_join(dep,logement_dep,by=c("DEP"="DEP"))
cvl_dep_agrege$nb_men=revenu_dep$NBMEN16[match(cvl_dep_agrege$DEP,revenu_dep$code_dep)]
cvl_dep_agrege$nb_pers=revenu_dep$NBPERS16[match(cvl_dep_agrege$DEP,revenu_dep$code_dep)]
cvl_dep_agrege$nb_uc=revenu_dep$NBUC16[match(cvl_dep_agrege$DEP,revenu_dep$code_dep)]
cvl_dep_agrege$Mediane_revenu=revenu_dep$Q216[match(cvl_dep_agrege$DEP,revenu_dep$code_dep)]
cvl_dep_agrege=left_join(cvl_dep_agrege,age_dep,by=c("DEP"="DEP"))
cvl_dep_agrege=left_join(cvl_dep_agrege,emploi_dep,by=c("DEP"="DEP"))
cvl_dep_agrege=left_join(cvl_dep_agrege,emploi_dep,by=c("DEP"="DEP"))

cvl_dep_agrege=cvl_dep_agrege %>% drop_na("P15_RP","nb_men")

cvl_dep_agrege=cvl_dep_agrege %>% relocate("imb_id","REG","code_dep")
cvl_dep_agrege=cvl_dep_agrege %>% relocate("code_techno_fibre",.after="code_techno")

head(cvl_dep_agrege)
summary(cvl_dep_agrege)

setwd("~/Perso/Ecole/Master/Carto/Projet")
write.csv(cvl_dep_agrege,"cvl_dep_agrege.csv")
