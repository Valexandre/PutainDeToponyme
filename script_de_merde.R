library(tidyverse)
library(sf)
library(ragg)
library(rtweet)

NomsCommunes<-readRDS("data/NomsCommunes.Rdata")

Tourette<-c("abruti","andouille","bouffon","batard", "bite","baise","bande","branlette","bordel","burnes","chatte","chiasse","chiottes","con","conne","connard", "connerie","coucougnettes","couilles","cul", "chier","pipe","étron","garce","mange-merde","ordure","pédoc", "trainée","trou du cul","zguègue", "foutoir", "foutre","gland", "merde","merdier","pet", "pisse", "putain","pute","roubignoles","roupettes","roustons","turlutte","zigounette","zob")

NomsCommunesSeparees<-NomsCommunes%>%separate(col=NOM_COM,sep = "-| ",into=c("M1","M2","M3","M4","M5","M6","M7","M8"),remove = F)
NomsCommunesSeparees<-NomsCommunesSeparees%>%filter(!(grepl("Canton|Commune",NOM_COM)))

NomsCommunesSepareesG<-NomsCommunesSeparees%>%
  gather(-INSEE_COM,-NOM_COM,-libelle,-POPULATION,-INSEE_DEP,key="ordre",value="mot")%>%
  mutate(ordre=gsub("M","",ordre))%>%
  arrange(INSEE_DEP,INSEE_COM,ordre)

NomsCommunesSepareesG<-NomsCommunesSepareesG%>%
  filter(!is.na(mot))%>%
  mutate(insulteaplacer=case_when(mot%in%c("de","d'","en","sur", "et","lès","les","le","la","Le","La")~"non",
                                  lag(mot)%in%c("de","d'","en","sur", "et","lès","les","le","la","Le","La")~"oui",
                                  TRUE~"non"))
NomsCommunesSepareesG<-NomsCommunesSepareesG%>%
  rowwise()%>%
  mutate(randomnumber=sample(1:length(Tourette),1,replace = F))%>%
  mutate(avecinsulte=ifelse(insulteaplacer=="oui",paste0(Tourette[randomnumber]," de ",mot),mot))

Regroupees<-NomsCommunesSepareesG%>%group_by(INSEE_COM,libelle,NOM_COM,POPULATION)%>%
  summarise(NouveauNom=paste0(avecinsulte,collapse = " "))%>%
  mutate(NouveauNom=gsub("de A","d'A",gsub("de E","d'E",gsub("de I","d'I",gsub("de O","d'O",gsub("de U","d'U",gsub("de Y","d'Y",NouveauNom)))))))

Regroupees<-Regroupees%>%mutate(chgtcar=stringdist::stringdist(NouveauNom,NOM_COM))%>%filter(chgtcar>2)
Regroupees<-Regroupees%>%arrange(desc(POPULATION))%>%mutate(PourAffichage=paste0(NouveauNom," (",INSEE_COM,") - ",libelle))

CommunesavecJoli<-st_read("https://github.com/Valexandre/france-geojson/raw/master/Communes_PPCDOMTOMBAS_V2.gpkg")
CommunesavecJoli_ctd<-CommunesavecJoli%>%st_centroid()%>%st_transform(crs=2154)
CommunesavecJoli_ctd<-cbind(CommunesavecJoli_ctd%>%select(INSEE_DEP2,INSEE_COM)%>%st_drop_geometry(),
                            CommunesavecJoli_ctd%>%st_coordinates())

Deps<-st_read("https://github.com/Valexandre/france-geojson/raw/master/Deps_PPC_DOMTOM_BAS_4326_V2.geojson")%>%st_transform(crs=2154)

Regroupeesrnd<-sample(Regroupees$INSEE_COM,1)

ragg::agg_png(filename=paste0("GROSSIER_",Sys.Date(),"_",Regroupeesrnd,".png"),width = 625,height=900,units = "px")
              Deps%>%ggplot()+
  geom_sf(fill="#141E28",colour="white")+
                geom_point(data=CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd),aes(X,Y),colour="white",size=8)+
                geom_point(data=CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd),aes(X,Y),colour="red",size=5)+
  labs(title=str_to_title(str_wrap(as.character(CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd)%>%
              left_join(Regroupees)%>%select(PourAffichage)),40)),
       caption="Carte et code par ce connard de @humeursdevictor")+
  theme_void()+
  theme(panel.background = element_rect(fill="#141E28",colour="white"),
        text=element_text(size=26,colour="white"),plot.background = element_rect(fill="#141E28",colour=NA))
dev.off()
