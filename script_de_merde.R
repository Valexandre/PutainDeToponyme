library(tidyverse)
library(sf)
library(ragg)
library(rtweet)
#Je suis fatigué

tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)

CommunesavecJoli_ctd<-readRDS("data/CommunesavecJoli_ctd.Rdata")
Deps<-readRDS("data/Deps.Rdata")
Regroupees<-readRDS("data/VillesTourette.Rdata")

Regroupeesrnd<-sample(Regroupees$INSEE_COM,1)
Commune<-Regroupees$NouveauNom[Regroupees$INSEE_COM==Regroupeesrnd]
print(Commune)
imageaha<-paste0("data/",Sys.Date(),"_",Regroupeesrnd,".png")
print(imageaha)

#rtweet::post_tweet(status="MAIS TEST BORDEL")
Txtstatus<-paste0(sample(c("Salut les glandus. Au menu : ","Bonjour les gros lourds. Au menu du jour : ","Hello les blaireaux. Le petit nouveau : "),1), Commune,". #VillesDeLaTourette")
print (Txtstatus)

ragg::agg_png(filename=imageaha,width = 625,height=900, units = "px")
plot(Deps%>%ggplot()+
geom_sf(fill="#141E28",colour="white")+
geom_point(data=CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd),aes(X,Y),colour="white",size=8)+
geom_point(data=CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd),aes(X,Y),colour="red",size=5)+
  labs(title=str_to_title(str_wrap(as.character(CommunesavecJoli_ctd%>%filter(INSEE_COM==Regroupeesrnd)%>%
              left_join(Regroupees)%>%select(PourAffichage)),40)),
       caption="Carte et code par ce connard de @humeursdevictor")+
  theme_void()+
  theme(panel.background = element_rect(fill="#141E28",colour="white"),
        text=element_text(size=26,colour="white"),plot.background = element_rect(fill="#141E28",colour=NA)))
 invisible(dev.off())


#Envoie le ton tweet à la con
rtweet::post_tweet(status = Txtstatus, media=  imageaha, media_alt_text = Commune, token = tweetbot_token)
