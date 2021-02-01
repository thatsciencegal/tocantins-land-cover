remotes::install_github("coolbutuseless/ggpattern")

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggpattern)
library(forcats)

##Data frame with dam names and opening dates
dam.dates <- data.frame(dam = c("sdm","cabr","sasa","pean","laje","estr"),year=c(1997,2002,2009,2006,2001,2011))
dam.dates$dam<-fct_relevel(dam.dates$dam,c("sdm","cabr","sasa","pean","laje","estr"))

##Label to change dam abbreviations to names
dam.labs <- c(sdm="Serra da Mesa",
              cabr="Cana Brava",
              sasa="São Salvador",
              pean="Peixe Angical",
              laje="Lajeado",
              estr="Estreito")

calc.lc<-function(year,lc,dam_name){
  ##Fucntion to calculate land cover percent/area for each year
  super_df<-data.frame()
  for(i in 1:length(year)){
    lulc.sm<-lc[[i]]
    tmp <- as.vector(lulc.sm)
    print("Made vector")
    df <- data.frame(x = tmp)
    df$x[df$x==0]<-NA
    print("Made data frame")
    df2 <- df %>%
      count(x) %>%
      filter(!is.na(x)) %>%
      mutate(perc = (n/sum(n))*100)
    print(paste0("Counted % LC ", i))
    df2 <- cbind(year = year[i], df2)
    print(paste0("Added year to LC ",i))
    super_df <- rbind(super_df, df2)
    print(paste0("Calculated LULC % ",i))
  }
  super_df %>% mutate(x=case_when(x == 3 ~ "forest",
                                  x == 4 ~ "savanna",
                                  x == 9 ~ "forest_plantation",
                                  x == 12 ~ "grassland",
                                  x == 13 ~ "non_forest_natural",
                                  x == 15 ~ "pasture",
                                  x == 18 ~ "ag",
                                  x == 19 ~ "crop",
                                  x == 20 ~ "semi_per_crop",
                                  x == 21 ~ "ag_pasture",
                                  x == 24 ~ "urban",
                                  x == 25 ~ "other_non_veg",
                                  x == 29 ~ "rock",
                                  x == 30 ~ "mining",
                                  x == 33 ~ "river"),
                            area=(n*30*30)/(1000*1000),
                            dam=dam_name)
}

defor <- function(t1,t2,A2,A1){
  ##Function to calculate deforestation
  ((1/(t2-t1))*log(A2/A1))*100
}

year <- seq(1985,2018, by=1)

##Vegetation in undammed riparian buffer (between reservoirs)
to.veg <- stack("./Data/GIS/Vegetation/VegetationClip/to_buff.tif")[[-35]]

to.veg.df <- calc.lc(year=year,lc=to.veg,dam_name = NA)

to.veg.area <- pivot_wider(to.veg.df,id_col=year,names_from="x",values_from="area")

write.csv(to.veg.df,"./Data/GIS/Vegetation/to_veg_500.csv")

to.buff.sm <- to.veg.df %>% filter(x == "forest" | x == "savanna" | x == "pasture") 

p1 <- ggplot(to.buff.sm, aes(x=year,y=perc,color=x))+geom_line(size=1)+ 
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#35978f","#dfc27d","#8c510a"),name="Cover type",labels=c("Forest","Pasture","Savanna"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"),
        legend.position="none")+
  ylim(c(0,50))+
  xlab("Year")+
  ylab("Percent land cover")+
  ggtitle("(A) 0-500 m buffer")

##Tocantins 500-1000 m buffer
to.1k <- stack("./Data/GIS/Vegetation/VegetationClip/to_1k_df.tif")

to.1k.df <- calc.lc(year=year,lc=to.1k,dam_name=NA)

to.1k.area <- pivot_wider(to.1k.df,id_col=year,names_from="x",values_from="area")

write.csv(to.1k.df,"./Data/GIS/Vegetation/to1k_diff_veg.csv")

to1k.sm <- to.1k.df %>% filter(x == "forest" | x == "savanna" | x == "pasture")
to1k.sm$x <- fct_relevel(to1k.sm$x, "forest","savanna","pasture")

p2 <- ggplot(to1k.sm, aes(x=year,y=perc,color=x))+
  geom_line(size=1)+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#35978f","#8c510a","#dfc27d"),name="Cover type",labels=c("Forest","Savanna","Pasture"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"),
        legend.position = "none")+
  ylim(0,50)+
  xlab("Year")+
  ylab("")+
  ggtitle("(B) 500-1000 m buffer")
p2

(p1|p2)
ggsave("./Images/fig3.png",dpi=300,height=3,width=7)

##Deforestation calculations
##Isolate the different land cover and times
covs <- data.frame(t2=c(2018,2018,2018,2018,2018,2018),
                   t1=c(1985,1985,1985,1985,1985,1985),
                   A2=c(to.veg.area$forest[34],to.veg.area$savanna[34],to.veg.area$pasture[34],
                        to.1k.area$forest[34],to.1k.area$savanna[34],to.1k.area$pasture[34]),
                   A1=c(to.veg.area$forest[1],to.veg.area$savanna[1],to.veg.area$pasture[1],
                        to.1k.area$forest[1],to.1k.area$savanna[1],to.1k.area$pasture[1]),
                   buff=c(500,500,500,1000,1000,1000),
                   lc=c("forest","savanna","pasture","forest","savanna","pasture"))

##Perform deforestation rate calculation
lot.rate<-covs %>% rowwise() %>% 
  mutate(rate=defor(t1=t1,t2=t2,A2=A2,A1=A1))

##Relevel factors to keep in certain order
lot.rate$lc <- fct_relevel(lot.rate$lc, "forest","savanna","pasture")
lot.rate$buff <- fct_relevel(as.character(lot.rate$buff),"500","1000")

##Vegetation in 500 m buffer around reservoir
##Serra da Mesa
sdm.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sdm_buff.tif")[[-35]]

sdm.buff.veg.df <- calc.lc(year=year,lc=sdm.buff.veg,dam="sdm")
write.csv(sdm.buff.veg.df,"./Data/GIS/Vegetation/sdm_buff_veg.csv")

##Cana Brava
cabr.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/cabr_buff.tif")[[-35]]

cabr.buff.veg.df <- calc.lc(year=year,lc=cabr.buff.veg,dam="cabr")
write.csv(cabr.buff.veg.df,"./Data/GIS/Vegetation/cabr_buff_veg.csv")

##São Salvador
sasa.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sasa_buff.tif")[[-35]]

sasa.buff.veg.df <- calc.lc(year=year,lc=sasa.buff.veg,dam="sasa")
write.csv(sasa.buff.veg.df,"./Data/GIS/Vegetation/sasa_buff_veg.csv")

##Peixe Angical
pean.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/pean_buff.tif")[[-35]]

pean.buff.veg.df <- calc.lc(year=year,lc=pean.buff.veg,dam="pean")
write.csv(pean.buff.veg.df,"./Data/GIS/Vegetation/pean_buff_veg.csv")

##Lajeado
laje.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/laje_buff.tif")[[-35]]

laje.buff.veg.df <- calc.lc(year=year,lc=laje.buff.veg,dam="laje")
write.csv(laje.buff.veg.df,"./Data/GIS/Vegetation/laje_buff_veg.csv")

##Estreito
estr.buff.veg <- stack("./Data/GIS/Vegetation/VegetationClip/estr_buff.tif")[[-35]]

estr.buff.veg.df <- calc.lc(year=year,lc=estr.buff.veg,dam="estr")
write.csv(estr.buff.veg.df,"./Data/GIS/Vegetation/estr_buff_veg.csv")

##Combine all into 1 data frame and filter out land cover of interest
res.buff <- rbind(sdm.buff.veg.df,cabr.buff.veg.df,sasa.buff.veg.df,pean.buff.veg.df,laje.buff.veg.df,estr.buff.veg.df)
res.buff.sm <- res.buff %>% filter(x == "forest" | x == "savanna" | x == "pasture")

##Vegetation in 500 m buffer around pre-dam river
##Serra da Mesa
sdm.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sdm_riv.tif")[[-35]]

sdm.pre.veg.df <- calc.lc(year,sdm.pre.veg,"sdm")
write.csv(sdm.pre.veg.df,"./Data/GIS/Vegetation/sdm_pre_veg.csv")

##Cana Brava
cabr.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/cabr_riv.tif")[[-35]]

cabr.pre.veg.df <- calc.lc(year,cabr.pre.veg,"cabr")
write.csv(cabr.pre.veg.df,"./Data/GIS/Vegetation/cabr_pre_veg.csv")

##São Salvador
sasa.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sasa_riv.tif")[[-35]]

sasa.pre.veg.df <- calc.lc(year,sasa.pre.veg,"sasa")
write.csv(sasa.pre.veg.df,"./Data/GIS/Vegetation/sasa_pre_veg.csv")

##Peixe Angical
pean.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/pean_riv.tif")[[-35]]

pean.pre.veg.df <- calc.lc(year,pean.pre.veg,"pean")
write.csv(pean.pre.veg.df,"./Data/GIS/Vegetation/pean_pre_veg.csv")

##Lajeado
laje.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/laje_riv.tif")[[-35]]

laje.pre.veg.df <- calc.lc(year,laje.pre.veg,"laje")
write.csv(laje.pre.veg.df,"./Data/GIS/Vegetation/laje_pre_veg.csv")

##Estreito
estr.pre.veg <- stack("./Data/GIS/Vegetation/VegetationClip/estr_riv.tif")[[-35]]

estr.pre.veg.df <- calc.lc(year,estr.pre.veg,"estr")
write.csv(estr.pre.veg.df,"./Data/GIS/Vegetation/estr_pre_veg.csv")

##Combine all the values and filter out land cover of interest
pre.res <- rbind(sdm.pre.veg.df,cabr.pre.veg.df,sasa.pre.veg.df,pean.pre.veg.df,laje.pre.veg.df,estr.pre.veg.df)
pre.res.sm <- pre.res %>% filter(x == "forest" | x == "savanna" | x == "pasture" | x == "river")

##Merge reservoir vegetation pre/post dam
sdm.buff.post <- sdm.buff.veg.df %>% filter(year >= 1997)
cabr.buff.post <- cabr.buff.veg.df %>% filter(year >= 2002)
sasa.buff.post <- sasa.buff.veg.df %>% filter(year >= 2009)
pean.buff.post <- pean.buff.veg.df %>% filter(year >= 2006)
laje.buff.post <- laje.buff.veg.df %>% filter(year >= 2001)
estr.buff.post <- estr.buff.veg.df %>% filter(year >= 2011)

sdm.buff.pre <- sdm.pre.veg.df %>% filter(year < 1997)
cabr.buff.pre <- cabr.pre.veg.df %>% filter(year < 2002)
sasa.buff.pre <- sasa.pre.veg.df %>% filter(year < 2009)
pean.buff.pre <- pean.pre.veg.df %>% filter(year < 2006)
laje.buff.pre <- laje.pre.veg.df %>% filter(year < 2001)
estr.buff.pre <- estr.pre.veg.df %>% filter(year < 2011)

##Combine pre and post dam riparian vegetation data frames
res.buffs <- rbind(sdm.buff.post,sdm.buff.pre,cabr.buff.post,cabr.buff.pre,sasa.buff.post,sasa.buff.pre,
                   pean.buff.post,pean.buff.pre,laje.buff.post,laje.buff.pre,estr.buff.post,estr.buff.pre)

##Filter out land cover of interest
res.buffs2 <- res.buffs %>% filter(x=="forest"|x=="savanna"|x=="pasture") 
##Relevel dams in order of where they are on river (upstream to downstream)
res.buffs2$dam<-fct_relevel(res.buffs2$dam, c("sdm","cabr","sasa","pean","laje","estr"))

p3 <- ggplot(res.buffs2,aes(x=year,y=perc,color=x))+
  geom_line()+
  facet_wrap(~dam, labeller=labeller(dam=dam.labs))+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#35978f","#dfc27d","#8c510a"),name="Cover type",labels=c("Forest","Pasture","Savanna"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  xlab("Year")+
  ylab("Percent land cover")

ggsave("./Images/fig5.png",dpi=300,width=8,height=5)

##Vegetation in reservoirs (pre damming)
##Serra da Mesa
sdm.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sdm.tif")[[-35]]

sdm.veg.df <- calc.lc(year,sdm.veg,"sdm")
write.csv(sdm.veg.df,"./Data/GIS/Vegetation/sdm_veg.csv")

##Cana Brava
cabr.veg <- stack("./Data/GIS/Vegetation/VegetationClip/cabr.tif")[[-35]]

cabr.veg.df <- calc.lc(year,cabr.veg,"cabr")
write.csv(cabr.veg.df,"./Data/GIS/Vegetation/cabr_veg.csv")

##São Salvador
sasa.veg <- stack("./Data/GIS/Vegetation/VegetationClip/sasa.tif")[[-35]]

sasa.veg.df <- calc.lc(year,sasa.veg,"sasa")
write.csv(sasa.veg.df,"./Data/GIS/Vegetation/sasa_veg.csv")

##Peixe Angical
pean.veg <- stack("./Data/GIS/Vegetation/VegetationClip/pean.tif")[[-35]]

pean.veg.df <- calc.lc(year,pean.veg,"pean")
write.csv(pean.veg.df,"./Data/GIS/Vegetation/pean_veg.csv")

##Lajeado
laje.veg <- stack("./Data/GIS/Vegetation/VegetationClip/laje.tif")[[-35]]

laje.veg.df <- calc.lc(year,laje.veg,"laje")
write.csv(laje.veg.df,"./Data/GIS/Vegetation/laje_veg.csv")

##Estreito
estr.veg <- stack("./Data/GIS/Vegetation/VegetationClip/estr.tif")[[-35]]

estr.veg.df <- calc.lc(year,estr.veg,"estr")
write.csv(estr.veg.df,"./Data/GIS/Vegetation/estr_veg.csv")

##Combine all the reservoirs
reservoirs <- rbind(sdm.veg.df,cabr.veg.df,sasa.veg.df,pean.veg.df,laje.veg.df,estr.veg.df)

##Reorder the factors
reservoirs$dam <- as.factor(reservoirs$dam)
reservoirs$dam <- fct_relevel(reservoirs$dam,c("sdm","cabr","sasa","pean","laje","estr"))

##Filter land cover types of interest
res.sm <- reservoirs %>% filter(x=="forest"|x=="savanna"|x=="pasture")

##Select the land cover for the year before and after the reservoir was filled
res.pre <- res.sm %>% slice(34:36,151:153,274:276,367:369,454:456,586:588) 
res.post<- res.sm %>% slice(40:42,154:156,277:279,370:372,460:462,589:591) 

##Calculate the difference in land cover before and after the reservoir was filled
res.diff <- res.post %>% mutate(diff=round(res.pre$area-area,2))
res.diff$x <- fct_relevel(res.diff$x,"forest","savanna","pasture")

##Calculate percent area that was riparian
pre.res.veg <- pre.res.sm %>% slice(45:47,201:203,365:367,489:491,605:607,781:783) %>% 
  select(dam,x,area) %>% mutate(per.chg=paste0(round((area/res.diff$diff)*100,0),"%"))

##Reorder the factors of interest and add in the total difference in area
pre.res.veg$dam<-fct_relevel(pre.res.veg$dam,"sdm","cabr","sasa","pean","laje","estr")
pre.res.veg$x<-fct_relevel(pre.res.veg$x,"forest","savanna","pasture")
pre.res.per <- inner_join(pre.res.veg,res.diff,by=c("dam","x")) 

ggplot(pre.res.per,aes(x=dam,fill=x))+
  geom_bar(aes(y=diff),position=position_dodge(width=0.85),stat="identity",width=0.75,color="black")+
  geom_text(aes(y=diff,label=per.chg),position=position_dodge(0.9),vjust=-0.35,size=3)+
  geom_bar_pattern(aes(y=area.x,pattern_fill=x),position=position_dodge(width=0.85),stat="identity",width=0.75,
                   pattern="stripe",
                   fill="black",
                   colour="black",
                   pattern_density=0.4)+
  geom_hline(yintercept=0,size=1)+
  scale_fill_manual(values=c("#35978f","#8c510a","#dfc27d"),name="Cover type",labels=c("Forest","Savanna","Pasture"))+
  scale_pattern_fill_manual(values=c("#35978f","#8c510a","#dfc27d"),name="",labels=c("Riparian forest","Riparian savanna","Riparian pasture"))+
  scale_x_discrete(labels=c("Serra da Mesa","Cana Brava","São Salvador","Peixe Angical","Lajeado","Estreito"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  labs(y=bquote("Area " ~(km^2)),x="Dam")

ggsave("./Images/fig6.png",dpi=300,width=7,height=3)

##Data frame with total area of the riparian buffer pre and post damming
pre.post.area<-data.frame(area=c(216.78,73.61,38.56,95.15,174.69,145.26,
                                 951.87,164.20,91.07,276.19,351.84,341.42),
                          dam=c("sdm","cabr","sasa","pean","laje","estr",
                                "sdm","cabr","sasa","pean","laje","estr"),
                          time=c("pre","pre","pre","pre","pre","pre",
                                 "post","post","post","post","post","post"))
pre.post.area$dam <- fct_relevel(pre.post.area$dam, "sdm","cabr","sasa","pean","laje","estr")
pre.post.area$time <- fct_relevel(pre.post.area$time, "pre","post")

pre.area <- pre.post.area %>% filter(time=="pre")
post.area <- pre.post.area %>% filter(time=="post")

ggplot()+
  geom_bar(data=post.area,aes(x=dam,y=area,fill=time),stat="identity",width=0.75,color="black")+
  geom_bar_pattern(data=pre.area,aes(x=dam,y=area,pattern_fill=time),stat="identity",width=0.75,
                   pattern="stripe",
                   fill="white",
                   colour="black",
                   pattern_density=0.5)+
  scale_pattern_fill_manual(values="black",labels=c("Pre"),name="Dam status")+
  scale_fill_manual(values="white",name="",labels=c("Post"))+
  scale_x_discrete(labels=c("Serra da Mesa","Cana Brava","São Salvador","Peixe Angical","Lajeado","Estreito"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  labs(y=bquote("Area " ~(km^2)),x="Dam")

ggsave("./Images/fig4.png",dpi=300,height=3,width=7)

##Entire buffer area (including lotic and lentic)
total.area <- rbind(res.buffs,to.sm.area)

##Calculate the total area of lotic and lentic buffer and calculate % of each cover type in the buffer
total.area <- total.area %>% group_by(year) %>% mutate(year.area=sum(area)) %>%
  ungroup %>% group_by(year,x) %>% mutate(veg.area=sum(area)) %>% 
  ungroup %>% mutate(perc=(veg.area/year.area)*100) %>% 
  filter(x=="forest"|x=="pasture"|x=="savanna") %>% 
  distinct(year,veg.area,.keep_all=TRUE) 

##Reorder factors
total.area$x<-fct_relevel(total.area$x,"forest","pasture","savanna")

p4<-ggplot()+
  geom_line(data=total.area,aes(x=year,y=veg.area,color=x,group=x))+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#35978f","#dfc27d","#8c510a"),name="Cover type",labels=c("Forest","Pasture","Savanna"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"),
        legend.position="none")+
  labs(y=bquote("Area " ~(km^2)),x="Year")+
  ggtitle("(A)")

p5<-ggplot(data=total.area,aes(x=year,y=perc,colour=x,group=x))+
  geom_line()+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#35978f","#dfc27d","#8c510a"),name="Cover type",labels=c("Forest","Pasture","Savanna"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  labs(y="Percent cover",x="Year")+
  ggtitle("(B)")

p4|p5

ggsave("./Images/fig7.png",dpi=300,height=3,width=8)

##Proportion lotic vs lentic over time
all.area <- rbind(res.buffs,to.sm.area)

tot.area2 <- all.area %>% mutate(env=case_when(dam=="sdm"&year<1997~"lotic",
                                                 dam=="cabr"&year<2002~"lotic",
                                                 dam=="sasa"&year<2009~"lotic",
                                                 dam=="pean"&year<2006~"lotic",
                                                 dam=="laje"&year<2001~"lotic",
                                                 dam=="estr"&year<2011~"lotic",
                                                 dam=="lotic"~"lotic",
                                                 dam=="sdm"&year>=1997~"lentic",
                                                 dam=="cabr"&year>=2002~"lentic",
                                                 dam=="sasa"&year>=2009~"lentic",
                                                 dam=="pean"&year>=2006~"lentic",
                                                 dam=="laje"&year>=2001~"lentic",
                                                 dam=="estr"&year>=2011~"lentic")) %>% 
             group_by(year,x,env) %>% 
             mutate(all.area=sum(area)) %>% 
             distinct(year,x,env,.keep_all = TRUE) %>%
             group_by(year,env) %>% 
             mutate(total.area=sum(all.area)) %>% 
             distinct(year,env,.keep_all = TRUE) %>% 
             group_by(year) %>% 
             mutate(prop=total.area/sum(total.area))

ggplot(tot.area2,aes(x=year,y=prop,color=env))+
  geom_line(size=1)+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  scale_color_manual(values=c("#dfc27d", "#35978f"),name="Environment",labels=c("Lentic","Lotic"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  xlab("Year")+
  ylab("Proportion of landscape")+
  ylim(0.00,1.00)

ggsave("./Images/Fig8.png",dpi=300,width=6.5,height=3.5)

##Vegetation within river
riv.veg <- stack("./Data/GIS/Vegetation/VegetationClip/to.tif")#[[-35]]

riv.veg.df<- calc.lc(year,riv.veg,NA)

riv.veg.wide <- riv.veg.df %>%
  pivot_wider(names_from=x, values_from=perc)

write.csv(riv.veg.wide,"./Data/GIS/Vegetation/VegetationClip/riv_chan_veg.csv")
write.csv(riv.area.wide,"./Data/GIS/Vegetation/riv_chan_area.csv")

riv.veg.sm <- riv.veg.df %>% filter(x=="forest") 

ggplot(riv.veg.sm,aes(x=year,y=area))+
  geom_line(color="#35978f")+
  geom_vline(data=dam.dates,aes(xintercept=year),linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  xlab("Year")+
  ylab(bquote("Forested area" ~(km^2)))

ggsave("./Images/fig9.png",dpi=300,height=3,width=5)

##Land cover change/deforestation rate in the Tocantins Wathershed
to.lc <- stack("./Data/GIS/Vegetation/to_wtrshd.tif")

to.wtrshd.df <- calc.lc(year,to.lc,NA)

to.wtrshd.sm <- to.wtrshd.df %>% filter(x == "forest" | x == "savanna" | x=="pasture")

p6<-ggplot()+
  geom_line(data=to.wtrshd.sm,aes(x=year,y=perc,color=x),size=1)+
  scale_color_manual(values=c("#35978f","#dfc27d","#8c510a"),name="Cover type",labels=c("Forest","Pasture","Savanna"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color="black"))+
  labs(y="Percent cover",x="Year")
p6
ggsave("./Images/figA1.png",dpi=300,width=6,height=4)

##Calculate deforestation in the Tocantins watershed
wtrshd.covs <- data.frame(t1=c(1985,1985,1985),
                          t2=c(2018,2018,2018),
                          A1=c(to.wtrshd.sm$area[1],to.wtrshd.sm$area[2],to.wtrshd.sm$area[3]),
                          A2=c(to.wtrshd.sm$area[100],to.wtrshd.sm$area[101],to.wtrshd.sm$area[102]))

wtrshd.defor <- wtrshd.covs %>% rowwise() %>% 
  mutate(rate=defor(t1=t1,t2=t2,A2=A2,A1=A1))
wtrshd.defor$cover<-c("forest","savanna","pasture")
