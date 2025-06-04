# SCRITP TO READ AND VISUALIZE THE DATA OF PAR SENSORS FROM THE LOGGER --------
### R. PEREZ, 16 Mai 2024


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table','cowplot','plotly', 'ggplot2', 'dplyr')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# inputs ------------------------------------------------------------------


### Apogee sensors coefficients

coefPAR=10.0 #micromol.m-2.s-1 per mV
# coefFR=51.45 #micromol.m-2.s-1 per mV

## vector of info names in the RawData column
nbSlots=7
names=paste0(c('slot','value'),rep(x = c(1:nbSlots),each=2))

vecName= c('Logger',names)

tableSlot=data.frame(Logger='C1',
                     sensor=c("T0 S2","T1 S3","T2 S1","T3 S1","T4 S1","T5 S1",'T6 S1'),
                     id=c('Bat','Temp','Control','West','West-center','East-center','East'),
                     variable=c('Battery','Temperature','PAR','PAR','PAR','PAR','PAR'))
# load the data -----------------------------------------------------------

allDat=NULL #init
files=list.files(path = '0-data/',pattern = 'DataPAR')

for (file in files){
  
  # file=files[2]
  
  print(file)
  
  name=str_remove(str_remove(string = file,pattern = 'DataPAR_'),pattern = '.CSV')
  TimeStart=ymd_hms(name)
  
  don_raw=data.table::fread(input = paste0('0-data/',file)) %>% 
    data.frame() %>% 
    mutate(time=ymd_hms(TimeStart+Timestamp),
           Date=str_sub(time,1,10),
           ref=str_sub(RawData,start=4,end=str_length(RawData)-1)) %>% 
    tidyr::separate(col = ref,into =vecName ,sep='#') 
  
  don=don_raw %>% select(ID,Timestamp,Days,Hours,Minutes,Date,time,RawData)
  for (i in 1: nbSlots){
    dsub=don_raw %>%
      mutate(info=paste(get(paste0('slot',i)),get(paste0('value',i))))
    
    colnames(dsub)[colnames(dsub)=='info']=paste0('info',i)
    
    don=cbind(don,dsub %>% 
                select(paste0('info',i)))
    
  }
  
  
  donF=don %>%
    tidyr::gather(key = 'info',value = 'value',contains('info')) %>% 
    mutate(value=str_remove(value,'_A11')) %>% 
    tidyr::separate(col = value,into = c('sensor','tension'),sep = '_') %>% 
    mutate(tension=as.numeric(tension),
           DATA=file)
  
  donF=merge(donF,tableSlot)
  
  allDat=rbind(allDat,donF)
  
}


# plots -------------------------------------------------------------------


donPlot=allDat %>%
  filter(variable %in% c('PAR','Battery','Temperature')) %>% 
  mutate(Date=str_sub(time,1,10),
         hms=hms(str_sub(time,12,19)),
         tension=ifelse(sensor=='T3 S1',tension*coefPAR,tension),
         tension=ifelse(sensor=='T2 S1',tension*coefPAR,tension),
         tension=ifelse(sensor=='T4 S1',tension*coefPAR,tension),
         tension=ifelse(sensor=='T5 S1',tension*coefPAR,tension),
         tension=ifelse(sensor=='T6 S1',tension*coefPAR,tension)) %>% 
filter(!(tension<50))


donPlot %>%
  filter(variable %in% c('PAR','PAR','Battery','Temperature')) %>%
  mutate(tension=ifelse(sensor=='T3 S1',tension*coefPAR,tension)) %>%
  mutate(tension=ifelse(sensor=='T2 S1',tension*coefPAR,tension)) %>%
  ggplot(aes(x=time,y=tension,col=sensor,group=DATA))+
  geom_line()+
  facet_wrap(~variable,scale='free_y')+
  scale_x_datetime()+
  labs(y='')

ggplotly(donPlot %>% 
           ggplot(aes(x=time,y=tension,col=sensor))+
           geom_line()+
           scale_x_datetime()+
           facet_wrap(~variable,scales='free_y'))


my_colors <- c(
  "Control" = "black",
  "West" = "green3",
  "West-center" = "blue",
  "East-center" = "red",
  "East" = "orange"
)

donPlot %>% 
  filter(variable %in% c('PAR')) %>% 
  # filter(Date== '2025-05-28') %>%
  ggplot()+
  geom_line(aes(x=hms, y=tension, col=id), linewidth=1) +
  facet_wrap(~Date) +
  scale_x_time(breaks = scales::breaks_width("30 min"),  # breaks toutes les 30 min (adapte à 15 min ou 1h si besoin)
               labels = scales::label_time("%H:%M")      # format de l’heure en HH:MM
  ) +
  scale_color_manual(values=my_colors) +
  ylab(expression('PFD '*(mu*mol*' '*m**-2*' '*s**-1))) +
  theme_classic(base_size=14)+
  theme(axis.text.x = element_text(angle=90))





# Sauvegarde automatique de p <- ggplot avec Nom du fichier et la date extraite
file_name <- paste0("2-figures/ggplotPAR_", name, ".png")

ggsave(file_name, plot=p, width=10, height=6, dpi=300)

print(paste("Graphique sauvegardé sous : ", file_name))




#heure d'ombrage à différents seuils


# Seuils à tester
seuils <- c(0.95, 0.90, 0.85, 0.5)

# Résultat final initialisé
ombrage_df <- NULL

for (seuil in seuils) {
  tmp <- donPlot %>%
    filter(variable == "PAR") %>%
    group_by(Date, id) %>%
    arrange(hms) %>%
    mutate(
      idx_max = which.max(tension),
      heure_max = hms[idx_max],
      pfd_max = max(tension, na.rm=TRUE)
    ) %>%
    # Pour chaque groupe, calculer la 1ʳᵉ heure après le max où PFD < seuil * pfd_max
    group_modify(~{
      after_max <- .x %>% slice(.x$idx_max:n())
      idx <- which(after_max$tension < seuil * after_max$pfd_max[1])[1]
      heure_ombrage <- if (is.na(idx)) NA else after_max$hms[idx]
      
      tibble(
        seuil = seuil,
        heure_max = .x$heure_max[1],
        heure_ombrage = heure_ombrage
      )
    }) %>%
    ungroup()
  
  ombrage_df <- bind_rows(ombrage_df, tmp)
}

# Résultat final trié
ombrage_df <- ombrage_df %>%
  arrange(Date, id, seuil)

print(ombrage_df)




ggplot(ombrage_df, aes(x=as.Date(Date), y=as.numeric(heure_ombrage), color=as.factor(seuil))) +
  geom_line(linewidth=1) +
  geom_point(size=2) +
  facet_wrap(~id) +
  scale_y_continuous(
    name="Heure du début d’ombrage (en secondes depuis minuit)",
    breaks=seq(0, 86400, 3600),
    labels=function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60)
  ) +
  labs(
    title="Évolution quotidienne de l’heure du début d’ombrage",
    x="Date",
    color="Seuil"
  ) +
  theme_light(base_size=14)



#Periode ombrages

seuil <- 0.2  # adapte-le à 0.9, 0.85, 0.5, etc.

ombrage_periods <- donPlot %>%
  filter(grepl("PAR", variable)) %>%
  group_by(Date, id) %>%
  arrange(hms) %>%
  mutate(
    pfd_max = max(tension, na.rm=TRUE),
    seuil_val = seuil * pfd_max,
    ombrage = tension < seuil_val,  # TRUE si sous le seuil
    
    # heure du PFD max par capteur
    idx_max = which.max(tension),
    heure_max = hms[idx_max]
  ) %>% 
  # identifier les périodes continues d'ombrage
  mutate(
    ombrage_group = cumsum(c(TRUE, diff(ombrage) != 0))
  ) %>%
  group_by(Date, id, ombrage_group) %>%
  summarise(
    ombrage = first(ombrage),
    heure_debut = first(hms),
    heure_fin = last(hms),
    heure_max = first(heure_max),
    .groups = "drop"
  ) %>%
  filter(ombrage) %>%
  # déterminer si avant ou après PFD max + calcul durée
  mutate(
    type_ombrage = ifelse(heure_fin <= heure_max, "avant", "après"),
    duree_sec = as.numeric(heure_fin - heure_debut, units="secs"),  # durée en secondes
    duree_min = duree_sec / 60                                       # durée en minutes
  ) %>%
  arrange(Date, id, heure_debut)

print(ombrage_periods)





# Convertir les Period en secondes pour l’axe X (ggplot comprend mal Period directement)
ombrage_periods_plot <- ombrage_periods %>%
  mutate(
    heure_debut_sec = as.numeric(heure_debut, units="secs"),
    heure_fin_sec = as.numeric(heure_fin, units="secs")
  )

# Visualisation en barres horizontales (type Gantt)
ggplot(ombrage_periods_plot) +
  geom_segment(aes(
    x=heure_debut_sec, xend=heure_fin_sec,
    y=id, yend=id, color=type_ombrage
  ), linewidth=4) +
  geom_text(aes(
    x=(heure_debut_sec + heure_fin_sec)/2,  # milieu de la barre
    y=id,
    label=paste0(round(duree_min, 1), " min")
  ),
  color="black", size=3, vjust=-0.5) +  # décalage vertical léger
  facet_wrap(~Date, scales="free_x", ncol=1) +
  scale_x_continuous(
    name="Heure (HH:MM)",
    breaks=seq(0, 86400, 3600),
    labels=function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60)
  ) +
  labs(
    title="Périodes d’ombrage détectées par capteur et type (avec durée)",
    y="Capteur",
    color="Type d’ombrage"
  ) +
  theme_minimal(base_size=14) +
  scale_color_manual(values=c("avant"="blue", "après"="red"))

