##############################################################################
## ISIS-MACCO VALIDATION -- FUNCTIONS FOR VALIDATION ANALYSIS
## Author : A. Ricouard -- antoine.ricouard.oceano@gmail.com
##############################################################################


### importing simulation datasets----
import_simu_abundance <- function(simu,tidy,species,years){
  # simu : the simulation path
  # years : the list of years required
  
  
  Data <- data.table::fread(file = paste0(simu,"/resultExports/AbondanceBeginMonth_Gpe_Janvier.csv"),
                            header=F,
                            sep=";") %>%
    rename(pop = V1, year = V2, group = V3, value = V4)
  
  
  for(y in seq(length(years))){
    
    Out <- Data %>% 
      filter(pop == species,
             year == y-1) %>%
      rename(N_jan = value) %>%
      select(group,N_jan)
    saveRDS(Out,file=paste0(tidy,"/abundance/",species,"/Njan_bygroup_",species,"_",years[y],".rds"))
    
  }
  
}

import_simu_byseason <- function(simu,tidy,species,year){
  # list_of_simu : a list containing all simulation paths
  
  
  Out <- data.table::fread(file = paste0(simu,"/resultExports/CapturesPoidsStrategies.csv"),
                           header=T,
                           sep=";",
                           select =  c("step","population","strategy","metier","group","value")) %>%
    filter(population == species) %>%
    mutate(season=1+step %/% 3) %>%
    group_by(strategy,metier,group,season) %>%
    summarise(value=sum(value)) %>%
    ungroup()
  
  saveRDS(Out,file=paste0(tidy,"/catch_season/",species,"/import_by_season_",species,"_",year,".rds"))
  
}

import_simu_byseason2 <- function(simu,tidy,species,year){
  # list_of_simu : a list containing all simulation paths
  
  
  Out <- data.table::fread(file = paste0(simu,"/resultExports/CatchWeightStrFltMetierQuarter.csv"),
                           header=T,
                           sep=";",
                           select =  c("population","group","strategy","metier","yearQuarter","valSim")) %>%
    filter(population == species) %>%
    rename(season = yearQuarter) %>%
    group_by(strategy,metier,group,season) %>%
    summarise(value=sum(valSim)) %>%
    ungroup()
  
  saveRDS(Out,file=paste0(tidy,"/catch_quarter/",species,"/import_by_season_",species,"_",year,".rds"))
  
}

import_simu_byseason3 <- function(simu,tidy,species,years){
  # simu : the simulation path
  # years : the list of years required
  
  
  Data <- data.table::fread(file = paste0(simu,"/resultExports/CatchWeightStrFltMetierQuarter.csv"),
                            header=T,
                            sep=";",
                            select =  c("population","group","strategy","metier","yearQuarter","valSim")) %>%
    filter(population == species) %>%
    rename(season = yearQuarter) %>%
    group_by(strategy,metier,group,season) %>%
    summarise(value=sum(valSim)) %>%
    ungroup()
  
  for(y in seq(length(years))){
    
    Out <- Data %>% 
      filter( 4*(y-1) <= season & season < 4*y ) %>%
      mutate(season = season %% 4)
    saveRDS(Out,file=paste0(tidy,"/catch_quarter/",species,"/import_by_season_",species,"_",years[y],".rds"))
    
  }
  
}

import_simu_byyear <- function(simu,tidy,species,year){
  # list_of_simu : a list containing all simulation paths
  
  
  Out <- data.table::fread(file = paste0(simu,"/resultExports/CapturesPoidsStrategies.csv"),
                           header=T,
                           sep=";",
                           select =  c("population","strategy","metier","group","value")) %>%
    filter(population == species) %>%
    group_by(strategy,metier,group) %>%
    summarise(value=sum(value)) %>%
    ungroup()
  
  saveRDS(Out,file=paste0(tidy,"/catch_year/",species,"/import_by_year",species,"_",year,".rds"))
  
}

import_simu_byyear2 <- function(simu,tidy,species,year){
  # simu : the simulation path
  
  
  Out <- data.table::fread(file = paste0(simu,"/resultExports/CatchWeightStrFltMetierQuarter.csv"),
                           header=T,
                           sep=";",
                           select =  c("population","group","strategy","metier","valSim")) %>%
    filter(population == species) %>%
    group_by(strategy,metier,group) %>%
    summarise(value=sum(valSim)) %>%
    ungroup()
  
  saveRDS(Out,file=paste0(tidy,"/catch_year/",species,"/import_by_year",species,"_",year,".rds"))
  
}

import_simu_Fmor <- function(simu,tidy,species,years){
  # simu : the simulation path
  # years : the list of years required
  
  
  Data <- data.table::fread(file = paste0(simu,"/resultExports/MortalitePecheGroupe.csv"),
                            header=T,
                            sep=";") %>%
    mutate(year = years[step %/% 11]) %>%
    rename(Fmor = value)
  
  for(y in seq(length(years))){
    
    Out <- Data %>% 
      filter(population == species,
             year == years[y]) %>%
      select(group,Fmor)
    saveRDS(Out,file=paste0(tidy,"/fishing_mortality/",species,"/Fmor_bygroup_",species,"_",years[y],".rds"))
    
  }
  
}

import_simu_Nbyyear <- function(simu,tidy,species,year){
  
  Out <- data.table::fread(file = paste0(simu,"/resultExports/CapturesNombre.csv"),
                            header=T,
                            sep=";",
                            select =  c("population","group","value")) %>%
    filter(population == species) %>%
    group_by(group) %>%
    summarise(value=sum(value)) %>%
    ungroup()
  
  saveRDS(Out,file=paste0(tidy,"/catch_year/",species,"/import_Nbyyear_",species,"_",year,".rds"))
  
}

import_simu_Nbyyear2 <- function(simu,tidy,species,years){
  
  Data <- data.table::fread(file = paste0(simu,"/resultExports/CapturesNombre.csv"),
                           header=T,
                           sep=";",
                           select =  c("step","population","group","value")) %>%
    filter(population == species)
    
  
  for(y in seq(length(years))){
    
    Out <- Data %>% 
      filter( 12*(y-1) <= step & step < 12*y ) %>%
      group_by(group) %>%
      summarise(value=sum(value)) %>%
      ungroup()
    saveRDS(Out,file=paste0(tidy,"/catch_year/",species,"/import_Nbyyear_",species,"_",years[y],".rds"))
    
  }
}

import_simu_ssb <- function(simu,tidy,species,years){
  # simu : the simulation path
  # years : the list of years required
  
  
  Data <- data.table::fread(file = paste0(simu,"/resultExports/BiomasseBeginMonthFeconde.csv"),
                            header=F,
                            sep=";",
                            select =  c(1,2,3,4,5)) %>%
    rename(pop = V1, group = V2, zone = V3, step = V4, SSBjan = V5) %>%
    filter(step %% 12 == 0, pop == species) %>%
    mutate(year = years[step %/% 12 + 1]) %>%
    group_by(zone,year) %>%
    summarise(SSBjan=sum(SSBjan)) %>%
    ungroup()
  
  for(y in seq(length(years))){
    
    Out <- Data %>% 
      filter(year == years[y]) %>%
      select(SSBjan,zone) %>%
      saveRDS(file=paste0(tidy,"/ssb/",species,"/SSBjan_",species,"_",years[y],".rds"))
    
  }
  
}


### reading tidy data----
read_tidysim_byfleet_byyear <- function(fleet_group,species,years,path){
  
  this_group <- fleet_group
  Dat <- data.frame()
  plan <- expand.grid(species,years)
  
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  tmp <- foreach(i = 1:nrow(plan), .packages = c("dplyr","stringr")) %dopar% {
    
    this_species <- plan$Var1[i]
    this_year <- plan$Var2[i]
    thisDat <-  readRDS(file = paste0(path,"/catch_year/",this_species,"/import_by_year",this_species,"_",this_year,".rds")) %>% 
      mutate(fleet = word(strategy,1,sep = ' @ ')) %>%
      mutate(fleet_group=case_when(grepl('BE',fleet)|grepl('ES',fleet)|grepl('UK',fleet) ~ 'foreign',
                                   !(grepl('BE',fleet)|grepl('ES',fleet)|grepl('UK',fleet)) & 
                                     (grepl('0-10',fleet)|grepl('10-12',fleet)) ~ 'inf12',
                                   !(grepl('BE',fleet)|grepl('ES',fleet)|grepl('UK',fleet)) & 
                                     (grepl('12-15',fleet)|
                                        grepl('15-18',fleet)|
                                        grepl('18-24',fleet)|
                                        grepl('24-40',fleet)|
                                        grepl('40-80',fleet)|
                                        grepl('80-Inf',fleet)) ~ 'sup12')) %>%
      filter(fleet_group == this_group) %>%
      group_by(fleet,metier) %>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      mutate(species=this_species,
             year=this_year)
    
    
    return(thisDat)
    
  }
  stopCluster(cl)
  
  
  for(i in seq(nrow(plan))){
    NewDat <- tmp[[i]]
    Dat <- rbind(Dat,NewDat)
  }
  
  return(Dat)
}

read_tidysim_byfleet_byseason <- function(species,years,path){
  
  #print(paste("start: ",Sys.time()))
  Dat <- data.frame()
  plan <- expand.grid(species,years)
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  tmp <- foreach(i = 1:nrow(plan), .packages = c("dplyr","stringr")) %dopar% {
  
    this_species <- plan$Var1[i]
    this_year <- plan$Var2[i]
    thisDat <-  readRDS(file = paste0(path,"/catch_quarter/",this_species,"/import_by_season_",this_species,"_",this_year,".rds")) %>% 
      group_by(strategy,metier,season) %>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      mutate(fleet_group=case_when(grepl('BE',strategy)|grepl('ES',strategy)|grepl('UK',strategy) ~ 'foreign',
                                   !(grepl('BE',strategy)|grepl('ES',strategy)|grepl('UK',strategy)) & 
                                     (grepl('0-10',strategy)|grepl('10-12',strategy)) ~ 'inf12',
                                   !(grepl('BE',strategy)|grepl('ES',strategy)|grepl('UK',strategy)) & 
                                     (grepl('12-15',strategy)|
                                        grepl('15-18',strategy)|
                                        grepl('18-24',strategy)|
                                        grepl('24-40',strategy)|
                                        grepl('40-80',strategy)|
                                        grepl('80-Inf',strategy)) ~ 'sup12')) %>%
      mutate(fleet = if_else(fleet_group != 'sup12',word(strategy,1,sep = ' @ '),strategy)) %>%
      mutate(fleet = if_else(fleet_group == 'sup12',str_replace(fleet,'bob_north-','North '),fleet)) %>%
      mutate(fleet = if_else(fleet_group == 'sup12',str_replace(fleet,'bob_south-','South '),fleet)) %>%
      mutate(fleet = str_replace(fleet,'@ ','')) %>%
      mutate(fleet = str_replace(fleet,'_',' and ')) %>%
      mutate(fleet = if_else(fleet_group == 'inf12',
                             case_when(fleet == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                       fleet == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                       fleet == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                       fleet == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                       fleet == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                       fleet == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                       fleet == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                       fleet == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                       fleet == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                       fleet == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                       fleet == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                       fleet == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                       fleet == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                       fleet == 'Palangriers exclusifs-10-12' ~ 'longline 10-12'),
                             fleet)) %>%
      mutate(species=this_species,
             year=this_year) %>%
      group_by(year,species,fleet,fleet_group,metier,season) %>%
      summarise(value=sum(value)) %>%
      ungroup()
    
    return(thisDat)
  }
  stopCluster(cl)
  
  for(i in seq(nrow(plan))){
    Dat <- rbind(Dat,tmp[[i]])
  }
  
  #print(paste("end: ",Sys.time()))
  
  return(Dat)
}

read_tidysim_byseason <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/catch_quarter/",species,"/import_by_season_",species,"_",years[i],".rds")) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_byyear <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/catch_year/",species,"/import_by_year",species,"_",years[i],".rds")) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_catchN <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/catch_year/",species,"/import_Nbyyear_",species,"_",years[i],".rds")) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_F <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/fishing_mortality/",species,"/Fmor_bygroup_",species,"_",years[i],".rds")) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_N <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/abundance/",species,"/Njan_bygroup_",species,"_",years[i],".rds")) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_SSB <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/ssb/",species,"/SSBjan_",species,"_",years[i],".rds")) %>% 
      summarise(SSBjan = sum(SSBjan)) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

read_tidysim_SSB_merluccius <- function(species,years,path){
  
  Dat <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(path,"/ssb/",species,"/SSBjan_",species,"_",years[i],".rds")) %>% 
      filter(!zone %in% c("Zone_CelticSea","Zone_NorthernArea")) %>%
      summarise(SSBjan = sum(SSBjan)) %>% 
      mutate(year=years[i])
    Dat <- rbind(Dat,NewDat)
  }
  return(Dat)
}

transform_tidy_byyear <- function(Data){
  Out <- Data %>% 
    group_by(strategy,metier,group,year) %>% 
    summarise(value=sum(value)) %>% 
    ungroup()
  
  return(Out)
}

### other dataset handling tools----
data_concatenation <- function(mod_list,run_list,qty_list,species_list,path){
  
  Plan <- expand.grid(mod = mod_list,run = run_list,qty = qty_list)
  for(m in seq(nrow(Plan))){
    
      this_path <- paste0(path,"/",Plan$mod[m],"/",Plan$run[m],"/",Plan$qty[m])
      Data <- data.frame()
      for(s in seq(length(species_list))){
      
        this_spp <- species_list[s]
        spp_short <- case_when(this_spp == "Merluccius_merluccius" ~ c("Merluccius","hake"),
                               this_spp == "Leucoraja_naevus" ~ c("Leucoraja","Leucoraja"),
                               this_spp == "Lepidorhombus_whiffiagonis" ~ c("megrim","Lepidorhombus"),
                               this_spp == "Lophius_piscatorius" ~ c("Lophius","monkfish"),
                               this_spp == "Nephrops_norvegicus" ~ c("nephrops","Nephrops"),
                               this_spp == "Raja_clavata" ~ c("Raja","Raja"),
                               this_spp == "Solea_solea" ~ c("sole","Solea"))
        
          this_file <- data.frame(name=list.files(this_path)) %>%
            filter(grepl(spp_short[1],name)|grepl(spp_short[2],name)) %>%
            mutate(name = paste0(this_path,"/",name))
          
          if(length(this_file$name)!=0){
            
            for(i in seq(length(this_file$name))){
              thisDat <- readRDS(this_file$name[i]) %>%
                mutate(species = this_spp)
              
              if("age_group" %in% names(thisDat)){
                thisDat <- thisDat %>% rename(age=age_group)
              }else if("group" %in% names(thisDat)){
                thisDat <- thisDat %>% rename(age=group)
              }
              
              Data <- rbind(Data,thisDat)
            }
            
          }
    
      }
      
      saveRDS(Data,file = paste0(this_path,"/",Plan$qty[m],"_allSpp_",min(Data$year),"_",max(Data$year),".rds"))
  
  }
  
}

data_concatenation_flt <- function(mod_list,run_list,fleet_group_list,qty_list,path){
  
  Plan <- expand.grid(mod = mod_list,run = run_list,qty = qty_list)
  for(m in seq(nrow(Plan))){
    
    this_path <- paste0(path,"/",Plan$mod[m],"/",Plan$run[m],"/",Plan$qty[m])
    Data <- data.frame()
    for(g in seq(length(fleet_group_list))){
      
      this_group <- fleet_group_list[g]
      
      this_file <- data.frame(name=list.files(this_path)) %>%
        filter(grepl(this_group,name)) %>%
        mutate(name = paste0(this_path,"/",name))
      
      if(length(this_file$name)!=0){
        
        for(i in seq(length(this_file$name))){
          thisDat <- readRDS(this_file$name[i])
          Data <- rbind(Data,thisDat)
        }
        
      }
      
    }
    
    saveRDS(Data,file = paste0(this_path,"/",Plan$qty[m],"_allFleets_",min(Data$year),"_",max(Data$year),".rds"))
    
  }
  
}

fleet_translation <- function(data,rm_fleets=NULL){
  # these damned name of fleets <12m are in french ! let us translate it in english !
   Data <- data %>% 
     filter(!fleet_isis %in% rm_fleets) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Chalutiers de fond','Bottom-trawlers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Chalutiers','Trawlers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Arts dormants','passive-gears')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Flottilles agrégées','Aggregated-fleets')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Tamiseurs','sievers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Chalutiers','Trawlers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'exclusifs','only')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Dragueurs','dredgers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Fileyeurs','Gill-netters')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Palangriers','Long-liners')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'Caseyeurs','trappers')) %>%
     mutate(fleet_isis = str_replace(fleet_isis,'mixtes','mixt')) %>%
     
     # let us attribute the group !
     mutate(fleet_group = case_when(str_detect(fleet_isis,"10") ~ "FR <12",
                                    str_detect(fleet_isis,"North")|str_detect(fleet_isis,"South") ~ "FR >12",
                                    str_detect(fleet_isis,"UK")|str_detect(fleet_isis,"BE")|str_detect(fleet_isis,"ES") ~ "Foreign")) %>%
     # let us modify the FR>12 name !
     mutate(fleet_isis = if_else(fleet_group != "FR >12",
                                 fleet_isis,
                                 if_else(str_detect(fleet_isis,"North"),
                                         paste(fleet_isis,"(north)"),
                                         paste(fleet_isis,"(south)")))) %>%
     mutate(fleet_isis=str_replace(fleet_isis,"North ","")) %>%
     mutate(fleet_isis=str_replace(fleet_isis,"South ","")) %>% #metier au début
     mutate(length=word(fleet_isis,1,sep = " ")) %>%
     mutate(fleet_isis = if_else(fleet_group=="FR >12",str_replace(fleet_isis,length,""),fleet_isis)) %>%
     mutate(fleet_isis = if_else(fleet_group=="FR >12",paste(fleet_isis,length),fleet_isis)) %>%
     select(-length)
    
     
  return(Data)
}

fleet_weight <- function(fleet_group_list,path,years=2015:2018,rm_fleets=NULL){
  
 
  Data <- data.frame()
  for(i in seq(length(fleet_group_list))){
    
    this_path <- paste0(path,"/land_by_species/",fleet_group_list[i])
    these_fleets <- list.files(this_path)
    for(j in seq(length(these_fleets))){
      this_file <- paste0(this_path,"/",these_fleets[j])
      
      this_data <- readRDS(file = this_file) %>%
        filter(year %in% years) %>%
        mutate(fleet_group = case_when(fleet_group_list[i]=="inf12" ~ "FR <12",
                                       fleet_group_list[i]=="sup12" ~ "FR >12",
                                       fleet_group_list[i]=="foreign" ~ "Foreign"))
        
        Data <- rbind(Data,this_data)
    }

  }
  
  Data <- Data %>% 
    select(fleet_isis,fleet_group,Landings_kg) %>%
    group_by(fleet_isis,fleet_group) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(prop = Landings_kg/sum(Landings_kg)) %>%
    mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
    mutate(fleet_isis=str_replace(fleet_isis,"@ ","")) %>%
    filter(!fleet_isis %in% rm_fleets) %>%
    mutate(fleet_isis = if_else(fleet_group != 'FR <12',
                                fleet_isis,
                                case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                          fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                          fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                          fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                          fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                          fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                          fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                          fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                          fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                          fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                          fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                          fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                          fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                          fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                          fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12'))) %>%
             mutate(fleet_isis = if_else(fleet_group != "FR >12",
                                         fleet_isis,
                                         if_else(str_detect(fleet_isis,"bob_north"),
                                                 paste(fleet_isis,"(north)"),
                                                 paste(fleet_isis,"(south)")))) %>%
             mutate(fleet_isis=str_replace(fleet_isis,"bob_north-","")) %>%
             mutate(fleet_isis=str_replace(fleet_isis,"bob_south-",""))  %>%
             mutate(fleet_isis=str_replace(fleet_isis,"_"," and "))  %>%
             mutate(length=word(fleet_isis,1,sep = " ")) %>%
             mutate(fleet_isis = if_else(fleet_group=="FR >12",str_replace(fleet_isis,length,""),fleet_isis)) %>%
             mutate(fleet_isis = if_else(fleet_group=="FR >12",paste(fleet_isis,length),fleet_isis)) %>%
             select(-length) %>% # classement des flottilles
             arrange(fleet_group,prop)
    
  return(Data)
}

skills_SRcomp_byperiod <- function(spp,mod,run_list,qty_list,path,SRorigin = "Hockey-Stick",SRalternate = "Ricker"){
  
  spp_short <- case_when(spp == "Merluccius_merluccius" ~ c("Merluccius","hake"),
                         spp == "Leucoraja_naevus" ~ c("Leucoraja","Leucoraja"),
                         spp == "Lepidorhombus_whiffiagonis" ~ c("megrim","Lepidorhombus"),
                         spp == "Lophius_piscatorius" ~ c("Lophius","monkfish"),
                         spp == "Nephrops_norvegicus" ~ c("nephrops","Nephrops"),
                         spp == "Raja_clavata" ~ c("Raja","Raja"),
                         spp == "Solea_solea" ~ c("sole","Solea"))
  
  
  Plan <- expand.grid(run = run_list,qty = qty_list)
  Data <- data.frame()
  for(i in seq(nrow(Plan))){
    
    this_run <- Plan$run[i]
    this_qty <- Plan$qty[i]
    
    
    this_path <- paste0(path,"/",mod,"/",this_run,"/",this_qty)
    this_file <- data.frame(name=list.files(this_path)) %>%
      filter(grepl(spp_short[1],name)|grepl(spp_short[2],name))
    
    if(nrow(this_file == 1)){
      
      this_data <- readRDS(file = paste0(this_path,"/",this_file$name)) %>%
        pivot_wider(names_from=data_type,values_from=value) %>%
        mutate(species=spp) %>%
        # relative error
        mutate(r.err2 = if_else(obs==0,
                                sim**2,
                                ((obs-sim)**2) / (obs**2))) %>%
        group_by(species) %>%
        # weights
        mutate(wt = (obs**2)/sum(obs**2)) %>%
        # variance
        mutate(s2_obs = var(obs),
               s2_sim = var(sim)) %>%
        # Average Absolute Error
        mutate(AAE = mean(abs(r.err2))) %>%
        # weighted RwMSE
        mutate(RwMSE=sqrt(sum(r.err2*wt))) %>%
        # correlation coefficient
        mutate(r = cor(sim,obs)) %>%
        # Modelling efficiency
        mutate(MEF = ( sum((obs-mean(obs))**2) - sum((sim-obs)**2) )/( sum((obs-mean(obs))**2) ) ) %>%
        # end
        ungroup() %>%
        mutate(mod = mod,
               run = this_run,
               qty = this_qty,
               period = if_else(year <= 2018,1,2),
               SR_type = case_when(!grepl("SR",this_run) ~ "forced recruitment",
                              grepl("SRalternative",this_run) ~ SRalternate,
                              grepl("SR",this_run) & !grepl("alternative",this_run) ~ SRorigin)) %>%
        select(period,species,mod,run,qty,SR_type,s2_obs,s2_sim,AAE,RwMSE,r,MEF) %>%
        distinct()
      
      Data <- rbind(Data,this_data)
      
    }
    
  }
  
  return(Data)
}

skills_SRcomp_byyear <- function(spp,mod,run_list,qty_list,path,SRorigin = "Hockey-Stick",SRalternate = "Ricker"){
  
  
  spp_short <- case_when(spp == "Merluccius_merluccius" ~ c("Merluccius","hake"),
                         spp == "Leucoraja_naevus" ~ c("Leucoraja","Leucoraja"),
                         spp == "Lepidorhombus_whiffiagonis" ~ c("megrim","Lepidorhombus"),
                         spp == "Lophius_piscatorius" ~ c("Lophius","monkfish"),
                         spp == "Nephrops_norvegicus" ~ c("nephrops","Nephrops"),
                         spp == "Raja_clavata" ~ c("Raja","Raja"),
                         spp == "Solea_solea" ~ c("sole","Solea"))
  
  Plan <- expand.grid(run = run_list,qty = qty_list)
  Data <- data.frame()
  for(i in seq(nrow(Plan))){
    
    this_run <- Plan$run[i]
    this_qty <- Plan$qty[i]
    
    
    this_path <- paste0(path,"/",mod,"/",this_run,"/",this_qty)
    this_file <- data.frame(name=list.files(this_path)) %>%
      filter(grepl(spp_short[1],name)|grepl(spp_short[2],name))
    if(nrow(this_file == 1)){
      
      this_data <- readRDS(file = paste0(this_path,"/",this_file$name)) %>%
        pivot_wider(names_from=data_type,values_from=value) %>%
        mutate(species=spp) %>%
        # relative error
        mutate(r.err2 = if_else(obs==0,
                                sim**2,
                                ((obs-sim)**2) / (obs**2))) %>%
        group_by(year,species) %>%
        # weights
        mutate(wt = (obs**2)/sum(obs**2)) %>%
        # variance
        mutate(s2_obs = var(obs),
               s2_sim = var(sim)) %>%
        # Weigted relative error
        mutate(AAE = mean(abs(r.err2))) %>%
        # weighted RwMSE
        mutate(RwMSE=sqrt(sum(r.err2*wt))) %>%
        # correlation coefficient
        mutate(r = cor(sim,obs)) %>%
        # Modelling efficiency
        mutate(MEF = ( sum((obs-mean(obs))**2) - sum((sim-obs)**2) )/( sum((obs-mean(obs))**2) ) ) %>%
        # end
        ungroup() %>%
        mutate(mod = mod,
               period = if_else(year <= 2018,1,2),
               run = this_run,
               qty = this_qty,
               SR_type = case_when(!grepl("SR",this_run) ~ "forced recruitment",
                                   grepl("SRalternative",this_run) ~ SRalternate,
                                   grepl("SR",this_run) & !grepl("alternative",this_run) ~ SRorigin)) %>%
        select(year,period,species,mod,run,SR_type,qty,s2_obs,s2_sim,AAE,RwMSE,r,MEF) %>%
        distinct()
      
      Data <- rbind(Data,this_data)
      
    }
    
  }
  
  return(Data)
}

skills_summary <- function(mod_list,run_list,qty_list,path,by_fleet=F,by_period=F){
  
  Plan <- expand.grid(mod = mod_list,run = run_list,qty = qty_list)
  Data <- data.frame()
  for(m in seq(nrow(Plan))){
    
    this_mod <- Plan$mod[m]
    this_run <- Plan$run[m]
    this_qty <- Plan$qty[m]
    
    
    this_path <- paste0(path,"/",this_mod,"/",this_run,"/",this_qty)
    if(by_fleet){
      this_file <- data.frame(name=list.files(this_path)) %>%
        filter(grepl(paste0("allFleets_20"),name)) 
      
      this_data <- readRDS(file = paste0(this_path,"/",this_file$name)) %>%
        distinct() %>%
        pivot_wider(names_from=data_type,values_from=value) %>%
        group_by(fleet_isis) 
      
    }else{
      this_file <- data.frame(name=list.files(this_path)) %>%
        filter(grepl(paste0("allSpp_20"),name))
      
      this_data <- readRDS(file = paste0(this_path,"/",this_file$name)) %>%
        distinct() %>%
        pivot_wider(names_from=data_type,values_from=value) %>%
        group_by(species)
    }
    
    
    
    this_data <- this_data %>%
      na.omit() %>%
      # relative error
      mutate(r.err = if_else(obs==0,
                             -sim,
                             (obs-sim)/obs)) %>%
      # squared relative error
      mutate(r.err2 = if_else(obs==0,
                              sim**2,
                              ((obs-sim)**2) / (obs**2))) %>%
      # Average Absolute Error
      mutate(AAE = mean(abs(r.err2))) %>%
      # weights
      mutate(wt = (obs**2)/sum(obs**2)) %>%
      # weighted RwMSE
      mutate(RwMSE=sqrt(sum(r.err2*wt))) %>%
      # Average error (bias)
      mutate(AE = mean(r.err)) %>%
      # correlation coefficient
      mutate(r = cor(sim,obs)) %>%
      # Modelling efficiency
      mutate(MEF = ( sum((obs-mean(obs))**2) - sum((sim-obs)**2) )/( sum((obs-mean(obs))**2) ) ) %>%
      # end
      ungroup() %>%
      mutate(mod = this_mod,
             run = this_run,
             qty = this_qty,
             SR = grepl("SR",this_run)) 
    
    if(by_fleet & by_period){
      this_data <- this_data %>%
        mutate(period = if_else(year <= 2018,1,2)) %>%
        select(period,fleet_isis,mod,run,qty,SR,AE,AAE,RwMSE,r,MEF) %>%
        distinct()
    }else if((!by_fleet) & by_period) {
      this_data <- this_data %>%
        mutate(period = if_else(year <= 2018,1,2)) %>%
        select(period,species,mod,run,qty,SR,AE,AAE,RwMSE,r,MEF) %>%
        distinct()
    }else if(by_fleet & (!by_period)){
      this_data <- this_data %>%
        select(year,fleet_isis,mod,run,qty,SR,AE,AAE,RwMSE,r,MEF) %>%
        distinct()
    }else{
      this_data <- this_data %>%
        select(year,species,mod,run,qty,SR,AE,AAE,RwMSE,r,MEF) %>%
        distinct()
    }
    
    Data <- rbind(Data,this_data)
  }
  
  return(Data)
}

### preparation of data for plotting----
group_to_age_merluccius <- function(group){
  
  age <- case_when(group %in% 0:19 ~ "0",
                   group %in% 20:37 ~ "1",
                   group %in% 38:45 ~ "2", 
                   group %in% 46:51 ~ "3",
                   group %in% 52:56 ~ "4",
                   group %in% 57:61 ~ "5",
                   group %in% 62:64 ~ "6",
                   group %in% 65:67 ~ "7",
                   group == 68 ~ "8",
                   group == 69 ~ "9-10",
                   group == 70 ~ "11-13",
                   group == 71 ~ "14+")
  
  return(age)
}

prep_catch_agegroup_byyear <- function(sim_data,species,years,obs_data_path,corresp_path,export,export_path){
  
  spp_single <- word(species,1,sep="_")
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  corresp_tab <- readRDS(file = paste0(corresp_path,"/",spp_single,"_length_age.rds")) %>%
    select(len_group,age_group) %>%
    rename(group = len_group)
  
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchKg_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      merge(corresp_tab) %>% 
      group_by(year,age_group) %>%
      summarise(obs = sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(group,year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(corresp_tab) %>%
    group_by(year,age_group) %>%
    summarise(sim = sum(sim)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") 
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catch_group/catch_group_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_catch_and_TAC <- function(catch_data,country_data,TAC_data,species,years){
  
  spp_tac_code <- case_when(species=="Lophius_piscatorius" ~ "ANF",
                            species=="Merluccius_merluccius" ~ "HKE",
                            species=="Lepidorhombus_whiffiagonis" ~ "LEZ",
                            species=="Nephrops_norvegicus" ~ "NEP",
                            species=="Raja_clavata" ~ "RJC",
                            species=="Leucoraja_naevus" ~ "RJN",
                            species=="Solea_solea" ~ "SOL")
  
  catchFR_prop <- country_data %>%
    filter(Country == "FRA") %>%
    pivot_wider(names_from = data_type) %>%
    rename(prop_sim = sim, prop_obs = obs)
  
  Catch <- catch_data %>%
    pivot_wider(names_from = data_type) %>%
    merge(catchFR_prop) %>%
    mutate(sim = sim * prop_sim,
           obs = obs * prop_obs) %>%
    select(year,sim,obs) %>%
    pivot_longer(c("sim","obs")) %>%
    rename(data_type=name)
  
  Data <- TAC_data %>%
    filter(Species.Code == spp_tac_code, 
           year %in% years) %>%
    group_by(year) %>%
    summarise(value=sum(Initial.Quantity)*1000) %>%
    ungroup() %>% 
    mutate(data_type='TAC') %>%
    rbind(Catch) %>%
    mutate(species=species)
  
  return(Data)
}

prep_catch_groupisis_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchKg_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) 
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(group,year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catch_group/catch_group_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_catchN_agegroup_byyear <- function(sim_data,species,years,obs_data_path,corresp_path,export,export_path){
  
  spp_single <- word(species,1,sep="_")
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  corresp_tab <- readRDS(file = paste0(corresp_path,"/",spp_single,"_length_age.rds")) %>%
    select(len_group,age_group) %>%
    rename(group = len_group)
  
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchN_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      merge(corresp_tab) %>% 
      group_by(year,age_group) %>%
      summarise(obs = sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(group,year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(corresp_tab) %>%
    group_by(year,age_group) %>%
    summarise(sim = sum(sim)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") 
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catchN_group/catchN_group_data_",spp_short,".rds"))
  }
  
  return(Data)
}

prep_catchN_groupisis_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchN_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) 
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(group,year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catchN_group/catchN_group_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_bycountry_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_country/",species,"/obs_landKg_",spp_short,"_country",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs= Landings_kg)
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    mutate(Country = case_when(grepl("BE",strategy) ~ "BEL",
                               grepl("ES",strategy) ~ "ESP",
                               grepl("UK",strategy) ~ "UK",
                               !(grepl("BE",strategy)|grepl("ES",strategy)|grepl("UK",strategy)) ~ "FRA")) %>%
    group_by(year,Country) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_country/land_country_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_byfleet_byseason <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_season/fleets/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_season/fleets/",fleet_group,"/",Files[i])) %>% 
      rename(obs= Landings_kg) %>%
      filter(year %in% years)
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>%
        mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                      fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                      fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                      fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                      fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                      fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                      fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                      fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                      fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                      fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                      fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                      fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                      fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12')) 
    }else if(fleet_group=="sup12"){
      NewDat <- NewDat  %>%
         mutate(fleet_isis = str_replace(fleet_isis,'bob_north-','North ')) %>%
         mutate(fleet_isis = str_replace(fleet_isis,'bob_south-','South ')) %>%
         mutate(fleet_isis = str_replace(fleet_isis,'@ ','')) %>%
         mutate(fleet_isis = str_replace(fleet_isis,'_',' and '))
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,season,fleet) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet,Season=season) %>%
    mutate(Season=Season+1) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,fleet_isis,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_season_fleet/land_season_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_byfleet_byyear <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_species/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_species/",fleet_group,"/",Files[i])) %>% 
      rename(obs= Landings_kg) %>%
      filter(year %in% years) %>%
      group_by(year,fleet_isis) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                                         fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                                         fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                                         fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                                         fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                                         fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                                         fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                                         fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                                         fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                                         fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                                         fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                                         fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                                         fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                                         fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12'))
    }else if(fleet_group=="sup12"){
      NewDat <- NewDat  %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_north-','North ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_south-','South ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'@ ','')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'_',' and '))
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,fleet) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_total_fleet/land_total_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_bymetier_byfleet_byyear <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_metier/fleets/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_metier/fleets/",fleet_group,"/",Files[i])) %>% 
      rename(obs= landing_kg) %>%
      filter(year %in% years) %>%
      mutate(metier_isis=if_else(grepl("BE",fleet_isis)|grepl("ES",fleet_isis)|grepl("UK",fleet_isis),
                                 substr(metier_isis, start = 1, stop = 7),
                                 metier_isis)) %>%
      group_by(year,fleet_isis,metier_isis) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>%
        mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                      fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                      fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                      fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                      fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                      fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                      fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                      fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                      fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                      fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                      fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                      fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                      fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12')) 
    }else if(fleet_group=="sup12"){
      NewDat <- NewDat  %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_north-','North ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_south-','South ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'@ ','')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'_',' and '))
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    mutate(metier_isis = if_else(grepl("27.",metier),
                                 word(metier,1,sep="-"),
                                 word(metier,1,sep="_Z"))) %>%
    mutate(metier_isis = word(metier_isis,1,sep="-")) %>%
    group_by(year,fleet,metier_isis) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,fleet_isis,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_metier_fleet/land_metier_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_bymetier_byfleet_byseason <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_metier_by_season/fleets/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_metier_by_season/fleets/",fleet_group,"/",Files[i])) %>% 
      rename(obs= Landings_kg) %>%
      filter(year %in% years)  %>%
      mutate(metier_isis=if_else(grepl("BE",fleet_isis)|grepl("ES",fleet_isis)|grepl("UK",fleet_isis),
                                 substr(metier_isis, start = 1, stop = 7),
                                 metier_isis)) %>%
      group_by(year,Season,fleet_isis,metier_isis) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>%
        mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                      fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                      fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                      fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                      fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                      fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                      fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                      fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                      fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                      fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                      fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                      fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                      fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12')) 
    }else if(fleet_group=="sup12"){
      NewDat <- NewDat  %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_north-','North ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'bob_south-','South ')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'@ ','')) %>%
        mutate(fleet_isis = str_replace(fleet_isis,'_',' and '))
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    mutate(metier_isis = if_else(grepl("27.",metier),
                                 word(metier,1,sep="-"),
                                 word(metier,1,sep="_Z"))) %>%
    mutate(metier_isis = word(metier_isis,1,sep="-")) %>%
    group_by(year,season,fleet,metier_isis) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet,Season=season) %>%
    mutate(Season=Season +1) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,fleet_isis,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_metier_season_fleet/land_metier_season_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_bymetier_byseason <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_metier_by_season/",species,"/obs_landKg_",spp_short,"_season_metier",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs= Landings_kg) %>%
      mutate(metier_isis=if_else(grepl("OTB",metier_isis)|
                                   grepl("GNS",metier_isis)|
                                   grepl("GTR",metier_isis)|
                                   grepl("LLS",metier_isis)|
                                   grepl("MIS",metier_isis)|
                                   grepl("TBB",metier_isis),
                                 substr(metier_isis, start = 1, stop = 7),
                                 metier_isis)) %>%
      group_by(year,Season,metier_isis) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,season,metier) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    mutate(metier_isis = if_else(grepl("27.",metier),
                                 word(metier,1,sep="-"),
                                 word(metier,1,sep="_Z"))) %>%
    mutate(metier_isis = word(metier_isis,1,sep="-")) %>%
    group_by(year,season,metier_isis) %>%
    summarise(sim = sum(sim)) %>%
    ungroup() %>%
    mutate(Season=season+1) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    mutate(Season = paste0(year," q.",Season)) %>%
    group_by(year,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup() %>%
    select(-c(season))
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_metier_season/land_met_seas_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_bymetier_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_metier/",species,"/obs_landKg_",spp_short,"_metier",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs= landing_kg) %>%
      mutate(metier_isis=if_else(grepl("OTB",metier_isis)|
                                   grepl("GNS",metier_isis)|
                                   grepl("GTR",metier_isis)|
                                   grepl("LLS",metier_isis)|
                                   grepl("MIS",metier_isis)|
                                   grepl("TBB",metier_isis),
                                 substr(metier_isis, start = 1, stop = 7),
                                 metier_isis)) %>%
      group_by(year,metier_isis) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,metier) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    mutate(metier_isis = if_else(grepl("27.",metier),
                                 word(metier,1,sep="-"),
                                 word(metier,1,sep="_Z"))) %>%
    mutate(metier_isis = word(metier_isis,1,sep="-")) %>%
    group_by(year,metier_isis) %>%
    summarise(sim = sum(sim)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup() 
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_metier/land_metier_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_byseason <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_season/",species,"/obs_landKg_",spp_short,"_season",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs= Landings_kg)
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    select(season,value,year) %>%
    group_by(year,season) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    mutate(Season=season+1) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    mutate(Season = paste0(year," q.",Season)) %>%
    group_by(year,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup() %>%
    select(-season)
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_season/land_seas_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_byspecies_byfleet_byseason <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_species_by_season/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_species_by_season/",fleet_group,"/",Files[i])) %>% 
      rename(obs= Landings_kg) %>%
      filter(year %in% years)
    
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>%
        mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                      fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                      fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                      fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                      fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                      fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                      fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                      fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                      fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                      fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                      fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                      fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                      fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12')) 
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,fleet,species,season) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               species == 'Lophius_piscatorius' ~ 'MNZ',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Leucoraja_naevus' ~ 'RJN'),
           Season = season + 1) %>%
    select(-season) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,fleet_isis,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_species_season_fleet/land_species_season_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_byspecies_byfleet_byyear <- function(sim_data,obs_data_path,fleet_group,years,export,export_path){
  
  Files <- list.files(paste0(obs_data_path,"/land_by_species/",fleet_group))
  Obs <- data.frame()
  for(i in seq(length(Files))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/land_by_species/",fleet_group,"/",Files[i])) %>% 
      rename(obs= Landings_kg) %>%
      filter(year %in% years)
    
    if(fleet_group=="inf12"){
      NewDat <- NewDat %>%
        mutate(fleet_isis = str_replace_all(fleet_isis,"'"," ")) %>% 
        mutate(fleet_isis = case_when(fleet_isis == 'Caseyeurs Métiers de l hameçon exclusifs-10-12' ~ 'pots and hooks 10-12',
                                      fleet_isis == 'Chalutiers Arts dormants-10-12' ~ 'trawl and passive gears 10-12',
                                      fleet_isis == 'Chalutiers Dragueurs-10-12' ~ 'dredge 10-12',
                                      fleet_isis == 'Chalutiers Tamiseurs-10-12' ~ 'sieve 10-12',
                                      fleet_isis == 'Chalutiers de fond exclusifs-10-12' ~ 'bottom-trawl 10-12',
                                      fleet_isis == 'Chalutiers mixtes exclusifs-10-12' ~ 'mixed-trawls 10-12',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-0-10' ~ 'nets and pots 0-10',
                                      fleet_isis == 'Fileyeurs Caseyeurs exclusifs-10-12' ~ 'nets and pots 10-12',
                                      fleet_isis == 'Fileyeurs Métiers de l hameçon exclusifs-10-12' ~ 'nets and hooks 10-12',
                                      fleet_isis == 'Fileyeurs exclusifs-0-10' ~ 'gillnet 0-10',
                                      fleet_isis == 'Fileyeurs exclusifs-10-12' ~ 'gillnet 10-12',
                                      fleet_isis == 'Flottilles agrégées-0-10' ~ 'aggregated fleets 0-10',
                                      fleet_isis == 'Flottilles agrégées-10-12' ~ 'aggregated fleets 10-12',
                                      fleet_isis == 'Palangriers exclusifs-0-10' ~ 'longline 0-10',
                                      fleet_isis == 'Palangriers exclusifs-10-12' ~ 'longline 10-12')) 
    }
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year,fleet,species) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    rename(fleet_isis=fleet) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               species == 'Lophius_piscatorius' ~ 'MNZ',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Leucoraja_naevus' ~ 'RJN')) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type") %>%
    group_by(year,fleet_isis,data_type) %>%
    mutate(value=value/sum(value)) %>%
    ungroup()
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_species_fleet/land_species_fleet_data_",fleet_group,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_land_groupisis_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_landKg_",spp_short,years[i],".rds")) %>% 
      mutate(year=years[i]) 
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(group,year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/land_group/land_group_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_N_agegroup <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "Solea",
                         species == "Lepidorhombus_whiffiagonis" ~ "Lepidorhombus",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "Lophius",
                         species == "Merluccius_merluccius" ~ "Merluccius",
                         species == "Nephrops_norvegicus" ~ "Nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/initial_abundance/",species,"/Abundance_",spp_short,"_",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs = N) %>%
      select(year,age,obs)
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    mutate(age = case_when(species=="Solea_solea" ~ as.character(group+2),
                           species=="Lepidorhombus_whiffiagonis" ~ as.character(group+1),
                           species=="Lophius_piscatorius" ~ as.character(group),
                           species %in% c("Raja_clavata","Leucoraja_naevus") ~ "all",
                           species == "Merluccius_merluccius" ~ group_to_age_merluccius(group))) %>%
    rename(sim = N_jan) %>%
    group_by(year,age) %>%
    summarise(sim=sum(sim)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/abundance_group/N_by_age_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_F_agegroup <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "Solea",
                         species == "Lepidorhombus_whiffiagonis" ~ "Lepidorhombus",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "Lophius",
                         species == "Merluccius_merluccius" ~ "Merluccius",
                         species == "Nephrops_norvegicus" ~ "Nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/F_bygroup/",species,"/F_",spp_short,"_",years[i],".rds")) %>% 
      mutate(year=years[i],
             age = as.character(age))  %>%
      rename(obs = Fmor) %>%
      select(year,age,obs)
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    mutate(age = case_when(species=="Solea_solea" ~ as.character(group+2),
                           species=="Lepidorhombus_whiffiagonis" ~ as.character(group+1),
                           species=="Lophius_piscatorius" ~ as.character(group),
                           species %in% c("Raja_clavata","Leucoraja_naevus") ~ "all",
                           species == "Merluccius_merluccius" ~ group_to_age_merluccius(group))) %>%
    rename(sim = Fmor) %>%
    select(year,age,sim) %>%
    group_by(year,age) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/F_group/F_by_age_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_recruit <- function(species,years,obs_data_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "Solea",
                         species == "Lepidorhombus_whiffiagonis" ~ "Lepidorhombus",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "Lophius",
                         species == "Merluccius_merluccius" ~ "Merluccius",
                         species == "Nephrops_norvegicus" ~ "Nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  file <- data.frame(name=list.files(paste0(obs_data_path,"/recruitment"))) %>%
    filter(grepl(spp_short,name))
  
  Data <-  fread(file = paste0(obs_data_path,"/recruitment/",file$name)) %>% 
    mutate(year=if_else(year=="mean","2023+",year)) %>%
    mutate(data_type = if_else(year=="2023+","mean","estim"))
  
  
  return(Data)
}

prep_recruit_sim <- function(species,years,sim_data_path){
  
  Data <- data.frame()
  for(y in seq(length(years))){
    
    Dat_new <-  readRDS(file = paste0(sim_data_path,"/abundance/",species,"/Njan_bygroup_",species,"_",years[y],".rds")) %>% 
      filter(group==0) %>%
      rename(rec_n = N_jan) %>%
      mutate(year=years[y],data_type = "sim") %>%
      select(-group)
    
    Data <- rbind(Data,Dat_new)
    
  }
  
  mean_rec <- Data %>%
    summarise(rec_n = exp(mean(log(rec_n)))) %>% 
    mutate(year="2023+",data_type="mean")
  
  Data <- Data %>%
    mutate(year=as.character(year)) %>%
    rbind(mean_rec)
  
  return(Data)
}

prep_SSB <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "Solea",
                         species == "Lepidorhombus_whiffiagonis" ~ "Lepidorhombus",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "Lophius",
                         species == "Merluccius_merluccius" ~ "Merluccius",
                         species == "Nephrops_norvegicus" ~ "Nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/ssb/",species,"/SSB_",spp_short,"_",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      rename(obs=SSB)
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    rename(sim = SSBjan) %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/ssb/SSBdata_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_total_catch_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "Raja")
    
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchKg_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      group_by(year) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catch_total/total_catch_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_total_catchN_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_catchN_",spp_short,"_groupisis",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      group_by(year) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/catchN_total/catchN_total_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_total_land_byyear <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "sole",
                         species == "Lepidorhombus_whiffiagonis" ~ "megrim",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "monkfish",
                         species == "Merluccius_merluccius" ~ "hake",
                         species == "Nephrops_norvegicus" ~ "nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/catch_by_groupisis/",species,"/obs_landKg_",spp_short,years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      group_by(year) %>%
      summarise(obs=sum(obs)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year) %>%
    summarise(sim = sum(value)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/total_land/total_land_data_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}

prep_total_N <- function(sim_data,species,years,obs_data_path,export,export_path){
  
  spp_short <- case_when(species == "Solea_solea" ~ "Solea",
                         species == "Lepidorhombus_whiffiagonis" ~ "Lepidorhombus",
                         species == "Leucoraja_naevus" ~ "Leucoraja",
                         species == "Lophius_piscatorius" ~ "Lophius",
                         species == "Merluccius_merluccius" ~ "Merluccius",
                         species == "Nephrops_norvegicus" ~ "Nephrops",
                         species == "Raja_clavata" ~ "Raja")
  
  Obs <- data.frame()
  for(i in seq(length(years))){
    NewDat <-  readRDS(file = paste0(obs_data_path,"/initial_abundance/",species,"/Abundance_",spp_short,"_",years[i],".rds")) %>% 
      mutate(year=years[i]) %>%
      group_by(year) %>%
      summarise(obs=sum(N)) %>%
      ungroup()
    
    Obs <- rbind(Obs,NewDat)
  }
  
  Data <- sim_data %>%
    group_by(year) %>%
    summarise(sim = sum(N_jan)) %>%
    ungroup() %>%
    merge(Obs) %>%
    pivot_longer(cols=c('sim','obs'),names_to="data_type")
  
  if(export){
    saveRDS(Data,file = paste0(export_path,"/total_abundance/N_total_",spp_short,"_",min(years),"_",max(years),".rds"))
  }
  
  return(Data)
}


### further analysis----
fleet_factor <- function(data,country){
  
  Data <- data %>% 
    filter(Country == country) %>%
    group_by(Country) %>%
    pivot_wider(names_from = data_type,values_from = value) %>%
    select(-year) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(ratio = obs/sim) %>%
    mutate(ratio = if_else(ratio==Inf,1,ratio)) 
  
  
  return(Data$ratio)
}

metier_frequency <- function(data){
  
  Obs_data <- data %>% 
    filter(data_type =="obs") %>%
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    mutate(freq = value/sum(value)) %>%
    arrange(-freq) %>%
    mutate(cumfreq = cumsum(freq))
  return(Obs_data)
}

main_metiers <- function(freq_data,thresh){
  
  last_met <-  freq_data %>%
    filter(cumfreq >= thresh) %>%
    filter(cumfreq == min(cumfreq)) %>% 
    select(metier_isis)
  
  metiers <- freq_data %>% 
    filter(cumfreq < thresh | metier_isis == last_met$metier_isis) %>% 
    select(metier_isis)
  
  return(metiers)
}

ratio_bymetier <- function(data,species){
  
  Out <- data %>%
    tidyr::pivot_wider(names_from = data_type, values_from = value) %>%
    mutate(ratio = obs/sim) %>%
    mutate(ratio = if_else(ratio==Inf,1,ratio)) %>%
    group_by(metier_isis) %>%
    summarise(ratio = mean(ratio)) %>%
    ungroup() %>% 
    mutate(Species=species)
  
  return(Out)
}

ratio_byseason <- function(data,species){
  
  Out <- data %>%
    tidyr::pivot_wider(names_from = data_type, values_from = value) %>%
    mutate(ratio = obs/sim) %>%
    mutate(ratio = if_else(ratio==Inf,1,ratio)) %>%
    mutate(Season=str_sub(Season,-1,-1)) %>%
    group_by(Season) %>%
    summarise(ratio = mean(ratio)) %>%
    ungroup() %>% 
    mutate(Species=species)
  
  return(Out)
}


### plotting functions----
plot_catch_and_TAC <- function(data,display_TAC=T){
  
  spp <- unique(data$species)
  
  if(!display_TAC){data <- data %>% filter(data_type != "TAC")}
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type,
                          linetype=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_linetype_manual(values=c("solid","solid","dotted"),guide = "none")+
    scale_color_manual(values = c("#006c86", "#4ebaaf","orange"),
                       labels = c("ref", "sim","TAC")) +
    labs(title = paste("Catch by year -", spp),color="Data type")+
    xlab(label = "Year")+
    ylab(label="Catch (kg)")+
    theme_bw()
  
  return(fig)
}

plot_catch_and_TAC_allSpp <- function(data,display_TAC=T){
  
  if(!display_TAC){data <- data %>% filter(data_type != "TAC")}
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type,
                          linetype=data_type)) +
    facet_wrap(~species,scales = "free_y")+
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_linetype_manual(values=c("solid","solid","dotted"),guide = "none")+
    scale_color_manual(values = c("#006c86", "#4ebaaf","orange"),
                       labels = c("ref", "sim","TAC")) +
    xlab(label = "Year")+
    ylab(label="Catch (kg)")+
    theme_bw()
  
  return(fig)
}

plot_catch_byage_merluccius <- function(data){
  
  facet_order <- c("Age 0","Age 1","Age 2","Age 3","Age 4",
                   "Age 5","Age 6","Age 7","Age 8",
                   "Age 9-10","Age 11-13","Age 14+")
  data <- data %>% mutate(age_group = paste("Age",age_group))
  
  fig <- ggplot(data, aes(x = as.character(year), y = value,fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order), scales = "free") +
    scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Catch by age group - Merluccius merluccius") )+
    xlab(label = "Year")+
    ylab(label="Catch (kg)")+
    theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  return(fig)
}

plot_catch_byage_nephrops <- function(data){
  
  data <- data %>% mutate(age_group = str_replace(age_group,"Female","Fem."))
  
  data1 <- data %>% filter(grepl("Male",age_group)) 
  
  data2 <- data %>% filter(!grepl("Male",age_group))
  
  facet_order1 <- c(paste("Male",1:13),
                    "Male 14-15",
                    "Male 16-18",
                    "Male 19-21",
                    "Male 22+")
  facet_order2 <- c("Mixed 0",
                    paste("Fem.",1:11),
                    "Fem. 12-13",
                    "Fem. 14-15",
                    "Fem. 16-17",
                    "Fem. 18-20",
                    "Fem. 21-23",
                    "Fem. 24-29",
                    "Fem. 30+")
  
  fig1 <- ggplot(data1, aes(x = as.character(year), y = value,fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order1), scales = "free") +
    scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = "Catch by age group - Nephrops norvegicus" )+
    xlab(label = "Year")+
    ylab(label="Catch (individuals)")+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  fig2 <- ggplot(data2, aes(x = as.character(year), y = value,fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order2), scales = "free") +
    scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = "Catch by age group - Nephrops norvegicus" )+
    xlab(label = "Year")+
    ylab(label="Catch (individuals)")+
    scale_y_continuous(n.breaks=3)+
    theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  return(list(fig1,fig2))
}

plot_catch_bygroup_byseason <- function(data,species){
  
  g_list <- data %>% select(group) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%4)
  list_of_fig <- list()
  
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$group[(1+4*(i-1)):(4+4*(i-1))]
    data_new <- data %>% 
      filter(group %in% g_new) 
    
    fig <- ggplot(data_new, aes(x = Season, y = value, fill=data_type)) +
      stat_summary(fun = "sum", geom = "bar", position = "dodge") +
      facet_wrap(~ group, scales = "free") +
      scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Catch by season -", species) )+
      xlab(label = "Year")+
      ylab(label="Catch (prop.)")+
      theme(axis.text.x=element_text(angle = 45, hjust = 1))
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
}

plot_catch_bygroup_byyear <- function(data,species){
  
  g_list <- data %>% select(group) %>% arrange(group) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%12)
  list_of_fig <- list()
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$group[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(group %in% g_new) 
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8)+
      geom_point(size=3)+
      scale_x_continuous(n.breaks = length(unique(data$year)))+
      facet_wrap(~group, scales = "free") +
      scale_color_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Catch by groupisis -", species) )+
      xlab(label = "Year")+
      ylab(label="Catch (kg)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45, hjust = 1),legend.title = element_blank())
    
    list_of_fig[[i]] <- fig
    
    
  }
  
  
  
  return(list_of_fig)
}

plot_catch_byseason <- function(data,species){
  
  fig <- ggplot(data, aes(x = Season, y = value, fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Catch by season -", species) )+
    xlab(label = "Season")+
    ylab(label="Catch (prop.)")+
    theme(axis.text.x=element_text(angle = 45, hjust = 1))
  
  return(fig)
}

plot_catch_total_byyear <- function(data,species){
  
  fig <- ggplot(data, aes(x = year, y = value, color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Total catch by year -", species) )+
    xlab(label = "Year")+
    ylab(label="Catch (kg)")+
    theme_classic()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20)
    )
  
  return(fig)
}

plot_catchN_bygroup_byyear <- function(data,species){
  
  g_list <- data %>% select(group) %>% arrange(group) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%12)
  list_of_fig <- list()
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$group[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(group %in% g_new) 
    
    
    fig <- ggplot(data_new, aes(x = as.character(year), y = value, fill= data_type)) +
      stat_summary(fun = "sum", geom = "bar", position = "dodge") +
      facet_wrap(~group, scales = "free") +
      scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Catch by groupisis -", species) )+
      xlab(label = "Year")+
      ylab(label="Catch (individuals)")+
      theme(axis.text.x=element_text(angle = 45, hjust = 1))
    
    list_of_fig[[i]] <- fig
    
    
  }
  
  
  
  return(list_of_fig)
}

plot_catchN_total_byyear <- function(data,species){
  
  fig <- ggplot(data, aes(x = as.character(year), y = value, fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Total catch by year -", species) )+
    xlab(label = "Year")+
    ylab(label="Catch (individuals)")
  theme_bw()
  
  return(fig)
}

plot_catch_bycountry_bygroup_byyear <- function(data,species){
  
  g_list <- data %>% select(group) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%5)
  list_of_fig <- list()
  
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$group[(1+5*(i-1)):(5+5*(i-1))]
    data_new <- data %>% 
      filter(group %in% g_new) 
    
    fig <- ggplot(data_new, aes(x = as.character(year), y = value, fill=data_type)) +
      stat_summary(fun = "sum", geom = "bar", position = "dodge") +
      facet_grid(group ~ Country, 
                 margins=FALSE,
                 labeller = labeller(.rows = label_both, .cols = label_value)) +
      scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Catch by country -", species) )+
      xlab(label = "Year")+
      ylab(label="Catch (prop.)")+
      theme_bw()
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
}

plot_diagonal <- function(data,species,metrics){
  
  # some ad hoc modifications (for the paper)
  # metrics <- str_replace(metrics,"landings","catch")
  #
  
  
  Colors <- c(brewer.pal(5, "Blues")[2],
              brewer.pal(5, "Blues")[3],
              brewer.pal(5, "Blues")[4],
              brewer.pal(5, "Blues")[5])
  
  Data <- data %>% pivot_wider(names_from = data_type,
                               values_from = value)
  
  
  fig <- ggplot(Data, aes(x = obs, 
                          y = sim, 
                          colour=as.character(year))) +
    scale_color_manual(values = Colors)+
    geom_point(size=2.5)+
    geom_abline(intercept = 0, 
                slope = 1,
                color="grey",
                linetype="dashed",
                linewidth=0.8)+
    xlim(min(data$value),max(data$value))+
    ylim(min(data$value),max(data$value))+
    labs(title = paste(species, "-",metrics),
         color = "Year")+
    xlab("ref")+
    theme_classic()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_text(size=24),
          legend.text = element_text(size=20)
    )
  
  return(fig)
}

plot_F_agegroup <- function(data,species){
  
  g_list <- data %>% select(age) %>% arrange(age) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%12)
  list_of_fig <- list()
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$age[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(age %in% g_new) 
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8)+
      geom_point(size=3)+
      scale_x_continuous(n.breaks = length(unique(data$year)))+
      facet_wrap(~age, scales = "free") +
      scale_color_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("F by age group -", species) )+
      xlab(label = "Year")+
      ylab(label="F")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45, hjust = 1),legend.title = element_blank())
    
    list_of_fig[[i]] <- fig
    
    
  }
  
  
  
  return(list_of_fig)
}

plot_fleet_weight <- function(data){
  
  fleet_order <- data %>% select(fleet_isis)
  
  fig <- ggplot(data, aes(x = prop, y = factor(fleet_isis,levels = fleet_order$fleet_isis))) +
    geom_col()+
    facet_grid(rows=vars(fleet_group),scale='free')+
    labs(title = "Cumulated frequency of landings by fleets")+
    xlab(label = "Proportion in landings")+
    ylab(label="Fleet")+
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9),
          title =element_text(size=8),
          axis.text.y = element_text(size=10),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(fig)
}

plot_heatmap <- function(data,metrics,by_fleet=F){
  # plot model skills as a heat map for one year (with  gradients)
  
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  year <- unique(data$year)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  
  
  Data <- data %>% 
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    # mutate(qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty)) %>%
    mutate(qty = str_replace_all(qty,"_"," "),
           species = str_replace(species,"_"," "))
  
  
  
  myOrder <- if_else(metrics %in% c("RwMSE","RwMSE","AAE"),"reverse","identity")
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= eval(parse(text = metrics))))+
    geom_tile() +
    scale_fill_distiller(palette = "Oranges",trans = myOrder)+
    labs(title = paste(metrics, year),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp <- function(data,metrics,from,to,by_fleet=F){
  # plot model skills improvement  between step "from" and step "to" as a heat map for one year (with  gradients)
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  year <- unique(data$year)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
 
  Data <- data %>% 
    filter(mod==from|mod==to) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(mod=case_when(mod==from ~ "mod_init",
                         mod==to ~"mod_fin")) %>%
    select(species,qty,mod,eval(metrics)) %>%
    pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
    mutate(met=metrics) %>%
    mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                          mod_init-mod_fin,
                          mod_fin-mod_init)) %>%
    #mutate(qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty)) %>%
    mutate(qty = str_replace_all(qty,"_"," "),
           species = str_replace(species,"_"," "))
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= diff))+
    geom_tile() +
    scale_fill_distiller(palette = "Blues",trans="reverse")+
    labs(title = paste(metrics, year),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp_period <- function(data,metrics,from,to,by_fleet=F){
  # plot model skills improvement  between step "from" and step "to" as a heat map for one year (with  gradients)
  
  Data <- data %>% 
    filter(mod==from|mod==to) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) 
  
  if(by_fleet){
    
    Ytitle <- "Fleets"
    
    Data <- Data %>%
      select(fleet_isis,fleet_group,period,qty,mod,eval(metrics)) %>%
      mutate(mod=case_when(mod==from ~ "mod_init",
                           mod==to ~"mod_fin")) %>%
      pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
      mutate(met=metrics) %>%
      mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                            mod_init-mod_fin,
                            mod_fin-mod_init)) %>%
      mutate(qty = str_replace_all(qty,"_"," "),
             period = if_else(period==1,"2015-2018","2019-2022"))
     
    
    # arrange by groups of fleets
    fleet_order <- Data %>%
      arrange(fleet_group) %>%
      select(fleet_isis) %>%
      distinct()
    fleet_order <- fleet_order$fleet_isis
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), 
                            y=factor(fleet_isis, level = fleet_order), 
                            fill=diff))+
      facet_grid(fleet_group ~ period,scale='free')
    
  }else{
    
    Ytitle <- "Species"
    
    Data <- Data %>%
      mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                                 species == 'Raja_clavata' ~ 'RJC',
                                 species == 'Nephrops_norvegicus' ~ 'NEP',
                                 species == 'Merluccius_merluccius' ~ 'HKE',
                                 species == 'Lophius_piscatorius' ~ 'MON',
                                 species == 'Leucoraja_naevus'  ~ 'RJN',
                                 species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                                 !species %in% c('Solea_solea',
                                                 'Raja_clavata',
                                                 'Nephrops_norvegicus',
                                                 'Merluccius_merluccius',
                                                 'Lophius_piscatorius',
                                                 'Leucoraja_naevus',
                                                 'Lepidorhombus_whiffiagonis') ~ species)) %>%
      select(species,period,qty,mod,eval(metrics)) %>%
      mutate(mod=case_when(mod==from ~ "mod_init",
                           mod==to ~"mod_fin")) %>%
      pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
      mutate(met=metrics) %>%
      mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                            mod_init-mod_fin,
                            mod_fin-mod_init)) %>%
      mutate(qty = str_replace_all(qty,"_"," "),
             period = if_else(period==1,"2015-2018","2019-2022"),
             species = str_replace(species,"_"," "))
    
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), 
                            y=factor(species, level = spp_order), 
                            fill=diff))+
      facet_wrap(~period)
  }
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  # plotting
  fig <- fig +
    geom_tile() +
    scale_fill_distiller(palette = "Blues",trans = "reverse")+
    labs(title = paste("From",from,"to",to),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=10),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp_period_rec <- function(data,metrics,from,to,by_fleet=F){
  # plot model skills improvement  between step "from" and step "to" as a heat map for one year (with  gradients)
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
  Data <- data %>% 
    filter(mod==from|mod==to) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(mod=case_when(mod==from ~ "mod_init",
                         mod==to ~"mod_fin")) %>%
    select(species,period,SR,qty,mod,eval(metrics)) %>%
    pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
    mutate(met=metrics) %>%
    mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                          mod_init-mod_fin,
                          mod_fin-mod_init)) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty=="land_metier" ~ 'Land./met.',
                           qty=="land_season" ~ 'Land./seas.',
                           qty=="land_metier_season" ~ 'Land./met.,seas.',
                           qty=="total_land" ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB')) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus' ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG'),
           period = if_else(period==1,"2015-2018","2019-2022"),
           #qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty),
           SR = if_else(SR,"SR","forced Rec."))
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    arrange(qty) %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  # empirical setting of min-max values
  min_val <- Data %>%
    mutate(min_val = case_when(met == 'AAE' ~ -0.3,
                               met == 'RwMSE' ~ -400,
                               met == 'RwMSE' ~ -1,
                               met == 'r' ~ -1.2,
                               met == 'MEF' ~ -300)) %>%
    select(min_val) %>%
    distinct() %>%
    as.numeric()
  
  max_val <- Data %>%
    mutate(max_val = case_when(met == 'AAE' ~ 0.7,
                               met == 'RwMSE' ~ 7000,
                               met == 'RwMSE' ~ 6,
                               met == 'r' ~ 1.2,
                               met == 'MEF' ~ 30)) %>%
    select(max_val) %>%
    distinct() %>%
    as.numeric()
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  # plotting
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= diff))+
    geom_tile() +
    facet_grid(SR ~ period)+
    scale_fill_distiller(palette = "Blues",
                         trans = "reverse",
                         limits = c(max_val,min_val))+
    labs(title = paste("From",from,"to",to),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp_thresh <- function(data,metrics,from,to,thresh=c(-0.1,-0.01,0.0,0.01,0.1),by_fleet=F){
  # plot model skills as a heat map for one year 
  year <- unique(data$year)
  
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  
  thresh_order <- c(paste("s <",thresh[1]),
                    paste(thresh[1],"< s <",thresh[2]),
                    paste(thresh[2],"< s <",thresh[3]),
                    paste(thresh[3],"< s <",thresh[4]),
                    paste(thresh[4],"< s <",thresh[5]),
                    paste(thresh[5],"< s"))
  
  Data <- data %>% 
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    filter(mod==from|mod==to) %>%
    mutate(mod=case_when(mod==from ~ "mod_init",
                         mod==to ~"mod_fin")) %>%
    select(species,qty,mod,eval(metrics)) %>%
    pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
    mutate(met=metrics) %>%
    mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                          mod_init-mod_fin,
                          mod_fin-mod_init)) %>%
    mutate(gp=case_when(diff <= thresh[1] ~ thresh_order[1],
                        thresh[1] < diff & diff <= thresh[2] ~ thresh_order[2],
                        thresh[2] < diff & diff <= thresh[3] ~ thresh_order[3],
                        thresh[3] < diff & diff <= thresh[4] ~ thresh_order[4],
                        thresh[4] < diff & diff <= thresh[5] ~ thresh_order[5],
                        thresh[5] < diff ~ thresh_order[6]) ) %>% 
    select(qty,species,gp) %>%
    drop_na()
  
  fill_colors <- brewer.pal(n=6,name="RdYlBu")
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= factor(gp, level = thresh_order)))+
    geom_tile() +
    scale_fill_manual(values = fill_colors,
                      breaks = thresh_order)+
    labs(title = paste(metrics, year),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp_thresh_period <- function(data,metrics,from,to,thresh=c(-0.1,-0.01,0,0.01,0.1),by_fleet=F,fleet_order=NULL){
  # plot model skills as a heat map for one year (with  thresholds)
  mod <- unique(data$mod)
  
  thresh_order <- c(paste("s <",thresh[1]),
                    paste(thresh[1],"< s <",thresh[2]),
                    paste(thresh[2],"< s <",thresh[3]),
                    paste(thresh[3],"< s <",thresh[4]),
                    paste(thresh[4],"< s <",thresh[5]),
                    paste(thresh[5],"< s"))
  
  Data <- data %>% 
    filter(mod==from|mod==to) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) 
  
  fill_colors <- brewer.pal(n=6,name="RdYlBu")
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  
  if(by_fleet){
    
    Data <- Data %>%
      mutate(mod=case_when(mod==from ~ "mod_init",
                           mod==to ~"mod_fin")) %>%
      select(fleet_isis,fleet_group,period,qty,mod,SR,eval(metrics)) %>%
      pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
      mutate(met=metrics) %>%
      mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                            mod_init-mod_fin,
                            mod_fin-mod_init)) %>%
      # mutate(qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty)) %>%
      mutate(qty = str_replace_all(qty,"_"," "),
             period = if_else(period==1,"2015-2018","2019-2022"),
             gp=case_when(diff <= thresh[1] ~ thresh_order[1],
                          thresh[1] < diff & diff <= thresh[2] ~ thresh_order[2],
                          thresh[2] < diff & diff <= thresh[3] ~ thresh_order[3],
                          thresh[3] < diff & diff <= thresh[4] ~ thresh_order[4],
                          thresh[4] < diff & diff <= thresh[5] ~ thresh_order[5],
                          thresh[5] < diff ~ thresh_order[6])) %>%
      select(qty,period,fleet_isis,fleet_group,gp) %>%
      drop_na()
    
    Ytitle <- "Fleets"
    
    
    # arrange by groups of fleets and importance in landings
    if(is.null(fleet_order)){
      fleet_order <- Data %>%
        arrange(fleet_group,fleet_isis) %>%
        select(fleet_isis) %>%
        distinct()
      fleet_order <- fleet_order$fleet_isis
    }
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), 
                            y=factor(fleet_isis, level = fleet_order), fill= factor(gp, level = thresh_order)))+
      geom_tile() +
      facet_grid(fleet_group ~ period, scale = 'free')
    
  }else{
    
    Data <- Data %>%
      mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                                 species == 'Raja_clavata' ~ 'RJC',
                                 species == 'Nephrops_norvegicus' ~ 'NEP',
                                 species == 'Merluccius_merluccius' ~ 'HKE',
                                 species == 'Lophius_piscatorius' ~ 'MON',
                                 species == 'Leucoraja_naevus'  ~ 'RJN',
                                 species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                                 !species %in% c('Solea_solea',
                                                 'Raja_clavata',
                                                 'Nephrops_norvegicus',
                                                 'Merluccius_merluccius',
                                                 'Lophius_piscatorius',
                                                 'Leucoraja_naevus',
                                                 'Lepidorhombus_whiffiagonis') ~ species)) %>%
      mutate(mod=case_when(mod==from ~ "mod_init",
                           mod==to ~"mod_fin")) %>%
      select(species,period,qty,mod,SR,eval(metrics)) %>%
      pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
      mutate(met=metrics) %>%
      mutate(diff = if_else(met %in% c("RwMSE","AAE","AE"),
                            mod_init-mod_fin,
                            mod_fin-mod_init)) %>%
      # mutate(qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty)) %>%
      mutate(qty = str_replace_all(qty,"_"," "),
             period = if_else(period==1,"2015-2018","2019-2022"),
             species = str_replace(species,"_"," "),
             gp=case_when(diff <= thresh[1] ~ thresh_order[1],
                          thresh[1] < diff & diff <= thresh[2] ~ thresh_order[2],
                          thresh[2] < diff & diff <= thresh[3] ~ thresh_order[3],
                          thresh[3] < diff & diff <= thresh[4] ~ thresh_order[4],
                          thresh[4] < diff & diff <= thresh[5] ~ thresh_order[5],
                          thresh[5] < diff ~ thresh_order[6])) %>%
      select(qty,period,species,gp) %>%
      drop_na()
    
    Ytitle <- "Species"
    
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= factor(gp, level = thresh_order)))+
      geom_tile() +
      facet_wrap(~period)
  }
  
  fig <- fig+
    scale_fill_manual(values = fill_colors,
                      breaks = thresh_order)+
    labs(title = paste("From",from,"to",to),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=10),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_imp_thresh_period_rec <- function(data,metrics,from,to,thresh=c(-0.1,-0.01,0,0.01,0.1),by_fleet=F){
  # plot model skills as a heat map for one year (with  gradients)
  
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  
  mod <- unique(data$mod)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
  
  thresh_order <- c(paste("s <",thresh[1]),
                    paste(thresh[1],"< s <",thresh[2]),
                    paste(thresh[2],"< s <",thresh[3]),
                    paste(thresh[3],"< s <",thresh[4]),
                    paste(thresh[4],"< s <",thresh[5]),
                    paste(thresh[5],"< s"))
  
  Data <- data %>%
    filter(mod==from|mod==to) %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(mod=case_when(mod==from ~ "mod_init",
                         mod==to ~"mod_fin")) %>%
    select(species,period,SR,qty,mod,eval(metrics)) %>%
    pivot_wider(names_from = mod, values_from = eval(metrics)) %>%
    mutate(met=metrics) %>%
    mutate(diff = if_else(met %in% c("RwMSE","RwMSE","AAE"),
                          mod_init-mod_fin,
                          mod_fin-mod_init)) %>%
    mutate(period = if_else(period==1,"2015-2018","2019-2022"),
           SR = if_else(SR,"SR","forced Rec."),
           #qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty),
           gp=case_when(diff <= thresh[1] ~ thresh_order[1],
                        thresh[1] < diff & diff <= thresh[2] ~ thresh_order[2],
                        thresh[2] < diff & diff <= thresh[3] ~ thresh_order[3],
                        thresh[3] < diff & diff <= thresh[4] ~ thresh_order[4],
                        thresh[4] < diff & diff <= thresh[5] ~ thresh_order[5],
                        thresh[5] < diff ~ thresh_order[6])) %>%
    select(qty,period,SR,species,gp) %>%
    drop_na()
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    arrange(qty) %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  
  
  fill_colors <- brewer.pal(n=6,name="RdYlBu")
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= factor(gp, level = thresh_order)))+
    geom_tile() +
    facet_grid(SR ~ period)+
    scale_fill_manual(values = fill_colors,
                      breaks = thresh_order)+
    labs(title = paste("From",from,"to",to),
         fill=TeX(paste("$\\Delta$",metrics)))+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_period <- function(data,metrics,by_fleet=F){
  # plot model skills as a heat map for one year (with  gradients)
  
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  mod <- unique(data$mod)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
 
  
  Data <- data %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    # mutate(qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty)) %>%
    mutate(qty = str_replace_all(qty,"_"," "),
           period = if_else(period==1,"2015-2018","2019-2022"),
           species = str_replace(species,"_"," "))
  
  
  myOrder <- if_else(metrics %in% c("RwMSE","RwMSE","AAE"),"reverse","identity")
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= eval(parse(text = metrics))))+
    geom_tile() +
    facet_wrap(~period)+
    scale_fill_distiller(palette = "Oranges",trans = myOrder)+
    labs(title = paste(mod,metrics),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_period_rec <- function(data,metrics,by_fleet=F){
  # plot model skills as a heat map for one year (with  gradients)
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  mod <- unique(data$mod)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
  Data <- data %>% 
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(period = if_else(period==1,"2015-2018","2019-2022"),
           #qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty),
           SR = if_else(SR,"SR","forced Rec."))
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    arrange(qty) %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  myOrder <- if_else(metrics %in% c("RwMSE","RwMSE","AAE"),"reverse","identity")
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= eval(parse(text = metrics))))+
    geom_tile() +
    facet_grid(SR ~ period)+
    scale_fill_distiller(palette = "Oranges",trans = myOrder)+
    labs(title = paste(mod,metrics),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_thresh <- function(data,metrics,thresh=c(0,0.2,0.4,0.8,1),by_fleet=F,sym=F){
  # plot model skills as a heat map for one year (with )
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  year <- unique(data$year)
  
  
  thresh_order <- c(paste("s <",thresh[1]),
                    paste(thresh[1],"< s <",thresh[2]),
                    paste(thresh[2],"< s <",thresh[3]),
                    paste(thresh[3],"< s <",thresh[4]),
                    paste(thresh[4],"< s <",thresh[5]),
                    paste(thresh[5],"< s"))
  
  Data <- data %>% 
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(gp=case_when(eval(parse(text = metrics)) <= thresh[1] ~ thresh_order[1],
                        thresh[1] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[2] ~ thresh_order[2],
                        thresh[2] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[3] ~ thresh_order[3],
                        thresh[3] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[4] ~ thresh_order[4],
                        thresh[4] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[5] ~ thresh_order[5],
                        thresh[5] < eval(parse(text = metrics)) ~ thresh_order[6]) ) %>% 
    select(qty,species,gp) %>%
    drop_na()
  
  if(metrics %in% c("RwMSE","AAE")){
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- brewer.pal(n=6,name="Oranges")
    }
    
  } else {
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- rev(brewer.pal(n=6,name="Oranges"))
    }
    
  }
  
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86"))
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= factor(gp, level = thresh_order)))+
    geom_tile() +
    scale_fill_manual(values = fill_colors,
                      breaks=thresh_order)+
    labs(title = paste(metrics, year),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_heatmap_thresh_period <- function(data,metrics,thresh=c(0,0.2,0.4,0.8,1),by_fleet=F,sym=F,fleet_order=NULL){
  # plot model skills as a heat map for one year (with  thresholds)
  mod <- unique(data$mod)
  
  quality_level <- case_when(metrics %in% c("MEF","r") ~ c("Poor:",
                                                           "        ",
                                                           "Acceptable:",
                                                           "                   ",
                                                           "Good:",
                                                           "          "),
                             metrics == "RwMSE" ~ c("Good:",
                                                    "          ",
                                                    "Acceptable:",
                                                    "                    ",
                                                    "Poor:",
                                                    "        "),
                             metrics == "AE" ~ c("Poor:",
                                                 "Acceptable:",
                                                 "Good:",
                                                 "Good:",                                                 
                                                 "Acceptable:",
                                                 "Poor:"))
  
  thresh_order <- c(paste(quality_level[1],metrics,"<",thresh[1]),
                    paste(quality_level[2],thresh[1],"<",metrics,"<",thresh[2]),
                    paste(quality_level[3],thresh[2],"<",metrics,"<",thresh[3]),
                    paste(quality_level[4],thresh[3],"<",metrics,"<",thresh[4]),
                    paste(quality_level[5],thresh[4],"<",metrics,"<",thresh[5]),
                    paste(quality_level[6],thresh[5],"<",metrics))
  
  Data <- data %>%
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(qty = str_replace_all(qty,"_"," "),
           period = if_else(period==1,"2015-2018","2019-2022"),
           gp=case_when(eval(parse(text = metrics)) <= thresh[1] ~ thresh_order[1],
                        thresh[1] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[2] ~ thresh_order[2],
                        thresh[2] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[3] ~ thresh_order[3],
                        thresh[3] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[4] ~ thresh_order[4],
                        thresh[4] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[5] ~ thresh_order[5],
                        thresh[5] < eval(parse(text = metrics)) ~ thresh_order[6])) 
  
  if(metrics %in% c("RwMSE","AAE")){
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- brewer.pal(n=6,name="Oranges")
    }
    
  } else {
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- rev(brewer.pal(n=6,name="Oranges"))
    }
    
  }
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86")) %>%
    arrange(color)
  
  if(by_fleet){
    
    Data <- Data %>% select(qty,period,fleet_isis,fleet_group,gp) %>% drop_na()
    
    # arrange by groups of fleets and importance in landings
    if(is.null(fleet_order)){
      fleet_order <- Data %>%
        arrange(fleet_group,fleet_isis) %>%
        select(fleet_isis) %>%
        distinct()
      fleet_order <- fleet_order$fleet_isis
    }
    
    
    # plot
    Ytitle <- "Fleets"
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), 
                            y=factor(fleet_isis,level = fleet_order), 
                            fill= factor(gp, level = thresh_order)))+
      facet_grid(fleet_group ~ period,scale='free')
    
  }else{
    
    Data <- Data %>%
      mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                                 species == 'Raja_clavata' ~ 'RJC',
                                 species == 'Nephrops_norvegicus' ~ 'NEP',
                                 species == 'Merluccius_merluccius' ~ 'HKE',
                                 species == 'Lophius_piscatorius' ~ 'MON',
                                 species == 'Leucoraja_naevus'  ~ 'RJN',
                                 species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                                 !species %in% c('Solea_solea',
                                                 'Raja_clavata',
                                                 'Nephrops_norvegicus',
                                                 'Merluccius_merluccius',
                                                 'Lophius_piscatorius',
                                                 'Leucoraja_naevus',
                                                 'Lepidorhombus_whiffiagonis') ~ species)) %>%
      mutate(species = str_replace(species,"_"," ")) %>%
      select(qty,period,species,gp) %>%
      drop_na()
    
   # arrange 
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
    
    # plot
    Ytitle <- "Species"
    
    fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species,level = spp_order), fill= factor(gp, level = thresh_order)))+
      facet_wrap(~period)
    
  }
  
  fig <- fig+
    geom_tile() +
    scale_fill_manual(values = fill_colors,
                      breaks=thresh_order)+
    labs(title = paste(mod,metrics),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=9),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  
  return(fig)
}

plot_heatmap_thresh_period_rec <- function(data,metrics,thresh=c(0,0.2,0.4,0.8,1),by_fleet=F,sym=F){
  # plot model skills as a heat map for one year (with  gradients)
  if(by_fleet){
    data <- data %>% rename(species=fleet_isis)
    Ytitle <- "Fleets"
  }else{
    Ytitle <- "Species"
  }
  
  mod <- unique(data$mod)
  
  # ices_qty <- c("Abundance/gp.",
  #               "Catch(kg)/gp.",
  #               "Catch(kg)(tot.)",
  #               "Catch(N)/gp.",
  #               "Catch(N)(tot.)",
  #               "F/gp.",
  #               "SSB",
  #               "Abundance(tot.)",
  #               "Land.(tot.)")
  # 
  
  thresh_order <- c(paste("s <",thresh[1]),
                    paste(thresh[1],"< s <",thresh[2]),
                    paste(thresh[2],"< s <",thresh[3]),
                    paste(thresh[3],"< s <",thresh[4]),
                    paste(thresh[4],"< s <",thresh[5]),
                    paste(thresh[5],"< s"))
  
  Data <- data %>% 
    mutate(qty = case_when(qty=="abundance_group" ~ 'Abundance/gp.',
                           qty=="total_abundance" ~ 'Abundance(tot.)',
                           qty=="catch_group" ~ 'Catch(kg)/gp.',
                           qty=="catch_total" ~ 'Catch(kg)(tot.)',
                           qty=="catchN_group" ~ 'Catch(N)/gp.',
                           qty=="catchN_total" ~ 'Catch(N)(tot.)',
                           qty=="F_group" ~ 'F/gp.',
                           qty=="land_country" ~ 'Land./ctry',
                           qty %in% c("land_metier","land_metier_fleet") ~ 'Land./met.',
                           qty %in% c("land_season","land_season_fleet") ~ 'Land./seas.',
                           qty %in% c("land_metier_season","land_metier_season_fleet") ~ 'Land./met.,seas.',
                           qty %in% c("total_land","land_total_fleet") ~ 'Land.(tot.)',
                           qty=="ssb" ~ 'SSB',
                           qty == "land_species_fleet" ~ "Land./spe.",
                           qty=="land_species_season_fleet" ~ "Land./spe.,seas.")) %>%
    mutate(species = case_when(species == 'Solea_solea' ~ 'SOL',
                               species == 'Raja_clavata' ~ 'RJC',
                               species == 'Nephrops_norvegicus' ~ 'NEP',
                               species == 'Merluccius_merluccius' ~ 'HKE',
                               species == 'Lophius_piscatorius' ~ 'MON',
                               species == 'Leucoraja_naevus'  ~ 'RJN',
                               species == 'Lepidorhombus_whiffiagonis' ~ 'MEG',
                               !species %in% c('Solea_solea',
                                               'Raja_clavata',
                                               'Nephrops_norvegicus',
                                               'Merluccius_merluccius',
                                               'Lophius_piscatorius',
                                               'Leucoraja_naevus',
                                               'Lepidorhombus_whiffiagonis') ~ species)) %>%
    mutate(period = if_else(period==1,"2015-2018","2019-2022"),
           SR = if_else(SR,"SR","forced Rec."),
           #qty = if_else(qty %in% ices_qty,paste(qty,"*"),qty),
           gp=case_when(eval(parse(text = metrics)) <= thresh[1] ~ thresh_order[1],
                        thresh[1] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[2] ~ thresh_order[2],
                        thresh[2] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[3] ~ thresh_order[3],
                        thresh[3] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[4] ~ thresh_order[4],
                        thresh[4] < eval(parse(text = metrics)) & eval(parse(text = metrics)) <= thresh[5] ~ thresh_order[5],
                        thresh[5] < eval(parse(text = metrics)) ~ thresh_order[6])) %>%
    select(qty,period,SR,species,gp) %>%
    drop_na()
  
  text_colors <- Data %>% 
    select(qty) %>% 
    distinct() %>%
    arrange(qty) %>%
    mutate(color= if_else(grepl("Land",qty)|grepl("Catch",qty),"#4ebaaf","#006c86")) %>%
    arrange(color)
  
  
  
  if(metrics %in% c("RwMSE","AAE")){
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- brewer.pal(n=6,name="Oranges")
    }
    
  } else {
    if(sym){
      fill_colors <- rev(brewer.pal(n=6,name="BrBG"))
    }else{
      fill_colors <- rev(brewer.pal(n=6,name="Oranges"))
    }
    
  }
  
  
  if(by_fleet){
    # arrange by groups of fleets
    spp_order <- Data %>%
      mutate(group = case_when(grepl("ES",species)|
                                 grepl("BE",species)|
                                 grepl("UK",species) ~ 1,
                               grepl("bob north",species)|
                                 grepl("bob south",species)|
                                 grepl("other",species) ~ 2,
                               grepl("Chalutiers",species)|
                                 grepl("Fileyeurs",species)|
                                 grepl("Flottilles",species)|
                                 grepl("Palangriers",species) ~ 3)) %>%
      arrange(group) %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }else{
    spp_order <- Data %>%
      select(species) %>%
      distinct()
    spp_order <- spp_order$species
  }
  
  fig <- ggplot(Data, aes(x=factor(qty,level = text_colors$qty), y=factor(species, level = spp_order), fill= factor(gp, level = thresh_order)))+
    geom_tile() +
    facet_grid(SR ~ period)+
    scale_fill_manual(values = fill_colors,
                      breaks=thresh_order)+
    labs(title = paste(mod,metrics),
         fill=metrics)+
    xlab("Variable")+
    ylab(Ytitle) +
    theme(axis.text.x = element_text(size = 14,
                                     angle = 60, 
                                     vjust = 0.9, 
                                     hjust= 0.9,
                                     color=text_colors$color),
          title =element_text(size=8),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          legend.text = element_text(size=10),
          legend.title = element_text(size=16),
          panel.background = element_rect(fill = "darkgrey",
                                          colour = "black",
                                          linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return(fig)
}

plot_hist_spp <- function(data,metrics,w_ratio,low_var,high_var){
  # high_var: observed variables -> high weight
  # low_var: model predicted variable -> low weight
  # w_ratio is: high_var weights/low_var weights
  
  mod <- unique(data$mod)
  
  # set variable weightings
  n_period <- data %>% select(period) %>% n_distinct()
  n_SR <- data %>% select(SR) %>% n_distinct()
  
  low_wt <- 1/(n_period * n_SR *(length(low_var)+ w_ratio*length(high_var)) )
  high_wt <- w_ratio * low_wt
  
  # prepare data
  Data <- data %>%
    select(species,qty,eval(metrics)) %>%
    rename(score =eval(metrics)) %>%
    mutate(species=case_when(species == "Lepidorhombus_whiffiagonis" ~ "L. whiffiagonis",
                             species == "Solea_solea" ~ "S. solea",
                             species == "Lophius_piscatorius" ~ "L. piscatorius",
                             species == "Nephrops_norvegicus" ~ "N. norvegicus",
                             species == "Merluccius_merluccius" ~ "M. merluccius",
                             species == "Raja_clavata" ~ "R. clavata",
                             species == "Leucoraja_naevus" ~ "L. naevus"))%>%
    #mutate(score = if_else(score>0,score,0))%>%
    mutate(score = if_else(qty %in% high_var, high_wt * score, low_wt * score)) %>%
    group_by(species) %>%
    summarise(score=sum(score)) %>%
    ungroup() %>% 
    arrange(score)
  
  my_order <- Data$species
  
  # plotting
  fig <- ggplot(Data, 
                aes(x = factor(species,level=my_order), y = score)) +
    stat_summary(geom = "bar") +
    labs(title = mod)+
    xlab(label = "Species")+
    ylab(label=paste(metrics,"- mean score"))+
    annotate("text",
             x = 1.8, y = max(Data$score)-0.02,
             label = paste("w-ratio =",w_ratio),
             size = 10)+
    theme_bw()+
    theme(title=element_text(size=18),
          axis.text.x = element_text(size=18,
                                     angle = 45, 
                                     vjust = 0.9, 
                                     hjust= 0.9),
          axis.text.y = element_text(size=18),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=22),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(fig)
}

plot_land_bycountry_byyear <- function(data,species,cst_scale = T){
  
  facet_order <- data %>% 
    filter(data_type=="obs") %>%
    group_by(Country) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    arrange(-value)
  
  fig <- ggplot(data, aes(x = year, y = value, color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    facet_wrap(~ factor(Country,level=facet_order$Country),scales="free") +
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Landings by country -", species) )+
    xlab(label = "Year")+
    ylab(label="Landings (prop.)")+
    theme_bw()+
    theme(legend.title = element_blank())
  
  if(cst_scale){ fig <- fig + ylim(0,1)}
  
  return(fig)
}

plot_land_byfleet_byseason <- function(data,cst_scale = T){
  
  facet_order <- data %>% 
    filter(data_type=="obs") %>%
    group_by(fleet_isis) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    arrange(-value)
  
  this_data <- data %>% mutate(year=case_when(Season == 1 ~ year,
                                              Season == 2  ~ year + 0.25,
                                              Season == 3  ~ year + 0.5,
                                              Season == 4  ~ year + 0.75))
  
  fig <- ggplot(this_data, aes(x = year, 
                          y = value, 
                          color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    facet_wrap(~ factor(fleet_isis,level=facet_order$fleet_isis),scales="free") +
    scale_x_continuous(breaks = unique(data$year))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = "Landings by fleet")+
    xlab(label = "Year")+
    ylab(label="Landings (kg)")+
    theme_bw()+
    theme(legend.title = element_blank())
  
  return(fig)
}

plot_land_byfleet_byyear <- function(data,cst_scale = T){
  
  facet_order <- data %>% 
    filter(data_type=="obs") %>%
    group_by(fleet_isis) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    arrange(-value)
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    facet_wrap(~ factor(fleet_isis,level=facet_order$fleet_isis),scales="free") +
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = "Landings by fleet")+
    xlab(label = "Year")+
    ylab(label="Landings (kg)")+
    theme_bw()+
    theme(legend.title = element_blank())
  
  return(fig)
}

plot_land_bymetier_byfleet_byseason <- function(data,cst_scale = T){
  
  # preparation
  m_list <- data %>%
    filter(data_type=="obs") %>%
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>% 
    distinct() 
  
  n_fig <- 1 + ((nrow(m_list)-1)%/%12)
  list_of_fig <- list()
  
  years <- unique(data$year)
  
  for(i in 1:n_fig){
    
    m_new <- m_list$metier_isis[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(metier_isis %in% m_new) %>%
      mutate(year=case_when(Season == 1 ~ year,
                            Season == 2  ~ year + 0.25,
                            Season == 3  ~ year + 0.5,
                            Season == 4  ~ year + 0.75))
    
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8) +
      geom_point(size=3) +
      scale_x_continuous(breaks = years)+
      scale_color_manual(values = c("#006c86", "#4ebaaf"),
                         labels = c("ref", "sim")) +
      labs(title = unique(data$fleet_isis)) +
      xlab(label = "Years")+
      ylab(label="Landings (prop.)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45),
            panel.background = element_rect(color = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            legend.title = element_blank())
    
    if(cst_scale){ fig <- fig + ylim(0,max(data$value))}
    
    if(i == 1){
      
      met_order1 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order1$metier_isis), scales = "free") 
      
    }else if(i==2){
      
      met_order2 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order2$metier_isis), scales = "free") 
      
    }else if(i == 3){
      
      met_order3 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order3$metier_isis), scales = "free") 
      
    }else{  fig <- fig +facet_wrap(~ metier_isis, scales = "free") }
    
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
  
}

plot_land_bymetier_byfleet_byyear <- function(data,cst_scale = T){
  
  # preparation
  m_list <- data %>%
    filter(data_type=="obs") %>%
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>% 
    distinct() 
  
  n_fig <- 1 + ((nrow(m_list)-1)%/%12)
  list_of_fig <- list()
  
  years <- unique(data$year)
 
  for(i in 1:n_fig){
    
    m_new <- m_list$metier_isis[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% filter(metier_isis %in% m_new) 
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8) +
      geom_point(size=3) +
      scale_x_continuous(breaks = years)+
      scale_color_manual(values = c("#006c86", "#4ebaaf"),
                         labels = c("ref", "sim")) +
      labs(title = unique(data$fleet_isis)) +
      xlab(label = "Years")+
      ylab(label="Landings (prop.)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45),
            panel.background = element_rect(color = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            legend.title = element_blank())
    
    if(cst_scale){ fig <- fig + ylim(0,max(data$value))}
    
    if(i == 1){
      
      met_order1 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order1$metier_isis), scales = "free") 
      
    }else if(i==2){
      
      met_order2 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order2$metier_isis), scales = "free") 
      
    }else if(i == 3){
      
      met_order3 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order3$metier_isis), scales = "free") 
      
    }else{  fig <- fig +facet_wrap(~ metier_isis, scales = "free") }
    
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
  
}

plot_land_bymetier_byseason <- function(data,main_metier,species,cst_scale = T){
  
  # preparation
  m_list <- data %>%
    filter(!metier_isis %in% main_metier$metier_isis,
           data_type=="obs") %>%
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>% 
    distinct() 
  
  n_fig <- 2 + ((nrow(m_list)-1)%/%12)
  list_of_fig <- list()
  
  years <- unique(data$year)
  
  
  
  # first panel: main metiers
  data_main <- data %>% 
    filter(metier_isis %in% main_metier$metier_isis) %>%
    mutate(year=case_when(str_detect(Season,'q.1') ~ year,
                          str_detect(Season,'q.2') ~ year + 0.25,
                          str_detect(Season,'q.3') ~ year + 0.5,
                          str_detect(Season,'q.4') ~ year + 0.75))
  
  met_order <- data_main %>% 
    filter(data_type=="obs") %>% 
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>%
    distinct()
  
  fig <- ggplot(data_main, aes(x = year, y = value, color= data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(breaks = years)+
    facet_wrap(~ factor(metier_isis,level=met_order$metier_isis), scales = "free") +
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Landings profile by metier (main metiers) -", species))+
    xlab(label = "Year")+
    ylab(label="Landings (prop.)")+
    theme_bw()+
    theme(plot.title = element_text(color="#006c86",face="bold"),
          axis.text.x=element_text(angle = 45 ),
          legend.title = element_blank())
  
  if(cst_scale){ fig <- fig + ylim(0,max(data$value))}
  
  list_of_fig[[1]] <- fig
  
  for(i in 2:n_fig){
    
    m_new <- m_list$metier_isis[(1+12*(i-2)):(12+12*(i-2))]
    data_new <- data %>% 
      filter(metier_isis %in% m_new) %>%
      mutate(year=case_when(str_detect(Season,'q.1') ~ year,
                            str_detect(Season,'q.2') ~ year + 0.25,
                            str_detect(Season,'q.3') ~ year + 0.5,
                            str_detect(Season,'q.4') ~ year + 0.75))
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8) +
      geom_point(size=3) +
      scale_x_continuous(breaks = years)+
      scale_color_manual(values = c("darkgrey", "grey"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Landings profile by metier -", species) ) +
      xlab(label = "Season")+
      ylab(label="Landings (prop.)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45),
            panel.background = element_rect(color = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            legend.title = element_blank())
    
    if(cst_scale){ fig <- fig + ylim(0,max(data$value))}
    
    if(i == 2){
      
      met_order2 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order2$metier_isis), scales = "free") 
      
    }else if(i==3){
      
      met_order3 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order3$metier_isis), scales = "free") 
      
    }else if(i == 4){
      
      met_order4 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
      
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order4$metier_isis), scales = "free") 
      
    }else{  fig <- fig +facet_wrap(~ metier_isis, scales = "free") }
    
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
}

plot_land_bymetier_byyear <- function(data,main_metier,species,cst_scale = T){
  
  # preparation
  m_list <- data %>%
    filter(!metier_isis %in% main_metier$metier_isis,
           data_type=="obs") %>%
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>% 
    distinct() 
  
  n_fig <- 2 + ((nrow(m_list)-1)%/%12)
  list_of_fig <- list()
  
  # first panel: main metiers
  data_main <- data %>% filter(metier_isis %in% main_metier$metier_isis)
  met_order <- data_main %>% 
    filter(data_type=="obs") %>% 
    group_by(metier_isis) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    arrange(-value) %>%
    select(metier_isis) %>%
    distinct()
  
  fig <- ggplot(data_main, aes(x = year, y = value, color= data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(breaks = unique(data$year))+
    facet_wrap(~ factor(metier_isis,level=met_order$metier_isis), scales = "free") +
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Landings profile by metier (main metiers) -", species))+
    xlab(label = "Year")+
    ylab(label="Landings (prop.)")+
    theme_bw() +
    theme(plot.title = element_text(color="#006c86",face="bold"),
          axis.text.x=element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  
  if(cst_scale){ fig <- fig + ylim(0,max(data$value))}
  
  list_of_fig[[1]] <- fig
  
  # subsequent panels: other metier
  data <- data %>% filter(!metier_isis %in% main_metier$metier_isis)
  for(i in 2:n_fig){
    
    m_new <- m_list$metier_isis[(1+12*(i-2)):(12+12*(i-2))]
    data_new <- data %>% filter(metier_isis %in% m_new) 
    
    if(i==2){
      met_order2 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
    }else if(i == 3){
      met_order3 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
    }else if(i == 4){
      met_order4 <- data_new %>% 
        filter(data_type=="obs") %>% 
        group_by(metier_isis) %>%
        summarise(value=sum(value)) %>%
        ungroup() %>%
        arrange(-value) %>%
        select(metier_isis) %>%
        distinct()
    }
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8)+
      geom_point(size=3)+
      scale_x_continuous(breaks = unique(data$year))+
      scale_color_manual(values = c("darkgrey", "grey"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Landings profile by metier -", species) )+
      xlab(label = "Year")+
      ylab(label="Landings (prop.)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45, hjust = 1),
            panel.background = element_rect(color = "white",
                                            colour = "white",
                                            linewidth = 0.5, linetype = "solid"),
            legend.title = element_blank())
    
    if(cst_scale){ fig <- fig + ylim(0,1)}
    
    if(i == 2){
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order2$metier_isis), scales = "free") 
    }else if(i==3){
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order3$metier_isis), scales = "free") 
    }else if(i == 4){
      fig <- fig +facet_wrap(~ factor(metier_isis,level=met_order4$metier_isis), scales = "free") 
    }else{
      fig <- fig +facet_wrap(~ metier_isis, scales = "free") 
    }
    
    
    list_of_fig[[i]] <- fig
    
  }
  
  return(list_of_fig)
}

plot_land_byseason <- function(data,species){
  
  data <- data %>% mutate(year=case_when(str_detect(Season,'q.1') ~ year,
                                         str_detect(Season,'q.2') ~ year + 0.25,
                                         str_detect(Season,'q.3') ~ year + 0.5,
                                         str_detect(Season,'q.4') ~ year + 0.75))
    
  
  fig <- ggplot(data, aes(x = year, y = value, color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("Landings by season -", species) )+
    xlab(label = "Year")+
    ylab(label="Landings (prop.)")+
    theme_classic()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20))
  
  
  return(fig)
}

plot_land_byspecies_byfleet_byseason <- function(data,this_fleet){
  
  this_data <- data %>% mutate(year=case_when(Season == 1 ~ year,
                                              Season == 2  ~ year + 0.25,
                                              Season == 3  ~ year + 0.5,
                                              Season == 4  ~ year + 0.75))
  
  
  fig <- ggplot(this_data, aes(x = year, 
                               y = value, 
                               color=data_type))+
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(breaks = unique(data$year))+
    facet_wrap(~ species, scales = "free") +
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = unique(this_fleet))+
    xlab(label = "Year")+
    ylab(label="Landings (kg)")+
    theme_bw()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20))
  
  return(fig)
}

plot_land_byspecies_byfleet_byyear <- function(data,this_fleet){
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type))+
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(breaks = unique(data$year))+
    facet_wrap(~ species, scales = "free") +
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = unique(this_fleet))+
    xlab(label = "Year")+
    ylab(label="Landings (kg)")+
    theme_bw()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20))
  
  return(fig)
}

plot_mean_access <- function(species,access_path){
  # access_path: the path where the csv files of accessibility are stored
  
  Colors <- c(brewer.pal(5, "Blues")[2],
              brewer.pal(5, "Blues")[3],
              brewer.pal(5, "Blues")[4],
              brewer.pal(5, "Blues")[5])
  
  file <- paste0(access_path,"/",species,"/MeanAcc_",species,".csv")
  
  if(species == "Nephrops_norvegicus"){
    
    data <- fread(file) %>% 
      pivot_longer(jan_jan:nov_dec) %>%
      mutate(season = case_when(grepl('jan',name)|grepl('feb',name)|grepl('mar',name) ~ 'Q1',
                                grepl('apr',name)|grepl('may',name)|grepl('jun',name) ~ 'Q2',
                                grepl('jul',name)|grepl('aug',name)|grepl('sep',name) ~ 'Q3',
                                grepl('oct',name)|grepl('nov',name)|grepl('dec',name) ~ 'Q4'),
             group = as.numeric(str_replace(q_name,"q",""))) %>%
      select(group,season,value) %>%
      distinct() %>%
      mutate(sex=if_else(group<=34,"Male","Female"))
    
    fig <- ggplot(data,aes(x = group,y= value,color=season))+
      geom_point(size=3)+
      geom_line(aes(x = group,y= value,color=season,group=season),
                linetype='dotted',
                linewidth=1)+
      facet_wrap(~factor(sex,levels = c("Male","Female")),nrow = 2)+
      scale_color_manual(values=Colors)+
      labs(title = species)+
      ylab(label='Mean accessibility')+
      theme_classic()+
      theme(title=element_text(size=18),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=22),
            strip.text.x = element_text(size = 18),
            legend.text = element_text(size=18))
    
    
  }else{
    
    data <- fread(file) %>% 
      pivot_longer(january:december) %>%
      mutate(season = case_when(grepl('jan',name)|grepl('feb',name)|grepl('mar',name) ~ 'Q1',
                                grepl('apr',name)|grepl('may',name)|grepl('jun',name) ~ 'Q2',
                                grepl('jul',name)|grepl('aug',name)|grepl('sep',name) ~ 'Q3',
                                grepl('oct',name)|grepl('nov',name)|grepl('dec',name) ~ 'Q4'),
             group = as.numeric(str_replace(q_name,"q",""))) %>%
      select(group,season,value) %>%
      distinct() 
    
    fig <- ggplot(data,aes(x = group,y= value,color=season))+
      geom_point(size=3)+
      geom_line(aes(x = group,y= value,color=season,group=season),
                linetype='dotted',
                linewidth=1)+
      scale_color_manual(values=Colors)+
      labs(title = species)+
      ylab(label='Mean accessibility')+
      theme_classic()+
      theme(title=element_text(size=18),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=22),
            legend.text = element_text(size=18))
    
  }
  
  return(fig)
  
}

plot_metier_frequency <-  function(data,thresh,species){
  
  x_order <- data  %>% arrange(cumfreq) %>% select(metier_isis) %>% as.vector()
  
  fig <- ggplot(data, aes(x = factor(metier_isis, level=x_order$metier_isis), y = cumfreq)) +
    geom_col()+
    labs(title = paste("Cumulated frequency of landings by metier (all years)  -", species) )+
    xlab(label = "Metier")+
    ylab(label="Frequency")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))+
    geom_hline(yintercept=thresh, linetype="dashed", 
               color = "orange", linewidth=1)
  
  
  return(fig)
  
}

plot_N_agegroup <- function(data,species){
  
  g_list <- data %>% select(age) %>% arrange(age) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%12)
  list_of_fig <- list()
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$age[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(age %in% g_new) 
    
    
    fig <- ggplot(data_new, aes(x = year, y = value, color= data_type)) +
      geom_line(linewidth=0.8)+
      geom_point(size=3)+
      scale_x_continuous(n.breaks = length(unique(data$year)))+
      facet_wrap(~age, scales = "free") +
      scale_color_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Abundance by age group -", species) )+
      xlab(label = "Year")+
      ylab(label="Abundance (ind.)")+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45, hjust = 1),
            legend.title = element_blank())
  
    
    list_of_fig[[i]] <- fig
    
    
  }
  
  
  
  return(list_of_fig)
}

plot_N_agegroup_merluccius <- function(data,species){
  
  facet_order <- c("0","1","2","3","4","5","6","7","8","9-10","11-13","14+")
  
  g_list <- data %>% select(age) %>% arrange(age) %>% distinct() 
  n_fig <- 1+ ((nrow(g_list)-1)%/%12)
  list_of_fig <- list()
  
  for(i in seq(n_fig)){
    
    g_new <- g_list$age[(1+12*(i-1)):(12+12*(i-1))]
    data_new <- data %>% 
      filter(age %in% g_new) 
    
    
    fig <- ggplot(data_new, aes(x = as.character(year), y = value, fill= data_type)) +
      stat_summary(fun = "sum", geom = "bar", position = "dodge") +
      facet_wrap(~ factor(age,level=facet_order), scales = "free") +
      scale_fill_manual(values = c("#006c86", "#4ebaaf"),
                        labels = c("ref", "sim")) +
      labs(title = paste("Abundance by age group -", species) )+
      xlab(label = "Year")+
      ylab(label="Abundance (ind.)")+
      theme(axis.text.x=element_text(angle = 45, hjust = 1))
    
    list_of_fig[[i]] <- fig
    
    
  }
  
  
  
  return(list_of_fig)
}

plot_N_total <- function(data,species){
  
  fig <- ggplot(data, aes(x = year, y = value, color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = paste("Total abundance by year -", species) )+
    xlab(label = "Year")+
    ylab(label="Abundance (ind.)")+
    theme_classic()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20))
  
  return(fig)
}

plot_radar <- function(data,species,metrics,years){
  # this uses ggradar : https://r-charts.com/ranking/ggradar/
  
  # data preparation
  spp <- species
  
  spp_short <- case_when(species == "Lepidorhombus_whiffiagonis" ~ "L. whiffiagonis",
                         species == "Solea_solea" ~ "S. solea",
                         species == "Lophius_piscatorius" ~ "L. piscatorius",
                         species == "Nephrops_norvegicus" ~ "N. norvegicus",
                         species == "Merluccius_merluccius" ~ "M. merluccius",
                         species == "Raja_clavata" ~ "R. clavata",
                         species == "Leucoraja_naevus" ~ "L. naevus")
  
  Q <- data %>% 
    filter(species == spp) %>%
    select(qty) %>%
    distinct()
  
  Data <- data %>% 
    filter(species == spp,
           year %in% years) %>%
    mutate(qty = case_when(qty == "total_abundance" ~ "Ntot",
                           qty == "abundance_group" ~ "Ngp",
                           qty == "catch_group" ~ "Cgp",
                           qty == "catch_total" ~ "Ctot",
                           qty == "F_group" ~ "Fgp",
                           qty == "land_country" ~ "Lctry",
                           qty == "land_metier" ~ "Lmet",
                           qty == "land_metier_season" ~ "Lmet-seas",
                           qty == "land_season" ~ "Lseas",
                           qty == "ssb" ~ "SSB",
                           qty == "total_land" ~ "Ltot",
                           qty == "catchN_group" ~ "Cn-gp",
                           qty == "catchN_total" ~ "Cn-tot")) %>%
    mutate(score = case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ 1 - eval(parse(text = metrics)),
                             metrics %in% c("r","MEF") ~ eval(parse(text = metrics)))) %>%
    mutate(score = if_else(score>0,score,0)) %>%
    select(year,qty,score) %>%
    drop_na() %>%
    pivot_wider(names_from = qty, values_from = score) %>%
    drop_na()
  
  
  # plotting
  lcols <- brewer.pal(n = max(3,length(years)), name = "YlGnBu")
  myTitle <- case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ paste(spp_short,": 1 -",metrics),
                       metrics %in% c("r","MEF") ~ paste(spp_short,": ",metrics))
  
  fig <- ggradar(Data, 
                 values.radar = c(0, 0.5, 1),
                 background.circle.colour = 'white',
                 group.colours = lcols,
                 legend.position = "right")+
    labs(title = myTitle)
  
  return(fig)
}

plot_radar_period_mod <- function(data,species,metrics){
  # this uses ggradar : https://r-charts.com/ranking/ggradar/
  
  # data preparation
  spp <- species
  
  spp_short <- case_when(species == "Lepidorhombus_whiffiagonis" ~ "L. whiffiagonis",
                         species == "Solea_solea" ~ "S. solea",
                         species == "Lophius_piscatorius" ~ "L. piscatorius",
                         species == "Nephrops_norvegicus" ~ "N. norvegicus",
                         species == "Merluccius_merluccius" ~ "M. merluccius",
                         species == "Raja_clavata" ~ "R. clavata",
                         species == "Leucoraja_naevus" ~ "L. naevus")
  
  Q <- data %>% 
    filter(species == spp) %>%
    select(qty) %>%
    distinct()
  
  Data <- data %>% 
    filter(species == spp) %>%
    mutate(qty = case_when(qty == "total_abundance" ~ "Ntot",
                           qty == "abundance_group" ~ "Ngp",
                           qty == "catch_group" ~ "Cgp",
                           qty == "catch_total" ~ "Ctot",
                           qty == "F_group" ~ "Fgp",
                           qty == "land_country" ~ "Lctry",
                           qty == "land_metier" ~ "Lmet",
                           qty == "land_metier_season" ~ "Lmet-seas",
                           qty == "land_season" ~ "Lseas",
                           qty == "ssb" ~ "SSB",
                           qty == "total_land" ~ "Ltot",
                           qty == "catchN_group" ~ "Cn-gp",
                           qty == "catchN_total" ~ "Cn-tot")) %>%
    mutate(score = case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ 1 - eval(parse(text = metrics)),
                             metrics %in% c("r","MEF") ~ eval(parse(text = metrics)))) %>%
    mutate(score = if_else(score>0,score,0)) %>%
    select(period,mod,qty,score) %>%
    drop_na() %>%
    pivot_wider(names_from = qty, values_from = score) %>%
    drop_na()
  
  Data1 <- Data %>% 
    filter(period==1) %>%
    select(-period)
  
  Data2 <- Data %>% 
    filter(period==2) %>%
    select(-period)
  
  # plotting
  lcols <- brewer.pal(n = max(3,length(mod)), name = "YlGnBu")
  myTitle <- case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ paste(spp_short,": 1 -",metrics),
                       metrics %in% c("r","MEF") ~ paste(spp_short,": ",metrics))
  
  fig1 <- ggradar(Data1, 
                  values.radar = c(0, 0.5, 1),
                  group.colours = lcols,
                  background.circle.colour = 'white',
                  plot.title = paste(spp_short,"-",metrics,"- period 1"),
                  legend.position = "right",
                  base.size = 7)
  
  fig2 <- ggradar(Data2, 
                  values.radar = c(0, 0.5, 1),
                  base.size = 7,
                  group.colours = lcols,
                  background.circle.colour = 'white',
                  legend.position = "right",
                  plot.title = paste(spp_short,"-",metrics,"- period 2"))
  
  fig <- ggarrange(fig1, fig2,
                   labels = c(paste(spp_short,"2015-2018"), "2019-2022"),
                   ncol = 2)
  
  return(list(fig1,fig2))
}

plot_radar_period_SR <- function(data,species,metrics,mod){
  # this uses ggradar : https://r-charts.com/ranking/ggradar/
  
  # data preparation
  spp <- species
  
  spp_short <- case_when(species == "Lepidorhombus_whiffiagonis" ~ "L. whiffiagonis",
                         species == "Solea_solea" ~ "S. solea",
                         species == "Lophius_piscatorius" ~ "L. piscatorius",
                         species == "Nephrops_norvegicus" ~ "N. norvegicus",
                         species == "Merluccius_merluccius" ~ "M. merluccius",
                         species == "Raja_clavata" ~ "R. clavata",
                         species == "Leucoraja_naevus" ~ "L. naevus")
  
  Q <- data %>% 
    filter(species == spp) %>%
    select(qty) %>%
    distinct()
  
  Data <- data %>% 
    filter(species == spp) %>%
    mutate(qty = case_when(qty == "total_abundance" ~ "Ntot",
                           qty == "abundance_group" ~ "Ngp",
                           qty == "catch_group" ~ "Cgp",
                           qty == "catch_total" ~ "Ctot",
                           qty == "F_group" ~ "Fgp",
                           qty == "land_country" ~ "Lctry",
                           qty == "land_metier" ~ "Lmet",
                           qty == "land_metier_season" ~ "Lmet-seas",
                           qty == "land_season" ~ "Lseas",
                           qty == "ssb" ~ "SSB",
                           qty == "total_land" ~ "Ltot",
                           qty == "catchN_group" ~ "Cn-gp",
                           qty == "catchN_total" ~ "Cn-tot")) %>%
    mutate(score = case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ 1 - eval(parse(text = metrics)),
                             metrics %in% c("r","MEF") ~ eval(parse(text = metrics)))) %>%
    mutate(score = if_else(score>0,score,0)) %>%
    select(period,mod,qty,score) %>%
    drop_na() %>%
    pivot_wider(names_from = qty, values_from = score) %>%
    drop_na()
  
  Data1 <- Data %>% 
    filter(period==1) %>%
    select(-period)
  
  Data2 <- Data %>% 
    filter(period==2) %>%
    select(-period)
  
  # plotting
  lcols <- brewer.pal(n = max(3,length(mod)), name = "YlGnBu")
  myTitle <- case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ paste(spp_short,": 1 -",metrics),
                       metrics %in% c("r","MEF") ~ paste(spp_short,": ",metrics))
  
  fig1 <- ggradar(Data1, 
                  values.radar = c(0, 0.5, 1),
                  group.colours = lcols,
                  background.circle.colour = 'white',
                  plot.title = paste(spp_short,"-",metrics,"- period 1"),
                  legend.position = "right",
                  base.size = 7)
  
  fig2 <- ggradar(Data2, 
                  values.radar = c(0, 0.5, 1),
                  base.size = 7,
                  group.colours = lcols,
                  background.circle.colour = 'white',
                  legend.position = "right",
                  plot.title = paste(spp_short,"-",metrics,"- period 2"))
  
  fig <- ggarrange(fig1, fig2,
                   labels = c(paste(spp_short,"2015-2018"), "2019-2022"),
                   ncol = 2)
  
  return(list(fig1,fig2))
}

plot_radar_SRtype <- function(data,species,metrics,period){
  # this uses ggradar : https://r-charts.com/ranking/ggradar/
  
  # data preparation
  spp <- species
  p <- period
  
  spp_short <- case_when(species == "Lepidorhombus_whiffiagonis" ~ "L. whiffiagonis",
                         species == "Solea_solea" ~ "S. solea",
                         species == "Lophius_piscatorius" ~ "L. piscatorius",
                         species == "Nephrops_norvegicus" ~ "N. norvegicus",
                         species == "Merluccius_merluccius" ~ "M. merluccius",
                         species == "Raja_clavata" ~ "R. clavata",
                         species == "Leucoraja_naevus" ~ "L. naevus")
  
  Q <- data %>% 
    filter(species == spp) %>%
    select(qty) %>%
    distinct()
  
  Data <- data %>% 
    filter(species == spp,
           period == p) %>%
    mutate(qty = case_when(qty == "total_abundance" ~ "Ntot",
                           qty == "abundance_group" ~ "Ngp",
                           qty == "catch_group" ~ "Cgp",
                           qty == "catch_total" ~ "Ctot",
                           qty == "F_group" ~ "Fgp",
                           qty == "land_country" ~ "Lctry",
                           qty == "land_metier" ~ "Lmet",
                           qty == "land_metier_season" ~ "Lmet-seas",
                           qty == "land_season" ~ "Lseas",
                           qty == "ssb" ~ "SSB",
                           qty == "total_land" ~ "Ltot",
                           qty == "catchN_group" ~ "Cn-gp",
                           qty == "catchN_total" ~ "Cn-tot")) %>%
    mutate(score = case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ 1 - eval(parse(text = metrics)),
                             metrics %in% c("r","MEF") ~ eval(parse(text = metrics)))) %>%
    mutate(score = if_else(score>0,score,0)) %>%
    select(SR_type,qty,score) %>%
    drop_na() %>%
    pivot_wider(names_from = qty, values_from = score) %>%
    drop_na()
  
  
  # plotting
  lcols <- brewer.pal(n = max(3,n_distinct(SR_type)), name = "YlGnBu")
  myTitle <- case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ paste(spp_short,"- p.",period,": 1 -",metrics),
                       metrics %in% c("r","MEF") ~ paste(spp_short,"- p.",period,": ",metrics))
  
  fig <- ggradar(Data, 
                 values.radar = c(0, 0.5, 1),
                 background.circle.colour = 'white',
                 group.colours = lcols,
                 legend.position = "right")+
    labs(title = myTitle)
  
  return(fig)
}

plot_radar_SRtype_years <- function(data,species,metrics,SR_type,period){
  # this uses ggradar : https://r-charts.com/ranking/ggradar/
  
  # data preparation
  spp <- species
  p <- period
  sr <- SR_type
  
  Q <- data %>% 
    filter(species == spp) %>%
    select(qty) %>%
    distinct()
  
  Data <- data %>% 
    filter(species == spp,
           period == p,
           SR_type == sr) %>%
    mutate(qty = case_when(qty == "total_abundance" ~ "Ntot",
                           qty == "abundance_group" ~ "Ngp",
                           qty == "catch_group" ~ "Cgp",
                           qty == "catch_total" ~ "Ctot",
                           qty == "F_group" ~ "Fgp",
                           qty == "land_country" ~ "Lctry",
                           qty == "land_metier" ~ "Lmet",
                           qty == "land_metier_season" ~ "Lmet-seas",
                           qty == "land_season" ~ "Lseas",
                           qty == "ssb" ~ "SSB",
                           qty == "total_land" ~ "Ltot",
                           qty == "catchN_group" ~ "Cn-gp",
                           qty == "catchN_total" ~ "Cn-tot")) %>%
    mutate(score = case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ 1 - eval(parse(text = metrics)),
                             metrics %in% c("r","MEF") ~ eval(parse(text = metrics)))) %>%
    mutate(score = if_else(score>0,score,0)) %>%
    select(year,qty,score) %>%
    drop_na() %>%
    pivot_wider(names_from = qty, values_from = score) %>%
    drop_na()
  
  n_years <- Data %>% select(year) %>% n_distinct()
  
  # plotting
  lcols <- brewer.pal(n = max(3,n_years), name = "YlGnBu")
  myTitle <- case_when(metrics %in% c("RwMSE","RwMSE","AAE") ~ paste(SR_type,"- p.",period,": 1 -",metrics),
                       metrics %in% c("r","MEF") ~ paste(SR_type,"- p.",period,": ",metrics))
  
  fig <- ggradar(Data, 
                 values.radar = c(0, 0.5, 1),
                 background.circle.colour = 'white',
                 group.colours = lcols,
                 legend.position = "right")+
    labs(title = myTitle)
  
  return(fig)
}

plot_radar_synthesis_fleet <- function(data,w_ratio,low_var,high_var,metrics=c("AE","RwMSE","r","MEF"),groups=c('FR <12','FR >12','Foreign')){
  # high_var: observed variables -> high weight
  # low_var: model predicted variable -> low weight
  # w_ratio is: high_var weights/low_var weights
  
  mod <- unique(data$mod)
  
  # set variable weightings
  low_wt <- 1/(w_ratio + 1)
  high_wt <- 1-low_wt
  
  # prepare data
  Data <- data %>%
    select(fleet_isis,qty,eval(metrics)) %>%
    mutate(group = case_when(grepl("ES",fleet_isis)|
                               grepl("BE",fleet_isis)|
                               grepl("UK",fleet_isis) ~ 'Foreign',
                             grepl("(north)",fleet_isis)|
                               grepl("(south)",fleet_isis) ~ 'FR >12',
                             grepl("10-12",fleet_isis)|
                               grepl("0-10",fleet_isis) ~ 'FR <12')) %>%
    pivot_longer(eval(metrics),names_to = 'metrics') %>%
    na.omit() %>%
    mutate(value = case_when(metrics =='AE' ~ if_else(abs(value)<1,1-abs(value),0),
                             metrics %in% c('MEF','r') ~ if_else(value <0,0,value),
                             metrics %in% c('RwMSE','AAE','AE') ~ if_else(value>1,0,1-value))) %>% # remove values too great or too low for facility
    mutate(wt = if_else(qty %in% low_var,low_wt,high_wt),
           var_type = if_else(qty %in% low_var,'II','I')) %>%
    group_by(fleet_isis,group,var_type,metrics,wt) %>%
    summarise(value=mean(value)) %>%
    ungroup() %>%
    mutate(value=wt*value) %>%
    group_by(fleet_isis,group,metrics) %>%
    summarise(value=sum(value)) %>%
    ungroup()
  
  plot_list <- list()
  for(i in seq(length(groups))){
    
    this_group <- groups[i]
    this_data <- Data %>%
      filter(group==this_group) %>%
      pivot_wider(names_from = metrics, values_from = value) %>%
      select(fleet_isis,group,eval(metrics)) %>%
      na.omit() %>%
      rowwise() %>%
      mutate(S=sum(c_across(eval(metrics)[1]:eval(metrics)[length(metrics)]))) %>%
      arrange(S) %>%
      ungroup() %>%
      select(-c(S,group))
    this_order <- this_data$fleet_isis
    
    names(this_data) <- paste0(c(rep("",1),rep("I(",length(metrics))),
                          names(this_data),
                          c(rep("",1),rep(")",length(metrics))))
    
    # plotting
    lcols <- brewer.pal(n = min(max(3,length(this_data$fleet_isis)),11), name = "Spectral")
    if(length(lcols) < length(this_data$fleet_isis)){
      for(i in seq(length(this_data$fleet_isis)-length(lcols))){
        lcols <- c(lcols,lcols[i])
      }
    }
    
    this_fig <- ggradar(this_data,
                        values.radar = c(0, 0.5, 1),
                        background.circle.colour = 'white',
                        group.line.width = 1.2,
                        group.point.size = 5,
                        axis.label.size = 7,
                        legend.text.size =24,
                        legend.position = "right",
                        plot.title = TeX(paste('$\\omega_I / \\omega_{II}=$',w_ratio)))+
      scale_color_manual(values = lcols,
                         breaks=this_order)
    
    plot_list[[i]] <- this_fig
    
  }
  
  this_data <- Data %>%
    group_by(group,metrics) %>%
    summarise(value=mean(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = metrics, values_from = value) %>%
    select(group,eval(metrics)) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(S=sum(c_across(eval(metrics)[1]:eval(metrics)[length(metrics)]))) %>%
    arrange(S) %>%
    ungroup() %>%
    select(-S) 
  this_order <- this_data$group
  
  names(this_data) <- paste0(c(rep("",1),rep("I(",length(metrics))),
                             names(this_data),
                             c(rep("",1),rep(")",length(metrics))))
  
  
  
  # plotting
  lcols <- brewer.pal(n = max(3,length(this_data$group)), name = "RdYlBu")
  
  this_fig <- ggradar(this_data,
                      values.radar = c(0, 0.5, 1),
                      background.circle.colour = 'white',
                      group.line.width = 1.2,
                      group.point.size = 5,
                      axis.label.size = 7,
                      legend.text.size =24,
                      legend.position = "right",
                      plot.title = TeX(paste('$\\frac{\\omega_I}{\\omega_{II}}=$',w_ratio)))+
    scale_color_manual(values = lcols,
                       breaks=this_order)
  
  plot_list[[length(groups)+1]] <- this_fig
  
  return(plot_list)
}

plot_radar_synthesis_spp <- function(data,w_ratio,low_var,high_var,metrics=c("AE","RwMSE","r","MEF")){
  # high_var: observed variables -> high weight
  # low_var: model predicted variable -> low weight
  # w_ratio is: high_var weights/low_var weights

  mod <- unique(data$mod)

  # set variable weightings
  low_wt <- 1/(w_ratio + 1)
  high_wt <- 1-low_wt

  # prepare data
  Data <- data %>%
    select(species,qty,eval(metrics)) %>%
    mutate(species=case_when(species == "Lepidorhombus_whiffiagonis" ~ "MEG",
                             species == "Solea_solea" ~ "SOL",
                             species == "Lophius_piscatorius" ~ "MON",
                             species == "Nephrops_norvegicus" ~ "NEP",
                             species == "Merluccius_merluccius" ~ "HKE",
                             species == "Raja_clavata" ~ "RJC",
                             species == "Leucoraja_naevus" ~ "RJN"))%>%
    pivot_longer(eval(metrics),names_to = 'metrics') %>%
    na.omit() %>%
    mutate(value = case_when(metrics =='AE' ~ if_else(abs(value)<1,1-abs(value),0),
                             metrics %in% c('MEF','r') ~ if_else(value <0,0,value),
                             metrics %in% c('RwMSE','AAE','AE') ~ if_else(value>1,0,1-value))) %>% # remove values too great or too low for facility
    mutate(wt = if_else(qty %in% low_var,low_wt,high_wt),
           var_type = if_else(qty %in% low_var,'II','I')) %>%
    group_by(species,var_type,metrics,wt) %>%
    summarise(value=mean(value)) %>%
    ungroup() %>%
    mutate(value=wt*value) %>%
    group_by(species,metrics) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = metrics, values_from = value) %>%
    rowwise() %>%
    select(species,eval(metrics)) %>%
    mutate(S=sum(c_across(eval(metrics)[1]:eval(metrics)[length(metrics)]))) %>%
    arrange(S) %>%
    ungroup() %>%
    select(-S)
  
  names(Data) <- paste0(c("",rep("I(",length(metrics))),
                        names(Data),
                        c("",rep(")",length(metrics))))

  
  # plotting
  lcols <- brewer.pal(n = max(3,length(Data$species)), 
                          name = "RdYlBu")
  my_order <- Data$species
  
  fig <- ggradar(Data,
                 values.radar = c(0, 0.5, 1),
                 background.circle.colour = 'white',
                 group.line.width = 1.2,
                 group.point.size = 5,
                 axis.label.size = 5,
                 legend.position = "right",
                 plot.title = TeX(paste('$\\frac{\\omega_I}{\\omega_{II}}=$',w_ratio)))+
    scale_color_manual(values = lcols,
                       breaks=my_order)
    

  return(fig)
}

plot_recruit <- function(data,species){
  
  fig <- ggplot(data, aes(x = year, y = rec_n,fill=data_type)) +
    stat_summary(fun = "sum", geom = "bar", width=0.2) +
    scale_fill_manual(values = c("#006c86","#4ebaaf")) +
    labs(title = paste("Recruitment", species) )+
    xlab(label = "Year")+
    ylab(label="Recruitment (ind.)")+
    labs(fill="Type")+
    theme_bw()
  
  return(fig)
  
}

plot_SSB <- function(data,species){
  
  fig <- ggplot(data, aes(x = year, y = value, color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("SSB by year -", species) )+
    xlab(label = "Year")+
    ylab(label="SSB (kg)")+
    theme_bw()+
    theme(legend.title = element_blank())
  
  return(fig)
}

plot_SSB_allSpp <- function(data){
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type)) +
    facet_wrap(~species,scales = "free_y")+
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    xlab(label = "Year")+
    ylab(label="SSB (kg)")+
    theme_bw()
  
  return(fig)
}

plot_SSB_line <- function(data,species){
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type)) +
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                      labels = c("ref", "sim")) +
    labs(title = paste("SSB by year -", species),color="Data type" )+
    xlab(label = "Year")+
    ylab(label="SSB (kg)")+
    theme_bw()
  
  return(fig)
}

plot_tseries_y <- function(data,species,metrics){
  
  fig <- ggplot(data, aes(x = year, 
                          y = value, 
                          color=data_type))+
    geom_line(linewidth=0.8)+
    geom_point(size=3)+
    scale_x_continuous(n.breaks = length(unique(data$year)))+
    scale_color_manual(values = c("#006c86", "#4ebaaf"),
                       labels = c("ref", "sim")) +
    labs(title = species)+
    xlab(label = "Year")+
    ylab(label=metrics)+
    theme_classic()+
    theme(title=element_text(size=12),
          axis.text.x = element_text(size=20,
                                     angle = 45, 
                                     vjust = 0.5),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.title = element_blank(),
          legend.text = element_text(size=20))
  
  return(fig)
}

save_last_heatmaps <- function(name,path,by_fleet){
  
  full_path <- if_else(by_fleet,
                       paste0(path,"/by_fleet/"),
                       paste0(path,"/by_species/"))
  ggsave(filename = paste0(full_path,name,".png"),
         height = if_else(by_fleet,2415,1470),
         units = "px")
  
}

save_last <- function(name,path,h){
  ggsave(filename = paste0(path,name,".png"),
         height = h,
         units = "px")
}


### exporting corrections----
export_new_q_csv <- function(q_season_corrections,previous_q_path,exported_q_path){
  
  Spp_list <- q_season_corrections %>% 
    filter(Species != "Leucoraja naevus") %>% # this species' accessibility is not seasonal
    select(Species) %>%
    distinct()
  
  for(i in seq(nrow(Spp_list))){
    
    thisSpp <- Spp_list$Species[i]
    thisSpp_code <- str_replace(thisSpp," ","_")
    
    Q1 <- q_season_corrections %>% filter(Species == thisSpp,Season==1) %>% select(ratio) %>% as.numeric()
    Q2 <- q_season_corrections %>% filter(Species == thisSpp,Season==2) %>% select(ratio) %>% as.numeric()
    Q3 <- q_season_corrections %>% filter(Species == thisSpp,Season==3) %>% select(ratio) %>% as.numeric()
    Q4 <- q_season_corrections %>% filter(Species == thisSpp,Season==4) %>% select(ratio) %>% as.numeric()
    
    matrix_list <- list.files(paste0(previous_q_path,"/",thisSpp_code),pattern="\\.csv")
    for(j in seq(matrix_list)){
      
        data <- read.table(file = paste0(previous_q_path,"/",thisSpp_code,"/",matrix_list[j]),
                           sep=";",
                           header = T)
        
        if(thisSpp == "Nephrops norvegicus"){
          
          # the seasonnality for this species is different
          # NB: previously, all months were equivalent so before seasonal transformation, no need to respect names
          if("january" %in% names(data)){
            data <- data %>%
              rename(jan_jan = january,
                     feb_mar = february,
                     apr_apr = march,
                     may_jun = april,
                     jul_aug = may,
                     sep_sep = june,
                     oct_oct = july,
                     nov_dec = august) %>%
              select(-c(september,october,november,december))
          }
          
           data <- data %>%
             mutate(jan_jan = jan_jan * Q1,
                    feb_mar = feb_mar * Q1,
                    apr_apr = apr_apr * Q2,
                    may_jun = may_jun * Q2,
                    jul_aug = jul_aug * Q3,
                    sep_sep = sep_sep * Q3,
                    oct_oct = oct_oct * Q4,
                    nov_dec = nov_dec * Q4) %>%
            write.table(file = paste0(exported_q_path,"/",thisSpp_code,"/",matrix_list[j]),
                        sep = ";",
                        row.names=F)
          
        }else{
          
          data <- data %>% 
            mutate(january = january * Q1,
                   february = february * Q1,
                   march = march * Q1,
                   april = april * Q2,
                   may = may * Q2,
                   june = june * Q2,
                   july = july * Q3,
                   august = august * Q3,
                   september = september * Q3,
                   october = october * Q4,
                   november = november * Q4,
                   december = december * Q4) %>%
            write.table(file = paste0(exported_q_path,"/",thisSpp_code,"/",matrix_list[j]),
                        sep = ";",
                        row.names=F)
          
        }
        
     }
    
  }
  
  print("New accessibility matrices (.csv) exported !")
  
}  

export_newTF <- function(TFcorrections,previousTF_path,exportedTF_path){

  Corr <- TFcorrections %>% mutate(ratio = if_else(is.na(ratio),1,ratio)) %>%
    mutate(Species = str_replace(Species," ","_"))

  Spp_list <- unique(Corr$Species)

  for(i in seq(length(Spp_list))){
    import_path <- paste0(previousTF_path,"/",Spp_list[i])
    export_path  <- paste0(exportedTF_path,"/",Spp_list[i])
    files <- data.frame(file = list.files(path = import_path,pattern="\\.txt")) %>% 
      mutate(metier = word(file,1,sep="_Z")) %>%
      mutate(metier = word(metier,1,sep="-"))
      
    for(j in seq(nrow(files))){
      # get the good correction
      this_corr <- Corr %>% filter(Species == Spp_list[i] & metier_isis == files$metier[j])
      # read the file
      lines <- readLines(paste0(import_path,"/",files$file[j])) %>% strsplit(split=';')
      lines <- data.table(old_lines = lines[[1]])

      # correct the appropriate line
      TF_line <- lines[nrow(lines)-1]$old_lines
      TF_old <- as.numeric(unlist(regmatches(TF_line, gregexpr("-?[0-9]+\\.?[0-9]*", TF_line))))
      
      if(nrow(this_corr)==1){
        TF_new <- TF_old * this_corr$ratio
      }else{
        TF_new <- TF_old
      }
      
      bracket <- grepl("}",TF_line)
      start_line <- if_else(bracket,"} return","return")
      lines <- lines %>% 
        mutate(new_lines = if_else(old_lines==TF_line,
                                   if_else(str_detect(old_lines,"sel"),
                                           paste(start_line, TF_new,"* sel"),
                                           paste(start_line, TF_new)),
                                   old_lines)) %>%
        mutate(new_lines = if_else(str_detect(new_lines,pattern="metier"),
                                    new_lines,
                                    paste(new_lines,";")))
      
      # write in a new file
      if(!dir.exists(export_path)){dir.create(export_path)}
      writeLines(lines$new_lines,paste0(export_path,"/",files$file[j]))

    }

  }

}

parse_semantic_matrix <- function(filename, val_col = "value", encoding = "UTF-8") {
  raw_text <- readLines(filename, encoding = encoding) %>%
    extract(. != "") # empty lines
  
  # détecter quelles lignes avec données numériques
  
  nb_dims <- raw_text[1] %>% # 1st line
    substr(., 2, nchar(.) - 1) %>% # enlever les crochets
    strsplit(., split = ",", fixed = TRUE) %>%
    extract2(1) %>%
    length()
  idx_lines_with_data <- rep("\\d+;", times = nb_dims) %>%
    paste0(collapse = "") %>%
    paste0("^", .) %>% # exemple: "^\\d+;\\d+;" si nb_dims = 2
    grepl(., raw_text)
  
  data_text <- raw_text[idx_lines_with_data]
  header_lines <- raw_text[!idx_lines_with_data][-1] # 1re ligne = shape = sert à rien
  var_cols <- gsub(":.+$", "", header_lines) # don’t take the last column (val_col) yet!
  
  df <- read.table(
    text = paste0(data_text, collapse = "\n"), sep = ";",
    header = FALSE, check.names = FALSE, col.names = c(var_cols, val_col)
  )
  
  categorical_val <- lapply(header_lines, function(x) {
    y <- gsub("^[^:]+:", "", x)
    res <- strsplit(y, split = ",", fixed = TRUE)
    return(unlist(res))
  }) %>% setNames(var_cols)
  
  df[, var_cols] <- imap_dfc(categorical_val, function(val, id) val[1 + df[[id]]])
  # +1 because R index starts from 1
  return(df)
}


