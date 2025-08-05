##############################################################################
## ISIS-MACCO CALIBRATION -- FUNCTIONS FOR LHS OUTPUTS ANALYSIS
## Author : A. Ricouard -- antoine.ricouard@gmail.com
##############################################################################


### Computation of error----
err_abs <- function(obs, sim) {
  res <- (obs - sim)**2
  return(res)
}

err_std <- function(obs, sim) {
  res <- if_else(obs == 0, sim, (obs - sim) / obs)
  return(res ^ 2)
}

### Conversion between length and age for the length structured species
conversion_merluccius <- function(save_path){
  
  # computation of age at length of Meluccius according to Vigier et al. (2022) formula
  # (the equation in the ISIS model)
  data <- data.frame(len_group=0:71) %>% 
    mutate(l_min = case_when(len_group <= 38 ~  1 + len_group,
                             38 < len_group & len_group <= 68 ~ 40 + 2*(len_group-39),
                             68 < len_group ~ 100 + 10*(len_group-69)),
           l_max = case_when(len_group <= 38 ~  1 + (len_group+1),
                             38 < (len_group+1) & (len_group+1) <= 68 ~ 40 + 2*((len_group+1)-39),
                             68 < (len_group+1) ~ 100 + 10*((len_group+1)-69)),
           l_moy = l_min+(l_max-l_min)/2) %>%
    mutate(age_min = floor((-1/0.177319) * log(1-(l_min/130))),
           age_moy = floor((-1/0.177319) * log(1-(l_moy/130))),
           age_max = floor((-1/0.177319) * log(1-(l_max/130)))) %>%
    mutate(age_group = case_when(age_moy <= 8 ~ as.character(age_moy),
                               age_moy == 9 ~ "9-10",
                               age_moy == 12 ~ "11-13",
                               age_min == 14 ~ "14+")) 
  
  saveRDS(data,file = paste0(save_path,"/Merluccius_length_age.rds"))
  
  
  Out <- data %>% select(len_group,age_group)
  return(Out)
  
}

conversion_nephrops <- function(save_path){
  
  # computation of age at length of Nephrops according to Vigier et al. (2022) model
  # further ref in WKNEP 2016 and Ulmestrand and Eggert (2001)
  
  data <- data.frame(len_group=0:57) %>%
    filter(len_group!= 34) %>%
    mutate(sex = case_when(len_group == 0 ~ "mixed",
                           1 <= len_group & len_group < 34 ~ "male",
                           34 < len_group ~ "female"))%>%
    mutate(l_min = case_when(len_group == 0 ~ 10,
                             sex == "male" ~  10 + 2 * (len_group - 1),
                             sex == "female" ~  10 + 2 * (len_group - 35)),
           l_max = case_when(len_group == 0 ~ 10,
                             sex == "male" ~  10 + 2 * (len_group),
                             sex == "female" ~  10 + 2 * (len_group - 34)),
           l_moy = l_min+(l_max-l_min)/2,
           l_inf = if_else(sex == "male",76,56),
           K = if_else(sex == "male",0.14,0.11)) %>%
    mutate(age_min = floor((-1/K) * log(1-(l_min/l_inf))),
           age_moy = floor((-1/K) * log(1-(l_moy/l_inf))),
           age_max = floor((-1/K) * log(1-(l_max/l_inf)))) %>%
    mutate(age_group = case_when(sex == "mixed" ~ "Mixed 0",
                                 sex == "male" & age_moy <= 13 ~ paste("Male",age_moy),
                                 sex == "male" & age_moy == 15 ~ "Male 14-15",
                                 sex == "male" & age_moy == 17 ~ "Male 16-18",
                                 sex == "male" & age_moy == 19 ~ "Male 19-21",
                                 sex == "male" & age_min == 21 ~ "Male 22+",
                                 sex == "female" & age_moy <= 11 ~ paste("Female",age_moy),
                                 sex == "female" & age_moy == 13 ~ "Female 12-13",
                                 sex == "female" & age_moy == 13 ~ "Female 12-13",
                                 sex == "female" & age_moy == 14 ~ "Female 14-15",
                                 sex == "female" & age_moy == 16 ~ "Female 16-17",
                                 sex == "female" & age_moy == 18 ~ "Female 18-20",
                                 sex == "female" & age_moy == 21 ~ "Female 21-23",
                                 sex == "female" & age_moy == 26 ~ "Female 24-29",
                                 age_min == 30 ~ "Female 30+")) 
  
  saveRDS(data,file = paste0(save_path,"/Nephrops_length_age.rds"))
  
  
  Out <- data %>% select(len_group,age_group)
  return(Out)
  
}


### Reading and formatting data----
raw_to_ObsSim <- function(raw_data,species,year){
  
  n_simul <- length(raw_data)
  
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  res_lhs <- foreach(i = 1:n_simul,  .packages = c("dplyr","tidyr","here")) %dopar% {
    
    source(paste0(here(),"/R_fun/lhs_report_fun.R"))
    
    catch_obs_vs_sim <- raw_data[[i]]$catch_obs_vs_sim %>%
      rename(err_std = err) %>%
      mutate(err_abs = err_abs(obs,sim),
             sim_num = i,
             q = raw_data[[i]]$params[group+1],
             q_name = if_else(group<10,paste0("q0",group), paste0("q",group)),
             species = species,
             year = year) %>%
      pivot_longer(cols = c(err_abs,err_std), 
                   names_to = "err_type",
                   values_to = "err_val")
    
    return(catch_obs_vs_sim = catch_obs_vs_sim)
    
  }
  stopCluster(cl)
  
  ObsSim_data <- data.frame()
  for( i in seq(length(res_lhs))){
    ObsSim_data <- rbind(ObsSim_data,res_lhs[[i]])
  }
  
  return(ObsSim_data)
}

age_aggreg <- function(data,save_conv_path){
  
  year <- unique(data$year)
  species <- unique(data$species)
  if(species == "Merluccius merluccius"){
    corresp <- conversion_merluccius(save_conv_path)
  }else if (species == "Nephrops norvegicus"){
    corresp <- conversion_nephrops(save_conv_path)
  }
  
  Data <- data %>%
    filter(err_type == "err_abs") %>%
    select(-c(err_type,err_val,year,species)) %>%
    rename(len_group = group) %>%
    merge(corresp) %>%
    mutate(`C/q` = if_else(q !=0,sim/q,0)) %>% 
    group_by(sim_num,age_group) %>%
    mutate(sim_sum = sum(sim),
           obs_sum = sum(obs),
           `sum C/q` = sum(`C/q`)) %>%
    ungroup() %>%
    mutate(age_q = if_else(sim_sum != 0,sim_sum/`sum C/q`,0),
           err_val = err_std(obs_sum,sim_sum),
           err_type = "std") %>%
    select(sim_num,q_name,age_group,sim_sum,obs_sum,age_q,err_val,err_type) %>%
    rename(sim = sim_sum, 
           obs = obs_sum, 
           q = age_q) %>%
    mutate(year = year,
           species = species)
  
  return(Data)
}

### Best parameter set----
best_set <- function(ObsSim_data){
  
  species <- unique(ObsSim_data$species)
  if(species %in% c("Merluccius merluccius","Nephrops norvegicus")){
    
    Data <- ObsSim_data %>% 
      group_by(age_group) %>%
      filter(err_val == min(err_val)) %>%
      select(species,year,q_name,q,sim,obs,err_val) %>%
      arrange(q_name) %>% 
      ungroup() %>%
      distinct()
    
  }else{
    
    Data <- ObsSim_data %>% 
      group_by(q_name) %>%
      filter(err_val == min(err_val)) %>%
      select(species,year,q_name,q,sim,obs,err_val) %>%
      arrange(q_name) %>% 
      ungroup() %>%
      distinct()
    
  }
  
  
  return(Data)
  
}

best_set_glob <- function(ObsSim_data){
  # get the parameter set minimising the global standard error
  
  Data <- ObsSim_data %>% 
    group_by(sim_num) %>%
    mutate(err_sum = sum(err_val)) %>%
    ungroup() %>%
    filter(err_sum == min(err_sum)) %>%
    select(age_group,species,year,q_name,sim,obs,err_val)
  
  return(Data)
  
}


### Plotting functions----
plotOF <- function(data,log_scale=F){
  

    
    fig <- ggplot(data) +
      geom_point(mapping = aes(x = q, y = err_val)) +
      ggtitle(paste("Objective functions vs. accessibility value -",data$species, data$year))+
      facet_wrap(~ q_name, scales = "free") +
      theme_bw()
  
  if(log_scale){ 
      fig <- fig + 
        scale_x_log10(labels = label_log()) +
        scale_y_log10(labels = label_log())
    }
  
  return(fig)
  
}

plotOF_merluccius <- function(data,log_scale=F){
  
  data <- data %>% mutate(age_group = paste("Age",age_group))
  facet_order <- data %>% select(age_group) %>% distinct()
  
  fig <- ggplot(data) +
    geom_point(mapping = aes(x = q, y = err_val)) +
    ggtitle(paste("Objective functions vs. accessibility value -",data$species, data$year))+
    facet_wrap(~ factor(age_group,level=facet_order$age_group), scales = "free") +
    theme_bw()
  
  if(log_scale){ 
    fig <- fig + 
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log())
  }
  
  return(fig)
  
}

plotOF_nephrops <- function(data,log_scale=F){
  
  data1 <- data %>% filter(grepl("Male",age_group))
  data2 <- data %>% filter(!grepl("Male",age_group))
  
  facet_order1 <- data1 %>% select(age_group) %>% distinct()
  facet_order2 <- data2 %>% select(age_group) %>% distinct()
  
  fig1 <- ggplot(data1) +
    geom_point(mapping = aes(x = q, y = err_val)) +
    ggtitle(paste("Objective functions vs. accessibility value -",data1$species, data1$year))+
    facet_wrap(~ factor(age_group,level=facet_order1$age_group), scales = "free") +
    theme_bw()
  
  fig2 <- ggplot(data2) +
    geom_point(mapping = aes(x = q, y = err_val)) +
    ggtitle(paste("Objective functions vs. accessibility value -",data2$species, data2$year))+
    facet_wrap(~ factor(age_group,level=facet_order2$age_group), scales = "free") +
    theme_bw()
  
  
  if(log_scale){ 
    fig1 <- fig1 + 
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log())
    fig2 <- fig2 + 
      scale_x_log10(labels = label_log()) +
      scale_y_log10(labels = label_log())
  }
  
  return(list(fig1,fig2))
  
}


plot_obs_vs_sim <- function(data){
  
  data <- data %>% pivot_longer(cols = c("sim","obs"), names_to = "datatype")
  
  fig <- ggplot(data, aes(x = datatype, y = value,fill=datatype)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ q_name, scales = "free") +
    scale_fill_manual(values = c("#9999CC", "#66CC99")) +
    labs(title = paste("Obs vs. sim -", data$species, data$year) )+
    theme_bw()

  return(fig)
}

plot_obs_vs_sim_merluccius <- function(data){
  
  facet_order <- c("Age 0","Age 1","Age 2","Age 3","Age 4",
                   "Age 5","Age 6","Age 7","Age 8",
                   "Age 9-10","Age 11-13","Age 14+")
  data <- data %>% 
    pivot_longer(cols = c("sim","obs"), names_to = "datatype") %>%
    mutate(age_group = paste("Age",age_group))
  
  fig <- ggplot(data, aes(x = datatype, y = value,fill=datatype)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order), scales = "free") +
    scale_fill_manual(values = c("#9999CC", "#66CC99")) +
    labs(title = paste("Obs vs. sim -", data$species, data$year) )+
    theme_bw()
  
  return(fig)
}

plot_obs_vs_sim_nephrops <- function(data){
  
  data1 <- data %>% 
    filter(grepl("Male",age_group)) %>%
    pivot_longer(cols = c("sim","obs"), names_to = "datatype") 
  
  data2 <- data %>% 
    filter(!grepl("Male",age_group)) %>%
    pivot_longer(cols = c("sim","obs"), names_to = "datatype") 
  
  facet_order1 <- data1 %>% select(age_group) %>% distinct()
  facet_order2 <- data2 %>% select(age_group) %>% distinct()
  
  fig1 <- ggplot(data1, aes(x = datatype, y = value,fill=datatype)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order1$age_group), scales = "free") +
    scale_fill_manual(values = c("#9999CC", "#66CC99")) +
    labs(title = paste("Obs vs. sim -", data1$species, data1$year) )+
    theme_bw()
  
  fig2 <- ggplot(data2, aes(x = datatype, y = value,fill=datatype)) +
    stat_summary(fun = "sum", geom = "bar", position = "dodge") +
    facet_wrap(~ factor(age_group,level=facet_order2$age_group), scales = "free") +
    scale_fill_manual(values = c("#9999CC", "#66CC99")) +
    labs(title = paste("Obs vs. sim -", data2$species, data2$year) )+
    theme_bw()
  
  return(list(fig1,fig2))
}

plot_q_estim <- function(best_set_data,mean_data){
  
  n_year <- best_set_data %>% select(year) %>% n_distinct()
  species <- best_set_data %>% select(species) %>% distinct()
  
  data1 <- best_set_data %>% select(year,q_name,q)
  data2 <- mean_data %>% mutate(year="mean") %>% select(year,q_name,q)
  data <-  rbind(data1,data2) %>% mutate(q_num = as.numeric(str_replace(q_name,"q","")))
  
  if(species == "Nephrops norvegicus"){
    
    data <- data %>%
      mutate(sex=if_else(q_num<=34,"Male","Female"))
    
    fig <- ggplot(data) +
      geom_point(mapping = aes(x = q_num, 
                               y = q, 
                               color = year, 
                               shape = year),
                 size = 3) +
      geom_line(mapping = aes(x = q_num, 
                              y = q, 
                              color = year),
                linewidth = 0.8,
                linetype='dotted') +
      facet_wrap(~factor(sex,levels = c("Male","Female")),nrow = 2)+
      scale_color_manual(values=c(brewer.pal(n_year, "Blues"),"orange")) +
      scale_shape_manual(values=c(rep(16,n_year),15)) +
      ggtitle(paste("q estimation by population group -", species))+
      xlab("Population group")+
      ylab("q Value") +
      theme_classic() +
      theme(title=element_text(size=14),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=18),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=22),
            strip.text.x = element_text(size = 18),
            legend.text = element_text(size=18),
            legend.title = element_text(size=22,face = "bold"),
            legend.position = "bottom")
    
  }else{
    
    fig <- ggplot(data) +
      geom_point(mapping = aes(x = q_num, 
                               y = q, 
                               color = year, 
                               shape = year),
                 size = 3) +
      geom_line(mapping = aes(x = q_num, 
                              y = q, 
                              color = year),
                linewidth = 0.8,
                linetype='dotted') +
      scale_color_manual(values=c(brewer.pal(n_year, "Blues"),"orange")) +
      scale_shape_manual(values=c(rep(16,n_year),15)) +
      ggtitle(paste("q estimation by population group -", species))+
      xlab("Population group")+
      ylab("q Value") +
      theme_classic() +
      theme(title=element_text(size=14),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=22),
            legend.text = element_text(size=18),
            legend.title = element_text(size=22,face = "bold"),
            legend.position = "bottom")
    
  }
  
  return(fig)
}

### Saving results----
save_q_noseason_csv <- function(best_set_data,save_path){
  thisYear <- best_set_data %>% select(year) %>% distinct() 
  species <- best_set_data %>% 
    select(species) %>%
    distinct() %>%
    str_replace(" ","_")
  
  if(species=="Nephrops_norvegicus"){
    best_set_data <- best_set_data %>%
      select(q_name,q) %>%
      rbind(data.frame(q_name=c("q33","q34"),q=c(0,0))) %>%
      arrange((q_name))
  }
  
  Q <- best_set_data %>%
    arrange(q_name) %>%
    mutate(january = q, february = q, march = q, april = q, 
           may = q, june = q, july = q, august = q,
           september = q, october = q, november = q, december=q) %>%
    select(q_name,january,february,march,april,may,june,
           july,august,september,october,november,december)
  
    write.table(Q,
              file = paste0(save_path,"/",species,"/acc_",thisYear,"_",species,".csv"),
              sep=';',
              row.names=F)
  
}

save_qmean_noseason_csv <- function(q_data,save_path){
  
  species <- q_data %>% 
    select(species) %>%
    distinct() %>%
    str_replace(" ","_")
  
  if(species=="Nephrops_norvegicus"){
    q_data <- q_data %>%
      select(q_name,q) %>%
      rbind(data.frame(q_name=c("q33","q34"),q=c(0,0))) %>%
      arrange((q_name))
  }
  
  Q <- q_data %>%
    arrange(q_name) %>%
    mutate(january = q, february = q, march = q, april = q, 
           may = q, june = q, july = q, august = q,
           september = q, october = q, november = q, december=q) %>%
    select(q_name,january,february,march,april,may,june,
           july,august,september,october,november,december)
  
  write.table(Q,
              file = paste0(save_path,"/",species,"/MeanAcc_",species,".csv"),
              sep=';',
              row.names=F)
  
}


