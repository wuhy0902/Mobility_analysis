timestr_generate = function(){
  time_AMPM = c(rep('AM',12),rep('PM',12))
  timestr = c()
  for (i in 0:24){
    if (i<12){
      timestr[(i+1)] = paste0(i,' AM')
    }else if (i==12){
      timestr[(i+1)] = paste0('12 PM')
    }else if (i>12 & i<24){
      timestr[(i+1)] = paste0((i-12),' PM')
    }else if(i==24){
      timestr[(i+1)] = '0 AM (next day)'
    }
  }
  return(timestr)
}


real_data_den_AS = function(dat2,idx_select,txt_use,all_day, t_start,save_path_name_density, save_path_name_AS){
  date_unique = unique(dat2$Date)
  # unique date
  tab_obs = table(dat2$Date)
  # number of observations per each date
  
  inv_mi = rep(NA, nrow(dat2))
  for(k_date in date_unique){
    idx_tmp = dat2$Date == k_date
    inv_mi[idx_tmp] = 1/sum(idx_tmp)
    # divide by mi (sum)
  }
  
  t_res = 50 ## Adjustable
  if (all_day==T){
    t_seq = (1:t_res)/t_res-1/(2*t_res)
    # this sequence represents the time interval we want to integrate
    w_matrix = matrix(NA, nrow = nrow(dat2), ncol = length(t_seq))
    
    for(t_idx in 1:length(t_seq)){
      t0 = t_seq[t_idx]
      
      w0 = dnorm(x= pmin(dat2$Time-t0,1-(dat2$Time-t0)), mean=0, sd = h_T)
      # pmin: correct for the time cyclic
      w1 = w0*inv_mi
      w2 = w1/sum(w1)
      w_matrix[,t_idx] = w2
    }
    #head(w_matrix)
    
    w3 = rowSums(w_matrix)/t_res
    dat2$weight = w3
  }else{
    t_end = t_start+1
    
    t_seq = ((1:t_res)/t_res-1/(2*t_res))*(t_end-t_start)/24+t_start/24
    w_matrix = matrix(NA, nrow = nrow(dat2), ncol = length(t_seq))
    
    for(t_idx in 1:length(t_seq)){
      t0 = t_seq[t_idx]
      
      w0 = dnorm(x= pmin(dat2$Time-t0,1-(dat2$Time-t0)), mean=0, sd = h_T)
      w1 = w0*inv_mi
      w2 = w1/sum(w1)
      w_matrix[,t_idx] = w2
    }
    w3 = rowSums(w_matrix)/t_res
  }
 
  dat_2d = cbind(dat2$Long, dat2$Lat)
  
  
  f_gps_int = kde(x=dat_2d[idx_select,], w = w3[idx_select], H= diag(h0^2,2))
  ## color plate
  z_display = log(f_gps_int$estimate+1, base=2)
  Mycolor = colorRampPalette(c("white","orange", "purple"))
  n_lv = 10
  if (all_day==T & k_select==1){
contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
          z = z_display,xlim=xlim0, ylim=ylim0,
          nlevels = n_lv,cex.axis=1.5,
          main="Example individual",
          cex.main=2)
  .filled.contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
                  z = z_display,
                  levels = z_lv,col = Mycolor(n_lv))
  
    mtext("average GPS log-density",side=3, at=0.05, line=-1.5, cex=1.5)
  image.plot(zlim = c(0,13), legend.only = TRUE, 
             col = Mycolor(n_lv), horizontal = FALSE, smallplot= c(.82,.86,0.6,0.9))

  }else{

    contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
          z = z_display,xlim=xlim0, ylim=ylim0,
          nlevels = n_lv,cex.axis=1.5,
          main=paste("Log-density ","(",txt_use,")",sep=""),
          cex.main=2)
  .filled.contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
                  z = z_display,
                  levels = z_lv,col = Mycolor(n_lv))
  if (all_day!=T){
    mtext(paste("Time: ", timestr[(t_start+1)],"-",timestr[(t_start+2)]),side=3, at=0.05, line=-1.5, cex=2)
    }
  image.plot(zlim = c(0,13), legend.only = TRUE, 
             col = Mycolor(n_lv), horizontal = FALSE, smallplot= c(.82,.86,0.6,0.9))
  }
  if (save_path_name_density!='No'){
    dir.create(dirname(save_path_name_density), showWarnings = FALSE, recursive = TRUE)
    dev.copy(pdf, save_path_name_density)
    dev.off()
  }
  
  
  
  ### activity space analysis
  dat_AS = dat2[idx_select,]
  f_gps_dat = kde(x=dat_2d[idx_select,], w = w3[idx_select], H= diag(h0^2,2),
                  eval.points = dat_2d[idx_select,])
  dat_AS$density = f_gps_dat$estimate
  dat_AS$weight = w3[idx_select]
  # updated weight at the current t
  dat_AS$weight = dat_AS$weight/sum(dat_AS$weight)
  
  ## computing the prob contents
  as_lv = (1:1000)/1000*max(dat_AS$density)
  z_as = rep(0, nrow=nrow(f_gps_int$estimate), ncol=ncol(f_gps_int$estimate))
  z_as = as.vector(z_as)
  for(j_lv in 1:length(as_lv)){
    as_tmp = as_lv[j_lv]
    idx_tmp = dat_AS$density<=as_tmp
    grid_tmp = as.vector(f_gps_int$estimate)>as_tmp
    z_as[grid_tmp] = sum(dat_AS$weight[idx_tmp])
  }
  z_as[is.na(z_as)]=0
  z_as[z_as==max(z_as)] = 1
  # force the grid point maximum to be 1 -- it may be lower than 1 due to resolution
  z_as = matrix(z_as, nrow = nrow(f_gps_int$estimate), ncol = ncol(f_gps_int$estimate))
  
  par(mar=c(4,4,2,1))
  contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
          z = z_as, xlim=xlim0, ylim=ylim0,cex.axis=1.5,
          nlevels = n_lv, main=paste("Activity space ","(",txt_use,")",sep=""), cex.main=2)
  .filled.contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
                  z = z_as,
                  levels = z_aslv,col = c("white","limegreen", "orange","blue","red"))
  legend("topright",col=c("limegreen", "orange","blue","red"),
         lwd=6, legend=c("99%","90%","70%","50%"), cex=1.5)
  if (all_day!=T){
  mtext(paste("Time: ", timestr[(t_start+1)],"-",timestr[(t_start+2)]),side=3, at=0.05, line=-1.5, cex=2)
  }
  if (save_path_name_AS!='No'){
    dir.create(dirname(save_path_name_AS), showWarnings = FALSE, recursive = TRUE)
    dev.copy(pdf, save_path_name_AS)
    dev.off()
  }
  if (all_day==T){
  return(dat2)
  }
}



cluster_plot_real_data = function(dat2,dat2d,clu_idx,save_path_density_folder='No',save_path_hourly_folder='No'){
  idx_select = dat2$Date %in% d_wd[hc_wd_cluster==clu_idx] 
  
  clu1_f_gps = kde(x=dat_2d[idx_select,], 
                   w = dat2$weight[idx_select], H= diag(h0^2,2),
                   xmin=c(xlim0[1],ylim0[1]), xmax=c(xlim0[2],ylim0[2]), 
                   gridsize=c(201,201))
  
  dat_ASc = dat2[idx_select,]
  dat_ASc$weight = dat_ASc$weight/sum(dat_ASc$weight)
  # correcting for the weight
  
  ### single contour selection for AS 2_clu1
  #pdf(paste("real_app/cluster/2_clu",clu_idx,".pdf", sep=""))
  par(mar=c(4,4,2,1))
  z_display = log(clu1_f_gps$estimate+1, base=2)
  contour(x=clu1_f_gps$eval.points[[1]], y= clu1_f_gps$eval.points[[2]],
          z = z_display,xlim=xlim0, ylim=ylim0,
          nlevels = n_lv, cex.axis=1.5,
          main=paste("Log-density Cluster ",clu_idx,sep=""),
          cex.main=2)
  .filled.contour(x=clu1_f_gps$eval.points[[1]], y= clu1_f_gps$eval.points[[2]],
                  z = z_display,
                  levels = z_lv,col = Mycolor(n_lv))
  if (save_path_density_folder!='No'){
    save_path_density = paste(save_path_density_folder,"/2_clu",clu_idx,".pdf", sep="")
    dir.create(dirname(save_path_density), showWarnings = FALSE, recursive = TRUE)
    dev.copy(pdf, save_path_density)
    dev.off()
  }

  
  ## center movement
  idx_select = dat2$Date %in% d_wd[hc_wd_cluster==clu_idx]

  date_unique = unique(dat2$Date)
  # unique date
  tab_obs = table(dat2$Date)
  # number of observations per each date
  
  inv_mi = rep(NA, nrow(dat2))
  for(k_date in date_unique){
    idx_tmp = dat2$Date == k_date
    inv_mi[idx_tmp] = 1/sum(idx_tmp)
    # divide by mi (sum)
  }

  t_res = 48
  t_seq = (1:t_res)/t_res-1/(t_res)
  t_matrix = matrix(NA, nrow = length(t_seq), ncol =  2)
  # every 30 mins
  
  dynamics_c = matrix(NA, ncol=3, nrow=t_res)
  for(idx_t in 1:t_res){
    t0 = t_seq[idx_t]
    
    w0 = dnorm(x= pmin(dat2$Time-t0,1-(dat2$Time-t0)), mean=0, sd = h_T)
    w1 = w0*inv_mi
    weight_t0 = w1/sum(w1)
    
    dynamics_c[idx_t,1] = idx_t/2
    dynamics_c[idx_t,2] = 
      sum(dat_2d[idx_select,1]*weight_t0[idx_select])/sum(weight_t0[idx_select])
    dynamics_c[idx_t,3] = 
      sum(dat_2d[idx_select,2]*weight_t0[idx_select])/sum(weight_t0[idx_select])
  }
  
  clu_f_gps = kde(x=dat_2d[idx_select,], 
                  w = dat2$weight[idx_select], H= diag(h0^2,2),
                  xmin=c(xlim0[1],ylim0[1]), xmax=c(xlim0[2],ylim0[2]), 
                  gridsize=c(101,101))
  
  #pdf(paste("real_app/cluster/3_clu",clu_idx,"c.pdf", sep=""))
  
  par(mar=c(4,4,2,1))
  
  contour(x=clu1_f_gps$eval.points[[1]], y= clu1_f_gps$eval.points[[2]],
          z = z_display,xlim=xlim0, ylim=ylim0,
          nlevels = n_lv,
          main=paste("Log-density Cluster ",clu_idx,sep=""),
          cex.main=2)
  .filled.contour(x=clu1_f_gps$eval.points[[1]], y= clu1_f_gps$eval.points[[2]],
                  z = z_display,
                  levels = z_lv,col = Mycolor(n_lv))
  legend("topright", pch=c(1:6), 
         legend=c("2 AM", "6 AM", "10 AM", "2 PM", "6 PM", "10 PM"),
         cex=1.5)
  
  points(dynamics_c[(1:6)*8-2,2:3], pch=c(1:6), cex=1.5, lwd=2)

  if (save_path_density_folder!='No'){
    save_path_density = paste(save_path_density_folder,"/3_clu",clu_idx,"c.pdf", sep="")
    dir.create(dirname(save_path_density), showWarnings = FALSE, recursive = TRUE)
    dev.copy(pdf, save_path_density)
    dev.off()
  }

  ## Plottings
  t_res = 50
  for(t_start in 0:23){
    t_end = t_start+1
    
    t_seq = ((1:t_res)/t_res-1/(2*t_res))*(t_end-t_start)/24+t_start/24
    w_matrix = matrix(NA, nrow = nrow(dat2), ncol = length(t_seq))
    
    for(t_idx in 1:length(t_seq)){
      t0 = t_seq[t_idx]
      
      w0 = dnorm(x= pmin(dat2$Time-t0,1-(dat2$Time-t0)), mean=0, sd = h_T)
      w1 = w0*inv_mi
      w2 = w1/sum(w1)
      w_matrix[,t_idx] = w2
    }
    
    w3 = rowSums(w_matrix)/t_res
    
    # just need to update weight at different time
    f_gps_int = kde(x=dat_2d[idx_select,], w = w3[idx_select], H= diag(h0^2,2))
    # update the kde since the weight is changed
    
    ## color plate
    z_display = log(f_gps_int$estimate+1, base=2)
    #pdf(paste("real_app/cluster/hour/fgps_clu",clu_idx,"_",t_start,".pdf", sep=""))
    par(mar=c(4,4,2,1))
    contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
            z = z_display,xlim=xlim0, ylim=ylim0,
            nlevels = n_lv, cex.axis=1.5,
            main=paste("Log-density Cluster ",clu_idx,sep=""),
            cex.main=2)
    .filled.contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
                    z = z_display,
                    levels = z_lv,col = Mycolor(n_lv))
    mtext(paste("Time: ", timestr[(t_start+1)],"-",timestr[(t_start+2)]),side=3, at=0.05, line=-1.5, cex=2)
    image.plot(zlim = c(0,13), legend.only = TRUE, 
               col = Mycolor(n_lv), horizontal = FALSE, smallplot= c(.82,.86,0.6,0.9))
    
    if (save_path_hourly_folder!='No'){
      save_path_density =paste(save_path_hourly_folder,"/fgps_clu",clu_idx,"_",t_start,".pdf", sep="")
      dir.create(dirname(save_path_density), showWarnings = FALSE, recursive = TRUE)
      dev.copy(pdf, save_path_density)
      dev.off()
    } 

    
    
    ### activity space
    dat_AS = dat2[idx_select,]
    clu1_f_gps_dat = kde(x=dat_2d[idx_select,], w = w3[idx_select], H= diag(h0^2,2),
                         eval.points = dat_2d[idx_select,])
    dat_AS$density = clu1_f_gps_dat$estimate
    dat_AS$weight = w3[idx_select]
    # updated weight at the current t
    dat_AS$weight = dat_AS$weight/sum(dat_AS$weight)
    
    ## computing the prob contents
    as_lv = (1:1000)/1000*max(dat_AS$density)
    z_as = rep(0, nrow=nrow(f_gps_int$estimate), ncol=ncol(f_gps_int$estimate))
    z_as = as.vector(z_as)
    for(j_lv in 1:length(as_lv)){
      as_tmp = as_lv[j_lv]
      idx_tmp = dat_AS$density<=as_tmp
      grid_tmp = as.vector(f_gps_int$estimate)>as_tmp
      z_as[grid_tmp] = sum(dat_AS$weight[idx_tmp])
    }
    z_as[is.na(z_as)]=0
    z_as[z_as==max(z_as)] = 1
    # force the grid point maximum to be 1 -- it may be lower than 1 due to resolution
    z_as = matrix(z_as, nrow = nrow(f_gps_int$estimate), ncol = ncol(f_gps_int$estimate))
    
    #pdf(paste("real_app/cluster/hour/fgps_clu",clu_idx,"AS_",t_start,".pdf", sep=""))
    par(mar=c(4,4,2,1))
    contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
            z = z_as, xlim=xlim0, ylim=ylim0,
            nlevels = n_lv, cex.axis=1.5,
            main=paste("Activity Space Cluster ",clu_idx,sep=""),
            cex.main=2)
    .filled.contour(x=f_gps_int$eval.points[[1]], y= f_gps_int$eval.points[[2]],
                    z = z_as, 
                    levels = z_aslv,col = c("white","limegreen", "orange","blue","red"))
    legend("topright",col=c("limegreen", "orange","blue","red"),
           lwd=10, legend=c("99%","90%","70%","50%"), cex=1.5)
    mtext(paste("Time: ", timestr[(t_start+1)],"-",timestr[(t_start+2)]),side=3, at=0.05, line=-1.5, cex=2)
    
    if (save_path_hourly_folder!='No'){
      save_path_AS =paste(save_path_hourly_folder,"/fgps_clu",clu_idx,"AS_",t_start,".pdf", sep="")
      dir.create(dirname(save_path_AS), showWarnings = FALSE, recursive = TRUE)
      dev.copy(pdf, save_path_AS)
      dev.off()
    } 
  }
}

scatterplot_generation = function(dat2,day_num,xlim0,ylim0,title,save_file_loc){
  dat_2d_day = dat2[dat2$Date==day_num,]
  plot(dat_2d_day$Long,dat_2d_day$Lat,xlim=xlim0,ylim=ylim0,xlab='x',ylab='y',main=title,axes=F)
  box()
  dir.create(dirname(save_file_loc), showWarnings = FALSE, recursive = TRUE)
  if (save_file_loc!='No'){
    dir.create(dirname(save_file_loc), showWarnings = FALSE, recursive = TRUE)
    dev.copy(pdf, save_file_loc)
  dev.off()
  }
  
}