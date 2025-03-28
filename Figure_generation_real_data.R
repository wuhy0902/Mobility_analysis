#--------------------------------Package----------------------------------------
library(ks)
library(plotrix)
library(fields)

source('real_data_appl_functions.R')
#------------------------- Data loading and processing--------------------------
{
  # A named vector to map weekdays to numbers:
  weekday_map <- c(
    "Monday"    = 1,
    "Tuesday"   = 2,
    "Wednesday" = 3,
    "Thursday"  = 4,
    "Friday"    = 5,
    "Saturday"  = 6,
    "Sunday"    = 7
  )
dat = read.csv("data125/GPS_shifted_125.csv")

set.seed(202502)

### processing time
t0 = as.POSIXct("0:00:00", format = "%H:%M:%S")
t_vec = rep(NA, length = nrow(dat))

for(j in 1:nrow(dat)){
  t_tmp = as.POSIXct(dat$Time[j], format = "%I:%M:%S %p")
  t1_tmp = as.numeric(difftime(t_tmp, t0, unit="hours"))/24
  
  t_vec[j] = t1_tmp
}


### processing date
date_ind <- numeric(nrow(dat))  # weekday index
  date_num <- numeric(nrow(dat))  # day of month
  
  for (j in seq_len(nrow(dat))) {
    date_str <- dat$Date[j]
    
    # Split by commas -> c("Wednesday", " Month 1", " day No.4")
    parts <- strsplit(date_str, ",\\s*")[[1]]
    
    # parts[1] = weekday name, e.g. "Wednesday"
    # parts[2] = e.g. "Month 1"
    # parts[3] = e.g. "day No.4"
    
    # 1) Weekday name to numeric index
    weekday_name <- parts[1]
    weekday_num  <- weekday_map[weekday_name]
    
    # 2) Extract the numeric month index from "Month 1"
    month_str <- parts[2] # e.g. "Month 1"
    # Remove "Month " and convert to numeric
    month_index <- as.numeric(sub("^Month\\s+", "", month_str))
    
    # 3) Extract the numeric day from "day No.4"
    day_str <- parts[3] # e.g. "day No.4"
    # Remove "day No." and convert to numeric
    day_num <- as.numeric(sub("^day No.", "", day_str))
    
    # Store results
    date_ind[j] <- weekday_num  # e.g. 3 if "Wednesday"
    date_num[j] <- day_num      # e.g. 4
  }

dat1 = data.frame(PID = dat$PID, Date = date_num, Time = t_vec,WE= date_ind,
                  Lat = dat$Latitude, Long = dat$Longitude)
dat2 = dat1[date_num!=10,]

h0 = 0.005
h_T = 0.02
# smoothing bandwidth

### Boundary
xlim0 = c(-0.2, 0.3)
ylim0 = c(-0.2, 0.3)

timestr = timestr_generate()

# 1 = all
# 2 = weekdays
# 3 = weekends
txt_use_all = c("All days","Weekdays","Weekends")
doc_name_all = c("all", "WD","WE")
idx_select_all = list(dat2$WE<10,dat2$WE<6,dat2$WE>5)

n_lv = 10

z_lv = seq(0, 13, length.out = n_lv)
# set it to be the same for all
z_aslv = c(0,0.01, 0.1,0.3, 0.5,1.1)
# the level for activity space

Mycolor = colorRampPalette(c("white","orange", "purple"))
}

#----------------------Picture reproduction-------------------------------------
##---------------------Figure 1 (obs plot) ------------------------------------
{
  scatterplot_generation(dat2,4,xlim0,ylim0,'Day 1',"real_app/D1-1.pdf")
  scatterplot_generation(dat2,5,xlim0,ylim0,'Day 2',"real_app/D1-2.pdf")
  scatterplot_generation(dat2,6,xlim0,ylim0,'Day 3',"real_app/D1-3.pdf")
  scatterplot_generation(dat2,8,xlim0,ylim0,'Day 4 (Weekends)',"real_app/D1-4.pdf")
}

##-------------------- Figure 4 (timestamp hist) generation -----------------------
{
## weekdays timestamps
hist(t_vec[date_ind<6], col="pink", main="Example individual (weekdays)", 
     xlab = "Time", cex.lab=1.5, ylab="",
     cex.main=2, breaks=20)
mtext("Frequency", side=2, line = 2.3, cex = 1.5)
dev.copy(pdf, paste("real_app/p125_hist1.pdf", sep=""))
dev.off()
## weekends timestamps
hist(t_vec[date_ind>5], col="limegreen", main="Example individual (weekends)", 
     xlab = "Time", cex.lab=1.5, ylab="",
     cex.main=2, breaks=20)
mtext("Frequency", side=2, line = 2.3, cex = 1.5)
dev.copy(pdf, paste("real_app/p125_hist2.pdf", sep=""))
dev.off()
}

##-------------------- Figure 4 (Log-density), 5 (activity space) generation -----------------------
{
k_select = 1
dat2 = real_data_den_AS(dat2,idx_select_all[[k_select]],txt_use_all[k_select],all_day=T, t_start=0,"real_app/p125_log.pdf" ,paste("real_app/","AS_",doc_name_all[k_select],".pdf", sep=""))

k_select = 2
dat2 = real_data_den_AS(dat2,idx_select_all[[k_select]],txt_use_all[k_select],all_day=T, t_start=0,paste("real_app/",doc_name_all[k_select],"_density",".pdf", sep=""),paste("real_app/","AS_",doc_name_all[k_select],".pdf", sep=""))

k_select = 3
dat2 = real_data_den_AS(dat2,idx_select_all[[k_select]],txt_use_all[k_select],all_day=T, t_start=0,paste("real_app/",doc_name_all[k_select],"_density",".pdf", sep=""),paste("real_app/","AS_",doc_name_all[k_select],".pdf", sep=""))
}

##-------------------- Figure 6 generation -----------------------
{
k_select = 2
for(t_start in c(1,7,12,17)){
  real_data_den_AS(dat2,idx_select_all[[k_select]],txt_use_all[k_select],all_day=F, t_start=t_start,paste("real_app/","hour/fgps_",doc_name_all[k_select],"_",t_start,".pdf", sep=""),paste("real_app/","hour/fgps_",doc_name_all[k_select],"AS_",t_start,".pdf", sep=""))
}
}

##-------------------- Figure 7 generation -----------------------
{
k_select = 3
for(t_start in c(1,7,12,17)){
  real_data_den_AS(dat2,idx_select_all[[k_select]],txt_use_all[k_select],all_day=F, t_start=t_start,paste("real_app/","hour/fgps_",doc_name_all[k_select],"_",t_start,".pdf", sep=""),paste("real_app/","hour/fgps_",doc_name_all[k_select],"AS_",t_start,".pdf", sep=""))
}
}

##-------------------- Figure 8,9 generation -----------------------
###-------------------- Preparation -----------------------
{
# Cluster
dat_2d = cbind(dat2$Long, dat2$Lat)

daily_f_gps = list()
date_unique = unique(dat2$Date)
for(idx_day in date_unique){
  idx_select = dat2$Date==idx_day
  daily_f_gps[[idx_day]] = kde(x=dat_2d[idx_select,], 
                               w = dat2$weight[idx_select], H= diag(h0^2,2),
                               xmin=c(xlim0[1],ylim0[1]), xmax=c(xlim0[2],ylim0[2]), 
                               gridsize=c(101,101))
  # fixed locations for evaluting density
}

## Cluster the activity


table(dat2$Date, dat2$WE)
d_we = c(7,8, 14, 15, 21, 22, 28,29)
d_wd = setdiff(date_unique, d_we)
# weekend/weekday indicator
# weekend pattern is a bit boring--only two days are on a trip to somewhere


### get distance matrix
### we focus on weekday
dat3_wd = dat2[dat2$WE<6,]
# weekday data

d_matrix_wd = matrix(0, nrow=length(d_wd), ncol=length(d_wd))
# distance matrix for weekday

for(i in 1:(length(d_wd)-1)){
  for(j in (i+1):length(d_wd)){
    idx_day1 = d_wd[i]
    idx_select1 = dat2$Date==idx_day1
    idx_day2 = d_wd[j]
    idx_select2 = dat2$Date==idx_day2
    
    z_display1 = log(daily_f_gps[[idx_day1]]$estimate+1,base=2)
    z_display2 = log(daily_f_gps[[idx_day2]]$estimate+1,base=2)
    
    diff_tmp = sum((z_display1-z_display2)^2)
    d_matrix_wd[i,j] = d_matrix_wd[j,i] = diff_tmp
  }
}
}

###--------------------  plotting ----------------------
{
par(mar=c(4,4,2,1))
hc_wd = hclust(d = as.dist(d_matrix_wd), method = "single")
plot(hc_wd,labels = d_wd, xlab="", main="Dendrogram (single linkage)", 
     cex.main=2, ylab="")
mtext("Distance", side=2, line=2.2, cex=1.5)
abline(h=2100, lwd=2, col="red")
mtext("Cluster 1", side=1, line=-.2, at =8, cex=1.5, col="brown")
mtext("Cluster 2", side=1, line=-.2, at =19, cex=1.5, col="blue")
dev.copy(pdf, paste("real_app/cluster/1_dendrogram.pdf", sep=""))
dev.off()

### clustering
hc_wd_cluster = cutree(hc_wd, h=2100)
hc_wd_cluster

cluster_plot_real_data(dat2,dat2d,1)
cluster_plot_real_data(dat2,dat2d,2)

}
