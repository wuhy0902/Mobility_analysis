
## Anchor points
find_local_maxima_discrete <- function(grid_center_need, density_values) {
  #### 1) Check input consistency
  if (nrow(grid_center_need) != length(density_values)) {
    stop("The number of density values must match the number of grid points.")
  }
  
  # Extract unique sorted x and y
  x_unique <- sort(unique(grid_center_need[, 1]))
  y_unique <- sort(unique(grid_center_need[, 2]))
  nx <- length(x_unique)
  ny <- length(y_unique)
  
  if (nx * ny != nrow(grid_center_need)) {
    stop("grid_center_need does not form a complete rectangular grid. 
Please check or consider an interpolation-based approach.")
  }
  
  #### 2) Sort by (x,y) and reshape
  ord <- order(grid_center_need[,1], grid_center_need[,2])
  grid_sorted <- grid_center_need[ord, ]
  dens_sorted <- density_values[ord]
  
  # Reshape into a matrix fmat[i,j] where
  #   i indexes x_unique (length nx)
  #   j indexes y_unique (length ny)
  fmat <- matrix(dens_sorted, nrow = nx, ncol = ny, byrow = TRUE)
  indice_mat = matrix(ord, nrow = nx, ncol = ny, byrow = TRUE)
  
  #### 3) Find discrete local maxima by neighbor comparison
  local_maxima <- data.frame(
    ix      = integer(),
    iy      = integer(),
    x_coord = numeric(),
    y_coord = numeric(),
    f_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # We check only "interior" points [2..nx-1, 2..ny-1]
  # so each point has 8 neighbors. 
  # (You can adapt if you want to consider boundaries too.)
  
  for (i in 2:(nx-1)) {
    for (j in 2:(ny-1)) {
      center_val <- fmat[i, j]
      
      # Collect the 8 neighbors
      neighbors <- c(
        fmat[i-1, j-1], fmat[i-1, j], fmat[i-1, j+1],
        fmat[i,   j-1],                fmat[i,   j+1],
        fmat[i+1, j-1], fmat[i+1, j], fmat[i+1, j+1]
      )
      
      # Check if center_val >= all neighbors
      if (center_val >= max(neighbors)) {
        # This is a local max (in the discrete sense)
        local_maxima <- rbind(local_maxima, data.frame(
          ind = indice_mat[i,j],
          ix      = i,
          iy      = j,
          x_coord = x_unique[i],
          y_coord = y_unique[j],
          f_value = center_val
        ))
      }
    }
  }
  
  # Optionally, sort results by descending density
  local_maxima <- local_maxima[order(-local_maxima$f_value), ]
  
  #### 4) Return local maxima
  return(local_maxima)
}


find_k_for_threshold <- function(grid_center_need, density_values, rho) {
  # 1) Sort indices by density in descending order
  ord <- order(density_values, decreasing = TRUE)
  
  # Reorder densities and their coordinates
  dens_desc <- density_values[ord]
  centers_desc <- grid_center_need[ord, , drop = FALSE]
  
  # 2) Compute cumulative sums
  cum_vals <- cumsum(dens_desc)
  
  # 3) Find the smallest k where sum of top k > rho
  k_index <- which(cum_vals > rho)[1]
  
  # If the sum never exceeds rho, use all points (or handle differently)
  if (is.na(k_index)) {
    k_index <- length(dens_desc)
    message("Total sum does not exceed rho; using all points.")
  }
  
  # 4) Retrieve top k points (coordinates + density)
  chosen_centers <- data.frame(
    x = centers_desc[seq_len(k_index), 1],
    y = centers_desc[seq_len(k_index), 2],
    density = dens_desc[seq_len(k_index)]
  )
  
  # Return both k and the chosen centers with densities
  list(
    k = k_index,
    ord_need = ord[1:k_index],
    chosen_centers = chosen_centers
  )
}

#result <- find_k_for_threshold(grid_center_need, all_res[[1]][[5]], rho = 0.999)
#result$k                 # number of top points
#head(result$chosen_centers,10)

single_day_densities = function(observations_multiple_days, grid_center_need, goal_interval,method='marginal',t_range_need=NULL){
all_res_single_day_all = list()
pb = txtProgressBar(min = 0, max = length(unique(observations_multiple_days$day)), style = 3)

for (single_day in unique(observations_multiple_days$day)){
  observations_multiple_single_day = observations_multiple_days[observations_multiple_days$day==single_day,]
  
  all_res_single_day_all[[single_day]] = kde_gps(observations_multiple_single_day, grid_center_need,method, t_range_need=t_range_need,time_interval=goal_interval,hs_const=hs_const,ht_const=ht_const)
   setTxtProgressBar(pb, single_day)
}
  return(all_res_single_day_all)
}


cluster_pattern = function(all_res_single_day_all, pattern_realizations_list, pattern_chosen,eps){
  
  day_indices =  which(pattern_realizations_list[[2]] %in% pattern_chosen)
  Lap_graph_chosen_days = matrix(0,length(day_indices),length(day_indices))
  
for (i in 1:length(day_indices)){
  for (j in 1:length(day_indices)){
    Lap_graph_chosen_days[i,j] = sum((log(all_res_single_day_all[[day_indices[i]]]+eps)-log(all_res_single_day_all[[day_indices[j]]]+eps))^2)
  } 
}
return(list(cluster_result=pattern_realizations_list[[2]][day_indices], Lap_graph= Lap_graph_chosen_days, day_indices=day_indices))
}


cluster_densities = function(observations_multiple_days,pattern_realizations_list,grid_center_need,pattern_vec=c(1:5),t_range_need_condi = c(6/24,9/24,12/24,15/24,18/24,21/24),method='integral_conditional'){
  all_res_single_cluster_all_f = list()
  for (cluster_no in pattern_vec){
    days_cluster_n = which(pattern_realizations_list[[2]]==cluster_no)
    observations_multiple_cluster_days = observations_multiple_days[observations_multiple_days$day %in% days_cluster_n,]
    all_res_single_cluster_all_f[[cluster_no]] = list()

    for (id_t in 1:length(t_range_need_condi)){
       all_res_single_cluster_all_f[[cluster_no]][[id_t]] = kde_gps(observations_multiple_cluster_days, grid_center_need, method, t_range_need= t_range_need_condi[id_t],time_interval=goal_interval,hs_const=hs_const,ht_const=ht_const)
  
      }
  }
  return(all_res_single_cluster_all_f)
}

cluster_centers = function(observations_multiple_days,pattern_realizations_list,grid_center_need,pattern_vec=c(1:5),t_range_need_condi = c(6/24,9/24,12/24,15/24,18/24,21/24)){
  all_res_single_cluster_all_x = list()
  all_res_single_cluster_all_y = list()
for (cluster_no in c(1:5)){
  days_cluster_n = which(pattern_realizations_list[[2]]==cluster_no)
  observations_multiple_cluster_days = observations_multiple_days[observations_multiple_days$day %in% days_cluster_n,]
  
center_x = rep(0,length(t_range_need_condi))
center_y = rep(0,length(t_range_need_condi))

for (id_t in 1:length(t_range_need_condi)){

  t = t_range_need_condi[id_t]
  ht = 0.05*(freq_parameter)^(-1/3)
  all_t_kernel = dnorm(pmin((observations_multiple_cluster_days$timestamp-t),1-(observations_multiple_cluster_days$timestamp-t))/ht)
  center_x[id_t] = sum(observations_multiple_cluster_days$x*all_t_kernel)/sum(all_t_kernel)
  center_y[id_t] = sum(observations_multiple_cluster_days$y*all_t_kernel)/sum(all_t_kernel)
}
all_res_single_cluster_all_x[[cluster_no]] = center_x
all_res_single_cluster_all_y[[cluster_no]] = center_y
}
return(list(center_x_list = all_res_single_cluster_all_x,center_y_list =all_res_single_cluster_all_y))
}


velocity_estimate = function(trial,pattern_realizations_list,observations_multiple_days){
  if (trial!=0){
  day_indices = which(pattern_realizations_list[[2]]==trial)
observations_multiple_days_needed = observations_multiple_days[observations_multiple_days$day %in% day_indices,]
}else{
  observations_multiple_days_needed = observations_multiple_days 
}

n <- nrow(observations_multiple_days_needed)

# Suppose we store data in a data.frame 'df' with columns:
#  G   : group indicator (some = 'g', others = 'other')
#  t   : time
#  X1  : first coordinate
#  X2  : second coordinate
#  m_i : number of measurements for the subject i

df_g <- data.frame(
  t  = observations_multiple_days_needed$timestamp,
  X1 = observations_multiple_days_needed$x,
  X2 = observations_multiple_days_needed$y,
  m_i = rep(479,n)
)

# Fit a loess model of degree 2 for X1
fit_X1_loess <- loess(
  X1 ~ t, 
  data = df_g, 
  weights = 1 / df_g$m_i,  # weighting
  span = 0.1,  # Smoothing parameter (adjust based on your data)
  degree = 2   # Local quadratic fit
)

fit_X2_loess <- loess(
  X2 ~ t, 
  data = df_g, 
  weights = 1 / df_g$m_i,  # weighting
  span = 0.1,  # Smoothing parameter (adjust based on your data)
  degree = 2   # Local quadratic fit
)


# Time grid for evaluation
t_grid <- seq(min(df_g$t), max(df_g$t), length.out = 1000)

# Predict fitted values at t_grid
predicted_X1 <- predict(fit_X1_loess, newdata = data.frame(t = t_grid))
predicted_X2 <- predict(fit_X2_loess, newdata = data.frame(t = t_grid))

# Compute numerical first derivative using finite differences
derivative_X1 <- diff(predicted_X1) / diff(t_grid)
derivative_X2 <- diff(predicted_X2) / diff(t_grid)


# Adjust t_grid to match derivative values (since diff reduces length by 1)
t_grid_mid <- (t_grid[-1] + t_grid[-length(t_grid)]) / 2

velocity <- sqrt(derivative_X1^2 + derivative_X2^2)
return(list(t_grid=t_grid_mid, velocity=velocity))
}
