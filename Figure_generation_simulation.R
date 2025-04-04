#--------------------------------Package----------------------------------------
### Spatial packages for generating the map and reference distribution
library(sf) 
library(sp) 

### Stats package linking to spatial packages
library(plyr) 
library(tidyverse)

### Stats package linking to spatial packages
library(smerc) #Regional counts in reference distribution
library(vctrs)# auxillary vector-matrix process package
library(EnvStats)# Truncated Gaussian
library(Matrix)

### Stats package linking to spatial packages
library(ks) #KDE

### Stats package linking to spatial packages
library(fields) #image.plot for density
library(latex2exp) #LaTeX
library(ggplot2) 
library(pheatmap) #Heatmap plot

source('data_generate_funcs_cluster_ks.R')
source('kde_funcs_cluster_revised_ks.R')
source('helper_functions.R')
source('plot_related_functions.R')
source('more_simulation_funcs.R')

#----------------------------Basic preparation-----------------------------------
ht_const = 0.05
hs_const = 0.065 #0.07

##------------------------For generating reference distribution----------------
goal_interval_all = list(c(8/24,10/24),c(0,0.99999)) # interested interval
measurement_error_all = c(0.1,0.2) # measurement error choice
time_range = c('overall','8 AM-10 AM')

#---------------------------Figures reproduction--------------------------------
##------------------ Figure 10 generation (8 mins,might be needed)---------------
{
  ## 4 pictures; Each for different interested interval, and measurement error
  for (id1 in c(1,2)){
    for (id2 in c(1,2)){
      
      ## Set interested interval, and measurement error; Also generate the reference distribution
      goal_interval = goal_interval_all[[id1]]
      measurement_error =measurement_error_all[id2] # In paper, set as 0.1, or 0.2
      
      ref_distri_info = ref_distri(days = 100, freq_parameter = 1439, measurement_error=measurement_error, 
                                   running_amount = 100, distri_true_need ='yes', time_interval =goal_interval,
                                   grid_cell_size = 0.2, way_of_ref = 'cell_obs_count') 
      #You can run ref_distri_help() to see the detail information of function ref_distri_info 
      
      # Extract relevant outputs from the function
      grid_center_need = ref_distri_info$grid_centers  # Grid center coordinates
      grid_need = ref_distri_info$grid_need            # Grid structure
      true_density_for_true = ref_distri_info$distri_ref_true  # True density reference
      true_density_for_obs = ref_distri_info$distri_ref  # Observed density reference
      
      # Set a small constant to avoid log(0) issues
      eps = 0.0001  
      
      # Determine the range for plotting based on log-transformed densities
      max_min_range = range_for_plot(list(true_density_for_obs), eps)
      
      # Plot the true density distribution (log-transformed)
    
      density_plot(true_density_for_obs, eps, grid_center_need, 
                   TeX(paste0("$\\log(f_{GPS}+\\xi), \\epsilon=",measurement_error,"$")), paste0('(',time_range[id1],')'), max_min_range, 
                   save_path_name = paste0('sim/pic/basic/density_plot/true_density_',time_range[id1],measurement_error,'.pdf'))
      
    }
  }
}


##-------------------- Table of evaluation generation (including table 2-5) -----------------------
### The following part can run on the cluster by setting running_time_total=100
{
  goal_interval = c(0,0.99999)
  measurement_error =0.2
  
  # Generate reference distribution
  ref_distri_info = ref_distri(days = 100, freq_parameter = 1439, measurement_error=measurement_error, 
                               running_amount = 100, distri_true_need ='yes', time_interval =goal_interval,
                               grid_cell_size = 0.2, way_of_ref = 'cell_obs_count') 
  # Extract relevant outputs from the function
  grid_center_need = ref_distri_info$grid_centers  # Grid center coordinates
  grid_need = ref_distri_info$grid_need            # Grid structure
  true_density_for_true = ref_distri_info$distri_ref_true  # True density reference
  true_density_for_obs = ref_distri_info$distri_ref  # Observed density reference
  
  ## The parameters for GPS observation generation:
  freq_pattern = 'even_space'      # Default to 'even_space'; in paper, we also tried 'irregular'
  days = 90                          # Default to 90 days; in paper, we also tried 30, 7
  freq_parameter = 479        # Default to 479 observations per day (1 obs per 3 mins); in paper, we also tried 159, 1439
  
  # Evaluate accuracy 
  # Define the total number of iterations for estimation; 
  # You can set it as a higher value to find the average accuracy of the estimator
  # Note: in this file, for reproducing all pictures in paper, don't change its value at first because the randomness may change.
  running_time_total = 1
  
  # Seed
  running_time = 20250224
  
  # Initialize accuracy metrics for different estimation methods
  accu_naive_sum = 0
  accu_marg_sum = 0
  accu_inte_sum = 0
  accu_naive_naive_sum = 0
  
  # Loop through the number of runs (only 1 in this case)
  for (running_time in 1:running_time_total) {
    # Generate observational data
    obs_info = data_generation(days, freq_pattern, freq_parameter, measurement_error, goal_interval, running_time)
    # You can run data_generation_help() for more information about generating observational data.
    
    print('Observation generation step is done!')  
    
    # Extract generated patterns and observations
    pattern_realizations_list = obs_info[[1]]
    observations_multiple_days = obs_info[[2]]
    
    # Define the probability density function and time range
    func = c('dnorm', 'dnorm')
    t1 = goal_interval[1]
    t2 = goal_interval[2]
    t_range_need = seq(t1, t2, length.out = 199)
    
    # Estimate densities using different kernel density estimation methods
    naive_esti_density = kde_gps(observations_multiple_days, grid_center_need, 'naive', 
                                 t_range_need = NULL, time_interval = goal_interval, 
                                 hs_const = hs_const, ht_const = ht_const)
    
    marginal_esti_density = kde_gps(observations_multiple_days, grid_center_need, 'marginal', 
                                    t_range_need = NULL, time_interval = goal_interval, 
                                    hs_const = hs_const, ht_const = ht_const)
    
    inte_conditional_esti_density = kde_gps(observations_multiple_days, grid_center_need, 
                                            'integral_conditional', t_range_need = t_range_need, 
                                            time_interval = goal_interval, hs_const = hs_const, 
                                            ht_const = ht_const)
    
    naive_naive_esti_density = kde_gps(observations_multiple_days, grid_center_need, 
                                       'naive_naive_bandwidth', t_range_need = NULL, 
                                       time_interval = goal_interval, hs_const = hs_const, 
                                       ht_const = ht_const)
    # You can run kde_gps_help() for more information about the KDE estimation process.
    
    # Compute estimation errors (MISE) for each method
    accu_naive = esti_error(true_density_for_obs, naive_esti_density, grid_center_need)
    accu_marg = esti_error(true_density_for_obs, marginal_esti_density, grid_center_need)
    accu_inte = esti_error(true_density_for_obs, inte_conditional_esti_density, grid_center_need)
    accu_naive_naive = esti_error(true_density_for_obs, naive_naive_esti_density, grid_center_need)
    
    # Accumulate errors for future averaging
    accu_naive_sum = accu_naive_sum + accu_naive
    accu_marg_sum = accu_marg_sum + accu_marg
    accu_inte_sum = accu_inte_sum + accu_inte
    accu_naive_naive_sum = accu_naive_naive_sum + accu_naive_naive
  }
  accu_vec = c(accu_marg_sum/running_time_total, accu_inte_sum/running_time_total,accu_naive_sum/running_time_total, accu_naive_naive_sum/running_time_total)
  
  accu_res = data.frame(MISE=accu_vec)
  rownames(accu_res) = c('Marginal', 'Integral', 'Naive (with same bandwidth)','Naive (with Sliverman bandwidth)')
  accu_res #MISE for 4 estimators
  ## Comment

}

#-------------------- More simulation preparation (must run before generating the following pictures)-----------------------
## Data generation
{

# Define the time interval for the observation data
goal_interval = c(0, 0.99999)

measurement_error = 0.2   # Default to 0.2
freq_pattern = 'even_space'      # Default to 'even_space'
days = 90                          # Default to 90 days
freq_parameter = 479        # Default to 479 observations per day

# Generate observational data based on the defined parameters
obs_info = data_generation(days, freq_pattern, freq_parameter, measurement_error, goal_interval, running_time)

# Extract elements from the generated data
pattern_realizations_list = obs_info[[1]]  # List of generated movement patterns
observations_multiple_days = obs_info[[2]]  # Observed data over multiple days
seq_of_extreme = obs_info[[3]]  # Sequence of extreme values (min, max coordinates)

# Define grid points for temporal density estimation
t_grid_center = seq(0, 1, length.out = 101)

# Set kernel bandwidth parameters for density estimation
hs_const = 0.065  # Spatial bandwidth parameter
ht_const = 0.05   # Temporal bandwidth parameter
}

{
# Define time range for kernel density estimation
t1 = 0.001
t2 = 0.999
t_range_need = seq(t1, t2, length.out = 99)

# Compute overall integral conditional density estimates
inte_conditional_esti_density = kde_gps(observations_multiple_days, grid_center_need, 
                                        'integral_conditional', t_range_need = t_range_need, 
                                        time_interval = goal_interval, hs_const = hs_const, 
                                        ht_const = ht_const)

# Filter observations by weekends and weekdays
days_weekends = which(pattern_realizations_list[[2]] %in% c(3, 4, 5))  # Weekend days
days_weekdays = which(pattern_realizations_list[[2]] %in% c(1, 2))  # Weekdays

# Extract weekday observations
observations_multiple_days_weekdays = observations_multiple_days[observations_multiple_days$day %in% days_weekdays, ]

# Compute density estimates for weekdays
inte_conditional_esti_density_weekdays = kde_gps(observations_multiple_days_weekdays, grid_center_need, 
                                                 'integral_conditional', t_range_need = t_range_need, 
                                                 time_interval = goal_interval, hs_const = hs_const, 
                                                 ht_const = ht_const)

# Extract weekend observations
observations_multiple_days_weekends = observations_multiple_days[observations_multiple_days$day %in% days_weekends, ]

# Compute density estimates for weekends
inte_conditional_esti_density_weekends = kde_gps(observations_multiple_days_weekends, grid_center_need, 
                                                 'integral_conditional', t_range_need = t_range_need, 
                                                 time_interval = goal_interval, hs_const = hs_const, 
                                                 ht_const = ht_const)


}

##-------------------- Figure 11 generation (Anchor points detection)-----------------------------------
{
# Define the file path to the anchor points CSV file
anchors_df_path <- paste0(getwd(), "/data_to_read/anchors.csv")

# Read the anchor points data from the CSV file
anchors_mat = as.matrix(read.csv(anchors_df_path)[, c('x', 'y')])

# Display the loaded anchor points matrix
anchors_mat

### Find peaks
# Identify local maxima in the estimated density for overall data
discrete_peaks <- find_local_maxima_discrete(grid_center_need, inte_conditional_esti_density)

# Extract indices of activity spaces where density is greater than 0.02
activity_space_indices = as.numeric(discrete_peaks$ind[which(discrete_peaks$f_value > 0.022)])

# Identify local maxima in the estimated density for weekdays
discrete_peaks <- find_local_maxima_discrete(grid_center_need, inte_conditional_esti_density_weekdays)
activity_space_indices_weekdays = as.numeric(discrete_peaks$ind[which(discrete_peaks$f_value > 0.022)])

# Identify local maxima in the estimated density for weekends
discrete_peaks <- find_local_maxima_discrete(grid_center_need, inte_conditional_esti_density_weekends)
activity_space_indices_weekends = as.numeric(discrete_peaks$ind[which(discrete_peaks$f_value > 0.022)])

### Extract peaks to detect anchor points
# Plot actual vs. detected anchor points for overall observations
anchor_point_detect_plot(activity_space_indices, anchors_mat, grid_center_need, 
                         "Actual v.s. Detected Anchor Points (overall)", 
                         seq_of_extreme, 
                         save_path_name = 'sim/pic/more_simulations/anchor_points/anchor_points_overall.pdf')

# Plot actual vs. detected anchor points for weekdays
anchor_point_detect_plot(activity_space_indices_weekdays, anchors_mat[c(1,2,3),], grid_center_need, 
                         "Actual v.s. Detected Anchor Points (weekdays)", 
                         seq_of_extreme, 
                         save_path_name = 'sim/pic/more_simulations/anchor_points/anchor_points_weekdays.pdf')

# Plot actual vs. detected anchor points for weekends
anchor_point_detect_plot(activity_space_indices_weekends, anchors_mat[c(1,4,5),], grid_center_need, 
                         "Actual v.s. Detected Anchor Points (weekends)", 
                         seq_of_extreme, 
                         save_path_name = 'sim/pic/more_simulations/anchor_points/anchor_points_weekends.pdf')
}

##-------------------- Figure 12 generation (Activity space)----------------------------------
{
### Probability level set activity space
### Activity space visualization using level set method

## All days
activity_space_plot(observations_multiple_days, c(1:nrow(observations_multiple_days)), 
                    'overall', goal_interval, method='integral_conditional', 
                    save_path_name='sim/pic/more_simulations/activity_space/activity_space_alldays.pdf')

## Weekdays
activity_space_plot(observations_multiple_days_weekdays, c(1:nrow(observations_multiple_days_weekdays)), 
                    'weekdays', goal_interval, method='integral_conditional', 
                    save_path_name='sim/pic/more_simulations/activity_space/activity_space_weekdays.pdf')

## Weekends
activity_space_plot(observations_multiple_days_weekends, c(1:nrow(observations_multiple_days_weekends)), 
                    'weekends', goal_interval, method='integral_conditional', 
                    save_path_name='sim/pic/more_simulations/activity_space/activity_space_weekends.pdf')
}

##-------------------- Figure 13 and 14 generation (Clustering)-----------------------
###--------------------Preparation---------------------------------------------------
{
## Find each day density
# Define the time interval for KDE estimation
t1 = goal_interval[1]
t2 = goal_interval[2]

# Generate a sequence of time points for density estimation
t_range_need = seq(t1, t2, length.out = 99)

# Compute density estimates for each individual day in the dataset
all_res_single_day_all = single_day_densities(observations_multiple_days, grid_center_need, 
                                              goal_interval, method = 'integral_conditional', 
                                              t_range_need = t_range_need)

### Cluster (Spectral clustering)
# Cluster pattern for weekends (patterns 3, 4, and 5)
clusters_weekends_res = cluster_pattern(all_res_single_day_all, pattern_realizations_list, c(3,4,5),eps=0.0001)

# Cluster pattern for weekdays (patterns 1 and 2)
clusters_weekdays_res = cluster_pattern(all_res_single_day_all, pattern_realizations_list, c(1,2),eps=0.0001)
}

###-------------------- Figure 13 generation (Distance matrix)---------------------------------------
{
# Plot heatmap of the affinity matrix ordered by spectral clustering for weekdays
plot_heatmap(clusters_weekdays_res, 
             "Distance Matrix Ordered by Hierarchical Clustering (Weekdays)", 
             save_path_name = 'sim/pic/more_simulations/clustering/affinity_matrix_weekdays.pdf')


# Plot heatmap of the affinity matrix ordered by spectral clustering for weekends
plot_heatmap(clusters_weekends_res, 
             "Distance Matrix Ordered by Hierarchical Clustering (Weekends)", 
             save_path_name = 'sim/pic/more_simulations/clustering/affinity_matrix_weekends.pdf')
# You can run plot_heatmap_help() for more information about the function plot_heatmap.
}
###-------------------- Figure 14 generation (hierarchical clustering)-----------------------
#pattern_realizations_list[[2]]
## Figure 14
{
# Dendrogram for weekdays (single linkage clustering)
cluster_plot(clusters_weekdays_res, abline_h = 4500, loc = c(22,55), 
             pattern_chosen = c(1,2), 
             title = "Dendrogram (single linkage), weekdays", 
             save_path_name = 'sim/pic/more_simulations/clustering/dendrogram_weekdays.pdf')

# Dendrogram for weekends (single linkage clustering)
cluster_plot(clusters_weekends_res, abline_h = 2750, loc = c(18,2,8), 
             pattern_chosen = c(3,4,5), 
             title = "Dendrogram (single linkage), weekends", 
             save_path_name = 'sim/pic/more_simulations/clustering/dendrogram_weekends.pdf')
}

##-------------------- Figure 15 and 16 generation (Conditional densities)-----------------------
{
### Estimation of f(x|t) for each clustered days set
# Define the time points at which to estimate density for different clusters
t_range_need_condi = c(6/24, 9/24, 12/24, 15/24, 18/24, 21/24)

# Compute density estimates for different clusters at specific time points
all_res_single_cluster_all_f = cluster_densities(observations_multiple_days, pattern_realizations_list, 
                                                 grid_center_need, pattern_vec = c(1:5), 
                                                 t_range_need_condi = t_range_need_condi, 
                                                 method = 'integral_conditional')

### Plot estimated conditional density (in log form)

# Define time labels for plotting and filenames
t_range_text = c("6:00 AM", "9:00 AM", "12:00 PM", "3:00 PM", "6:00 PM", "9:00 PM")
file_time_text = c("6AM", "9AM", "12PM", "3PM", "6PM", "9PM")
}
{
# Generate conditional density plots for each cluster at different times
for (cluster_id in 1:5) {
  
  # Compute the min-max range for consistent color scaling
  min_max_range = range_for_plot(all_res_single_cluster_all_f[[cluster_id]])
  
  for (time_id in 1:length(t_range_text)) {
    # Generate and save the conditional density plot
    conditional_density_plot(all_res_single_cluster_all_f, cluster_id, 
                             grid_center_need, time_id, t_range_text, 
                             min_max_range,title=TeX(paste0("$\\log(\\hat{f}_{c}(x|t)+\\xi)$, cluster ", as.character(cluster_id))),
                             save_path_name = paste0('sim/pic/more_simulations/conditional_density/cluster_', 
                                                     cluster_id, '_', file_time_text[time_id], '.pdf'))
  }
}
}


###-------------------- Figure 17 generation (Centers)-----------------------
{
# Compute cluster centers at different times for each movement pattern
cluster_center_list = cluster_centers(observations_multiple_days, pattern_realizations_list, 
                                      grid_center_need, pattern_vec = c(1:5), 
                                      t_range_need_condi = t_range_need_condi)

# Extract x and y coordinates of cluster centers
all_res_single_cluster_all_x = cluster_center_list$center_x_list
all_res_single_cluster_all_y = cluster_center_list$center_y_list

### Plot the centers for each clusters
# Define time labels for plotting and filenames
t_range_need_condi = c(6/24, 9/24, 12/24, 15/24, 18/24, 21/24)
t_range_text = c("6:00 AM", "9:00 AM", "12:00 PM", "3:00 PM", "6:00 PM", "9:00 PM")

# Generate centroid plots for all movement clusters and historical observations
for (cluster_no in c(0:5)) {
  dense_centroid_plot(all_res_single_cluster_all_x, all_res_single_cluster_all_y, 
                      cluster_no, t_range_text, observations_multiple_days, 
                      t_range_need_condi, 
                      save_path_name = paste0('sim/pic/more_simulations/centers/cluster_', cluster_no, '.pdf'))
}
}
