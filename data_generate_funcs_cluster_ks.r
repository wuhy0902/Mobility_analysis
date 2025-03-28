library(dplyr)

anchor_map = function(anchors_mat,linking_info=NULL){
  SpatialPointsDataFrame(SpatialPoints(anchors_mat),linking_info)
}

segments_map = function(segments_mat, map_name){
  get_line=function(id) Line(segments_mat[segments_mat[,1]==id,c(2,3)])
  all_segs = lapply(unique(segments_mat[,1]),get_line)
  Lines(all_segs,ID=map_name)
}

routes_summarize = function(line_seg, routes_table){
  splice = function(segments_in_route, direction_in_route, line_segs){
    for (j in 1:length(segments_in_route)){
      seg_index_in_route = segments_in_route[j]
      seg_mat_in_route = line_seg %>% filter(id==seg_index_in_route)
      if (direction_in_route[j]==-1) seg_mat_in_route = seg_mat_in_route[c(2,1),]
      if (j==1) {
        binding_seg_mat = seg_mat_in_route[,c(2,3)]
      }else{
        binding_seg_mat = rbind(binding_seg_mat, seg_mat_in_route[-1,c(2,3)])
      }
    }
    Line(binding_seg_mat)
  }
  
  routes_no_vec = unique(routes_table$route_no)
  routes_comment_vec = unique(routes_table$comment)
  routes = list()
  
  for (i in routes_no_vec){
    segments_in_route = routes_table[routes_table$route_no==i,]$segments
    direction_in_route = routes_table[routes_table$route_no==i,]$direction
    routes[[i]] = splice(segments_in_route, direction_in_route, line_segs)
  }
  routes
}

pattern_info_for_person = function(pattern_pool, pattern_prob, person_name){
  setClass("pattern_info",slots = list(pattern_pools="list", pattern_prob="vector", person_name="character"))
  get_pattern=function(id_single){
    pattern_pool %>% filter(id==id_single)
  }
  new("pattern_info", pattern_pools=lapply(unique(pattern_pool$id),get_pattern),pattern_prob=pattern_prob, person_name=person_name)
}

pattern_realization_generate = function(pattern_infos,days){
  every_day_pattern = sample(1:length(pattern_infos@pattern_prob),days,prob=pattern_infos@pattern_prob,replace = T)
  add_jitter = function(distribution_name, distribution_para, bound_para, duration){
    if (distribution_name=='Trun_Gaussian'){
      #rnorm(1,0, distribution_para)
      rnormTrunc(1, mean = 0, sd = distribution_para, min = -bound_para, max = bound_para)
    }else if(distribution_name=='uniform'){
      runif(1,-bound_para/2,bound_para/2)
    }
  }
  
  adjust_pattern = function(pattern_id){
    pattern_at_that_day = pattern_infos@pattern_pools[[pattern_id]]
    pattern_at_that_day_realiza = data.frame(matrix(NA,nrow=nrow(pattern_at_that_day),ncol=8))
    colnames(pattern_at_that_day_realiza) = c('id','position','location','tstart','duration','next_position','next_location','comment')
    pattern_at_that_day_realiza$id = 1:nrow(pattern_at_that_day_realiza)
    pattern_at_that_day_realiza$position = pattern_at_that_day$position
    pattern_at_that_day_realiza$location = pattern_at_that_day$location
    pattern_at_that_day_realiza$next_position = pattern_at_that_day$next_position
    pattern_at_that_day_realiza$next_location = pattern_at_that_day$next_location
    pattern_at_that_day_realiza$comment = pattern_at_that_day$comment
    pattern_at_that_day_realiza$tstart[1] = pattern_at_that_day$tstart_center[1]
    
    if (nrow(pattern_at_that_day)==1){
      pattern_at_that_day_realiza$duration[1] = pattern_at_that_day$duration_center[1]
    }else{
      pattern_at_that_day_realiza$duration[1] = pattern_at_that_day$duration_center[1]+add_jitter(pattern_at_that_day$duration_dist[1],pattern_at_that_day$duration_dist_para[1], pattern_at_that_day$duration_bound[1])
      
      for (i in 2:nrow(pattern_at_that_day)){
        #'id', 'position','location','tstart','duration','next_position','next_location','comment'
        pattern_at_that_day_realiza$tstart[i]=pattern_at_that_day_realiza$tstart[(i-1)]+pattern_at_that_day_realiza$duration[(i-1)]
        if (i!=nrow(pattern_at_that_day)){
          
          pattern_at_that_day_realiza$duration[i] = pattern_at_that_day$duration_center[i]+add_jitter(pattern_at_that_day$duration_dist[i],pattern_at_that_day$duration_dist_para[i], pattern_at_that_day$duration_bound[i]) 
        }else{
          pattern_at_that_day_realiza$duration[i]=24-pattern_at_that_day_realiza$tstart[i]
        }
      }
    }
    pattern_at_that_day_realiza[,c('tstart','duration')] = pattern_at_that_day_realiza[,c('tstart','duration')]/24
    pattern_at_that_day_realiza
  }
  all_day_patterns = lapply(every_day_pattern, adjust_pattern)
  list(all_day_patterns,every_day_pattern)
}

file_determine = function(folder_path,time_start,time_end){
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a variable to track the minimum number of rows for any day
  day_num <- -1
  if ((time_end-time_start)>0.5){
    thres_count = 15
  }else{
thres_count = 10
  }
  # Use a while loop to repeatedly select a file until we find a valid one
  while (day_num < (thres_count-1)) {
    # 2. Randomly select a file
    selected_file <- sample(files, 1)
    
    # 3. Load the selected CSV file
    data <- read.csv(selected_file)
    
    # 4. Ensure that the relevant columns exist
    if (!all(c("Date", "Time", "PosSrc") %in% colnames(data))) {
      stop("The required columns (Date, Time, PosSrc) are not present in the file.")
    }
    
    # 5. Convert Date column to Date type if needed
    # data$Date <- as.Date(data$Date, format="%A, %B %d, %Y")
    
    # 6. Check the minimum number of rows for any day where PosSrc = 'GPS'
  
daily_counts <- data %>%
  filter(PosSrc == "GPS") %>%
  mutate(
    Time_24h = parse_date_time(Time, orders = "I:M:S p"),  # Convert to full datetime format
    Time_numeric = (hour(Time_24h) + minute(Time_24h) / 60 + second(Time_24h) / 3600) / 24  # Convert to fraction of a day
  ) %>%
  filter(Time_numeric >= time_start & Time_numeric <= time_end) %>%  # Apply time filter
  group_by(Date) %>%
  summarise(row_count = n())

# Set day_num as the maximum row count across all days
day_num <- ifelse(nrow(daily_counts) == 0, 0, max(daily_counts$row_count))
    
    
    #print(day_num)
    # If day_num >= 10, the file is valid and the while loop will terminate
  }
  
  dates_with_high_counts <- daily_counts %>%
    filter(row_count > thres_count) %>%
    pull(Date) 
  print(selected_file)
  return(list(data=data,dates_to_choose=dates_with_high_counts))
  
}

generate_new_values <- function(n, density_estimate) {
  # Sample with inverse transform sampling
  cum_density <- cumsum(density_estimate$y) / sum(density_estimate$y)
  sampled_values <- approx(cum_density, density_estimate$x, runif(n))$y
  return(sampled_values)
}

random_timestamp_generator_v5 <- function(day_id, data_list, num_of_timestamps,time_start,time_end) {
  # 1. List all CSV files in the folder
  data = data_list$data
  dates_with_high_counts = data_list$dates_to_choose
    # 6. Check the minimum number of rows for any day where PosSrc = 'Cell'
  
  # 7. Once a valid file is found, randomly choose one day
  selected_day <- sample(dates_with_high_counts, 1)
  #print(selected_day)
  # 8. Extract rows where PosSrc = 'Cell' on the selected day
  filtered_data <- subset(data, Date == selected_day & PosSrc == "GPS")
  
  # Extract the 'Time' column
  time_col <- filtered_data$Time
  
  # Convert the time to POSIXct format for easier manipulation
  time_col <- as.POSIXct(time_col, format="%I:%M:%S %p")
  
  # Convert the time to numeric between 0 and 1
  timestamps_ext_vec <- as.numeric(format(time_col, "%H")) / 24 +
                       as.numeric(format(time_col, "%M")) / (24 * 60) +
                       as.numeric(format(time_col, "%S")) / (24 * 3600)

density_estimate <- density(timestamps_ext_vec, from = 0, to = 1, n=2048)

# Function to sample new values from the estimated density

# Generate 1000 new values
count_in_interval = 0
  while (count_in_interval<5){
    timestamps_ext_vec <- generate_new_values(num_of_timestamps, density_estimate)
    count_in_interval = length(which(timestamps_ext_vec>time_start & timestamps_ext_vec<time_end))
    #print(count_in_interval)
  }

    timestamps_ext_vec <- sort(timestamps_ext_vec)
  
  return(timestamps_ext_vec)  # Return the final sorted vector of timestamps
}

# Location generate
ture_locations_generate = function(anchors_map, routes_all, pattern_realizations, frequency_pattern, freq_parameter,time_start,time_end,running_time=1){
  set.seed(running_time+202402)
  if (frequency_pattern=='even_space'){
    f = function(i){
      c(1:freq_parameter)/(freq_parameter+1)
    }
    time_list = lapply(1:length(pattern_realizations), f)
    
  }else if(frequency_pattern=='random_uniform'){
    f = function(i){
      sort(runif(freq_parameter,0,1))
    }
    time_list = lapply(1:length(pattern_realizations), f)
  }else if(frequency_pattern=='distance_based'){
    f = function(i){
      c(1:86400)/(86400+1)
    }
    time_list = lapply(1:length(pattern_realizations), f)
  }else if(frequency_pattern=='irregular'){
    folder_path = paste0(getwd(),"/timestamp_irregular_sample")
    #folder_path <- here("timestamp_irregular_sample")
    #folder_path = "C:/Users/why25/Dropbox/UW/research/With Yen-Chi/meeting preparation/20240323/Adjusted_code/debug/wave_data"
    data_list = file_determine(folder_path,time_start,time_end)
    
    time_list = lapply(1:length(pattern_realizations),random_timestamp_generator_v5, data_list, freq_parameter,time_start,time_end)
	temp_vec <- data.frame(Value = unlist(time_list))
  }else{
    time_list = freq_parameter
  }
  
  relative_location_generate = function(id){
    
    pattern_at_one_day = pattern_realizations[[id]]
    quantile_fun = function(timestamp){
      location_index = length(which(pattern_at_one_day$tstart<timestamp))
      location_rate = (timestamp-pattern_at_one_day$tstart[location_index])/pattern_at_one_day$duration[location_index]
      c(location_index, location_rate)
    }
    
    location_indices = lapply(time_list[[id]],quantile_fun)
    matrix(unlist(location_indices), nrow = length(time_list[[id]]), byrow = TRUE)
    # it is the index in the pattern table instead of the location number of anchor points or routes
  }
  
  all_locations_relative = lapply(1:length(pattern_realizations), relative_location_generate)
  coor_location_generate = function(id){
    location_at_one_day = all_locations_relative[[id]]
    pattern_at_one_day = pattern_realizations[[id]]
    find_location = function(state_id){
      state = pattern_at_one_day[pattern_at_one_day$id==state_id,]
      if(length(which(location_at_one_day[,1]==state_id))==1){
        observations_for_state <- matrix(location_at_one_day[which(location_at_one_day[,1]==state_id),],nrow=1)
        nrow_indicator = 1
      }else if(length(which(location_at_one_day[,1]==state_id))>1){
        observations_for_state <- location_at_one_day[which(location_at_one_day[,1]==state_id),]
        nrow_indicator = 1
      }else{
        nrow_indicator = 0
      }
      
      if (state$position==0 && nrow_indicator>0){# anchor point
        df = data.frame(matrix(0,nrow = nrow(observations_for_state),ncol=3))
        colnames(df) = c('x', 'y', 'comment')
        df[,c('x','y')] = matrix(rep(as.vector(coordinates(anchors_map)[state$location[1],]),nrow(observations_for_state)),ncol=2,byrow = T)
        df$comment = rep(state$comment[1],nrow(df))
        df$state_index = rep(state_id,nrow(df))
        df
      }else if(state$position==1 && nrow_indicator>0){
        coor_road = coordinates(routes_all[[state$location[1]]])
        weight_of_each_seg_raw = sqrt(rowSums((coor_road[2:nrow(coor_road),]-coor_road[1:(nrow(coor_road)-1),])^2))
        weight_of_each_seg = c(0,as.vector(cumsum(weight_of_each_seg_raw/sum(weight_of_each_seg_raw))))
        correspond_to_road_coor = function(location_rate){
          segment_index = length(which(weight_of_each_seg<location_rate))
          (coor_road[(segment_index+1),]-coor_road[segment_index,])*(location_rate-weight_of_each_seg[(segment_index)])/(weight_of_each_seg[(segment_index+1)]-weight_of_each_seg[(segment_index)])+coor_road[(segment_index),]
        }
        coor_lists = lapply(observations_for_state[,2],correspond_to_road_coor)
        df = data.frame(matrix(0,nrow = nrow(observations_for_state),ncol=3))
        colnames(df) = c('x', 'y', 'comment')
        df[,c('x','y')] = matrix(unlist(coor_lists), nrow = length(coor_lists), byrow = TRUE)
        df$comment = rep(state$comment[1],nrow(df))
        df$state_index = rep(state_id,nrow(df))
        df
      }else{
        df = data.frame(matrix(0,ncol = 4, nrow = 0))
        colnames(df) = c('x', 'y', 'comment','state_index')
        df
      }
    }
    df_one_day = do.call(rbind, lapply(1:nrow(pattern_at_one_day),find_location))
    df_one_day$timestamp = time_list[[id]]
    df_one_day$day = rep(id,nrow(df_one_day))
    if (frequency_pattern=='distance_based'){
      recorded_indices = c(1)
      last_recorded_index = 1
      for (index_df_one_day in 2:nrow(df_one_day)){
        distance_bet = sqrt(sum((df_one_day[index_df_one_day,c('x','y')]-df_one_day[last_recorded_index,c('x','y')])^2))
        if (distance_bet>freq_parameter){
          last_recorded_index = index_df_one_day
          recorded_indices = append(recorded_indices, last_recorded_index)
        }
      }
      df_one_day[recorded_indices,]
    }else{
      df_one_day
    }
  }
  
  lapply(1:length(pattern_realizations),coor_location_generate)
  
}

observations_generate = function(true_locations_multiple_days, measurement_error){
  one_day_observation_generate = function(id){
    true_locations_one_day = true_locations_multiple_days[[id]]
    true_locations_one_day[,c('x','y')] = true_locations_one_day[,c('x','y')] + matrix(rnorm(2*nrow(true_locations_one_day),0,measurement_error),ncol=2)
    true_locations_one_day
  }
  lapply(1:length(true_locations_multiple_days),one_day_observation_generate)
}

observations_generate1 = function(true_locations_multiple_days, measurement_error,more_points){
  one_day_observation_generate = function(id){
    true_locations_one_day = true_locations_multiple_days[[id]]
    f_copy = function(id2,vec_use){vec_use}
    true_locations_one_day = do.call(rbind, lapply(1:more_points, f_copy,true_locations_one_day))
    true_locations_one_day[,c('x','y')] = true_locations_one_day[,c('x','y')] + matrix(rnorm(2*nrow(true_locations_one_day),0,measurement_error),ncol=2)
    true_locations_one_day
  }
  lapply(1:length(true_locations_multiple_days),one_day_observation_generate)
}

data_generation = function(days = 28,
                           frequency_pattern = 'even_space',
                           freq_parameter = 8639,
                           measurement_error = 0.1,goal_interval=c(0,0.99999),running_time){
  # anchor points
  anchors_df_path <- paste0(getwd(),"/data_to_read/anchors.csv")
  anchors_df = read.csv(anchors_df_path)

  anchors_mat = as.matrix(anchors_df[,c('x','y')])
  link_info = data.frame(anchor_points_name=anchors_df$anchor_points_name)
  anchors_map = anchor_map(anchors_mat,link_info)
  
  # line segments
  line_seg_path <-paste0(getwd(),"/data_to_read/segments.csv")
  line_seg = read.csv(line_seg_path)

  intrinsic_map = segments_map(line_seg,'example_map')
  
  # Draw picture
  minx = min(c(anchors_df[,c('x')], line_seg[,c('X')]))
  maxx = max(c(anchors_df[,c('x')], line_seg[,c('X')]))
  miny = min(c(anchors_df[,c('y')], line_seg[,c('Y')]))
  maxy = max(c(anchors_df[,c('y')], line_seg[,c('Y')]))
  minx = minx-abs(minx)*0.1
  miny = miny-abs(miny)*0.1
  maxx = maxx+abs(maxx)*0.1
  maxy = maxy+abs(maxy)*0.1
  # Routes
  routes_table_path <-paste0(getwd(),"/data_to_read/routes.csv")
  routes_table = read.csv(routes_table_path)

  routes_all = routes_summarize(line_seg, routes_table)
  
  # Pattern
  pattern_pool_path <- paste0(getwd(),"/data_to_read/patterns.csv")
  pattern_pool = read.csv(pattern_pool_path)
  
  pattern_prob_path <- paste0(getwd(),"/data_to_read/pattern probability.csv")
  pattern_prob_df = read.csv(pattern_prob_path)
  
  pattern_prob = pattern_prob_df$prob
  person_name = 'person_name'
  pattern_infos = pattern_info_for_person(pattern_pool, pattern_prob, person_name)
  
  # Pattern realization
  pattern_realizations_list = pattern_realization_generate(pattern_infos,days)
  pattern_realizations = pattern_realizations_list[[1]]
  
  # True location generate
  true_locations_multiple_days = ture_locations_generate(anchors_map, routes_all, pattern_realizations, frequency_pattern, freq_parameter,goal_interval[1],goal_interval[2],running_time)
    # Add measurement
  observations_multiple_days_list = observations_generate(true_locations_multiple_days, measurement_error)
  
  observations_multiple_days = do.call(rbind, observations_multiple_days_list)
  #write.csv(observations_multiple_days, output_file_name)
  
  return(list(pattern_realizations_list,observations_multiple_days,c(minx,miny,maxx,maxy)))
}


ref_distri = function(days = 100,
                           freq_parameter = 1439,
                           measurement_error = 0.1,
running_amount=100,distri_true_need='yes',time_interval=c(0,1),grid_cell_size=0.2,way_of_ref='cell_obs_count'){
  # anchor points
  anchors_df_path <- paste0(getwd(),"/data_to_read/anchors.csv")
  anchors_df = read.csv(anchors_df_path)
  anchors_mat = as.matrix(anchors_df[,c('x','y')])
  
  link_info = data.frame(anchor_points_name=anchors_df$anchor_points_name)
  anchors_map = anchor_map(anchors_mat,link_info)
  
  # line segments
  line_seg_path <- paste0(getwd(),"/data_to_read/segments.csv")
  line_seg = read.csv(line_seg_path)

  intrinsic_map = segments_map(line_seg,'example_map')
  
  # Draw picture
  minx = min(c(anchors_df[,c('x')], line_seg[,c('X')]))
  maxx = max(c(anchors_df[,c('x')], line_seg[,c('X')]))
  miny = min(c(anchors_df[,c('y')], line_seg[,c('Y')]))
  maxy = max(c(anchors_df[,c('y')], line_seg[,c('Y')]))
  minx = minx-abs(minx)*0.1
  miny = miny-abs(miny)*0.1
  maxx = maxx+abs(maxx)*0.1
  maxy = maxy+abs(maxy)*0.1
  # Routes
  routes_table_path <- paste0(getwd(),"/data_to_read/routes.csv")
  routes_table = read.csv(routes_table_path)
  routes_all = routes_summarize(line_seg, routes_table)
  
  # Pattern
  pattern_pool_path <- paste0(getwd(),"/data_to_read/patterns.csv")
  pattern_pool = read.csv(pattern_pool_path)
  
  pattern_prob_path <- paste0(getwd(),"/data_to_read/pattern probability.csv")
  pattern_prob_df = read.csv(pattern_prob_path)
  
  pattern_prob = pattern_prob_df$prob
  person_name = 'person_name'
  pattern_infos = pattern_info_for_person(pattern_pool, pattern_prob, person_name)
  
  obs_generate_one_time = function(running_time,time_interval){
    # Pattern realization
    pattern_realizations_list = pattern_realization_generate(pattern_infos,days)
    pattern_realizations = pattern_realizations_list[[1]]
    
    # True location generate
    true_locations_multiple_days = ture_locations_generate(anchors_map, routes_all, pattern_realizations, 'even_space', freq_parameter,running_time)
    # Add measurement
    observations_multiple_days_list = observations_generate(true_locations_multiple_days, measurement_error)
    
    observations_multiple_days_ref = do.call(rbind, observations_multiple_days_list)

    indices_in_interval = which(observations_multiple_days_ref$timestamp>time_interval[1] & observations_multiple_days_ref$timestamp<time_interval[2])
    
    observations_multiple_days_true = as.matrix(do.call(rbind, true_locations_multiple_days)[,c(1,2)])

    return(
      list(
        observations_multiple_days_ref = observations_multiple_days_ref[indices_in_interval,c(1,2)],
        observations_multiple_days_true = observations_multiple_days_true[indices_in_interval,]
    )
    )
  }
  
# Number of grid cells in x and y directions:
n_x <- round((maxx-minx)/grid_cell_size)
n_y <- round((maxy-miny)/grid_cell_size)

# 1) Create sequences of 'edge' coordinates
x_edges <- seq(minx, maxx, length.out = n_x + 1)
y_edges <- seq(minx, maxy, length.out = n_y + 1)

# 2) Compute the midpoints of consecutive edges to get center coordinates
x_centers <- 0.5 * (x_edges[-1] + x_edges[-length(x_edges)])
y_centers <- 0.5 * (y_edges[-1] + y_edges[-length(y_edges)])

# 3) Create all combinations of (x_center, y_center) using expand.grid
grid_centers <- as.matrix(expand.grid(x_centers, y_centers))

sfc <- st_sfc(
  st_polygon(
    list(
      rbind(
        c(minx, miny),
        c(minx, maxy),
        c(maxx, maxy),
        c(maxx, miny),
        c(minx, miny)
      )
    )
  )
)

dx <- x_edges[2] - x_edges[1]
dy <- y_edges[2] - y_edges[1]

get_count_obs_each_cell = function(grid_centers, observations_multiple_days_ref,x_edges,y_edges,n_x,n_y){
  colnames(grid_centers) <- c("x_center", "y_center")
  obs_df <- data.frame(
    obs_x = observations_multiple_days_ref[, 1],
    obs_y = observations_multiple_days_ref[, 2]
  )

obs_df$bin_x <- findInterval(obs_df$obs_x, x_edges)
obs_df$bin_y <- findInterval(obs_df$obs_y, y_edges)

obs_df <- subset(obs_df,
  bin_x >= 1 & bin_x <= n_x &
  bin_y >= 1 & bin_y <= n_y
)

obs_df$cell_id <- obs_df$bin_x + (obs_df$bin_y - 1)*n_x

counts <- obs_df %>%
  group_by(cell_id) %>%
  summarize(n_points = n())

all_cells <- data.frame(cell_id = seq_len(n_x * n_y))

all_cells <- left_join(all_cells, counts, by = "cell_id")
all_cells$n_points[is.na(all_cells$n_points)] <- 0
return(all_cells$n_points)
}

make_cell_polygon <- function(cx, cy, halfx, halfy) {
  # corners: (cx ± halfx, cy ± halfy)
  coords <- rbind(
    c(cx - halfx, cy - halfy),
    c(cx - halfx, cy + halfy),
    c(cx + halfx, cy + halfy),
    c(cx + halfx, cy - halfy),
    c(cx - halfx, cy - halfy)  # close the polygon
  )
  st_polygon(list(coords))
}
grid_polys <- apply(grid_centers, 1, function(pt) {
  cx <- pt[1]
  cy <- pt[2]
  make_cell_polygon(cx, cy, halfx = dx/2, halfy = dy/2)
})
grid_sfc <- st_sfc(grid_polys, crs = st_crs(sfc))
grid_need <- st_as_sf(grid_sfc) %>%
  mutate(id = row_number())


observations = obs_generate_one_time(1,time_interval)
observations_multiple_days_ref = observations$observations_multiple_days_ref
if (way_of_ref=='kde'){
  distri_ref = kde(observations_multiple_days_ref, eval.points = grid_centers)$estimate
  if (distri_true_need=='yes'){
      observations_multiple_days_true  = observations$observations_multiple_days_true 
      distri_ref_true = kde(observations_multiple_days_true, eval.points = grid_centers)$estimate
  }
}else{
  distri_ref = get_count_obs_each_cell(grid_centers, observations_multiple_days_ref,x_edges,y_edges,n_x,n_y)
  if (distri_true_need=='yes'){
      observations_multiple_days_true  = observations$observations_multiple_days_true 
      distri_ref_true = get_count_obs_each_cell(grid_centers, observations_multiple_days_true,x_edges,y_edges,n_x,n_y)
  }
}


  for (simu_time in 2:running_amount){
    observations= obs_generate_one_time(simu_time,time_interval)
    observations_multiple_days_ref = observations$observations_multiple_days_ref 
    if (way_of_ref=='kde'){
        distri_ref = distri_ref + kde(observations_multiple_days_ref, eval.points = grid_centers)$estimate
    }else{
      distri_ref = distri_ref + get_count_obs_each_cell(grid_centers, observations_multiple_days_ref,x_edges,y_edges,n_x,n_y)
    }
    if (distri_true_need=='yes'){
        observations_multiple_days_true  = observations$observations_multiple_days_true 
        if (way_of_ref=='kde'){
          distri_ref_true = distri_ref_true + kde(observations_multiple_days_true, eval.points = grid_centers)$estimate
        }else{
          distri_ref_true = distri_ref_true + get_count_obs_each_cell(grid_centers, observations_multiple_days_true,x_edges,y_edges,n_x,n_y)
        }
    }
      if (simu_time %% 10==0){
        print(paste0('days:', (simu_time-10)*days+1, ' to ', simu_time*days, ' simulated.'))
    }
}
  distri_ref = (distri_ref/sum(distri_ref))/(dx*dy)

  
   if (distri_true_need=='yes'){
    return(list(distri_ref=distri_ref,distri_ref_true=distri_ref_true,grid_centers=grid_centers,grid_need = grid_need))
   }else{
     return(list(distri_ref=distri_ref,grid_centers=grid_centers,grid_need = grid_need))
   }
}

esti_error = function(true_den, esti_den, grid_center){
    edge1 = diff(unique(grid_center_need[,1]))[1]
    edge2 = diff(unique(grid_center_need[,2]))[1]

    return(sum((true_den- esti_den)^2)*(edge1*edge2))
}


convert_to_time <- function(x) {
  if (x < 0 || x > 1) {
    stop("Input must be between 0 and 1")
  }
  
  # Convert fraction to seconds in a day
  total_seconds <- round(x * 24 * 60 * 60)
  
  # Get hours, minutes, and seconds
  hours <- (total_seconds %/% 3600) %% 24
  minutes <- (total_seconds %% 3600) %/% 60
  seconds <- total_seconds %% 60
  
  # Determine AM/PM
  period <- ifelse(hours < 12, "AM", "PM")
  
  # Convert to 12-hour format
  display_hours <- ifelse(hours == 0, 12, ifelse(hours > 12, hours - 12, hours))
  
  # Format the time string
  time_string <- sprintf("%02d:%02d:%02d %s", display_hours, minutes, seconds, period)
  
  return(time_string)
}
