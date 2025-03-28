
anchor_map = function(anchors_mat,linking_info=NULL){
  SpatialPointsDataFrame(SpatialPoints(anchors_mat),linking_info)
}

picture_entity_obtain = function(){
    anchors_df_path <- paste0(getwd(),"/data_to_read/anchors.csv")
  anchors_df = read.csv(anchors_df_path)
  anchors_mat = as.matrix(anchors_df[,c('x','y')])
  
  link_info = data.frame(anchor_points_name=anchors_df$anchor_points_name)
  anchors_map2 = anchor_map(anchors_mat,link_info)
  
  # line segments
  line_seg_path <- paste0(getwd(),"/data_to_read/segments.csv")
  line_seg = read.csv(line_seg_path)
  return(list(anchors_map2 = anchors_map2, line_seg=line_seg))
}

#' Generate a Map with Anchor Points and Line Segments
#'
#' This function creates a map with anchor points and line segments.
#' It provides an option to save the plot as a PDF.
#'
#' @param save_path_name (string) The file path to save the plot. 
#'        If not provided or set to 'No', the plot is displayed but not saved.
#' @return A plotted map with anchor points and segments.
#'         Optionally saves the plot as a PDF if a valid file path is provided.
#' @examples
#' plot_map()  # Display the plot without saving
#' plot_map(save_path_name = "pic/basic/map/map.pdf")  # Save the plot to a file
plot_map = function(save_path_name = 'No') {
  # Set margin sizes to minimize white space around the plot
  par(mar = c(1, 1, 1, 1))
  
  # Obtain entities required for plotting
  entities_for_plot = picture_entity_obtain()
  anchors_map2 = entities_for_plot$anchors_map2  # Anchor points for the map
  line_seg = entities_for_plot$line_seg          # Line segments for the map
  
  # Plot the anchor points
  plot(
    anchors_map2, pch = 17, cex = 1.5, col = 'blue', 
    ylim = c(-15, 15), xlim = c(-15, 15),  # Set axis limits
    xlab = "", ylab = "", xaxt = "n", yaxt = "n"  # Remove axis labels and ticks
  )
  
  # Add line segments to the plot using SpatialLines
  lines(SpatialLines(list(segments_map(line_seg, 'example_map'))))
  
  # Add a legend to indicate what the blue triangles represent
  legend(
    "right",                     # Position of the legend
    legend = "Anchor Points",     # Label for the legend
    pch = 17, col = "blue",       # Match point shape & color
    pt.cex = 1, cex = 1,          # Adjust symbol & text sizes
    bty = "o"                     # 'o' adds a surrounding box
  )
  
  # Save the plot as a PDF if a valid file path is provided
  if (save_path_name != 'No') {
    dev.copy(pdf, save_path_name)  # Copy the current plot to a PDF file
    dev.off()                      # Close the PDF device
  }
}

plot_fun=function(id,observations_multiple_days_video, anchors_map2,line_seg){
  time_string = paste0('Day ',observations_multiple_days_video$day[id], '-',convert_to_time(observations_multiple_days_video$timestamp[id]))
  comment_for_id = observations_multiple_days_video$comment[id]

plot(anchors_map2, pch = 17, cex = 0.5, col='blue', ylim = c(-15, 15), xlim = c(-15, 15),
     main = paste0(time_string, "\n", comment_for_id),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")

lines(SpatialLines(list(segments_map(line_seg, 'example_map'))))
points(observations_multiple_days_video[id, 'x'], 
       observations_multiple_days_video[id, 'y'], 
       col = 'red', pch = 19, cex = 0.75)

}

video_generate = function(observations_multiple_days_video, video_name='obs_animation.mp4'){ 
  library(av)
  
  entities_for_plot = picture_entity_obtain()
  anchors_map2 = entities_for_plot$anchors_map2
  line_seg = entities_for_plot$line_seg

  num_frames = nrow(observations_multiple_days_video) # Total number of frames
  
  # Initialize the progress bar
  pb = txtProgressBar(min = 0, max = num_frames, style = 3)

  # Create PNG images
  png("input%07d.png", width = 480, height = 480, res = 108)
  for(i in 1:num_frames){
    plot_fun(i, observations_multiple_days_video, anchors_map2, line_seg)
    setTxtProgressBar(pb, i)  # Update progress bar
  }
  dev.off()
  close(pb)  # Close the progress bar
  
  # Encode video
  png_files <- sprintf("input%07d.png", 1:num_frames)
  av::av_encode_video(png_files, video_name, framerate = 12)
  utils::browseURL(video_name)
}


range_for_plot = function(density_list,eps=0.001){
  max_value = -Inf
  min_value = Inf
  for (time_id in 1:length(density_list)){
    z_display = log(density_list[[time_id]]+eps)
    min_value = min(min_value, min(z_display))
    max_value = max(max_value, max(z_display))
  }
  return(c(min_value,max_value))
}

density_plot = function(density_to_plot, eps, grid_center_need, title, mtext_in_plot, min_max_range,save_path_name='No'){ 
  # Ensure the fields package is loaded for image.plot()

 # Increase right margin space for the color bar
  par(mar = c(5, 5, 3, 5))  
  z_display = log(density_to_plot+ eps)
  nrow_need = length(unique(grid_center_need[,1]))
  ncol_need = length(unique(grid_center_need[,2]))
  z_display = matrix(z_display, nrow = nrow_need , ncol = ncol_need)

  n_lv = 10
  z_lv = seq(min(z_display), max(z_display), length.out = n_lv)
  Mycolor = colorRampPalette(c("white", "orange", "purple"))

  contour(x = unique(grid_center_need[,1]), 
          y = unique(grid_center_need[,2]),
          z = z_display,
          nlevels = n_lv, cex.axis = 1.5,
          main = title,
          cex.main = 1.75)

  .filled.contour(x = unique(grid_center_need[,1]), 
                  y = unique(grid_center_need[,2]),
                  z = z_display,
                  levels = z_lv, col = Mycolor(n_lv))
if (mtext_in_plot!=''){
  mtext(mtext_in_plot, side = 3, at = -2.5, line = -1.5, cex = 1.5)

}

  # Position the color bar outside (right side)
  image.plot(zlim = c(min_max_range[1], min_max_range[2]), 
             legend.only = TRUE, 
             col = Mycolor(n_lv), 
             horizontal = FALSE, 
             smallplot = c(0.9, 0.93, 0.2, 0.8))  # Adjusted to move it to the right
    
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}


# Function to extract line segment data for ggplot2
segments_map = function(segments_mat, map_name) {
  get_line = function(id) {
    # For each line ID, extract coordinate columns (2, 3)
    Line(segments_mat[segments_mat[,1] == id, c(2,3)])
  }
  all_segs = lapply(unique(segments_mat[,1]), get_line)
  Lines(all_segs, ID = map_name)
}

# A helper function to extract segment data from a 'Lines' object
extract_line_segments_df <- function(lines_obj) {
  # lines_obj@Lines is a list of 'Line' objects
  do.call(rbind, lapply(seq_along(lines_obj@Lines), function(i) {
    coords <- lines_obj@Lines[[i]]@coords
    # Each 'Line' object in sp is basically a matrix of x,y
    # We'll create segments by linking consecutive points
    data.frame(
      x = coords[-nrow(coords), 1],      # Start x
      y = coords[-nrow(coords), 2],      # Start y
      xend = coords[-1, 1],             # End x
      yend = coords[-1, 2]              # End y
    )
  }))
}

anchor_point_detect_plot = function(activity_space_indices,anchors_mat,grid_center_need,title_input,seq_of_extreme,save_path_name='No'){
  minx=seq_of_extreme[1];miny=seq_of_extreme[2];maxx=seq_of_extreme[3];maxy=seq_of_extreme[4];
# Example data (replace these with your actual matrices):
# Suppose 'actual' and 'estimated' are each 5x2 matrices
actual <- anchors_mat
estimated <- grid_center_need[activity_space_indices,]
df <- rbind(
  data.frame(X = actual[,1],   Y = actual[,2],   type = "Actual anchor points"),
  data.frame(X = estimated[,1], Y = estimated[,2], type = "Detected anchor points")
)
# line segments
  line_seg_path <- paste0(getwd(), "/data_to_read/segments.csv")
line_seg <- read.csv(line_seg_path)

# Create a Lines object
intrinsic_map <- segments_map(line_seg, "example_map")

# Convert the sp 'Lines' object to a data frame for ggplot
line_segments_df <- extract_line_segments_df(intrinsic_map)

# Base ggplot scatter plot
print(
    ggplot(df, aes(x = X, y = Y, shape = type, color = type)) +
  
  # 2. Overlay the line segments
  geom_segment(
  data = line_segments_df,
  aes(x = x, y = y, xend = xend, yend = yend),
  color =  "gray90",       # or any color of your choice
  linewidth = 1.2,
  inherit.aes = FALSE    # disable inherited shape/color = type
)+
  # 1. Plot anchor points
  geom_point(size = 4, stroke = 1.5) +
  
  # 3. Shape and color scales
  scale_shape_manual(values = c("Actual anchor points" = 1, 
                                "Detected anchor points" = 4)) +
  scale_color_manual(values = c("Actual anchor points" = "red", 
                                "Detected anchor points" = "blue")) +
  
  # 4. Title, labels, and theming
  labs(
    title = title_input,
    shape = "Point Type", 
    color = "Point Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),          
    axis.ticks = element_blank(),         
    panel.grid = element_blank(),         
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    legend.title = element_blank(),

    # Set legend position to bottom-left
    legend.position = c(0.01, 0.01),   # (0,0) = bottom-left, (1,1) = top-right
    legend.justification = c(0, 0)   # Ensures the legend box starts from bottom-left

  ) +
  
  # 5. Adjust plot limits (if needed)
  coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy))
)
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}



activity_space_plot = function(dat2, idx_select, title_need, goal_interval, time_text=' whole day',method='integral_conditional',save_path_name='No'){

all_w = unlist(lapply(unique(dat2$day),get_weight_single_day,dat2,goal_interval[1],goal_interval[2]))

all_w = all_w/sum(all_w)

dat_AS = dat2[idx_select,]
#marginal_esti_density_weekdays = kde_gps(dat_AS, grid_center_need, 'marginal', measurement_error, t_range_need=NULL)
#gps_int = kde_gps(dat_AS, grid_center_need, 'integral_conditional', measurement_error, t_range_need)
#dat_AS$density  = kde_gps(dat_AS, as.matrix(dat_AS[,c(1,2)]), 'integral_conditional', measurement_error, t_range_need)

  gps_int = kde_gps(dat_AS, grid_center_need, method, t_range_need= t_range_need,time_interval=goal_interval,hs_const=hs_const,ht_const=ht_const)
   dat_AS$density = kde_gps(dat_AS, as.matrix(dat_AS[,c(1,2)]), method, t_range_need= t_range_need,time_interval=goal_interval,hs_const=hs_const,ht_const=ht_const)

dat_AS$weight = all_w
      # updated weight at the current t
  
nrow_need = length(unique(grid_center_need[,1]))
ncol_need = length(unique(grid_center_need[,2]))

    ## computing the prob contents
    as_lv = (1:10000)/10000*max(dat_AS$density)
    z_as = matrix(0, nrow(grid_center_need))
    z_as = as.vector(z_as)
    for(j_lv in 1:length(as_lv)){
      as_tmp = as_lv[j_lv]
      idx_tmp = dat_AS$density<=as_tmp
      grid_tmp = as.vector(gps_int)>as_tmp
      z_as[grid_tmp] = sum(dat_AS$weight[idx_tmp])
    }
    z_as[is.na(z_as)]=0
    z_as[z_as==max(z_as)] = 1
    # force the grid point maximum to be 1 -- it may be lower than 1 due to resolution
    z_as = matrix(z_as, nrow=nrow_need , ncol=ncol_need)
    z_aslv = c(0,0.001,0.01, 0.1,0.3, 0.5,1.1)
      n_lv = 10
      par(mar=c(4,4,2,1))
contour(x=unique(grid_center_need[,1]), y= unique(grid_center_need[,2]),
            z = z_as,cex.axis=1.5,
            nlevels = n_lv, main=paste("Activity space ","(", title_need,")",sep=""), cex.main=2)
    .filled.contour(x=unique(grid_center_need[,1]), y= unique(grid_center_need[,2]),
                    z = z_as,
                    levels = z_aslv,col = c("white","grey","limegreen", "orange","blue","red"))
   legend("topleft", 
       col = c("grey", "limegreen", "orange", "blue", "red"),
       lwd = 4,          # Reduce line width (was 6)
       cex = 1,          # Reduce text size (was 1.5)
       pt.cex = 0.8,     # Reduce legend symbols
       legend = c("99.9%", "99%", "90%", "70%", "50%"),
       bty = "o",        # Use "o" for a small box, or "n" to remove it
       box.lwd = 0.8,    # Reduce box line width
       inset = 0.01)     # Move legend closer to the plot
    mtext(paste("Time: ",time_text),side=3, at=-2.5, line=-1.5, cex=2)
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}


# Get cluster labels
plot_heatmap = function(clusters_res,title,save_path_name='No'){
  similarity_matrix = clusters_res$Lap_graph
  cluster_labels=clusters_res$cluster_result
  sorted_indices <- order(cluster_labels)  # Sort by cluster label
ordered_similarity_matrix <- similarity_matrix[sorted_indices, sorted_indices]

# 1) Prepare your matrix
## 1) Make sure your matrix has row/column names
##
if (is.null(rownames(ordered_similarity_matrix))) {
  rownames(ordered_similarity_matrix) <- paste0("Row", seq_len(nrow(ordered_similarity_matrix)))
}
if (is.null(colnames(ordered_similarity_matrix))) {
  colnames(ordered_similarity_matrix) <- paste0("Col", seq_len(ncol(ordered_similarity_matrix)))
}

mat <- as.matrix(ordered_similarity_matrix)

##
## 2) Define your color scale for numeric data
##    (from blue -> red across the matrix range)
##
n_colors <- 100
max_val  <- max(mat)
my_breaks <- seq(0, max_val, length.out = n_colors + 1)
my_colors <- colorRampPalette(c("blue", "red"))(n_colors)

##
## 3) Build the cluster vector, then drop unused levels
##
clust_vec <- cluster_labels[sorted_indices]
# Convert to factor if not already:
clust_vec <- factor(clust_vec)
# Drop unused levels:
clust_vec <- droplevels(clust_vec)

# Ensure length matches nrow(mat) if you annotate rows (and ncol if columns).
if (length(clust_vec) != nrow(mat)) {
  stop("Mismatch: 'clust_vec' length != nrow(mat). Adjust or remove annotation.")
}

##
## 4) Prepare row/column annotation
##    (for a square matrix, you can reuse the same vector for columns)
##
row_anno <- data.frame(Cluster = clust_vec)
rownames(row_anno) <- rownames(mat)

col_anno <- NULL
if (ncol(mat) == length(clust_vec)) {
  col_anno <- data.frame(Cluster = clust_vec)
  rownames(col_anno) <- colnames(mat)
}

##
## 5) Define annotation_colors only for the levels that actually exist
##
unique_clusters <- levels(clust_vec)   # the factor levels in use
# Example color assignment:
# Here, we just pick a few distinct colors. You can choose whichever palette you like.
some_colors <- c("red", "cyan", "orange", "green", "purple", "yellow")
# Subset to the number of actual cluster levels
some_colors <- some_colors[seq_along(unique_clusters)]
names(some_colors) <- unique_clusters

annotation_colors_list <- list(
  Cluster = some_colors
)

##
## 6) (Optional) Gaps for block splitting based on clusters
##
tbl      <- table(clust_vec)
cum_tbl  <- cumsum(tbl)
row_gaps <- head(cum_tbl, -1)

col_gaps <- NULL
if (!is.null(col_anno)) {
  col_gaps <- head(cum_tbl, -1)
}

##
## 7) pheatmap call
##
pheatmap(
  mat,
  color            = my_colors,
  breaks           = my_breaks,
  cluster_rows     = FALSE,
  cluster_cols     = FALSE,
  show_rownames    = FALSE,
  show_colnames    = FALSE,

  # Link in the annotation data frames and colors:
  annotation_row   = row_anno,
  annotation_col   = col_anno,
  annotation_colors = annotation_colors_list,

  # Show cluster "splits":
  gaps_row         = row_gaps,
  gaps_col         = col_gaps,

  main             = title
)
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}



cluster_plot = function(clusters_res,abline_h = 0.00015,loc=0, pattern_chosen,title,save_path_name='No'){
   
   Lap_graph = clusters_res$Lap_graph
  cluster_labels=clusters_res$cluster_result
  day_indices = clusters_res$day_indices
  
  if (sum((loc-rep(0,length(pattern_chosen)))^2)==0){
    loc = c(1:length(pattern_chosen))
  }
  hc_wd = hclust(d = as.dist(Lap_graph), method = "single")
plot(hc_wd,labels = day_indices, xlab="", main=title, sub = "",
     cex.main=2, ylab="")
mtext("Distance", side=2, line=2.2, cex=1.5)
abline(h=abline_h, lwd=2, col="red")
col_list = c("brown","blue","darkgreen","yellow", "pink")
for (i in 1:length(pattern_chosen)){
  mtext(paste0("Cluster ",pattern_chosen[i]), side=1, line=-.2, at =loc[i], cex=1, col=col_list[i])
}
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}


range_for_plot = function(density_list,eps=0.0001){
  max_value = -Inf
  min_value = Inf
  for (time_id in 1:length(density_list)){
    z_display = log(density_list[[time_id]]+eps)
    min_value = min(min_value, min(z_display))
    max_value = max(max_value, max(z_display))
  }
  return(c(min_value,max_value))
}


conditional_density_plot = function(all_res_single_cluster_all_f, cluster_id, grid_center_need, time_id, t_range_text,min_max_range,eps=0.0001,title,save_path_name='No'){
z_display = log(all_res_single_cluster_all_f[[cluster_id]][[time_id]]+eps)
nrow_need = length(unique(grid_center_need[,1]))
ncol_need = length(unique(grid_center_need[,2]))
z_display = matrix(z_display, nrow=nrow_need , ncol=ncol_need)

n_lv = 10
z_lv = seq(min(z_display), max(z_display), length.out = n_lv)
Mycolor = colorRampPalette(c("white","orange", "purple"))

par(mar=c(4,4,2,1))
contour(x=unique(grid_center_need[,1]), y= unique(grid_center_need[,2]),
            z =z_display,
            nlevels = n_lv,cex.axis=1.5,
            main = title,
            cex.main=2)
.filled.contour(x=unique(grid_center_need[,1]), y=unique(grid_center_need[,2]),
                    z = z_display,
                    levels = z_lv,col = Mycolor(n_lv))
    mtext(paste("Time: ", t_range_text[time_id]),side=3, at=-2.5, line=-1.5, cex=1.5)
    image.plot(zlim = c(min_max_range[1],min_max_range[2]), legend.only = TRUE, 
               col = Mycolor(n_lv), horizontal = FALSE, smallplot= c(.82,.86,0.6,0.9))
    if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}


dense_centroid_plot = function(all_res_single_cluster_all_x, all_res_single_cluster_all_y, cluster_no,t_range_text, observations_multiple_days,t_range_need_condi,save_path_name='No'){

if (cluster_no == 0) {
  title_need <- 'Estimated centroids for all historical observations'
  
  center_x <- rep(0, length(t_range_need_condi))
  center_y <- rep(0, length(t_range_need_condi))
  ht = 0.05*(freq_parameter)^(-1/3)
  for (t_id in 1:length(t_range_need_condi)) {
    t <- t_range_need_condi[t_id]
    all_t_kernel <- dnorm(pmin((observations_multiple_days$timestamp - t),1-(observations_multiple_days$timestamp - t)) / ht)
    center_x[t_id] <- sum(observations_multiple_days$x * all_t_kernel) / sum(all_t_kernel)
    center_y[t_id] <- sum(observations_multiple_days$y * all_t_kernel) / sum(all_t_kernel)
  }
  
  plot_data <- data.frame(
    cluster_no = cluster_no, 
    x = center_x, 
    y = center_y,
    t_id = factor(1:6, labels = t_range_text)  # Map t_id to time labels
  )
  
} else {
  title_need <- paste0('Estimated centroids for cluster ', cluster_no)
  
  plot_data <- data.frame(
    cluster_no = cluster_no, 
    x = all_res_single_cluster_all_x[[cluster_no]], 
    y = all_res_single_cluster_all_y[[cluster_no]], 
    t_id = factor(1:6, labels = t_range_text)  # Map t_id to time labels
  )
}


# Define a shape palette for different `t_id` values
shape_palette <- c(15, 16, 17, 18, 3, 8)  # Different point shapes

anchors_df_path <- paste0(getwd(),"/data_to_read/anchors.csv")
anchors_df = read.csv(anchors_df_path)
anchors_mat = as.matrix(anchors_df[,c('x','y')])

actual <- anchors_mat
estimated <- grid_center_need[activity_space_indices,]
df <- data.frame(X = actual[,1],   Y = actual[,2],   type = "Anchor points")

# line segments
  line_seg_path <- paste0(getwd(), "/data_to_read/segments.csv")
line_seg <- read.csv(line_seg_path)

# Create a Lines object
intrinsic_map <- segments_map(line_seg, "example_map")

# Convert the sp 'Lines' object to a data frame for ggplot
line_segments_df <- extract_line_segments_df(intrinsic_map)

# Plot using ggplot2 for each cluster
print(
  ggplot() +
     geom_point(
      data = anchors_df, 
      aes(x = x, y = y),
      shape = 2,   # example shape (triangle)
      size = 3
    ) +
  # 2. Overlay the line segments
  geom_segment(
  data = line_segments_df,
  aes(x = x, y = y, xend = xend, yend = yend),
  color =  "gray90",       # or any color of your choice
  linewidth = 1.2,
  inherit.aes = FALSE    # disable inherited shape/color = type
)+  # Plot lines
    geom_point(data = plot_data, color = "blue", aes(x = x, y = y, shape = t_id), size = 3) +
   scale_x_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-11, 10), expand = c(0, 0)) +
  coord_fixed() +
    # Shape by t_id
    scale_shape_manual(values = shape_palette) +  # Apply manual shape mapping
    theme_minimal() +
    theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    ) +
    labs(title = title_need, x = "X Coordinate", y = "Y Coordinate", shape = "Time of Day")  # Legend for time labels
)
 if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}



moredense_centroid_plot = function(all_res_single_cluster_all_x, all_res_single_cluster_all_y, cluster_no,t_range_text, observations_multiple_days,t_range_need_condi, highlighted_points= c(24, 33, 42, 51, 57)){

if (cluster_no == 0) {
  title_need <- 'Estimated centroids for all historical observations'
  
  center_x <- rep(0, length(t_range_need_condi))
  center_y <- rep(0, length(t_range_need_condi))
  ht = 0.05*(freq_parameter)^(-1/3)
  for (t_id in 1:length(t_range_need_condi)) {
    t <- t_range_need_condi[t_id]
    all_t_kernel <- dnorm(pmin((observations_multiple_days$timestamp - t),1-(observations_multiple_days$timestamp - t)) / ht)
    center_x[t_id] <- sum(observations_multiple_days$x * all_t_kernel) / sum(all_t_kernel)
    center_y[t_id] <- sum(observations_multiple_days$y * all_t_kernel) / sum(all_t_kernel)
  }
  
  plot_data <- data.frame(
    cluster_no = cluster_no, 
    x = center_x, 
    y = center_y 
  )
  
} else {
  title_need <- paste0('Estimated centroids for group ', cluster_no)
  
  plot_data <- data.frame(
    cluster_no = cluster_no, 
    x = all_res_single_cluster_all_x[[cluster_no]], 
    y = all_res_single_cluster_all_y[[cluster_no]]
  )
}

 
# Assign labels: Specific names for highlighted points, "Other time" for the rest
plot_data$category <- ifelse(seq_len(nrow(plot_data)) %in% highlighted_points,
                             paste0("Time ", seq_len(nrow(plot_data))),  # Custom text
                             "Other time")  # Default category for all other points

# Define colors for the legend (adjust as needed)
color_palette = c()
color_highlight = c("red","green","purple","orange","pink","yellow")
for (hid in 1:length(highlighted_points)){
    highlight_point = highlighted_points[hid]
    color_palette[paste0("Time ", highlight_point)] = color_highlight[hid]
}
color_palette["Other time"] = "blue"

anchors_df_path <- paste0(getwd(),"/data_to_read/anchors.csv")
anchors_df = read.csv(anchors_df_path)
anchors_mat = as.matrix(anchors_df[,c('x','y')])

actual <- anchors_mat
estimated <- grid_center_need[activity_space_indices,]
df <- data.frame(X = actual[,1],   Y = actual[,2],   type = "Anchor points")

# line segments
  line_seg_path <- paste0(getwd(), "/data_to_read/segments.csv")
line_seg <- read.csv(line_seg_path)

# Create a Lines object
intrinsic_map <- segments_map(line_seg, "example_map")

# Convert the sp 'Lines' object to a data frame for ggplot
line_segments_df <- extract_line_segments_df(intrinsic_map)

# Plot using ggplot2 for each cluster
print(
  ggplot() +
     geom_point(
      data = anchors_df, 
      aes(x = x, y = y),
      shape = 2,   # example shape (triangle)
      size = 3
    ) +
  
  # 2. Overlay the line segments
  geom_segment(
  data = line_segments_df,
  aes(x = x, y = y, xend = xend, yend = yend),
  color =  "gray90",       # or any color of your choice
  linewidth = 1.2,
  inherit.aes = FALSE    # disable inherited shape/color = type
)+  # Plot lines
    geom_point(
      data = plot_data,
      aes(x = x, y = y, color = category),  # Assign color by category
      size = 1.5
    ) +  # Shape by t_id
    scale_color_manual(values = color_palette) +  # Apply manual shape mapping
    xlim(-15, 15) + ylim(-15, 15) +  # Set plot limits
    theme_minimal() +
    theme(
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis text
      axis.ticks = element_blank(),  # Remove axis ticks
      panel.grid = element_blank(),  # Remove grid lines
      panel.background = element_blank(),  # Remove background
      plot.background = element_blank()  # Remove outer background
    ) +
    labs(title = title_need, x = "X Coordinate", y = "Y Coordinate", shape = "Time of Day")  # Legend for time labels
)
}


velocity_plot = function(trial, velocity_estimation, title_need, save_path_name="No"){
  t_grid_mid = velocity_estimation$t_grid
  velocity = velocity_estimation$velocity
plot_data <- data.frame(time = t_grid_mid, velocity = velocity)


# Generate the ggplot
print(
ggplot(plot_data, aes(x = time, y = velocity)) +
  geom_line(color = "purple", linewidth = 1.2) +  # Line with width 2 (size = 1.2)
  labs(title = title_need, x = "Time", y = "Estimated Velocity") +  # Labels
  ylim(-5, 450) +  # Set y-axis limits
  theme_minimal()  # Use a clean theme
)
if (save_path_name!='No'){
    dev.copy(pdf, save_path_name)
  dev.off()
}
}
 