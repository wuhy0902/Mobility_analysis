===============================================================================
README: R SCRIPT FOR FIGURE GENERATION
===============================================================================

This project analyzes an individual's mobility activity using our proposed 
framework. It provides R scripts used to generate figures presented in the 
manuscript. The project is divided into two parts:

  1) PART 1: REAL DATA ANALYSIS
  2) PART 2: SIMULATED DATA ANALYSIS


===============================================================================
BEFORE RUNNING THE CODE: SAVING OR VIEWING FIGURES
===============================================================================

If you would like to save the figures:

You can specify a custom save location by passing a "save_path_name" argument 
to each plotting function, if the location does not exist, it will be created.

If you do not want to save figures and prefer only to view them in the R IDE, 
simply omit the "save_path_name" argument when calling the plotting functions.

===============================================================================
PART 1: REAL DATA ANALYSIS
===============================================================================


INPUT FILES:
  - data125/GPS_shifted_125.csv
    This file contains the GPS observational data. To comply with confidentiality
    requirements, the original recording dates are hidden, and longitude/latitude 
    values have been shifted to an arbitrary origin.


REQUIRED DATA FORMAT:
Any dataset used for this analysis must contain exactly the four columns listed
below. The name and format of each column should match these specifications:

+------------+---------------------------------------------+----------------------------------------+
| Column     | Example                                     | Description                            |
+------------+---------------------------------------------+----------------------------------------+
| Date       | Wednesday, Month 1, day No.4                | Day of recording                       |
| Time       | 2:19:33 AM                                  | Time of recording                      |
| Latitude   | Numeric (float)                             | Shifted latitude coordinates           |
| Longitude  | Numeric (float)                             | Shifted longitude coordinates          |
+------------+---------------------------------------------+----------------------------------------+


HOW TO RUN THE CODE:
  - Main Script:
      Figure_generation_real_data.R
    Run this script directly to reproduce Figures 1 and 4–9 in the manuscript.

  - Auxiliary Script:
      real_data_appl_functions.R
    Provides all functions required for real data analysis.


OUTPUT FILES AND FOLDERS:
After running "Figure_generation_real_data.R" without modifications, figures will
be generated and saved into several folders:

1) real_app/
   - D1-1, D1-2, D1-3, D1-4:
       Observations on selected days.
   - p125_hist1, p125_hist2:
       Individual timestamp histograms (weekdays and weekends).
   - p125_log, WD_density, WE_density:
       log(estimated density + eps) plots for all days, weekdays, and weekends.
   - AS_all, AS_WD, AS_WE:
       Activity space visualizations for all days, weekdays, and weekends.

2) real_app/hour/
   - fgps_WD_x, fgps_WE_x:
       log(estimated density + eps) during x:00–(x+1):00, 
       separately for weekdays (WD) and weekends (WE).
   - fgps_WDAS_x, fgps_WEAS_x:
       Activity space plots during x:00–(x+1):00, 
       separately for weekdays (WD) and weekends (WE).

3) real_app/cluster/
   - 1_dendrogram:
       Dendrogram showing weekday activity clustering via hierarchical clustering.
   - 2_clu1, 2_clu2, ...
       log density plots for each identified cluster.
   - 3_clu1c, 3_clu2c, ...
       log density plots highlighting cluster centers at specific time points.

4) real_app/cluster/hour/
   - fgps_clux_y:
       log(estimated density + eps) for cluster x during y:00–(y+1):00.
   - fgps_cluxAS_y:
       Activity space plots for cluster x during y:00–(y+1):00.

--------------------------------------------------------------------------------
END OF PART 1
--------------------------------------------------------------------------------


===============================================================================
PART 2: SIMULATED DATA ANALYSIS
===============================================================================


INPUT FILES:
--------------------------------------------------------------------------------

1) data_to_read/anchors
   - Contains the coordinates for all anchor points.
   - 3 columns:
       x (float): x-coordinate
       y (float): y-coordinate
       anchor_points_name (optional, string)

2) data_to_read/segments
   - Describes all road segments. A segment can be a simple line segment (2 points)
     or a polyline (multiple points).
   - The coordinate order indicates the default positive direction for movement.

   Example:
       id |  x  |  y
       1  |  0  |  1
       1  |  0  |  0
     Traveling from (0,1) to (0,0) along segment 1 is direction +1; the reverse 
     is direction -1.

3) data_to_read/routes
   - Lists all possible routes (between anchor points) that a simulated individual
     might follow.
   - Each route specifies:
       routes_no (int): unique route ID
       segment    (int): segment ID (in sequential order)
       direction  (+1/-1): direction of travel along each segment
       comment    (optional, string)

4) data_to_read/patterns
   - Describes all possible activity patterns (anchor–route–anchor–...–anchor).
   - Each line represents one segment of an activity sequence, including 
     expected start times and durations.

   Columns (example):
       id                 (int)  : pattern ID
       position           (0/1)  : 0 = anchor, 1 = route
       location           (int)  : anchor ID if position=0, segment ID if position=1
       tstart_center      (float, 0–24): expected start time 
       duration_center    (float) : expected duration
       duration_dist      (str)   : duration distribution name (e.g. "Trun_Gaussian")
       duration_dist_para1(float) : distribution parameter (e.g. std. dev. for truncated Gaussian)
       duration_bound     (float) : bound for truncated Gaussian
       next_position      (0/1)   : 0 = anchor, 1 = route
       next_location      (int)   : next location’s anchor or segment ID
       comment            (optional, string)

   Example interpretation:
     An example pattern with id=4 might be:
       - Home  (expected duration=10 hours)
       - Route 8  (expected duration=0.8 hours)
       - Beach (expected duration=5.7 hours)
       - Route 9  (expected duration=0.8 hours)
       - Home  (expected duration=6.7 hours)

5) data_to_read/pattern probability
   - Contains the probability of each pattern being chosen on a given day.
   - Format typically includes at least:
       pattern_id, probability_value
     (additional columns if needed).

6) timestamp_irregular_sample
   - For simulations requiring irregular GPS sampling intervals, this folder 
     holds reference timestamps taken from real data. These can be used to 
     replicate non-uniform sampling patterns.

--------------------------------------------------------------------------------
HOW TO RUN THE CODE:
--------------------------------------------------------------------------------

Main Script:
  - Figure_generation_simulation.R
    Run this script to reproduce Figures 10–17 in the manuscript.

Auxiliary Scripts:
  - data_generate_funcs_cluster_ks.R
    Functions for generating simulated GPS observations based on the map and 
    activity patterns from the data_to_read folder.

  - kde_funcs_cluster_revised_ks.R
    Implements the proposed KDE (kernel density estimation) methods.

  - more_simulation_funcs.R
    Contains functions used in Section 5 (simple movement model) for further 
    mobility analysis using our model.

  - plot_related_functions.R
    Utility functions for creating plots.

  - helper_functions.R
    General helper functions used across the simulation and plotting processes.

--------------------------------------------------------------------------------
OUTPUT:
--------------------------------------------------------------------------------

After running "Figure_generation_simulation.R" without any changes, all generated
figures will be saved in the "sim/" folder, organized as follows:

1) sim/pic/basic/map
   - The intrinsic map on which the simulated individual moves.

2) sim/pic/basic/density_plot
   - true_density_(interested interval)(measurement error index):
       log of density for the true observations (without measurement error).
   - marginal_estimated_density_(interested interval)(measurement error index):
       log of estimated density (marginal method) for observations (with error).
   - inte_cond_estimated_density_(interested interval)(measurement error index):
       log of estimated density (conditional method) for observations (with error).

3) sim/pic/more_simulations/anchor_points
   - anchor_points_overall, anchor_points_weekdays, anchor_points_weekeds:
       Detected anchor points vs. true anchor points for all days, weekdays,
       and weekends.

4) sim/pic/more_simulations/activity_space
   - activity_space_overall, activity_space_weekdays, activity_space_weekeds:
       Estimated activity spaces for all days, weekdays, and weekends.

5) sim/pic/more_simulations/centers
   - cluster_x:
       Centers of each cluster at specific time points (e.g. 2 AM, 6 AM, 10 AM,
       2 PM, 6 PM, 10 PM). x=0 means all days combined.

6) sim/pic/more_simulations/clustering
   - Contains the affinity matrix (ordered distance matrix) and dendrogram
     plots for weekday/weekend clustering.

7) sim/pic/more_simulations/conditional_density
   - cluster_x_y:
       Estimated density of cluster x, given time y (e.g., 2 AM, 6 AM, etc.).

--------------------------------------------------------------------------------
END OF PART 2
--------------------------------------------------------------------------------