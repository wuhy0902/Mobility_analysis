get_weight_single_day3 = function(day_i, obs_input, t1_temp, t2_temp){
    observations_at_day_i = obs_input %>% filter(day==day_i)
  
    if (nrow(observations_at_day_i)==0){return(0)}
    if (nrow(observations_at_day_i)<=3){
      weight_need = rep(1,nrow(observations_at_day_i))
    }else{
        w10 = observations_at_day_i$timestamp[2]/2
        weight_mid = (observations_at_day_i$timestamp[3:nrow(observations_at_day_i)]-observations_at_day_i$timestamp[1:(nrow(observations_at_day_i)-2)])/2
        wn = (1-observations_at_day_i$timestamp[(nrow(observations_at_day_i)-1)])/2
        weight_need = c(w10,weight_mid,wn)
        weight_need = weight_need/sum(weight_need)
        all_weight_day_i <- cumsum(weight_need)

        first_index <- which(all_weight_day_i >= t1_temp)[1]
        if (length(first_index)!=0){
            weight_need[first_index] = (all_weight_day_i[first_index]-t1_temp)
        }
        last_index <- which(all_weight_day_i >= t2_temp)[1]
        if (length(last_index)!=0){
            weight_need[last_index] = weight_need[last_index] - (all_weight_day_i[last_index]-t2_temp) 
        }
        
        if (first_index>1){
            weight_need[1:(first_index-1)] = 0
        }
        if (last_index<length(all_weight_day_i)){
            weight_need[(last_index+1):length(all_weight_day_i)] = 0
        }
            
    }
    weight_need = weight_need/sum(weight_need)
    return(weight_need)
}

get_weight_single_day = function(day_i, obs_input, t1_temp, t2_temp){
    observations_at_day_i = obs_input %>% 
    filter(day==day_i) %>%
    filter(timestamp >=  t1_temp & timestamp <=  t2_temp)
  
    if (nrow(observations_at_day_i)==0){return(0)}
    if (nrow(observations_at_day_i)<=3){
      weight_need = rep(1,nrow(observations_at_day_i))
    }else{
        w10 = (observations_at_day_i$timestamp[2]-t1_temp)/2
        weight_mid = (observations_at_day_i$timestamp[3:nrow(observations_at_day_i)]-observations_at_day_i$timestamp[1:(nrow(observations_at_day_i)-2)])/2
        wn = (t2_temp-observations_at_day_i$timestamp[(nrow(observations_at_day_i)-1)])/2
        weight_need = c(w10,weight_mid,wn)
    }
    weight_need = weight_need/sum(weight_need)
    return(weight_need)
}

get_weight_single_day2 = function(day_i, obs_input, t1_temp, t2_temp){
    observations_at_day_i = obs_input %>% 
    filter(day==day_i)
    obs_amount = nrow(observations_at_day_i)
    first_index <- which(observations_at_day_i$timestamp>= t1_temp)[1]
    last_index =  which(observations_at_day_i$timestamp<t2_temp)[1]-1

    observations_at_day_i = obs_input %>% 
    filter(day==day_i) %>%
    filter(timestamp >=  t1_temp & timestamp <=  t2_temp)
  
    if (nrow(observations_at_day_i)==0){return(0)}
    if (nrow(observations_at_day_i)<=3){
      weight_need = rep(1,nrow(observations_at_day_i))
    }else{
        
        w10 = (observations_at_day_i$timestamp[2]-t1_temp)/2
        weight_mid = (observations_at_day_i$timestamp[3:nrow(observations_at_day_i)]-observations_at_day_i$timestamp[1:(nrow(observations_at_day_i)-2)])/2
        wn = (t2_temp-observations_at_day_i$timestamp[(nrow(observations_at_day_i)-1)])/2
        weight_need = c(w10,weight_mid,wn)
    }
    if (first_index>1){
            weight_need = c(rep(0,first_index),weight_need)
        }
        if (last_index<obs_amount){
            rest0_amount = obs_amount-length(weight_need)
            weight_need = c(weight_need, rep(0,rest0_amount))
            
        }
            
    weight_need = weight_need/sum(weight_need)
    return(weight_need)
}


hs_determine = function(obs_input,all_w,hs_const,condi,indices_selected=NULL){
    
    mu_w = sum(all_w*obs_input$x)
    sw2 = sum(all_w*(obs_input$x-mu_w)^2)
    mu_wy = sum(all_w*obs_input$y)
    sw2y = sum(all_w*(obs_input$y-mu_wy)^2)
    if (condi=='mar'){
        hs = (hs_const*sqrt(sw2+sw2y))*(length(which(all_w>0)))^(-1/6)
    }else if(condi=='int'){
        hs = (hs_const*sqrt(sw2+sw2y))*(length(obs_input$x))^(-1/6)
    }else{
            hs = (hs_const*sqrt(sw2+sw2y))*(length(indices_selected))^(-1/6)
    }
    
    return(hs)
}

ht_determine = function(obs_amount,ht_const){
    ht = ht_const*mean(obs_amount)^(-1/3)
    return(ht)
}
kde_gps = function(obs_input, grid_center_need, method,t_range_need=NULL,time_interval=c(0,0.99999), hs_const=0.8,ht_const=0.3){
    
    if (method=='marginal'){
        indices_selected = which(obs_input$timestamp>=time_interval[1] & obs_input$timestamp<=time_interval[2])
        all_w = unlist(lapply(unique(obs_input$day),get_weight_single_day,obs_input,time_interval[1],time_interval[2]))
        hs = hs_determine(obs_input[indices_selected,],all_w/sum(all_w),hs_const,'mar')
        # print(hs)
        H = matrix(c(hs^2,0,0,hs^2),2,2)
        all_w = length(indices_selected)*all_w/sum(all_w)
        kde_estimate = kde(as.matrix(obs_input[indices_selected,c(1,2)]), w=all_w, H=H, eval.points=grid_center_need)$estimate
        return(kde_estimate)
    }else if (method=='integral_conditional'){
        all_days = unique(obs_input$day)
        obs_amount = rep(0,length(all_days))
        obs_all_day_list = list()
        for (day_id in 1:length(all_days)){
            day = all_days[day_id]
            obs_all_day_list[[day_id]] = obs_input[obs_input$day==day,]
            obs_amount[day_id] = nrow(obs_all_day_list[[day_id]])
        }
        #-------------same weight
        all_w = unlist(lapply(unique(obs_input$day),get_weight_single_day2,obs_input,time_interval[1],time_interval[2]))
        hs = hs_determine(obs_input,all_w/sum(all_w),hs_const,'int')
        
        H = matrix(c(hs^2,0,0,hs^2),2,2)
        #------------------------
        ht = ht_determine(obs_amount,ht_const)

        all_obs = obs_input[,c(1,2)]
       
        get_weight_condition_raw=function(day_id,t,obs_all_day_list){
            return(dnorm((obs_all_day_list[[day_id]]$timestamp-t)/ht)/nrow(obs_all_day_list[[day_id]]))
        }

        get_weight_condition=function(day_id,t,obs_all_day_list){
            return(dnorm(pmin(obs_all_day_list[[day_id]]$timestamp-t, 1-(obs_all_day_list[[day_id]]$timestamp-t))/ht)/nrow(obs_all_day_list[[day_id]]))
        }

        kde_esti = rep(0,nrow(grid_center_need))
        w_sum = rep(0,nrow(as.matrix(obs_input[,c(1,2)])))
        for (t_id in 1:length(t_range_need)){
            t = t_range_need[t_id]
            w = unlist(lapply(1:length(all_days),get_weight_condition,t,obs_all_day_list))
        
            w = nrow(obs_input)*w/sum(w)
            w_sum = w_sum + w
            
        }
        w_sum = nrow(obs_input)*w_sum/sum(w_sum)
        kde_esti = kde(as.matrix(obs_input[,c(1,2)]), H=H,w = w_sum, eval.points=grid_center_need)$estimate
        
        return(kde_esti)
    }else if (method=='naive'){
        
        indices_selected = which(obs_input$timestamp>=time_interval[1] & obs_input$timestamp<=time_interval[2])
        all_w = unlist(lapply(unique(obs_input$day),get_weight_single_day,obs_input,time_interval[1],time_interval[2]))
        hs = hs_determine(obs_input[indices_selected,],all_w/sum(all_w),hs_const,'naive', indices_selected)
        # print(hs)
        H = matrix(c(hs^2,0,0,hs^2),2,2)
        kde_estimate = kde(as.matrix(obs_input[indices_selected,c(1,2)]), H=H, eval.points=grid_center_need)$estimate
        return(kde_estimate)
    
    }else if (method=='naive_naive_bandwidth'){
        
	indices_selected = which(obs_input$timestamp>=time_interval[1] & obs_input$timestamp<=time_interval[2])
    
	kde_estimate = kde(as.matrix(obs_input[indices_selected,c(1,2)]),eval.points=grid_center_need)$estimate
    return(kde_estimate)
    }else{
        cat('Unavailable!')
    }
    
   
}
