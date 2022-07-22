acc_metrix_QA <- function(data){
  
  #data = test.pred
  
  
  # format data table 
  data <- data  %>% 
    mutate_if(is.character, as.factor)
  
  data <- data %>% 
    dplyr::rename("target" = mapunit1.x, #QA
                 "target2" = mapunit2.x,
                 ".pred_class" = mapunit1.y, #
                 ".pred_class2" = mapunit2.y)
  
  targ.lev <- levels(data$target)
  pred.lev <- levels(data$.pred_class)
  levs <- c(targ.lev, pred.lev) %>% unique()
  data$target <- factor(data$target, levels = levs)
  data$.pred_class <- factor(data$.pred_class, levels = levs)
 
  # basic accuracy (primary vs primary # not accounting for secondary calls)
  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
  
  # ###some aspatial metrics
  aspatial_pred <- data  %>% 
    dplyr::select(.pred_class) %>% 
    group_by(.pred_class) %>% 
    mutate(pred.tot = n()) %>% 
    ungroup() %>% distinct()
  
  aspatial_target <- data %>% 
    dplyr::select(target) %>% 
    group_by(target) %>% 
    mutate(trans.tot = n()) %>% 
    ungroup() %>% distinct()

  
  aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) %>% 
    # dplyr::mutate_if(is.integer, funs(replace_na(., 0))) %>% 
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>% 
    dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
    ungroup() %>%
    mutate(aspat_p_overall = sum(aspat_p)) 
  
  trans.sum <- unique(aspatial_sum$trans.sum)
  
  aspatial_sum <- aspatial_sum %>% 
    rowwise()%>%
    dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0))%>%
    ungroup()%>%
    dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
                  spat_p_overall =  acc$.estimate)
  
  
  # generate spatially explicit results for primary and prime/alternate
  xx <- data %>% tabyl(target, .pred_class)
  xy <- pivot_longer(xx, cols = !target) 
  
  # 2) generate  spatial primary accuracy 
  spat_p <- xy %>%
    filter(target == name) %>%
    mutate(spat_p_correct = value ) %>%
    dplyr::select(target, spat_p_correct) 
  
  outsum <- left_join(aspatial_sum, spat_p, by = "target")
  
  # generate spatial primary mean accuracy 
  outsum <- outsum %>%
    rowwise()%>%
    mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    ungroup() %>%
    mutate(spat_p_meanacc = mean(spat_p_unit_pos))
  
  # for the test data compariosn we can calculate the alternate calls also
    
    # spatially explicit calls: 
    spat_pa <- data %>%
      filter(!is.na(target2)) %>%
      filter(target != .pred_class)
    
    # # check if there are any calls alt points 
    if(nrow(spat_pa) > 0){
      
      # 5) calculate spatial prime / alt call accuracy 
      spat_pa <- spat_pa %>%
        tabyl(target2, .pred_class) %>%
        pivot_longer(cols = !target2) %>%
        filter(target2 == name) %>%
        dplyr::mutate(target = target2, 
                      spat_pa_correct = value) %>%
        dplyr::select(target, spat_pa_correct) 
      
          
      # calculate spatial pa calculations
      
      outsum <- left_join(outsum, spat_pa, by = "target") %>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        rowwise() %>%
        dplyr::mutate(spat_pa_total = (spat_pa_correct + spat_p_correct)) %>%
        ungroup() 
      
      outsum <- outsum %>%
        dplyr::mutate(spat_pa_overall = (sum(spat_pa_total)/trans.sum))%>%
        rowwise() %>%
        dplyr::mutate(spat_pa_unit_pos = min(spat_pa_total,trans.tot)/trans.tot)%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        ungroup() %>%
        dplyr::mutate(spat_pa_meanacc = mean(spat_pa_unit_pos)) 
      
      # calculate aspatial pa calcs
      
      aspat_pa_df <- outsum %>%
        dplyr::select(target, trans.tot, pred.tot, spat_pa_correct) %>%
        rowwise() %>%
        dplyr::mutate(aspat_pa_min_correct = min(trans.tot, pred.tot), 
                      aspat_pa_extra = spat_pa_correct,
                      aspat_pa_total = aspat_pa_min_correct + aspat_pa_extra,
                      aspat_pa_pred = min((aspat_pa_total/trans.sum), (trans.tot/trans.sum)),
                      aspat_pa_unit_pos = min(trans.tot, aspat_pa_total)/trans.tot) %>%
        ungroup()%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(aspat_pa_overall = sum(aspat_pa_pred),
                      aspat_pa_meanacc = mean(aspat_pa_unit_pos)) %>%
        dplyr::select( -c(trans.tot, pred.tot, spat_pa_correct, aspat_pa_min_correct)) 
      
      outsum <- left_join(outsum, aspat_pa_df, by = "target") 
      
   
      outsum$accuracy <- acc$.estimate
      
   # } else {
    #   
    #   print ("no secondary calls in test dataset")
    #   
    #   fix_cols <- outsum %>%
    #     mutate(spat_pa_correct = 0,
    #            spat_pa_total = 0, 
    #            "spat_pa_overall" = 0 ,  
    #            "spat_pa_unit_pos" = 0,
    #            "spat_pa_meanacc" = 0 ,    
    #            "aspat_pa_extra"= 0 ,      
    #            "aspat_pa_total"= 0 ,      
    #            "aspat_pa_pred" = 0 ,     
    #            "aspat_pa_unit_pos" = 0 ,  
    #            "aspat_pa_overall" = 0 ,  
    #            "aspat_pa_meanacc" = 0 ,   
    #            "spat_fpa" = 0 ,        
    #            "spat_fpa_overall" = 0 ,   
    #            "spat_fpa_unit_pos" = 0 ,  
    #            "spat_fpa_meanacc" = 0 ,         
    #            "aspat_fpa_extra"= 0 ,     
    #            "aspat_fpa_total"= 0 ,    
    #            "aspat_fpa_pred" = 0 ,     
    #            "aspat_fpa_unit_pos"= 0 , 
    #            "aspat_fpa_overall"= 0 ,   
    #            "aspat_fpa_meanacc" = 0 )
    #   
    #   outsum <- fix_cols
    #   
    # }
    # 
    #rowwise() %>%
    #dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE)) 
  } 
  #write.csv(outsum, "test26.csv")
  outsum 
  
}

