# makes a matrix of probabilities based on the child and family characterstic clusters 

# make dataframe full of clusters
# child clusters 

# bucketed periods of child development
child_preteen <- ifelse(afcars.foster$AgeAtLatRem <= 12, 1, 0)
child_not_preteen <- ifelse(afcars.foster$AgeAtLatRem > 12, 1, 0)

# child is unwell 
child_disabled <- ifelse(afcars.foster$CLINDIS == 1, 1, 0)
child_neglect <- ifelse(afcars.foster$NEGLECT == 1, 1, 0)
child_well <- 

# child moves frequently 
child_moving_high <- ifelse(afcars.foster$NUMPLEP > 2, 1, 0)

# child's original parent age
child_orig_parent_old <- ifelse(2015 - afcars.foster$CTK1YR > 50, 1, 0)

# length of child's stay 
child_in_fc_long <- ifelse(afcars.foster$LifeLOS > 1092, 1, 0)

# family clusters

# family setting 
family_setting_pre_adopt <- ifelse(afcars.foster$CURPLSET == 1, 1, 0)
family_setting_relative <- ifelse(afcars.foster$CURPLSET == 2, 1, 0)
family_setting_non_relative <- ifelse(afcars.foster$CURPLSET == 3, 1, 0)
family_setting_group <- ifelse(afcars.foster$CURPLSET == 4, 1, 0)
family_setting_inst <- ifelse(afcars.foster$CURPLSET == 5, 1, 0)
family_setting_indep <- ifelse(afcars.foster$CURPLSET == 6, 1, 0)
family_setting_runaway <- ifelse(afcars.foster$CURPLSET == 7, 1, 0)
family_setting_trial <- ifelse(afcars.foster$CURPLSET == 8, 1, 0)

# family payment 
family_payment_high <- ifelse(afcars.foster$FCMntPay > 3327,1,0)
  
# family and child race match 
family_same_race <- ifelse(afcars.foster$AMIAKN == 1 & afcars.foster$RF1AMAKN == 1, 1,
                                         ifelse(afcars.foster$ASIAN == 1 & afcars.foster$RF1ASIAN == 1, 1,
                                                ifelse(afcars.foster$BLKAFRAM == 1 & afcars.foster$RF1BLKAA == 1, 1,
                                                       ifelse(afcars.foster$HAWAIIPI == 1 & afcars.foster$RF1NHOPI == 1, 1,
                                                              ifelse(afcars.foster$WHITE == 1 & afcars.foster$RF1WHITE == 1, 1,
                                                                     ifelse(afcars.foster$UNTODETM == 1 & afcars.foster$RF1UTOD == 1, 1,
                                                                            ifelse(afcars.foster$HISORGIN == 1 & afcars.foster$HOFCCTK1 == 1, 1, 0)))))))


# discharge reason outcome column
outcome_discharge_reason <- afcars.foster[, "DISREASN"]

# combine child and family clusters into new dataframe
cluster_df_norace <- data.frame(child_preteen, child_disabled, child_neglect, child_moving_high, child_orig_parent_old,
                         child_in_fc_long, family_setting_pre_adopt, family_setting_relative, family_setting_non_relative, 
                         family_setting_group, family_setting_inst, family_setting_indep, family_setting_runaway,
                         family_setting_trial, outcome_discharge_reason)

cluster_df_racematch <- data.frame(child_preteen, child_disabled, child_neglect, child_moving_high, child_orig_parent_old,
                                child_in_fc_long, family_setting_pre_adopt, family_setting_relative, family_setting_non_relative, 
                                family_setting_group, family_setting_inst, family_setting_indep, family_setting_runaway,
                                family_setting_trial, outcome_discharge_reason)

cluster_df_no_racematch <- data.frame(child_preteen, child_disabled, child_neglect, child_moving_high, child_orig_parent_old,
                                child_in_fc_long, family_setting_pre_adopt, family_setting_relative, family_setting_non_relative, 
                                family_setting_group, family_setting_inst, family_setting_indep, family_setting_runaway,
                                family_setting_trial, outcome_discharge_reason)

child_var = c(1:12) # child variables col numbers


family_var = c(13:23)# family variables col numbers

child_var_names = colnames(cluster_df)[child_var]
family_var_names = colnames(cluster_df)[family_var]

prob_of_success <- data.frame(c_var=character(),f_var=character(),prob_suc=integer(),sample_size=integer())

# race not considered 
for(i in 1:length(child_var))
{
  for (j in 1:length(family_var))
  {
    temp <- subset(cluster_df,cluster_df[[i]]==1 | cluster_df[[j]]==1)
    
    prob_of_success = # regression 

    temp_row <- data.frame(child_var_names[i],family_var_names[j],prob_reunification,prob_relative,prob_adopted,prob_guardianship)
    prob_of_success <- rbind(prob_of_success,temp_row)
  }
}

# race matches
for(i in 1:length(child_var))
{
  for (j in 1:length(family_var))
  {
    temp <- subset(cluster_df,cluster_df[[i]]==1 | cluster_df[[j]]==1)
    
    prob_of_success = # regression 
      
      temp_row <- data.frame(child_var_names[i],family_var_names[j],prob_reunification,prob_relative,prob_adopted,prob_guardianship)
    prob_of_success <- rbind(prob_of_success,temp_row)
  }
}

# race does not match 
for(i in 1:length(child_var))
{
  for (j in 1:length(family_var))
  {
    temp <- subset(cluster_df,cluster_df[[i]]==1 | cluster_df[[j]]==1)
    
    prob_of_success = # regression 
      
      temp_row <- data.frame(child_var_names[i],family_var_names[j],prob_reunification,prob_relative,prob_adopted,prob_guardianship)
    prob_of_success <- rbind(prob_of_success,temp_row)
  }
}