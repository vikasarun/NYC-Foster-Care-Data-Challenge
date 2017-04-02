# makes a matrix of probabilities based on the child and family characterstic clusters 

# make dataframe full of clusters

# limit the dataframe to only those with outcomes within NYC and with a discharge reason == 1, 2, 3, or 5 
# and the clusters that we decided on in the logistic regression (this chunk was taking from v8)

# child clusters 
child_age_old <- ifelse(afcars.foster$AgeAtLatRem > 12, 1, 0)
child_female <- ifelse(afcars.foster$SEX == 2, 1, 0)
child_disabled <- ifelse(afcars.foster$CLINDIS == 1, 1, 0)
child_behavior_prob <- ifelse(afcars.foster$CHBEHPRB == 1, 1, 0)
child_neglect <- ifelse(afcars.foster$NEGLECT == 1, 1, 0)
child_moving_high <- ifelse(afcars.foster$NUMPLEP > 2, 1, 0)
child_orig_fam_married <- ifelse(afcars.foster$CTKFAMST == 1, 1, 0)
child_orig_fam_unmarried <- ifelse(afcars.foster$CTKFAMST == 2, 1, 0)
child_orig_fam_sing_female <- ifelse(afcars.foster$CTKFAMST == 3, 1, 0)
child_orig_fam_sing_male <- ifelse(afcars.foster$CTKFAMST == 4, 1, 0)
child_orig_parent_old <- ifelse(2015 - afcars.foster$CTK1YR > 50, 1, 0)
child_in_fc_long <- ifelse(afcars.foster$LifeLOS > 1092, 1, 0)

# family clusters
family_struct_married <- ifelse(afcars.foster$FOSFAMST == 1, 1, 0)
family_struct_unmarried <- ifelse(afcars.foster$FOSFAMST == 2, 1, 0)
family_struct_sing_female <- ifelse(afcars.foster$FOSFAMST == 3, 1, 0)
family_struct_sing_male <- ifelse(afcars.foster$FOSFAMST == 4, 1, 0)
family_metro_million <- ifelse(afcars.foster$RU13 == 1, 1, 0)
family_FIPS_36061 <- ifelse(afcars.foster$FIPSCode == 36061, 1, 0)
family_parent_old <- ifelse(2015 - afcars.foster$FCCTK1YR > 50, 1, 0)
family_payment_high <- ifelse(afcars.foster$FCMntPay > 3817, 1, 0)
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
df <- data.frame(child_age_old, child_female, child_disabled, child_behavior_prob, child_neglect,
                         child_moving_high, child_orig_fam_married, child_orig_fam_unmarried,
                         child_orig_fam_sing_female, child_orig_fam_sing_male, child_orig_parent_old,
                         child_in_fc_long,
                         family_struct_married, family_struct_unmarried, family_struct_sing_female,
                         family_struct_sing_male, family_metro_million, family_FIPS_36061, family_FIPS_36103,
                         family_FIPS_36029, family_parent_old, family_payment_high, family_same_race,
                         outcome_discharge_reason)

# child clusters 
# ******************************* important that these clusters match the df from the logistic regression, needs to be fixed 

# bucketed periods of child development
c_preteen <- ifelse(df$AgeAtLatRem <= 12, 1, 0)
c_not_preteen <- ifelse(df$AgeAtLatRem > 12, 1, 0)

# child is unwell 
c_disabled <- ifelse(df$CLINDIS == 1, 1, 0)
c_well <- ifelse(df$CLINDIS == 0 & df$NEGLECT == 0, 1, 0)

# child moves frequently 
c_moving_high <- ifelse(df$NUMPLEP > 3, 1, 0)
c_not_moving_high <- ifelse(df$NUMPLEP <= 3, 1, 0)

# family clusters

# family setting 
f_setting_pre_adopt <- ifelse(df$CURPLSET == 1, 1, 0)
f_setting_relative <- ifelse(df$CURPLSET == 2, 1, 0)
f_setting_non_relative <- ifelse(df$CURPLSET == 3, 1, 0)
f_setting_group <- ifelse(df$CURPLSET == 4, 1, 0)
f_setting_inst <- ifelse(df$CURPLSET == 5, 1, 0)

# family payment 
f_payment_high <- ifelse(family_payment_high==1)
f_payment_not_high <- ifelse(df$FCMntPay <= 3327,1,0)
  
# family and child race match 
f_same_race <- ifelse(family_same_race==1)
f_not_same_race <- ifelse(family_same_race==0) 

# combine child and family clusters into new dataframe
cluster_df_norace <- data.frame(c_preteen, c_not_preteen, c_disabled, c_well, c_moving_high, 
                         c_not_moving_high, f_setting_pre_adopt, f_setting_relative, f_setting_non_relative, 
                         f_setting_group, f_setting_inst, f_payment_high, f_payment_high)

cluster_df_racematch <- data.frame(c_preteen, c_not_preteen, c_disabled, c_well, c_moving_high, 
                                c_not_moving_high, f_setting_pre_adopt, f_setting_relative, f_setting_non_relative, 
                                f_setting_group, f_setting_inst, f_payment_high, f_payment_high, f_same_race)

cluster_df_no_racematch <- data.frame(c_preteen, c_not_preteen, c_disabled, c_well, c_moving_high, 
                                c_not_moving_high, f_setting_pre_adopt, f_setting_relative, f_setting_non_relative, 
                                f_setting_group, f_setting_inst, f_payment_high, f_payment_high,f_not_same_race)

# child variables col numbers
child_names_all = c(1:6)
child_dev_var = c(1:2) 
child_unwell_var = c(3:4)
child_move_var = c(5:6)

# family variables col numbers
family_names_all = c(7:15)
family_setting_var = c(7:11)
family_payment_var = c(12:13)
family_race_var = c(14:15)

child_var_names = colnames(cluster_df)[child_names_all]
family_var_names = colnames(cluster_df)[family_names_all]

# i made a diagram 
# another idea is to just make an actual matrix instead... 
prob_of_success <- data.frame(c_var=character(),f_var=character(),prob_suc=integer(),sample_size=integer())

# race not considered 
for(i in 1:length(child_dev_var))
{
  for (j in 1:length(child_unwell_var))
  {
    for (k in 1:length(child_move_var)) {
      temp_df <- subset(cluster_df_norace,cluster_df_norace[[i]]==1 & cluster_df_norace[[j]]==1 & cluster_df_norace[[k]]==1) #&
      #                 cluster_df_norace[[i+???]]==1 & cluster_df_norace[[j+???]]==1 & cluster_df_norace[[k+???]]==1)
      # how to iterate through all the family variables too? ^ 
        
      # makes a matrix of all the column means 
      x = colMeans(temp_df)
      
      temp_success = # regression 
      size = nrow(temp_df)
      
      # insert some concatenated list of child variables' names relevant to this row
      temp_child = 
        
      # insert some concatenated list of fam variables' names relevant to this column
      temp_family = 
      
      temp_row <- data.frame(temp_child,temp_family,temp_success,size)
      prob_of_success <- rbind(temp_success,temp_row)
    }
  }
}