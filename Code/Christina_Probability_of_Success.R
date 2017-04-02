setwd("C:/Users/Vikas/Google Drive/NYC Foster Care Data Challenge")
# Change the above directory to the foster care data challenge folder on your computer

  # make dataframe full of clusters
  # child clusters -> still have lot to discuss on thresholds and splitting
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
  family_FIPS_36103 <- ifelse(afcars.foster$FIPSCode == 36103, 1, 0)
  family_FIPS_36029 <- ifelse(afcars.foster$FIPSCode == 36029, 1, 0)
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
  cluster_df <- data.frame(child_age_old, child_female, child_disabled, child_behavior_prob, child_neglect,
                           child_moving_high, child_orig_fam_married, child_orig_fam_unmarried,
                           child_orig_fam_sing_female, child_orig_fam_sing_male, child_orig_parent_old,
                           child_in_fc_long,
                           family_struct_married, family_struct_unmarried, family_struct_sing_female,
                           family_struct_sing_male, family_metro_million, family_FIPS_36061, family_FIPS_36103,
                           family_FIPS_36029, family_parent_old, family_payment_high, family_same_race,
                           outcome_discharge_reason)
  
  child_var = c(1:12) # child variables col numbers
  family_var = c(13:23) # family variables col numbers
  child_var_names = colnames(cluster_df)[child_var]
  family_var_names = colnames(cluster_df)[family_var]

  prob_of_success <- data.frame(c_var=character(),f_var=character(),prob_reunion=integer(),prob_rel=integer(),prob_adopt=integer(),prob_guardian=integer())
  
for(i in 1:length(child_var))
{
  for (j in 1:length(family_var))
  {
    temp <- subset(cluster_df,cluster_df[[i]]==1 | cluster_df[[j]]==1)
    
    # reunified with parent 
    prob_reunification = sum(temp$outcome_discharge_reason==1)/nrow(temp)

    # living with other relative(s) 
    prob_relative = sum(temp$outcome_discharge_reason==2)/nrow(temp)

    # adopted
    prob_adopted = sum(temp$outcome_discharge_reason==3)/nrow(temp)

    # guardianship
    prob_guardianship = sum(temp$outcome_discharge_reason==5)/nrow(temp)

    temp_row <- data.frame(child_var_names[i],family_var_names[j],prob_reunification,prob_relative,prob_adopted,prob_guardianship)
    prob_of_success <- rbind(prob_of_success,temp_row)
  }
}