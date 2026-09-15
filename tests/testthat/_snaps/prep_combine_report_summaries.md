# prep_combine_report_summaries works

    Code
      comb_sum
    Output
      $Data
        STUDY_SEGMENT VAR_NAMES com_item_missingness.NUM_com_qum_spec
      1         Study         x                                     0
      2         Study         y                                     0
      4         Study         z                                     0
        com_item_missingness.NUM_int_vfe_missunc com_item_missingness.PCT_com_crm_mv
      1                                        2                              33.33%
      2                                        1                              16.67%
      4                                        1                              16.67%
        com_item_missingness.PCT_com_qum_spec
      1                                 0.00%
      2                                 0.00%
      4                                 0.00%
        com_item_missingness.PCT_int_vfe_missunc
      1                                   33.33%
      2                                   16.67%
      4                                   16.67%
      
      $Table
        STUDY_SEGMENT VAR_NAMES com_item_missingness.NUM_com_qum_spec
      1         Study         x                                     0
      2         Study         y                                     0
      4         Study         z                                     0
        com_item_missingness.NUM_int_vfe_missunc com_item_missingness.PCT_com_crm_mv
      1                                        2                               33.33
      2                                        1                               16.67
      4                                        1                               16.67
        com_item_missingness.PCT_com_qum_spec
      1                                     0
      2                                     0
      4                                     0
        com_item_missingness.PCT_int_vfe_missunc
      1                                    33.33
      2                                    16.67
      4                                    16.67
      
      $meta_data
        VAR_NAMES LABEL DATA_TYPE SCALE_LEVEL MISSING_LIST JUMP_LIST END_DIGIT_CHECK
      1         x     x   integer       ratio            |         |           FALSE
      2         y     y   integer     nominal            |         |           FALSE
      3         z     z   integer     nominal            |         |           FALSE
        N_RULES UNIVARIATE_OUTLIER_CHECKTYPE MISSING_LIST_TABLE VARIABLE_ROLE
      1      NA                         <NA>               <NA>       primary
      2      NA                         <NA>               <NA>       primary
      3      NA                         <NA>               <NA>       primary
      
      attr(,"class")
      [1] "dq_report2_summary"

