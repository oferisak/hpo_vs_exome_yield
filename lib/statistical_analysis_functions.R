# a function that takes in an hpo term and a table and outputs whether that hpo term is significantly associated with 
# solved status (using the fisher exact)
test_hpo_vs_solved<-function(hpo_to_test,hpos_table){
  # for every id, check if the hpo term was given and whether it was solved
  tmp_table<-hpos_table%>%
    group_by(id,solved)%>%
    summarize(!!sym(hpo_to_test):=ifelse(hpo_to_test%in%hpo_name,1,0))
  # now create the count table
  count_table<-table(tmp_table%>%pull(hpo_to_test),tmp_table%>%pull(solved))
  fisher_res<-broom::tidy(fisher.test(count_table))%>%select(estimate,conf.low,conf.high,p.value)
  #chisq_res<-broom::tidy(chisq.test(count_table))%>%select(statistic,p.value)
  return(data.frame(hpo_name=hpo_to_test,fisher_res))
}

# a function that takes in an hpo term and a table and outputs whether that hpo term is significantly associated with 
# solved status (using the fisher exact)
test_hpo_ancestry_vs_solved<-function(hpo_to_test,hpos_table){
  print(hpo_to_test)
  if (hpo_to_test%in%c('all','phenotypic abnormality')){return(NULL)}
  # first collect the ancestors and descendants of the given hpo term
  hpo_id_to_test<-names(hpo_obo$name[which(hpo_obo$name==hpo_to_test)])
  hpo_ancestors<-get_hpo_ancestors(hpo_id_to_test,hpo_obo)%>%filter(hpo_id==hpo_id_to_test,ancestor!=hpo_id_to_test)%>%pull(ancestor)
  hpo_descendants<-get_hpo_descendants(hpo_id_to_test,hpo_obo)%>%filter(hpo_id==hpo_id_to_test,descendant!=hpo_id_to_test)%>%pull(descendant)
  # for each id check whether any of its hpo ids match the given hpo term or its descendants
  tmp_table<-hpos_table%>%
    group_by(id,solved)%>%
    summarize(!!sym(hpo_to_test):=ifelse(length(intersect(hpo_id,c(hpo_id_to_test,hpo_descendants)))>0,1,0))

  # now create the count table
  count_table<-table(tmp_table%>%pull(hpo_to_test),tmp_table%>%pull(solved))
  fisher_res<-broom::tidy(fisher.test(count_table))%>%select(estimate,conf.low,conf.high,p.value)
  #chisq_res<-broom::tidy(chisq.test(count_table))%>%select(statistic,p.value)
  return(data.frame(hpo_name=hpo_to_test,fisher_res))
}
