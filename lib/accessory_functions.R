# A function that takes in the raw hpo table, parses the phenotypes and fixes by comparing them to the hpo api
fix_input_hpo_table <- function(hpo_terms) {
  hpo_terms_with_id_raw<-NULL
  for (query in hpo_terms$hpo_name){
    print(query)
    query_res<-hpo_search_query(query)
    print(query_res)
    if (class(query_res)=='list'){
      hpo_id<-NA
      matching_hpo_term<-NA
    }else{# if the term returned results
      # check if one of the results is an exact match
      query_in_term<-which(tolower(query_res$name)==query)
      # if the query was not found, grab the first row otherwise grab the exact match
      to_grab<-ifelse(identical(query_in_term,integer(0)),1,query_in_term)
      hpo_id<-query_res%>%slice(to_grab)%>%pull(ontologyId)
      matching_hpo_term<-query_res%>%slice(to_grab)%>%pull(name)
    }
    hpo_terms_with_id_raw<-hpo_terms_with_id_raw%>%rbind(data.frame(hpo_name=query,matching_hpo_term,hpo_id=hpo_id))
  }
  # check if term is the same
  hpo_terms_with_id_raw<-hpo_terms_with_id_raw%>%mutate(exact_match=hpo_name==tolower(matching_hpo_term))
  # exact matches
  hpo_terms_with_id<-hpo_terms_with_id_raw%>%filter(exact_match)
  # now collect non exact and missing
  missing_hpo_terms<-hpo_terms_with_id_raw%>%filter(is.na(hpo_id))
  writexl::write_xlsx(missing_hpo_terms,path=glue('{main_output_folder}/missing_hpo_terms_{Sys.Date()}.xlsx')) 
  non_exact_hpo_terms<-hpo_terms_with_id_raw%>%filter(!(exact_match))
  writexl::write_xlsx(non_exact_hpo_terms,path=glue('{main_output_folder}/non_exact_hpo_terms_{Sys.Date()}.xlsx')) 
  # save exact matches
  exact_match_output_table<-'./data/hpo_term_to_id.csv'
  message(glue('Writing the exact match table to {exact_match_output_table}'))
  write.table(hpo_terms_with_id,file=exact_match_output_table,sep='\t',row.names = F)
}
