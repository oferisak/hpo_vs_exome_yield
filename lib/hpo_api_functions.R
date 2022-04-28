
httr::set_config(config(ssl_verifypeer = FALSE))

hpo_search_query<-function(query){
  api_request<-URLencode(glue('https://hpo.jax.org/api/hpo/search/?q={query}'))
  res = GET(api_request)
  output_terms = fromJSON(rawToChar(res$content))$terms
  return(output_terms)
}

hpo_get_id_parents<-function(hpo_id){
  api_request<-URLencode(glue('https://hpo.jax.org/api/hpo/term/{hpo_id}'))
  res = GET(api_request)
  id_parents = fromJSON(rawToChar(res$content))$relations$parents
  return(id_parents)
}

parse_hpo_obo<-function(){
  hpo_obo_file<-'/media/SSD/Bioinformatics/Databases/hpo/hp.obo'
  hpo_obo<-ontologyIndex::get_ontology(hpo_obo_file,propagate_relationships = 'is_a')
  return(hpo_obo)
}

get_hpo_names<-function(hpo_ids,hpo_obo){
  hpo_ids_with_names<-hpo_ids%>%purrr::map_df(function(x) data.frame(hpo_id=x,hpo_name=hpo_obo%>%ontologyIndex::get_term_property('name',x)))
  return(hpo_ids_with_names)
}

get_hpo_ancestors<-function(hpo_ids,hpo_obo){
  names(hpo_ids)<-hpo_ids
  per_hpo_ancestors<-hpo_ids%>%purrr::map(function(x) ontologyIndex::get_ancestors(hpo_obo,x))
  hpos_with_ancestors<-NULL
  for (hpo_id in names(per_hpo_ancestors)){
    hpos_with_ancestors<-hpos_with_ancestors%>%
      bind_rows(data.frame(hpo_id=hpo_id,ancestor=per_hpo_ancestors[[hpo_id]]))
  }
  # now add the names for each hpo ancestor
  ancestor_names<-hpos_with_ancestors$ancestor%>%unique()%>%purrr::map_df(function(x) data.frame(ancestor=x,ancestor_name=hpo_obo%>%ontologyIndex::get_term_property('name',x)))
  hpos_with_ancestors<-hpos_with_ancestors%>%left_join(ancestor_names)
  return(hpos_with_ancestors)
}

