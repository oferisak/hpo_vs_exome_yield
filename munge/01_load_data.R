# Load the data
hpo_raw_data<-readr::read_delim(hpo_data_file)

hpo_data<-hpo_raw_data%>%mutate(id=row_number(),.before=1,
                                solved=factor(ifelse(is.na(`Most likely gene - solved`),'N','Y')))
hpos_long<-hpo_data%>%separate_rows(Symptoms,sep = ',')%>%mutate(hpo_name=tolower(trimws(Symptoms)),.keep='unused')
# fix some fields
hpos_long<-hpos_long%>%mutate(hpo_name=case_when(
  hpo_name=='seizure'~'seizures',
  TRUE~as.character(hpo_name)
))