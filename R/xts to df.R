xts_to_dataframe<-function(data_xts){
  df_t<-data.frame(date=(index(data_xts)),
                   value=coredata(data_xts))
  colnames(df_t)<-c("date", "value")
  df_t
}
