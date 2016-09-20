OAS_getMarineImpressionsKeyWordSection <- function(allKeyWords){
  
  my_credentials <- oasAuth()
  keyWordDel <- NULL
  for(i in 1:NROW(allKeyWords)){
    print(paste0(i," of ", NROW(allKeyWords)))
      temp <- oas_report(credentials=my_credentials, 
                         report_type='keyword delivery', 
                         report_name='impressions by Section', 
                         id=allKeyWords[i],
                         start_date=as.character(Sys.Date()-31), 
                         end_date=as.character(Sys.Date()-1)
      )
      keywords <- allKeyWords[i]
      section <- ifelse(is.null(temp$Section),"unknown",temp$Section)
      imps <- ifelse(is.null(temp$Imps),0,as.numeric(temp$Imps))
      
      temp2 <- cbind.data.frame(keywords, section, imps)
      
      keyWordDel <- rbind(keyWordDel, temp2)
    }
  
  return(keyWordDel)
}


