OAS_getMarineKeyWords <- function(){
  
  manList <- oas_list(oasAuth(), request_type = 'Keyword',search_criteria_attributes = c(pageSize="30000")
                      ,search_criteria = list(newXMLNode("Status", 'A'),newXMLNode("Keyword", 'man=%'))
                      
  )
  
  classList <- oas_list(oasAuth(), request_type = 'Keyword',search_criteria_attributes = c(pageSize="30000")
                        ,search_criteria = list(newXMLNode("Status", 'A'),newXMLNode("Keyword", 'cl=%'))
                        
  )
  
  makeList <- oas_list(oasAuth(), request_type = 'Keyword',search_criteria_attributes = c(pageSize="30000")
                       ,search_criteria = list(newXMLNode("Status", 'A'),newXMLNode("Keyword", 'make=%'))
                       
  )
  
  categoryList <- oas_list(oasAuth(), request_type = 'Keyword',search_criteria_attributes = c(pageSize="30000")
                           ,search_criteria = list(newXMLNode("Status", 'A'),newXMLNode("Keyword", 'category=%'))
                           
  )
  
  typeList <- oas_list(oasAuth(), request_type = 'Keyword',search_criteria_attributes = c(pageSize="30000")
                       ,search_criteria = list(newXMLNode("Status", 'A'),newXMLNode("Keyword", 'type=%'))
                       
  )
  typeList <- typeList %>% filter(grepl('^type.+',text,ignore.case = TRUE))
  
  
  allKeyWords <- rbind(categoryList,classList,makeList,manList,typeList)
  my_credentials <- oasAuth()
  keyWordDel <- NULL
  for(i in 1:NROW(allKeyWords)){
    print(paste0(i," of ", NROW(allKeyWords)))
    if(allKeyWords[i,1] != 'man=blohm&voss')
    {
      temp <- oas_report(credentials=my_credentials, 
                         report_type='keyword delivery', 
                         report_name='impressions by site', 
                         id=allKeyWords[i,1],
                         start_date=as.character(Sys.Date()-31), 
                         end_date=as.character(Sys.Date()-1)
      )
      
      keywords <- allKeyWords[i,1]
      portal <- ifelse(is.null(temp$Site),"unknown",temp$Site)
      imps <- ifelse(is.null(temp$Imps),0,as.numeric(temp$Imps))
      
      temp2 <- cbind.data.frame(keywords, portal, imps)
      
      keyWordDel <- rbind(keyWordDel, temp2)
    }
    
    
    
  }
  
  websites <- unique(grep('yacht|boattrader|boats', keyWordDel$portal, ignore.case = TRUE, perl = FALSE, value = TRUE,
                          fixed = FALSE, useBytes = FALSE, invert = FALSE))
  
  keyWordDel2 <- unique(keyWordDel[keyWordDel$portal %in% websites,1])
  
  return(keyWordDel2)
}


