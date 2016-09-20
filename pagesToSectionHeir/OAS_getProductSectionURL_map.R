OAS_getProductSectionURL_map <- function(){

  
  # library("googlesheets")
  suppressPackageStartupMessages(library("dplyr"))
  # 
  # 
  # gs_title('Product_Catalog_MAIN', verbose = TRUE)
  # 
  # gs_key(x, lookup = NULL, visibility = NULL, verbose = TRUE)
  # 
  # gs_url(x, lookup = NULL, visibility = NULL, verbose = TRUE)
  # 
  # gs_ws_feed(x, lookup = NULL, verbose = TRUE)
  # 
  # gs_gs(x, visibility = NULL, verbose = TRUE)
  # 
  # ss = 'https://docs.google.com/spreadsheets/d/11IuoQHCThgkIJx_vpGPYT9GRGdgr-9iga37lhZ67kic/edit#gid=494074132'
  # ws = 'OP1ALLALPHA'
  # sheet <- gs_read(ss = ss,ws = ws)
  
  productSection <- read.csv('productSection.csv',stringsAsFactors = FALSE)
  
  strsplit(productSection$Section[1],",")
  
  productSection_Map <- NULL
  for(i in 1:NROW(productSection)){
    if(productSection$Section[i] != ""){
      product = productSection$Product[i]
      section = unlist(strsplit(productSection$Section[i],","))
      temp <- cbind.data.frame(product,section)
      productSection_Map <- rbind(productSection_Map,temp)
    }
    
  }
  
  uniqueSections <- unique(productSection_Map$section)
  
  
  sectionURL_Map <- NULL
  for(j in 1:NROW(uniqueSections)){
    
    print(paste0(j," of ",NROW(uniqueSections)))
    temp <- oas_list(credentials = oasAuth(),
                     request_type = 'Page',
                     search_criteria_attributes = c(pageSize="30000"),
                     search_criteria=list(newXMLNode("SectionId", uniqueSections[j]))
    )
    
    if(NROW(temp)>0){
      url = temp$Url
      site = temp$SiteId
    } else {
      
      url = 'no associated URL found'
      site = 'no associated site found'
      
    }
    
    section = uniqueSections[j]
    temp <- cbind.data.frame(section, url, site)
    
    sectionURL_Map <- rbind(sectionURL_Map, temp)
    
    
  }
  
  
  productSectionUrl_MAP <- sectionURL_Map %>% filter(url != 'no associated URL found') %>% 
    inner_join(productSection_Map, by = "section") %>% arrange(product)
  
  
  return(productSectionUrl_MAP)
}


