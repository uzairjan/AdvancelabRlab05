  
  #' Pollution data
  #'
  #' @field response_list list. 
  #' @field url character. 
  #' @field countries list. 
  #' @import methods
  #' @export PollutionData
  #' @exportClass PollutionData 
  #' @examples PollutionData
PollutionData = 
  setRefClass(
    "PollutionData",
    fields = list(
      response_list = "list",
      url ="character",
      countries = "list"
    ),
    methods = list(
      initialize = function(listCountries =list("Sweden")){
        if(!is.list(listCountries) | any(!unlist(lapply(listCountries, is.character)))){
          stop("list of countries is not a list")
        }
        countriesSupported = list(
          "Pakistan",
          "India",
          "Afghanistan",
          "Sweden",
          "Bangladesh"
        )
        if(any(!(listCountries %in% countriesSupported))){
          stop("Provided country is supported")
        }
        if(length(listCountries) == 0){
          stop("Please provide a country")
        }
        
        countries <<- listCountries
        
        url <<- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldwide-pollution"
        response_list <<- getAllCountriesList()
        
      },
      getCountryData = function(country, facets=c()){
        response = jsonlite::fromJSON(getReqUrl(getReqPart(facets,"facet"), getReqQuery("refine.country", country), getReqQuery("rows", "10000")))
        return(response)
      },
      getAllCountriesList = function(){
        ls = list()
        for(country in countries){
          cat(country, "sending request ...", sep = " ", "\n")
          resp = jsonlite::fromJSON(getReqUrl(getReqQuery("refine.country",country), getReqQuery("rows","1000")))
          ls[[country]] = resp
          cat(country, "responded!", sep = " ", "\n")
        }
        return(ls)
      },
      getReqQuery = function(key, val){
        query = paste(list("&", key, "=", gsub(" ", "%20", val)), collapse = "")
        print(query)
        return(query)
      },
      getReqPart = function(secttionList, key){
        return(paste(lapply(secttionList, FUN=getReqQuery, key=key), collapse=""))
      },
      getReqUrl = function(...){
        ls = list(...)
        return(paste(c(url,ls), collapse = ""))
      },
      getOnlySectionData = function(response,sectionVector){
        if(length(sectionVector)==0){
          stop("secton vector cannot be empty!!!")
        }
        return(response$records$fields[,sectionVector])
      },
      getOnlySectionData = function(response, sectionVector){
        if(length(sectionVector) == 0){
          stop("Section Vector cannot be empty")
        }
        return(response$records$fields[,sectionVector])
      },
      getAllSectionResponse = function(sectionVector){
        if(length(sectionVector) == 0){
          stop("Section vector can not be empty")
        }
        if(!(is.vector(sectionVector) && is.character(sectionVector))){
          stop("provided section vector should be of character type")
        }
        res = NA
        i = 1
        for(resp in response_list){
          if(i == 1){
            res = getOnlySectionData(resp, sectionVector)
          }else{
            res = rbind(res, getOnlySectionData(resp, sectionVector))
          }
          i = i+1
        }
        return(res)
      }
      
    )
  )