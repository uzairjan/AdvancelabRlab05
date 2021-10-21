
#' Title
#'
#' @field responses list. 
#' @field url character. 
#' @field countries list. 
#'
#' @return
#' @export
#'
#' @examples
PollutionData =   
  setRefClass(
    "PollutionData",
    fields = list(
      responses = "list",
      url = "character",
      countries = "list"
    ),
    methods = list(
      initialize = function(listCountries=list("Sweden")){
        if(!is.list(listCountries) | any(!unlist(lapply(listCountries, is.character))))
          stop("The input is not a list!")
        supported_countries = list(
          "Pakistan",
          "India",
          "Afghanistan",
          "Sweden"
        )
        if(any(!(listCountries %in% supported_countries)))
          stop("No correct input")
        if(length(listCountries)==0)
          stop("listCountries parameter cannot be empty!")
        
        
        countries <<- listCountries
        url <<- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldwide-pollution"
        responses <<- get_all_country_responses()
      },
      
      get_country_data = function(country, facets=c()){
        response = jsonlite::fromJSON(get_req_url(get_req_part(facets,"facet"), get_req_query("refine.country", country), get_req_query("rows", "10000")))
        return(response)
      },
      
      get_all_country_responses = function(){
        ress = list()
        for (country in countries) {
          cat(country, "request sent..." , sep = " ", "\n")
          res = jsonlite::fromJSON(get_req_url(get_req_query("refine.country", country), get_req_query("rows", "10000")))
          ress[[country]]=res
          cat(country, "responded!" , sep = " ", "\n")
        }
        return(ress)
      },
      
      get_req_query = function(key,val){
        return(paste(list("&",key,"=",gsub(" ", "%20", val)), collapse = ""))
      },
      
      get_req_part = function(facet_list, key){
        return(paste(lapply(facet_list, FUN=get_req_query, key=key), collapse=""))
      },
      
      get_req_url = function(...){
        elements = list(...)
        return(paste(c(url, elements), collapse=""))
      },
      
      get_only_faced_data = function(response,facet_vector){
        if(length(facet_vector)==0)
          stop("facet_vector cannot be empty!!!")
        return(response$records$fields[,facet_vector])
      },
      
      get_facets_all_responses = function(facet_vector){
        if(length(facet_vector)==0)
          stop("facet_vector cannot be empty!!!")
        if(!(is.vector(facet_vector) && is.character(facet_vector)))
          stop("facet_vector should be character vector!!!")
        d=NA
        counter=1
        for (res in responses) {
          if(counter==1)
            d=get_only_faced_data(res, facet_vector) 
          else
            d=rbind(d, get_only_faced_data(res, facet_vector))
          counter=counter+1
        }
        return(d)
      }
      
    )                        
  )