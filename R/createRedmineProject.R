#' @name createRedmineProject
#' @title Create Redmine Project
#' @description Creates a new Redmine Project and returns the object relating to the object. If the project already exists, 
#' it will return the project object without overwriting it.
#' @param projectName String representing the project name
#' @param apiKey Your API key
#' @param url URL for your Redmine server
#' @param identifier Project identifier in Redmine. If not provided, will be a cleaned version of your project name
#' @param description Optional description for your project
#' @seealso \link{http://www.redmine.org/projects/redmine/wiki/Rest_Projects#Creating-a-project}
#' @return Reference Class of type RedmineProject
createRedmineProject = function(projectName,
                                apiKey,
                                url,
                                identifier=NULL,
                                description=NULL
                                ){
 if (is.null(identifier)) 
   identifier = projectName
 
 # Remove invalid characters from identifier
 identifier = strtrim(
   x = gsub(
     pattern = "[^a-z\\d\\-_]+", 
     replacement = "_", 
     x = tolower(identifier)
   ),
   width = 100)
 localurl = regmatches(url, regexec("(http){1}s{0,1}:\\/{2}([a-z0-9]+\\.?){2,6}(:\\d+){0,1}",  url))[[1]][1]
 
 url = paste0(localurl, "/projects.json")
 
 if (is.null(description)){
   fields = list(
     project=list(
       name = projectName,
       identifier = identifier
     )
   )
 } else {
   fields = list(
     project=list(
       name = projectName,
       identifier = identifier,
       description = description
     )
   )
 }
 
 fields = toJSON(fields)
 # Build request
 h = basicTextGatherer()
 proj = curlPerform(
   url = url,
   httpheader=c('Content-Type'="application/json", 'X-Redmine-API-Key'=apiKey),
   postfields = fields,
   writefunction = h$update,
   verbose=F
 )
 proj
 ret =RedmineProject(apiKey = apiKey, url = localurl, projectId = identifier)
 return(ret)
}

