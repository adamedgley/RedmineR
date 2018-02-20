#' @name RedmineProject-class
#' @title Redmine Project object
#' @description Reference Class used to interact with a Redmine Project
#' @field apiKey The API key for your user, retrieved from your account page. Protect this key, or consider a special system account with limited permissions.
#' @field url The URL of your Redmine instance, including http:// or https://. At this stage, the package does not support Redmine addresses such as http://<domain>/redmine. Different ports are supported though.
#' @field projectId The project identifier
#' @field projectDetails List containing all information about the project
#' @field lastUpload List containing the last upload details so it can be attached to an issue
#' @field availableTrackers The trackers that are available in the project
#' @field issueCategories The issue categories that are available in the project
#' @field assignedUsers 
#' @exportClass RedmineProject
#' @export RedmineProject
#' @import RCurl
#' @import methods
#' @importFrom methods new
RedmineProject <- setRefClass(
  Class="RedmineProject", 
  fields=list(apiKey="character", 
              url = "character",
              projectId = "character",
              projectDetails = "list",
              lastUpload = "list",
              availableTrackers = "data.frame",
              issueCategories = "data.frame",
              assignedUsers = "data.frame"
  ),
  methods=list(
    initialize=function(apiKey, url, projectId){
      'Initialise the class \n
      apiKey: Required character string. The API key obtained from your Redmine user profile. RESTful API must be enable by the Redmine administrator \n
      url: Required character string. The URL for your Redmine installation. Note that this is stripped to a base url. \n
      projectId: Required character string. The Redmine project identifier.'
      require(RCurl)
      require(RJSONIO)
      stopifnot(is.character(apiKey), is.character(url), is.character(projectId))
      # Clean the url down to the base url
      localUrl = regmatches(url, regexec("(http){1}s{0,1}:\\/{2}([a-z0-9]+\\.?){2,6}(:\\d+){0,1}",  url))[[1]][1]
      .self$apiKey = apiKey
      .self$url = localUrl
      .self$projectId = projectId
      projUrl = paste0(.self$url, "/projects/", .self$projectId, ".json?include=trackers,issue_categories")
      tryCatch({.self$projectDetails = fromJSON(getURL(projUrl, httpheader=c('X-Redmine-API-Key'=apiKey)))},
               error=function(e){
                 stop('Could not retrieve project', call.=T)
               })
      .self$availableTrackers = as.data.frame(do.call('rbind', .self$projectDetails$project$tracker))
      .self$issueCategories = as.data.frame(do.call('rbind', .self$projectDetails$project$issue_categories))
      # TODO: Extract member list
      .self$fetchMembers()
      
    },
    getIssueList = function(params=list()){
      'Retrieve the issue list for the project.
      params: Optional named list of filter terms from Redmine. Following are permitted by the API:
       - subproject_id - limit issues to subproject
       - tracker_id - Only retrieve issues for given tracker id
       - status_id - Only retrieve issues for the given status id
       - assigned_to_id - User id currently assigned
       - cf_<x> Custom field, where x is the field id, and valid value for the field'
      issueUrl = paste0(.self$url, "/issues.json")
      x = getForm(
        issueUrl,
        .params=c(project_id = .self$projectId, params),
        .opts=list(
          httpheader=c('X-Redmine-API-Key'=.self$apiKey)
        )
      )
      x = fromJSON(x)
      return(x)
    },
    uploadFile = function(filePath, description=NULL){
      'Upload file to Redmine, ready for attaching to an issue.
      filePath: Required valid path to file
      description: Optional character string describing the file'
      stopifnot(length(filePath)==1,
                is.character(filePath),
                file.exists(filePath)
      )
      uploadUrl = paste0(.self$url, "/uploads.json")
      commandString = paste0("curl --data-binary \"@", filePath, "\" -H \"Content-Type: application/octet-stream\" -H \"X-Redmine-API-Key: ", .self$apiKey, "\" -X POST ", uploadUrl)
      x = system(command=commandString, ignore.stderr=T, intern=T)
      lastUpload <<- list(uploads=list(
        c(list(
          token=unname(fromJSON(x)$upload),
          filename=basename(filePath),
          description=as.character(description),
          content_type = unname(guessMIMEType(filePath, default='text/plain'))
        )
        )
      )
      )                 
      return(.self$lastUpload)
    },
    updateIssue = function(issueId, notes, trackerId=NULL, description=NULL, statusId=NULL, subject=NULL, attachLastUpload=F){
      'Updates an issue on the Redmine project.
      issueId: Required Numeric - The issue number to update
      notes: Required Character - The notes to add to the issue
      description: Optional character - Update the issue description
      trackerId: Optional - Change the tracker that this issue uses (numeric)
      statusId: Optional Numeric - Update to this status ID. Must be a valid status id and the workflow must permit the transition
      subject: Optional character - Change the description to this character string.
      attachLastUpload: Optional logical - Attach last upload. Default is false.
      '
      stopifnot(!is.null(issueId),
                !is.null(notes),
                is.character(notes)
      )
      updateUrl = paste0(.self$url, "/issues/", issueId, ".json")
      fields = list(notes=notes)
      
      if (is.numeric(statusId))
        fields = c(fields, 
                   status_id = statusId)
      if (is.numeric(trackerId))
        fields = c(fields, 
                   tracker_id = trackerId)
      if (is.character(description))
        fields = c(fields,
                   description=description)
      if (attachLastUpload)
        fields = c(fields, 
                   .self$lastUpload)
      fields = list(issue=fields)
      ret = httpPUT(updateUrl, httpheader=c('X-Redmine-API-Key'=.self$apiKey, 'Content-Type'='application/json'), content=toJSON(fields))
      return(ret)
    },
    createNewIssue = function(
      subject, 
      description = '',
      trackerId = NULL, 
      categoryId = NULL,
      attachLastUpload = F,
      statusId = NULL, 
      assignedToId = NULL,
      parentIssueId = NULL,
      customFields = list(),
      watcherUserIds = NULL,
      verbose=F
    ){
      'Create a new issue
      trackerId: REQUIRED - Numeric ,
      subject, 
      description, 
      categoryId = NULL, 
      statusId = NULL,
      categoryId = NULL
      assigned_to_id = NULL,
      parent_issue_id = NULL,
      custom_fields = list(),
      watcher_user_ids = NULL
      '
      issueUrl = paste0(.self$url, '/issues.json')
      stopifnot(is.character(subject))
      stopifnot(is.logical(verbose))
      fields = list(
        project_id = .self$projectId,
        subject = subject
      )
      
      if (is.character(description))
        fields = c(fields, description = description)
      
      if (is.numeric(trackerId))
        if (trackerId %in% .self$availableTrackers$id)
          fields = c(fields, tracker_id = trackerId)
      else
        stop('Invalid tracker id. Please see $availableTrackers')
      
      if (is.numeric(categoryId))
        if (categoryId %in% .self$issueCategories$id)
          fields = c(fields, category_id = categoryId)
      
      if (attachLastUpload)
        fields = c(fields, 
                   .self$lastUpload)
      if (is.numeric(statusId))
        fields = c(fields, status_id = statusId)
      
      if (is.numeric(assignedToId))
        fields = c(fields, assigned_to_id = assignedToId)
      
      if (is.numeric(parentIssueId))
        fields = c(fields, parent_issue_id = parentIssueId)
      
      if (is.list(customFields)) {
        if(length(customFields > 0)) {
          stopifnot(grepl("^cf_([0-9])+$", names(customFields)))
          customFieldIds = gsub("cf_", "", names(customFields))
          #customFieldIds = as.list(customFieldIds)
          customFieldsFinal = list()
          for (i in 1:length(customFields)) {
            customFieldsFinal = c(customFieldsFinal, list(list(id = as.numeric(customFieldIds[i]), value=customFields[[i]])))
          }
          fields = c(fields, custom_fields = list(customFieldsFinal))
        }
      }
      
      if (is.numeric(watcherUserIds))
        fields = c(fields, watcher_user_ids = watcherUserIds)
      
      fields = list(issue = fields)
      fields = toJSON(fields)
      cat(fields)
      h = basicTextGatherer()
      ret = curlPerform(
        url = issueUrl,
        httpheader=c('Content-Type'="application/json", 'X-Redmine-API-Key'=.self$apiKey),
        postfields = fields,
        writefunction = h$update,
        verbose=verbose
        )
      return(ret)
    },
    show = function(){
      cat("Project Name: ", .self$projectDetails$project$name, "\n")
      cat("Project Identifier: ", .self$projectDetails$project$identifier, "\n")
      cat("Description: \n", .self$projectDetails$project$description, "\n")
      cat("Available Trackers: \n")
      print(.self$availableTrackers, row.names=F)
      cat("\n")
      if (length(.self$issueCategories) > 0){
        cat("Available issue categories:\n")
        print(.self$issueCategories, row.names=F)
      }
      cat("Member list:\n")
      print(.self$assignedUsers)
      
    },
    fetchMembers = function(){
      'Fetches member list for the project and update the object'
      require(foreach)
      x = fromJSON(getURL(paste0(.self$url, "/projects/", .self$projectId, "/memberships.json"),  httpheader=c('X-Redmine-API-Key'=.self$apiKey)))
      users = as.data.frame(
        foreach(s = x$memberships, .combine = rbind) %do%
          do.call(cbind, s$user)
      )
      .self$assignedUsers = users
      return(users)
    }
  )
)
