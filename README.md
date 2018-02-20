RedmineR
========

R package that communicates with Redmine, a project management tool, via the RESTful API. The primary motivation for this project was to facilitate automation of R processes, such as:

* Error reporting and handling
* Report delivery (non-HTML)
* Pre-production check of model stability

As such, the functionality completed first has been around issue creation and updating. More user friendly processing of data is on the roadmap for the future.

Please see [http://www.redmine.org/projects/redmine/wiki/Rest_api] for the API reference.

Usage & Design
---------------

The design of the package is to have a single object that represents the Redmine Project, then using that object to facilitate requests to Redmine.

```Rscript
library(RedmineR)
# Reference an existing project
myProject = RedmineProject(
  url = 'http://myRedmine.example.com',
  apiKey = 'myApiKey',
  projectId = 'project_identifier'
  )

# Create a new project
myProject = createRedmineProject(
  projectName = 'My New Project',
  apiKey = 'myApiKey',
  url = 'http:/myredmine.example.com',
  description = 'An example R project'
  )

# You can then create or update an issue.
myProject$createNewIssue(subject="A new issue")

# Or create a new issue with an attached file
myProject$uploadFile("path/to/your/file", description="My file upload")
myProject$createNewIssue(subject="New issue with attachment", attachLastUpload=T)

# To update an issue, you need to have it's ID. These can be retrieved with the getIssueList() method:
myProject$getIssueList()

```

A list of params can be passed to getIssueList(). Supported are:
* subproject_id
* tracker_id
* status_id
* assigned_to_id
* cf_<x> for custom fields, where x is the custom field number

```Rscript

# Updating the issue:
myProject$updateIssue(
  issueId = 1234,
  notes = 'Notes are required',
  # Optional fields
  description = 'new issue description',
  trackerId = 1,
  status_id = 2,
  subject = 'new subject',
  attachLastUpload = T
  )
```

[![Build Status](https://travis-ci.org/adamedgley/RedmineR.svg?branch=master)](https://travis-ci.org/adamedgley/RedmineR)
