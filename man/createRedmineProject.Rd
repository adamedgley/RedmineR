% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRedmineProject.R
\name{createRedmineProject}
\alias{createRedmineProject}
\title{Create Redmine Project}
\usage{
createRedmineProject(projectName, apiKey, url, identifier = NULL,
  description = NULL)
}
\arguments{
\item{projectName}{String representing the project name}

\item{apiKey}{Your API key}

\item{url}{URL for your Redmine server}

\item{identifier}{Project identifier in Redmine. If not provided, will be a cleaned version of your project name}

\item{description}{Optional description for your project}
}
\value{
Reference Class of type RedmineProject
}
\description{
Creates a new Redmine Project and returns the object relating to the object. If the project already exists, 
it will return the project object without overwriting it.
}
\examples{
\dontrun{
library(RedmineR)
library(config)
cfg = config::get("redmine")
rm = createRedmineProject("Test Project", cfg$api_key, cfg$url, "tpg", "Test project")
}
}
\seealso{
http://www.redmine.org/projects/redmine/wiki/Rest_Projects#Creating-a-project
}
