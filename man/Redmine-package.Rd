\name{Redmine-package}
\alias{Redmine-package}
\alias{Redmine}
\docType{package}
\title{
R interface to Redmine
}
\description{
Exposes part of the Redmine RESTful API to R. Uses CURL to interface with a project.
}
\details{
\tabular{ll}{
Package: \tab Redmine\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-21\cr
License: \tab GPL-3\cr
}
This package contains a Reference class, RedmineProject, which is instantiated for each Redmine project that is being worked with.
Use RedmineProject$methods() for a list of methods, and RedmineProject$help(<method name>) for specific documentation of each method.
}
\author{
Adam Edgley

Maintainer: Adam Edgley <adam.edgley@gmail.com>

}
\references{
http://www.redmine.org/projects/redmine/wiki/Rest_api
}

\keyword{ package }
