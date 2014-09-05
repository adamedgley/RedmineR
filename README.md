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

