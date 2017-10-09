![Build Status](https://travis-ci.org/crimzie/gcho-backend.svg?branch=master)

##Scala backend for an online GURPS™ toolkit.

###Setup
* Install sbt
* Install newman

###Running in dev
* Have MongoDB running on local machine
* `sbt run`
* API description with Swagger: http://localhost:9000/swagger?url=api.yml
* Testing: in `test/` `newman run api_spec.json --ignore-redirects`
