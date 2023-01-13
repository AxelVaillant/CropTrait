#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
email_user <- args[1]
email_PI <- args[2]
project_summary <- args[3]

#-------Get credentials----------------------#
credentials<-read.table(file = "credentials.csv",header = TRUE,sep = "\t")
api_key<-credentials$api_key
api_secret<-credentials$api_secret
croptraitmail <- "croptrait@post.com"


send_email <- httr::POST(
  url = "https://api.mailjet.com/v3.1/send",
  httr::authenticate(
    api_key,
    api_secret
  ),
  httr::add_headers(
    `content-type` = "application/json"
  ),
  body = jsonlite::toJSON(
    list(
      Messages = list(
        list(
          From = list(
            Email = croptraitmail,
            Name = "CropTrait Application"
          ),
          To = list(
            list(
              Email = "axel.vaillant@cefe.cnrs.fr",
              Name = ""
            )
          ),
          Subject = paste0("[CropTrait] Some of your data have been downloaded."),
          TextPart = paste0(
            'Hello,\n\n',
            'Some of your data have been downloaded by a user of CropTrait.',
            'The email of the concerned user is ',email_user,' and his project summary is ',project_summary,'.\n\n',
            'This email is automatic. Please don t answer it.'
          ),
          HTMLPart = paste0(
            'Hello,<br><br>',
            'Some of your data have been downloaded by a user of CropTrait.<br><br>',
            'The email of the concerned user is ',email_user,' and his project summary is ',project_summary,'.<br><br>',
            '<i>This email is automatic. Please don t answer it.</i>'
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
)