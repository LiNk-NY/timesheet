setwd("~/Documents/Admin/Tutoring/")

usertkn <- googlecalendar::gc_auth(
    new_user = TRUE,
    key = Sys.getenv("GKEY"),
    secret = Sys.getenv("GSECRET"),
    token = NULL,
    cache = TRUE
)

saveRDS(usertkn, file = "googlecalendar_token.rds")
