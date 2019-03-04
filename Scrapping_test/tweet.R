library(twitteR)
library(ROAuth)
library(rtweet)

api_key <- "STBRYKp94wAGStQ0j0BA4h8eq"
api_secret <- "aRuTMkDaBONAQAR1ElNl18Ol0wzAHNvqUa0CE0cYWr3GtpwKdS"
access_token <- "1847123360-E2tPObEpAGXAonLgXDlYTgemO93Sjos7Zk8q14l"
access_token_secret <- "20u0suiEauEHc8KQNFp82ckj7vWYdn3FTiK6kk5iNLPqu"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# had to run this to make it work
# file.remove(".httr-oauth")


tmls <- get_timelines("SenSanders", n = 30)

