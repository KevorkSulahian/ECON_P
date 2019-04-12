library(ROAuth)
library(rtweet)
library(twitteR)

api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# had to run this to make it work
# file.remove(".httr-oauth")


tmls <- get_timelines(c("SenSanders"), n = 1)

tmls <- tmls[is.na(tmls$reply_to_status_id),]

tmls <- tmls[c("text", "screen_name", "display_text_width", "favorite_count", "retweet_count", "name", "location", "followers_count", "friends_count",
               "listed_count", "statuses_count", "favourites_count", "verified")]
writexl::write_xlsx(tmls, "df.xlsx")


users <- c("Benioff", "tim_cook", "jack", "levie", "elonmusk","bhalligan","MichaelDell","aneelb","jack","JonasPrising","drewhouston","jeremys","eldsjal","WesternUnionCEO","satyanadella","JohnLegere","YuanqingYang","anandmahindra","alanjope","finkd","hmikitani","larryculpjr","gary_kelly","mtbarra","SteveHare","AntonioNeri_HPE","BruceDBroussard","jonoringer","andy_penn","spencerrascoff","sasan_goodarzi","readmark","BillRMcDermott","MedtronicCEO","MarkVHurd","johnfallon","devinwenig","jptricoire","ChuckRobbins","MarkOkerstrom","SteveEasterbrk","jgsilverman","WarrenBuffett","JohnChen","JeffBezos","jamiejlerner","Dan_Schulman","michael_saylor","jmalvpal","DanDSpringer","reedhastings","GinniRometty","evanspiegel","BoeingCEO","RobertIger","LisaSu","andrew_anagnost","DavidJHenshall","Gary_Norcross","jeffyabuki","KenXieFortinet","ramirahim","MicronCEO","NetAppCEO","stevemollenkopf","JWhitehurst")

df <- get_timelines(users, n = 2000)

# df <- df[is.na(df$reply_to_status_id),]

rownames(df) <- NULL

df <- df[c("name","screen_name","text","display_text_width", "favorite_count",'reply_to_status_id', "retweet_count", "location", "followers_count", "friends_count",
           "listed_count", "statuses_count", "favourites_count", "verified")]

writexl::write_xlsx(df, "df.xlsx")





