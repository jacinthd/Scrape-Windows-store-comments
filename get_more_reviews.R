list_seciter

more_url<-xpathApply(list_seciter,"//a[@id='moreReviews']")[[1]]%>%xmlGetAttr(.,"href")

list_thirditer<-htmlTreeParse(getURL( more_url, useragent="R"))$children$html

xpathApply(list_thirditer, "//ul/li/div[not(@data-href)][@itemprop]")

xpathApply(list_thirditer, "//div[@class='reviewText']")


#tryin to check what happens when there is no more URL's left
more_url_check<-"http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1/reviews?after=cHQ6R2V0UmV2aWV3cyBwZDpkZTJkZjI3OS00ODVkLTQ5YmItYjUzZS0zZjZhMmE5NDAxYzEgY2M6VVMgb3Q6TGF0ZXN0IHN5bmM6OTIyMzM3MjAzNjU5NjYyMDMzODt1cGQ6MjUxOTgyNjYxMTE5NTYyOTk5OSByaWQ6YjhmNzAxNmYtYjEwNS00MGZjLTgyOGMtZmZkMWMxYmYwNzRl"

test_tmp<-htmlTreeParse(getURL( more_url_check, useragent="R"))$children$html

xpathApply(test_tmp, "//ul/li/div[not(@data-href)][@itemprop]")

#have to figure out how to use tryCatch
variab<-tryCatch(xpathApply(test_tmp,"//a[@id='moreReviews']")[[1]],
         error=function(cond){
           message(paste("URL does not seem to exist"))
           message("Here's the original error message:")
           message(cond)
           return(0)
         })


tryCatch(print(paste("log of", input, "=", log(input))),           
         warning = function(warning) {print(paste("negative argument", input)); 
                                log(-input)},
         error = function(error) {print(paste("non-numeric argument", input));
                              NaN})


