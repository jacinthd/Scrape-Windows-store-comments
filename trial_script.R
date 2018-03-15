#get app category
app.category<-RobustXpath(list_seciter,1,"//strong[@itemprop='applicationCategory']")

#get app name 
app.name<-RobustXpath(list_seciter,1,"//h1[@itemprop='name']")

#get app price,aggregate rating and number of reviews
app.price<-RobustXpath(list_seciter,1,"//div[@id='offerRating']//span[@itemprop='price']")

app.no.of.reviews<-RobustXpath(list_seciter,1,"//div[@id='offerRating']//span[not(@itemprop)]")

app.aggregate.rating<-RobustXpath(list_seciter,0,"//div[@id='offerRating']//meta[@itemprop='ratingValue']")

#get app description
app.description<-RobustXpath(list_seciter,1,"//div[@id='appDescription']")

#get app publisher
app.publisher<-RobustXpath(list_seciter,1,"//div[@id='publisher']/span")

#get size of app
app.size<-RobustXpath(list_seciter,1,"//div[@id='packageSize']/span")

#get date of last update
app.last.update<-RobustXpath(list_seciter,1,"//div[@id='releaseDate']/span")

#get date of publication
app.date.published<-RobustXpath(list_seciter,0,"//div[@id='releaseDate']//meta[@itemprop='datePublished']")

#get app version
app.version<-RobustXpath(list_seciter,1,"//div[@id='version']/span")

#get app's supported languages
app.languages<-RobustXpath(list_seciter,1,"//div[@id='languages']//span") #All languages can be extracted!

#get the devices that app works with
app.compatible.devices<-RobustXpath(list_seciter,1,"//div[@id='softwareRequirements']/ul/li")

#get personal info acesss required by app 
app.required.permissions<-RobustXpath(list_seciter,1,"//div[@id='hardwareRequirements']/ul/li")



RobustXpath<-function(input.list,input.function,input.path){
  found.value<-tryCatch(xpathApply(input.list,input.path),
                        error=function(cond){
                          return(NA)
                        })
  if(input.function){
    return(sapply(found.value,xmlValue))
  } else {
    return(sapply(found.value,function(x) xmlGetAttr(x,"content")))
  }
}
