library("RSelenium")

remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L)
remDr$open()
remDr$close()
remDr$maxWindowSize()

# Navigation
remDr$navigate("https://www.flightradar24.com/data/airports/bod/arrivals")
remDr$screenshot(display = TRUE)

script <- "document.getElementById('navContainer').hidden = true;"

#Cookie click
cookiesButton <- remDr$findElement(using = 'xpath',"//div[@class='important-banner__close']")
cookiesButton$clickElement()

#document.getElementById('data-section-navbar').hidden;
#remDr$executeScript(script, args = list())

# search until earlier flights text disapear
# explain how to clic, copy paste xpath 
loadmorebutton <- remDr$findElement(using = 'xpath', "//button[@class='btn btn-table-action btn-flights-load' and not(contains(@style,'display: none;'))]")


loadmorebutton <- remDr$findElements(using = 'xpath', "//button[@class='btn btn-table-action btn-flights-load' and not(contains(@style,'display: none;'))]")

unlist(lapply(loadmorebutton, function(x){x$getElementText()}))

loadmorebutton$getElementText()

#ex.ExecuteScript("arguments[0].click();", elementToClick);
loadmorebutton$clickElement()

remDr$executeScript(paste("window.scrollTo(0,",y,")"))

#loadFlight <- function loadFlight(x) {
#  
#}
library(stringr)  

str_detect(remDr$getPageSource(),"hideBtnLaterFlights == true")

x <- loadmorebutton$getElementLocation()$x
y <- loadmorebutton$getElementLocation()$y

x
y

remDr$executeScript(paste("window.scrollTo(0,",y,")"))

remDr$executeScript(paste("window.scrollTo(0,document.body.scrollHeight)"))

remDr$executeScript("arguments[0].click();", loadmorebutton);

loadmorebutton$mouseMoveToLocation(webElement = loadmorebutton)
loadmorebutton$mouseMoveToLocation(x,y)
loadmorebutton$clickElement()
Sys.sleep(30)


remDr$errorDetails()

# search until later flights text disapear 

stop

get data


# DOM Interaction