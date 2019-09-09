install.packages("RSelenium") #For automated scraping
install.packages("h2o")
library(RSelenium)
library(dplyr)
library(stringr)
library(qdapRegex)

driver <- rsDriver(browser = c("chrome"), port =9515L, chromever = "74.0.3729.6")
remDr <- driver$client

#Use to sto pports running
driver[["server"]]$stop()

remDr$navigate("https://www.glassdoor.co.uk/Interview/Barclays-Interview-Questions-E3456.htm") #To fireup the page

#Click the signin on the popup
signin <- remDr$findElement(using = 'css',".ml-xsm") #Use this when page loads popup
signin <- remDr$findElement(using = 'css',".sign-in") #Use this when page does not load popup
signin$clickElement()

#Fill in signin details
mailid<-remDr$findElement(using = 'css', "[id = userEmail]")
mailid$clickElement()
mailid$sendKeysToElement(list("********@yahoo.com"))
mailpassword<-remDr$findElement(using = 'css', "[id = userPassword]")
mailpassword$sendKeysToElement(list("*******"))
signin <- remDr$findElement(using = 'css',".minWidthBtn")
signin$clickElement()

next_button <- remDr$findElement(using = 'css',".next")
next_button$clickElement()

#URL list of companies
url_link <- c("https://www.glassdoor.co.uk/Interview/Wells-Fargo-Interview-Questions-E8876.htm",
          "https://www.glassdoor.co.uk/Interview/HSBC-Holdings-Interview-Questions-E3482.htm",
          "https://www.glassdoor.co.uk/Interview/J-P-Morgan-Interview-Questions-E145.htm",
          "https://www.glassdoor.co.uk/Interview/Barclays-Interview-Questions-E3456.htm",
          "https://www.glassdoor.co.uk/Interview/BBVA-Interview-Questions-E5731.htm",
          "https://www.glassdoor.co.uk/Interview/Capital-One-Interview-Questions-E3736.htm",
          "https://www.glassdoor.co.uk/Interview/Credit-Suisse-Interview-Questions-E3141.htm",
          "https://www.glassdoor.co.uk/Interview/Deutsche-Bank-Interview-Questions-E3150.htm",
          "https://www.glassdoor.co.uk/Interview/Merrill-Lynch-Interview-Questions-E440.htm",
          "https://www.glassdoor.co.uk/Interview/Standard-Chartered-Bank-Interview-Questions-E226853.htm",
          "https://www.glassdoor.co.uk/Interview/Goldman-Sachs-Interview-Questions-E2800.htm",
          "https://www.glassdoor.co.uk/Interview/Deloitte-Interview-Questions-E2763.htm",
          "https://www.glassdoor.co.uk/Interview/PwC-Interview-Questions-E8450.htm",
          "https://www.glassdoor.co.uk/Interview/EY-Interview-Questions-E2784.htm",
          "https://www.glassdoor.co.uk/Interview/KPMG-Interview-Questions-E2867.htm",
          "https://www.glassdoor.co.uk/Interview/McKinsey-and-Company-Interview-Questions-E2893.htm",
          "https://www.glassdoor.co.uk/Interview/BCG-Consulting-Interview-Questions-E713189.htm",
          "https://www.glassdoor.co.uk/Interview/Bain-and-Company-Interview-Questions-E3752.htm",
          "https://www.glassdoor.co.uk/Interview/Accenture-Interview-Questions-E4138.htm",
          "https://www.glassdoor.co.uk/Interview/IBM-Interview-Questions-E354.htm",
          "https://www.glassdoor.co.uk/Interview/Microsoft-Interview-Questions-E1651.htm",
          "https://www.glassdoor.co.uk/Interview/Google-Interview-Questions-E9079.htm")

coy_name <- c("Wells Fargo", "HSBC", "JP Morgan", "Barclays",
              "BBVA", "Capital one", "Credit Suisse", "Deutsche bank",
              "Merrill Lynch", "Standard Chartered bank", "Goldman Sachs",
              "Deloitte", "PwC", "EY", "KPMG", "Mckinsey & Company",
              "BCG", "Bain & Co", "Accenture", "IBM", "Microsoft", "Google")

company <- cbind(coy_name,url_link)


#Get all the feedbacks on a page
page_df <- data.frame()
get_application_details <- function(coy){
  f <- remDr$findElements(using = 'css', ".interviewOutcomes:nth-child(1)")
  feedbacks = list()
  feedbacks = sapply(f, function(x) {x$getElementText()}) %>% unlist()
  feedbacks <- str_split(feedbacks, "\n", simplify=T)
  
  offer = character(0)
  experience = character(0)
  interview = character(0)
  
  for (j in 1:nrow(feedbacks)){
    
    inter <- ""
    off <- ""
    expe <- ""
    for (i in seq_along(feedbacks[j,])){
      if (grepl("Interview", feedbacks[j,i])){
        inter <- feedbacks[j,i]
      }
      else if (grepl("Offer", feedbacks[j,i])){
        off <- feedbacks[j,i]
      }
      else if (grepl("Experience", feedbacks[j,i])){
        expe <- feedbacks[j,i]
      }
    }
    offer <- append(offer,off)
    experience <- append(experience,expe)
    interview <- append(interview,inter)
  }
  
  #Scrape application/interview experience
  r <- remDr$findElements(using = 'css', ".interviewReviewDetails")
  review = list()
  review = sapply(r, function(x) {x$getElementText()}) %>% unlist()
  
  application <- character(0)
  interv <- character(0)
  for (i in 1:length(review)){
    hold <- ""
    app_detail <- ""
    int_detail <- ""
    
    hold <- str_split(as.character(review[i]), "\\nInterview\\n")[[1]]
    app_detail <- gsub("Application\\n", "",hold[1] )
    int_detail <- str_split(as.character(hold[2]), "\\nInterview Questions\\n")[[1]][1]
    application <- append(application,app_detail)
    interv <- append(interv,int_detail)
  }
  page_df <- data.frame(Interview_exp=interview, Experience=experience,
                        App_details=application, Int_details = interv,
                        Offer=offer)
  page_df$Company_name <- coy
  return(page_df)
}

app_df <- data.frame()

#Function to iterate through pages and get reviews
get_dataFrame <- function(coy, link){
  remDr$navigate(link)
  coy <- coy
  for(p_no in 1:50){
    df <- get_application_details(coy) #Returns offer, experience & interview df
    app_df <- rbind(df, app_df)
    
    #Press next
    next_button <- remDr$findElement(using = 'css',".next")
    next_button$clickElement()
  }
  return(app_df)
}

temp_df <- data.frame()
final_df <- data.frame()

#Iterates through list of companies to get their reviews
for (ind in 1:length(company[,1])){
  coy <- company[ind,][1]
  link <- company[ind,][2] 
  temp_df <- get_dataFrame(coy, link)
  final_df <- rbind(final_df,temp_df)
  print(paste0(ind,":",scales::percent(ind/length(company[,1]))))
  flush.console()
}

write.csv(final_df, file="Glassdoor_update.csv")

#Stop server
#----
remDr$close()
driver$server$stop()
driver$server$process
