#install.packages("highcharter")
#Load relevant libraries

library(tidyverse) #for data wrangling
library(h2o) #ML package
library(highcharter) #for charts
library(ggplot2)
library(tidyquant)

head(new_data)

#Data preparation
data <- read_csv("Glassdoor_update2.csv") #load data
data %>% as.tibble()

new_data[1:6,] %>% as.tibble()

#Feature engineering
data$App_details[1]
str_extract(data$App_details[1], "I applied.*?\\.")

#Extract referral source
new_data <- data %>%
  mutate(referral = str_extract(App_details, "I applied.*?\\."),
         referral = str_replace_all(referral, "I applied|through|an |a |\\.","") %>% str_trim() %>% str_to_title()) %>%
  add_count(referral) %>%
  filter(n>2)

#Extract process time
str_extract(new_data$App_details[2], "\\d+.*?(week|month|day)") %>%
  str_extract("\\d+")

new_data <- new_data %>%
  mutate(process_time = str_extract(App_details, "\\d+.*?(week|month|day)"), 
         process_time = case_when(
           str_detect(process_time, "week") ~ as.numeric(str_extract(process_time, "\\d+"))*7,
           str_detect(process_time, "month") ~ as.numeric(str_extract(process_time, "\\d+"))*30,
           str_detect(process_time, "day") ~ as.numeric(str_extract(process_time, "\\d+")),
           TRUE ~ NA_real_
         ))

new_data <- read_csv("New Data.csv") #load data
new_data <- new_data %>% 
  mutate_if(is.character, as.factor)

#Add Industry sector of company
new_data <- new_data %>%
  mutate(sector = case_when(
    Company_name=="Google"| Company_name=="IBM"|
      Company_name=="Microsoft"|Company_name=="Accenture" ~ "Tech",
    
    Company_name=="Barclays"|Company_name=="BBVA"|
      Company_name=="Capital one"|Company_name=="Credit Suisse"|Company_name=="Goldman Sachs"|
      Company_name=="Deutsche bank"|Company_name=="HSBC"|
      Company_name=="JP Morgan"|Company_name=="Merrill Lynch"|
      Company_name=="Standard Chartered bank"|Company_name=="Wells Fargo" ~ "Banking",
    
    Company_name=="Bain & Co"|Company_name=="BCG"|
      Company_name=="Mckinsey & Company" ~ "Consulting",
    
    Company_name=="Deloitte"|Company_name=="EY"|
      Company_name=="KPMG"| Company_name=="PwC" ~ "Consulting",
    TRUE ~ NA_character_ ), sector=as.factor(sector) )

#Explore data
company_count <- new_data %>% 
  count(sector,Company_name) %>%
  rename(total = n)


#Plot success rate chart 
success_rate<- new_data %>% 
  filter(Offer != "No Offer") %>%
  count(Company_name) %>%
  rename(offer = n) %>%
  inner_join(company_count, by= "Company_name") %>%
  mutate(offer_rate = offer/total*100) %>% 
  arrange(desc(offer_rate))

#Chart of Rate of Success @ interviews
hchart(success_rate, "column", hcaes(x=Company_name,y=offer_rate))
hchart(success_rate, "column", hcaes(x=Company_name,y=offer_rate,color=sector))
hchart(success_rate, "column", hcaes(x=Company_name,y=offer_rate,group=sector))

#Plot interview difficulty chart 
interview_diff <- new_data %>% 
  filter(Interview_difficulty == "Difficult Interview") %>%
  count(Company_name) %>%
  rename(difficulty = n) %>%
  inner_join(company_count, by= "Company_name") %>%
  mutate(diff_rate = difficulty/total*100) %>% 
  arrange(desc(diff_rate))

diff_vs_success <- success_rate %>%
  select(Company_name, offer_rate, sector) %>%
  inner_join(interview_diff %>% 
               select(Company_name, diff_rate))

hchart(interview_diff, "column", hcaes(x=Company_name,y=diff_rate,group=sector))
hchart(interview_diff, "column", hcaes(x=Company_name,y=diff_rate,group=sector),color=c("deepblue","red","green","purple"))

hchart(diff_vs_success, "scatter", hcaes(x=offer_rate,y=diff_rate, group=Company_name)) %>% hc_legend(enabled=F)
hchart(diff_vs_success, "scatter", hcaes(x=offer_rate,y=diff_rate, group=sector),color=c("deepblue","red","yellow","purple"))


#Plot interview/difficulty chart 
interview_exp <- new_data %>% 
  add_count(Interview_experience) %>%
  filter(Offer == "Offer") %>%
  group_by(Interview_experience) %>%
  summarise(count=n(),total=mean(n)) %>%
  mutate(percentage_offer=count/total *100, feature="Experience") %>%
  filter(!(is.na(Interview_experience))) %>%
  select(-count,-total)

interview_diff <- new_data %>% 
  add_count(Interview_difficulty) %>%
  filter(Offer == "Offer") %>%
  group_by(Interview_difficulty) %>%
  summarise(count=n(),total=mean(n)) %>%
  mutate(percentage_offer=count/total *100, feature="Difficulty") %>%
  filter(!(is.na(Interview_difficulty))) %>%
  select(-count,-total)

names(interview_exp) <- c("type","percentage_offer","feature")
names(interview_diff) <- c("type","percentage_offer","feature")

new_data %>% 
  add_count(Interview_difficulty) %>%
  filter(Offer == "Offer" & Interview_experience == "Positive Experience" & !(is.na(Interview_difficulty))) %>%
  count(Interview_difficulty) %>%
  mutate(sorting_variable=c(2,3,1)) %>%
  arrange(sorting_variable) %>%
  hchart("column", hcaes(x=Interview_difficulty, y=n))

new_data %>% 
  filter(Offer == "Offer" & Interview_experience == "Positive Experience" & !(is.na(Interview_difficulty))) %>%
  count(Interview_experience,Interview_difficulty) %>%
  hchart("sankey", hcaes(from = Interview_experience, to=Interview_difficulty, weight=n))

dat <- data.frame(from = c('AT', 'DE', 'CH', 'DE'),
                  to = c('DE', 'CH', 'DE', 'FI'),
                  weight = c(10, 5, 15, 5)) %>%
  toJSON()

#PLOT THIS CHART IN REPORT
hchart(arrange(rbind(interview_exp,interview_diff), desc(percentage_offer)), "column",
       hcaes(x=type,y=percentage_offer,group=feature))

#Plot interview Difficulty chart 
new_data %>% 
  add_count(Interview_difficulty) %>%
  filter(Offer == "Offer") %>%
  group_by(Interview_difficulty) %>%
  summarise(count=n(),total=mean(n)) %>%
  mutate(percentag_offer=count/total *100)%>%
  filter(!(is.na(Interview_difficulty))) %>%
  arrange(desc(percentag_offer)) %>%
  hchart("column", hcaes(x=Interview_difficulty,y=percentag_offer))%>%
  hc_title(text="Title")

#Plot Referral chart 
hchart(new_data$Referral, "pie")

diff_vs_success <- success_rate %>%
  select(Company_name, offer_rate, sector) %>%
  inner_join(interview_difficulty %>% 
               select(Company_name, diff_rate))

#Change offer in data to Offer & No Offer
new_data <- new_data %>%
  mutate(Offer = str_replace_all(Offer, "Accepted |Declined ",""))

#Change offer feature to factor
new_data <- new_data %>%
  mutate(Offer = factor(Offer))

#Drop irrelevant columns
new_data <- new_data %>% select(-n,-App_details,-Int_details,-X1)
noNa_data <- new_data[complete.cases(new_data),]

write.csv(new_data, "New Data.csv", row.names = F)

#-------
#Machine learing 
h2o.init()
#h2o.shutdown(F)

names(new_data) <- c("Interview_difficulty","Interview_experience","Offer","Company_name","Referral","process_time","sector" )

data_h2o <- as.h2o(select(new_data,-sector,-Company_name))

splits <- h2o.splitFrame(data_h2o, ratios = c(0.6,0.2),
                         destination_frames = c("train", "valid", "test"), seed = 1234)

train = h2o.getFrame("train")
test = h2o.getFrame("test")
valid = h2o.getFrame("valid")
response = 3
features = setdiff(1:ncol(train), response)

h2o.saveModel(best_model)
best_m <- h2o.loadModel("myGBM_Model")

#GBM
model = h2o.gbm(x=features,y=response,training_frame = train, validation_frame = valid)

#GLM
model = h2o.glm(x=features,y=response,training_frame = train, validation_frame = valid, family = "binomial")

#Random forest
model = h2o.randomForest(x=features,y=response,validation_frame = valid, training_frame = train)

#Extract Model ID from model
getID = function(model) {
  ID = model@model_id
  return(ID)
}

#Function to check validation set accuracy
test_accuracy = function(model) {
  acc <- h2o.performance(model,newdata = test)
  auc <- h2o.auc(acc)
  return(auc)
}

models = c("h2o.randomForest", "h2o.deeplearning" ,"h2o.gbm", "h2o.naiveBayes")

#Run different models
list_of_models =invoke_map(models, x=features,y=response,training_frame =train,validation_frame = valid)

list_of_models %>% 
  map_dbl(test_accuracy)

model_ID <- list_of_models %>%
  map_chr(getID)

best_model <- h2o.getModel(getID(model))

best_model <- h2o.getModel(model_ID[3])
best_model_without_company <- h2o.getModel(model_ID[3])

h2o.predict(best_model, test[33,])

h2o.varimp_plot(best_model) #importance of variables
h2o.varimp_plot(best_model_without_company) #importance of variables

#Google LIME: Local Interpretable Model_Agnostic Explanation

#Importance of vairables in the variables
h2o.partialPlot(best_model,train,"Interview_experience")
h2o.partialPlot(best_model,train,"Referral")
h2o.partialPlot(best_model,train,"Interview_difficulty")
process_time_plot <- h2o.partialPlot(best_model,train,"process_time", nbins = 100)
process_time_df <- as.data.frame(process_time_plot)

process_time_df %>%
  filter(process_time < 200) %>%
  hchart("line", hcaes(x=process_time,y=mean_response))

referral_plot <- h2o.partialPlot(best_model,train,"referral")
referral_plot %>%
  filter(process_time < 200) %>%
  hchart("line", hcaes(x=process_time,y=mean_response))

hchart(new_data, "column", hcaes(x=process_time,y=mean_response))

new_data %>%
  filter(Offer == "Offer") %>%
  group_by(Company_name) %>%
  summarise(comp_avg = mean(process_time, na.rm = T)) %>%
  arrange(desc(comp_avg)) %>%
  hchart("column", hcaes(x=Company_name,y=comp_avg))

h2o.saveModel(best_model, "my_project_model")

referral_plot <- h2o.partialPlot(best_model,train,"Referral", plot=F) %>% as.data.frame() %>% arrange(-mean_response)
hchart(referral_plot, "bar", hcaes(x=Referral,y=mean_response))

#----
#MEDIUM BLOGPOST VISUALIZATIONS
company_count <- new_data %>% 
  count(sector,Company_name) %>%
  rename(total = n)

interview_diff <- new_data %>% 
  filter(Interview_difficulty == "Difficult Interview") %>%
  count(Company_name) %>%
  rename(difficulty = n) %>%
  inner_join(company_count, by= "Company_name") %>%
  mutate(diff_rate = difficulty/total*100) %>% 
  arrange(desc(diff_rate))

hchart(interview_diff, "column", hcaes(x=Company_name,y=diff_rate,group=sector)) %>%
  hc_title(text="Percentage Review with Difficult Interview")

success_rate<- new_data %>% 
  filter(Offer != "No Offer") %>%
  count(Company_name) %>%
  rename(offer = n) %>%
  inner_join(company_count, by= "Company_name") %>%
  mutate(offer_rate = offer/total*100) %>% 
  arrange(desc(offer_rate))

diff_vs_success <- success_rate %>%
  select(Company_name, offer_rate, sector) %>%
  inner_join(interview_diff %>% 
               select(Company_name, diff_rate))

hchart(diff_vs_success, "scatter", hcaes(x=offer_rate,y=diff_rate, color=sector)) %>%
  hc_add_series_scatter(dataLabels = list(enabled = TRUE, format = '{point.name}')) %>%
  hc_title(text="Interview Difficulty vs Offer Rate")%>%
  hc_xAxis(title = list(text = "% of people that got Offers")) %>%
  hc_yAxis(title = list(text = "% of people that had difficult interviews")) %>%
  hc_legend(enabled=F)

highchart() %>%
  hc_add_series_scatter(diff_vs_success$offer_rate, diff_vs_success$diff_rate,
                        label = diff_vs_success$Company_name,
                        dataLabels = list(enabled = TRUE, format = '{point.label}')) %>%
  hc_tooltip(crosshairs = TRUE, pointFormat = "{point.label}:{point.x:,.2f},{point.y:,.2f}") %>%
  hc_title(text="Interview Difficulty vs Offer Rate")%>%
  hc_xAxis(title = list(text = "% of people that got Offers")) %>%
  hc_yAxis(title = list(text = "% of people that had difficult interviews"))

ggplot(diff_vs_success, aes(x=offer_rate,y=diff_rate)) +
  geom_point(aes(colour=sector, size=3)) +
  geom_text(aes(label=Company_name), size=3, hjust = 0, nudge_x = 0.1) +
  labs(title="Interview Difficulty vs Offer Rate",
       x="Offer Rate",y="Interview Difficulty") +
  guides(size=F) +
  theme_tq()
