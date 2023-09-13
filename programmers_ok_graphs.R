# going to mess with the data here and then make it pretty later
library(stringr)
library(gganimate)
library(ggplot2)

#2014 data
#mental_health_dta<-read.csv("C:/Users/Ndidi/Desktop/2022-23/spring/dsci 304/final/tech_mental_health_data/survey.csv")
dta_2016<-read.csv("C:/Users/Ndidi/Desktop/2022-23/spring/dsci 304/final/2016_data/mental-health-data-2016.csv")
View(mental_health_dta)

#some data cleaning

#age
dta_2016$age <- dta_2016$What.is.your.age.

dta_2016$age<-ifelse(dta_2016$age>0 | dta_2016$age<100, dta_2016$age, "NA")
#table(dta_2016$age)
#dta_2016$age <- dta_2016$What.is.your.age.[dta_2016$What.is.your.age.>0 & dta_2016$What.is.your.age.<100]

#gender
#table(dta_2016$gender)
# woman is 2, man is 1, other is 3
dta_2016$gender <- NA
dta_2016$gender <- str_replace(dta_2016$What.is.your.gender., "Female|female|I identify as female.|female ,Female assigned at birth |F|Woman|fm|f|Cis female |Transitioned|M2F|Genderfluid (born female)|Female or Multi-Gender Femme|Female |woman|female/woman|Cisgender Female|fem|Female (props for making this a freeform field, though)| Female|Cis-woman|female-bodied; no feelings about gender|AFAB", "2")
dta_2016$gender <- str_replace(dta_2016$gender, "Bigender|non-binary|Other/Transfeminine|Androgynous|Other|nb masculine|none of your business|genderqueer|Human|Genderfluid|Enby|genderqueer woman|mtf|Queer|Agender|Fluid|Nonbinary|human|Unicorn|Genderqueer|Genderflux demi-girl|Transgender woman", "3")
dta_2016$gender[dta_2016$gender != 2 & dta_2016$gender != 3] <- 1
# now save with labels
dta_2016$gender <- factor(dta_2016$gender) 
dta_2016$gender<-factor(
  dta_2016$gender, levels = 1:3,
  labels = c("Male", "Female", "Other")
)

#rename the locations
#table(dta_2016$What.country.do.you.work.in.)
dta_2016$location <- dta_2016$What.country.do.you.work.in.
dta_2016$location_fac<-factor(dta_2016$location)

#label the company sizes
#table(dta_2016$How.many.employees.does.your.company.or.organization.have.)
dta_2016$company_size_lab <- factor(dta_2016$How.many.employees.does.your.company.or.organization.have.)
us_uk_dta <- subset(dta_2016, location_fac == "United Kingdom" | location_fac == "United States of America")
table(us_uk_dta$location)

#plot 1
ggplot(data=us_uk_dta, aes(location_fac))+
  geom_bar(aes(fill = gender))+
  ggtitle("Gender Distribution of Tech Workers By Country (2016)")+ 
  labs(y= "Number of Respondents", x = "Country", color = "Gender")+
  scale_fill_manual(name="Gender",values=c("dodgerblue","hotpink", "darkgreen"),labels=c("Male","Female", "Other"))

#plot 2
us_uk_dta$age[us_uk_dta$age > 100] <- NA
table(us_uk_dta$age)
ggplot(us_uk_dta, aes(x=location_fac, y = age, fill = gender)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('dodgerblue', 'hotpink', 'darkgreen')) +
  labs(title='Boxplot of Ages of Tech Industry Workers (2016)',
       x='Country of Employment',
       y='Age (in Years)', 
       fill='Gender')

#plot 3
us_uk_dta$company_size<-factor(
  us_uk_dta$company_size, levels = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"),
  labels = 1:6
)
#table(us_uk_dta$company_size_lab)

us_uk_dta$company_size_lab<-factor(
  us_uk_dta$company_size, levels = 1:6,
  labels = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000")
)
us_uk_dta$ill <- factor(us_uk_dta$Do.you.currently.have.a.mental.health.disorder.)
#table(us_uk_dta$ill)

p1<-ggplot(us_uk_dta, aes(x=age, y=ill, color=gender))+
  geom_point(position="jitter")+
  ggtitle("Effect of Age, Sex, and Company Size on Mental Illness")+
  labs(subtitle = 'Company Size: {closest_state} Employees', y = "Does Respondent have a mental illness?")
p1_anim<-p1+transition_states(company_size_lab)
animate(p1_anim, renderer=gifski_renderer(), fps=7)

#making stuff for plot 4
us_uk_dta$neg_mental_l <- us_uk_dta$Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.
us_uk_dta$neg_mental <-factor(
  us_uk_dta$neg_mental_l, levels = c("Yes", "Maybe", "No"),
  labels = 1:3
)
table(us_uk_dta$neg_mental_lab)
us_uk_dta$neg_mental_lab<-factor(
  us_uk_dta$neg_mental, levels = 1:3,
  labels = c("Yes", "Maybe", "No")
)
us_uk_dta$neg_physical_l <- us_uk_dta$Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences.
us_uk_dta$neg_physical <-factor(
  us_uk_dta$neg_physical_l, levels = c("Yes", "Maybe", "No"),
  labels = 1:3
)
us_uk_dta$neg_physical_lab<-factor(
  us_uk_dta$neg_physical, levels = 1:3,
  labels = c("Yes", "Maybe", "No")
)

# combo plots 4
men_v_phys <- subset(us_uk_dta, !is.na(neg_physical) & !is.na(neg_mental))
table(men_v_phys$neg_mental)

menp<- ggplot(data=men_v_phys, aes(neg_mental_lab))+
  geom_bar(aes(fill = gender))+
  ggtitle("Effect of Company Size on Preceived Negative Consequences of Mental Health")+ 
  labs(y= "Number of Respondents", x = "Will discussing health with employer have negative consequences?", subtitle = 'Company Size: {closest_state} Employees', color = "Gender")+
  scale_fill_manual(name="Gender",values=c("dodgerblue","hotpink", "darkgreen"),labels=c("Male","Female", "Other"))


physp <- ggplot(data=men_v_phys, aes(neg_physical_lab))+
  geom_bar(aes(fill = gender))+
  ggtitle("Effect of Gender on Preceived Negative Consequences of Physical Health")+ 
  labs(y= "Number of Respondents", x = "Will discussing health with employer have negative consequences?", subtitle = 'Company Size: {closest_state} Employees', color = "Gender")+
  scale_fill_manual(name="Gender",values=c("dodgerblue","hotpink", "darkgreen"),labels=c("Male","Female", "Other"))

menp_anim<-menp+transition_states(company_size_lab)
physp_anim <- physp+transition_states(company_size_lab)

animate(menp_anim, renderer=gifski_renderer())
animate(physp_anim, renderer=gifski_renderer())

#plot 5
#resources
us_uk_dta$resources <- us_uk_dta$Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.
table(us_uk_dta$Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.)

us_uk_dta$resources <-factor(
  us_uk_dta$resources, levels = c("Yes", "No", "I don't know"),
  labels = 1:3
)
us_uk_dta$resources_lab<-factor(
  us_uk_dta$resources, levels = 1:3,
  labels = c("Yes", "No", "I don't know")
)
us_uk_dta <- subset(us_uk_dta, !is.na(resources))
us_uk_dta$has_help <- ifelse(us_uk_dta$resources_lab == "No" | us_uk_dta$resources_lab == "I don't know", "No", "Yes")

table(us_uk_dta$resources_lab)

# og interactive
p1<-ggplot(us_uk_dta, aes(x=company_size_lab, y=resources_lab, color=gender))+
  geom_point(position="jitter")+
  ggtitle("Effect of Age, Sex, and Company Size \n on Mental Illness")+
  labs(y = "Does Respondent have access to resources?", x = "Company Size")+
  scale_fill_manual(name="resources",values="resouces_lab")

p1
library(plotly)

ggplotly(p1)
saveWidget(ggplotly(p1), file = "age_sex_comp.html")


# attempt to make the bar graph
# multi stack, yes no and idk, each one has a bar for each company size. might have to exclude gender

ggplot(us_uk_dta, aes(fill=company_size_lab, y = gender, x=resources_lab)) + 
  geom_bar(position="dodge", stat="identity")

# doesn't work, can't get y to be frequency?? giving up
