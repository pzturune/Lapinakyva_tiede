###################################
### OPEN SCIENCE FINLAND SURVEY ###
###################################

### PRE DATA-COLLECTION DATA SIMULATION

### Original code by Anton Kunnari
### Updated to reflect the final questionnaire by Piia Turunen

###please note that some responses allow for multiple choice but are not coded here thusly, e.g. Q3&Q7

set.seed(0)

# sample size
n = 100

dat <- data.frame(ID = 1:n)

labs <- c("First stage", 
          "Second stage",
          "Third stage",
          "Fourth stage")

dat$position <- sample(labs, n, replace=TRUE)

labs <- c("Psychology",
          "Social psychology",
          "Other")

dat$field <- sample(labs, n, replace=TRUE)

labs <- c("Qualitative research", "Quantitative research", "Other")

dat$res_area <-  sample(labs, n, replace=TRUE)



## 4. How much experience do you have of the following open science practices?
  
## How much experience do you have of the following open science practices?
## Item anchoring:
# 1 Not familiar with the practice
# 2 I’m familiar with the practice, but have no practical experience
# 3 I have little experience
# 4 I have some experience
# 5 I have lots of experience

dat$OSexp.open.access <- rbinom(100, 4, 0.5)+1
dat$OSexp.open.data <- rbinom(100, 4, 0.5)+1
dat$OSexp.open.script <- rbinom(100, 4, 0.5)+1
dat$OSexp.open.material <- rbinom(100, 4, 0.5)+1
dat$OSexp.preregistration <- rbinom(100, 4, 0.5)+1
dat$OSexp.regrep <- rbinom(100, 4, 0.5)+1
dat$OSexp.preprint <- rbinom(100, 4, 0.5)+1
dat$OSexp.open.edu <- rbinom(100, 4, 0.5)+1

## 5. How important do you think the following open science practices are?
## Item anchoring:
# 1. Completely unimportant
# 2. Somewhat unimportant
# 3. Neutral
# 4. Somewhat important 
# 5. Extremely important

dat$OSimp.open.access <- rbinom(100, 4, 0.5)+1
dat$OSimp.open.data <- rbinom(100, 4, 0.5)+1
dat$OSimp.open.script <- rbinom(100, 4, 0.5)+1
dat$OSimp.open.material <- rbinom(100, 4, 0.5)+1
dat$OSimp.preregistration <- rbinom(100, 4, 0.5)+1
dat$OSimp.regrep <- rbinom(100, 4, 0.5)+1
dat$OSimp.preprint <- rbinom(100, 4, 0.5)+1
dat$OSimp.open.edu <- rbinom(100, 4, 0.5)+1

##6. What kind of guidance would you like to receive for the following?

labs <- c("Written material", 
          "Lectures", 
          "Workshops", 
          "Personal consultation", 
          "I have already received enough guidance", 
          "I don’t need or want guidance")

nams <- levels(interaction(c("openaccess", "open.data", "open.script", "open.material", 
      "preregistration", "regrep", "preprint", "open.edu"), gsub(" ", ".", labs)))
nams <- sort(nams)

mat <- matrix(round(runif(n*48, 0, 1)), nrow=n, ncol=48)
colnames(mat) <- nams

dat <- cbind(dat, mat)

## 7. Which language would you like the materials or training to be in? 
##    You can choose multiple option if you wish.
# please not: in this code, each respondent only chooses one option unlike in the questionnaire

labs <- c("Finnish", 
          "English",
          "Swedish",
          "other")

dat$language <- sample(labs, n, replace=TRUE)


## 8. Which of the following obstacles to implementing open research practices have you experienced during the past two years? Choose all that apply.



labs <- c(
"I didn’t think open science practices were necessary or important",
"Open science practices were not applicable to my research",
"I was not familiar with open science practices",
"I lacked the necessary knowledge",
"I did not receive guidance",

"The open practices were too difficult to implement",
"My workload was already too heavy",
"Implementing open science practices would have taken too much time",
"I did not have sufficient funding",
"Implementing open science practices could have decreased publishing opportunities",

"Other researchers could have misused information, data, or analysis code",
"My research idea could have been stolen",
"My study would have been more exposed to criticism",
"Sensitivity of the data or other confidentiality related issues prevented implementing the practice",
"There wasn’t a suitable platform or archive for openly sharing the data",

"There was no permission to share the data from the participants of the study",
"Department or other work community had a negative attitude towards open practices",
"Employer or boss had a negative attitude towards open practices",
"Reviewers recommended removing or altering the used open science practices",
"A third party prevented implementing the practices",

"I wasn’t motivated because others don’t implement open science practices",
"Other, please specify:",
"I have not experienced any of the obstacles mentioned above")

mat <- matrix(0, nrow=n, ncol=length(labs))
for(row in 1:n) {
  mat[row,sample(1:12, 5)] <- 1
}

colnames(mat) <- labs
dat <- cbind(dat, mat)

## 10. I would implement open practices more if...(choose up to 5 most significant)
# please not: responses are not restricted to 5 in this code

labs <- c(
  "Journal or funder would open research practices from all",
  "Open research practices were the norm in my science community",
  "My employer would require open research practices",
  "Part of the working time or funding would be reserved for learning about and implementing open practices",
  "Training and guidance for implementing open research practices would be available",
  
  "Implementing open research practices would advance my career as a researcher (e.g. considered in evaluations)",
  "Implementing open research practices would enhance my future funding possibilities",
  "Implementing open research practices would lead to a higher citation count",
  "Implementing open research practices would lead to a better reputation as a researcher",
  "Implementing open research practices would increase the probability of getting published",
  
  "Other, please describe")

mat <- matrix(0, nrow=n, ncol=length(labs))
for(row in 1:n) {
  mat[row,sample(1:12, 5)] <- 1
}

colnames(mat) <- labs
dat <- cbind(dat, mat)

## 11. Are you familiar with the term replication crisis?
labs <- c("I don’t recall having heard of it",
"I’ve heard of it, but I’m unsure of its definition",
"I am somewhat familiar with it",
"I am very familiar with it")

dat$repcrisis.familiar <- sample(labs, n, replace=TRUE)

##12. Is there a replication crisis in your field of research?
  
labs <- c("No, there is no replication crisis",
"Yes, there is a slight replication crisis",
"Yes, there is a significant crisis",
"I don’t know")

dat$repcrisis.yourfield <- sample(labs, n, replace=TRUE)

## 13. If you want to, please expand on your answer: 
dat$repcrisis.expand <- rep("Lorem Ipsum", n)
  
  
## 14. If a finding in my field doesn’t replicate, the reason is usually...

labs <- c("Bad luck in the original studies (e.g. the effect was found by accident even if it doesn’t actually exist)",
"A shortcoming of the original studies (e.g. lack of methodological skills or expertise)",
"Fraud or intentional misrepresentation in the original studies",
"Bad luck in the replication study (e.g. the effect was not found by accident even if it does actually exist)",
"A shortcoming of the replication study (e.g. lack of methodological skills or expertise)",
"Fraud or intentional misrepresentation in the replication study",
"Insufficient information on the original study (e.g. not enough information to replicate the design or analyses)",
"More extensive issues with the reliability of the published research literature (e.g. resulting from publication bias or insufficient peer review)",
"Phenomena are complicated and changing. It’s unavoidable that they are hard to capture in replication studies",
"None of the above")

dat$repcrisis.reason <- sample(labs, n, replace=TRUE)

# 
write.csv(dat, "OSfinland_simulated_data.csv")
