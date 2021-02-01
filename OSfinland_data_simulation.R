###################################
### OPEN SCIENCE FINLAND SURVEY ###
###################################

### PRE DATA-COLLECTION DATA SIMULATION

set.seed(0)

# sample size
n = 100

dat <- data.frame(ID = 1:n)

labs <- c("Doctoral researcher, PhD/Doctoral student/candidate", 
          "Postdoctoral researcher",
          "Lecturer",
          "Assistant professor",
          "Professor or principal investigator",
          "I’m not currently doing research (in addition, please select another option describing your latest position as a researcher)",
          "Other, please describe")

dat$position <- sample(labs, n, replace=TRUE)

labs <- c("Qualitative research", "Quantitative research", "Other, please describe")

dat$res_area <-  sample(labs, n, replace=TRUE)

paper_num <- round(rexp(n, rate=1.5)*10)

paper_num

labs <- c("Political science",
          "Psychology",
          "Economics",
          "Social sciences",
          "Humanities",
          "Population or health sciences",
          "Linguistics",
          "Other, please describe")

dat$field <- sample(labs, n, replace=TRUE)

## 5. How much experience do you have of the following open science practices?
  
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

## 6. How important do you think the following open science practices are?
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

##7. What kind of guidance would you like to receive for the following?

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

labs <- c(
"I learned about the practice too late to implement it",
"My workload was already too heavy",
"Implementing open science practices took too much time",
"I didn’t have enough funding to implement open science practices",
"Carrying out the open practice was too difficult",
"I didn’t receive sufficient information or guidance for carrying out the practice",

"Paper was rejected in peer review because of the use of open science practices",
"Implementing open science practices would have decreased publishing opportunities",
"Other researchers could have misused information, data, or analysis code",
"My research idea could have been stolen",
"My study would have been more exposed to criticism",

"Participants of the study were not asked for permission for data sharing",
"Sensitivity of the data or other confidentiality related issues prevented implementing the practice",
"There wasn’t a suitable platform or archive for openly sharing the data",

"Faculty or other work community hadmost a negative attitude towards open practices",
"Employer or boss had a negative attitude towards open practices",
"Reviewers recommended removing or altering the used open science practices",
"A third party prevented implementing the practices",
"Business related reasons prevented implementing the practices",
"I wasn’t motivated because others don’t implement open science practices",
"I didn’t think open science practices were necessary or important")

mat <- matrix(round(runif(length(labs)*n, 0, 1)), nrow=n, ncol=length(labs))
colnames(mat) <- labs

dat <- cbind(dat, mat)

dat$other.obstacles <- rep("Lorem Ipsum", n)

labs <- c("Journal or funder requires open research practices from all",
"Open research practices are the norm in my science community",
"My employer requires open research practices",
"Part of the working time or funding is reserved for learning about and implementing open practices",
"Training and guidance for implementing open research practices",
"Implementing open research practices advances my career as a researcher (e.g. it’s considered in evaluations)",
"Implementing open research practices enhances my future funding possibilities",
"Implementing open research practices leads to a higher citation count",
"Implementing open research practices leads to a better reputation as a researcher",
"Implementing open research practices increases the probability of getting published",
"Nothing",
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
