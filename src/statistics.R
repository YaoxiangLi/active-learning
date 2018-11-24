# install.packages("readxl")

library(readxl, Hmisc)
url = "https://github.com/lzyacht/active/blob/master/data/feature_set_1.xlsx?raw=true"
destfile = "feature_set_1.xlsx"
curl::curl_download(url, destfile)
feature_set_1 = read_excel(destfile)
feature_set_1_num = clean_feature_set_one(feature_set_1)
describe(feature_set_1)
describe(feature_set_1_num) 

describe(as.numeric(feature_set_1_num$usefulness))
describe(as.numeric(feature_set_1_num$convience))



mean(as.numeric(feature_set_1_num$usefulness))
median(as.numeric(feature_set_1_num$usefulness))

feature_set_1_num$usefulness_group = rep(NA, nrow(feature_set_1_num))
feature_set_1_num$usefulness_group[feature_set_1_num$usefulness >= mean(as.numeric(feature_set_1_num$usefulness))] = "Useful"
feature_set_1_num$usefulness_group[feature_set_1_num$usefulness < mean(as.numeric(feature_set_1_num$usefulness))] = "Useless"

feature_set_1_num$convience_group = rep(NA, nrow(feature_set_1_num))
feature_set_1_num$convience_group[feature_set_1_num$usefulness >= mean(as.numeric(feature_set_1_num$usefulness))] = "Useful"
feature_set_1_num$convience_group[feature_set_1_num$usefulness < mean(as.numeric(feature_set_1_num$usefulness))] = "Useless"

# 
x = feature_set_1_num[feature_set_1_num$usefulness_group == "Useful", ]
y = feature_set_1_num[feature_set_1_num$usefulness_group == "Useless", ]


# compare 1 and 3
x = feature_set_1_num[feature_set_1_num$usefulness == "1", ]
y = feature_set_1_num[feature_set_1_num$usefulness == "3", ]


t.test(as.numeric(x$sex) , as.numeric(y$sex))
t.test(as.numeric(x$temp) , as.numeric(y$temp))
t.test(as.numeric(x$light) , as.numeric(y$light))
t.test(as.numeric(x$teacher_voice) , as.numeric(y$teacher_voice))
t.test(as.numeric(x$legibility) , as.numeric(y$legibility))
t.test(as.numeric(x$room_size) , as.numeric(y$room_size))
t.test(as.numeric(x$room_crowd) , as.numeric(y$room_crowd))
t.test(as.numeric(x$teacher_dist) , as.numeric(y$teacher_dist))
t.test(as.numeric(x$student_dist) , as.numeric(y$student_dist))
t.test(as.numeric(x$atmosphere) , as.numeric(y$atmosphere))
t.test(as.numeric(x$express) , as.numeric(y$express))
t.test(as.numeric(x$communication) , as.numeric(y$communication))
t.test(as.numeric(x$ownership) , as.numeric(y$ownership))
t.test(as.numeric(x$contribution) , as.numeric(y$contribution))
t.test(as.numeric(x$motivation) , as.numeric(y$motivation))
t.test(as.numeric(x$lesson_care) , as.numeric(y$lesson_care))
t.test(as.numeric(x$attention_easy) , as.numeric(y$attention_easy))
t.test(as.numeric(x$tired_easy) , as.numeric(y$tired_easy))
t.test(as.numeric(x$cool_al) , as.numeric(y$cool_al))
t.test(as.numeric(x$ict_plenty) , as.numeric(y$ict_plenty))
t.test(as.numeric(x$chair_easy) , as.numeric(y$chair_easy))
t.test(as.numeric(x$ceil_height) , as.numeric(y$ceil_height))



cared_vars = c("usefulness", "sex", "temp", "light", "teacher_voice",
               "legibility", "room_size", "room_crowd",
               "teacher_dist", "student_dist", "atmosphere",
               "express", "communication", "ownership",
               "contribution", "motivation", "lesson_care",
               "attention_easy", "tired_easy", "cool_al",
               "ict_plenty", "chair_easy", "ceil_height")

model_data = feature_set_1_num[, colnames(feature_set_1_num) %in% cared_vars]
model_data = as.data.frame(apply(model_data, 2, as.numeric))
head(model_data)
library(MASS)

fit <- lm(usefulness ~ sex + temp + light + teacher_voice +
                       legibility + room_size + room_crowd +
                       teacher_dist + student_dist + atmosphere +
                       express + communication + ownership + 
                       contribution + motivation + lesson_care +
                       attention_easy + tired_easy + cool_al +
                       ict_plenty + chair_easy + ceil_height,
          data=model_data)
step <- stepAIC(fit, direction="both")
step$anova # display results
summary(fit) # show results
summary(step)
cor(model_data)

fit2 <- lm(usefulness ~ motivation + sex + teacher_dist + cool_al,
          data=model_data)
summary(fit2) # show results

fit3 <- lm(usefulness ~ temp + light + teacher_voice + legibility +
               room_size + room_crowd + student_dist + atmosphere +
               express + communication + ownership + contribution +
               lesson_care + attention_easy + tired_easy + ict_plenty +
               chair_easy + ceil_height,
           data=model_data)
summary(fit3) # show results

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


library(ggplot2)
ggplot(feature_set_1_num, aes(y=convience, x=usefulness)) + geom_point()



#' Cleaning Feature set one by feature reference table.
#' using the following code to install the dependencies:
#' install.packages("readxl")
#'
#' @param feature_set_1 R dataframe saving data
#' 
#' License: GPL (>= 3)
#' @export
clean_feature_set_one = function(feature_set_1) {
    require(readxl)
    
    url = "https://github.com/lzyacht/active/blob/master/data/feature_ref.xlsx?raw=true"
    destfile = "feature_ref.xlsx"
    curl::curl_download(url, destfile)
    al_type = read_excel(destfile, sheet = "al_type")
    sex = read_excel(destfile, sheet = "sex")
    usefulness = read_excel(destfile, sheet = "usefulness")
    convience = read_excel(destfile, sheet = "convience")
    temp = read_excel(destfile, sheet = "temp")
    light = read_excel(destfile, sheet = "light")
    teacher_voice = read_excel(destfile, sheet = "teacher_voice")
    legibility = read_excel(destfile, sheet = "legibility")
    room_size = read_excel(destfile, sheet = "room_size")
    room_crowd = read_excel(destfile, sheet = "room_crowd")
    teacher_dist = read_excel(destfile, sheet = "teacher_dist")
    student_dist = read_excel(destfile, sheet = "student_dist")
    atmosphere = read_excel(destfile, sheet = "atmosphere")
    express = read_excel(destfile, sheet = "express")
    communication = read_excel(destfile, sheet = "communication")
    ownership = read_excel(destfile, sheet = "ownership")
    contribution = read_excel(destfile, sheet = "contribution")
    motivation = read_excel(destfile, sheet = "motivation")
    lesson_care = read_excel(destfile, sheet = "lesson_care")
    attention_easy = read_excel(destfile, sheet = "attention_easy")
    tired_easy = read_excel(destfile, sheet = "tired_easy")
    cool_al = read_excel(destfile, sheet = "cool_al")
    ict_plenty = read_excel(destfile, sheet = "ict_plenty")
    chair_easy = read_excel(destfile, sheet = "chair_easy")
    ceil_height = read_excel(destfile, sheet = "ceil_height")

    for (i in 1:nrow(feature_set_1)){
        feature_set_1$sex[i] = sex$number[which(feature_set_1$sex[i] == sex$name)]
        feature_set_1$usefulness[i] = usefulness$number[which(feature_set_1$usefulness[i] == usefulness$name)]
        feature_set_1$convience[i] = convience$number[which(feature_set_1$convience[i] == convience$name)]
        feature_set_1$temp[i] = temp$number[which(feature_set_1$temp[i] == temp$name)]
        feature_set_1$light[i] = light$number[which(feature_set_1$light[i] == light$name)]
        feature_set_1$teacher_voice[i] = teacher_voice$number[which(feature_set_1$teacher_voice[i] == teacher_voice$name)]
        feature_set_1$legibility[i] = legibility$number[which(feature_set_1$legibility[i] == legibility$name)]
        feature_set_1$room_size[i] = room_size$number[which(feature_set_1$room_size[i] == room_size$name)]
        feature_set_1$room_crowd[i] = room_crowd$number[which(feature_set_1$room_crowd[i] == room_crowd$name)]
        feature_set_1$teacher_dist[i] = teacher_dist$number[which(feature_set_1$teacher_dist[i] == teacher_dist$name)]
        feature_set_1$student_dist[i] = student_dist$number[which(feature_set_1$student_dist[i] == student_dist$name)]
        feature_set_1$atmosphere[i] = atmosphere$number[which(feature_set_1$atmosphere[i] == atmosphere$name)]
        feature_set_1$express[i] = express$number[which(feature_set_1$express[i] == express$name)]
        feature_set_1$communication[i] = communication$number[which(feature_set_1$communication[i] == communication$name)]
        feature_set_1$ownership[i] = ownership$number[which(feature_set_1$ownership[i] == ownership$name)]
        feature_set_1$contribution[i] = contribution$number[which(feature_set_1$contribution[i] == contribution$name)]
        feature_set_1$motivation[i] = motivation$number[which(feature_set_1$motivation[i] == motivation$name)]
        feature_set_1$lesson_care[i] = lesson_care$number[which(feature_set_1$lesson_care[i] == lesson_care$name)]
        feature_set_1$attention_easy[i] = attention_easy$number[which(feature_set_1$attention_easy[i] == attention_easy$name)]
        feature_set_1$tired_easy[i] = tired_easy$number[which(feature_set_1$tired_easy[i] == tired_easy$name)]
        feature_set_1$cool_al[i] = cool_al$number[which(feature_set_1$cool_al[i] == cool_al$name)]
        feature_set_1$ict_plenty[i] = ict_plenty$number[which(feature_set_1$ict_plenty[i] == ict_plenty$name)]
        feature_set_1$chair_easy[i] = chair_easy$number[which(feature_set_1$chair_easy[i] == chair_easy$name)]
        feature_set_1$ceil_height[i] = ceil_height$number[which(feature_set_1$ceil_height[i] == ceil_height$name)]
    }
    
    return(feature_set_1)
}





