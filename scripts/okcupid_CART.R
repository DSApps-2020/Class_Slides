okcupid <- read_csv("data/okcupid.csv.zip")
ethnicities_keep <- c("white", "asian", "hispanic / latin", "black", "indian", "pacific islander", "middle eastern", "native american")
body_type_not_perfect <- c("a little extra", "average", "curvy", "full figured", "overweight", "used up", "rather not say")
signs <- c("leo", "libra", "cancer", "virgo", "scorpio", "gemini", "taurus", "aries", "pisces", "aquarius", "sagittarius", "capricorn")
job_underpaid <- c("administrative", "media", "other", "rather not say", "student", "unemployed")
narrow_category <- function(category, sep = " ") {
  if (is.na(category)) return(NA)
  split_diet <- str_split(category, sep)
  if (length(split_diet[[1]]) == 1) return(category)
  return(split_diet[[1]][2])
}
okcupid <- okcupid %>%
  mutate(height_cm = 2.54 * height,
         income = ifelse(income == -1, NA, log10(income/100000)),
         religion2 = case_when(
           str_detect(religion, "agnosticism") | str_detect(religion, "atheism") ~ "atheist",
           str_detect(religion, "buddhism") ~ "buddhist",
           str_detect(religion, "christianity") | str_detect(religion, "catholicism") ~ "christian",
           str_detect(religion, "judaism") ~ "jewish",
           str_detect(religion, "hinduism") ~ "hindu",
           str_detect(religion, "islam") ~ "muslim",
           TRUE ~ "NA"),
         education_kind = case_when(
           str_detect(education, "^dropped") ~ "dropped",
           str_detect(education, "^graduated") ~ "graduated",
           str_detect(education, "^working") ~ "working",
           TRUE ~ "other"),
         ethnicity2 = ifelse(ethnicity %in% ethnicities_keep, ethnicity, "other"),
         part_black = factor(str_detect(ethnicity, "black")),
         part_white = factor(str_detect(ethnicity, "white")),
         part_asian = factor(str_detect(ethnicity, "asian")),
         part_hispanic = factor(str_detect(ethnicity, "hispanic")),
         body_type_not_perfect = factor(body_type %in% body_type_not_perfect),
         diet2 = map_chr(diet, narrow_category),
         location_sf = factor(location == "san francisco, california"),
         pets_has_dogs = factor(str_detect(pets, "has dogs")),
         pets_has_cats = factor(str_detect(pets, "has cats")),
         pets_likes_dogs = factor(str_detect(pets, "likes dogs")),
         pets_likes_cats = factor(str_detect(pets, "likes cats")),
         sign_fun = factor(str_detect(sign, "fun")),
         sign_not_matter = factor(str_detect(sign, "but it does")),
         sign_matters = factor(str_detect(sign, "matters")),
         sign2 = str_extract(sign, str_c(signs, collapse = "|")),
         speaks_english = factor(str_detect(speaks, "english")),
         speaks_spanish = factor(str_detect(speaks, "spanish")),
         speaks_french = factor(str_detect(speaks, "french")),
         speaks_german = factor(str_detect(speaks, "german")),
         speaks_chinese = factor(str_detect(speaks, "chinese")),
         education2 = case_when(
           education == "graduated from high school" ~ "high_school",
           education == "graduated from two-year college" ~ "college",
           education == "graduated from college/university" ~ "degree1",
           education == "graduated from masters program" ~ "degree2",
           education == "graduated from ph.d program" ~ "degree3",
           education == "working on two-year college" ~ "student0",
           education == "working on college/university" ~ "student1",
           education == "working on masters program" ~ "student2",
           education == "working on ph.d program" ~ "student3",
           is.na(education) ~ "NA",
           TRUE ~ "other"
         ),
         education_academic = factor(education2 %in% c("degree1", "degree2", "degree3", "student2", "student3")),
         status = ifelse(status == "unknown", NA, status),
         job2 = map_chr(job, narrow_category, sep = " / "),
         job_underpaid = factor(job2 %in% job_underpaid),
         )

predictors <- c("age", "height_cm", "sex", "body_type", "body_type_not_perfect",
                "diet2", "drinks", "drugs", "religion2",
                "education2" ,"education_kind", "education_academic",
                "ethnicity2", "part_black", "part_white",
                "part_asian", "part_hispanic", "job2", "religion2",
                "orientation", "pets_has_dogs", "pets_has_cats", "pets_likes_cats", "pets_likes_dogs",
                "sign_fun", "sign_not_matter", "sign_matters", "sign2", "speaks_english",
                "speaks_spanish", "speaks_french", "speaks_german", "speaks_chinese", "status")

predictors2 <- c("age", "height_cm", "sex", "body_type_not_perfect", "diet2",
                 "drinks", "drugs", "religion2", "education_academic",
                 "ethnicity2", "orientation", "sign", "status", "job_underpaid")

okcupid2 <- okcupid %>%
  drop_na(income) %>%
  select(income, predictors2)

# test_idx <- sample(1:nrow(okcupid2), 2000, replace = FALSE)
okcupid2_test <- okcupid2[test_idx, ]
okcupid2_train <- okcupid2[-test_idx, ]

# valid_idx <- sample(1:nrow(okcupid2_train), 1504, replace = FALSE)
okcupid2_valid <- okcupid2_train[valid_idx, ]
okcupid2_train <- okcupid2_train[-valid_idx, ]

# write_rds(list(test_idx = test_idx, valid_idx = valid_idx), "data/okcupid_test_valid_idx.rda")
# x <- read_rds("data/okcupid_test_valid_idx.rda")
# test_idx <- x$test_idx
# valid_idx <- x$valid_idx


###
# library(rpart)
# library(rpart.plot)
# library(ggparty)

mod <- rpart(income ~ ., data = okcupid2_train, control = rpart.control(cp = 0.005))
print(mod)
plot(mod)
text(mod, pretty = 1, use.n = TRUE)
pred <- predict(mod, okcupid2_valid)

rmse <- function(o, p){
  sqrt(mean((o - p)^2))
}
plot(okcupid2_valid$income, pred)
tibble(income = okcupid2_valid$income, pred = pred) %>%
  count(income, pred) %>%
  ggplot(aes(income, pred)) +
  geom_point(aes(size = n))

cor(okcupid2_valid$income, pred)
rmse(okcupid2_valid$income, pred)

# rpart.plot::prp(mod, type = 5, extra = 1)

py <- as.party(mod)
plot(py)
print(py)

autoplot(py)

ggparty(py) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_label(aes(label = str_c("n = ", nodesize)),
                  ids = "terminal", nudge_y = 0.02) +
  geom_node_plot(gglist = list(geom_boxplot(aes(y = income)),
                                 theme(axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank())),
                 shared_axis_labels=TRUE)


sse_age <- function(age_cut, df) {
  income_above <- df$income[df$age >= age_cut]
  income_below <- df$income[df$age < age_cut]
  sse_above <- sum((income_above - mean(income_above))^2)
  sse_below <- sum((income_below - mean(income_below))^2)
  return(sse_above + sse_below)
}
age <- seq(18,69, 1)
plot(age, map_dbl(age, sse_age, df = okcupid2_train))

