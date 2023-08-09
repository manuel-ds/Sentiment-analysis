## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(rtweet)
library(lubridate)
library(tidytext)
library(lubridate)
library(scales)
library(MetBrewer)
library(ggstream)
library(ggtext)
library(jsonlite)


## -----------------------------------------------------------------------

url = "https://www.googleapis.com/books/v1/volumes?q=statistics"

result = fromJSON(url)

res = tibble(result[[3]])



## -----------------------------------------------------------------------

api_key <- "mjt5LxeoGK5apsZWAYejKYY6E"
api_secret_key <- "zHsOmgPn6EwsOTS5p8CtwW1rK18QjyQ8x9ruIYze7loRHHfejq"
access_token <- "1213606973058404352-0ZDkhLGIMzP9CM0wBXdskg2CDMNbjz"
access_token_secret <- "giERMinlG5tTWRXMeAb9fTpWfKM4D1KS1zFpTJcxGYEfv"

token <- create_token(app = "app_name", consumer_key = api_key,
    consumer_secret = api_secret_key, access_token = access_token,
    access_secret = access_token_secret, set_renv = FALSE)

get_token()

rate_limits()


## -----------------------------------------------------------------------
timeline <- get_timeline(c("304909941", "1146329871418843136"),
    n = Inf, token = token)
timeline

timeline_plot <- timeline %>%
  group_by(month = floor_date(created_at, "month")) %>% 
  count(month, screen_name) %>% 
  ggplot() +
  geom_line(aes(x=month, y=n, col = screen_name), size=1, show.legend = F) +
  scale_colour_manual(values = c("#bd3106","#d9700e")) +
  theme_bw() +
  theme(plot.title = element_markdown(size = 15), 
        plot.subtitle = element_markdown(size = 12)) +
  labs(x="Date", y="number of tweets per month", 
       title = "<span style> Twitter timeline </span>",
       subtitle = "<b><span style='color:#bd3106';>Lagarde</span></b> 
       <span style>vs</span> <b><span style='color:#d9700e';>Von Der Leyen</span></b>")
timeline_plot

ggsave("timelineplot.pdf", plot = timeline_plot, dpi="retina", width=7, height=5, units="in")


text <- timeline %>%
  filter(created_at >= as.Date("2019-07-03")) %>% 
  select(status_id, text, screen_name, created_at, lang) %>% 
  mutate(ID = seq_along(text))

tidy_tweets <- text %>%
  unnest_tweets(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "^http")) %>% 
  filter(str_detect(word, "^[\\#|\\@|[:alpha:]]")) %>% 
  filter(lang=="en")
tidy_tweets


word_usage <- tidy_tweets %>%
  filter(!str_detect(word, "^amp$")) %>% 
  filter(str_detect(word, "^[^@]")) %>% 
  filter(!str_detect(word, "^de$")) %>% 
  filter(!str_detect(word, "^im$")) %>% 
  group_by(screen_name) %>%
  count(word, sort = T) %>%
  mutate(prop = n/sum(n)) %>%
  select(screen_name, word, prop) %>%
  pivot_wider(names_from = screen_name, values_from = prop) %>%
  arrange(Lagarde, vonderleyen) %>%
  ggplot(aes(Lagarde, vonderleyen)) + 
  geom_jitter(alpha = 0.17, size = 2, width = 0.2, height = 0.25, colour="#d9700e") +
  geom_text(aes(label = word), check_overlap = T, vjust = 0) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) +
  theme_bw() +
  geom_abline(color = "#d9700e", size = 0.5) + 
  labs(title = "Words and Hastags most used by Lagarde and Von der Leyen",
       color = "#d9700e") +
  theme(plot.title = element_text(size = 14, colour = "#d9700e")) 
word_usage

ggsave("word_usage.pdf", plot = word_usage, dpi = "retina", width = 7, height = 5,   units = "in")

words <- tidy_tweets %>% 
  filter(lang == "en") %>% 
  filter(!str_detect(word, "^amp$")) %>% 
  group_by(screen_name) %>% 
  count(word, sort = T) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  mutate(word = reorder_within(word, perc, screen_name)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x=word, y=n, fill = screen_name)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~screen_name, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_markdown(size = 13)) +
  labs(x=NULL, y="Word's Count", 
       title = "<span style>Most common words in</span> <b><span style='color:#bd3106';>Lagarde</span></b> <span style>and</span> <b><span style='color:#0f7ba2';>Von Der Leyen</span></b> <span style>Twitter timeline </span>") + 
  scale_fill_manual(values = met.brewer("Egypt")) 
words

ggsave("words.pdf", plot = words, dpi = "retina", width = 7, height = 5,   units = "in")


tidy_tweets %>% 
  filter(lang == "en") %>% 
  filter(str_detect(word, "^\\#")) %>% 
  group_by(screen_name) %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder_within(word, n, screen_name)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x=word, y=n, fill = screen_name)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~screen_name, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  labs(title = "Most common hashtags in Lagarde vs Von Der Leyen twitter timeline",
      y = "Word's count", x = NULL) + 
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(color = "black", face = "bold", size = 12)) +
  scale_fill_manual(values = met.brewer("Egypt"))





## -----------------------------------------------------------------------

user_id = "1146329871418843136"
data <- get_timeline(user_id, n = 3000, token = token)
data

try <- data %>% 
  group_by(month = floor_date(created_at, "month")) %>% 
  summarise(retweet = sum(is_retweet), quote = sum(is_quote), 
            reply = sum(!is.na(reply_to_user_id)) , 
            tweet = n() - retweet - quote - reply) %>% 
  pivot_longer(cols = retweet:tweet, names_to = "content", values_to = "n")
try

#evolution of the content 
content_plot <- ggplot(try, aes(x = month, y = n, fill = content)) +
  geom_stream() +
  scale_fill_manual(values = c("#859b6c","#62929a","#004f63", "#122451")) +
  theme_bw() +
  labs(y=NULL, title = "Evolution of @vonderleyen's twitter content", x="Date") +
  theme(plot.title = element_text(size = 14, colour = "#004f63"))
content_plot
ggsave("content.pdf", plot = content_plot, dpi = "retina", width = 7, height = 5,   units = "in")

#how many tweets, retweets and replies
content_static <- try %>% 
  filter(month > "2021-12-01") %>% 
  group_by(content) %>% 
  summarise(n_type = sum(n)) %>% 
  mutate(perc = n_type/sum(n_type))
  ggplot() +
  geom_col(aes(x=content, y=perc, fill=content)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Type of Content Published by Ursula Von Der Leyen", x=NULL, 
       y="Pubblications (%)") +
  scale_fill_manual(values = met.brewer("Degas")) +
  theme(plot.title = element_text(size = 14, colour = "#556219"))
content_static
ggsave("content_static.pdf", plot = content_static, dpi = "retina", width = 7, height = 5,   units = "in")

# content distribution
# tweet 74%
# retweet 6%
# reply 13%
# quote 7%

data %>% 
  filter(created_at == min(created_at)|created_at==max(created_at))
#2019-07-03 first tweet
#2022-01-21 last tweet

mydata <- data %>%
  select(created_at) %>%
  mutate(date = round_date(created_at, unit = "week")) %>% 
  count(date) %>% 
  ggplot() +
  geom_area(aes(x=date, y=n), fill = "steelblue", size = 0.7) +
  theme_bw() +
  labs(title = "Ursula Von der Leyen's tweet timeline", y="tweets per week")
mydata
  



## -----------------------------------------------------------------------

tidy_tweets %>% 
  filter(screen_name == "Lagarde") %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x=word, y=n, fill = word)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most common words in Lagarde twitter timeline",
      y = "Word's count", x = NULL) + 
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(color = "black", face = "bold")) +
   scale_fill_manual(values = met.brewer("Renoir"))  


tidy_tweets %>% 
  filter(screen_name == "vonderleyen") %>% 
  filter(!str_detect(word, "^amp$")) %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  ggplot(aes(x=word, y=n, fill = word)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most common words in vonderleyen twitter timeline",
      y = "Word's count", x = NULL) + 
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(color = "black", face = "bold")) +
   scale_fill_manual(values = met.brewer("Renoir"))  

# most frequent hashtag for vonderleyen
hashtag_plot <- tidy_tweets %>% 
  filter(screen_name == "vonderleyen") %>% 
  filter(str_detect(word, "^\\#")) %>% 
  count(word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_max(n, n = 12, with_ties = F) %>% 
  ggplot(aes(x=word, y=n, fill = word)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Common Hashtags in @vonderleyen Twitter Timeline",
      y = "Word's count", x = NULL) + 
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(color = "#355828", size = 14)) +
   scale_fill_manual(values = met.brewer("Renoir"))
hashtag_plot
ggsave("hashtag_plot.pdf", plot = hashtag_plot, dpi = "retina", width = 7, height = 5,   units = "in")


library(widyr)
recovery_corr <- tidy_tweets %>% 
  filter(screen_name == "vonderleyen") %>% 
  filter(!str_detect(word, "^amp$")) %>% 
  pairwise_cor(word, ID, sort = T) %>% 
  filter(correlation > 0.2) %>% 
  filter(item1 == "recovery") 

corr_plot <- recovery_corr %>% 
  ggplot() +
  geom_point(aes(x=correlation, y=item2, col=item2), show.legend = F, size=4) +
  ylab(NULL) +
  xlab("Correlation coefficient") +
  theme_bw() +
  scale_color_manual(values = met.brewer("Redon")) +
  labs(title = "Most Correlated Words With 'Recovery'") +
  theme(plot.title = element_text(colour = "#af4f2f", size = 14))
corr_plot
ggsave("corr_plot.pdf", plot = corr_plot, dpi = "retina", width = 7, height = 5,   units = "in")





## -----------------------------------------------------------------------
urs_timeline <- timeline %>% 
  filter(screen_name=="vonderleyen") %>% 
  filter(created_at < "2022-01-21 17:00:00")
urs_timeline <- urs_timeline %>%   
  mutate(id = 1:nrow(urs_timeline)) %>% 
  filter(lang=="en")
urs_timeline

text <- urs_timeline %>%
    select(text,id,lang)

tidy_tweets_urs <- text %>%
  unnest_tweets(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "^http")) %>% 
  filter(str_detect(word, "^[\\#|\\@|[:alpha:]]")) %>% 
  filter(!str_detect(word, "^amp$")) %>% 
  filter(lang=="en") %>% 
  select(id, word) %>% 
  count(id, word)
tidy_tweets_urs
  
dtm <- tidy_tweets_urs %>%
    cast_dtm(id, word, n)
dtm

library(topicmodels)

train <- sample(rownames(dtm), nrow(dtm) * 0.75)
dtm_train <- dtm[rownames(dtm) %in% train, ]
dtm_test <- dtm[!rownames(dtm) %in% train, ]

topic <- data.frame(k = c(5, 10, 15, 20, 25), perplexity = NA)

for (i in 1:nrow(topic)) {
    print(topic$k[i])
    m = LDA(dtm_train, method = "Gibbs", k = topic$k[i], control = list(alpha = 0.01,
        seed = 123456))
    topic$perplexity[i] = perplexity(m, dtm_test)
}

ggplot(topic, aes(x = k, y = perplexity)) + geom_line(col = "steelblue") +
    theme_bw()

library(DirichletReg)

dirichlet <- rdirichlet(1500, alpha = c(0.05, 0.05, 0.05))
dr <- DR_data(dirichlet)
plot(dr)

rdiri


m17 <- LDA(dtm, method = "Gibbs", k = 17, control = list(alpha = 0.01,
    seed = 123456))
terms(m17, 7)

m18 <- LDA(dtm, method = "Gibbs", k = 18, control = list(alpha = 0.01,
    seed = 123456))
terms(m18,7)

m19 <- LDA(dtm, method = "Gibbs", k = 19, control = list(alpha = 0.01,
    seed = 123456))
terms(m19,7)

m20 <- LDA(dtm, method = "Gibbs", k = 20, control = list(alpha = 0.01,
    seed = 123456))
terms(m20, 7)


m21 <- LDA(dtm, method = "Gibbs", k = 21, control = list(alpha = 0.01,
    seed = 123456))
terms(m21, 7)




## -----------------------------------------------------------------------

tidy_topics <- tidy(m17, matrix = "beta")  #per topic per word probabilities

tidy_topics

write_rds(tidy_topics, file = "beta.rds")

tidy_topics <- read_rds("beta.rds")
tidy_topics

tidy_topics %>% 
  filter(topic==6) %>% 
  arrange(desc(beta))

top_terms <- tidy_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10, with_ties = F) %>%
    ungroup() %>%
    arrange(topic, -beta)

all_topics_plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) + geom_col(show.legend = F, fill = "#2d5380") +
  facet_wrap(~topic, scales = "free") + scale_y_reordered() +
  labs(y=NULL) +
  theme_bw()
all_topics_plot
ggsave("all_topics_plot17.pdf", plot = all_topics_plot, dpi = "retina", width = 11, height = 7, units = "in")

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  filter(topic >8) %>% 
  ggplot(aes(beta, term, fill = topic)) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered() +
  labs(y=NULL) +
  theme_bw()

top_terms %>% 
  filter(topic==2)

word_ass <- augment(m17, dtm)
word_ass %>% 
  count(term, sort = T)


tidy_gamma <- tidy(m17, matrix = "gamma") #per document per topic proportions
tidy_gamma

write_rds(tidy_gamma, "gamma.rds")

tidy_gamma <- read_rds("gamma.rds")

max_gamma <- tidy_gamma %>% 
  group_by(document) %>% 
  filter(gamma==max(gamma)) %>% 
  mutate(id = as.numeric(document))

max_gamma %>% 
  filter(topic==2)

topic <- as.factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
topic_name <- c("EU values", "Covid-19", "Belarus", "EU commission", "Crisis", "Migration",
                "Cooperation", "vaccines", "Women", "Energetic crisis", "Nextgenerationeu", "Humanitarian aid", "Restrictive measures", "Climate change", "Balkans","Digital","Global health")


df <- tibble(topic, topic_name)
df  

# how many documents per topic
doc_topic <- tidy_gamma %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma)) %>% 
  group_by(topic) %>% 
  summarise(n_doc = n()) %>% 
  mutate(perc = n_doc/sum(n_doc)*100) %>% 
  mutate(topic = reorder(topic, perc)) %>% 
  inner_join(df)
  
doc_topic

plot_topics <- doc_topic %>% 
  select(-topic, -n_doc) %>% 
  mutate(topic_name = reorder_within(topic_name, perc, topic_name)) %>% 
  ggplot() +
  geom_col(aes(x=topic_name, y=perc), fill="#2d5380") +
  coord_flip() +
  labs(title = "Number of Tweets per Topic", y="Documents (%)", x=NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 15, colour = "#2d5380")) +
  scale_x_reordered()
plot_topics 

ggsave("plot_topics.pdf", plot = plot_topics, dpi = "retina", width = 7, height = 5, units = "in")


topic_time <- urs_timeline %>% 
  select(id, created_at, text) %>% 
  right_join(max_gamma) %>% 
  select(-document)
topic_time

climate_change <- topic_time %>% 
  filter(topic==14) %>% 
  group_by(month = floor_date(created_at, "month")) %>% 
  mutate(topic = as.factor(topic)) %>% 
  count(month, topic) %>% 
  ggplot() +
  geom_line(aes(x=month, y=n), size = 1.4, color = "#375624") +
  theme_bw() +
  labs(y="Number of documents per month", x="Date", title = "Tweets about Climate Change") +
  theme(plot.title = element_text(colour = "#375624", size = 15))
climate_change
ggsave("climate_change.pdf", plot = climate_change, dpi = "retina", width = 7, height = 5, units = "in")

urs_timeline %>% 
  filter(id==2124)



