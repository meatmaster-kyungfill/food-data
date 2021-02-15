# 국내 최대 비건 커뮤니티인 한울벗 채식나라의 데이터를 불러옵니다
library(readr)
raw_vegan_comment <- read_csv("vegan_community.csv")

library(dplyr)
library(stringr)
library(textclean)

#데이터를 전처리 합니다.
vegan_comment <- raw_vegan_comment %>%
  select(title) %>%
  mutate(title = str_replace_all(title, "[^가-힣]", " "),
         title = str_squish(title),
         name = row_number())

library(tidytext)
library(KoNLP)

#데이터를 토큰화 해줍니다.
comment_pos <- vegan_comment %>%
  unnest_tokens(input = title,
                output = word,
                token = SimplePos22,
                drop = F)

comment_pos %>%
  select(word, title)

#품사별로 행을 분리해 줍니다.
library(tidyr)
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")

comment_pos %>%
  select(word, title)

#명사 추출을 해줍니다.
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun %>%
  select(word, title)

noun %>%
  count(word, sort = T)

#동사, 형용사 추출해줍니다.
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))

pvpa %>%
  select(word, title)

pvpa %>%
  count(word, sort = T)

#품사를 결합해줍니다.
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(name)

comment %>%
  select(word, title)

library(widyr)
pair <- comment %>%
  pairwise_count(item = word,
                 feature = name,
                 sort = T)
pair

pair %>% filter(item1 == "채식")



#----------------------------
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 20) %>%
  as_tbl_graph()

graph_comment

library(ggraph)

ggraph(graph_comment) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))
#----------------------------

#그래프 이쁘게 만들기

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
set.seed(1234)
ggraph(graph_comment, layout ="fr") +
  
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(color = "lightcoral",
                 size = 5) +
  
  geom_node_text(aes(label = name),
                  reple = T,
                  size = 5,
                  family = "nanumgothic") +
  theme_graph()

#네트워크 함수를 만들어줍니다.(다시 활용하기 위해서)
word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   reple = T,
                   size = 5,
                   family = "nanumgothic") +
    theme_graph()
}


#유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "채식"), "채식", word),
         word = ifelse(str_detect(word, "건강"), "건강", word),
         word = ifelse(str_detect(word, "명상"), "명상", word))

pair <- comment %>%
  pairwise_count(item = word,
                 feature = name,
                 sort = T)

#네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

set.seed(1234)
word_network(graph_comment)

#--------------

word_cors <- comment %>%
  add_count(word) %>%
  filter( n >= 10) %>%
  pairwise_cor(item = word,
               feature = name,
               sort = T)

word_cors


target <- c("채식","건강", "음식", "과일", "현미", "추천", "식물", "환경", "자연")
top_cors <- word_cors %>%
filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n=8)

top_cors$item1 <- factor(top_cors$item1, levels = target)

library(ggplot2)
ggplot(top_cors, aes(x= reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
