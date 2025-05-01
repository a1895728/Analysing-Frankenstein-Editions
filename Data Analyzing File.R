## Frankenstein Editions Comparative Analysis

# --- Load Required Libraries ---
library(pacman)
pacman::p_load(
  tidyverse, tidytext, stringr, ggwordcloud, ggraph, igraph,
  grid, png, udpipe, textstem, forcats
)

# --- Load Lemmatized Word-Level Data ---
lemmatized_data <- read_csv("frankenstein_cleaned_lemmatized.csv")

# ================================
# --- Zipf's Law Analysis ---
# ================================

freq_by_rank <- lemmatized_data %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  mutate(rank = row_number(), term_frequency = n / sum(n)) %>%
  ungroup()

zipf_plot <- ggplot(freq_by_rank, aes(rank, term_frequency, color = book)) +
  geom_line(linewidth = 1.1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Rank (log scale)",
    y = "Term Frequency (log scale)",
    title = "Zipf’s Law for Frankenstein (Lemmatized)",
    color = "Edition"
  ) +
  scale_color_manual(values = c("1818 Edition" = "lightsalmon", "1831 Edition" = "turquoise3")) +
  theme_minimal(base_size = 13)

print(zipf_plot)
ggsave("EPS_Graphs/zipfs_law_frankenstein.eps", plot = zipf_plot, device = "eps", width = 8, height = 6)


# ================================
# --- Wordcloud ---
# ================================

top_words <- lemmatized_data %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = 100) %>%
  ungroup()

set.seed(1234)
wordcloud_plot <- ggplot(top_words, aes(label = word, size = n, color = book)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 14) +
  scale_color_manual(values = c("1818 Edition" = "salmon", "1831 Edition" = "turquoise3")) +
  facet_wrap(~book) +
  labs(title = "Wordcloud Comparison: Frankenstein Editions") +
  theme_minimal(base_size = 14)

print(wordcloud_plot)
ggsave("EPS_Graphs/wordcloud_frankenstein.eps", plot = wordcloud_plot, device = "eps", width = 10, height = 7)


# ================================
# --- Top 20 Words per Edition ---
# ================================

top20_words <- lemmatized_data %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, book))

top20_plot <- ggplot(top20_words, aes(x = n, y = word, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("1818 Edition" = "lightsalmon", "1831 Edition" = "turquoise3")) +
  labs(title = "Top 20 Words per Edition", x = "Frequency", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

print(top20_plot)
ggsave("EPS_Graphs/top20_words_per_edition.eps", plot = top20_plot, device = "eps", width = 10, height = 6)


# ================================
# --- Interest Words by Chapter (Filtered to Chapters 1–24) ---
# ================================


# Define refined interest words
interest_words_refined <- c("creature", "monster", "fear", "father", "death")

# Filter lemmatized data for interest words in chapters 1–24
freq_interest_chap_refined <- lemmatized_data %>%
  filter(word %in% interest_words_refined, chapter <= 24) %>%
  count(book, chapter, word, sort = TRUE)

# Combine data and ensure consistent edition order
freq_interest_combined <- freq_interest_chap_refined %>%
  mutate(book = factor(book, levels = c("1818 Edition", "1831 Edition")))

# Define edition color palette
edition_colors <- c("1818 Edition" = "lightsalmon", "1831 Edition" = "turquoise3")

# Generate faceted line plot: frequency per chapter per word, colored by edition
interest_words_facet_plot <- ggplot(freq_interest_combined, aes(x = chapter, y = n, color = book)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~word, ncol = 2, scales = "free_y") +
  scale_color_manual(values = edition_colors,, name = NULL) +
  scale_x_continuous(breaks = 1:24, limits = c(1, 24)) +
  labs(
    title = "Interest Word Frequency by Chapter (Faceted by Word)",
    x = "Chapter Number", y = "Frequency", color = "Edition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"  # <-- Legend moved here
  )

# Display and save as EPS
print(interest_words_facet_plot)
ggsave("EPS_Graphs/interest_words_by_chapter_facet.eps",plot = interest_words_facet_plot, device = "eps",width = 10, height = 7)


# =====================================================
# --- Thematic Word Group Analysis---
# =====================================================


# --- Define Thematic Word Groups ---
word_theme_df <- tibble(
  word = c(
    # Creation & Science
    "create", "creation", "science", "knowledge", "discover", "experiment",
    "secret", "power", "spark", "lightning", "nature", "life", "animation",
    
    # Emotion & Inner Conflict
    "fear", "despair", "horror", "guilt", "love", "hope", "hate", "revenge", "grief",
    "misery", "pain", "suffering", "passion", "regret",
    
    # Identity & Alienation
    "monster", "creature", "wretch", "abhor", "reject", "alone", "solitude",
    "abandon", "outcast", "isolate", "hide", "stranger",
    
    # Responsibility & Morality
    "responsibility", "duty", "justice", "innocent", "crime", "punishment", "fate",
    "curse", "conscience", "trial", "murder",
    
    # Symbolism & Setting
    "night", "ice", "darkness", "storm", "fire", "light", "cold", "mountain", "glacier",
    "winter", "moon", "wind", "sky",
    
    # Key Characters
    "victor", "elizabeth", "father", "william", "justine", "walton", "henry", "frankenstein"
  ),
  theme = c(
    rep("Creation & Science", 13),
    rep("Emotion & Inner Conflict", 14),
    rep("Identity & Alienation", 12),
    rep("Responsibility & Morality", 11),
    rep("Symbolism & Setting", 13),
    rep("Key Characters", 8)
  )
)

# --- Join with Lemmatized Dataset to Tag Thematic Words ---
thematic_tagged <- lemmatized_data %>%
  inner_join(word_theme_df, by = "word")

# Count Thematic Word Frequency per Chapter and Edition
theme_chapter_freq <- thematic_tagged %>%
  group_by(theme, book, chapter) %>%
  summarise(count = n(), .groups = "drop")

# Define color for editions
edition_colors <- c("1818 Edition" = "lightsalmon", "1831 Edition" = "turquoise3")

# Plot: 1 panel per theme, two lines per theme for editions
theme_plot_facet_by_theme <- ggplot(theme_chapter_freq, aes(x = chapter, y = count, color = book)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~theme, ncol = 2, scales = "free_y") +
  scale_color_manual(values = edition_colors) +
  scale_x_continuous(breaks = 1:24, limits = c(1, 24)) +
  labs(
    title = "Thematic Word Frequency by Chapter (Faceted by Theme)",
    x = "Chapter Number", y = "Word Count", color = "Edition"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

# Display and save
print(theme_plot_facet_by_theme)
ggsave("EPS_Graphs/thematic_words_by_theme_facet.eps", plot = theme_plot_facet_by_theme, device = "eps", width = 12, height = 8)

# ================================
# --- TF-IDF Analysis ---
# ================================

tf_idf_df <- lemmatized_data %>%
  count(book, word, sort = TRUE) %>%
  bind_tf_idf(word, book, n)

top_tf_idf <- tf_idf_df %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, book))

tfidf_plot <- ggplot(top_tf_idf, aes(x = tf_idf, y = word, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("1818 Edition" = "salmon", "1831 Edition" = "turquoise3")) +
  labs(
    title = "Top TF-IDF Words by Edition",
    x = "TF-IDF Score", y = NULL
  ) +
  theme_minimal(base_size = 13)

print(tfidf_plot)
ggsave("EPS_Graphs/tfidf_top_words.eps", plot = tfidf_plot, device = "eps", width = 10, height = 6)


# ================================
# --- Sentiment Analysis ---
# ================================

afinn <- get_sentiments("afinn") %>%
  mutate(value = (((value + 5) / 10) * 2 - 1))

# --- Chapter wise Sentiment ---

word_freq <- lemmatized_data %>%
  count(book, chapter, word, sort = TRUE)

### Sentiment contributions
sentiment_df <- word_freq %>%
  inner_join(afinn, by = "word") %>%
  mutate(contribution = n * value)

###  Chapter-wise average sentiment 
sentiment_chapter <- sentiment_df %>%
  group_by(book, chapter) %>%
  summarise(weighted_sentiment = mean(value), .groups = "drop")

### Plot Chapter-wise Sentiment 
sentiment_chapter_plot <- ggplot(sentiment_chapter, aes(x = chapter, y = weighted_sentiment, color = book)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:24) +
  labs(
    title = "Chapter-wise Sentiment in Frankenstein Editions",
    x = "Chapter", y = "Weighted Sentiment Score", color = "Edition"
  ) +
  theme_minimal(base_size = 13)

print(sentiment_chapter_plot)
ggsave("EPS_Graphs/sentiment_by_chapter.eps", plot = sentiment_chapter_plot, device = "eps", width = 8, height = 5)


# --- Select Top 15 Positive and Top 15 Negative Words for each edition ---

word_freq <- lemmatized_data %>%
  count(book, word, sort = TRUE)

#Join with sentiment scores 
sentiment_data <- word_freq %>%
  inner_join(afinn, by = "word") %>%
  mutate(sentiment = ifelse(value >= 0, "Positive", "Negative"))

## Select Top 15 Positive and Top 15 Negative Words for each edition 
top_sentiment_words <- sentiment_data %>%
  group_by(book, sentiment) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, book))

## Plot
sentiment_word_plot <- ggplot(top_sentiment_words, aes(x = n, y = word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment + book, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("Positive" = "turquoise3", "Negative" = "salmon")) +
  labs(
    title = "Top Sentiment Words in Frankenstein (1818 vs 1831)",
    x = "Frequency", y = NULL
  ) +
  theme_minimal(base_size = 13)

## Print and Save 
print(sentiment_word_plot)
ggsave("EPS_Graphs/top_sentiment_words.eps", plot = sentiment_word_plot, device = "eps", width = 10, height = 6)


# --- Top Sentiment Contributors ---

top_sentiment <- sentiment_df %>%
  group_by(book) %>%
  slice_max(abs(contribution), n = 20) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, contribution, book))

sentiment_plot <- ggplot(top_sentiment, aes(x = contribution, y = word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(
    title = "Top Sentiment Contributors (Frequency × Score)",
    x = "Contribution", y = NULL
  ) +
  theme_minimal(base_size = 13)

print(sentiment_plot)
ggsave("EPS_Graphs/sentiment_contributors.eps", plot = sentiment_plot, device = "eps", width = 10, height = 6)


# ================================
# --- Bigram Network ---
# ================================

book_texts <- lemmatized_data %>%
  group_by(book) %>%
  summarise(text = paste(word, collapse = " "))

bigrams_df <- book_texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1), !is.na(word2)) %>%
  count(book, word1, word2, sort = TRUE)

plot_bigram_graph <- function(book_name, color) {
  bigram_data <- bigrams_df %>%
    filter(book == book_name) %>%
    slice_max(n, n = 30) %>%
    select(word1, word2, n)
  
  graph <- graph_from_data_frame(bigram_data)
  
  ggraph(graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), arrow = grid::arrow(type = "closed", length = unit(0.15, "inches")), end_cap = circle(0.07, "inches"), show.legend = FALSE, color = color) +
    geom_node_point(color = color) +
    geom_node_text(aes(label = name), vjust = 1.5, hjust = 1, size = 3) +
    labs(title = paste("Bigram Network for", book_name)) +
    theme_minimal(base_size = 13)+
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
}

#plot_1818 <- plot_bigram_graph("1818 Edition", "lightsalmon")
#plot_1831 <- plot_bigram_graph("1831 Edition", "turquoise3")


plot_1818 <- plot_bigram_graph("1818 Edition", "firebrick")
plot_1831 <- plot_bigram_graph("1831 Edition", "deepskyblue3")

print(plot_1818)
print(plot_1831)

ggsave("EPS_Graphs/bigram_1818.eps", plot = plot_1818, device = "eps", width = 8, height = 6)
ggsave("EPS_Graphs/bigram_1831.eps", plot = plot_1831, device = "eps", width = 8, height = 6)