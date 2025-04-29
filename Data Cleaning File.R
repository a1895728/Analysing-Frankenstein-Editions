# --- Load Required Libraries ---
library(pacman)
pacman::p_load(tidyverse, tidytext, gutenbergr, stringr, udpipe, textstem)

# --- Create Output Directory for EPS ---
dir.create("EPS_Graphs", showWarnings = FALSE)


# --- Download Raw Editions -----

frankenstein_1818_raw <- gutenberg_download(41445, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")
frankenstein_1831_raw <- gutenberg_download(42324, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

# -----------------------------------------------------------
# --- Cleaned Data 1a – Standardize Metadata and Identify Structure ---
# -----------------------------------------------------------

# Function to remove Gutenberg metadata headers/footers
dynamic_remove_metadata <- function(book_text) {
  book_text <- tolower(book_text)  # Convert entire text to lowercase
  start_index <- which(str_detect(book_text, "start of (this )?project gutenberg"))  ## Detect start of book content
  end_index <- which(str_detect(book_text, "end of (this )?project gutenberg"))      ## Detect end of book content
  if (length(start_index) > 0 & length(end_index) > 0) {
    book_text <- book_text[(start_index + 1):(end_index - 1)]  # Trim metadata
  }
  book_text[book_text != ""]  # Remove blank lines
}

# Function to extract structure (letter/chapter) from text using regex
download_and_structure_book <- function(book_id, edition_label) {
  raw_book <- gutenberg_download(book_id, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")
  text_cleaned <- dynamic_remove_metadata(raw_book$text)
  book_tbl <- tibble(text = text_cleaned)
  
  structured <- book_tbl %>%
    mutate(
      linenumber = row_number(),
      is_letter = str_detect(text, regex("^letter\\s+[ivxlcdm]+\\.", ignore_case = TRUE)),  # Detect letters
      is_chapter = str_detect(text, regex("^chapter\\s+[ivxlcdm]+\\.", ignore_case = TRUE)),  # Detect chapters
      keep_next = lag(is_letter | is_chapter, default = FALSE),  # Retain text right after header
      letter = cumsum(is_letter),  # Numbering letters
      chapter = cumsum(is_chapter),  # Numbering chapters
      book = edition_label
    ) %>%
    filter(!is_letter & !is_chapter | keep_next) %>%  # Remove headers but keep associated text
    select(book, letter, chapter, text)
  
  return(structured)
}

# Load and merge both editions
frankenstein_1818 <- download_and_structure_book(41445, "1818 Edition")
frankenstein_1831 <- download_and_structure_book(42324, "1831 Edition")
frankenstein_combined <- bind_rows(frankenstein_1818, frankenstein_1831)

# -----------------------------------------------------------
# Pre-cleaning Summary: Token count BEFORE processing
# -----------------------------------------------------------

raw_summary <- frankenstein_combined %>%
  unnest_tokens(word, text) %>%                    # Tokenize the text column into individual words
  group_by(book) %>%                               # Group by book edition (1818 or 1831)
  summarise(
    Total_Words_Before_Cleaning = n(),             # Count total number of tokens
    Distinct_Words = n_distinct(word),             # Count distinct vocabulary size
    .groups = "drop"
  )

print("Summary before cleaning:")
print(raw_summary)

# Save raw chapter-level text
write_csv(frankenstein_combined, "frankenstein_cleaned_chapter_text.csv")

chapter_word_counts_raw <- frankenstein_combined %>%
  group_by(book, chapter) %>%
  summarise(word_count = sum(str_count(text, "\\w+")), .groups = "drop")

write_csv(chapter_word_counts_raw, "chapter_word_counts_raw.csv")

# -----------------------------------------------------------
# Cleaned Data 1b – Remove Non-Textual and Redundant Elements
# -----------------------------------------------------------

frankenstein_combined <- frankenstein_combined %>%
  group_by(book, letter, chapter) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(
    text = text %>%
      str_remove_all("\\[[:print:]+?\\]") %>%  # Remove editorial notes e.g. [Editor’s note]
      str_to_lower() %>%  # Lowercase again
      str_replace_all("[^a-z ]", " ") %>%  # Remove punctuation, symbols, and digits
      str_squish()  # Standardize white space
  )

# -----------------------------------------------------------
# Cleaned Data 1c – Text Annotation and Lemmatization via UDPipe
# -----------------------------------------------------------

# Save cleaned chapter text before annotation
write_csv(frankenstein_combined, "frankenstein_cleaned_chapter_text.csv")

# Download and load the UDPipe English model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Assign a text ID for reference
frankenstein_text <- frankenstein_combined %>%
  mutate(text_id = row_number()) %>%
  select(text_id, book, letter, chapter, text)

# Annotate the text using UDPipe (POS, Lemmas, etc.)
ud_result <- udpipe_annotate(ud_model, x = frankenstein_text$text, doc_id = frankenstein_text$text_id)
ud_df <- as_tibble(ud_result)

# Filter and format annotated tokens
lemmatized_data <- ud_df %>%
  mutate(doc_id = as.integer(doc_id)) %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%  # Focus on content words
  filter(!is.na(lemma), nchar(lemma) > 2) %>%  # Remove short/missing lemmas
  mutate(word = tolower(lemma)) %>%
  left_join(frankenstein_text, by = c("doc_id" = "text_id")) %>%
  select(book, letter, chapter, word) %>%
  mutate(word = lemmatize_words(word))  # Normalize again using textstem

# -----------------------------------------------------------
# Cleaned Data 1d – Remove Stopwords and Noise
# -----------------------------------------------------------

data(stop_words)
custom_stopwords <- tibble(word = c("much", "can", "may", "shall", "upon", "however", "yet", "also", "must", "thus", "perhaps", "indeed", "chapter", "letter"))

lemmatized_data <- lemmatized_data %>%
  anti_join(stop_words, by = "word") %>%  # Remove standard stopwords
  anti_join(custom_stopwords, by = "word")  # Remove domain-specific noise

# -----------------------------------------------------------
# Cleaned Data 1e – Export and Structural Checks
# -----------------------------------------------------------

write_csv(lemmatized_data, "frankenstein_cleaned_lemmatized.csv")

chapter_word_counts_cleaned <- lemmatized_data %>%
  group_by(book, chapter) %>%
  summarise(word_count = n(), .groups = "drop")

write_csv(chapter_word_counts_cleaned, "chapter_word_counts_cleaned.csv")

# -----------------------------------------------------------
# Cleaned Data 1f – Summary Statistics After Cleaning
# -----------------------------------------------------------

summary_after <- lemmatized_data %>%
  group_by(book) %>%
  summarise(
    `Total Tokens` = n(),
    `Unique Words` = n_distinct(word),
    .groups = "drop"
  )

print("Summary after cleaning:")
print(summary_after)

# -----------------------------------------------------------
# Cleaned Data 1g – Chapter-wise Word Count Summary for Comparison
# -----------------------------------------------------------

# Load chapter-wise summaries
raw <- read_csv("chapter_word_counts_raw.csv") %>% mutate(stage = "Raw")
cleaned <- read_csv("chapter_word_counts_cleaned.csv") %>% mutate(stage = "Cleaned")

# Plot 1: Raw Word Counts by Chapter
plot_raw <- ggplot(raw, aes(x = chapter, y = word_count, color = book)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:24) + 
  labs(
    title = "Raw Word Count per Chapter by Edition",
    x = "Chapter Number", y = "Word Count", color = "Edition"
  ) +
  theme_minimal()

# Plot 2: Cleaned Word Counts by Chapter
plot_cleaned <- ggplot(cleaned, aes(x = chapter, y = word_count, color = book)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:24) +  
  labs(
    title = "Cleaned Word Count per Chapter by Edition",
    x = "Chapter Number", y = "Word Count", color = "Edition"
  ) +
  theme_minimal()

# Display plots
print(plot_raw)
print(plot_cleaned)

# Optional: Save to EPS or PNG
ggsave("EPS_Graphs/wordcount_raw_by_chapter.eps", plot = plot_raw, device = "eps", width = 8, height = 5)
ggsave("EPS_Graphs/wordcount_cleaned_by_chapter.eps", plot = plot_cleaned, device = "eps", width = 8, height = 5)

