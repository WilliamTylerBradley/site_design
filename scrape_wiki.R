#--------------------------------------------------------------
# This code scrapes the 'Data Science' page of Wikipedia
# Then, gets the letter frequencies of that page
# It also gets the letter frequencies for the English language
# Then saves them so that they can be compared later
#--------------------------------------------------------------

#----------
# Libraries
#----------

# Set up
library(here)
library(ggplot2)

# Library for getting html
library(rvest)
help(html_text)

#--------------------
# 'Data Science' data
#--------------------

# Scrape the Data Science data
data_science <- read_html("https://en.wikipedia.org/wiki/Data_science")

# Pull out main title
heading1 <- html_text(html_nodes(data_science, "h1"))
heading1

# Pull out body text
body <- html_text(html_nodes(data_science, "p"))
head(body)

# Add main title
body[1] <- paste(heading1, body[1])
body[1]

# Crush together
body <- paste(body, collapse = " ")

# barplot of counts
ds_letter <- data.frame(table(
  strsplit(tolower(gsub("[^[:alpha:]]", "", body)), "")))
names(ds_letter) <- c("Letter", "Frequency")
ds_letter$Frequency <- ds_letter$Frequency / sum(ds_letter$Frequency) * 100
ds_letter

ggplot(data = ds_letter, aes(Letter, Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Data Science Wikipedia Page")
ggsave(filename = here::here("images", "ds_letter.png"))

#---------------
# Overall data
#---------------

# Get overall letter counts page
letter_frequency <-
  xml2::read_html("https://en.wikipedia.org/wiki/Letter_frequency")

# Find the right table
# Thanks Robert Lewand and Pavel Micka
tables <- html_nodes(letter_frequency, "table")
overall_letter <- html_table(tables[1], fill = TRUE)[[1]]
overall_letter <- overall_letter[c(1, 3)]
names(overall_letter) <- c("Letter", "Frequency")
overall_letter

ggplot(data = overall_letter, aes(Letter, Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Overall")
ggsave(filename = here::here("images", "overall_letter.png"))

#-------------
# Combine data
#-------------
overall_letter$Source <- "Overall"
ds_letter$Source <- "Data Science"
freqs <- rbind(overall_letter, ds_letter)
freqs

# Save the file in case it's needed later after Wikipedia updates
write.csv(freqs,
          file = here::here("freqs.csv"),
          row.names = FALSE)
