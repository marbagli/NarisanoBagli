library(dplyr)
library(ggplot2)
library(VennDiagram)
library(readxl)
library(stringr)

data <- read.csv("/Users/marcobagli/Documents/marco/NarisanoBagliData.csv", sep=";")

taste <- data %>% 
  filter(verb == "taste") %>% 
  group_by(category) %>% 
  summarise(Count=sum(frequency))

ggplot(taste, aes(x = reorder(category, -Count), y=Count, fill=category))+
  geom_bar(stat="identity", width=0.5, color="black")+
  geom_text(aes(label = Count, y = Count), angle= 0, size=4, color="black", vjust=1.5)+
  scale_x_discrete(labels = str_to_upper(levels(factor(taste$category)))) +
  scale_fill_manual(values = c("result" = "#fc3103", "experience" = "#fcba03", "feature" = "#fcfc03", 
                               "feeling"= "#03fc03", "authority"="#03fcdf")) +
  labs(title= "Semantic categories of the direct objects of taste",
       x = element_blank(),
       y = "Frequency")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle=0, hjust=0.5),
    axis.ticks.x = element_blank(),
    legend.position= "none")

smell <- data %>% 
  filter(verb == "smell") %>% 
  group_by(category) %>% 
  summarise(Count=sum(frequency))

ggplot(smell, aes(x = reorder(category, -Count), y=Count, fill=category))+
  geom_bar(stat="identity", width=0.5, color="black")+
  #geom_text(aes(label = category, y = Count/2), angle= 90, size=4, color="black")+
  labs(title= "Semantic categories of the direct objects of smell",
       x = "Categories",
       y = "Frequency")+
  theme_minimal()+
  theme(
    legend.title=element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position= "bottom")


taste_words <- data %>%
  filter(verb == "taste") %>%
  pull(object) %>%
  unique()

smell_words <- data %>%
  filter(verb == "smell") %>%
  pull(object) %>%
  unique()

# Create the Venn Diagram
venn.plot <- draw.pairwise.venn(
  area1 = length(taste_words),
  area2 = length(smell_words),
  cross.area = length(intersect(taste_words, smell_words)),
  category = c("Taste", "Smell"),
  fill = c("gold", "skyblue"),
  alpha = c(0.5, 0.5),
  lty = "solid",
  cex = 1.5,
  cat.cex = 1.5,
  cat.col = c("darkgoldenrod", "darkblue")
)

# Display the plot
grid.draw(venn.plot)


library(grid)


# Prepare unique words for each verb
taste_words <- data %>%
  filter(verb == "taste") %>%
  pull(object) %>%
  unique()

smell_words <- data %>%
  filter(verb == "smell") %>%
  pull(object) %>%
  unique()

# Calculate shared and unique words
common_words <- intersect(taste_words, smell_words)
taste_only_words <- setdiff(taste_words, smell_words)
smell_only_words <- setdiff(smell_words, taste_words)

# Create the Venn diagram
venn.plot <- draw.pairwise.venn(
  area1 = length(taste_words),
  area2 = length(smell_words),
  cross.area = length(common_words),
  category = c("Taste", "Smell"),
  fill = c("gold", "skyblue"),
  alpha = c(0.5, 0.5),
  lty = "solid",
  cex = 1.5,
  cat.cex = 1.5,
  cat.col = c("darkgoldenrod", "darkblue")
)

# Add words to the Venn diagram
grid.text(
  paste(taste_only_words, collapse = "\n"), 
  x = 0.3, y = 0.5, 
  gp = gpar(fontsize = 10)
)
grid.text(
  paste(smell_only_words, collapse = "\n"), 
  x = 0.7, y = 0.5, 
  gp = gpar(fontsize = 10)
)
grid.text(
  paste(common_words, collapse = "\n"), 
  x = 0.5, y = 0.5, 
  gp = gpar(fontsize = 10)
)

######################
# Install necessary packages if not already installed
if (!requireNamespace("VennDiagram", quietly = TRUE)) {
  install.packages("VennDiagram")
}
if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
}

# Load libraries
library(VennDiagram)
library(tm)
library(wordcloud)

# Prepare unique words for each verb
taste_words <- data %>%
  filter(verb == "taste") %>%
  pull(object) %>%
  unique()

smell_words <- data %>%
  filter(verb == "smell") %>%
  pull(object) %>%
  unique()

# Calculate shared and unique words
common_words <- intersect(taste_words, smell_words)
taste_only_words <- setdiff(taste_words, smell_words)
smell_only_words <- setdiff(smell_words, taste_words)

# Create the Venn diagram
venn.plot <- draw.pairwise.venn(
  area1 = length(taste_words),
  area2 = length(smell_words),
  cross.area = length(common_words),
  category = c("Taste", "Smell"),
  fill = c("gold", "skyblue"),
  alpha = c(0.5, 0.5),
  lty = "solid",
  cex = 1.5,
  cat.cex = 1.5,
  cat.col = c("darkgoldenrod", "darkblue")
)

# Generate word clouds for each region
par(new = TRUE) # Overlay the word clouds on the Venn diagram

# Word cloud for "Taste Only" words
wordcloud(
  words = taste_only_words,
  scale = c(1.5, 0.5), # Adjust the scale for word size
  colors = "gold",
  random.order = FALSE
)

# Word cloud for "Smell Only" words
wordcloud(
  words = smell_only_words,
  scale = c(1.5, 0.5),
  colors = "skyblue",
  random.order = FALSE
)

# Word cloud for "Common" words
wordcloud(
  words = common_words,
  scale = c(1.5, 0.5),
  colors = "green",
  random.order = FALSE
)





# Prepare data for each verb
taste_data <- data %>%
  filter(verb == "taste") %>%
  group_by(object) %>%
  summarise(Taste_Frequency = sum(frequency))

smell_data <- data %>%
  filter(verb == "smell") %>%
  group_by(object) %>%
  summarise(Smell_Frequency = sum(frequency))

# Merge datasets and classify words
combined <- full_join(taste_data, smell_data, by = "object") %>%
  #replace_na(list(Taste_Frequency = 0, Smell_Frequency = 0)) %>%
  mutate(Word_Type = case_when(
    Taste_Frequency > 0 & Smell_Frequency > 0 ~ "Common",
    Taste_Frequency > 0 ~ "Taste Only",
    Smell_Frequency > 0 ~ "Smell Only"
  ))

# Add jittered positions for plotting
set.seed(123) # For reproducibility
combined <- combined %>%
  mutate(
    x_pos = case_when(
      Word_Type == "Taste Only" ~ runif(n(), 0, 0.4),
      Word_Type == "Smell Only" ~ runif(n(), 0.6, 1),
      Word_Type == "Common" ~ runif(n(), 0.4, 0.6)
    ),
    y_pos = runif(n(), 0, 1)
  )

# Plot the Venn-style scatter plot
ggplot(combined, aes(x = x_pos, y = y_pos, size = Taste_Frequency + Smell_Frequency, color = Word_Type)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = object), hjust = 0, vjust = 1, size = 3) +
  scale_color_manual(values = c("Taste Only" = "black", "Smell Only" = "red", "Common" = "magenta")) +
  labs(
    title = "Semantic Overlap Between 'Taste' and 'Smell'",
    x = NULL,
    y = NULL,
    color = "Word Type",
    size = "Combined Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

