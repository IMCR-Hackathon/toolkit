# Calls to the IMCR OntoSoft API

# Get JSON --------------------------------------------------------------------

# Get JSON for each software
ids <- jsonlite::fromJSON('http://imcr.ontosoft.org/repository/software')$id
json <- lapply(ids, jsonlite::fromJSON)

# Parse JSON ------------------------------------------------------------------

# Get keywords
keywords <- lapply(
  seq_along(json),
  function(x){
    json[[x]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]$label
  }
)

# Get implementation languages
languages <- lapply(
  seq_along(json),
  function(x){
    json[[x]]$value[['http://ontosoft.org/software#hasImplementationLanguage']]$label
  }
)

# Get creators
creators <- lapply(
  seq_along(json),
  function(x){
    json[[x]]$value[['http://ontosoft.org/software#hasCreator']]$label
  }
)

# Plot ------------------------------------------------------------------------
# Data viz examples from here: http://loiyumba.github.io/2016-08-01-eurocup2016vis/

# Plot implementation languages
dfc <- plyr::count(unlist(languages))
other <- sum(dfc$freq[(dfc$x != 'Python') & (dfc$x != 'R')])
dfc <- data.frame(
  x = c('R', 'Python', 'Other'),
  freq = c(dfc$freq[dfc$x == 'R'], dfc$freq[dfc$x == 'Python'], other),
  stringsAsFactors = FALSE
)
dfc$freq <- (dfc$freq/sum(dfc$freq))*100
colnames(dfc) <- c("class", "freq")
dfc$class <- factor(dfc$class, levels = rev(as.character(dfc$class)))


# ggplot2::ggplot(dfc[1:10, ], aes("", freq, fill = class)) +
#   geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
#   coord_polar("y") +
#   geom_text(aes(label = paste0(round(freq), "%")),
#             position = position_stack(vjust = 0.5),
#             size = 5) +
#   labs(x = NULL, y = NULL, fill = NULL,
#        title = "Implementation Languages") +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   # scale_fill_brewer(type = 'qual', palette = 1) +
#   scale_fill_brewer('Blues') +
#   theme_classic() +
#   guides(fill=guide_legend(title=NULL)) +
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         title = element_text(size = 20),
#         legend.text = element_text(size = 18))


# Plot keywords
# dfc <- plyr::count(unlist(keywords))
# colnames(dfc) <- c("term", "freq")
# dfc <- arrange(dfc, desc(freq))
# ggplot2::ggplot(dfc, aes(x = reorder(term, freq), y = freq)) +
#   geom_bar(stat = "identity", fill = '#0091ea') +
#   geom_text(aes(label = freq), hjust = -0.2) + ###
#   coord_flip() +
#   ggtitle("Total Software by Task") +
#   labs(x = "", y = "Number") +
#   theme(axis.text.y = element_text(size = 14),
#         axis.text.x = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         title = element_text(size = 20))


# # Plot creators
# # As workcloud https://www.jasondavies.com/wordcloud/
# creators <- unlist(creators)
# use_i <- (creators == 'Matt Jones') | (creators == 'Matthew B. Jones')
# creators[use_i] <- 'Matt Jones'
# write.csv(creators, "C:\\Users\\Colin\\Desktop\\sandbox\\ontosoft\\word_cloud_creators.csv")





