library(ggplot2)
library(ggdebug)

# Start with a list of vectors to compare.
# Within each vector, there must not be any duplicated elements.
lst <- list(
  Set1 = c(letters[1:8]),
  Set2 = c(letters[20:26]),
  Set3 = c(letters[8:20])
)

# Use prepare_venn() to convert the list into a data.frame.
# of the correct format. You can add extra columns to the data.frame.
# Here we add a column named fill.
df <- prepare_venn(lst, fill = c("blue", "green", "red"))

# Add continuous fills to the ellipse segments
ggdebug(ggvd:::GeomVenn$setup_data)
ggundebug(ggvd:::GeomVenn$setup_data)
ggdebug(ggvd:::GeomVenn$setup_params)

ggplot() +
  geom_venn(aes(set_name = set_name, elements = elements, fill = count),
            data = df, type = "continuous") +
  theme_void() +
  scale_fill_gradientn(colors = alpha(c("white", "red"), 0.7))



data <- aaa
params <- bbb
n <- 360L

data2 <- data[, !names(data) %in% "elements"]
data_long <- data2[rep(seq_len(params$n_sets), each = n), ]

str(ggvd:::ggvd_data)


data3 <- cbind(data_long, ggvd:::ggvd_data[[as.character(params$n_sets)]][["discrete"]])

if (params$type == "continuous") {
  fill_df <- merge(ggvd:::ggvd_data[[as.character(params$n_sets)]][[params$type]], 
                   params$count_matrix[, c("count", "segment")], by = "segment")
  names(fill_df)[names(fill_df) == "count"] <- "fill"
  data4 <- rbind(data3, fill_df)
}
data4



