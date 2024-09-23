#where are we?
getwd()

#Import Data to Tinker With
attach(mtcars)
View(mtcars)
colnames(mtcars)

#mpg = mils per gallon
#cyl = numbe rof cylinders in engine
#disp = displacement (cubic inches, volume of all cylinders)
#hp = horsepower
#drat = rear axle ratio (ratio between number of revolution of the drive shaft vs the rear wheels)
#wt = weight in pounds
#qsec = time in seconds to speed from 0 to a quarter of a mile per hour
#vs = engine configurations V (0) or Straight (1)
#am = automatic (0) or manual (1) transmission
#gear = number of gears
#carbs = number of carburetors


#Tables ----

#Base R functions ---- These will create table in your console only

table(c(1,2,3))

#summary() ## Provides summary statistics for a data frame or an individual vector.

summary(mtcars)


#aggregate() #Computes summary statistics (like mean, sum) for a dataset grouped by one or more variables.

aggregate(mpg ~ cyl, data = mtcars, FUN = mean)


#And many others Base R figure functions!
#margin.table()
#ftable()
#prop.table()
#addmargins()
#xtabs()
#by()
#tapply()

#Other Results that R will spit out in a table you may want to keep ----

#Results from a T-test

vs0 <- subset(mtcars, vs == 0)
vs1 <- subset(mtcars, vs == 1)

t.test(vs0$mpg, vs1$mpg)
ttestresult <- t.test(vs0$mpg, vs1$mpg)
print(ttestresult)

#Saving console output into: -----

#Saving your console output to a text file ----

#Create text file to "sink" output into
sink("results.txt")
#Create output
t.test(vs0$mpg, vs1$mpg)
#Close sink function
sink()

#Create text file to "sink" output into
sink("results.txt")
#Print title
print("T-Test mpg of automatic cars vs mpg of gear cars")
#Create output
t.test(vs0$mpg, vs1$mpg)
#Close sink function
sink()

#Don't overwrite your text file!
sink("results.txt", append=TRUE)
#Let's append to the text file the summary statistic of a linear model from this dataset
lm(mpg~wt+disp+hp)
sink()

##  QUESTIONS???

#stargazer: A Package for Regression an Summary Statistics Tables ----

#These tables let you compare the summary statistics of several models in one single table

#Outputs: LaTeX, HTML, ASCII

#Let's create a summary statistics table from our dataset with the stargazer package

#install.packages("stargazer")
library(stargazer)

#These are three basic arguments of the stargazer function
stargazer(mtcars, 
          type= "text", 
          title= "Summary Statistics", 
          out= "dat1.txt")

#stargazer does not support APA style. You can adjust some details in the code (like number of decimals, or keeping statistics in the same row) but the rest (like italics for p-values or aligning number you will have to do mnually in LaTeX or HTML)
stargazer(mtcars, 
          type= "text", 
          title= "Summary Statistics", 
          digits = 2, 
          single.row = TRUE, 
          out= "dat1_apa.txt")

#Let's create a few models from our cars dataset and make a table for them with the stargazer package

m1 <- lm(mpg ~ hp, mtcars)
m2 <- lm(mpg~ drat, mtcars)
m3 <- lm(mpg ~ hp + drat, mtcars)

stargazer(m1, m2, m3,
          type = "html",
          out = "reg1.html",
          digits = 1,
          header = FALSE,
          title= "Regression Results",
          covariate.labels = c("Horsepower", "Rear axle ratio"))


##  QUESTIONS???

#knitr ----
#install.packages("knitr")
library(knitr)
?kable #simple table generator. can output laxet or html
kable(head(mtcars), caption = "A Simple Table from the mtcars dataset")

kable(head(mtcars), row.names = FALSE, caption = "Table withOUT Row Names")

kable(head(mtcars), col.names = c("Miles/Gallon", "Cylinders", "Displacement", "Horsepower", 
                                  "Rear Axle Ratio", "Weight", "Quarter Mile Time", 
                                  "V/S", "Transmission", "Gears", "Carburetors"),
      caption = "Customized Column Names")


kable(head(mtcars), format = "html", align = "c", caption = "Centered and HTML-formatted Table")


#install.packages('kableExtra')
library(kableExtra)

# Create a styled table
kable(head(mtcars), format = "html", caption = "Styled Table with kableExtra") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

kable(mtcars, format = "html", caption = "Styled Table with kableExtra") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store the full version of the previous table in an object
html_table <- kable(mtcars, format = "html", caption = "mtcars Dataset - HTML Table") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Save the HTML table to a file
save_kable(html_table, "mtcars_table.html")

#Create a similar table in latex
kable(mtcars, format = "latex", caption = "mtcars Dataset - LaTeX Table") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Create a simple LaTeX table
latex_table <- kable(mtcars, format = "latex", caption = "mtcars Dataset - LaTeX Table") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Save the LaTeX table to a file
save_kable(latex_table, "mtcars_table.tex")

# Subset of the mtcars dataset (first 5 columns)
subset_table <- kable(mtcars[, 1:5], format = "html", caption = "Subset of mtcars - HTML") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Save the subset table as HTML
save_kable(subset_table, "mtcars_subset_table.html")

# Summary statistics table (mean of each column)
summary_stats <- sapply(mtcars, mean)
summary_table <- kable(as.data.frame(summary_stats), format = "html", caption = "Summary Statistics - HTML") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Save the summary table as LaTeX
save_kable(summary_table, "mtcars_summary_table.html")

?sapply #return list after applying desired function
?as.data.frame #Functions to check if an object is a data frame, or coerce it if possible.

#APA Style is not built in in knitr but here are some things we can do: a header that is centered, centering the content of the table, footnote. Bold and italics require adding that to html out <b></b><i></i>

# Create an APA-style table from the mtcars dataset
apa_table <- kable(mtcars[1:5, 1:4], format = "html", booktabs = TRUE, 
                   caption = "Table 1\nDescriptive Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Vehicle Characteristics" = 4)) %>%  # Grouping columns
  footnote(general = "Note. This table provides descriptive statistics for the mtcars dataset.", 
           general_title = "Note.",
           footnote_as_chunk = TRUE)

# Save the table as an HTML file
save_kable(apa_table, "apa_style_table.html")

#full_width = False means that the tale will not occupy the full width of its container, but just the necessary width  based on the content of the columns 
#" "= 1 means that the first column in the header will be blank
#"Vehicle Characteristics" = 4 means that the label "vehicle characteristics" will span across 4 columns

##  QUESTIONS???

#gt ---- outputs html, latex, rtf, png, word if you use RMarkdown or Quarto, powerpoint via Officer Package, plain text. no apa styling format

#gt() creates a gt table object that you can then style

#install.packages("gt")
#install.packages("dplyr")
library(gt)
library(dplyr)

# Create a basic gt table from the mtcars dataset
basic_table <- mtcars %>%
  head(5) %>%  # Display only the first 5 rows
  gt() %>%
  tab_header(
    title = "Mtcars Dataset",
    subtitle = "Summary of the First 5 Rows"
  ) %>%
  fmt_number( #fomatting numeric values (number of decimals, type of separators)
    columns = vars(mpg, cyl, disp, hp, drat), #vars specifies which columns of the dataframe should be affected by this operation 
    decimals = 1
  ) %>%
  cols_label(
    mpg = "Miles Per Gallon",
    cyl = "Cylinders",
    disp = "Displacement",
    hp = "Horsepower",
    drat = "Rear Axle Ratio"
  )

# Print the gt table
basic_table

# Highlight rows based on condition and format the table
highlighted_table <- mtcars %>%
  head(10) %>%  # Display the first 10 rows
  gt() %>%
  tab_header(
    title = "Mtcars Dataset",
    subtitle = "Highlighted Rows: Cars with More than 6 Cylinders"
  ) %>%
  fmt_number(
    columns = vars(mpg, cyl, disp, hp),
    decimals = 1
  ) %>%
  tab_row_group(
    group = "Cylinders > 6",
    rows = cyl > 6
  ) %>%
  tab_row_group( #grouping rows by a specific value
    group = "Cylinders <= 6",
    rows = cyl <= 6
  ) %>%
  cols_label(
    mpg = "Miles Per Gallon",
    cyl = "Cylinders",
    disp = "Displacement",
    hp = "Horsepower"
  )

# Print the highlighted table #CHECK THAT IT ACTUALLY HIGHLIGHTS IT
highlighted_table

# Summary statistics table
summary_stats_table <- mtcars %>%
  summarise(
    Mean_MPG = mean(mpg),
    Mean_HP = mean(hp),
    Max_MPG = max(mpg),
    Max_HP = max(hp),
    Min_MPG = min(mpg),
    Min_HP = min(hp)
  ) %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics of Mtcars Dataset",
    subtitle = "Miles Per Gallon (MPG) and Horsepower (HP)"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2
  )

# Print the summary statistics table
summary_stats_table

# Grouped data table by cylinder count
grouped_table <- mtcars %>%
  group_by(cyl) %>% #grouop by number of cilinders
  summarise(
    Avg_MPG = mean(mpg),
    Avg_HP = mean(hp),
    Max_MPG = max(mpg),
    Max_HP = max(hp)
  ) %>%
  gt() %>%
  tab_header(
    title = "Grouped by Cylinder Count",
    subtitle = "Average and Maximum MPG and Horsepower"
  ) %>%
  fmt_number(
    columns = vars(Avg_MPG, Avg_HP, Max_MPG, Max_HP),
    decimals = 1
  ) %>%
  cols_label(
    cyl = "Cylinders",
    Avg_MPG = "Average MPG",
    Avg_HP = "Average HP",
    Max_MPG = "Max MPG",
    Max_HP = "Max HP"
  )

# Print the grouped table
grouped_table

# Table with footnotes and source notes
footnote_table <- mtcars %>%
  head(5) %>%
  gt() %>%
  tab_header(
    title = "Mtcars Dataset",
    subtitle = "Table with Footnotes"
  ) %>%
  cols_label(
    mpg = "Miles Per Gallon",
    cyl = "Cylinders",
    disp = "Displacement",
    hp = "Horsepower"
  ) %>%
  tab_footnote( #add footnote
    footnote = "MPG: Miles per Gallon", #text of footnote
    locations = cells_column_labels(columns = vars(mpg)) #location of footname: under the column named mpg
  ) %>%
  tab_source_note(
    source_note = "Source: mtcars dataset from R."
  )

# Print the table with footnotes
footnote_table

#generating the tables in the format of your preference

?gtsave #save a gt table to a file

basic_table %>% gtsave("table.html")

basic_table %>% gtsave("table.tex")

basic_table %>% gtsave("table.rtf")

#install.packages("webshot2")
library(webshot2)
#basic_table %>% gtsave("table.png")

#install.packages("officer")
library(officer)

# Add the table image to a PowerPoint slide
#read_pptx() %>%
#  add_slide() %>%
#  ph_with(external_img("table.png"), location = ph_location_type(type = "body")) %>%
#  print(target = "presentation.pptx")

#In RMarkdown you'd just have to write
#basic_table



##  QUESTIONS???

#gtsummary ---- creates summary tables, allows for html, latex, word, rtf and plain text outputs

??tbl_summary #falcaulates descriptive statistics


# Load necessary libraries
library(gtsummary)

# Create a basic summary table of the mtcars dataset
basic_summary <- mtcars %>%
  tbl_summary()  # Summarize the entire dataset

# Display the table
basic_summary


# Create a summary table grouped by number of cylinders
grouped_summary <- mtcars %>%
  tbl_summary(by = cyl)  # Group by the 'cyl' column

# Display the grouped summary table
grouped_summary

# Custom summary table showing mean and standard deviation
custom_summary <- mtcars %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})")  # Show mean and standard deviation for all continuous variables
  )

# Display the custom summary table
custom_summary

# Custom labels for the variables
custom_label_summary <- mtcars %>%
  tbl_summary(
    label = list(
      mpg ~ "Miles Per Gallon",
      cyl ~ "Cylinders",
      disp ~ "Displacement",
      hp ~ "Horsepower"
    )
  )

# Display the summary table with custom labels
custom_label_summary

# Fit a linear regression model
lm_model <- lm(mpg ~ wt + hp + cyl, data = mtcars)

# Create a regression summary table
regression_summary <- lm_model %>%
  tbl_regression()

# Display the regression summary table
regression_summary

# Combine summary table and regression table
combined_table <- tbl_merge(
  tbls = list(custom_summary, regression_summary),
  tab_spanner = c("**Summary Statistics**", "**Regression Results**") #adds title to each table
)

# Display the combined table
combined_table

#creating APA style table
# Load necessary libraries
#install.packages("gtsummary")
library(gtsummary)
library(gt)

# Create a summary table from the mtcars dataset
apa_summary <- mtcars %>%
  select(mpg, cyl, disp, hp, wt) %>%  # Select relevant columns
  tbl_summary(
    label = list(
      mpg ~ "Miles per Gallon",
      cyl ~ "Cylinders",
      disp ~ "Displacement",
      hp ~ "Horsepower",
      wt ~ "Weight"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",  # Show mean and standard deviation
    missing = "no"  # Exclude missing data
  ) %>%
  # Add a header to approximate APA-style
  modify_header(label ~ "**Variable**") %>%  # Rename the column label to "Variable"
  modify_caption("**Table 1.** Descriptive Statistics of the mtcars Dataset") %>%  # Add caption in APA style
  modify_footnote(
    all_stat_cols() ~ "Mean (SD) values are presented for each variable.",  # Add footnote for statistics
    abbreviation = TRUE
  ) %>%
  as_gt() %>%  # Convert to a gt object for further customization
  # Further gt formatting for APA style
  tab_options(
    table.font.names = "Times",  # Set font to Times for APA style
    table.border.top.style = "none",  # Remove top border
    table.border.bottom.style = "none",  # Remove bottom border
    column_labels.border.top.style = "none",  # Remove top border from column labels
    column_labels.border.bottom.width = px(1),  # Thin line under column labels
    table_body.hlines.style = "none",  # Remove horizontal lines in the body
    table_body.vlines.style = "none"  # Remove vertical lines
  )

# Display the APA-style table
apa_summary

#export

#html
gtsave(apa_summary, "apa_summary_table.html")

# Save the table as an RTF file
gtsave(apa_summary, "apa_summary_table.rtf")

#latex
# Save the table as LaTeX code for a PDF document
gtsave(apa_summary, "apa_summary_table.tex")

#plain text
# Export as plain text (raw HTML) to a .txt file
plain_text_output <- as_raw_html(apa_summary)
write(plain_text_output, file = "apa_summary_table.txt")


# Export the table to a Word document using RMarkdown or Quarto (in a .Rmd or .qmd file)
# Within an RMarkdown document, you would use:
#apa_summary %>%
#  as_flextable() %>%
#  flextable::save_as_docx(path = "apa_summary_table.docx")


#Figures ----
#Base R Functions ----

#plot() #creates scatter and line plots

plot(mpg, wt)
plot(mpg, wt, main = "Miles per Gallon and Number of Cylinders in a car", xlab = "Miles per Gallon", ylab = "Weight of Car (Pounds)")

#hist() # creates a histogram of the distribution of a numeric vector
hist(qsec)
hist(qsec, main = "Distribution of Speeding time from 0 to a Quarter Mile per Hour", xlab = "Seconds", ylab = "Number of Cars in the Dataset")

#boxplot() #box and whiskers plot
boxplot(hp)
boxplot(hp, main = "Horse Power in the Dataset", ylab = "Horsepower")

#qqnorm() # normality assessment by plotting sampling quatiles to normal quantiles
qqnorm(mtcars$mpg)

#And many others Base R figure functions!
#qqplot()
pairs(mtcars)
#barplot()
pie(gear)
#matplot()
#stripchart()
#image()

#Saving your console output to an image file ----

#create file
png("mpg_cyl.png")
#create graph
plot(mpg, cyl, main = "Miles per Gallon and Number of Cylinders in a car", xlab = "Miles per Gallon", ylab = "Number of Cyllinders")
#send graph to file
dev.off()

#Saving multiples figures into one image file

#create file
png("four_figures.png")
#create graph
par(mfrow=c(2,2))
plot(mpg, cyl, main = "Miles per Gallon and Number of Cylinders in a car", xlab = "Miles per Gallon", ylab = "Number of Cyllinders")
hist(qsec, main = "Distribution of Speeding time from 0 to a Quarter Mile per Hour", xlab = "Seconds", ylab = "Number of Cars in the Dataset")
boxplot(hp, main = "Horse Power in the Dataset", ylab = "Horsepower")
qqnorm(mtcars$mpg)
dev.off()

##  QUESTIONS???
#ggplot2 ----
#install.packages("ggplot2")
library(ggplot2)

# Scatter plot of MPG vs Horsepower
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "Miles per Gallon vs Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal()

# Boxplot of MPG by Cylinders
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Miles per Gallon by Number of Cylinders",
       x = "Cylinders",
       y = "Miles per Gallon") +
  theme_minimal()

# Histogram of MPG
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Distribution of Miles per Gallon",
       x = "Miles per Gallon",
       y = "Frequency") +
  theme_minimal()

# Bar plot of Average Horsepower by Cylinder
ggplot(mtcars, aes(x = as.factor(cyl), y = hp)) +
  stat_summary(fun = "mean", geom = "bar", fill = "orange", color = "black") +
  labs(title = "Average Horsepower by Number of Cylinders",
       x = "Cylinders",
       y = "Average Horsepower") +
  theme_minimal()


# Line plot of MPG for each car in the dataset
ggplot(mtcars, aes(x = rownames(mtcars), y = mpg)) +
  geom_line(color = "purple", group = 1) +
  geom_point(color = "red") +
  labs(title = "Miles per Gallon for Each Car",
       x = "Car",
       y = "Miles per Gallon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("scatter_plot_mpg_vs_hp.png", width = 6, height = 4)

ggsave("scatter_plot_mpg_vs_hp.pdf", width = 6, height = 4)

ggsave("scatter_plot_mpg_vs_hp.jpeg", width = 6, height = 4)

ggsave("scatter_plot_mpg_vs_hp.tiff", width = 6, height = 4)

#APA style figure
#install.packages("gridExtra")
library(gridExtra)  # For combining multiple plots if needed

# Create the plot with APA style elements
ggplot(mtcars, aes(x = hp, y = mpg)) + #aesthetics mapping
  geom_point(size = 3, color = "black") +          # Black points for clarity
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Relationship Between Horsepower and Miles per Gallon",
       x = "Horsepower",
       y = "Miles per Gallon") +
  theme_minimal(base_size = 12) +                   # Simplified theme, base font size 12
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    axis.title.x = element_text(face = "bold"),    # Bold x-axis label
    axis.title.y = element_text(face = "bold"),    # Bold y-axis label
    axis.text = element_text(size = 12),           # Make axis text a bit larger
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "grey80"),  # Light grey gridlines
    panel.grid.minor = element_blank(),            # Remove minor gridlines
    panel.border = element_blank(),                # Remove the panel border
    axis.line = element_line(size = 0.5)           # Add axis lines
  )

ggsave("APA_Style_Figure.png", width = 6, height = 4)

# Fit a simple linear model predicting mpg based on hp
model <- lm(mpg ~ hp, data = mtcars)
# Create a data frame with actual and predicted values
mtcars$predicted_mpg <- predict(model, mtcars)
# Load the ggplot2 package
library(ggplot2)

# Create the plot
ggplot(mtcars, aes(x = hp)) +
  geom_point(aes(y = mpg), color = "blue", size = 3, alpha = 0.6) +  # Actual data points
  geom_line(aes(y = predicted_mpg), color = "red", size = 1.2) +  # Predicted values (line)
  labs(title = "Actual vs Predicted MPG Based on Horsepower",
       x = "Horsepower (HP)",
       y = "Miles per Gallon (MPG)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    axis.title.x = element_text(face = "bold"),    # Bold x-axis label
    axis.title.y = element_text(face = "bold")     # Bold y-axis label
  )

ggsave("actual_vs_predicted_mpg.png", width = 6, height = 4)


##  QUESTIONS???

#patchwork ---- #Combining plots

#storing plots

scatter_plot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "MPG vs Horsepower", x = "Horsepower", y = "Miles per Gallon") +
  theme_minimal()

histogram_plot <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black") +
  labs(title = "Distribution of Miles per Gallon", x = "Miles per Gallon", y = "Frequency") +
  theme_minimal()

boxplot_cyl <- ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "MPG by Number of Cylinders", x = "Cylinders", y = "Miles per Gallon") +
  theme_minimal()


# Combine the scatter plot, histogram, and boxplot into a grid layout
#install.packages('patchwork')
library(patchwork)

combined_plot <- (scatter_plot | histogram_plot) / boxplot_cyl #vertical bar separates the plots horizontally, forward slash stacks plots vertically

# Display the combined plot
combined_plot

# Save the combined plot to a file
ggsave("combined_plot_mtcars.png", plot = combined_plot, width = 8, height = 6)


##  QUESTIONS???

#sjplot ----

# Install the sjPlot package if you haven't already
# install.packages("sjPlot")
# install.packages("sjmisc")  # Required for data utility functions

# Load the necessary libraries
#install.packages("sjPlot")
#install.packages("sjmisc")
library(sjPlot)
library(sjmisc)
library(ggplot2)  # For working with ggplot objects

# Fit a linear model
model <- lm(mpg ~ hp + cyl, data = mtcars)

# Plot the model
plot_model(model, type = "pred", terms = "hp") + #plot_model is a sjplot function, generates plot for the model
  labs(title = "Predicted MPG Based on Horsepower", #type = pred plots the predicted values by the model
       x = "Horsepower", #terms = hp asks the figure to focus on hp as the predictor of interest
       y = "Predicted Miles per Gallon")

# Grouped frequency plot of MPG by Cylinders

# Visualize a correlation matrix between select variables
sjp.corr(mtcars[, c("mpg", "hp", "wt", "cyl")], 
         corr.method = "pearson", 
         title = "Correlation Matrix: MPG, HP, WT, and Cylinders")

# Save the plot to a PNG file
ggsave("sjplot_mtcars.png", width = 6, height = 4)


#APA Style with sjPlot

# Load required libraries
library(sjPlot)
library(ggplot2)

# Fit a linear model predicting mpg from hp and cyl
model_lm <- lm(mpg ~ hp + cyl, data = mtcars)

# Generate predicted values from the model
mtcars$predicted_mpg <- predict(model_lm)

# Create the APA-style plot comparing actual and predicted MPG
plot_actual_vs_predicted <- ggplot(mtcars, aes(x = hp)) +
  
  # Plot the actual MPG values
  geom_point(aes(y = mpg), color = "blue", size = 3, alpha = 0.6, shape = 16) +  # Actual data points
  
  # Plot the predicted MPG values as a line
  geom_line(aes(y = predicted_mpg), color = "red", size = 1.2, linetype = "solid") +  # Predicted line
  
  # APA style labels and title
  labs(title = "Actual vs Predicted MPG by Horsepower", 
       x = "Horsepower (HP)", 
       y = "Miles per Gallon (MPG)") +
  
  # APA style theming
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    axis.title.x = element_text(face = "bold"),             # Bold x-axis label
    axis.title.y = element_text(face = "bold"),             # Bold y-axis label
    panel.grid.major = element_line(color = "grey80"),      # Light grey major gridlines
    panel.grid.minor = element_blank(),                     # Remove minor gridlines
    axis.text = element_text(size = 12),                    # Axis text size
    panel.border = element_blank(),                         # Remove plot border
    axis.line = element_line(size = 0.5)                    # Add axis lines
  )

# Display the plot
plot_actual_vs_predicted


##  QUESTIONS???
#ggeffects ----

# Install the ggeffects package if you haven't already
# install.packages("ggeffects")

# Load necessary libraries
#install.packages("ggeffects")
library(ggplot2)
library(ggeffects) # for complex models
# Fit a linear model predicting mpg based on hp and cyl
model_lm <- lm(mpg ~ hp + cyl, data = mtcars)
# Get predicted values for hp while controlling for cyl
predicted_effects <- ggpredict(model_lm, terms = "hp")
# Plot the predicted effects of horsepower (hp) on mpg
plot(predicted_effects) +
  labs(title = "Predicted MPG Based on Horsepower",
       x = "Horsepower (HP)", 
       y = "Predicted Miles per Gallon (MPG)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    axis.title.x = element_text(face = "bold"),             # Bold x-axis label
    axis.title.y = element_text(face = "bold"),             # Bold y-axis label
    axis.text = element_text(size = 12)                     # Adjust axis text size
  )

#A more complex model, interaction terms

# Fit a complex model with interaction terms
model_complex <- lm(mpg ~ hp * wt + cyl, data = mtcars)
# Get predicted values for hp at different levels of wt (interaction term)
predicted_effects_complex <- ggpredict(model_complex, terms = c("hp", "wt [2, 3.5]"))
# Plot the predicted effects of horsepower on mpg at different weights (2 and 3.5)
plot(predicted_effects_complex) +
  labs(title = "Predicted MPG by Horsepower and Weight",
       x = "Horsepower (HP)", 
       y = "Predicted Miles per Gallon (MPG)",
       color = "Weight (WT)") +  # Legend label for weight
  theme_minimal(base_size = 12) +  # APA-style minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    axis.title.x = element_text(face = "bold"),             # Bold x-axis label
    axis.title.y = element_text(face = "bold"),             # Bold y-axis label
    panel.grid.major = element_line(color = "grey80"),      # Light grey major gridlines
    panel.grid.minor = element_blank(),                     # Remove minor gridlines
    axis.text = element_text(size = 12),                    # Adjust axis text size
    legend.position = "right"                               # Position legend on the right
  )

###Questions