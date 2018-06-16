# Modify the path below to match your folders structure
projFld <- "/Users/namsan/Desktop/Spring\ 2018/Big\ Data/projFld"

# Shortcuts to folders of interest
CleanData <- paste0(projFld,"/CleanData")
Dictionaries <- paste0(projFld,"/Dictionaries")
RawData <- paste0(projFld,"/RawData")
RCode <- paste0(projFld,"/RCode")
RData <- paste0(projFld,"/RData")
Output <- paste0(projFld,"/Output")

library(scales); library(grid); library(RColorBrewer)
library(ggplot2)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


# Average Star Rating by Category
densitytable.long1 <- copy(densitytable.long)
wordfreq.plot <- aggregate(frequency ~ word, densitytable.long, sum)
wordfreq.plot <- wordfreq.plot[with(wordfreq.plot,order(-frequency)), ]
wordfreq.plot <- head(wordfreq.plot,30)

#tiff('Average Star Rating by Category.tiff',units="in", width=6, height=4, res=300)
ggplot(data=wordfreq.plot, aes(reorder(word, frequency), frequency)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(frequency, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(y="Frequency", x="Words", title="Most frequent words")
#dev.off()



