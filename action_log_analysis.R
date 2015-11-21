

data_file = '~/Documents/data/POND_actionlogs/PondActionLog.csv'

action_map_file =  '~/Documents/data/POND_actionlogs/PondActionIdMapping.csv'

histo_colors = c('darkorange1', 
                 'darkorchid', 
                 'deepskyblue1', 
                 'gold1', 
                 'deeppink1', 
                 'yellowgreen', 
                 'cadetblue', 
                 'maroon1', 
                 'navy', 
                 'tan4', 
                 'black')

LoadLibraries <- function(){
  library(gdata)
  library(ggplot2)
  library(plyr)
  library(dplyr)
  library(class)
  library(gmodels)
}

action_log <- read.csv(data_file)
head(action_log)

actionId_map <- read.csv(action_map_file)

nrow(action_log)
action_log <- action_log[action_log$pptId != '',]
action_log$when <- strptime(action_log$timestamp, format='%m/%d/%y %H:%M')
action_log$when.l <- as.integer(as.POSIXct(action_log$when))

## Fine for a moment, but should really be shifted by time of day when the study started
action_log$studyDay <- floor(action_log$mins / 1440)

action_log <- merge(action_log, actionId_map, by = 'actionId', all.x = TRUE )
action_log <- unique(action_log) ## It looks like there are 3-4 copies of each log in the data set. 

# actionId == 14 => start of study

table(action_log$typeId)

p1001 <- action_log[action_log$pptId == 'p1001', ]
p1001$when <- strptime(p1001$timestamp, format='%m/%d/%y %H:%M')
p1001$when.l <- as.integer(as.POSIXct(p1001$when))
p1001.start <- p1001[p1001$actionId == 14, ]


ggplot(data = p1001[1:100,], aes(x = when.l, y = pptId, 
                                 fill = factor(actionId), 
                                 color = factor(actionId), 
                                 alpha = 0.75)) + 
  geom_dotplot(binwidth = 60 * 60)

ggplot(data = p1001[1:1000,], aes(x = when, y = pptId, 
                                 fill = factor(actionId), 
                                 color = factor(actionId))) + 
  scale_color_manual( values = histo_colors) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60, )


ggplot(data = p1001[1:100,], aes(x = hrs, y = pptId, 
                                 fill = factor(actionId), 
                                 color = factor(actionId), 
                                 alpha = 0.75)) + 
  geom_dotplot(dotsize = .8, binwidth = 6)


## Plots all actionIds for all Ppts
ggplot(data = action_log, aes(x = when, y = pptId, 
                      fill = factor(actionId), 
                      color = factor(actionId))) + 
  scale_color_manual( values = histo_colors) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60, )

## Plots actionIds for all Ppts, by mins (which is mins from study start)
ggplot(data = unique(action_log), aes(x = mins, y = pptId, 
                              fill = actionName, 
                              color = actionName)) + 
  scale_color_manual( values = histo_colors) +
#  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60, )

## Plots actionIds for all Ppts, by days
ggplot(data = unique(action_log), aes(x = studyDay, y = 1, 
                                      fill = actionName, 
                                      color = actionName)) + 
  scale_color_manual( values = histo_colors) +
  scale_x_continuous(breaks = seq(0, 29), 
                     labels= minToDayLabels) +
  #  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60 ) + 
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1)) +
  facet_grid(pptId ~ .) + 
  ggtitle('All Ppts, All Actions')

ggplot(data = unique(action_log), aes(x = studyDay, y = 1, 
                                      fill = actionName, 
                                      color = actionName)) + 
  scale_color_manual( values = histo_colors) +
  scale_x_continuous(breaks = seq(0, 29), 
                     labels= minToDayLabels) +
  #  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60 ) + 
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1)) +
  facet_grid(actionName ~ .) + 
  ggtitle('All Ppts, All Actions')




all.actions.facet <- ggplot(data = unique(action_log), aes(x = studyDay, y = pptId, 
                                      fill = actionName, 
                                      color = actionName)) + 
  scale_color_manual( values = histo_colors) +
  scale_x_continuous(breaks = seq(0, 29)) +
  #  scale_x_datetime(breaks = date_breaks("1 day")) + 
  #geom_jitter(binwidth = 60 ) + 
  theme_light() + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = 14) + 
  geom_vline(xintercept = 21) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1)) +
  facet_grid(actionName ~ .) + 
  ggtitle('All Ppts, All Actions')

png(filename="allActionsFacetAllPpts.png", 
    units="in", 
    width=7, 
    height=10, 
    pointsize=12, 
    res=150)

print(all.actions.facet)
dev.off()

minToDayLabels <- lapply(seq(0, 42373, by = 60 * 24), 
                         FUN = function(x)(return(sprintf('Day %d', x/1440))))

## Plots all ppts, startApp
ggplot(data = action_log[action_log$actionId == 0,], aes(x = mins, y = pptId, 
                                      fill = actionName, 
                                      color = actionName)) + 
  scale_color_manual( values = histo_colors) +
  scale_x_continuous(breaks = seq(0, 42373, by = 60 * 24), 
                     labels= minToDayLabels) +
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1440 * 7) + 
  geom_vline(xintercept = 1440 * 14) + 
  geom_vline(xintercept = 1440 * 21) + 
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1)) +
  geom_point(binwidth = 60)


## Plots allPpts, when the study starts
ggplot(data = action_log[action_log$actionId == 14, ], aes(x = when, y = pptId, 
                              fill = factor(actionId), 
                              color = factor(actionId))) + 
  scale_color_manual( values = histo_colors) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  geom_jitter(binwidth = 60, )


action_log[action_log$actionId == 14, c('actionId', 'pptId', 'timestamp')]

stripchart(when.l ~ actionId, data = p1001[, c('when.l', 'actionId')], method = 'jitter', col = histo_colors)


unique(action_log[, c('actionId', 'comment')])

pptPlots <- llply(unique(action_log$pptId), 
                  .fun = function(x)(PlotOneUser(action_log, x)))



PlotOneUser <- function(df, pptId){
  p1001 <- action_log[df$pptId == pptId, ]
  p1001$when <- strptime(p1001$timestamp, format='%m/%d/%y %H:%M')
  p1001$when.l <- as.integer(as.POSIXct(p1001$when))
  
  pptPlot <- ggplot(data = p1001, aes(x = when, y = pptId, 
                                    fill = factor(actionId), 
                                    color = factor(actionId))) + 
    scale_color_manual( values = histo_colors) +
    scale_x_datetime(breaks = date_breaks("1 day")) + 
    geom_jitter(binwidth = 60, )
  print(pptPlot)
  return(pptPlot)
}



## Now, ddply to calculate the number of app starts for each day, and plot that. 


starts.only <- action_log[action_log$actionId == 0,
                           c('pptId', 'studyDay', 'actionId')]
num.starts.per.day <- ddply(starts.only, 
                           c('pptId', 'studyDay'), count)

num.starts.per.day.plot <- 
  ggplot(data = num.starts.per.day, aes(x = studyDay, y = n, 
                                      colour = pptId)) + 
  #scale_color_brewer(type = 'qual', palette = 'Set1') +
  scale_x_continuous(breaks = seq(-1, 29, by = 1), 
                     labels= c('Day -1', minToDayLabels)) +
  theme_light() +
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept =  7) + 
  geom_vline(xintercept = 14) + 
  geom_vline(xintercept = 21) + 
  geom_hline(yintercept = 3) + 
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1)) +
  geom_line() + 
  facet_grid(pptId ~ .) + 
  ggtitle('Number of times app was started each day')

png(filename="numStartsPerDayAllPpts.png", 
    units="in", 
    width=5, 
    height=10, 
    pointsize=12, 
    res=150)

print(num.starts.per.day.plot)
dev.off()


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

