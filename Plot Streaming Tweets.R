library(ggplot2)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(magrittr)

setwd('/Users/Kelly/Documents/BMIS694/Live Tweet Sentiment/Sentiment Files')

group1 = 'saints'
group2 = 'falcons'

# Loop that will plot the sentiment real time
        repeat {
                d = read.table(paste(group1,group2,'Sentiment.txt', sep = ''), sep = '\t', col.names = c("Name", "Time", "Sentiment"))
                dgroup = d %>% group_by( Name, Time, add = FALSE) %>% summarise(Sentiment = last(Sentiment)) %>% set_colnames(.,c("Name", "Time", "Sentiment"))
                dgroup = dgroup %>% top_n(n=15, Time)
                
                plt = ggplot(dgroup,  aes(x=Time,y=Sentiment, colour=Name)) + 
                        geom_line(aes(colour=Name, group=Name)) +
                        geom_hline(yintercept = 0) +
                        scale_color_manual(values=c('#A6192E','#D3BC8D'))
                print(plt)
                        
                Sys.sleep(60)
        }

# Code to plot entire file
        d = read.table(paste(group1,group2,'Sentiment.txt', sep = ''), sep = '\t', col.names = c("Name", "Time", "Sentiment"))
        
        dgroup = d %>% group_by( Name, Time, add = FALSE) %>% summarise(Sentiment = mean(Sentiment)) %>% set_colnames(.,c("Name", "Time", "Sentiment"))
        
        plt = ggplot(dgroup,  aes(x=Time,y=Sentiment, colour=Name)) +
                ggtitle("Sentiment Analysis of Saints vs. Falcons") +
                geom_line(aes(colour=Name, group=Name)) +
                geom_hline(yintercept = 0) +
                xlim(NA,15) +
                geom_vline(xintercept = 6, linetype =2,color = '#D3BC8D') +
                geom_vline(xintercept = 18, linetype =2,color = '#A6192E') +
                geom_vline(xintercept = 57,color = '#A6192E') +
                geom_vline(xintercept = 68,color = '#D3BC8D') +
                geom_vline(xintercept = 100,color = '#D3BC8D') +
                geom_vline(xintercept = 144,color = '#A6192E') +
                geom_vline(xintercept = 158, linetype =2,color = '#A6192E') +
                geom_vline(xintercept = 18, linetype =2,color = '#A6192E') +
                geom_vline(xintercept = 180) +
                scale_x_time() +
                theme(plot.title = element_text(face = 'bold', hjust = 0.5, vjust = 0.5)) + 
                scale_color_manual(values=c('#A6192E','#D3BC8D'))
        print(plt)
