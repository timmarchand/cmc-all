remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)

devtools::source_gist("2a1bb0133ff568cbe28d", 
                      filename = "geom_flat_violin.R")

ggplot(dimsf,aes(x=corpus,y=dimension1, fill = corpus))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal() +
  easy_remove_legend()

ggplot(dimsf,aes(x=corpus,y=dimension2, fill = corpus))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal()+
  easy_remove_legend()

ggplot(dimsf,aes(x=corpus,y=dimension3, fill = corpus))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal()+
  easy_remove_legend()

ggplot(dimsf,aes(x=corpus,y=dimension4, fill = mode))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal()+
  easy_remove_legend()

ggplot(dimsf,aes(x=corpus,y=dimension5, fill = corpus))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal()+
  easy_remove_legend()

ggplot(dimsf,aes(x=corpus,y=dimension6, fill = mode))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.2)) +
  theme_minimal()+
  easy_remove_legend()

filter1 <- ggplot(dims,aes(x=corpus,y=dimension1, fill = mode))+
geom_flat_violin(scale = "count", trim = FALSE) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.05)) + 
  geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
               position = position_nudge(-0.025)) + 
  
  theme_light() +
  theme(legend.position = "none") 
  
filter2 <-   ggplot(dims_500,aes(x=corpus,y=dimension1, fill = mode))+
  geom_flat_violin(scale = "count", trim = FALSE) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.05)) + 
  geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
               position = position_nudge(-0.025)) + 
  theme_light() +
  theme(legend.position = "none") 
  
 filter3 <-  ggplot(dims_1000,aes(x=corpus,y=dimension6, fill = mode))+
  geom_flat_violin(scale = "count", trim = FALSE) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_nudge(0.05)) + 
  geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
               position = position_nudge(-0.025)) + 
   theme_light() +
   theme(legend.position = "none") 
 
 
 dim4_half_violin_plot1  <- ggplot(dims,aes(x=corpus,y=dimension4, fill = mode))+
   geom_flat_violin(scale = "count", trim = TRUE) + 
   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                geom = "pointrange", position = position_nudge(0.05)) + 
   geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
                position = position_nudge(-0.025)) + 
   labs(title = "Range of dimension 4 factor scores", subtitle = "(with no token count threshold of texts)",  y = "Dimension 4 scores", x = "corpus") + 
   theme_light() 
 
 dim4_half_violin_plot2  <- ggplot(dims_1000,aes(x=corpus,y=dimension4, fill = mode))+
   geom_flat_violin(scale = "count", trim = TRUE) + 
   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                geom = "pointrange", position = position_nudge(0.05)) + 
   geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
                position = position_nudge(-0.025)) + 
   labs(title = "Range of dimension 4 factor scores", subtitle = "(with a 1000 token minimum threshold of texts)", y = "Dimension 4 scores", x = "corpus")+ 
   theme_light()
 
 
 dim3_half_violin_plot1  <- ggplot(dims,aes(x=corpus,y=dimension3, fill = mode))+
   geom_flat_violin(scale = "count", trim = FALSE) + 
   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                geom = "pointrange", position = position_nudge(0.05)) + 
   geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
                position = position_nudge(-0.025)) + 
   labs(title = "Range of dimension 3 factor scores", subtitle = "with no token count threshold of texts", y = "Dimension 3 scores", x = "corpus") + 
   theme_light() +
   theme(legend.position = "none") 
 
 dim3_half_violin_plot2  <- ggplot(dims_1000,aes(x=corpus,y=dimension3, fill = mode))+
   geom_flat_violin(scale = "count", trim = FALSE) + 
   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                geom = "pointrange", position = position_nudge(0.05)) + 
   geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
                position = position_nudge(-0.025)) + 
   labs(title = "Range of dimension 3 factor scores", subtitle = "with a 1000 token minimum threshold of texts", y = "Dimension 3 scores", x = "corpus")+ 
   theme_light() +
   theme(legend.position = "none") 
   
 grid.arrange(dim4_half_violin_plot1,dim4_half_violin_plot2, nrow = 1)
 
  theme_light() +
  theme(legend.position = "none") + 
  
  theme_light() +
  theme(legend.position = "none") + 




  geom_dotplot(binaxis = "y", dotsize = 0.5, stackdir = "down", binwidth = 0.1, 
               position = position_nudge(-0.025)) + 
  theme(legend.position = "none") 
  
  ylab('Score')+xlab('Group') + theme_cowplot()+
  ggtitle('Figure 2: Basic Rainclouds or Little Prince Plot')
