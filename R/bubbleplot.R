# library(plotly)
#
# BubblePlot <- function(x)
# {
#     nms <- names(x)
#     plot_ly(x, x = get(nms[1]), y = get(nms[2]), text = paste(nms[5], ": ", get(nms[5])),
#         mode = "markers", color = get(nms[3]), size = get(nms[4]))
# }
# print.BubbleWorthy <- function(x)
# {
# 	print(BubblePlot(x))
# }
#
# set.seed(100)
# dat1 <- diamonds[sample(nrow(diamonds), 1000), c("carat","price", "depth", "table", "clarity")]
# class(dat1) <- c("BubbleWorthy", class(dat1))
# dat1
#
# dat1
#
#
#
# library(plotly)
#
# BubblePlot <- function(data)
# {
#     nms <- names(data)
#     plot_ly(data, x = get(nms[1]), y = get(nms[2]), text = paste(nms[5], ": ", get(nms[5])),
#         mode = "markers", color = get(nms[3]), size = get(nms[4]))
# }
# z1 <- diamonds[sample(nrow(diamonds), 1000), c("carat","price", "depth", "table", "clarity")]
# BubblePlot(z1)
#
#
# print.BubbleWorthy <- function(x)
# {
# 	print(BubblePlot(x))
# }
#
# set.seed(100)
# dat1 <- diamonds[sample(nrow(diamonds), 1000), c("carat","price", "depth", "table", "clarity")]
# class(dat1) <- c("BubbleWorthy", class(dat1))
# dat1
#
#
#
#
#
#
#
#
#
# install.packages("stringi")
# install.packages("plotly")
# prin
#
#
# summary(dat)
#
# print.BubbleWorthy(dat)
#
#
#
#
#
# BubblePlot(dat)
#
# print.BubbleWorthy <- function(x)
# {
#     plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
#             mode = "markers", color = dat[,3], size = dat[,4])
#
#
#
# 	plot(x)
# }
# dat
# print.BubbleWorthy(dat)
#
#
#
# plot_ly(dat, x = get("carat"), y = price, text = paste("Clarity: ", get("clarity")),
#         mode = "markers", color = get("depth"), size = get("table"))
#
#
# plot_ly(dat, x = get("carat"), y = dat[,2], text = paste("Clarity: ", dat[,3]),
#         mode = "markers", color = dat[,3], size = dat[,4])
#
#
# plot_ly(dat, x = dat[,1], y = dat[,2], text = paste("Clarity: ", dat[,3]),
#         mode = "markers", color = dat[,3], size = dat[,4])
#
# plot_ly(dat, x = substitute("carat"), y = price, text = paste("Clarity: ", clarity),
#         mode = "markers", color = depth, size = table)
#
# plot_ly(dat, x = substitute(eval("carat")), y = price, text = paste("Clarity: ", clarity),
#         mode = "markers", color = carat, size = carat)
#
# plot_ly(dat, x = substitute("carat"), y = price, text = paste("Clarity: ", clarity),
#         mode = "markers", color = depth, size = table)
#
#
#
# BubblePlot
