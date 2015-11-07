p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_violin()
