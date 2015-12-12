# ################################################
# ####    MOONPLOT FOR CORRESPONDENCE ANALYSIS####
# ################################################
#
# corrected.corresp <- function(x)
# {
#     library(MASS)
#     result <- corresp(x,2)
#     #result$cor <- result$cor
#     result$rscore <- sweep(result$rscore,2,result$cor,"*")
#     result$cscore <- sweep(result$cscore,2,result$cor,"*")
#     #Freq=x)
# result}
#
# space <- function(x,gap=0.05)
# {#ensures that the space between all obervations is at least gap
# n <- length(x)
# x.sort <- sort(x)
# x.order <- order(x,1:n)
# gap <- gap*diff(range(x))
# b <- x.sort[-1]-x.sort[-n]
# if(sum(b<gap)>0)
#     b[b<gap] <- gap
# b <- c(x.sort[1],b)
# new.val <- function(b,x.sort)
# {sum((x.sort-cumsum(b))^2)}
# b <- optim(b,new.val,x.sort=x.sort,lower=c(-Inf,rep(gap,n-1)),method="L-BFGS-B")$par
# replace(x,x.order,cumsum(b))}
#
#
# space.degrees <- function(x,gap=0.05)
# {#variant of space which ensures minimum angles between alternatives
# n <- length(x)
# x <- x+(1:n)/1000
# x.sort <- sort(x)
# b <- x.sort
# diffs <- c(x.sort[1]+360-x.sort[n],x.sort[-1]-x.sort[-n])
# base <- (1:n)[max(diffs)==diffs]
# if (base==1)
# {  x.lookup <- x.sort}
# else
# {
# x.lookup <- c(x.sort[base:n],x.sort[1:(base-1)]+360)}
# x.lookup <- space(x.lookup,gap)
# if (base>1)
#     x.lookup <- c(x.lookup[-1:-(n-base+1)]-360,x.lookup[1:(n-base+1)])
# if(min(x.lookup)<0)
#    x.lookup[x.lookup<0] <- x.lookup[x.lookup<0]+360
# if(max(x.lookup)>360)
#    x.lookup[x.lookup>360] <- x.lookup[x.lookup>360]-360
# result <- vector('numeric',n)
# for (i in 1:n)
#     result[i] <- x.lookup[(1:n)[x.sort==x[i]]]
# result <- result-(1:n)/1000
# result
# }
#
#
# moonplot.symmetric <- function (x, y, var.axes = TRUE, col, cex = rep(par("cex"), 2),
#     xlabs = NULL, ylabs = NULL, expand = 1, xlab.offsets=NULL,xlab.pos=1,
#     xlab.mult = 1.1,ylab.mult=1.1,offsets=c(0,0),space.gap=0,
#     arrow.len = 0.1,y.cex=1.5,y.cex.scale=0.5,circle.scaler=1.1,...)
# #y.cex.scale = values below 1 reduce font size differences
# # xlab.offsets = array of number of rows of x with two columns, showing how much the points representing the brands should be moved (for use to avoid overlapping brands)
# {
#     n <- nrow(x)
#     p <- nrow(y)
#     if (missing(xlabs)) {
#         xlabs <- dimnames(x)[[1]]
#         if (is.null(xlabs))
#             xlabs <- 1:n
#     }
#     xlabs <- as.character(xlabs)
#     dimnames(x) <- list(xlabs, dimnames(x)[[2]])
#     if (missing(ylabs)) {
#         ylabs <- dimnames(y)[[1]]
#         if (is.null(ylabs))
#             ylabs <- paste("Var", 1:p)
#     }
#     ylabs <- as.character(ylabs)
#     dimnames(y) <- list(ylabs, dimnames(y)[[2]])
#     if (length(cex) == 1)
#         cex <- c(cex, cex)
#     if (missing(col)) {
#         col <- par("col")
#         if (!is.numeric(col))
#             col <- match(col, palette())
#         col <- c(col, col + 1)
#     }
#     else if (length(col) == 1)
#         col <- c(col, col)
#     max.x <- c(max(x)*xlab.mult,min(x)*xlab.mult,max(y),min(y))
#     max.x <- max(abs(max.x))
#     xlim <- ylab.mult*max.x*c(-1,1)+offsets
#     ylim <- xlim
#     ratio <- 1#max(rangy1/rangx1, rangy2/rangx2)/expand
#     on.exit(par(oldpar))
#     oldpar <- par(pty = "s")
#     par(plt=c(0,1,0,1))
#     y.atan <- (atan(y[,2]/y[,1]))
#     y.srt <- y.atan*180/pi
#     srt.adj <- rep(0,n)
#     y.srt[y[,1]<0] <- y.srt[y[,1]<0]+180
#     y.srt[y[,2]<0] <- y.srt[y[,2]<0]+90
#     y.srt <- space.degrees(y.srt,space.gap)
#     y.moved <- array(NA,dim(y),dimnames=dimnames(y))
#     y.moved[,1] <- sin(y.srt/180*pi)*max.x
#     y.moved[,2] <- (max.x^2-y.moved[,1]^2)^.5
#     y.moved <- abs(y.moved)*sign(y)
#     z <- y.moved[,2]>0
#     y.moved[z,1:2] <- y.moved[z,2:1]
#     y.moved <- abs(y.moved)*sign(y)
#     y.srt[y[,1]<0] <- y.srt[y[,1]<0]-180
#     y.srt[y[,2]<0] <- y.srt[y[,2]<0]-90
#     y.dist <- (y[,1]^2+y[,2]^2)^.5
#     y.max.dist <- max(y.dist)
#     y.pos = rep(4,p)
#     y.pos[y.moved[,1]<0] <- 2
#     if ( sum(offsets!=0))
#        {x <- sweep(x,2,offsets,"+")
#         y <- sweep(y,2,offsets,"+")}
#     plot(x, axes=F,type = "n", xlim = xlim, ylim = ylim, col = col[1],asp=T,...)
#     if (is.null(xlab.offsets))
#        text(x, xlabs, cex = cex[1], col = col[1], ...)
#     else
#         {x.offset <- !(xlab.offsets[,1]==0 & xlab.offsets[,2]==0)
#         points(x[x.offset,],pch=19)
#         if (sum(x.offset) != length(x.offset))
#             text(x[!x.offset,], xlabs[!x.offset], cex = cex[1], col = col[1], ...)
#         text(x[x.offset,]+xlab.offsets[x.offset,], xlabs[x.offset],pos=xlab.pos, cex = cex[1], col = col[1], ...)
#         for (i in 1:n)
#         if (x.offset[i])
#             lines(c(x[i,1],x[i,1]+xlab.offsets[i,1]),c(x[i,2],x[i,2]+xlab.offsets[i,2]))
#          }
#     par(new = TRUE)
#     plot(y, axes = FALSE, type = "n", xlim = xlim * ratio, ylim = ylim *
#         ratio, xlab = "", ylab = "", col = col[1],asp=T, ...)
#     for (i in 1:p)
#          text(y.moved[i,1],y.moved[i,2], labels = ylabs[i], cex = cex[2]*y.cex*(y.dist[i]/y.max.dist)^y.cex.scale, col = col[2],pos=y.pos[i],offset=0,srt=y.srt[i],...)
#     symbols(offsets[1],offsets[2],sqrt(min(apply((y.moved^2),1,sum)))*.98,inches=F,add=TRUE)
# #    symbols(offsets[1],offsets[2],max.x/ylab.mult*circle.scaler,inches=F,add=TRUE)
#     invisible()
# }
#
# moonplot <- function (x, brands.row=T, type = c("symmetric", "rows", "columns"),
#    trad.ca=F,xlab.offsets=NULL,xlab.pos=1,trad.ca.xlim=NULL,...)
# {
#     if (!brands.row)
#      x <- t(x)
#     n <- nrow(x)
#     obj <- corresp(x,2)
#     type <- match.arg(type)
#     X <- obj$rscore[, 1:2]
#     if (type != "columns")
#         X <- X %*% diag(obj$cor[1:2])
#     colnames(X) <- rep("", 2)
#     Y <- obj$cscore[, 1:2]
#     if (type != "rows")
#         Y <- Y %*% diag(obj$cor[1:2])
#     colnames(Y) <- rep("", 2)
#     #  Checking offets for x labels
#     if (!is.null(xlab.offsets))
#     {   if(sum(abs(xlab.offsets))==0)
#           {warning("xlab.offsets are all 0")
#           xlab.offsets=NULL}
#         if(sum(dim(xlab.offsets)==c(n,2))!=2)
#            stop("xlab.offsets must be an array with two columns and the same number of rows as x")}
#     #   Traditional correspondence analysis
#     if(trad.ca)
#     {   old.par <- par(no.readonly = TRUE) # all par settings which
#                                            # could be changed.
#         on.exit(par(old.par))
#         par("plt"=c(.01,.99,.01,.99))
#         plot(X,type="n",asp=1,axes=F,xlim=trad.ca.xlim,...)
#         text(Y,dimnames(Y)[[1]],font=3)
#         points(0,0,pch=3,cex=3)
#         box()
#         xlabs <- dimnames(X)[[1]]
#     if (is.null(xlab.offsets))
#         text(X,xlabs)
#     else
#         {
#         x.offset <- !(xlab.offsets[,1]==0 & xlab.offsets[,2]==0)
#         points(X[x.offset,],pch=19)
#         text(X[!x.offset,], xlabs[!x.offset], ...)
#         text(X[x.offset,]+xlab.offsets[x.offset,], xlabs[x.offset],pos=xlab.pos, ...)
#         for (i in 1:n)
#         if (x.offset[i])
#             lines(c(X[i,1],X[i,1]+xlab.offsets[i,1]),c(X[i,2],X[i,2]+xlab.offsets[i,2]))
#         }
#     }
#     else
#     {#offsets
#     switch(type, symmetric = moonplot.symmetric(X, Y, var.axes = FALSE,xlab.offsets=xlab.offsets,xlab.pos=xlab.pos,...),
#          rows = biplot.bdr(X, Y, ...), columns = biplot.bdr(Y, X, ...))
#     points(0, 0, pch = 3, cex = 3)
# }
#     invisible()
# }
#
# ## Moonplot
# library(MASS)
#
# CSDperceptions <- matrix(c(0.3004, 0.6864, 0.4975, 0.2908, 0.2781, 0.2642, 0.1916, 0.284,  0.3514, 0.2534, 0.2089,
# c(  0.0198, 0.4604, 0.2151, 0.5235, 0.1151, 0.12,   0.5457, 0.3041, 0.06312,    0.384,  0.06064),
# c(  0.01114,    0.4111, 0.1904, 0.4494, 0.06931,    0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
# c(  0.01114,    0.2373, 0.089,  0.2707, 0.05322,    0.06436,    0.2756, 0.1656, 0.02967,    0.1916, 0.02228),
# c(  0.0198, 0.177,  0.07054,    0.0297, 0.0396, 0.02719,    0.0136, 0.02847,    0.0198, 0.02847,    0.02472),
# c(  0.4543, 0.1275, 0.07673,    0.02847,    0.07293,    0.1077, 0.01609,    0.05198,    0.321,  0.01856,    0.0297),
# c(  0.06807,    0.1089, 0.06064,    0.0198, 0.1174, 0.04084,    0.01609,    0.01733,    0.03465,    0.01361,    0.03589),
# c(  0.08168,    0.224,  0.1015, 0.04579,    0.04815,    0.04084,    0.03094,    0.05562,    0.05322,    0.04084,    0.02847)),nrow=8,byrow=TRUE,
# dimnames=list(Brand=c('Coke','V',"Red\nBull","Lift\nPlus",'Diet.Coke','Fanta','Lift','Pepsi'),
# Attribute=c('Kids', 'Teens',    "Enjoy life",   'Picks you up', 'Refreshes',    'Cheers you up',    'Energy',   'Up-to-date',   'Fun',  'When tired',   'Relax')))
#
# CSDdata = round(100*CSDperceptions)
#
# # traditional plot
# moonplot(CSDdata,trad.ca = TRUE)
#
# #moonplot
# moonplot(CSDdata,xlab.mult=1.2,y.cex.scale=0.5,space.gap=.012,xlab.pos=1,col=1)
#
# #moonplot with overlapping points re-arranged
# moonplot(round(100*CSDperceptions),xlab.mult=1.2,y.cex.scale=0.5,space.gap=.012,xlab.offsets=matrix(c(0,0,-.2,-.2,0,-.38,.25,-.2,rep(0,8)),byrow=T,ncol=2),xlab.pos=1,col=1)
