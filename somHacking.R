# som hacking:


ciplot <- function(x, y, se, n=1, d, ywindow) {
	ind <-  (inrange(y+n*se, ywindow) & inrange(y-n*se, ywindow))
	if (any(ind)) {
		x <- x[ind];y <- y[ind];se <- se[ind]
		segments(x, y+n*se, x, y-n*se)
		segments(x-d, y+n*se, x+d, y+n*se)
		segments(x-d, y-n*se, x+d, y-n*se)
	}
}

plotcell <- function(x, y, dat, code, n, sdbar=1, ylim, yadj,
					 shapes='.', colors="black", legend.lty=1, legend.col="black") { 
	##  yadj <- 0.1
	#text(x+1/2, y+(1-yadj/2), paste("n=", n, sep=""))
	
	if (!is.data.frame(dat)) dat <- as.data.frame(dat)
	ylen <- diff(ylim)
	## n <- nrow(dat)
	mm <- code
	l <- length(code)
	if (n > 1) {
		##      l <- ncol(dat)
		##      mm <- sapply(dat, mean)
		ss <- sapply(dat, sd)/sqrt(n)
	}
	else {
		##      mm <- dat[,1]
		##      l <- length(mm)
		##      print(mm)
		ss <- rep(0, l)
	}
	# som coords:
	#text(x+1/2, y+(1-yadj), paste("x=", x+1, " y=", y+1, sep=""))
	lines(x+(1:3)/l, rep(x=y+(1-yadj), 3), lwd=2, lty=legend.lty, col=legend.col)
	lines( x+((1:l)-1/2)/l, y+1+(ylim[1] + mm)*(1-yadj)/ylen, type='b', col="black", pch=NA_integer_)
	points(x+((1:l)-1/2)/l, y+1+(ylim[1] + mm)*(1-yadj)/ylen, pch=shapes, col=colors)
	
	if (sdbar > 0 && n > 1)
		ciplot(x+((1:l)-1/2)/l, y+1+(ylim[1] + mm)*(1-yadj)/ylen,
			   ss, n=sdbar, 1/100, ywindow=c(y, y+1-yadj))
}

somgrids <- function(xdim, ydim, color,
					 yadj=0.1, hexa, ntik, ylim, axes)
{
	if (color) color <- rainbow(ydim*xdim, start=.7, end=.1)
	else color <- rep("gray", xdim*ydim)
	for (i in 0:(ydim-1)) {
		if (hexa) d <- (i %% 2)/2
		else d <- 0
		lines(c(0+d,xdim+d), c(i,i))
		for (j in 0:xdim) {
			segments(j+d, i, j+d, i+1)
			if (j == xdim) break
			#rect(j+d, i+1-yadj, j+1+d, i+1, col=color[j*ydim+i+1])
		}
		lines(c(0+d,xdim+d), c(i+1,i+1))
		if(axes)
		{
			if (i %% 2 == 1) axis(2, seq(i, i+1-yadj, length=ntik), seq(ylim[1], ylim[2], length=ntik))
			else axis(4, seq(i, i+1-yadj, length=ntik), seq(ylim[1], ylim[2], length=ntik))
		}
	}
}

plot.som <- function(x, sdbar=1, ylim=c(-3, 3), color=TRUE, ntik=3, yadj=0.1,
					 xlab="", ylab="", axes=T, shapes='.', colors="black", 
					 legend.lty=1, legend.col="black",...) {
	if (class(x) != "som" ) stop("The funciton must apply to a som object.\n")
	hexa <- (x$topol == "hexa")
	if (hexa) d <- 1/2
	else d <- 0
	xdim <- x$xdim; ydim <- x$ydim
	plot(c(0,xdim+d), c(0,ydim), xlim=c(0,xdim+d), ylim=c(0, ydim),
		 type="n", xlab=xlab, ylab=ylab, axes=FALSE, ...)
	if(axes)
		axis(1, 0:xdim, 0:xdim)
	
	somgrids(xdim, ydim, color=color, yadj=yadj, hexa=hexa, ylim=ylim, ntik=ntik, axes=axes)
	
	for (i in 0:(ydim-1))  ## horizontal loop from left to right
	{
		if (hexa) d <- (i %% 2)/2
		else d <- 0
		for (j in 0:(xdim-1)) ## vertical loop from down up
		{
			ind <- x$visual$x==j & x$visual$y==i
			n <- length(ind[ind])
			cat(j*xdim+i+1)
			
			plotcell(j+d, i,
					 x$data[ind, ], x$code[i*xdim + j+1,], n,
					 sdbar=sdbar, ylim=ylim, yadj=yadj, shapes=shapes, colors=colors,
					 legend.lty=legend.lty[j*xdim+i+1], legend.col=legend.col[j*xdim+i+1])
		}
	}
}
