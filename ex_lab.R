#Graphical Data Analysis with R
#Cap 7
library("gridExtra")
library("ggplot2")
library("vcd")
library("vcdExtra")
library("TSP")
library("extracat")
require(lattice) 

##1
#a
library("vcdExtra")
cancer <- as.data.frame(Cancer)

hist1 <- ggplot(cancer, aes(Grade, Freq, fill=Grade, width=Freq*0.01)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

hist2 <- ggplot(cancer, aes(Center, Freq, fill=Center, width=Freq*0.01)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

grid.arrange(hist1, hist2)

#b
doubledecker(Survival ~ Center + Grade, data = Cancer,
             gp = gpar(fill = c("#0caa00","#c70b00")))

doubledecker(Survival ~ Grade + Center, data = Cancer,
             gp = gpar(fill = c("#0caa00","#c70b00")))

#The order does not matter because they are grouped in the same way and the results are the same

##2
#This mosaic represents the rate of survival based on the age of the crew (child, adult), 
#as we can see half of the childs died in accident and half survied,
#also, a lot more adults died 
titanic <- as.data.frame(Titanic)
mosaicplot(xtabs(Freq ~  Age + Survived, data=titanic),main="", color = "#0c9cac", border = "black")


##3
#As we can see, no white person who had a black victim received death penalty 
#Also, a few black people who aggressed another black person received a death penalty
#Since there are more white people, of course the largest block represents white vs white, but surprisingly,
#they received no death sentence 
library("asbio")
library("catdata")
data(death.penalty, package="asbio")
data(deathpenalty, package="catdata")

mosaicplot(xtabs(count ~  d.race + v.race + verdict, data=death.penalty),main="", color = "#0c9cac", border = "black")

##4
#UCBAdmissions
uniAdmission <- as.data.frame.table(UCBAdmissions)
#Admission depending on dept and gender
doubledecker(Admit ~ Dept + Gender, data = UCBAdmissions, margins = c(0,6, 3, 0), keep_aspect_ratio = TRUE, gp = gpar(fill = c("#0caa00","#c70b00")))


##5
#package not working
library(fastR)
data(airlineArrival, package="fastR")


##6
#a)I would go for 4 different doubledeckers representing knowledge based on different sources
#
library("vcdExtra")
dyke <- as.data.frame.table(Dyke)

par(mfrow=c(1,4))
doubledecker(Knowledge ~  Reading, data = Dyke, margins = c(0,6, 3, 0), keep_aspect_ratio = TRUE, gp = gpar(fill = c("#0caa00","#c70b00")))
doubledecker(Knowledge ~  Radio, data = Dyke, margins = c(0,6, 3, 0), keep_aspect_ratio = TRUE, gp = gpar(fill = c("#0caa00","#c70b00")))
doubledecker(Knowledge ~  Lectures, data = Dyke, margins = c(0,6, 3, 0), keep_aspect_ratio = TRUE, gp = gpar(fill = c("#0caa00","#c70b00")))
doubledecker(Knowledge ~  Newspaper, data = Dyke, margins = c(0,6, 3, 0), keep_aspect_ratio = TRUE, gp = gpar(fill = c("#0caa00","#c70b00")))

#b)I would go for a rmb plot (not working)
library("purrr")
library(dplyr)


rmb(formula = ~Knowledge+Readingt, data = dyke,
    col.vars = c(FALSE,TRUE,TRUE,FALSE),
    label.opt = list(abbrev = 3, yaxis=FALSE))


##7
#The first conclusion is that more people have a moderate attitude towards corporal punishment
#the second conclusion is that people with higher education tend to have a negative attitude towards this
#also the attitude changes with the age: older people tend to have a more positive attitude towards corporal punishment
mosaicplot(xtabs(Freq ~ attitude + education, data=Punishment),main="", color = "#0c9cac", border = "black")
mosaicplot(xtabs(Freq ~ attitude + age, data=Punishment),main="", color = "#0c9cac", border = "black")

##8
#a)
#First conclusion: most people have brown hair and brown eyes (most common combination)
#There are a lot of people with blond hair and blue eyes
#The least common combinations is Black hair and green eyes and blonde hair and brown eyes

hairEyeColor <- as.data.frame.table(HairEyeColor)

mosaicplot(xtabs(Freq ~ Hair + Eye, data=HairEyeColor),main="", color = "#0c9cac", border = "black")


fluctile <- function(tab, dir = "b", just = "c", hsplit = FALSE,shape ="r", gap.prop = 0.1,border = NULL, label = TRUE, lab.opt = list(), add = FALSE, maxv = NULL, tile.col = hsv(0.1,0.1,0.1,alpha=0.6), bg.col = ifelse(add,NA,"lightgrey"), tile.border = NA, vp = NULL,  ...  ){
  #tab <- t(tab)
  dm <- dim(tab)
  if(is.null(maxv)){
    maxv <- max(tab)
  }
  
  nd <- length(dm)
  
  # expand gap.prop
  tmp <- rep(tail(gap.prop,1),nd)
  tmp[seq_along(gap.prop)] <- gap.prop
  gap.prop <- tmp
  
  #if(add){
  #	bg.col = NA
  #}
  if( dir %in% c("vh","hv","b","both")){
    dir <- "b"
  }
  if( dir %in% c("h","horizontal")){
    dir <- "h"
  }
  if( dir %in% c("v","vertical")){
    dir <- "v"
  }
  if(nchar(just)[1] == 2){
    just <- c(substr(just[1],1,1),substr(just[1],2,2))
  }
  just <- sapply(just, function(j){
    switch(j, t="top", b = "bottom", c = "center", r = "right", l = "left", NULL)
  })
  if("abbrev" %in% names(lab.opt)){
    abbrev <- rep(lab.opt$abbrev,nd)[1:nd]
  }else{
    abbrev <- rep(40,nd)
  }
  if("lab.cex" %in% names(lab.opt)){
    lab.cex <- lab.opt$lab.cex
  }else{
    lab.cex <- 1.2
  }
  if("rot" %in% names(lab.opt)){
    rot <- lab.opt$rot
  }else{
    rot <- 65
  }
  if("lwd" %in% names(lab.opt)){
    tile.lwd <- lab.opt$lwd
  }else{
    tile.lwd <- 1
  }
  data <- as.data.frame(as.table(tab))
  
  
  
  # logical vector for the split directions
  if(length(hsplit) == 1){
    hsp <- rep( c(hsplit,!hsplit),ncol(data) )[1:nd] 
    hsplit <- hsp
  }else{
    hsp <- hsplit
  }
  for(i in which(!hsp) ){
    data[,i] <- factor(data[,i], levels = rev(levels(data[,i])))	
  }
  tab <- xtabs(Freq~., data = data)
  
  # prepare for border and labs
  
  
  label <- rep(label,nd)[1:nd]
  rot <- rep(rot,2)[1:2]
  lab.cex <- rep(lab.cex,2)[1:2]
  
  
  abbrev <- abbrev * as.integer(label)
  
  
  
  nx <- max(sum(hsp*label),1) # min 2
  ny <- max(sum( (!hsp)*label),1)
  
  
  if(is.null(border)){
    border <- 0.1
  }
  if(length(border) == 1){
    border <- 	border * c( nx/(nx+1),1/(nx+1),1/(ny+1), ny/(ny+1) )
  }
  if(length(border) == 2){
    border <- 	c( border[1] * c( nx/(nx+1),1/(nx+1)) , border[2] * c( ny/(ny+1),1/(ny+1)) )
  }
  
  
  
  wph <- dm[2]/dm[1]
  #dev.new( width = 1000*(1-gap.prop)*wph + gap.prop*1000, height = 1000 )
  
  
  if(is.null(vp)){
    if(!add){
      grid.newpage()
    }
    vp <- viewport()
  }
  
  
  #if(!is.null(vp)){
  if(!inherits(vp,"viewport")){
    #stopifnot(length(vp)>1)
    if(length(vp) < 2){
      stop("Wrong viewport specification.")
    }
    if(is.null(current.viewport()$layout)){
      print("No layout specified. Try:")
      print("mat.layout <- grid.layout(nrow, ncol); grid.newpage(); vp.mat <- viewport(layout = mat.layout); pushViewport(vp.mat)")
    }
    
    vp <- viewport(layout.pos.row = vp[1], layout.pos.col = vp[2])
    
  }
  pushViewport(vp)
  #}else{
  
  #}
  
  vp0 = viewport(x = border[1] + (1-border[1]-border[2])/2 , y = border[3] + (1-border[3]-border[4])/2, width = 1-border[1]-border[2], height = 1-border[3]-border[4],name="base")
  if(length(dm)==2){
    
    #color workaround:
    #if(shape %in% c("r","c")){
    nn <- length(tab)
    tile.col <- rep(tile.col, ceiling(nn/length(tile.col)))[1:nn]
    tile.border <- rep(tile.border, ceiling(nn/length(tile.border)))[1:nn]
    dim(tile.col) <- dim(tile.border) <- dim(tab)
    
    tile.col = tile.col[nrow(tile.col):1,]
    tile.border = tile.border[nrow(tile.border):1,]
    #}
    
    
    pushViewport(vp0)
    if(!add){
      #grid.newpage()
      grid.rect(gp = gpar(fill=rgb(0,0,0,alpha=0.05),col=NA))
    }
    # handle the last 2 dimensions
    if( hsp[1] ){
      if( hsp[2] ){
        dim(tab) <- c(1, prod(dim(tab)))	
      }else{
        tab <- as.table(t(tab))	
        dm <- dim(tab)
      }
    }else{
      if( !hsp[2] ){
        dim(tab) <- c( prod(dim(tab)), 1)	
      }
    }
    gridfluc(tab,dir,just,shape,gap.prop, maxv=maxv, bg = bg.col, col = tile.col,border = tile.border, lwd = tile.lwd)
    popViewport()
    
    # add labels
    if(any(label)){
      
      rn <- abbreviate(dimnames(tab)[[1]],abbrev[1])
      cn <- abbreviate(dimnames(tab)[[2]],abbrev[2])
      
      m <- dm[2]
      xc <- seq(0.5,m-0.5) / m
      xc <- xc - gap.prop[2]/(m-1)/2
      xc <- xc/(1 - gap.prop[2]/(m-1))
      
      
      n <- dm[1]
      yc <- seq(0.5,n-0.5) / n
      yc <- yc - gap.prop[1]/(n-1)/2
      yc <- yc/(1 - gap.prop[1]/(n-1))
      
      
      vpR <- viewport(x = border[1]/2, y = border[3] + (1-border[3] -border[4])/2, width = border[1], height = 1-border[3]-border[4],name="rowlabs")
      pushViewport(vpR)
      y0<-0.5
      yjust <- "centre"
      x0<-0.5
      xjust <- "centre"
      
      if(rot[1] == 90){
        x0 <- 0.7
      }
      if(rot[1] == 0){
        x0 <- 0.9
        xjust <- "right"
      }
      if(rot[2] == 0){
        y0 <- 0.1
        yjust <- "left"
      }
      if(rot[2] == 90){
        y0 <- 0.3
      }
      
      
      
      grid.text(rn, x = x0, y = yc, gp = gpar(cex=lab.cex[1]), rot = rot[1], just <- xjust)
      popViewport()
      
      vpC <- viewport(x = border[1] + (1-border[1]-border[2])/2, y = 1-border[4]/2, width = 1-border[1]-border[2], height = border[4],name="collabs")
      pushViewport(vpC)
      grid.text(cn, x = xc, y = y0,rot = 90-rot[2], gp = gpar(cex=lab.cex[2]),just = yjust)
      
      popViewport()
    }
    
    #if(!add | !is.null(vp)){
    #	try( upViewport(), silent = TRUE )
    #}
    
    #if(!is.null(vp)){
    #	try( upViewport(), silent = TRUE )
    #}
    upViewport()
    
    return(vp0)
  }else{
    e1 <- new.env()
    ltrs <- expand.grid(letters,letters,letters)
    
    e1$vpn <- paste(ltrs[,1],ltrs[,2],ltrs[,3],sep="")
    e1$k <- 0
    hsplit <- !hsplit
    ss <- fluctree(dm,parent=vp0, hsplit=hsp, gap.prop=gap.prop, env=e1)
    #grid.newpage()
    pushViewport(ss)
    seekViewport("base")
    
    e1$k <- 0	
    
    #flucplot creates a viewport tree in the e1 environment
    flucplot(tab = tab, gap.prop = gap.prop, hsplit = hsp, env=e1)
    
    # use the tree in e1 to plot the 2-dimensional fluctuation diagrams
    mapply(function(x,y,hs,gp) {
      #pushViewport(ss)
      seekViewport(y)
      
      if(hsp[length(hsp)-1] & hsp[length(hsp)]) dim(x) <- c(1, prod(dim(x)))
      if(!hsp[length(hsp)-1] & !hsp[length(hsp)]) dim(x) <- c( prod(dim(x)), 1)
      if(hsp[length(hsp)-1] & !hsp[length(hsp)]) x <- as.table(t(x))
      
      #TODO: do something similar for the labels
      
      gridfluc(x,dir,just,shape,gp, maxv=maxv, bg = bg.col, col = tile.col, border = tile.border, lwd = tile.lwd)
    }, x = e1$tablist, y = e1$namlist, hs = e1$hslist, gp = e1$gplist)	
    
    upViewport()
    # go back to surface
    for(i in 1:(nd-2)){
      upViewport()
    }
    
    #######################################################################################################	
    # -------------------------------------------- LABELING  -------------------------------------------- #
    #
    #labs <- lapply(data[,-(nd+1)],function(s) abbreviate(levels(as.factor(s)),abbrev))
    labs <- mapply( function(y,z) abbreviate(levels(y),z)  ,y = data[,-(nd+1)], z = as.list(abbrev), SIMPLIFY = FALSE)
    
    ind <- label & (!hsp)
    if(any(ind)){
      
      
      vp1 <- viewport(x = border[1]/2, y = border[3] + (1-border[3]-border[4])/2, width = border[1], height = 1-border[3]-border[4],name="ylab")
      pushViewport(vp1)
      #grid.rect(0.5,0.5,1,1,gp=gpar(fill=rgb(0,0,0,alpha=0.1)))
      
      
      # create labels for the y-axis
      
      
      ind <- which(ind)
      rpt <- c(1,cumprod( dim(tab)[ind] ))
      
      nlvl <- dim(tab)[ind]
      gaps <- 0
      blocksize <- 1
      
      row.gap.prop <- gap.prop[!hsp]
      
      for( i in 1:ny ){
        # viewport for the label column
        currvp <- viewport( x = 1/ny/2+ (i-1)/ny, y = 0.5, width = 1/ny, height = 1)
        pushViewport(currvp)
        
        rlabs <- rep(labs[[ ind[i] ]], rpt[i])
        
        nl <- length(rlabs)
        
        
        # the gaps for the new dimension
        newgaps <- c(0:(nlvl[i]-1)) * blocksize * row.gap.prop[i] / (nlvl[i]-1)
        newgaps <- rep(newgaps, rpt[i])
        
        gaps <- rep(gaps, each = nlvl[i])
        
        x <- 0.5
        if( i == 1 ){
          y <- seq(1/nl/2,1-1/nl/2,1/nl)*prod(1-row.gap.prop[1:i]) + newgaps  #seq*((1-gap.prop)^i)
        }else{
          # the old coordinates +- the new gaps and cellwidths
          y <- rep(y,each = nlvl[i]) + newgaps - max(newgaps)/2 + (1-row.gap.prop[i])*blocksize * rep(seq(1/nlvl[i]/2,1-1/nlvl[i]/2,1/nlvl[i])-0.5, rpt[i])
        }
        
        grid.text(rlabs, x = x, y = y , rot = rot[1], gp = gpar(cex=lab.cex[1]))
        #grid.points( x = rep(x,length(y)), y = y , gp = gpar(col="red"))
        popViewport()
        
        blocksize <- blocksize * (1-row.gap.prop[i]) / nlvl[i]
      }
      popViewport()
      
      
      #grid.rect(0.5,0.5,1,1,gp=gpar(fill=rgb(0,0,0,alpha=0.1)))
    } # any ind
    
    ind <- label & hsp
    if(any(ind)){
      
      vp2 <- viewport(x = border[1] + (1-border[1]-border[2])/2, y = 1 - border[4]/2, width = 1-border[1]-border[2], height = border[4],name="xlab")
      pushViewport(vp2)
      
      ind <- which(ind)
      
      
      rpt <- c(1,cumprod( dim(tab)[ind] ))
      
      nlvl <- dim(tab)[ind]
      gaps <- 0
      blocksize <- 1
      col.gap.prop <- gap.prop[hsp]
      
      for( i in 1:nx ){
        # viewport for the label column
        
        currvp <- viewport( x = 0.5, y = 1 - 1/nx/2 - (i-1)/nx, width = 1, height = 1/nx)
        pushViewport(currvp)
        
        #labs <- rep(levels(data[, ind[i]]), rpt[i])
        clabs <- rep(labs[[ ind[i] ]], rpt[i])
        nl <- length(clabs)
        
        
        newgaps <- c(0:(nlvl[i]-1)) * blocksize * col.gap.prop[i] / (nlvl[i]-1)
        newgaps <- rep(newgaps, rpt[i]) 
        
        #cat("labeling variable", names(data)[ind[i]], " with nlvl = ", nlvl[i], " and levels = ", labs," and rpt = ", rpt[i]) 
        
        y <- 0.5
        if( i == 1 ){
          x <- seq(1/nl/2,1-1/nl/2,1/nl)*prod(1-col.gap.prop[1:i]) + newgaps # seq*((1-gap.prop)^i)
        }else{
          x <- rep(x,each = nlvl[i]) + newgaps - max(newgaps)/2 + (1-col.gap.prop[i])*blocksize * rep(seq(1/nlvl[i]/2,1-1/nlvl[i]/2,1/nlvl[i])-0.5, rpt[i])
        }
        #cat( "lab.x = ",x)
        grid.text(clabs, x = x, y = y , rot = 90-rot[2], gp = gpar(cex=lab.cex[2]))
        #grid.points( x = x, y = rep(y,length(x)) , gp = gpar(col="red"))
        popViewport()
        
        blocksize <- blocksize * (1-col.gap.prop[i]) / nlvl[i]
      }
      popViewport()
    }# any ind
    #
    # -------------------------------------------- LABELING  -------------------------------------------- #
    #######################################################################################################	
    #try(popViewport(),silent = TRUE)
    
    #if(!add){
    #	try( upViewport(), silent = TRUE )
    #}
    
    #if(!is.null(vp)){
    #	try( upViewport(), silent = TRUE )
    #}
    upViewport()
    
    return(invisible(ss))
  }
}	

flucplot <- function(tab, gap.prop, hsplit, env, ...){
  if(! "tablist" %in% ls(env) ){
    env$tablist = list()
    env$namlist = list()
    env$hslist = list()
    env$gplist = list()
    env$k2 = 0
  }
  
  dm <- dim(tab)
  
  
  
  #print(env$k)
  #print(env$vpn[env$k])
  
  if(length(dm) == 2){
    
    
    env$k2 <- env$k2+1
    k2 <- env$k2
    env$tablist[[k2]] <- tab
    env$namlist[[k2]] <- env$vpn[env$k]
    env$hslist[[k2]] <- hsplit
    env$gplist[[k2]] <- gap.prop
    env$k <- env$k+1
  }else{
    hsplit <- hsplit[-1]		
    gap.prop <- gap.prop[-1]
    env$k <- env$k+1
    apply(tab,1,function(z){
      flucplot(z,gap.prop, hsplit, env)
    })
  }
  #
  
  return(invisible(TRUE))
}

fluctree <- function(dims,parent, hsplit, gap.prop, env, ...){
  nv <- dims[1]
  dims <- dims[-1]
  
  
  if(hsplit[1]){
    w <- rep((1-gap.prop[1])/nv,nv)
    x <-  w/2 + 0:(nv-1)*(w + gap.prop[1]/(nv-1)) 
    h <- rep(1,nv)
    y <- rep(0.5,nv)
  }else{
    h <- rep((1-gap.prop[1])/nv,nv)
    y <- h/2 + 0:(nv-1)*(h + gap.prop[1]/(nv-1))
    w <- rep(1,nv)
    x <- rep(0.5,nv)
  }
  #	if(length(hsplit) == 1){
  #		hsplit <- !hsplit	
  #}else{
  hsplit <- hsplit[-1]	#hsplit[-length(hsplit)]#	
  #}
  gap.prop <- gap.prop[-1]
  
  if(length(dims) > 2){
    children <- vpList()
    for(i in 1:nv){
      env$k <- env$k+1
      tmp <- viewport( x[i],y[i],w[i],h[i],just="centre" , name = env$vpn[env$k])
      children[[i]] <- fluctree(dims,parent = tmp,hsplit, gap.prop, env)
      
    }
    return(vpTree(parent,children))
    
  }else{
    children <- vpList()
    for(i in 1:nv){
      env$k <- env$k+1
      children[[i]] <- viewport( x[i],y[i],w[i],h[i],just="centre" , name = env$vpn[env$k])
    }
    return(invisible(vpTree(parent,children)))
  }
  
  
}

gridfluc <- function(tab,dir = "both", just = "centre", shape = "r", gap.prop = 0.1, maxv = NULL,vp = NULL, col = NULL, bg = NULL, border = NA, lwd = 1, ...){
  
  gap.prop <- rep(gap.prop,2)
  
  if(shape != "r" ){
    just <- "centre"
    dir <- "b"
  }
  #if(just[1] == "c") just[1] <- "centre"
  
  if( is.null(bg) ){
    bg <- NA
  }
  if( is.null(col) ){
    col <- hsv(0,0,0,alpha=0.7)
  }
  
  n <- nrow(tab)
  m <- ncol(tab)
  w <- (1-gap.prop[2])/m
  h <- (1-gap.prop[1])/n
  #centered x- and y-coords:
  x <-  w/2  
  y <- h/2 
  
  if(n > 1){
    y <- y + 0:(n-1)*(h+gap.prop[1]/(n-1))	
  } 
  if(m > 1){
    x <- x + 0:(m-1)*(w+gap.prop[2]/(m-1))
  }  
  if(is.null(maxv)){
    maxv <- max(tab)	
  }
  
  
  draw2( h, w,  t(replicate(n,x)) , replicate(m,y), border = NA,bg=bg, lwd = lwd, vp=vp, just = "centre")
  
  if("right" %in% just){
    x <- x+w/2
  }
  if("left" %in% just){
    x <- x-w/2
  }
  if("top" %in% just){
    y <- y+h/2
  }
  if("bottom" %in% just){
    y <- y-h/2
  }
  if(dir == "b"){
    tab <- sqrt(tab/maxv)
    ht <- as.matrix(h*tab)
    wt <- as.matrix(w*tab)
  }
  if(dir == "v"){
    tab <- tab/maxv
    wt <- as.matrix(w*tab^0)
    ht <- as.matrix(h*tab)
  }
  if(dir == "h"){
    tab <- tab/maxv
    ht <- as.matrix(h*tab^0)
    wt <- as.matrix(w*tab)
  }
  if(dir == "n"){
    tab <- tab/maxv
    ht <- matrix(0,ncol=ncol(tab), nrow=nrow(tab))
    wt <- ht
    ht[tab > 0] <- h
    wt[tab > 0] <- w
  }
  if(shape == "r"){
    draw2(ht, wt,  t(replicate(n,x)), replicate(m,y), border = border,bg=col, lwd = lwd, vp=vp, just = just)
  }
  
  if(shape == "c"){
    draw3(R = ht/2* (min(n,m)/max(m,n)), t(replicate(n,x)), replicate(m,y), border = border,bg=col, lwd = lwd, vp=vp, just = just)
  }
  if(shape %in% c("o", "d")){
    if(shape == "o"){
      angles <- seq(22.5,360,45)/180*pi
      wt <- wt / cos(pi/8)
      ht <- ht / cos(pi/8)
    }
    if(shape == "d"){
      angles <- seq(0,360,90)/180*pi
    }
    #if(shape == "c"){
    #	angles <- seq(0,360,1)/180*pi
    #}
    corners <-  cbind( cos(angles), sin(angles))
    #mapply(function(x,y,h,w,c){
    #					   grid.polygon(x = x+corners[,1]*w/2, y = y+corners[,2]*h/2, gp = gpar(col = border, fill = c, alpha = 1))
    #		   }, x = t(replicate(n,x)), y = replicate(m,y), h = ht, w = wt, c = rep(col,length(ht))[1:length(ht)] )
    ncr <- length(corners[,1])
    colv <- rep(col,length(ht))
    
    x2 <- rep(x,each=n*ncr) + rep(corners[,1], n*m )*rep(wt,each=ncr)/2
    y2 <- rep(rep(y,m),each=ncr) + rep(corners[,2], n*m )*rep(ht,each=ncr)/2
    #grid.polygon(x2, y2 , gp = gpar(col = border, fill = rep(colv, each = ncr), alpha = 1), 
    #id = rep(1:(m*n),each=ncr))
    
    uc <- unique(colv)
    colv <- rep(colv, each = ncr)
    idv <- rep(1:(m*n),each=ncr)
    for(cc in uc){
      ii <- which(colv == cc)
      grid.polygon(x2[ii], y2[ii] , gp = gpar(col = border, fill = cc, alpha = 1,lwd = lwd), 
                   id = idv[ii])
    }
    
  }
  
  return(invisible(TRUE))	
}


draw2 <- function (H, W, X, Y, alpha = 1, border = "black", bg = "white", lwd = 1, 
                   vp = NULL, just = "centre") 
{
  if(!is.null(dim(border))){
    border[which(H*W == 0)] <- NA
  }
  if(!is.null(dim(bg))){
    bg[which(H*W == 0)] <- NA
  }
  grid.rect(x = unit(X, "npc"), y = unit(Y, "npc"), width = unit(W, 
                                                                 "npc"), height = unit(H, "npc"), just = just, default.units = "npc", 
            name = NULL, gp = gpar(col = border, fill = bg, alpha = alpha, lwd = lwd), 
            draw = TRUE, vp = vp)
}



draw3 <- function (R, X, Y, alpha = 1, border = "black", bg = "white", lwd = 1,
                   vp = NULL, just = "centre") 
{
  if(!is.null(dim(border))){
    border[which(R == 0)] <- NA
  }
  if(!is.null(dim(bg))){
    bg[which(R == 0)] <- NA
  }
  grid.circle(x = unit(X, "npc"), y = unit(Y, "npc"), r = unit(R, 
                                                               "npc"), default.units = "npc", 
              name = NULL, gp = gpar(col = border, fill = bg, alpha = alpha,lwd = lwd), 
              draw = TRUE, vp = vp)
}

addrect = function( vp ,breaks, col = "red", lwd = 2, lty = 1, gap.prop = 0, rev.y = FALSE, fill = NULL){
  yc <- breaks[[1]]
  xc <- breaks[[2]]
  
  nyc <- length(yc)
  nxc <- length(xc)
  stopifnot( nxc > 1 & nxc==nyc)
  n <- yc[nyc]
  m <- xc[nxc]
  pushViewport(vp)
  
  xc <- (xc-1)/(m-1)
  xc <- xc - gap.prop/(m-1)/2
  xc <- xc/(1 - gap.prop/(m-1))
  xc[1] <- 0
  xc[nxc] <- 1
  
  yc <- (yc-1)/(n-1)
  yc <- yc - gap.prop/(n-1)/2
  yc <- yc/(1 - gap.prop/(n-1))
  yc[1] <- 0
  yc[nyc] <- 1
  
  dyc <- diff(yc)
  dxc <- diff(xc)
  if(rev.y){
    mapply( function(x,y,w,h){
      grid.rect(x,y,w,h,gp=gpar(fill=fill,col=col,lwd=lwd, lty = lty),just=c("left","top"))
    }, x = as.list( xc[-nxc] ), y = as.list( 1-yc[-nyc] ),w = as.list(dxc),h = as.list(dyc))
    
  }else{
    mapply( function(x,y,w,h){
      grid.rect(x,y,w,h,gp=gpar(fill=fill,col=col,lwd=lwd, lty = lty),just=c("left","bottom"))
    }, x = as.list( xc[-nxc] ), y = as.list( yc[-nyc] ),w = as.list(dxc),h = as.list(dyc))
  }
  
  upViewport()
  return(invisible(TRUE))
  
}


cfluctile <- function(x, tau0 = NULL, method="Kendall", nsplit = NULL, maxsplit = NULL,  trafo = I, gap.prop = 0.2, 
                      floor = 0, rev.y = FALSE, add = FALSE, shape = "r", just = "c", dir = "b", plot = TRUE ,rect.opt = list(), border = NULL, label = TRUE, lab.opt = list(), tile.col = hsv(0.1,0.1,0.1,alpha=0.6), tile.border = NA, bg.col = "lightgrey", ...){
  
  stopifnot(inherits(x,"table") | inherits(x,"matrix") )
  stopifnot( length(dim(x)) == 2 )
  
  # get parameters for rectangles
  if( "lwd" %in% names(rect.opt) ){
    lwd <- rect.opt$lwd
  }else{
    lwd <- 2
  }
  if( "lty" %in% names(rect.opt) ){
    lty <- rect.opt$lty
  }else{
    lty <- 1
  }
  
  if( "col" %in% names(rect.opt) ){
    col <- rect.opt$col
  }else{
    col <- "red"
  }
  if( "fill" %in% names(rect.opt) ){
    fill <- rect.opt$fill
  }else{
    fill <- alpha(col,0.05)
  }
  
  
  #if(kendalls(x) < 0){
  #    x <- x[,ncol(x):1]
  #    print("Reversed column category order...")
  #}
  
  if( method %in% c("Kendall","kendall","tau",1) ){
    method <- as.integer(1)	
  }
  if( method %in% c("kappa","Cohen",2) ){
    method <- as.integer(2)	
  }
  if( method %in% c("WBCI","wbci","WBCC",3) ){
    method <- as.integer(3)	
  }
  if( method %in% c("BCI","bci","BCC",4) ){
    method <- as.integer(4)	
  }
  if( method %in% c("R","r","res","resid","residual",5) ){
    method <- as.integer(5)	
  }
  if( method %in% c("minres","mr","min.res",6) ){
    method <- as.integer(6)	
  }
  ret.int <- ( storage.mode(x) == "integer" )
  storage.mode(x) <- "numeric"
  
  
  
  n <- nrow(x)
  m <- ncol(x)
  if(is.null(maxsplit)){
    maxsplit <- min(n,m)+1
  }
  if(!is.null(nsplit)){
    stopifnot(nsplit <= maxsplit)
    tau0 <- -1
    maxsplit <- nsplit
  }
  if(maxsplit == 1){
    singlesplit <- 1
  }else{
    singlesplit <- min(n,m)+1
  }
  
  #storage.mode(x) <- "integer"
  if(is.null(tau0)){
    if(method == 1){
      tau0 <- kendalls(x)
    }
    if(method == 2){
      tau0 <- cohen(x)
    }
    if(method == 3){
      tau0 <- 1 - WBCI(x)
    }
    if(method == 4){
      tau0 <- 1 - BCI(x)
    }
    if(method == 5){
      #ix <- itab(x)
      tau0 <- 0.9 #<- 1 - exp(min( (x-ix)/sqrt(ix)  ))
    }
    if(method == 6){
      #ix <- itab(x)
      tau0 <- sum( abs(x - itab(x))/sqrt(itab(x)) )/sum(x)/2
    }
  }
  
  if( floor > 0 ){
    x2 <- apply(x,1:2, function(z){
      ifelse(z < floor, 0, z)
    })
    #storage.mode(x2) <- "integer"
    cuts <- .Call("getclust",x2,as.integer(dim(x)),tau0,method,as.integer(singlesplit))
  }else{
    cuts <- .Call("getclust",x,as.integer(dim(x)),tau0,method,as.integer(singlesplit))
  }
  
  
  r <- length(cuts)/4 #/2
  if(rev.y){
    x <- as.table(x[nrow(x):1,]	)
  }
  b1 <- c(1,cuts[1:r]+1)
  b2 <- c(1,cuts[(r+1):(r+r)]+1)
  tau.values <- cuts[(2*r+1):(3*r-1)]
  cut.ids <- cuts[(3*r+1):(4*r-1)]
  
  if( r > 1 ){
    # get cut order (tau before level!)
    nc <- length(cut.ids)
    xtci <- c(0,cut.ids,0)
    lp <- rp <- NULL #left and right parent cut
    for(i in 2:(nc+1) ){
      lp[i-1]	<- max( which( xtci[1:(i-1)] < xtci[i]))
      rp[i-1]	<- i + min( which( xtci[(i+1):(nc+2)] < xtci[i]))
    }
    parents <- cbind(lp,rp)
    # level of left and right parent
    rlp <- c(0,cut.ids,0)[lp]
    rrp <- c(0,cut.ids,0)[rp]
    lrrp <- cbind(rlp,rrp)
    # parent level
    parent <- apply(lrrp,1,which.max)
    for(i in seq_along(parent)){
      parent[i] <- parents[i,parent[i]]
    }
    #parent.tau <- c(1,tau.values)[parent]
    
    
    cut.rank <- rep(0,nc)
    cut.rank[which(cut.ids==1)] <- (k<-1)
    acc.parents <- c(TRUE,rep(FALSE,nc))
    acc.parents[which(cut.ids==1)+1] <- TRUE
    while(!all(acc.parents)){
      # candidates: parent accepted and not an acc. parent already
      (candidates <- which( acc.parents[parent] & !acc.parents[-1]))
      (best <- candidates[ which.max( tau.values[candidates])])
      acc.parents[best+1] <- TRUE
      cut.rank[best] <- (k<-k+1)
      
    }
  }else{
    # r == 1
    cut.rank <- 1
  }
  #cut.rank <- rank(tau.values+cut.ids) # old: this used level before tau
  exclude <- cut.rank > maxsplit
  
  if( any( exclude) ){
    exclude <- which(exclude)
    b1 <- b1[-(exclude+1)]
    b2 <- b2[-(exclude+1)]
    cut.rank <- cut.rank[-exclude]
    tau.values <- tau.values[-exclude]
    cut.ids <- cut.ids[-exclude]
  }
  breaks <- list(b1,b2)
  r <- length(b1)-1
  row.list = list()
  col.list = list()
  #cat("r= ",r)
  #print(b1)
  for(i in 1:r){
    row.list[[i]] <- rownames(x)[ b1[i]:(b1[i+1]-1) ]
    col.list[[i]] <- colnames(x)[ b2[i]:(b2[i+1]-1) ]
    #row.list[[i]] <-paste("'R",i,"' = list('",	paste(c(rownames(x)[ b1[i]:(b1[i+1]-1) ]),collapse="','"),"')",sep="")
    #col.list[[i]] <-paste("'C",i,"' = list('",	paste(c(colnames(x)[ b2[i]:(b2[i+1]-1) ]),collapse="','"),"')",sep="")
    
  }
  
  #list1 <- paste("list(",paste(row.list,collapse = ","),")",sep="")
  #list2 <- paste("list(",paste(col.list,collapse = ","),")",sep="")
  
  if(plot){
    class(x) <- "matrix"
    if(!add){
      bs <- fluctile(trafo(x), gap.prop=gap.prop, shape = shape, just = just, dir = dir, tile.col = tile.col, tile.border = tile.border, bg.col = bg.col, label = label, lab.opt = lab.opt, border = border)
    }else{
      bs <- fluctile(trafo(x), gap.prop=gap.prop, bg.col=rgb(0,0,0,alpha=0),tile.col = rgb(0,0,0,alpha=0),tile.border = tile.border, add = TRUE, shape = shape, just = just, dir = dir, label = label, lab.opt = lab.opt, border = border)	
    }
    addrect(bs, breaks, col, gap.prop, rev.y = !rev.y, lwd = lwd, lty = lty, fill = fill)
  }
  
  ret <- list(row.list,col.list)
  attr(ret,"orders") <- list( lapply(row.list, function(w) match(w,rownames(x))), lapply(col.list, function(w) match(w,colnames(x)))  )
  attr(ret,"tau.values") <- tau.values
  attr(ret,"level") <- cut.ids
  attr(ret,"cut.rank") <- cut.rank
  attr(ret,"tau0") <- tau0
  return(invisible(ret))
}


fluctile(HairEyeColor)
#b)It does not matter
#c)I would choose the fluctuate diagram. 
#As shown above, the sex slightly matters

##9
#

gilby <- as.data.frame(Gilby)
fluctile(Gilby)
mosaicplot(xtabs(Freq ~  Dullness + Clothing, Gilby),main="", color = "#0c9cac", border = "black")

ggplot(gilby, aes(Dullness, Freq, fill=Clothing)) +
  geom_bar(stat = "identity") +
  facet_grid(Dullness ~ Clothing) +
  theme(legend.position="none")

ggplot(gilby, aes(Dullness, Freq, fill=Clothing)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none")

hist1 <- ggplot(gilby, aes(Dullness, Freq, fill=Dullness)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

hist2 <- ggplot(gilby, aes(Clothing, Freq, fill=Clothing)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

grid.arrange(hist1, hist2)

##10
#
library("MCPAN")
library("reshape2")
data(HCD, package="MCPAN")
hcd <- as.data.frame(HCD)
formatted <- melt(hcd)

hist1 <- ggplot(formatted, aes(Level, value, fill=Level)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

hist2 <- ggplot(formatted, aes(variable, value, fill=variable)) +
  geom_bar(stat="identity") +
  ylab("Death rate")

grid.arrange(hist1, hist2)

#b)
mosaicplot(xtabs(Freq.value ~  Freq.variable + Freq.Level, data=as.data.frame.table(formatted)),main="", color = "#0c9cac", border = "black")


