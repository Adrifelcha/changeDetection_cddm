#source("./functions/auxiliary/color_palettes.R")
#source("./functions/auxiliary/transformations.R")

data_perCondition <- function(data, condition, sub=NA){
  # Just in case the condition is specified by name
  if(condition=="stationary"){ condition <- 1 }else{
  if(condition=="abrupt"){     condition <- 2 }else{
  if(condition=="gradual"){    condition <- 3 }else{
  if(condition=="mixed"){      condition <- 4 }}}}
  # Isolate data pertaining this condition (and participant)
  if(sum(is.na(sub))>0){   sub <- 1:24    }
  P <- length(sub)
  # Set-up plotting space
  grid <- grid_size(P)
  par(pty="s",                  # Square canvas
      mfrow=c(grid[1],grid[2]), # Grid size
      mar = c(0, 0, 0, 0),      # inner margins
      oma=c(1,1,3.5,1))           # outer margins
  # Define points to draw circumference
  radius <- 10
  pm <- radius + 0.2
  all.Angles <- seq(0,2*pi,0.001) 
  circle <- polarToRect(all.Angles,radius)
  if(condition==1){
    color_choices <- color_all_conditions(0.15)[[condition]]
    color_mean <- color_all_conditions(1)[[condition]]
  }else{if(condition==2){
    color_choices <- color_all_conditions(0.3)[[condition]]
    color_mean <- color_all_conditions(1)[[condition]]
  }else{if(condition==3){
    color_choices <- color_all_conditions(0.2)[[condition]]
    color_mean <- rgb(0,0,0,0.15)
  }else{if(condition==4){
    color_choices <- color_all_conditions(0.35)[[condition]]
    color_mean <- rgb(0,0,0,0.5)
  }}}}
  # Make a plot for every subject
  for(i in sub){
      # Get the subject-specific means for the Normal distributions
      resp <- data[,"resp",condition,i]  
      means <- unique(data[,"mean",condition,i])
      # Draw the base circle
      plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE, xlim=c(-pm,pm),ylim=c(-pm,pm))      
      # Find relevant points to plot
      if(condition==1){
          points(circle[,1],circle[,2], type="l", col="gray80")
          draw_resp <- polarToRect(resp,radius)  # Observed responses
          points(draw_resp[,1], draw_resp[,2], col=color_choices[i], pch=16, cex=1.5)
          draw_mean <- polarToRect(means,radius+5)
          lines(c(draw_mean[1],0), c(draw_mean[2],0),col = "black", lty=2, lwd=1)
          points(0,0,cex=2, col="white", pch=16)
          points(0,0,cex=1.5, col=color_mean[i])
          if(draw_mean[2]>0){ text(0,-3,paste("P-", i,sep=""), col="gray40") 
          }else{ text(0,3,paste("P-", i,sep=""), col="gray40") }
      }else{if(condition==2){
          M <- length(means)
          r <- seq(3,radius,length.out=M)
          for(j in 1:M){
              subOrbit <- polarToRect(all.Angles,r[j])
              points(subOrbit[,1],subOrbit[,2], type="l", col="gray80")
              k <- which(data[,"mean",condition,i] == means[j])
              draw_resp <- polarToRect(resp[k],r[j])  # Observed responses
              points(draw_resp[,1], draw_resp[,2], col=color_choices[j], pch=16, cex=1)
              draw_mean <- polarToRect(means,radius)
              draw_mean2 <- polarToRect(means,r[1])
              lines(c(draw_mean[j,1],draw_mean2[j,1]), c(draw_mean[j,2],draw_mean2[j,2]),
                    col = color_mean[j]) 
          }
          text(0,0,paste("P-", i,sep=""),cex=0.8)
      }else{if(condition==3){
          big_circle <- polarToRect(all.Angles,radius-0.5)
          points(big_circle[,1],big_circle[,2], type="l", col="gray95", lwd=10)
          draw_resp <- polarToRect(resp,radius)  # Observed responses
          points(draw_resp[,1], draw_resp[,2], col=color_choices[i], pch=16, cex=1)
          draw_mean <- polarToRect(means,radius-1)
          points(draw_mean[,1], draw_mean[,2], pch=3, cex=0.5, col=color_mean)
          mtext("Gradual condition",outer=TRUE, line = 1, cex=1.5)
          text(0,0,paste("P-", i,sep=""), col="gray70", cex=1.5)
      }else{if(condition==4){
          change_at <- which(data[,"change",condition,i]==1)
          same_until <- c(change_at-1,300)
          C <- length(same_until)
          r <- seq(3,radius,length.out=C)
          if(C==3){r <- seq(6,radius,length.out=C)}
          start_at <- 1
          for(j in 1:C){
              subOrbit <- polarToRect(all.Angles,r[j])
              points(subOrbit[,1],subOrbit[,2], type="l", col="gray80")
              k <- start_at:same_until[j]
              draw_resp <- polarToRect(resp[k],r[j])  # Observed responses
              points(draw_resp[,1], draw_resp[,2], col=color_choices[j], pch=16, cex=1)
              draw_mean <- polarToRect(means[k],r[j])
              points(draw_mean[,1], draw_mean[,2], pch=3, cex=0.5, col=color_mean)
              start_at <- same_until[j]+1
          }
          text(0,0,paste("P-", i,sep=""))  
      }else{return(paste("Please specify a valid condition 1-4"))}}}}
  }
  if(condition==1){
    mtext("Stationary condition",outer=TRUE, line = 1, cex=1.5)
  }else{if(condition==2){
    mtext("Abrupt condition",outer=TRUE, line = 1, cex=1.5)
  }else{if(condition==3){
    mtext("Gradual condition",outer=TRUE, line = 1, cex=1.5)
  }else{if(condition==4){
    mtext("Mixed condition",outer=TRUE, line = 1, cex=1.5)
  }}}}
  
}

if(!exists("test")){  test <- TRUE     }
if(test){
    for(i in 1:4){
      pdf(file = paste("../figs/data_condition-",i,".pdf",sep=""),   # The directory you want to save the file in
          width = 4, # The width of the plot in inches
          height = 4) # The height of the plot in inches
      data_perCondition(ag,i) 
      dev.off()
    }
}