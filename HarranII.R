
# This script is a digical copying of the Frank Stella piece "Harran II" 1967

harranII = function(x = 5, y = 5, frac = 2/3, width = 500, height = 500, circle_size = 1, circle_num = 8){

  require(rbokeh)
  require(RColorBrewer)
  
  source("shapes.r")
  
  circles <- function(figure, x, y, size, n = 6, 
                      colors = rep("Set3",6), line_color = "beige", 
                      wedge_col = NULL, wedge_ang = rep(pi/2,6), 
                      wedge_off = rep(0,6), wedge_sz = rep(1.1,6)){
    
    # Colors use RColorBrewer package, see display.brewer.all() for options
    require(rbokeh)
    
    p <- ly_annulus(figure, rep(x[1],n[1]), rep(y[1],n[1]), 
                    inner_radius = 0,
                    outer_radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
                    fill_color = brewer.pal(n,colors[1]),
                    line_color = line_color) 
    if (!is.null(wedge_col)){
      p <- ly_wedge(p, rep(x[1],n[1]), rep(y[1],n[1]), 
                    radius = seq(size[1]*wedge_sz[1],size[1]*wedge_sz[1]/n[1],-size[1]*wedge_sz[1]/n[1]),
                    start_angle = pi/4 - wedge_ang[1]/2 + wedge_off[1],
                    end_angle = pi/4 + wedge_ang[1]/2 + wedge_off[1],
                    direction = "anticlock",
                    fill_color = brewer.pal(n,wedge_col[1]),
                    line_color = "beige",
                    alpha = 5) %>%
        ly_wedge(rep(x[1],n[1]), rep(y[1],n[1]), 
                 radius = seq(size[1]*wedge_sz[1],size[1]*wedge_sz[1]/n[1],-size[1]*wedge_sz[1]/n[1]),
                 start_angle = 5*pi/4 - wedge_ang[1]/2 + wedge_off[1],
                 end_angle = 5*pi/4 + wedge_ang[1]/2 + wedge_off[1],
                 direction = "anticlock",
                 fill_color = brewer.pal(n,wedge_col[1]),
                 line_color = "beige",
                 alpha = 5)
    }
    
    if (length(x) > 1){
      for (i in 2:length(x)){
        p <- ly_annulus(p, rep(x[i],n[i]), rep(y[i],n[i]), 
                        inner_radius = 0,
                        outer_radius = seq(size[i],size[i]/n[i],-size[i]/n[i]),
                        fill_color = brewer.pal(n,colors[i]),
                        line_color = line_color)
        if (!is.null(wedge_col)){
          p <- ly_wedge(p, rep(x[i],n[i]), rep(y[i],n[i]), 
                        radius = seq(size[i]*wedge_sz[i],size[i]*wedge_sz[i]/n[i],-size[i]*wedge_sz[i]/n[i]),
                        start_angle = pi/4 - wedge_ang[i]/2 + wedge_off[i],
                        end_angle = pi/4 + wedge_ang[i]/2 + wedge_off[i],
                        direction = "anticlock",
                        fill_color = brewer.pal(n,wedge_col[i]),
                        line_color = "beige",
                        alpha = 5) %>%
            ly_wedge(rep(x[i],n[i]), rep(y[i],n[i]), 
                     radius = seq(size[i]*wedge_sz[i],size[i]*wedge_sz[i]/n[i],-size[i]*wedge_sz[i]/n[i]),
                     start_angle = 5*pi/4 - wedge_ang[i]/2 + wedge_off[i],
                     end_angle = 5*pi/4 + wedge_ang[i]/2 + wedge_off[i],
                     direction = "anticlock",
                     fill_color = brewer.pal(n,wedge_col[i]),
                     line_color = "beige",
                     alpha = 5)
        }
      }
    }
    
    return(p)
    
  }
  
  wedge <- function(figure, x, y, size, n=6, 
                     ang = pi/2, offset = 0,
                     colors = rep("Set3",6), line_color = "beige"){
    p <- ly_wedge(figure, rep(x[1],n[1]), rep(y[1],n[1]), 
                  radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
                  start_angle = pi/4 - ang/2 + offset,
                  end_angle = pi/4 + ang/2 + offset,
                  direction = "anticlock",
                  fill_color = brewer.pal(n,colors[1]),
                  line_color = "beige",
                  alpha = 5) 
    
    return(p)
    
}
  
  square <- function(figure, x, y, size = 0.5, thickness = 0.0833, quadrant = 1,
                     fill_color = "#66C2A5", line_color = "beige"){
    
    # This makes squares. the topright quadrant is 1 and it moves CCW
    
    t = thickness
    if(quadrant == 1) ang = 0
    if(quadrant == 2) ang = pi/2
    if(quadrant == 3) ang = pi
    if(quadrant == 4) ang = 3*pi/2
    
    top = c(cos(ang+pi/2),cos(ang))[which.max(abs(c(cos(ang+pi/2),cos(ang))))]
    bot = c(cos(ang), cos(ang-pi/2))[which.max(abs(c(cos(ang), cos(ang-pi/2))))]
    
    p <- ly_patch(figure, x = x + c(0, #1
                                    size * cos(ang+pi/2),
                                    size * top, #3
                                    size * cos(ang), #4
                                    0, #5
                                    t * top,
                                    size * cos(ang) - t*bot, #7
                                    (size-t) * top,
                                    size * cos(ang + pi/2) + t*bot, #9
                                    t * top),
                  y = y + c(0, #1
                            size * cos(ang), #2
                            size * bot,
                            size * cos(ang-pi/2), #4
                            0, #5
                            t * bot,
                            size * sin(ang) + t*top, #7
                            (size-t) * bot,
                            size * sin(ang + pi/2) - t*top, #9
                            t * bot),
                  fill_color = fill_color,
                  line_color = line_color) %>%
      ly_segments(0+x, 0+y,
                  t*top + x, t*bot + y,
                  color = fill_color,
                  width = 1.5)
    
    return(p)
  }
  
  round_tri <- function(figure, x, y, 
                        rect_thick = 0.0833, curved_thick = 0.0833,
                        inner_rad = 0.5, start_angle = 0, end_angle = pi/2, 
                        fill_color = "#66C2A5", line_color = "beige"){
    
    # there is a bug in this function where at start angles more than 0 one must change the 
    
    l = inner_rad + curved_thick/2
    ang2 = (end_angle-start_angle)/2
    ang1 = ang2 + start_angle
    ang3 = end_angle-asin(rect_thick/l)
    ang5 = start_angle+asin(rect_thick/l)
    
    p <- ly_patch(figure, 
                  x = x + c(0, 
                            l*cos(end_angle), 
                            l*cos(end_angle-asin(rect_thick/l)), 
                            (rect_thick/sin(ang2))*cos(ang1), 
                            l*cos(start_angle+asin(rect_thick/l)), 
                            l*cos(start_angle)),
                  y = y + c(0, 
                            l*sin(end_angle), 
                            l*sin(end_angle-asin(rect_thick/l)), 
                            (rect_thick/sin(ang2))*sin(ang1), 
                            l*sin(start_angle+asin(rect_thick/l)), 
                            l*sin(start_angle)),
                  fill_color = fill_color,
                  line_color = line_color) %>%
      ly_annular_wedge(x, y,
                       start_angle = start_angle,
                       end_angle = end_angle,
                       inner_radius = inner_rad,
                       outer_radius = inner_rad + curved_thick,
                       direction = "anticlock",
                       fill_color = fill_color,
                       line_color = line_color) %>%
      ly_arc(x,y,
             start_angle = start_angle + 0.0025,
             end_angle = end_angle - 0.0025,
             color = fill_color,
             radius = inner_rad,
             width=2) %>%
      ly_arc(x, y,
             start_angle = start_angle + acos(rect_thick/inner_rad) + .003,
             end_angle = end_angle - acos(rect_thick/inner_rad),
             radius = inner_rad  - 0.002,
             direction = "clock",
             color = line_color,
             width = 1)
    
    return(p)
  }
  
  rand_past <- function(){
    x <- brewer.pal(3,sample(brew.cols, 1, replace = TRUE))[1]
    
    return(x)
}
  
  figure <- figure(width = width, height = height, xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
         xlim = c(0,x+1), ylim = c(0,y+1), 
         tools = c("pan","wheel_zoom","resize","save"))
  
  # ### Diagonal Grid
  ncol = y # this must be odd!
  nrow = x
  n = ncol*nrow
  N = ceiling(n/2)
  # sampling
  samp = base::sample(1:N,frac*N)
  pts = rep(0,N)
  pts[samp] = 1
  # putting in grid
  proc = matrix(0, ncol = N, nrow = 2)
  proc[1,] = pts
  grid = matrix(proc, ncol = ncol, nrow = nrow, byrow = T)
  # Selecting for full circles and squares
  zipzip = list()
  zipzip[["squares"]] = c(0,0,0,0)
  geom = matrix(zipzip, ncol = ncol, nrow = nrow)
  
  library(RColorBrewer)
  brew.cols <- c("Set3","Set2","Pastel2","Pastel1","Paired","Accent")
  
  # we cycle though the gridpoints for processing
  for (i in 1:ncol){
    for (j in 1:nrow){
      if (grid[j,i] == 1){
        
        # center processing
        if (i %in% 2:(ncol-1) & j %in% 2:(nrow-1)){
          if (grid[j+1,i+1] == 1)  geom[j,i][[1]][1] = 1
          if (grid[j+1,i-1] == 1)  geom[j,i][[1]][2] = 1
          if (grid[j-1,i-1] == 1)  geom[j,i][[1]][3] = 1
          if (grid[j-1,i+1] == 1)  geom[j,i][[1]][4] = 1
        }
        
        # left processing
        if (i == 1 & j %in% 2:(nrow-1)){
          if (grid[j+1,i+1] == 1)  geom[j,i][[1]][1] = 1
          if (grid[j-1,i+1] == 1)  geom[j,i][[1]][4] = 1
        }
        
        # right processing
        if (i == ncol & j %in% 2:(nrow-1)){
          if (grid[j+1,i-1] == 1)  geom[j,i][[1]][2] = 1
          if (grid[j-1,i-1] == 1)  geom[j,i][[1]][3] = 1
        }
        
        # bot processing
        if (i %in% 2:(ncol-1) & j == 1){
          if (grid[j+1,i+1] == 1)  geom[j,i][[1]][1] = 1
          if (grid[j+1,i-1] == 1)  geom[j,i][[1]][2] = 1
        }
        
        # top processing
        if (i %in% 2:(ncol-1) & j == nrow){
          if (grid[j-1,i-1] == 1)  geom[j,i][[1]][3] = 1
          if (grid[j-1,i+1] == 1)  geom[j,i][[1]][4] = 1
        }
        
        # bottom left
        if (i == 1 & j == 1){
          if (grid[j+1,i+1] == 1)  geom[j,i][[1]][1] = 1
        }
        
        # top left
        if (i == ncol & j == 1){
          if (grid[j+1,i-1] == 1)  geom[j,i][[1]][2] = 1
        }
        
        # bottom right
        if (i == 1 & j == nrow){
          if (grid[j-1,i+1] == 1)  geom[j,i][[1]][4] = 1
        }
        
        # top right
        if (i == ncol & j == nrow){
          if (grid[j-1,i-1] == 1)  geom[j,i][[1]][3] = 1
        }
        
        # if it has links on both top and bottom place a circle
        if (sum(geom[j,i][[1]][1:2]) > 0 & 
            sum(geom[j,i][[1]][3:4]) > 0){
          figure <- circles(figure, j, i, size = circle_size, n = circle_num, 
                            colors = rep(sample(brew.cols,1),6), line_color = "beige")
        }
        
        # if it has links on top place a wedge
        if (sum(geom[j,i][[1]][1:2]) > 0 & 
            sum(geom[j,i][[1]][3:4]) == 0){
          figure <- wedge(figure, j,i, size = circle_size, n=circle_num, 
                          colors = sample(brew.cols,1), ang = pi, offset = pi/4)
        }
        
        # if it has links on bottom place a wedge
        if (sum(geom[j,i][[1]][1:2]) == 0 & 
            sum(geom[j,i][[1]][3:4]) > 0){
          figure <- wedge(figure, j, i, size = circle_size, n=circle_num, 
                          colors = sample(brew.cols,1), ang = pi, offset = 5*pi/4)
        }
      }
    }
  }
  
  rand_past <- function(){
    x <- sample(brewer.pal(8,sample(brew.cols, 1, replace = TRUE)),1)
    
    return(x)
  }
  
  # place the squares and triangle things
  for (i in 1:ncol){
    for (j in 1:nrow){
      if (grid[j,i] == 1 & sum(geom[j,i][[1]]) != 0){
        
        # First quadrant 1
        if (sum(geom[j,i][[1]][1:2]) > 0){
          if (geom[j,i][[1]][1] == 1){
            figure <- square(figure, j, i, size = circle_size, thickness = 1/circle_num,
                             quadrant = 1, fill_color = rand_past(), line_color = "beige")
          } else {
            figure <- round_tri(figure, j, i, rect_thick = 1/circle_num, curved_thick = 1/circle_num,
                                inner_rad = 1 - 1/circle_num, start_angle = 0, end_angle = pi/2,
                                fill_color = rand_past(), line_color = "beige")
          }
        }
        
        # Quadrant 2
        if (sum(geom[j,i][[1]][1:2]) > 0){
          if (geom[j,i][[1]][4] == 1){
            figure <- square(figure, j, i, size = circle_size, thickness = 1/circle_num,
                             quadrant = 2, fill_color = rand_past(), line_color = "beige")
          } else {
            figure <- round_tri(figure, j, i, rect_thick = 1/circle_num, curved_thick = 1/circle_num,
                                inner_rad = 1 - 1/circle_num, start_angle = pi/2, end_angle = pi,
                                fill_color = rand_past(), line_color = "beige")
          }
        }
        
        # Quadrant 3
        if (sum(geom[j,i][[1]][3:4]) > 0){
          if (geom[j,i][[1]][3] == 1){
            figure <- square(figure, j, i, size = circle_size, thickness = 1/circle_num,
                             quadrant = 3, fill_color = rand_past(), line_color = "beige")
          } else {
            figure <- round_tri(figure, j, i, rect_thick = 1/circle_num, curved_thick = 1/circle_num,
                                inner_rad = 1 - 1/circle_num, start_angle = pi, end_angle = 3*pi/2,
                                fill_color = rand_past(), line_color = "beige")
          }
        }
        
        # Quadrant 4
        if (sum(geom[j,i][[1]][3:4]) > 0){
          if (geom[j,i][[1]][2] == 1){
            figure <- square(figure, j, i, size = circle_size, thickness = 1/circle_num,
                             quadrant = 4, fill_color = rand_past(), line_color = "beige")
          } else {
            figure <- round_tri(figure, j, i, rect_thick = 1/circle_num, curved_thick = 1/circle_num,
                                inner_rad = 1 - 1/circle_num, start_angle = 3*pi/2, end_angle = 2*pi,
                                fill_color = rand_past(), line_color = "beige")
          }
        }
      }
    }
  }
  
  return(figure)

}