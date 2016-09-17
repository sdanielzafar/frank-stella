# coencentric circles

library(rbokeh)
library(RColorBrewer)

x <- 1
y <- 1
numcircles <- 6
size = 60
colorBrewerPalette = "Set3"

#display.brewer.all()

raise <- data.frame(flipper = seq(0.5,0.0833,-0.0833),
                    x = rep(1,numcircles),
                    y = rep(1,numcircles),
                    flav = sample(25:50,6),
                    col = brewer.pal(numcircles,colorBrewerPalette ))

# this is where I experiment
p <- figure(xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
            tools = c("pan","wheel_zoom","resize","save")) %>%
  ly_annulus(x, y, 
          data = raise,
          inner_radius = 0,
          outer_radius = flipper*.9,
          fill_color = brewer.pal(numcircles,"Set3"),
          line_color = "beige") %>%
  ly_wedge(x, y, 
            data = raise, 
            radius = flipper,
            start_angle = rep(0,6),
            end_angle = rep(pi/2,6),
            direction = "anticlock",
            fill_color = brewer.pal(numcircles,"Paired"),
            line_color = "beige",
            alpha = 5) %>%
  ly_wedge(x, y, 
           data = raise, 
           radius = flipper,
           start_angle = rep(3*pi/2,6),
           end_angle = rep(pi,6),
           direction = "clock",
           fill_color = brewer.pal(numcircles,"Set2"),
           line_color = "beige",
           alpha = 5) %>%
  ly_patch(x = x + c(0.5-0.0833,0.5,0.5,-0.5,-0.5,0.5,0.5-0.0833,-0.5+0.0833,-0.5+0.0833,0.5-0.0833),
           y = y + c(0.5-0.0833,0.5,-0.5,-0.5,0.5,0.5,0.5-0.0833,0.5-0.0833,-0.5+0.0833,-0.5+0.0833),
           fill_color = "#66C2A5",
           line_color = "beige") %>%
  ly_segments(0.5-0.0833+x,0.5-0.0833+y,0.5+x,0.5+0.001+y,
              color = "#66C2A5",
              width = 1.5)

p

# Experimenting with the arc & rectangle shape
x=1; y=1; arc_w = 47; 
q <- figure(xlim = x + c(-.1, 0.75), ylim = y + c(-.1, 0.75),
            xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
            tools = c("pan","wheel_zoom","resize","save"),
            h_symmetry = TRUE,
            v_symmetry = TRUE) %>%
  ly_annulus(x, y, 
             data = raise,
             inner_radius = 0,
             outer_radius = flipper,
             fill_color = brewer.pal(numcircles,"Set3"),
             line_color = "beige") %>%
  ly_patch(x = x + c(.5+.0833/2, 0, 0, 0.0833, 0.0833, 0.5+0.0833/2),
           y = y + c(0, 0, 0.5 + 0.0833/2, 0.5+0.0833/2, 0.0833, 0.0833),
           fill_color = "#66C2A5",
           line_color = "beige") %>%
  ly_annular_wedge(x, y,
         start_angle = 0,
         end_angle = pi/2,
         outer_radius = 0.5 + 0.0833,
         inner_radius = 0.5,
         direction = "anticlock",
         fill_color = "#66C2A5",
         line_color = "beige") %>%
  ly_arc(x,y,
         start_angle = 0 + 0.0025,
         end_angle = pi/2 - 0.0025,
         color = "#66C2A5",
         radius = 0.5) %>%
  ly_arc(x, y,
         start_angle = acos(0.0833/0.5) + .003,
         end_angle = pi/2 - acos(0.0833/0.5),
         radius = 0.5 - 0.001,
         direction = "clock",
         color = "beige",
         width = 1)
  
q

circles <- function(figure, x, y, size, n = 6, 
                    colors = rep("Set3",6), line_color = "beige", 
                    wedge_col = NULL, wedge_ang = rep(pi/2,6), 
                    wedge_off = rep(0,6), wedge_sz = rep(1.1,6)){
  
  # Colors use RColorBrewer package, see display.brewer.all() for options
  require(rbokeh)
  
  p <- ly_annulus(figure, rep(x[1],n[1]), rep(y[1],n[1]), 
                  inner_radius = 0,
                  outer_radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
                  fill_color = brewer.pal(numcircles,colors[1]),
                  line_color = line_color) 
  if (!is.null(wedge_col)){
    p <- ly_wedge(p, rep(x[1],n[1]), rep(y[1],n[1]), 
                  radius = seq(size[1]*wedge_sz[1],size[1]*wedge_sz[1]/n[1],-size[1]*wedge_sz[1]/n[1]),
                  start_angle = pi/4 - wedge_ang[1]/2 + wedge_off[1],
                  end_angle = pi/4 + wedge_ang[1]/2 + wedge_off[1],
                  direction = "anticlock",
                  fill_color = brewer.pal(numcircles,wedge_col[1]),
                  line_color = "beige",
                  alpha = 5) %>%
      ly_wedge(rep(x[1],n[1]), rep(y[1],n[1]), 
               radius = seq(size[1]*wedge_sz[1],size[1]*wedge_sz[1]/n[1],-size[1]*wedge_sz[1]/n[1]),
               start_angle = 5*pi/4 - wedge_ang[1]/2 + wedge_off[1],
               end_angle = 5*pi/4 + wedge_ang[1]/2 + wedge_off[1],
               direction = "anticlock",
               fill_color = brewer.pal(numcircles,wedge_col[1]),
               line_color = "beige",
               alpha = 5)
  }
  
  if (length(x) > 1){
    for (i in 2:length(x)){
      p <- ly_annulus(p, rep(x[i],n[i]), rep(y[i],n[i]), 
                 inner_radius = 0,
                 outer_radius = seq(size[i],size[i]/n[i],-size[i]/n[i]),
                 fill_color = brewer.pal(numcircles,colors[i]),
                 line_color = line_color)
      if (!is.null(wedge_col)){
        p <- ly_wedge(p, rep(x[i],n[i]), rep(y[i],n[i]), 
                      radius = seq(size[i]*wedge_sz[i],size[i]*wedge_sz[i]/n[i],-size[i]*wedge_sz[i]/n[i]),
                      start_angle = pi/4 - wedge_ang[i]/2 + wedge_off[i],
                      end_angle = pi/4 + wedge_ang[i]/2 + wedge_off[i],
                      direction = "anticlock",
                      fill_color = brewer.pal(numcircles,wedge_col[i]),
                      line_color = "beige",
                      alpha = 5) %>%
          ly_wedge(rep(x[i],n[i]), rep(y[i],n[i]), 
                   radius = seq(size[i]*wedge_sz[i],size[i]*wedge_sz[i]/n[i],-size[i]*wedge_sz[i]/n[i]),
                   start_angle = 5*pi/4 - wedge_ang[i]/2 + wedge_off[i],
                   end_angle = 5*pi/4 + wedge_ang[i]/2 + wedge_off[i],
                   direction = "anticlock",
                   fill_color = brewer.pal(numcircles,wedge_col[i]),
                   line_color = "beige",
                   alpha = 5)
      }
    }
  }
  
  return(p)
  
}

wedges <- function(figure, x, y, size, n=6, 
                   ang = pi/2, offset = 0,
                   colors = rep("Set3",6), line_color = "beige"){
  p <- ly_wedge(figure, rep(x[1],n[1]), rep(y[1],n[1]), 
           radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
           start_angle = pi/4 - ang/2 + offset,
           end_angle = pi/4 + ang/2 + offset,
           direction = "anticlock",
           fill_color = brewer.pal(numcircles,colors[1]),
           line_color = "beige",
           alpha = 5) %>%
    ly_wedge(rep(x[1],n[1]), rep(y[1],n[1]), 
                radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
                start_angle = 5*pi/4 - ang/2 + offset,
                end_angle = 5*pi/4 + ang/2 + offset,
                direction = "anticlock",
                fill_color = brewer.pal(numcircles,colors[1]),
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
                  fill_color = "#66C2A5",
                  line_color = "beige") %>%
    ly_segments(0+x, 0+y,
                t*top + x, t*bot + y,
                color = "#66C2A5",
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

library(RColorBrewer)
brew.cols <- c("Set3","Set2","Pastel2","Pastel1","Paired","Dark2","Accent")

n = sample(3:20,1)
cir <- data.frame(x = sample(1:10, n, replace = TRUE),
                  y = sample(1:10,n, replace = TRUE),
                  size = sample(seq(1,3,.25),n, replace = TRUE),
                  n = sample(3:8,n, replace = TRUE),
                  color = sample(brew.cols,n, replace = TRUE))

wedge <- data.frame(col = sample(brew.cols,n, replace = TRUE),
                    ang = sample(seq(pi/16,pi/2,pi/32), n, replace = T),
                    offset = sample(seq(0,pi,pi/32), n, replace = T),
                    size = sample(seq(1.1,1.5,0.05), n, replace = T))

c <- figure(xgrid = FALSE, ygrid = FALSE, 
            xlab = "", ylab = "",
            xlim = c(-2,12), ylim = c(-2,12),
            legend_location = NULL) %>%
  circles(x = cir$x, y = cir$y, 
          size = cir$size,
          n = cir$n,
          colors = as.character(cir$color),
          wedge_col = wedge$col, 
          wedge_ang = wedge$ang, 
          wedge_off = wedge$offset, 
          wedge_sz = wedge$size)
c


