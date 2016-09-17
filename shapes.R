# this script contains the functions used for the First frank stella piece


library(rbokeh)

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

wedges <- function(figure, x, y, size, n=6, 
                   ang = pi/2, offset = 0,
                   colors = rep("Set3",6), line_color = "beige"){
  p <- ly_wedge(figure, rep(x[1],n[1]), rep(y[1],n[1]), 
                radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
                start_angle = pi/4 - ang/2 + offset,
                end_angle = pi/4 + ang/2 + offset,
                direction = "anticlock",
                fill_color = brewer.pal(n,colors[1]),
                line_color = "beige",
                alpha = 5) %>%
    ly_wedge(rep(x[1],n[1]), rep(y[1],n[1]),
             radius = seq(size[1],size[1]/n[1],-size[1]/n[1]),
             start_angle = 5*pi/4 - ang/2 + offset,
             end_angle = 5*pi/4 + ang/2 + offset,
             direction = "anticlock",
             fill_color = brewer.pal(n,colors[1]),
             line_color = "beige",
             alpha = 5)
  
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
