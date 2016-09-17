
bing = 8
window = 20  

figure(xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
       xlim = c(-window,window), ylim = c(-window,window), 
       tools = c("pan","wheel_zoom","resize","save")) %>%
  # The circles
  wedge(bing,-bing, size = bing-1, n=bing-1, colors = "Pastel1",
        ang = pi, offset = pi/4) %>%
  wedge(0, 0, size = bing-1, n=bing-1, colors = "Set2",
         ang = pi, offset = -pi/4) %>%
  wedge(0, 0, size = bing, n=bing, colors = "Paired",
        ang = pi, offset = 3*pi/4) %>%
  wedge(-bing,bing, size = bing-1, n=bing-1, colors = "Pastel2",
        ang = pi, offset = 5*pi/4) %>%
  # the squares
  square(0, 0, size = bing, thickness = 1, quadrant = 2,
         fill_color = "#66C2A5", line_color = "beige") %>%
  square(0, 0, size = bing, thickness = 1, quadrant = 4,
         fill_color = "#ffa388", line_color = "beige") %>%
  # the arc things
  round_tri(0, 0, rect_thick = 1, curved_thick = 1,
            inner_rad = bing - 1, start_angle = 0, end_angle = pi/2,
            fill_color = "#b05d75", line_color = "beige") %>%
  round_tri(0, 0, rect_thick = 1, curved_thick = 1,
            inner_rad = bing - 1, start_angle = pi, end_angle = 3*pi/2,
            fill_color = "#d2dce6", line_color = "beige") %>%
  round_tri(bing,-bing, rect_thick = 1, curved_thick = 1,
            inner_rad = bing - 1, start_angle = 0, end_angle = pi/2,
            fill_color = "#9fd175", line_color = "beige") %>%
  round_tri(-bing,bing, rect_thick = 1, curved_thick = 1,
            inner_rad = bing - 1, start_angle = pi, end_angle = 3*pi/2,
            fill_color = rand_past(), line_color = "beige") 
