q.colors <-  c(grDevices::rgb(91, 155, 213, 255, max = 255), # blue
grDevices::rgb(237, 125, 49, 255, max = 255), # orange
grDevices::rgb(165, 165, 165, 255, max = 255), # grey
grDevices::rgb(30, 192, 0, 255, max = 255), # yelow
grDevices::rgb(68, 114, 196, 255, max = 255), # darker blue
grDevices::rgb(112, 173, 71, 255, max = 255), # green
grDevices::rgb(37, 94, 145, 255, max = 255), # even darker blue
grDevices::rgb(158, 72, 14, 255, max = 255), # blood
grDevices::rgb(99, 99, 99, 255, max = 255), # dark grey
grDevices::rgb(153, 115, 0, 255, max = 255), # brown
grDevices::rgb(38, 68, 120, 255, max = 255), # very dark blue
grDevices::rgb(67, 104, 43, 255, max = 255), # darker green
grDevices::rgb(255, 255, 255, 255, max = 255), # black
grDevices::rgb(255, 35, 35, 255, max = 255)) # red

devtools::use_data(q.colors, internal = FALSE, overwrite = TRUE)
