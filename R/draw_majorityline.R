draw_majorityline <- function(..., 
                              n = NULL, 
                              type = c("horseshoe",
                                       "semicircle",
                                       "opposing_benches")) {
   
  structure(
    list(
      type = type,
      n = n
    ),
    class="majorityLine"
  )
  
}


ggplot_add.majorityLine <- function(object, plot, object_name){
  
  new_dat <- plot$data %>% 
    dplyr:: filter(government == 1) %>% 
    dplyr::filter(dplyr::row_number() == object$n)
  x_pos <- new_dat$x
  
  
  y_pos_oppbenches <- new_dat$y
  
  if(object$type == "semicircle"){
    plot + 
      ggplot2::geom_segment(aes(y=0.8, yend=2.2, x= x_pos, xend=x_pos), colour='grey', size=0.6, linetype=1, alpha=0.6) + 
      ggplot2::annotate("text", x=x_pos, y=0.6, label = paste0(object$n, " seats\nneeded for\na majority."))
  } else{
    if(object$type == "horseshoe"){
     plot +
        ggplot2::geom_segment(aes(y=7, yend=10.2, x= x_pos, xend=x_pos), colour='grey', size=0.6, linetype=1, alpha=0.6) + 
        ggplot2::annotate("text", x=x_pos, y=6, label = paste0(object$n, " seats\nneeded for\na majority."))
    }else{
      if(object$type == "opposing_benches"){
       plot + 
          ggplot2::geom_segment(aes(y=y_pos_oppbenches, yend=y_pos_oppbenches, x=min(plot$data$x) - 1, xend=max(plot$data$x) + 1), colour='grey', size=0.6, linetype=1, alpha=0.6) +
          ggplot2::annotate("text", x= 5, y=y_pos_oppbenches + 3, label = paste0(object$n, " seats needed\nfor a majority."))
      } else{
        warning("parliament layout not supported")
      }
    }
  }
  
}

