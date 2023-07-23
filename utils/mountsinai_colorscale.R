mountsinai_palette <- function(n, pal){
  
  lightblue = c("#00688F", "#00B0E7", "#0094CC", 
                "#59B8F1", "#95CBF4", "#BCDBF7")
  pink = c("#9E0665", "#B80877", "#CE0A85",
           "#DC5A9D", "#E595B9", "#EDBCD0")
  darkblue = c("#161452", "#1B1960", "#201D6D",
               "#2B0D72", "#5F5E89", "#BDBDC9")
  darkgray <- c("#828382", "#979897", "#979897",
                "#BCBCBC", "#CDCECD", "#DDDDDD")
  lightgray <- c("#A2A3A2", "#BDBDBD", "#D3D4D3",
                 "#E1E2E1", "#E8E9E8", "#EFEFEF")
  
  if (pal == "lightblue")
    if (n <= 6){
      out <- lightblue[1:n]
    } else {
      aux <- colorRampPalette(c(lightblue[1], lightblue[6]))
      out <- aux(n)
    }
  if (pal == "pink")
    if (n <= 6){
      out <- pink[1:n]
    } else {
      aux <- colorRampPalette(c(pink[1], pink[6]))
      out <- aux(n)
    }
  if (pal == "darkblue")
    if (n <= 6){
      out <- darkblue[1:n]
    } else {
      aux <- colorRampPalette(c(darkblue[1], darkblue[6]))
      out <- aux(n)
    }
  if (pal == "darkgray")
    if (n <= 6){
      out <- darkgray[1:n]
    } else {
      aux <- colorRampPalette(c(darkgray[1], darkgray[6]))
      out <- aux(n)
    }
  if (pal == "lightgray")
    if (n <= 6){
      out <- lightgray[1:n]
    } else {
      aux <- colorRampPalette(c(lightgray[1], lightgray[6]))
      out <- aux(n)
    }
  if (pal == "pink_darkblue"){
    aux <- colorRampPalette(c(pink[3], darkgray[6],  darkblue[3]))
    out <- aux(n)
  }
  
  if (pal == "pink_lightblue"){
    aux <- colorRampPalette(c(pink[3], lightgray[6], lightblue[3]))
    out <- aux(n)
  }
  
  if (pal == "dark_lightblue"){
    aux <- colorRampPalette(c(darkblue[3], lightgray[6], lightblue[3]))
    out <- aux(n)
  }
  
  if (pal == "divergent"){
    aux <- colorRampPalette(c(pink[3], darkgray[3], darkblue[3], lightgray[3], lightblue[3]))
    out <- aux(n)
  }
  
  if (pal == "convergent"){
    aux <- colorRampPalette(c(darkgray[3],  pink[3], darkblue[3], lightblue[3], lightgray[3]))
    out <- aux(n)
  }
  
  return(out)
}


