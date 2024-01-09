generate_tabset <- function(tabs,
                            tabtitle,
                            tablevel = 1) {
  # markdown <- paste0(
  #   paste(rep("#",tablevel),collapse=""),
  #   " ",
  #   tabtitle,
  #   " ",
  #   "{.tabset}\n\n")
  
  # markdown <- "::: panel-tabset"
  markdown <- ""
  
  for (i in seq_along(tabs)) {
    title <- names(tabs)[i]
    content <- tabs[[i]]
    markdown <- paste0(markdown, 
                       paste(rep("#",tablevel + 1),collapse=""),
                       " ", 
                       title, 
                       "\n\n", 
                       content, 
                       "\n\n")
  }
  # markdown <- paste0(markdown,"\n",":::")
  return(markdown)
}