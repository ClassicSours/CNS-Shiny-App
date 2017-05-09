pkgs <-c('shiny','ggplot2','stringr','tidyr','plotly')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm('p','pkgs')


posters <- read.csv("poster_list.csv", header=TRUE)

names(posters) <- c("ID","author","title")

posters.df <<- separate(posters, col = "ID", 
                       into = c("Category","ID"),
                       sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
              mutate(Category = factor(Category), ID = factor(ID))

fields <- c("Category","ID","JudgeID","total","best")


posterInfo <- function(poster){
  poster <- str_replace_all(poster, "[ ]", "")
  category <- toupper(str_extract_all(poster,"[a-zA-Z]+"))
  if(category == "G")
    category = "GF"
  if(category == "U")
    category = "UF"
  posterno <- str_extract(poster,"[0-9]+")
  if(category == "CHARACTER(0)")
    category <- NA
  return(c(category,posterno))
}

saveData <- function(data) {
  data <- as.data.frame(data)
  if (exists("responses")) {
    ## Validate if judge exists ?
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

saveGSM <- function(votes){
  if(exists("GSM")){
    GSM <<- rbind(GSM,votes)
  } else {
    GSM <<- votes
  }
}

savePeoplesChoice <- function(votes) {
  if(exists("PEOPLESCHOICE")){
    PEOPLESCHOICE <<- rbind(PEOPLESCHOICE,votes)
  } else {
    PEOPLESCHOICE <<- votes
  }
}

getpalette <- function
(input){
  if(input == "GF")
    return("Reds")
  if(input == "UF")
    return("Greens")
  if(input == "S")
    return("Blues")
  if(input == "GSM")
    return("Oranges")
  return("Set3")
}

## All plotlynow
# plotData <- function(category){
#   if(category == "GF" | category == "S" | category == "UF"){
#     if(exists("responses")){
#       if(length(which(responses[,"Category"] == category)) == 0) {
#         return(NULL)
#       }
#       return(
#         responses %>% 
#           filter(Category == category) %>% 
#           select(Category, ID, total, best) %>%
#           group_by(ID) %>%
#           mutate(score = sum(total,best)) %>%
#           select(Category, ID, score) %>%
#           unique() %>%
#           arrange(desc(score)) %>% head(n=5) %>%  
#           
#           ggplot(aes(x = reorder(paste(Category,ID,sep=""),desc(score)), y = score, fill = ID)) +
#             scale_fill_brewer(palette = paste(getpalette(category))) +
#             geom_bar(stat="identity") +
#             geom_text(aes(label = score), vjust = 0) + 
#             theme(legend.position = "none", 
#                axis.ticks = element_blank(), 
#                panel.background = element_blank()) +
#             labs(x = "Poster ID", y = "Total Score")
#       )
#     }  
#   }
#   if(category == "GSM"){
#     if(exists("GSM")){
#       return(GSM %>% 
#                group_by("ID" = paste(Category,ID,sep="")) %>% 
#                tally() %>% 
#                data.frame() %>% head(n = 5) %>%
#                
#                ggplot(aes(x = reorder(ID,desc(n)), y = n, fill = ID)) + 
#                scale_fill_brewer(palette = paste(getpalette("GSM"))) +
#                geom_bar(stat="identity") + 
#                geom_text(aes(label = n), vjust = 0) +
#                theme(legend.position = "none",
#                      axis.ticks = element_blank(),
#                      panel.background = element_blank()) +
#                labs(x = "Poster ID", y = "Total Votes"))
#     }   
#   }
#   if(category == "PEOPLESCHOICE"){
#     if(exists("PEOPLESCHOICE")){
#       return(PEOPLESCHOICE %>% 
#                group_by(Category, "ID" = paste(Category,ID,sep="")) %>% 
#                tally() %>% 
#                top_n(n=2) %>%
#                
#                ggplot(aes(x = reorder(ID,desc(n)), y = n, 
#                           fill = 
#                             c("1","2","3")[as.numeric(Category)])) +
#                scale_fill_manual(values = c("red","blue","green")) +
#                geom_bar(stat="identity") + 
#                geom_text(aes(label = n), vjust = 0) +
#                theme(legend.position = "none",
#                      axis.ticks = element_blank(),
#                      panel.background = element_blank()) +
#                labs(x = "Poster ID", y = "Total Votes")
#              )
#     }
#   }
# }

plotlyData <- function(category){
  if(category == "GF" | category == "S" | category == "UF"){
    if(exists("responses")){
      if(length(which(responses[,"Category"] == category)) == 0) {
        return(NULL)
      }
      suppressMessages(
        suppressWarnings(
          df <- responses %>% 
                  filter(Category == category) %>% 
                  select(Category, ID, total, best) %>%
                  group_by(ID) %>%
                  mutate(score = sum(total,best)) %>%
                  select(Category, ID, score) %>%
                  unique() %>%
                  arrange(desc(score)) %>% head(n=5) %>%
                  left_join(posters.df)
        )
      )
      gg <- ggplot(df, aes(x = reorder(paste(Category,ID,sep=""),desc(score)), 
                       y = score, 
                       fill = ID,
                       "poster.author" = author,
                       "poster.title" = title)) +
              scale_fill_brewer(palette = paste(getpalette(category))) +
              geom_bar(stat="identity") +
              geom_text(aes(label = score), vjust = 0) + 
              theme(legend.position = "none", 
                axis.ticks = element_blank(), 
                panel.background = element_blank()) +
              labs(x = "Poster ID", y = "Total Score")
      
      ply.gg <- ggplotly(gg, tooltip = c("poster.author","poster.title"))
      
      for (i in 1:length(ply.gg$x$data)){
        ply.gg$x$data[[i]]$text <- c(ply.gg$x$data[[i]]$text, "") 
      }
      
      return(ply.gg)
    }  
  }
  if(category == "GSM"){
    if(exists("GSM") & exists("posters.df")){
      suppressMessages(
        suppressWarnings(
          df <- GSM %>% 
                  group_by("NAME" = factor(paste(Category,ID,sep=""))) %>%
                  tally() %>% 
                  ungroup() %>%
                  separate(col = "NAME", into = c("Category","ID"),
                               sep = "(?<=[A-Z]) ?(?=[0-9])", remove = FALSE) %>%
                  data.frame() %>% head(n = 5) %>% left_join(posters.df)
        )
      )
      gg <- df %>% 
              ggplot(aes(x = reorder(NAME,desc(n)), 
                        y = n,
                        fill = NAME,
                        label = n,
                        "poster.author" = author,
                        "poster.title" = title)) + 
              geom_text(vjust = 0) +
              geom_bar(stat="identity") + 
              scale_fill_brewer(palette = paste(getpalette(category))) +
              theme(legend.position = "none",
                    axis.ticks = element_blank(),
                    panel.background = element_blank()) +
              labs(x = "Poster ID", y = "Total Votes")

      ply.gg <- ggplotly(gg, tooltip = c("poster.author","poster.title"))
      
      for (i in 1:length(ply.gg$x$data)){
        ply.gg$x$data[[i]]$text <- c(ply.gg$x$data[[i]]$text, "") 
      }
      
      return(ply.gg)
    }   
  }
  if(category == "PEOPLESCHOICE"){
    if(exists("PEOPLESCHOICE") & exists("posters.df")){
      suppressWarnings(
        suppressMessages(
          df <- PEOPLESCHOICE %>% 
            group_by(Category, "NAME" = paste(Category,ID,sep="")) %>% 
            tally() %>% 
            top_n(n=2) %>% data.frame() %>% ungroup() %>%
            separate(col = "NAME", into = c("Original C","ID"),
                 sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
            left_join(posters.df)
        )
      )
      
      gg <- df %>% ggplot(aes(x = reorder(paste(Category,ID,sep=""),desc(n)), 
                              y = n,
                              fill = Category,
                              "poster.author" = author,
                              "poster.title" = title)) +
               geom_bar(stat="identity") + 
               scale_fill_brewer(palette = paste(getpalette(category))) +
               geom_text(aes(label = n), vjust = 0) +
               theme(legend.position = "none",
                     axis.ticks = element_blank(),
                     panel.background = element_blank()) +
               labs(x = "Poster ID", y = "Total Votes")
      ply.gg <- ggplotly(gg, tooltip=c("poster.author","poster.title"))
      
      for (i in 1:length(ply.gg$x$data)){
        ply.gg$x$data[[i]]$text <- c(ply.gg$x$data[[i]]$text, "") 
      }
      
      return(ply.gg)
    }
  }
  return(ggplot() + theme_void())
}

getWinners <- function(category){
  if(exists("posters.df")){
    if(category == "GF" | category == "S" | category == "UF"){
      if(exists("responses")){
        suppressWarnings(
          suppressMessages(
            winners <- responses %>% 
              filter(Category == category) %>% 
              left_join(posters.df) %>% rowwise() %>%
              mutate(score = sum(total,best)) %>% 
              arrange(desc(score)) %>% 
              data.frame()
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$score)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$score)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$score)
        }
        return(to.return)
      }
    }
    if(category == "PEOPLESCHOICE"){
      if(exists("PEOPLESCHOICE") & exists("posters.df")){
        suppressWarnings(
          suppressMessages(
            winners <- PEOPLESCHOICE %>% 
              group_by(Category, "NAME" = paste(Category,ID,sep="")) %>% 
              tally() %>% 
              slice(which.max(n)) %>% data.frame() %>% ungroup() %>%
              separate(col = "NAME", into = c("Original C","ID"),
                       sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
              left_join(posters.df)
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$n)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$n)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$n)
        }
        return(subset(to.return,!is.na(to.return$author) & !is.na(to.return$author) & !is.na(to.return$score)))
      }
    }
    if(category == "GSM"){
      if(exists("GSM")){
        suppressWarnings(
          suppressMessages(
            winners <- GSM %>% 
              group_by("ID" = paste(Category,ID,sep="")) %>%
              count(sort = TRUE) %>% 
              data.frame() %>% head(n = 3) %>% 
              separate(col = "ID", into = c("Category","ID"), 
                       sep = "(?<=[A-Z]) ?(?=[0-9])") %>%
              left_join(posters.df)
          )
        )
        to.return <- data.frame("author" = rep("NA",3),"title" = rep("NA",3),  "score" = rep(NA,3), stringsAsFactors = FALSE)
        num.posters <- nrow(winners)
        if(num.posters >= 1) {
          to.return[1,]$author <- as.character(winners[1,]$author)
          to.return[1,]$title <- as.character(winners[1,]$title)
          to.return[1,]$score <- as.integer(winners[1,]$n)
        }
        if(num.posters >= 2){
          to.return[2,]$author <- as.character(winners[2,]$author)
          to.return[2,]$title <- as.character(winners[2,]$title)
          to.return[2,]$score <- as.integer(winners[2,]$n)
        }
        if(num.posters >= 3){
          to.return[3,]$author <- as.character(winners[3,]$author)
          to.return[3,]$title <- as.character(winners[3,]$title)
          to.return[3,]$score <- as.integer(winners[3,]$n)
        }
        return(subset(to.return,!is.na(to.return$author) & !is.na(to.return$author) & !is.na(to.return$score)))
      }
    }
  }
}
