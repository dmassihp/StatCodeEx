library(shiny)
library(ggplot2)
library(wordcloud)
library(tm)
library(SnowballC)

shinyServer(function(input, output) {
  
  webapp_theme <- function(){
    theme(
      plot.title = element_text(size = 18), 
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
      legend.title = element_text(size=14),
      legend.text = element_text(size=14)
      )
  }
  
  okcupid <- read.csv("profiles.csv", header = T)
  
  ##Jun---------------------------------------------------------------------------------------------
  
  okcupid2 <- okcupid
  okcupid2$diet <- as.character(okcupid2$diet)
  okcupid2$religion <- as.character(okcupid2$religion)
  okcupid2$offspring <- as.character(okcupid2$offspring)
  
  religion <- c("agnosticism", "atheism", "buddhism", 
                "catholicism", "christianity", "hinduism",
                "islam", "judaism", "other")
  religion_caps <- c("Agnosticism", "Atheism", "Buddhism", 
                     "Catholicism", "Christianity", "Hinduism",
                     "Islam", "Judaism", "Other")
  
  for (i in 1:9) 
  {
    curr_religion <- grepl(pattern = religion[i], x = okcupid2$religion)
    okcupid2$religion[curr_religion] <- religion_caps[i]
  }
  
  
  generaldiet <- okcupid2
  
  generaldiet <- generaldiet[!(is.na(generaldiet$diet) | 
                                 generaldiet$diet == ""), ]
  
  diet <- c("anything", "halal", "kosher", 
            "vegetarian", "vegan", "other")
  diet_caps <- c("Anything", "Halal", "Kosher",
                 "Vegetarian", "Vegan", "Other")
  
  for (i in 1:6)
  {
    curr_diet <- grepl(pattern = diet[i], x = generaldiet$diet)
    generaldiet$diet[curr_diet] <- diet_caps[i]
  }
  
  
  vegetarian <- subset(okcupid2, 
                       diet == "strictly vegetarian" |
                         diet == "mostly vegetarian" |
                         diet == "vegetarian")
  
  
  vegetarian <- vegetarian[!(is.na(vegetarian$religion) | 
                               vegetarian$religion==""), ]
  
  no_kids <- grepl(pattern = "doesn&rsquo;t", x = vegetarian$offspring)
  vegetarian$offspring[no_kids] = "Doesn't Have Children"
  one_kid <- grepl(pattern = "has a kid", x = vegetarian$offspring)
  vegetarian$offspring[one_kid] = "Has a Child"
  multiple_kids <- grepl(pattern = "has kids", x = vegetarian$offspring)
  vegetarian$offspring[multiple_kids] = "Has Children"
  
  vegetarian <- subset(vegetarian, 
                       offspring %in% c("Doesn't Have Children",
                                        "Has a Child",
                                        "Has Children"))
  
  
  output$religion_plot <- renderPlot({
    
    ggplot(vegetarian) + 
      geom_bar(aes(x = religion), fill = "lightgreen") +
      ggtitle("Number of Vegetarians Based on Religion") +
      labs(x = "Religion", y = "Number of Vegetarians")
    
  })
  
  
  output$age <- renderPlot({
    
    if (input$age_input == 1)
      
      age_plot <- ggplot(vegetarian) + 
        geom_density(aes(x = age)) + xlim(0, 80) +
        ggtitle("Age Distribution of Vegetarians") +
        labs(x = "Age", y = "Density") +
        scale_colour_discrete(name = "")
    
    else if (input$age_input == 2) {
      
      age_plot <- ggplot(vegetarian) + 
        geom_density(aes(x = age, colour = offspring)) +
        xlim(0, 80) +
        ggtitle("Age Distribution of Vegetarians based on Offspring") +
        labs(x = "Age", y = "Density") +
        theme(legend.position = c(.8, .8),
              legend.title=element_blank())
      
    }
    
    else {
      
      age_plot <- ggplot(generaldiet) +
        geom_density(aes(x = age, colour = diet)) +
        xlim(0, 80) +
        ggtitle("Age Distribution based on Diet") +
        labs(x = "Age", y = "Density") +
        theme(legend.position = c(.8, .8),
              legend.title=element_blank())      
    }
    
    age_plot
    
  })
  
  ##Jim---------------------------------------------------------------------------------------------
  
  profiles <- okcupid
  
  height_weight <- data.frame(
    diet = profiles$diet,
    height = profiles$height,
    body_type = profiles$body_type,
    is_vegetarian = TRUE
  )
  
  height_weight$is_vegetarian[profiles$diet != "strictly vegetarian" &
                                profiles$diet != "mostly vegetarian" &
                                profiles$diet != "vegetarian" &
                                profiles$diet != "vegan" &
                                profiles$diet != "mostly vegan" &
                                profiles$diet != "strictly vegan"] = FALSE
  
  height_weight <- height_weight[profiles$diet != "",]
  
  
  
  profiles <- profiles[profiles$diet == "strictly vegetarian" |
                      profiles$diet == "mostly vegetarian" |
                      profiles$diet == "vegetarian",]
  
  profiles_non_v <- profiles[profiles$diet != "strictly vegetarian" &
                               profiles$diet != "mostly vegetarian" &
                               profiles$diet != "vegetarian" &
                               profiles$diet != "vegan" &
                               profiles$diet != "mostly vegan" &
                               profiles$diet != "strictly vegan" &
                               profiles$diet != "",] 
  
  smoke_drink <- data.frame(
    smokes = profiles$smokes,
    drinks = profiles$drinks
  )
  
  
  
  smoke_drink <- smoke_drink[levels(smoke_drink$smokes)[smoke_drink$smokes] != "" &
                             levels(smoke_drink$drinks)[smoke_drink$drinks] != "",]
  
  smoke_drink$smokes[smoke_drink$smokes != 'no'] <- 'yes'
  
  order_var <- vector(length = length(smoke_drink$drinks))
  order_var[smoke_drink$drinks == 'not at all'] <- 1
  order_var[smoke_drink$drinks == 'rarely'] <- 2
  order_var[smoke_drink$drinks == 'socially'] <- 3
  order_var[smoke_drink$drinks == 'often'] <- 4
  order_var[smoke_drink$drinks == 'very often'] <- 5
  order_var[smoke_drink$drinks == 'desperately'] <- 6
  
  
  output$smoke_drink_plot_1 <- renderPlot({
    
    smoke_drink_hist <- ggplot(data = smoke_drink) + aes(x = reorder(drinks, order_var), fill = smokes) +
      geom_bar(position = "dodge") +
      labs(title = "Histogram of Smoking Habit Given Drinking Habit", x="Drinking Habit")
    
    if(input$sqrt_scale) {
      smoke_drink_hist <- ggplot(data = smoke_drink) + aes(x = reorder(drinks, order_var), fill = smokes) +
        geom_bar(position = "dodge") +
        labs(title = "Histogram of Smoking Habit Given Drinking Habit (Sqrt Scale)", x="Drinking Habit") +
        coord_trans(y = "sqrt")
    }
    
    smoke_drink_hist + webapp_theme()
    
  })
  
  smoke_drink_tab <- table(smoke_drink$smokes, smoke_drink$drinks)
  smoke_drink_tab <- smoke_drink_tab[c(2, 6), 2:7]
  smoke_drink_proportion <- smoke_drink_tab[2,] / (smoke_drink_tab[1,] + smoke_drink_tab[2,])
  
  
  output$smoke_drink_plot_2 <- renderPlot({
    
    ggplot() + aes(x = reorder(c("desperately", "not at all", "often", "rarely", "socially", "very often"),c(6, 1, 4, 2, 3, 5)), y = smoke_drink_proportion) +
      geom_bar(stat = "identity", fill = "#408040") + 
      labs(title = "Proportion of Smokers Given Drinking Habit", x = "Drinking Habit", y = "Proportion") +
      webapp_theme()
    
  })
  
  output$smoke_drink_plot_3 <- renderPlot({
    
    mosaicplot(smoke_drink_tab[,c(2,4,5,3,6,1)], shade = T, las=1, main="Association Between Drinking and Smoking Habits", xlab="Smoke", ylab="Drink", cex=1.25)
    
  })
  
  
  
  output$height_weight_plot_1 <- renderPlot({
    
    if(input$show_stats){
      ggplot(data = height_weight) + aes(x = is_vegetarian, y = height) +
        geom_boxplot(fill = c("#da9826", "#408040")) +
        labs(title="Height Distributions of Vegetarians vs Non-vegetarians", x="Vegetarian", y="Inches") +
        webapp_theme() +
        geom_text(size=6,aes(x = 1, y = 25, label=paste("Median:", median(height_weight$height[height_weight$is_vegetarian == FALSE])))) +
        geom_text(size=6,aes(x = 1, y = 21, label=paste("Mean:", round(mean(height_weight$height[height_weight$is_vegetarian == FALSE]), digits=1)))) +
        geom_text(size=6,aes(x = 2, y = 25, label=paste("Median:", median(height_weight$height[height_weight$is_vegetarian == TRUE])))) +
        geom_text(size=6,aes(x = 2, y = 21, label=paste("Mean:", round(mean(height_weight$height[height_weight$is_vegetarian == TRUE]), digits=1))))
    }
    else{
      ggplot(data = height_weight) + aes(x = is_vegetarian, y = height) +
        geom_boxplot(fill = c("#da9826", "#408040")) +
        labs(title="Height Distributions of Vegetarians vs Non-vegetarians", x="Vegetarian", y="Inches") +
        webapp_theme()
    }
  })
  
  height_weight <- height_weight[height_weight$body_type != "" &
                                 height_weight$body_type != "rather not say",]
  
  body_veg_tab <- table(height_weight$is_vegetarian, height_weight$body_type)
  body_veg_tab <- body_veg_tab[,c(-1,-10)]
  body_veg_proportion <- body_veg_tab[2,] / (body_veg_tab[1,] + body_veg_tab[2,])
  
  output$height_weight_plot_2 <- renderPlot({
    
    if(input$prop_veg){
      ggplot() + aes(x = colnames(body_veg_tab), y = body_veg_proportion) +
        geom_bar(stat = "identity", fill = "#408040") + 
        labs(title = "Proportion of Vegetarians Given Body Type", x = "Body Type", y = "Proportion") +
        webapp_theme() +
        theme(
          axis.text = element_text(size=10)
        )
    }
    else{
      ggplot(data = height_weight) + aes(x = body_type, fill = is_vegetarian) +
        geom_bar(position = "dodge") +
        labs(title="Distribution of Body Type and Diet", x="Body Type") +
        webapp_theme() +
        scale_fill_manual("Vegetarian", values = c("#da9826", "#408040")) +
        theme(
          axis.text = element_text(size=10)
        )
    }
    
  })
  
  ##Jennifer----------------------------------------------------------------------------------------
  
  okcupid_all <- okcupid
  okcupid_veg <- subset(okcupid, grepl("vegetarian", diet))
  
  essay <- function(essay_num, gender) {
    okcupid_gender <- subset(okcupid_veg, sex==gender)
    if(essay_num == "4") {
      essay <-  Corpus(VectorSource(okcupid_gender$essay4))
      essay <- lapply(essay, function(x) 
        gsub("book|movie|show|music|food|favorite", " ", x))
    }
    if(essay_num == "5") {
      essay <-  Corpus(VectorSource(okcupid_gender$essay5))
    }
    plain_essay <- lapply(essay, function(x) 
      gsub("ilink|<br />|href|class|amp|nbsp", " ", x)) 
    plain_essay <-  Corpus(VectorSource(plain_essay))
    plain_essay <- tm_map(plain_essay, PlainTextDocument)
    plain_essay <- tm_map(plain_essay, removeWords, stopwords('english'))
    plain_essay <- tm_map(plain_essay, removePunctuation)
    plain_essay <- tm_map(plain_essay, stemDocument)
    
    return(plain_essay)
  }
  
  #preload word cloud data
  essay_4_m <- essay("4", "m")
  essay_4_f <- essay("4", "f")
  essay_5_m <- essay("5", "m")
  essay_5_f <- essay("5", "f")
  
  output$body_type_plot <- renderPlot({
    
    okcupid <- okcupid[okcupid$diet != "",]
    okcupid <- okcupid[okcupid$body_type != "",]
    
    okcupid$gen_diet <- ifelse(okcupid$diet == "vegetarian" | 
                                 okcupid$diet == "mostly vegetarian" |
                                 okcupid$diet == "strictly vegetarian", "vegetarian",
                               ifelse(okcupid$diet == "vegan" | 
                                        okcupid$diet == "mostly vegan" |
                                        okcupid$diet == "strictly vegan", "vegan", "other"))
    
    okcupid$specific_veg_diet <- ifelse(okcupid$diet == "vegetarian", "vegetarian",
                                        ifelse(okcupid$diet == "strictly vegetarian", "strictly vegetarian",
                                               ifelse(okcupid$diet == "mostly vegetarian", "mostly vegetarian", "other")))
    
    
    
    if (input$diet_type == "All Diets") {
      p <- ggplot(okcupid, (aes(x=okcupid$diet, fill=okcupid$body_type))) + 
        geom_bar(position="fill") +
        ggtitle("Body Type Proportions by Diet") +
        labs(x = "Diet", y = "Proportion of Users") +
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        scale_fill_discrete(name="Body Type")
    }
    
    if (input$diet_type == "Diets Generalized") {
      p <- ggplot(okcupid, (aes(x=okcupid$gen_diet,fill=okcupid$body_type))) + 
        geom_bar(position="fill") +
        ggtitle("Body Type Proportions by Diet") +
        labs(x = "Diet", y = "Proportion of Users") +
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        scale_fill_discrete(name="Body Type")
      
    }
    
    if (input$diet_type == "Vegetarian Diets and Other") {
      p <- ggplot(okcupid, (aes(x=okcupid$specific_veg_diet,
                                fill=okcupid$body_type))) + 
        geom_bar(position="fill") +
        ggtitle("Body Type Proportions by Diet") +
        labs(x = "Diet", y = "Proportion of Users") +
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        scale_fill_discrete(name="Body Type")
      
    }
    
    print(p)
  })
  
  
  output$word_cloud_4 <- renderPlot({
    
    if (input$gender == "Males") {
      wordcloud(essay_4_m, max.words = 100, random.order = FALSE, 
                colors = brewer.pal(8,"Dark2"))
    }
    
    if (input$gender == "Females") {
      wordcloud(essay_4_f, max.words = 100, random.order = FALSE,
                colors = brewer.pal(8,"Dark2"))
    }
    
  })
  
  output$word_cloud_5 <- renderPlot({
    if (input$gend == "Males") {
      wordcloud(essay_5_m, max.words = 100, random.order = FALSE,
                colors = brewer.pal(8,"Dark2"))
    }
    
    if (input$gend == "Females") {
      wordcloud(essay_5_f, max.words = 100, random.order = FALSE,
                colors = brewer.pal(8,"Dark2"))
    }
  })
  
  ##Dorsa--------------------------------------------------------------------------------------------
  
  profiles <- okcupid
  
  profiles$age <- as.numeric(as.character(profiles$age))
  
  profiles.subset <- subset(profiles, (diet == "mostly vegetarian" | diet == "vegetarian" | diet == "strictly vegetarian"))
  
  profiles.subset.nonveg <- subset(profiles, diet == "mostly anything" | diet == "strictly anything" | diet == "anything" | diet == "mostly vegan" | diet == "vegan" | diet == "strictly vegan" | diet == "mostly other" | diet == "strictly other" | diet == "other" | diet == "halal" | diet == "strictly halal" | diet == "mostly halal" | diet == "kosher" | diet == "mostly kosher" | diet == "strictly kosher")
  
  # Combining pet categories for vegetarian dataset
  dogs <- grepl(pattern = "(likes dogs)|(has dogs)", x = profiles.subset$pets)
  
  cats <- grepl(pattern = "(likes cats)|(has cats)" , x = profiles.subset$pets)
  
  both <- dogs & cats
  
  neither <- !dogs & !cats
  
  profiles.subset$pet_categories <- ifelse(both, "likes both", 
                                           ifelse(cats, "likes cats",
                                                  ifelse(dogs, "likes dogs", "likes neither")))
  
  # Combining Pet categories : Same manipulation for nonvegetarian subset
  dogs <- grepl(pattern = "(likes dogs)|(has dogs)", x = profiles.subset.nonveg$pets)
  
  cats <- grepl(pattern = "(likes cats)|(has cats)" , x = profiles.subset.nonveg$pets)
  
  both <- dogs & cats
  
  neither <- !dogs & !cats
  
  
  profiles.subset.nonveg$pet_categories <- ifelse(both, "likes both", 
                                                  ifelse(cats, "likes cats",
                                                         ifelse(dogs, "likes dogs", "likes neither")))
  
  
  # Combining non-vegetarian categories for profiles.subset.nonveg
  vegan <- grepl(pattern = "(mostly vegan)|(vegan)|(strictly vegan)", x = profiles.subset.nonveg$diet)
  
  other <- grepl(pattern = "(other)|(strictly other)|(mostly other)" , x = profiles.subset.nonveg$diet)
  
  anything <- grepl(pattern = "(anything)|(strictly anything)|(mostly anything)" , x = profiles.subset.nonveg$diet)
  
  halal <- grepl(pattern = "(halal)|(strictly halal)|(mostly halal)" , x = profiles.subset.nonveg$diet)
  
  kosher <- grepl(pattern = "(kosher)|(strictly kosher)|(mostly kosher)" , x = profiles.subset.nonveg$diet)
  
  profiles.subset.nonveg$diet_simple <- ifelse(vegan, "vegan", 
                                               ifelse(other, "other", 
                                                      ifelse(halal, "halal", 
                                                             ifelse(kosher, "kosher", "anything"))))
  library(gridExtra)
  output$bar_ageorsex <- renderPlot({
    p <- ggplot(data = profiles.subset, aes(x = diet)) +
      geom_bar(position = position_dodge()) +
      aes(fill = sex) +
      ggtitle("Bar Chart of Gender Given Vegetarian") +
      labs(x = "Diet", y = "Count") +
      scale_fill_manual(values = c("purple", "green4"), guide = guide_legend(title = "Gender"))
    
    q <- ggplot(data = profiles.subset.nonveg, aes(x = diet_simple)) +
      geom_bar(position = position_dodge()) +
      aes(fill = sex) +
      ggtitle("Bar Chart of Gender Given Non Vegetarians") +
      labs(x = "Diet", y = "Count") +
      scale_fill_manual(values = c("purple", "green4"), guide = guide_legend(title = "Gender"))
    
    r <- (ggplot(profiles.subset) + aes(x = age) +
            geom_density() +
            ggtitle("Conditional Distribution of Age Given Vegetarian") +
            labs(x = "Age", y = "Density"))
    
    s <- (ggplot(profiles.subset.nonveg) + aes(x = age) + geom_density() +
            ggtitle("Conditional Distribution of Age Given Non-Vegetarian") +
            labs(x = "Age", y = "Density"))
    
    if (input$sex == "Sex") {
      grid.arrange(p, q, ncol = 2)
    }
    else {
      grid.arrange(r, s, ncol = 2)
      
    }
  })
  
  
  output$pets_plot <- renderPlot({
    pets <- ggplot(data = profiles.subset, 
                   aes(x = pet_categories)) +
      geom_bar() + aes(fill = diet) +
      ggtitle("Bar Chart of Pet Preferences of Vegetarians") +
      labs(x = "Pet Preferences", y = "Count") +
      scale_fill_manual(values = c("purple", "green4", "deepskyblue"), guide = guide_legend(title = "diet"))
    
    
    
    pets.nonveg <- ggplot(data = profiles.subset.nonveg, 
                          aes(x = pet_categories)) +
      geom_bar() + aes(fill = diet_simple) +
      ggtitle("Bar Chart of Pet Preferences of Vegetarians") +
      labs(x = "Pet Preferences", y = "Count") +
      scale_fill_manual(values = c("purple", "green4", "deepskyblue",
                                   "yellow", "tan1"), guide = guide_legend(title = "Diet"))
    
    if (input$pets == "Vegetarians") {
      print(pets)
    }
    else {
      print(pets.nonveg)
    }
    
  })
})