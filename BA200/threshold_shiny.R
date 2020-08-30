# Academic Innovation
# Tailoring Thresholds: 
#   Check how many students meet complex tailoring thresholds (BA 200)
# Data: BA200_2260_beginning_of_term_getting_started_survey_responses.csv
#
# Author: Amy (Ming-Chen) Lu
# Updated: Mar 10, 2020
#80: ---------------------------------------------------------------------------
# Libraries: -------------------------------------------------------------------
library(tidyverse)

# Read in data: ----------------------------------------------------------------
#setwd("/Users/Amy/Desktop/AI/tandem/BA200")
file = read_csv("https://github.com/mclu/AI/raw/master/BA200_F19_BOT.csv")
welcome_res = read_csv("https://github.com/mclu/AI/raw/master/BA200_welcome_responses.csv")

# Data Preparation: ------------------------------------------------------------
names(file) = str_remove(names(file), "ba200__")
BT_PastCount = rowSums(file[, 14:19] == 4)
welcome_res = welcome_res %>% 
  transmute(user_id, res = ifelse(
    ba200__WelcomeInterest == 1 | ba200__WelcomeUsefulRating == 1, 1, 0 ))

vars = c("user_id", "BT_Concerns", "Extraversion", "BT_Belongingness", "SpeakUp",
         "Procrastination", "Control", "BT_StretchGrade", "Gender", 
         "BT_Orientation", "GroupPreference")
data = cbind(file[, vars], BT_PastCount)
data = left_join(data, welcome_res, by = "user_id") %>%
  mutate(BT_Concerns = 
           str_split(str_replace_all(data$BT_Concerns, '\\[|\\]', ''), ",")) %>%
  filter(Gender == "Female" | Gender == "Male")

# Define UI: -------------------------------------------------------------------
ui = fluidPage(navbarPage(
  "BA200 F19 - Tailoring Threshold",
  
  # Goal 1: --------------------------------------------------------------------
  tabPanel(
    # tab title
    "Goal1",
    # content title
    h2("Find you voice"),
    br(),
    
    fluidRow(
      column(4, 
             sliderInput("ext1", "Extraversion", min = 1, max = 7, value = c(1,3)),
             sliderInput("bel1", "BT_Belongingness", min = 1, max = 7, value = c(1,3)),
             sliderInput("spe1", "SpeakUp", min = 1, max = 7, value = c(1,3)),
             sliderInput("pc1", "BT_PastCount", min = 0, max = 7, value = c(3,7))
      ),
      column(8,
             plotOutput("g1")
      )
    ),
    h5("When the criteria relax to (Extraversion <= 4, BT_Belongingness <= 4,
       SpeakUp <= 4, BT_PastCount >= 2), the counts become 61 students.")
  ),
  
  # Goal 2: --------------------------------------------------------------------
  tabPanel(
    # tab title
    "Goal2",
    h2("Think of this as a learning opportunity"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("ext2", "Extraversion", min = 1, max = 7, value = c(1,3)),
             sliderInput("bel2", "BT_Belongingness", min = 1, max = 7, value = c(1,3)),
             sliderInput("spe2", "SpeakUp", min = 1, max = 7, value = c(1,3)),
             sliderInput("pc2", "BT_PastCount", min = 0, max = 7, value = c(0,3)),
             sliderInput("sg2", "BT_StretchGrade", min = 1, max = 7, value = c(5,7))
      ),
      column(8,
             plotOutput("g2")
      )
    ),
    h5("When the criteria relax to (Extraversion <= 4, BT_Belongingness <= 4,
       SpeakUp <= 4, BT_PastCount <= 4, BT_StretchGrade >= 4), 
       the counts become 61 students.")
  ),
  # Goal 3: --------------------------------------------------------------------
  tabPanel(
    "Goal3",
    h2('Spend more time in learning mode and less time in "I do not know how"'),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("ext3", "Extraversion", min = 1, max = 7, value = c(1,3)),
             sliderInput("bel3", "BT_Belongingness", min = 1, max = 7, value = c(1,3)),
             sliderInput("spe3", "SpeakUp", min = 1, max = 7, value = c(1,3)),
             sliderInput("pc3", "BT_PastCount", min = 0, max = 7, value = c(0,3)),
             sliderInput("sg3", "BT_StretchGrade", min = 1, max = 7, value = c(1,3))
      ),
      column(8,
             plotOutput("g3")
      )
    ),
    h5("When the criteria relax to (Extraversion <= 4, BT_Belongingness <= 4,
       SpeakUp <= 4, BT_PastCount <= 4, BT_StretchGrade <= 4), 
       the counts become 57 students.")
  ),
  # Goal 4: --------------------------------------------------------------------
  tabPanel(
    "Goal4",
    h2("Lead by stepping back"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("ext4", "Extraversion", min = 1, max = 7, value = c(5,7)),
             sliderInput("spe4", "SpeakUp", min = 1, max = 7, value = c(5,7)),
             sliderInput("con4", "Control", min = 1, max = 7, value = c(5,7))
      ),
      column(8,
             plotOutput("g4")
      )
    )
  ),
  # Goal 5: --------------------------------------------------------------------
  tabPanel(
    "Goal5",
    h2("Advocate for equity"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("ext5", "Extraversion", min = 1, max = 7, value = c(5,7)),
             sliderInput("spe5", "SpeakUp", min = 1, max = 7, value = c(5,7)),
             sliderInput("con5", "Control", min = 1, max = 7, value = c(1,3))
      ),
      column(8,
             plotOutput("g5")
      )
    )
  ),
  # Goal 6: --------------------------------------------------------------------
  tabPanel(
    "Goal6",
    h2("Set yourself up for success"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("pro6", "Procrastination", min = 1, max = 7, value = c(1,3)),
             sliderInput("con6", "Control", min = 1, max = 7, value = c(1,3)),
             sliderInput("pc6", "BT_PastCount", min = 0, max = 7, value = c(0,3)),
             h5('BT_Concerns includeds "PeerHarm"')
      ),
      column(8,
             plotOutput("g6")
      )
    ),
    h5("When the criteria relax to (Procrastination <= 4, Control <= 5,
       BT_PastCount <= 4, the counts become 57 students.")
  ),
  # Goal 7: --------------------------------------------------------------------
  tabPanel(
    "Goal7",
    h2("Set yourself up to be a good teammate"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("pro7", "Procrastination", min = 1, max = 7, value = c(1,3)),
             sliderInput("con7", "Control", min = 1, max = 7, value = c(1,3)),
             sliderInput("pc7", "BT_PastCount", min = 0, max = 7, value = c(0,3)),
             h5('BT_Orientation == "Groupwork"')
      ),
      column(8,
             plotOutput("g7")
      )
    ),
    h5("When the criteria relax to (Procrastination <= 5, Control <= 5,
       BT_PastCount <= 4, the counts become 55 students.")
  ),
  # Goal 8: --------------------------------------------------------------------
  tabPanel(
    "Goal8",
    h2("Find the value in your team"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("con8", "Control", min = 1, max = 7, value = c(5,7)),
             sliderInput("pc8", "BT_PastCount", min = 0, max = 7, value = c(3,7)),
             sliderInput("sg8", "BT_StretchGrade", min = 1, max = 7, value = c(1,3)),
             h5('GroupPreference == "Alone"'),
             h5('BT_Concerns does not contain "Social", "SelfDoubt", "BeingHeard","PeerHarm"')
      ),
      column(8,
             plotOutput("g8")
      )
    ),
    h5("When the criteria relax to (Control >= 3, BT_PastCount <= 2, 
       BT_StretchGrade <= 5), the counts become 66 students.")
  ),
  # Goal 9: --------------------------------------------------------------------
  tabPanel(
    # tab title
    "Goal9",
    h2("Take the time to be thoughtful"),
    br(),
    
    fluidRow(
      column(4,
             sliderInput("con9", "Control", min = 1, max = 7, value = c(3.99, 4.01)),
             sliderInput("sg9", "BT_StretchGrade", min = 1, max = 7, value = c(3.99, 4.01)),
             sliderInput("spe9", "SpeakUp", min = 1, max = 7, value = c(3.99, 4.01)),
             h5('WelcomeInterest == 1 or WelcomeUsefulRating == 1')
      ),
      column(8,
             plotOutput("g9")
      )
    )
  )
)
)



# Define server: ---------------------------------------------------------------
server = function(input, output, session){
  
  # data input: ----------------------------------------------------------------
  data_input = reactive({
    df = data.frame(Gender = c("Female", "Male"), matrix(NA, ncol = 9))
    colnames(df)[2:10] = paste0("G", 1:9) 
    
    # Find your voice
    df[,2] = data %>%
      filter(Extraversion >= input$ext1[1], Extraversion <= input$ext1[2],
             BT_Belongingness >= input$bel1[1], BT_Belongingness <= input$bel1[2], 
             SpeakUp >= input$spe1[1], SpeakUp <= input$spe1[2], 
             BT_PastCount >= input$pc1[1], BT_PastCount <= input$pc1[2]) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Think of this as a learning opportunity
    df[,3] = data %>%
      filter(Extraversion >= input$ext2[1], Extraversion <= input$ext2[2],
             BT_Belongingness >= input$bel2[1], BT_Belongingness <= input$bel2[2], 
             SpeakUp >= input$spe2[1], SpeakUp <= input$spe2[2], 
             BT_PastCount >= input$pc2[1], BT_PastCount <= input$pc2[2],
             BT_StretchGrade >= input$sg2[1], BT_StretchGrade <= input$sg2[2]) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Spend more time in learning mode and less time in "I don't know how"
    df[,4] = data %>%
      filter(Extraversion >= input$ext3[1], Extraversion <= input$ext3[2],
             BT_Belongingness >= input$bel3[1], BT_Belongingness <= input$bel3[2], 
             SpeakUp >= input$spe3[1], SpeakUp <= input$spe3[2], 
             BT_PastCount >= input$pc3[1], BT_PastCount <= input$pc3[2],
             BT_StretchGrade >= input$sg3[1], BT_StretchGrade <= input$sg3[2]) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Lead by stepping back
    df[,5] = data %>%
      filter(Extraversion >= input$ext4[1], Extraversion <= input$ext4[2],
             SpeakUp >= input$spe4[1], SpeakUp <= input$spe4[2], 
             Control >= input$con4[1], Control <= input$con4[2]) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Advocate for equity
    df[,6] = data %>%
      filter(Extraversion >= input$ext5[1], Extraversion <= input$ext5[2],
             SpeakUp >= input$spe5[1], SpeakUp <= input$spe5[2], 
             Control >= input$con5[1], Control <= input$con5[2]) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Set yourself up for success
    df[,7] = data %>%
      mutate(Concerns = lapply(BT_Concerns, function(x) sum(grepl("PeerHarm", x)))) %>%
      filter(BT_PastCount >= input$pc6[1], BT_PastCount <= input$pc6[2],
             Control >= input$con6[1], Control <= input$con6[2], 
             Procrastination >= input$pro6[1], Procrastination <= input$pro6[2], 
             Concerns == 1) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Set yourself up to be a good teammate
    df[,8] = data %>%
      filter(BT_PastCount >= input$pc7[1], BT_PastCount <= input$pc7[2],
             Control >= input$con7[1], Control <= input$con7[2], 
             Procrastination >= input$pro7[1], Procrastination <= input$pro7[2],
             BT_Orientation == "Groupwork") %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Find the value in your team
    df[,9] = data %>%
      mutate(Concerns = lapply(BT_Concerns, 
                               function(x) sum(grepl("Social|SelfDoubt|BeingHeard|PeerHarm", x)))) %>%
      filter(BT_PastCount >= input$pc8[1], BT_PastCount <= input$pc8[2],
             BT_StretchGrade >= input$sg8[1], BT_StretchGrade <= input$sg8[2],
             Control >= input$con8[1], Control <= input$con8[2], 
             Concerns == 0, GroupPreference == "Alone") %>%
      summarise(cnt = n()) %>% select(cnt)
    
    # Take the time to be thoughtful
    df[,10] = data %>%
      filter(SpeakUp >= input$spe9[1], SpeakUp <= input$spe9[2], 
             BT_StretchGrade >= input$sg9[1], BT_StretchGrade <= input$sg9[2],
             Control >= input$con9[1], Control <= input$con9[2], 
             res == 1) %>%
      group_by(Gender) %>%
      summarise(cnt = n()) %>% select(cnt)

    # ggplot
    totals = df %>% pivot_longer(cols = 2:10) %>% 
      group_by(name) %>% summarise(total = sum(value))
    
    plot = 
      df %>% 
      pivot_longer(cols = 2:10) %>%
      ggplot(aes(x = name, y = value, fill = Gender)) +
      geom_bar(stat = "identity") +
      geom_text(aes(name, total+5, label = total, fill = NULL), data = totals) +
      ylab("Counts") + labs(fill = "Survey") + xlab("Shorthand") +
      scale_fill_manual(values = c("#F2c649", "#00285E")) +
      theme_bw()
    
    return(plot)
    
  })
  
  # output: --------------------------------------------------------------------
  output$g1 = renderPlot({ data_input() })
  output$g2 = renderPlot({ data_input() })
  output$g3 = renderPlot({ data_input() })
  output$g4 = renderPlot({ data_input() })
  output$g5 = renderPlot({ data_input() })
  output$g6 = renderPlot({ data_input() })
  output$g7 = renderPlot({ data_input() })
  output$g8 = renderPlot({ data_input() })
  output$g9 = renderPlot({ data_input() })
}

# Run the shiny app: -----------------------------------------------------------
shinyApp(ui, server, options = list(width = "100%", height = 650))


# Deploy shiny app
rsconnect::deployApp("/Users/Amy/Desktop/AI/tandem/BA200/BA200_TailoringThreshold")


# Default counts: --------------------------------------------------------------
df = data.frame(Gender = c("Female", "Male"), matrix(NA, ncol = 9))
colnames(df)[2:10] = paste0("G", 1:9) 

# Find your voice
df[,2] = data %>%
  filter(Extraversion <= 3, BT_Belongingness <= 3, 
         SpeakUp <= 3, BT_PastCount >= 3) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Think of this as a learning opportunity
df[,3] = data %>%
  filter(Extraversion <= 3, BT_Belongingness <= 3, SpeakUp <= 3, 
         BT_PastCount <= 3, BT_StretchGrade >= 5) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Spend more time in learning mode and less time in "I don't know how"
df[,4] = data %>%
  filter(Extraversion <= 3, BT_Belongingness <= 3, SpeakUp <= 3, 
         BT_PastCount <= 3, BT_StretchGrade <= 3) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Lead by stepping back
df[,5] = data %>%
  filter(Extraversion >= 5, SpeakUp >= 5, Control >= 5) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Advocate for equity
df[,6] = data %>%
  filter(Extraversion >= 5, SpeakUp >= 5, Control <= 3) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Set yourself up for success
df[,7] = data %>%
  mutate(Concerns = lapply(BT_Concerns, function(x) sum(grepl("PeerHarm", x)))) %>%
  filter(BT_PastCount <= 3, Control <= 3, 
         Procrastination <= 3, Concerns == 1) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Set yourself up to be a good teammate
df[,8] = data %>%
  filter(BT_PastCount <= 3, Control <= 3, 
         Procrastination <= 3, BT_Orientation == "Groupwork") %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

# Find the value in your team
df[,9] = data %>%
  mutate(Concerns = lapply(BT_Concerns, 
                           function(x) sum(grepl("Social|SelfDoubt|BeingHeard|PeerHarm", x)))) %>%
  filter(BT_PastCount >= 3, BT_StretchGrade <= 3, Control >= 5,
         Concerns == 0, GroupPreference == "Alone") %>%
  summarise(cnt = n()) %>% select(cnt)

# Take the time to be thoughtful
df[,10] = data %>%
  filter(SpeakUp == 4, BT_StretchGrade == 4, Control == 4, res == 1) %>%
  group_by(Gender) %>%
  summarise(cnt = n()) %>% select(cnt)

totals = df %>% pivot_longer(cols = 2:10) %>% 
  group_by(name) %>% summarise(total = sum(value))

df %>% pivot_longer(cols = 2:10) %>%
  ggplot(aes(x = name, y = value, fill = Gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(name, total+5, label = total, fill = NULL), data = totals) +
  ylab("Counts") + labs(fill = "Survey") + xlab("Shorthand") +
  scale_fill_manual(values = c("#F2c649", "#00285E")) +
  theme_bw()
