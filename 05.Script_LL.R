
# Exploration of the CPUE data


# 1. Libraries ------------------------------------------------------------

library(corrplot)
library(GGally)
library(ggplot2)
library(dplyr)
library(ggpubr)

# 2. Load data ------------------------------------------------------------

# catch and effort 
LL= read_excel( "Data/SIOFA_3b_Toothfish_catch_effort-2025.xlsx")

# tagging 
Rel= read_excel( "Data/SIOFA_3b_tagging-2025.xlsx",sheet ='tag_release')
Rec= read_excel( "Data/SIOFA_3b_tagging-2025.xlsx",sheet ='tag_recapture')
Rlink= read_excel( "Data/SIOFA_3b_tagging-2025.xlsx",sheet ='tag_linking')

# link tag fished and set"~/Travail/CC-ORGP/SIOFA/Workshop-Trend analysis 2025/R/SIFOA Workshop Analysis/Data/Marco_SIOFA/SIOFA_all_tag_operation_link-2025_rev1.xlsx"
LinkOP_release= read_excel( "Data/SIOFA_all_tag_operation_link-2025_rev1.xlsx",sheet ='tag_release')
LinkOP_recapture= read_excel( "Data/SIOFA_all_tag_operation_link-2025_rev1.xlsx",sheet ='tag_recapture')

# biological sampling
Bio= read_excel( "Data/SIOFA_3b_fish_sampling-2025.xlsx")


# extract LineLength and catch numbers from other DataSet
LL_LineLength= read_excel("Data/SIOFA-TOP-Data-3b-2025-09-Release_rev2.xlsx",sheet="CatchEffort_rev2")
LL_LineLength_Select = LL_LineLength %>% 
  dplyr::select(HBHfoID, LineLenght, TcatchWeight, TcatchNumber) %>%
  dplyr::rename(opeID = HBHfoID, LineLength = LineLenght) %>%
  mutate(opeID =as.character(opeID))


# Create File with LineLenth and catch numbers
LL=LL %>% left_join(LL_LineLength_Select)


LL$dist_geo = 
  sapply(1:nrow(LL),function(i)
    distGeo(as.data.frame(LL)[i,c('fishopSetStartLongitude','fishopSetStartLatitude')],
            as.data.frame(LL)[i,c('fishopSetEndLongitude','fishopSetEndLatitude')]))


LL_3b = LL %>% mutate(Season = SIOFA_Season(fishopSetStartDate)) %>% 
  filter(Season>=minSeason & Season<=maxSeason & speciesFAOCode %in%c('TOP')) %>% 
  filter(Subarea == '3b') 


LL_3b=assign_areas_rev(Input=LL_3b,
                       NamesIn = c("fishopSetStartLatitude","fishopSetStartLongitude"),
                       Polys = c('MU'),
                       AreaNameFormat=c('name'),
                       Buffer=0, 
                       NamesOut=c('MU'))

dim(LL_3b)

LL_3b=LL_3b %>% filter(MU=="DC") %>%   dplyr::select(HooksSet, LineLength, dist_geo,vesselCode,Catch_Kg)#,TcatchWeight,TcatchNumber ) incomplete data

names(LL_3b)
# Remove missing values
LL_3b <- na.omit(LL_3b)




# 3.Data Exploration -------------------------------------------------------



# Correlation matrix
cor_mat <- cor(LL_3b, method = "pearson")

print(cor_mat)

corrplot(
  cor_mat,
  method = "circle",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)




ggpairs(
  LL_3b,
  columns = c("HooksSet", "LineLength", "dist_geo"),
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(continuous = "smooth"),
  diag = list(continuous = "densityDiag")
)

summary(LL_3b)


cor(LL_3b, method = "spearman")



## 3.1.Correlation line length and dist_geo -----------------------------------
p1 <- ggplot(
  LL_3b,
  aes(LineLength, dist_geo, color = as.factor(vesselCode))
) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    x = "Line length",
    y = "Geographic distance",
    color = "Vessel Code"
  )

p2 <- ggplot(
  LL_3b %>% dplyr::filter(HooksSet < 8000),
  aes(LineLength, HooksSet, color = as.factor(vesselCode))
) +
  geom_point(alpha = 0.6) +
  coord_equal() +
  coord_cartesian(ylim = c(0, 5000)) +
  theme_minimal() +
  labs(
    x = "Line length",
    y = "Number of hooks",
    color = "Vessel Code"
  )

LL_plot <- ggarrange(
  p2, p1,
  ncol = 2,
  labels = c("A", "B"),
  common.legend = TRUE,
  legend = "bottom"
)+ theme(
  legend.title = element_text(face = "bold"),
  legend.text = element_text(size = 9)
)

ggsave(
  filename = "Output/line_length_comparison_3b.png",
  plot = LL_plot,
  width = 10,
  height = 5,
  dpi = 300
)


## 3.2.Exploring Catch vs Line length and HooksSet ----------------------------------------------------------
LL_CPUE= LL_3b %>% mutate(CPUE_Length=Catch_Kg/LineLength,CPUE=Catch_Kg/HooksSet) %>% filter(HooksSet<5000) # two lines

  
  
  p1 <- ggplot(
    LL_CPUE,
    aes(HooksSet,Catch_Kg, color = as.factor(vesselCode))
  ) +
  geom_point(alpha = 0.6) +
  # geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  # geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
    # coord_cartesian(xlim = c(0, 5000)) +
  labs(
    x = "HooksSet",
    y = "Catch_Kg",
    color = "Vessel Code"
  )

  
  p2 <- ggplot(
    LL_CPUE,
    aes(LineLength, Catch_Kg, color = as.factor(vesselCode))
  ) +
    geom_point(alpha = 0.6) +
    coord_equal() +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "black"
    ) +
    theme_minimal() +
    labs(
      x = "Line length",
      y = "Catch_Kg",
      color = "Vessel Code"
    )
  
  LL_plot_CPUE <- ggarrange(
    p2, p1,
    ncol = 2,
    labels = c("A", "B"),
    common.legend = TRUE,
    legend = "bottom"
  )+ theme(
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )
  
  LL_plot_CPUE
  
  ggsave(
    filename = "Output/CPUE_comparison_3b.png",
    plot = LL_plot_CPUE,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  

# 3.3. CPUE Boxplot -------------------------------------------------------

# Plot CPUE
  LL_CPUE_long <- LL_CPUE %>%
    dplyr::rename(
      CPUE_Hooks      = CPUE,
      CPUE_LineLength = CPUE_Length
    ) %>% 
    tidyr::pivot_longer(
      cols = c(CPUE_Hooks, CPUE_LineLength),
      names_to = "CPUE_type",
      values_to = "CPUE_value"
    )
  
  boxplot=ggplot(
    LL_CPUE_long,
    aes(x = as.factor(vesselCode), y = CPUE_value)
  ) +
    geom_boxplot(outlier.shape = NA) +   # remove outliers
    facet_wrap(~ CPUE_type, scales = "fixed") +  # same y-scale
    labs(
      x = "Vessel code",
      y = "CPUE"
    ) +
    ylim(0,0.3)+
    theme_minimal()

  
  ggsave(
    filename = "Output/boxplot_CPUE_comparison_DC.png",
    plot = boxplot,
    width = 10,
    height = 5,
    dpi = 300
  ) 
  
  
# Plot Z-score
  LL_CPUE %>%
    group_by(vesselCode) %>%
    dplyr::summarise(
      mean_CPUE = mean(CPUE, na.rm = TRUE)
    ) %>%
    mutate(
      z_CPUE = (mean_CPUE - mean(mean_CPUE,na.rm=TRUE)) / sd(mean_CPUE)
    ) %>%
    ggplot(aes(x = reorder(as.factor(vesselCode), z_CPUE), y = z_CPUE)) +
    geom_col(fill = "grey70") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", colour = "red") +
    labs(
      x = "Vessel code",
      y = "Standardised CPUE (z-score)",
      title = "Relative CPUE by vessel"
    ) +
    theme_minimal()


  LL_CPUE_long <-   LL_CPUE %>%
    dplyr::rename(
      CPUE_Hooks      = CPUE,
      CPUE_LineLength = CPUE_Length
    ) %>% 
    tidyr::pivot_longer(
      cols = c(CPUE_Hooks, CPUE_LineLength),
      names_to = "CPUE_type",
      values_to = "CPUE_value"
    ) %>%
    group_by(CPUE_type) %>%
    mutate(
      CPUE_scaled = (CPUE_value - mean(CPUE_value, na.rm = TRUE)) /
        sd(CPUE_value, na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  boxplot_Z=ggplot(
    LL_CPUE_long,
    aes(x = as.factor(vesselCode), y = CPUE_scaled)
  ) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~ CPUE_type, scales = "fixed") +
    labs(
      x = "Vessel code",
      y = "Standardised CPUE (z-score)"
    ) +
    ylim(-1.5,3)+
    theme_minimal()
  
  ggsave(
    filename = "Output/boxplot_Z_CPUE_comparison_DC.png",
    plot = boxplot_Z,
    width = 10,
    height = 5,
    dpi = 300
  )   
  