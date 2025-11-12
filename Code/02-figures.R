# Figures WWBI 

#Reproducibility Figures present in this code:

#Figure 1
#Figure 2
#Figure 3
#Figure 4
#Figure 5 
#Figure B1
#Figure B2
#Figure B2
#Figure 12
#Figure 15
#Figure 16 

# 04. Figures

# Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(wbstats)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(lubridate)
library(scales)
library(patchwork)
library(zoo)
library(scales)
library(purrr)
library(forcats)
library(readxl)
library(scales) 
library(stringr)
library(stringi)
library(readr)
library(ggrepel) 
library(officer)

# Load data 

# Paths

data_path_in <- "C:/WBG/GitHub/Syntesis-Report-WWBI-Figures"

data_path_out <- "C:/WBG/GitHub/Syntesis-Report-WWBI-Figures/Outputs/wwbi"

data_path <- "C:/WBG/GitHub/Syntesis-Report-WWBI-Figures"



# Graphs ----

#Figure 1:Public sector employment per 1000 people

#Open data set 

plot_df_f1 <- read_rds(file.path(data_path, "Data/out/plot_df_f1.rds"))


plot_df_f1 <- plot_df_f1 %>%
  mutate(
    inc_rank = case_when(
      incgroup == 4 ~ 1,  # High income
      incgroup == 3 ~ 2,  # Upper middle
      incgroup == 2 ~ 3,  # Lower middle
      incgroup == 1 ~ 4,  # Low income
      TRUE ~ 5
    )
  ) %>%
  arrange(inc_rank, desc(pse_per_1k)) %>%
  mutate(country_order = factor(country_name, levels = unique(country_name)),
         idx = row_number())

# ---  Vertical separators between income groups ---
seps <- plot_df_f1 %>%
  group_by(inc_rank) %>%
  summarise(x = max(idx) + 0.5, .groups = "drop") %>%
  filter(inc_rank != max(inc_rank))   # no line after last group

# --- 3) Keep only target countries for labeling ---
targets_raw <- c(
  "Iceland","Luxembourg","Belgium","Ireland","Austria","Portugal","Lithuania",
  "Slovenia","Czech Republic","Panama","Uruguay","United States of America",
  "China","Albania","Venezuela","Bulgaria","Gabon","Mexico","Ecuador","Peru",
  "Paraguay","Colombia","Moldova","Timor-Leste","Vietnam","Cambodia",
  "Sri Lanka","Ghana","Zambia","Nicaragua","Honduras","India","Cameroon",
  "Bangladesh","Tajikistan","Afghanistan","Sierra Leone","Chad",
  "Rwanda","Madagascar","Burkina Faso"
)

# Label vector
label_vec <- ifelse(plot_df_f1$country_name %in% targets_raw,
                    plot_df_f1$country_name, "")
#  Plot

f1 <- ggplot(plot_df_f1, aes(x = idx, y = pse_per_1k)) +
  geom_point(size = 2.6, color = "#002244") +
  geom_vline(data = seps, aes(xintercept = x),
             linetype = "dashed", color = "grey60", linewidth = 0.6) +
  scale_y_continuous(
    limits = c(0, 160),
    breaks = seq(0, 160, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(
    breaks = plot_df_f1$idx,
    labels = label_vec,
    expand = c(0, 0)
  ) +
  labs(x = NULL, y = "Public sector employment per 1,000 people") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )

f1



#Save plot 

ggsave(file.path(data_path_out, "Figure_1.png"), f1, width = 15, height = 6, dpi = 300)

#Figure 2: share of total public employees

#Load data set 

data_f2 <- read_rds(file.path(data_path, "Data/out/data_f2.rds"))



#  Plot -------------------------------------------------------------------

f2 <- ggplot(data_f2, aes(x = income_level, y = share, fill = category)) +
  geom_col(width = 0.8) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100),
                     expand = expansion(mult = c(0, .02))) +
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

f2

#Save plot 

ggsave(file.path(data_path_out, "Figure_2.png"), f2, width = 10, height = 8, dpi = 300)

#Table

tbl_flex <- data_f2 %>%
  arrange(income_level, desc(share)) %>%
  flextable() %>%
  set_header_labels(
    income_level = "Income level",
    category = "Category",
    share = "Share (%)"
  ) %>%
  colformat_num(col_keys = "share", digits = 1) %>%
  theme_vanilla() %>%
  autofit()

tbl_flex

read_docx() |>
  body_add_flextable(tbl_flex) |>
  print(target = file.path(data_path_out, "Table_figure2.docx"))


#Figure 3:Public sector employment 

# Define a color palette for regions

region_colors <- c(
  "Middle East & North Africa" = "#e41a1c",
  "East Asia & Pacific" = "#ff7f00",
  "Latin America & Caribbean" = "#377eb8",
  "Sub-Saharan Africa" = "#4daf4a",
  "Europe & Central Asia" = "#984ea3",
  "South Asia" = "#a65628"
)


# 1 as a percentage of paid employment

#Load data set

public_sector_emp_paid_f1 <- read_rds(file.path(data_path, "Data/out/public_sector_emp_paid_f1.rds"))

# Now plot

plot_paid <- ggplot(public_sector_emp_paid_f1, aes(x = value_percentage, y = wb_region, color = wb_region)) +
  geom_point(size = 2) +
  scale_color_manual(values = region_colors) +
  labs(x = "", y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# See the graph
plot_paid


# Save the plot

ggsave(file.path(data_path_out, "Figure_3.png"), plot_paid, width = 10, height = 8, dpi = 300)

# 2 as percentage of formal employment

#Load data set 

public_sector_emp_formal <- read_rds(file.path(data_path, "Data/out/public_sector_emp_formal.rds"))

# Now plot

plot_formal <- ggplot(public_sector_emp_formal, aes(x = value_percentage, y = wb_region, color = wb_region)) +
  geom_point(size = 2) +
  scale_color_manual(values = region_colors) +
  labs(x = "", y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# See the graph
plot_formal


# Save the plot

ggsave(file.path(data_path_out, "Figure_3b.png"), plot_formal, width = 10, height = 8, dpi = 300)

#Figure 4: Public sector employment Non FCV and FCV countries

#Load data set 

by_fcv <- read_rds(file.path(data_path, "Data/out/data_fcv.rds"))

# --- Plot -------------------------------------------------------------------

cat_levels <- c(
  "Public sector employment (% of formal employment)",
  "Public sector employment (% of paid employment)",
  "Public sector employment (% of total employment)"
)


by_fcv <- by_fcv %>%
  mutate(fcv_group = factor(fcv_group, levels = c("FCV","Non-FCV")))

f4 <- ggplot(by_fcv, aes(x = mean_share, y = category, fill = fcv_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = scales::percent(mean_share/100, accuracy = 1)),
            position = position_dodge(width = 0.8), hjust = -0.1, size = 3.6) +
  scale_x_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100)) +
  scale_y_discrete(limits = rev(cat_levels)) +         
  scale_fill_manual(values = c("FCV"="#ED7D31","Non-FCV"="#4472C4")) +
  labs(x=NULL, y=NULL, fill=NULL, title=NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position="top", panel.grid.minor=element_blank())

f4

#Save 

ggsave(file.path(data_path_out, "Figure_4.png"), f4, width = 15, height = 8, dpi = 300)

#Figure 5: Log GDP per capita vs Public sector employment

#Load data set

df_plot <- read_rds(file.path(data_path, "Data/out/data_f5.rds"))

#Plot

f5_total <- ggplot(df_plot, aes(x = log_gdp, y = value_percentage)) +
  geom_point(size = 2, color = "#002244", alpha = 0.85, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "red", se = FALSE, na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 50),
                     labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "", x = "Log of GDP per capita 2015", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none")

f5_total

# Save the plot

ggsave(file.path(data_path_out, "Figure_5.png"), f5_total, width = 12, height = 8, dpi = 300)

#Figure B1: indicador es females, as a share of public paid and private paid employment

#Load data set 

df_scatter <- read_rds(file.path(data_path, "Data/out/data_b1.rds"))

# Region colors 

region_cols <- c(
  "East Asia & Pacific"              = "#1f77b4",
  "Europe & Central Asia"            = "#ff7f0e",
  "Latin America & the Caribbean"    = "#2ca02c",
  "Middle East & North Africa"       = "#17becf",
  "North America"                    = "#9467bd",
  "South Asia"                       = "#8c564b",
  "Sub-Saharan Africa"               = "#e0bf00"
)

# Plot: Private on x, Public on y; 45° line; dashed 50% refs

fb1 <- ggplot(df_scatter, aes(x = Private, y = Public, color = wb_region)) +
  geom_point(size = 2, alpha = 0.9) +
  # 45-degree line (y = x)
  geom_abline(slope = 1, intercept = 0, color = "lightblue", linewidth = 1) +
  # dashed reference lines at 50%
  geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  scale_color_manual(values = region_cols, name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(x = "Private sector", y = "Public sector", title = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

fb1

#Save 


ggsave(file.path(data_path_out, "Figure_B1.png"), fb1, width = 12, height = 8, dpi = 300)


#Figure B2: Public sector workforce 

#Load data set

df_plotb2 <- read_rds(file.path(data_path, "Data/out/df_plotb2.rds"))

# --- Colors & factor order (legend) -----------------------------------
sector_cols <- c(
  "Public Administration" = "#93C5F1",  # light blue
  "Education"             = "#ED7D31",  # orange
  "Health"                = "#2E7D32"   # dark green
)

df_plotb2 <- df_plotb2 %>%
  mutate(indicator_name = factor(indicator_name,
                                 levels = c("Public Administration","Education","Health")))

# --- Plot -------------------------------------------------------------
fb2 <- ggplot(df_plotb2, aes(x = log_gdp, y = value_percentage, color = indicator_name)) +
  geom_point(size = 2, alpha = 0.9, na.rm = TRUE) +
  scale_color_manual(values = sector_cols, name = NULL) +
  # X axis: 2.5 to 5.0 by 0.5
  scale_x_continuous(
    limits = c(2.5, 5.0),
    breaks = seq(2.5, 5.0, 0.5),
    labels = number_format(accuracy = 0.1)
  ) +
  # Y axis: -60% to 40% with 10% ticks, formatting as %
  scale_y_continuous(
    limits = c(-60, 40),
    breaks = seq(-60, 40, 10),
    labels = label_percent(scale = 1, accuracy = 1)
  ) +
  labs(
    x = "Log of GDP per capita (constant 2015 USD)",
    y = "Gender wage premium",
    title = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

fb2

# Save the plot
ggsave(file.path(data_path_out, "Figure_B2.png"), fb2, width = 12, height = 8, dpi = 300)

#Figure 12: Public sector wage premium (compared to all private employees)

#Load data set

df_plotf12 <- read_rds(file.path(data_path, "Data/out/df_plotf12.rds"))

#  Plot

f12 <- ggplot(df_plot, aes(x = log_gdp, y = value_percentage, color = wb_region)) +
  geom_point(size = 2, alpha = 0.9) +
  labs(
    x = "Log GDP per capita (Constant 2015 USD)",
    y = "Public sector wage premium",
    title = NULL, color = NULL
  ) +
  # y: -60% to 1200%, ticks every 10%
  scale_y_continuous(
    limits = c(-60, 120),
    breaks  = seq(-60, 120, 20),
    labels  = label_percent(scale = 1, accuracy = 1)
  ) +
  # x: 2.5 to 5.0, ticks every 0.5
  scale_x_continuous(
    limits = c(2.5, 5.0),
    breaks = seq(2.5, 5.0, 0.5)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

f12


#Save

ggsave(file.path(data_path_out, "Figure_12.png"), f12, width = 12, height = 8, dpi = 300)



#Figure 15: Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)

#Load data set 

df_plotf15 <- read_rds(file.path(data_path, "Data/out/df_plotf15.rds"))


# World Bank navy 

wb_blue <- "#0b3954"

#Plot 

f15 <- ggplot(df_plotf15, aes(x = level, y = avg_pct)) +
  geom_col(width = 0.6, fill = wb_blue) +
  geom_text(aes(label = paste0(round(avg_pct, 1), "%")),
            vjust = -0.4, size = 5) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Public wage premium (compared to formal private workers)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 6)),
    plot.margin = margin(10, 20, 10, 20)
  )
f15

#Save

ggsave(file.path(data_path_out, "Figure_15.png"), f15, width = 12, height = 8, dpi = 300)

#Figure 16: pay compression ratio (90/10th percentile) de private sector y de public sector

#Load data set

df_plotf16 <- read_rds(file.path(data_path, "Data/out/df_plotf16.rds"))

# Plot

f16 <- ggplot(df_plotf16, aes(x = income_level, y = mean_ratio, fill = indicator_name)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("Private Sector" = "#4472C4", "Public Sector" = "#ED7D31"), name = NULL) +
  labs(x = NULL, y = "Pay compression ratio (90/10th percentile earners)", title = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

f16

#Save 

ggsave(file.path(data_path_out, "Figure_16.png"), f16, width = 12, height = 8, dpi = 300)

