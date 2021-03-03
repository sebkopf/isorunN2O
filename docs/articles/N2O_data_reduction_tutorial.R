## ---- message=FALSE, warning=FALSE---------------------------------------
library(isorunN2O)

## ------------------------------------------------------------------------
data_folder <- system.file("extdata", package = "isorunN2O") 
iso_files <- load_run_folder(file.path(data_folder, "test_run"))

## ------------------------------------------------------------------------
names(iso_files) %>% head(n=5) 

## ---- message=FALSE, warning = FALSE, fig.width = 9, fig.height = 7------
iso_files[["MAT25392080_P02E_run02_Conditioner-0000.dxf"]]$make_ggplot()

## ------------------------------------------------------------------------
df.raw <- iso_files %>%
  # pull out the data summary from the raw isodat file:
  get_isodat_data_tables() %>% 
  # derive file categories:
  parse_file_names() %>% 
  # discard the reference peaks:
  select_N2O_peak( c(360, 370)) %>% 
  # focus on the columns we care about:
  rename(d45 = `d 45N2O/44N2O`, d46 = `d 46N2O/44N2O`, area = `Intensity All`) %>% 
  # select which columns to keep:
  select_columns(folder, date, analysis, run_number, category, name, volume, area, d45, d46) 

## ------------------------------------------------------------------------
df.raw %>% head(n=5) 

## ------------------------------------------------------------------------
df.raw %>% group_by(category) %>% generate_data_table(area, d45, d46) %>% knitr::kable()
df.raw %>% group_by(category, name) %>% generate_data_table(area, d45, d46, cutoff = 3)%>% knitr::kable()

## ----first_look_at_data, fig.width = 9, fig.height = 9-------------------
df.raw %>% plot_overview(d45)

## ----first_look_more_details, fig.width = 9, fig.height = 7--------------
df.raw %>% plot_overview(
  d45, size = area, 
  color = ifelse(category %in% c("IAEA-NO3", "USGS-34"), name, category), 
  panel = factor(category, levels = c("N2O", "IAEA-NO3", "USGS-34")))

## ---- cache = FALSE, fig.width = 9, fig.height = 7-----------------------
make_interactive() 

## ----cat_assignments-----------------------------------------------------
df.cat <- df.raw %>% 
  change_category(name %in% c("IAEA-NO3 37 uM ctrl", "USGS-34 37 uM ctrl"), "control") %>%
  change_category(run_number == 68, "excluded") %>%
  change_category(name == "LNSW Blank", "blank")

## ----drift_correction, fig.width = 9, fig.height = 7---------------------
df.drift <- df.cat %>% 
  evaluate_drift(d45, d46, correct = TRUE, plot = TRUE,
                correct_with = category %in% c("USGS-34", "IAEA-NO3", "N2O"),
                method = "loess")

## ----data_overview_after_drift_correction, fig.width = 9, fig.height = 7----
df.drift %>% plot_overview(d45.drift, panel = factor(category, levels = c("N2O", "IAEA-NO3", "USGS-34")))

## ------------------------------------------------------------------------
df.O17 <- df.drift %>%
  correct_N2O_for_17O(d45.drift, d46.drift) %>% 
  select_columns(-d45, -d45.drift, -d46, -d46.drift) # no longer needd, remove these columns

