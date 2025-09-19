#' Generate statistical plots with tests, post-hoc results, and compact letter displays
#'
#' This function creates boxplots or mean plots for one or more response variables,
#' performs the specified statistical tests, and optionally adds post-hoc results,
#' compact letter displays (CLD), and annotations. It can also arrange plots into a grid,
#' save plots to files, and export statistical summaries to a PDF.
#'
#' @importFrom dplyr group_by mutate arrange desc ungroup filter summarise slice left_join bind_rows select all_of rename
#' @importFrom rlang as_name ensym .data :=
#' @importFrom forcats fct_inorder
#' @importFrom tibble tibble
#' @importFrom multcompView multcompLetters2
#' @importFrom broom tidy
#' @importFrom FSA dunnTest
#' @importFrom ggplot2 ggplot geom_boxplot geom_jitter scale_y_continuous theme geom_text aes
#' @importFrom ggplot2 scale_x_discrete scale_fill_manual geom_errorbar geom_point geom_segment expansion annotate element_text element_blank ggsave labs scale_x_continuous scale_y_discrete element_rect margin
#' @importFrom rcompanion cldList
#' @importFrom scales label_wrap
#' @importFrom cowplot plot_grid ggdraw draw_plot draw_label
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra grid.arrange arrangeGrob tableGrob ttheme_default
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom stats as.formula aov pairwise.t.test TukeyHSD kruskal.test t.test sd wilcox.test
#' @importFrom utils head
#' @importFrom ggnewscale new_scale_fill
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom patchwork wrap_plots
#' @importFrom biostat make_cld
#' @importFrom grDevices pdf dev.off
#'
#' @param data A data.frame.
#' @param response Character vector of response variables.
#' @param factor Character string for the factor variable.
#' @param method The statistical test to perform.
#'   Available options are:
#'   - `"aov"` or `"anova"`: Analysis of Variance (ANOVA). TukeyHSD as default posthoc test (pairwise t.test with an adjustment method).
#'   - `"kw"` or `"kruskal"` or `"kruskal-wallis"`: Kruskal-Wallis test. Dunn's test as default posthoc test.
#'   - `"t.test"` or `"student"` or `"t"`: Student's t-test.
#'   - `"wilcox"` or `"wilcox.test"` or `"wilcoxon"` or `"mann-whitney"`: Wilcoxon test.
#' @param adjust The p-value adjustment method to apply in post-hoc tests.
#'   Available options are `"holm"`, `"hochberg"`, `"bonferroni"`, `"bh"`, `"by"`, `"fdr"`, `"hommel"` (for aov), `"sidak"` (for kw), `"hs"` (for kw), and `"none"`. Default is `"none"`.
#' @param var.method Named character vector specifying the statistical test to use for each response variable.
#'   The names must match exactly the variable names provided in `response`,
#'   and the values must be valid options accepted for the `method` argument.
#' @param var.adjust Named character vector specifying the p-value adjustment method to use for each response variable.
#'   The names must match exactly the variable names provided in `response`,
#'   and the values must be valid options accepted for the `adjust` argument.
#' @param flip Logical. If `TRUE`, the axes are inverted: the `factor` variable is plotted on the y-axis, and the `response` variables are plotted on the x-axis. Default is `FALSE`.
#' @param filter.na Logical. If `TRUE`, rows with `NA` values in the current response variable are removed before analysis. Default is `FALSE`.
#' @param group.order Optional character vector specifying the order of the `x`-axis categories in the plot. Values must exactly match the factor levels in `x`.
#' @param print.test Logical. If `TRUE`, prints the test summary results to the console. Default is `FALSE`.
#' @param export.test Logical. If `TRUE`, exports statistical test details (test table, post-hoc, CLD) to a PDF report. Default is `FALSE`.
#' @param show.test Logical. If `TRUE`, the statistical test name and p-value are displayed in the top-left corner. Default is `FALSE`.
#' @param show.ph Logical. If `TRUE`, the post-hoc test name and p-value are displayed in the top-left corner. Default is `FALSE`.
#' @param show.letters Logical. If `TRUE`, significance letters for all groups are displayed (only for ANOVA and Kruskal-Wallis). Default is `FALSE`.
#' @param show.brackets Logical. If `TRUE` when `show.letters` is `TRUE`, a bracket is displayed for adjacent groups sharing the same letter. Default is `FALSE`.
#' @param plot.type Character string specifying the type of plot to produce. Either `"boxplot"` for box-and-whisker plots or `"meanplot"` for plots with group means and error bars. Default is `"boxplot"`.
#' @param fill Optional character. Specifies which plot element is filled with colors based on the factor variable. Default is `FALSE`.
#'   - With `plot.type = "boxplot"`:
#'       - `"box"`: fills the boxes according to the factor levels.
#'       - `"point"`: fills the points according to the factor levels.
#'       - `"both"`: fills both boxes and points independently.
#'   - With `plot.type = "meanplot"`:
#'       - `"point"`: fills the points according to the factor levels.
#'   - If `FALSE`, no elements are colored by factor.
#' @param fill.box Optional. Depending on the `fill` argument, is either a single color (e.g., `"white"`) applied to all boxes, a character vector of colors (length matching the number of factor levels), or the name of a ColorBrewer palette to fill the boxes. Default is `"white"`.
#' @param box.alpha Optional numeric between 0 and 1. Specifies the transparency (alpha) of the box fill when `plot.type = "boxplot"`. Default is `1` (fully opaque).
#' @param fill.point Optional. Depending on the `fill` argument, is either a single color (e.g., `"black"`) applied to all points, a character vector of colors (length matching the number of factor levels), or the name of a ColorBrewer palette to fill the points. Default is `"white"`.
#' @param point.alpha Optional numeric between 0 and 1. Specifies the transparency (alpha) of the points. Default is `1` (fully opaque).
#' @param fill.mean Optional. A single color used to fill all mean points when `plot.type = "meanplot"`. Default is `"black"`.
#' @param mean.alpha Optional numeric between 0 and 1. Specifies the transparency (alpha) of the mean points when `plot.type = "meanplot"`. Default is `1` (fully opaque).
#' @param log Logical. If `TRUE`, y-axis breaks and limits are adjusted to properly accommodate data values that may be negative or positive as a result of a logarithmic transformation. Default is `FALSE`.
#' @param group.label Optional character vector specifying the labels to display on the x-axis.
#' @param show.legend Logical. If `TRUE`, the plot legend is displayed. Default is `FALSE`.
#' @param point.shape Optional numeric. Specifies the shape of the points in the plot. Default is `21`.
#' @param mean.shape Optional numeric. Specifies the shape of the mean points in the plot. Default is `21`.
#' @param point.size Optional numeric. Specifies the size of the points in the plot. Default is `2.5`.
#' @param mean.size Optional numeric. Specifies the size of the mean points in the plot. Default is `2.5`.
#' @param jitter.width Optional numeric. Specifies the amount of horizontal jitter applied to points. Default is `0.2`.
#' @param error.width Optional numeric. Specifies the width of the error bars when `plot.type = "meanplot"`. Default is `0.2`.
#' @param error.alpha Optional numeric between 0 and 1. Specifies the transparency (alpha) of the error bars when `plot.type = "meanplot"`. Default is `1` (fully opaque).
#' @param axis.title.x Optional character string to rename the x-axis title. If not provided, the default x variable name is used.
#' @param axis.title.y Optional character string to rename the y-axis title. If not provided, the default y variable name is used.
#' @param axis.breaks.size Optional numeric. Specifies the size of all axis breaks text. Default is `12`.
#' @param axis.title.size Optional numeric. Specifies the size of the axis title text. Default is `12`.
#' @param axis.title.face Optional character. Specifies the font face (e.g., `"plain"`, `"bold"`, `"italic"`) for both axis titles. Default is `"plain"`.
#' @param label.angle Optional numeric. Angle of the axis tick labels. Default is `0`.
#' @param label.face Optional character. Font face for the axis tick labels. Default is `"plain"`.
#' @param letters.face Optional character. Specifies the font face for all significance letters. Default is `"plain"`.
#' @param letters.size Optional numeric. Specifies the size of all significance letters. Default is `5`.
#' @param text.family Optional character. Specifies the font family for all text elements in the plot. Default is `"sans"`.
#' @param theme.style Character. Specifies the ggplot2 theme style preset to apply to the plot.
#'   Options include `"classic"`, `"bw"`, `"minimal"`, `"light"`, `"dark"`, `"gray"`, and `"void"`. Default is `"classic"`.
#' @param padding Optional numeric. Proportion of extra space added to the top of the y-axis to avoid clipping letters/brackets. Default is `0.2`.
#' @param grid Optional logical. If `TRUE`, arranges all response variable plots into a combined grid layout. Default is `FALSE`.
#' @param grid.axis Optional logical. If `TRUE` and `grid = TRUE`, individual plot axes are hidden and shared x and y axes are displayed for the entire grid. Default is `FALSE`.
#' @param ncol Integer. Number of columns in the combined grid plot.
#' @param nrow Integer. Number of rows in the combined grid plot.
#' @param grid.axis.x Optional character string to name the shared x-axis title. Default is `factor`.
#' @param grid.axis.y Optional character string to name the shared y-axis title. Default is `"Concentration"`.
#' @param x.title.shift Optional numeric. Horizontal shift applied to the shared x-axis label in grid layouts. Default is `0.02`.
#' @param var.size Optional numeric. Size of the labels displaying the name of each individual plot in the combined grid. Default is `18`.
#' @param var.face Optional character. Specifies the font face of the labels displaying the name of each individual plot in the combined grid. Default is `"bold"`.
#' @param save.plot Optional logical. If `TRUE`, saves each individual plot as a png file in the working directory. Default is `FALSE`.
#' @param by.var Optional logical. Used only if `save.plot = TRUE`. If `TRUE`, saves each plot individually in separate folders named after each response variable. Default is `FALSE`.
#' @param plot.height Optional numeric. Height of the saved plot image in inches when `save.plot = TRUE`. Default is `5`.
#' @param plot.width Optional numeric. Width of the saved plot image in inches when `save.plot = TRUE`. Default is `5`.
#' @param plot.name Optional character. Name of the saved plot file. Defaults to `"plot_{var}.png"`, where `{var}` is replaced by the response variable name when saving multiple plots.
#' @param save.grid Optional logical. If `TRUE`, saves the combined grid as a JPEG file in the working directory. Default is `FALSE`.
#' @param grid.name Optional character. Name of the saved combined grid plot file. Default is `"grid.png"`.
#' @param grid.height Optional numeric. Height of the saved combined grid plot image in inches when `save.grid = TRUE`. Default is `8`.
#' @param grid.width Optional numeric. Width of the saved combined grid plot image in inches when `save.grid = TRUE`. Default is `7`.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 10),
#'   value1 = c(rnorm(10, 5), rnorm(10, 6), rnorm(10, 7)),
#'   value2 = c(rnorm(10, 3), rnorm(10, 4), rnorm(10, 5))
#' )
#'
#' # Basic boxplot with ANOVA and significance letters
#' get.plot(
#'   data = df,
#'   response = "value1",
#'   factor = "group",
#'   method = "aov",
#'   group.order = c("A", "B", "C"),
#'   show.letters = TRUE,
#'   plot.type = "boxplot"
#' )
#'
#' # Mean plot with Kruskal-Wallis test and error bars
#' get.plot(
#'   data = df,
#'   response = "value2",
#'   factor = "group",
#'   method = "kw",
#'   group.order = c("A", "B", "C"),
#'   plot.type = "meanplot",
#'   fill = "point",
#'   show.test = TRUE
#' )
#'
#' # Multiple response variables arranged in a grid
#' get.plot(
#'   data = df,
#'   response = c("value1", "value2"),
#'   factor = "group",
#'   method = "aov",
#'   group.order = c("A", "B", "C"),
#'   grid = TRUE,
#'   nrow = 1,
#'   ncol = 2,
#'   grid.axis = TRUE,
#'   grid.axis.x = "Experimental group",
#'   grid.axis.y = "Measured value"
#' )
#'
#' @return
#' - If `grid = TRUE`: returns a single `ggplot` object representing the combined grid of all response variable plots.
#' - If `grid = FALSE`: the function invisibly prints each plot to the current graphics device and stores them in internal lists (`plots`, `plots1`) for grid construction if needed.
#' - If `export.test = TRUE`: in addition to plots, a PDF file (`Stats_Report.pdf`) is created in the `"elements"` folder containing tables of statistical test results, post-hoc results, and compact letter displays (CLD).
#' - If `save.plot = TRUE` or `save.grid = TRUE`: plots are saved to file as PNG images in the specified location.
#'
#' In all cases, the function is primarily used for its side effects (plots and exports). The main value returned is either a grid `ggplot` object (if `grid = TRUE`) or `NULL` invisibly.
#' @export

get.plot <- function(
    data,
    response,
    factor,
    method = NULL,
    adjust = NULL,
    var.method = NULL,
    var.adjust = NULL,
    flip = FALSE,
    filter.na = FALSE,
    group.order = NULL,
    print.test = FALSE,
    export.test = FALSE,
    show.test = FALSE,
    show.ph = FALSE,
    show.letters = FALSE,
    show.brackets = FALSE,
    plot.type = "boxplot",
    fill = FALSE,
    fill.box = "white",
    box.alpha = 1,
    fill.point = "white",
    point.alpha = 1,
    fill.mean = "black",
    mean.alpha = 1,
    log = FALSE,
    group.label = NULL,
    show.legend = FALSE,
    point.shape = 21,
    mean.shape = 21,
    point.size = 2.5,
    mean.size = 2.5,
    jitter.width = 0.2,
    error.width = 0.2,
    error.alpha = 1,
    axis.title.x = NULL,
    axis.title.y = NULL,
    axis.breaks.size = 12,
    axis.title.size = 12,
    axis.title.face = "plain",
    label.angle = 0,
    label.face = "plain",
    letters.face = "plain",
    letters.size = 5,
    text.family = "sans",
    theme.style = "classic",
    padding = 0.2,
    grid = FALSE,
    grid.axis = FALSE,
    ncol,
    nrow,
    grid.axis.x = factor,
    grid.axis.y = "Concentration",
    x.title.shift = 0.02,
    var.size = 18,
    var.face = "bold",
    save.plot = FALSE,
    by.var = FALSE,
    plot.height = 5,
    plot.width = 5,
    plot.name = "plot_{var}.png",
    save.grid = FALSE,
    grid.name = "grid.png",
    grid.height = 8,
    grid.width = 7
) {

  if (is.null(var.adjust)) var.adjust <- list()
  if (is.null(var.method)) var.method <- list()

  anova <- c("anova", "aov")
  kruskal <- c("kruskal", "kruskal-wallis", "kw")
  student <- c("student", "t.test", "t")
  wilcox <- c("wilcox", "wilcox.test", "wilcoxon", "mann-whitney")

  if (!is.character(factor)) {
    factor <- rlang::as_name(rlang::ensym(factor))
  }

  if (length(factor) != 1 || !nzchar(factor)) {
    stop("The `factor` argument must be a single non-empty variable.")
  }

  if (save.plot == TRUE | save.grid == TRUE) {
    main_dir <- file.path("elements")
    dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
  }

  plots <- list()
  plots1 <- list()

  axis.range <- function(vec, n_breaks = 5) {
    min_val <- min(vec, na.rm = TRUE)
    max_val <- max(vec, na.rm = TRUE)
    range <- c(
      min_val * 0.95,
      max_val * 1.15
    )
    breaks <- pretty(range, n = n_breaks)
    return(breaks)
  }

  log_range <- function(vec, n_breaks = 5) {
    min_val <- min(vec, na.rm = TRUE)
    max_val <- max(vec, na.rm = TRUE)
    range <- c(
      if (min_val < 0) {
        min_val * 1.05
      } else {
        min_val * 0.95
      },
      max_val * 1.05
    )
    breaks <- pretty(range, n = n_breaks)
    return(breaks)
  }

  labels.dgt <- function(x) {
    sapply(x, function(num) {
      if (is.na(num)) return(NA_character_)

      if (num != 0 && (abs(num) < 0.001 || abs(num) >= 10000)) {
        sci_str <- formatC(num, format = "e", digits = 2)
        parts <- strsplit(sci_str, "e")[[1]]
        base <- sub("0+$", "", parts[1])
        base <- sub("\\.$", "", base)
        paste0(base, "e", parts[2])
      } else {
        format(num, scientific = FALSE, trim = TRUE, digits = 5)
      }
    })
  }

  log.dgt <- function(breaks) {
    sapply(breaks, function(x) {
      if (abs(x) < .Machine$double.eps^0.5) {
        "0"
      } else if (abs(x) < 0.001) {
        formatC(x, format = "e", digits = 2)
      } else {
        trimws(format(x, scientific = FALSE, digits = 6, trim = TRUE))
      }
    })
  }

  split.test <- function(text, max_length = 20) {
    if (nchar(text) <= max_length) {
      return(text)
    }
    midpoint <- floor(nchar(text) / 2)
    spaces <- gregexpr(" ", text)[[1]]
    if (all(spaces == -1)) {
      return(text)
    }
    closest_space <- spaces[which.min(abs(spaces - midpoint))]
    paste0(substr(text, 1, closest_space - 1), "\n", substr(text, closest_space + 1, nchar(text)))
  }

  user_supplied_fill_box <- !missing(fill.box)
  if (plot.type == "meanplot" && user_supplied_fill_box) {
    message("The `fill.box` argument is ignored when `plot.type = 'meanplot'`.")
  }

  get_colors <- function(x, n) {
    if (length(x) == 1) {
      if (x %in% rownames(RColorBrewer::brewer.pal.info)) {
        max_colors <- RColorBrewer::brewer.pal.info[x, "maxcolors"]
        n_use <- min(n, max_colors)
        return(RColorBrewer::brewer.pal(n_use, x))
      } else {
        return(rep(x, n))
      }
    } else if (length(x) >= n) {
      return(x[1:n])
    } else {
      stop("Color vector shorter than number of levels")
    }
  }

  validate_response <- function(custom_list, list_name, valid_vars) {
    if (!is.null(custom_list)) {
      invalid_names <- setdiff(names(custom_list), valid_vars)
      if (length(invalid_names) > 0) {
        warning(sprintf(
          "The following names in %s do not match any valid response variable: %s",
          list_name,
          paste(invalid_names, collapse = ", ")
        ))
      }
    }
  }

  format_stats <- function(df, p_col = NULL, cols = NULL) {
    target_cols <- if (!is.null(cols)) cols else names(df)

    trim_zeros <- function(x_chr) {
      vapply(x_chr, function(s) {
        if (is.na(s)) return(NA_character_)
        if (grepl("[eE]", s)) {
          parts <- strsplit(s, "[eE]")[[1]]
          mant  <- sub("0+$", "", parts[1])
          mant  <- sub("\\.$", "", mant)
          expo  <- parts[2]
          paste0(mant, "e", expo)
        } else {
          s <- sub("0+$", "", s)
          sub("\\.$", "", s)
        }
      }, character(1))
    }

    df <- df %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(target_cols),
        ~ if (is.numeric(.x)) {
          ifelse(
            is.na(.x), NA_character_,
            ifelse(
              .x == 0, "0",
              ifelse(
                abs(.x) < 0.001 | abs(.x) >= 1000,
                trim_zeros(formatC(.x, format = "e", digits = 2)),
                ifelse(
                  abs(.x) < 1,
                  trim_zeros(formatC(.x, format = "f", digits = 3)),
                  trim_zeros(formatC(.x, format = "f", digits = 2))
                )
              )
            )
          )
        } else {
          .x
        }
      ))

    if (!is.null(p_col) && p_col %in% names(df)) {
      df <- df %>%
        dplyr::mutate(Significance = dplyr::case_when(
          suppressWarnings(as.numeric(.data[[p_col]])) < 0.001 ~ "***",
          suppressWarnings(as.numeric(.data[[p_col]])) < 0.01  ~ "**",
          suppressWarnings(as.numeric(.data[[p_col]])) <= 0.05 ~ "*",
          TRUE ~ "ns"
        ))

      if ("term" %in% names(df)) {
        df <- df %>% dplyr::mutate(
          Significance = ifelse(term == "Residuals", "", Significance)
        )
      }
    }

    df
  }

  #Stops before Loop
  if (is.null(method) && is.null(var.method)) {
    stop("You must specify at least one of the arguments `method` or `var.method`.")
  }
  validate_response(var.method, "var.method", response)
  validate_response(var.adjust, "var.adjust", response)

  theme.options <- list(
    classic = ggplot2::theme_classic(),
    bw      = ggplot2::theme_bw(),
    minimal = ggplot2::theme_minimal(),
    light   = ggplot2::theme_light(),
    dark    = ggplot2::theme_dark(),
    gray    = ggplot2::theme_gray(),
    void    = ggplot2::theme_void()
  )

  if (!theme.style %in% names(theme.options)) {
    stop(sprintf("The `theme.style` must be one of the following : %s",
                 paste(names(theme.options), collapse = ", ")))
  }

  theme.chosen <- theme.options[[theme.style]]

  for (var in response) {
    if (by.var) {
      dirs <- file.path("elements", var)
    } else {
      dirs <- file.path("elements", "Plots")
    }
    dir.create(dirs, recursive = TRUE, showWarnings = FALSE)

    current_test <- if (!is.null(var.method[[var]])) var.method[[var]] else method
    current_adjust <- if (!is.null(var.adjust[[var]])) var.adjust[[var]] else if (!is.null(adjust)) adjust else "none"

    adj_norm <- tolower(current_adjust)

    map_aov <- c(
      "holm" = "holm",
      "hochberg" = "hochberg",
      "hommel" = "hommel",
      "bonferroni" = "bonferroni",
      "bh" = "BH",
      "by" = "BY",
      "fdr" = "fdr",
      "none" = "none"
    )
    map_kw <- c(
      "holm" = "holm",
      "bonferroni" = "bonferroni",
      "sidak" = "sidak",
      "hs" = "hs",
      "hochberg" = "hochberg",
      "bh" = "bh",
      "by" = "by",
      "fdr" = "bh",
      "none" = "none"
    )

    if (is.null(current_test)) {
      stop("Either `method` or `var.method` is required.")
    }

    if (current_test %in% kruskal) {
      current_adjust <- map_kw[[adj_norm]]
    } else if (current_test %in% anova) {
      current_adjust <- map_aov[[adj_norm]]
    }

    if (is.null(current_adjust)) {
      current_adjust <- "none"
    }

    formula <- as.formula(paste(var, "~", factor))
    cld <- NULL

    data2 <- if (filter.na) {
      data %>% filter(!is.na(.data[[var]]))
    } else {
      data
    }

    n_levels <- length(levels(factor(data2[[factor]])))
    fill.box <- get_colors(fill.box, n_levels)
    fill.point <- get_colors(fill.point, n_levels)


    #Stops and Messages
    if (missing(factor)) {
      stop("The `factor` argument is required.")
    }
    if (is.null(group.order)) {
      stop("The `group.order` argument is required.")
    }
    if (!is.null(adjust) && length(var.adjust) > 0) {
      stop("You cannot use both `adjust` and `var.adjust` at the same time.")
    }
    if (!is.null(method) && length(var.method) > 0) {
      stop("You cannot use both `method` and `var.method` at the same time.")
    }
    if (current_test %in% student && !is.null(current_adjust)) {
      stop("The `adjust` argument is not supported for the `'t.test'` method.")
    }
    if (current_test %in% student && (show.ph || show.letters || show.brackets)) {
      stop("The arguments `show.ph`, `show.letters`, and `show.brackets` are not supported for the 't.test' method.")
    }
    if (current_test %in% wilcox && !is.null(current_adjust)) {
      stop("The `adjust` argument is not supported for the 'wilcox.test' method.")
    }
    if (current_test %in% wilcox && (show.ph || show.letters || show.brackets)) {
      stop("The arguments `show.ph`, `show.letters`, and `show.brackets` are not supported for the 'wilcox.test' method.")
    }
    if (plot.type == "meanplot" && !missing(box.alpha)) {
      message("The `box.alpha`` argument is ignored when `plot.type = 'meanplot'`.")
    }
    if (plot.type == "boxplot" && !missing(fill.mean)) {
      message("The `fill.mean` argument is ignored when `plot.type = 'boxplot'`.")
    }
    if (plot.type == "boxplot" && !missing(mean.alpha)) {
      message("The `mean.alpha`` argument is ignored when `plot.type = 'boxplot'`.")
    }
    if (plot.type == "boxplot" && !missing(error.width)) {
      message("The `error.width` argument is ignored when `plot.type = 'boxplot'`.")
    }
    if (plot.type == "boxplot" && !missing(mean.size)) {
      message("The `mean.size` argument is ignored when `plot.type = 'boxplot'`.")
    }
    if (save.plot == FALSE && !missing(plot.height)) {
      message("The `plot.height`` argument is ignored when `save.plot = FALSE`.")
    }
    if (save.plot == FALSE && !missing(plot.width)) {
      message("The `plot.width` argument is ignored when `save.plot = FALSE`.")
    }
    if (grid == FALSE && !missing(grid.axis.y)) {
      message("The `grid.axis.y` argument is ignored when `grid = FALSE`.")
    }
    if (grid == FALSE && !missing(grid.axis.x)) {
      message("The `grid.axis.x`` argument is ignored when `grid = FALSE`.")
    }
    if (grid == FALSE && !missing(var.size)) {
      message("The `var.size` argument is ignored when `grid = FALSE`.")
    }
    if (grid == FALSE && !missing(var.face)) {
      message("The `var.face` argument is ignored when `grid = FALSE`.")
    }
    if (grid == FALSE && !missing(grid.name)) {
      message("The `grid.name` argument is ignored when `grid = FALSE`.")
    }
    if (save.grid == FALSE && !missing(grid.name)) {
      message("The `grid.name` argument is ignored when `save.grid = FALSE`.")
    }
    if (grid == FALSE && !missing(grid.height)) {
      message("The `grid.height` argument is ignored when `grid = FALSE`.")
    }
    if (save.grid == FALSE && !missing(grid.height)) {
      message("The `grid.height` argument is ignored when `save.grid = FALSE`.")
    }
    if (grid == FALSE && !missing(grid.width)) {
      message("The `grid.width` argument is ignored when `grid = FALSE`.")
    }
    if (grid == TRUE && missing(ncol) | grid == TRUE && missing(nrow)) {
      stop("The arguments `ncol` and `nrow` must be defined when `grid = TRUE`.")
    }
    if (save.grid == FALSE && !missing(grid.width)) {
      message("The `grid.width` argument is ignored when `save.grid = FALSE`.")
    }
    if (!plot.type %in% c("boxplot", "meanplot")) {
      stop("The `plot.type` argument must be either `'boxplot'` or `'meanplot'`.")
    }
    if (plot.type == "boxplot" && !fill %in% c("box", "point", "both", FALSE)) {
      stop("The `fill` argument must be `'box'`, `'point'`, `'both'`, or `FALSE` when `plot.type = 'boxplot'`.")
    }
    if (plot.type == "meanplot" && !fill %in% c("point", FALSE)) {
      stop("The `fill` argument must be `'point'` or `FALSE` when `plot.type = 'meanplot'`.")
    }
    if (save.plot == FALSE && !missing(by.var)) {
      stop("The `by.element` argument cannot be used when `save.plot == FALSE`.")
    }
    if (grid == FALSE && !missing(save.grid)) {
      stop("The `save.grid` argument cannot be used when `grid == FALSE`.")
    }
    if (grid == FALSE && !missing(grid.axis)) {
      stop("The `grid.axis` argument cannot be used when `grid == FALSE`.")
    }
    if (current_test %in% c(anova, kruskal)) {
      if (!missing(group.order) && length(group.order) < 3) {
        stop("`group.order` must contain at least 3 groups")
      }
      if (!missing(group.label) && length(group.label) < 3) {
        stop("`group.label`  must contain at least 3 groups")
      }
    }
    if (current_test %in% c(student, wilcox)) {
      if (!missing(group.order) && length(group.order) != 2) {
        stop("`group.order` must contain exactly 2 groups")
      }
      if (!missing(group.label) && length(group.label) != 2) {
        stop("`group.label` must contain exactly 2 groups")
      }
    }
    if (!current_test %in% c(anova, kruskal, student, wilcox)) {
      stop("Invalid statistical method: ", current_test)
    }

    test_summary <- NULL

    # Method
    #Anova
    if (current_test %in% anova) {

      test <- aov(formula, data = data2)
      test_name <- "ANOVA"
      pval <- summary(test)[[1]][["Pr(>F)"]][1]

      test_summary <- broom::tidy(test) %>% format_stats(p_col = "p.value")

      group_means <- data2 %>%
        group_by(.data[[factor]]) %>%
        summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(mean_val))

      data2[[factor]] <- factor(data2[[factor]], levels = group_means[[factor]])
      ordre_groupes <- group_means[[factor]]

      if (!is.null(current_adjust) && current_adjust != "none") {
        posthoc <- pairwise.t.test(
          x = data2[[var]],
          g = data2[[factor]],
          p.adjust.method = current_adjust,
          paired = FALSE
        )

        posthoc_summary <- as.data.frame(posthoc$p.value) %>%
          tibble::rownames_to_column("Comparison") %>%
          format_stats(p_col = "p.value")

        ph_name <- "Pairwise T-test"

        cld_raw <- make_cld(posthoc)
        cld_raw <- cld_raw %>% slice(match(ordre_groupes, cld_raw$group))

        group_summary <- data2 %>%
          group_by(.data[[factor]]) %>%
          summarise(
            Mean = mean(.data[[var]], na.rm = TRUE),
            SD   = sd(.data[[var]], na.rm = TRUE),
            .groups = "drop"
          )

        cld <- tibble(
          !!factor := cld_raw$group,
          cld = cld_raw$cld
        ) %>%
          left_join(group_summary, by = factor) %>%
          format_stats(cols = c("Mean", "SD")) %>%
          mutate(Mean_sd = paste0(Mean, " \u00B1 ", SD)) %>%
          rename("Mean \u00B1 sd" = Mean_sd) %>%
          select(all_of(factor), cld, "Mean \u00B1 sd")

      } else {
        posthoc <- TukeyHSD(test)
        posthoc_summary <- broom::tidy(posthoc) %>%
          format_stats(p_col = "adj.p.value")

        ph_name <- "Tukey Test"

        cld_raw <- multcompLetters2(formula, posthoc[[factor]][, "p adj"], as.data.frame(data2))

        group_summary <- data2 %>%
          group_by(.data[[factor]]) %>%
          summarise(
            Mean = mean(.data[[var]], na.rm = TRUE),
            SD   = sd(.data[[var]], na.rm = TRUE),
            .groups = "drop"
          )

        cld <- tibble(
          !!factor := names(cld_raw$Letters),
          cld = cld_raw$Letters
        ) %>%
          left_join(group_summary, by = factor) %>%
          format_stats(cols = c("Mean", "SD")) %>%
          mutate(Mean_sd = paste0(Mean, " \u00B1 ", SD)) %>%
          rename("Mean \u00B1 sd" = Mean_sd) %>%
          select(all_of(factor), cld, "Mean \u00B1 sd")
      }

      if (!is.null(cld) && length(unique(cld$cld)) == 1) {
        cld <- NULL
      }

      #Kruskal
    } else if (current_test %in% kruskal) {
      group_means <- data2 %>%
        group_by(.data[[factor]]) %>%
        summarise(mean_val = mean(.data[[var]], na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(mean_val))

      data2 <- data2 %>%
        mutate(!!factor := factor(.data[[factor]], levels = group_means[[factor]]))

      test <- kruskal.test(formula, data = data2)
      test_summary <- broom::tidy(test) %>%
        dplyr::rename(`Chi-squared` = statistic,
                      df = parameter) %>%
        format_stats(p_col = "p.value")

      if (is.null(current_adjust)) {
        suppressMessages({
          posthoc <- dunnTest(formula, data = data2, method = "none")
        })
      } else {
        suppressMessages({
          posthoc <- dunnTest(formula, data = data2, method = current_adjust)
        })
      }

      raw_posthoc <- posthoc$res

      group_levels <- levels(data2[[factor]])
      p_mat <- matrix(1, nrow = length(group_levels), ncol = length(group_levels),
                      dimnames = list(group_levels, group_levels))

      for (i in seq_len(base::nrow(raw_posthoc))) {
        pair <- unlist(strsplit(raw_posthoc$Comparison[i], " - "))
        p_val <- raw_posthoc$P.adj[i]
        p_mat[pair[1], pair[2]] <- p_val
        p_mat[pair[2], pair[1]] <- p_val
      }

      cld_raw <- multcompView::multcompLetters(p_mat, threshold = 0.05)

      posthoc_summary <- raw_posthoc %>% format_stats(p_col = "P.adj")

      group_summary <- data2 %>%
        group_by(.data[[factor]]) %>%
        summarise(
          Mean = mean(.data[[var]], na.rm = TRUE),
          SD   = sd(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )

      cld <- tibble(
        !!factor := names(cld_raw$Letters),
        cld = cld_raw$Letters
      ) %>%
        left_join(group_summary, by = factor) %>%
        format_stats(cols = c("Mean", "SD")) %>%
        mutate(Mean_sd = paste0(Mean, " \u00B1 ", SD)) %>%
        rename("Mean \u00B1 sd" = Mean_sd) %>%
        select(all_of(factor), cld, "Mean \u00B1 sd")

      test_name <- "Kruskal-Wallis"
      ph_name <- "Dunn's Test"
      pval <- test$p.value

      if (!is.null(cld) && length(unique(cld$cld)) == 1) {
        cld <- NULL
      }
      #Student
    } else if (current_test %in% student) {

      data2[[factor]] <- factor(data2[[factor]])

      if (length(levels(data2[[factor]])) != 2) {
        stop("The method `'t.test'` requires 2 groups.")
      }

      test <- t.test(data2[[var]] ~ data2[[factor]], paired = FALSE)

      test_summary <- tibble::tibble(
        Group1 = g1, Group2 = g2, Statistic = test$statistic, P.value = test$p.value
      ) %>% format_stats(p_col = "P.value")

      test_name <- "T-Test"
      pval <- test$p.value

      #Wilcoxon
    } else if (current_test %in% wilcox) {

      data2[[factor]] <- factor(data2[[factor]])

      if (length(levels(data2[[factor]])) != 2) {
        stop("The method `'wilcox.test'` requires 2 groups.")
      }

      test <- wilcox.test(data2[[var]] ~ data2[[factor]], paired = FALSE)

      test_summary <- tibble::tibble(
        Group1 = g1, Group2 = g2, Statistic = test$statistic, P.value = test$p.value
      ) %>% format_stats(p_col = "P.value")

      test_name <- "Wilcoxon Rank Sum Test"
      pval <- test$p.value
    }

    else {
      stop("Method: use: `'anova'`, `'kruskal'`, `'t.test'` or `'wilcox.test'`")
    }

    pval_formatted <- if (!is.na(pval) && pval < 0.001) "p < 0.001" else paste0("p = ", format(round(pval, 3), nsmall = 3))

    if (print.test == TRUE) {
      if (current_test %in% c(anova, kruskal)) {
        message(
          "Variable: ", var, "\n",
          "  Test: ", test_name, " (", pval_formatted, ")\n",
          "  Post-hoc: ", ph_name, "\n",
          "  Adjustment: ", current_adjust, "\n"
        )
      } else {
        message(
          "Variable: ", var, "\n",
          "  Test: ", test_name, " (", pval_formatted, ")\n",
        )
      }
    }

    if (!exists("export_details")) export_details <- list()

    export_details[[var]] <- list(
      Test        = test_summary,
      TestName    = test_name,
      Posthoc     = posthoc_summary,
      PosthocName = ph_name,
      Adjustment  = current_adjust,
      CLD         = cld
    )
    export_details[[var]]$Log <- log

    if (!is.null(group.order)) {
      data2[[factor]] <- factor(data2[[factor]], levels = group.order)
      if (!is.null(cld)) {
        cld[[factor]] <- factor(cld[[factor]], levels = group.order)
      }
    }

    ph_label <- paste0("(", ph_name, ")")

    if (show.ph == TRUE & show.test == TRUE) {
      annot_text1 <- paste0(test_name, ", ", pval_formatted)
      annot_text2 <- ph_label
      annot_text_pval <- pval_formatted
    } else if (show.ph == FALSE & show.test == TRUE) {
      annot_text <- paste0(test_name, ", ", pval_formatted)
      annot_text_pval <- pval_formatted

      if (current_test %in% wilcox) {
        annot_text <- split.test(annot_text)
      }
    }

    if (is.null(cld)) {
      dt <- data2 %>%
        group_by(.data[[factor]]) %>%
        summarise(w = mean(.data[[var]], na.rm = TRUE),
                  sd = sd(.data[[var]], na.rm = TRUE),
                  max = max(.data[[var]], na.rm = TRUE),
                  upper = w + sd,
                  .groups = "drop") %>%
        mutate(y.label = pmax(upper, max, na.rm = TRUE)) %>%
        mutate(cld = NA_character_)
    } else {
      dt <- data2 %>%
        group_by(.data[[factor]]) %>%
        summarise(w = mean(.data[[var]], na.rm = TRUE),
                  sd = sd(.data[[var]], na.rm = TRUE),
                  max = max(.data[[var]], na.rm = TRUE),
                  upper = w + sd,
                  .groups = "drop") %>%
        mutate(y.label = pmax(upper, max, na.rm = TRUE)) %>%
        left_join(cld, by = factor)
    }

    if (!is.null(cld)) {
      if (show.brackets == TRUE) {
        runs <- rle(as.character(dt$cld))
        run_lengths <- runs$lengths
        run_letters <- runs$values
        starts <- cumsum(c(1, head(run_lengths, -1)))
        ends <- cumsum(run_lengths)

        dt$show_letter <- TRUE
        for (i in seq_along(run_letters)) {
          if (run_lengths[i] >= 2) {
            dt$show_letter[starts[i]:ends[i]] <- FALSE
          }
        }
      }
    }

    if (log == TRUE) {
      y_breaks <- log_range(data2[[var]])
      label_breaks <- log.dgt(y_breaks)
      expand_mult <- c(0, 0.10)
    } else {
      if(plot.type == "boxplot") {
        y_breaks <- axis.range(data2[[var]])
      } else if (plot.type == "meanplot") {
        y_breaks <- axis.range(c(data2[[var]], dt$y.label))
      }

      label_breaks <- labels.dgt(y_breaks)
      expand_mult <- c(0, 0.10)
    }

    if (!flip) {
      p <- ggplot(data2, aes(x = .data[[factor]], y = .data[[var]])) +
        scale_x_discrete(
          name = if (!is.null(axis.title.x)) axis.title.x else factor,
          labels = if (!is.null(group.label) && label.angle == 0) label_wrap(10)(group.label) else if (!is.null(group.label) && label.angle != 0) group.label else levels(factor(data2[[factor]]))
        ) + theme.chosen +
        theme(axis.text.x = element_text(
          angle = label.angle,
          vjust = if (label.angle == 0) 1 else 1.03,
          hjust = if (label.angle == 0) 0.5 else 1,
          face  = label.face
        ))
    } else {
      p <- ggplot(data2, aes(x = .data[[var]], y = .data[[factor]])) +
        scale_y_discrete(
          name = if (!is.null(axis.title.y)) axis.title.y else factor,
          labels = if (!is.null(group.label) && label.angle == 0) label_wrap(10)(group.label) else if (!is.null(group.label) && label.angle != 0) group.label else levels(factor(data2[[factor]]))
        ) + theme.chosen +
        theme(axis.text.y = element_text(angle = label.angle, vjust = if (label.angle == 0) 0.5 else 0, hjust = 1, margin = margin(r = 2), face = label.face))
    }

    if (!flip) {
      if (plot.type == "boxplot") {
        if (fill == "box") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]], fill = .data[[factor]]), alpha = box.alpha,
                         show.legend = show.legend, outlier.shape = NA, na.rm = TRUE) +
            scale_fill_manual(values = fill.box) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", fill = fill.point, alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE)

        } else if (fill == "point") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]]),
                         fill = fill.box, alpha = box.alpha, show.legend = show.legend,
                         outlier.shape = NA, na.rm = TRUE) +
            geom_jitter(aes(fill = .data[[factor]]), alpha = point.alpha,
                        width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, na.rm = TRUE, show.legend = show.legend) +
            scale_fill_manual(values = fill.point)

        } else if (fill == "both") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]], fill = .data[[factor]]), alpha = box.alpha,
                         show.legend = show.legend, outlier.shape = NA, na.rm = TRUE) +
            scale_fill_manual(values = fill.box, name = "Box") +
            ggnewscale::new_scale_fill() +
            geom_jitter(aes(fill = .data[[factor]]),
                        width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, show.legend = show.legend) +
            scale_fill_manual(values = fill.point, name = "Point")

        } else if (fill == FALSE) {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]]), alpha = box.alpha,
                         fill = fill.box[1], show.legend = show.legend,
                         outlier.shape = NA, na.rm = TRUE) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point[1])
        }

      } else if (plot.type == "meanplot") {
        if (fill == "point") {
          p <- p +
            geom_jitter(aes(fill = .data[[factor]]),
                        width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, show.legend = show.legend, na.rm = TRUE) +
            geom_errorbar(data = dt,
                          aes(x = .data[[factor]],
                              ymin = if (log) w - sd else ifelse(w - sd < 0, 0, w - sd),
                              ymax = w + sd), alpha = error.alpha,
                          width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = .data[[factor]], y = w), alpha = mean.alpha,
                       shape = mean.shape, size = mean.size, fill = fill.mean, color = "black") +
            scale_fill_manual(values = fill.point)

        } else if (fill == FALSE) {
          p <- p +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point[1]) +
            geom_errorbar(data = dt,
                          aes(x = .data[[factor]],
                              ymin = if (log) w - sd else ifelse(w - sd < 0, 0, w - sd),
                              ymax = w + sd), alpha = error.alpha,
                          width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = .data[[factor]], y = w), alpha = mean.alpha,
                       shape = mean.shape, size = mean.size, fill = fill.mean, color = "black")
        }
      }

    } else {
      if (plot.type == "boxplot") {
        if (fill == "box") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]], fill = .data[[factor]]), alpha = box.alpha,
                         show.legend = show.legend, outlier.shape = NA, na.rm = TRUE) +
            scale_fill_manual(values = fill.box) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", fill = fill.point, alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE)

        } else if (fill == "point") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]]), alpha = box.alpha,
                         fill = fill.box, show.legend = show.legend,
                         outlier.shape = NA, na.rm = TRUE) +
            geom_jitter(aes(fill = .data[[factor]]),
                        width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, show.legend = show.legend) +
            scale_fill_manual(values = fill.point)

        } else if (fill == "both") {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]], fill = .data[[factor]]), alpha = box.alpha,
                         show.legend = show.legend, outlier.shape = NA, na.rm = TRUE) +
            scale_fill_manual(values = fill.box, name = "Box") +
            ggnewscale::new_scale_fill() +
            geom_jitter(aes(fill = .data[[factor]]), alpha = point.alpha,
                        width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, na.rm = TRUE, show.legend = show.legend) +
            scale_fill_manual(values = fill.point, name = "Point")

        } else if (fill == FALSE) {
          p <- p +
            geom_boxplot(aes(group = .data[[factor]]), alpha = box.alpha,
                         fill = fill.box[1], show.legend = show.legend,
                         outlier.shape = NA, na.rm = TRUE) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point[1])
        }

      } else if (plot.type == "meanplot") {
        if (fill == "point") {
          p <- p +
            geom_jitter(aes(fill = .data[[factor]]), alpha = point.alpha,
                        width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, show.legend = show.legend, na.rm = TRUE) +
            geom_errorbar(data = dt,
                          aes(y = .data[[factor]],
                              xmin = if (log) w - sd else ifelse(w - sd < 0, 0, w - sd),
                              xmax = w + sd), alpha = error.alpha,
                          width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = w, y = .data[[factor]]), alpha = mean.alpha,
                       shape = mean.shape, size = mean.size, fill = fill.mean, color = "black") +
            scale_fill_manual(values = fill.point)

        } else if (fill == FALSE) {
          p <- p +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", alpha = point.alpha,
                        stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point[1]) +
            geom_errorbar(data = dt,
                          aes(y = .data[[factor]],
                              xmin = if (log) w - sd else ifelse(w - sd < 0, 0, w - sd),
                              xmax = w + sd), alpha = error.alpha,
                          width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = w, y = .data[[factor]]), alpha = mean.alpha,
                       shape = mean.shape, size = mean.size, fill = fill.mean, color = "black")
        }
      }
    }

    letters_df <- dt %>%
      mutate(
        x_pos = as.numeric(factor(.data[[factor]])),
        y_pos = y.label
      )

    if (!is.null(cld) & show.letters == TRUE) {
      runs <- rle(as.character(letters_df$cld))
      run_lengths <- runs$lengths
      run_letters <- runs$values
      starts <- cumsum(c(1, head(run_lengths, -1)))
      ends <- cumsum(run_lengths)

      bracket_data <- tibble(
        xstart = numeric(0), xend = numeric(0), xmid = numeric(0),
        y = numeric(0), letter = character(0), vertical_height = numeric(0)
      )

      isolated_letters <- tibble(
        x = numeric(0), y = numeric(0), letter = character(0)
      )

      bracket_letters <- tibble(
        x = numeric(0), y = numeric(0), letter = character(0)
      )

      for (i in seq_along(run_letters)) {
        group_indices <- starts[i]:ends[i]
        group_x <- letters_df$x_pos[group_indices]
        group_y <- letters_df$y_pos[group_indices]

        bracket_y <- max(group_y, na.rm = TRUE) + 0.032 * diff(range(y_breaks))

        if (run_lengths[i] >= 2 && show.brackets) {
          bracket_letters <- bind_rows(bracket_letters,
                                       tibble(
                                         x = mean(group_x),
                                         y = bracket_y,
                                         letter = run_letters[i]
                                       ))

          bracket_data <- bind_rows(bracket_data,
                                    tibble(
                                      xstart = min(group_x),
                                      xend   = max(group_x),
                                      xmid   = mean(group_x),
                                      y      = bracket_y,
                                      letter = NA_character_,
                                      vertical_height = 0.01 * diff(range(y_breaks))
                                    ))
        } else {
          isolated_letters <- bind_rows(isolated_letters,
                                        tibble(
                                          x = group_x,
                                          y = group_y,
                                          letter = letters_df$cld[group_indices]
                                        ))
        }
      }

      if (!flip) {
        p <- p + geom_text(
          data = isolated_letters,
          aes(x = x, y = y, label = letter),
          inherit.aes = FALSE, fontface = letters.face,
          family = text.family, size = letters.size,
          vjust = -0.85
        )

        p <- p + geom_text(
          data = bracket_letters,
          aes(x = x, y = y, label = letter),
          inherit.aes = FALSE, fontface = letters.face,
          family = text.family, size = letters.size,
          vjust = -0.5
        )
      } else {
        p <- p + geom_text(
          data = isolated_letters,
          aes(y = x, x = y, label = letter),
          inherit.aes = FALSE, fontface = letters.face,
          family = text.family, size = letters.size,
          hjust = -0.85
        )

        p <- p + geom_text(
          data = bracket_letters,
          aes(y = x, x = y, label = letter),
          inherit.aes = FALSE, fontface = letters.face,
          family = text.family, size = letters.size,
          hjust = -0.5
        )
      }

      if (base::nrow(bracket_data) > 0 & show.brackets) {
        if (!flip) {
          p <- p +
            geom_segment(data = bracket_data,
                         aes(x = xstart, xend = xend, y = y, yend = y), inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(x = xstart, xend = xstart, y = y, yend = y - vertical_height), inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(x = xend, xend = xend, y = y, yend = y - vertical_height), inherit.aes = FALSE)
        } else {
          p <- p +
            geom_segment(data = bracket_data,
                         aes(y = xstart, yend = xend, x = y, xend = y), inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(y = xstart, yend = xstart, x = y, xend = y - vertical_height), inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(y = xend, yend = xend, x = y, xend = y - vertical_height), inherit.aes = FALSE)
        }
      }
    }

    max_data <- max(data2[[var]], na.rm = TRUE)

    text_height <- 0.035 * diff(range(y_breaks))

    top_letters <- if (exists("isolated_letters") && base::nrow(isolated_letters) > 0) {
      max(isolated_letters$y + 0.85 * text_height, na.rm = TRUE)
    } else {
      -Inf
    }

    top_brackets <- if (exists("bracket_letters") && base::nrow(bracket_letters) > 0) {
      max(bracket_letters$y + 0.5 * text_height, na.rm = TRUE)
    } else {
      -Inf
    }

    top_bracket_lines <- if (exists("bracket_data") && base::nrow(bracket_data) > 0) {
      max(bracket_data$y + bracket_data$vertical_height, na.rm = TRUE)
    } else {
      -Inf
    }

    true_max <- max(c(max_data, top_letters, top_brackets, top_bracket_lines), na.rm = TRUE)

    axis_min  <- min(data2[[var]], na.rm = TRUE)
    axis_span <- true_max - axis_min

    adjusted_max <- true_max + padding * axis_span

    min_data   <- min(data2[[var]], na.rm = TRUE)
    range_data <- max_data - min_data

    if (!flip) {
      p <- p + scale_y_continuous(
        name = if (!is.null(axis.title.y)) axis.title.y else paste(var),
        limits = c(min(y_breaks), adjusted_max),
        breaks = y_breaks,
        labels = label_breaks,
        expand = expansion(mult = expand_mult)
      )
    } else {
      p <- p + scale_x_continuous(
        name = if (!is.null(axis.title.x)) axis.title.x else paste(var),
        limits = c(min(y_breaks), adjusted_max),
        breaks = y_breaks,
        labels = label_breaks,
        expand = expansion(mult = expand_mult)
      )
    }

    p1 <- p

    if (current_test %in% c(anova, kruskal)) {
      if (show.test == TRUE & show.ph == FALSE) {
        p <- p +
          annotate("text",
                   x = Inf, y = Inf,
                   label = annot_text,
                   hjust = 1, vjust = 1.2,
                   size = 4.5, fontface = "italic", family = text.family)
        if (!flip) {
          p1 <- p1 +
            annotate("text",
                     x = -Inf, y = Inf,
                     label = annot_text_pval,
                     hjust = -0.1, vjust = 1.3,
                     size = 4.5, fontface = "italic", family = text.family)
        } else {
          p1 <- p1 +
            annotate("text",
                     x = Inf, y = Inf,
                     label = annot_text_pval,
                     hjust = 1.7, vjust = 1.3,
                     size = 4.5, fontface = "italic", family = text.family)

        }

      } else if (show.test == TRUE & show.ph == TRUE) {
        p <- p +
          annotate("text",
                   x = Inf, y = Inf,
                   label = annot_text1,
                   hjust = 1, vjust = 1.2,
                   size = 4.5, fontface = "italic", family = text.family) +
          annotate("text",
                   x = Inf, y = Inf,
                   label = annot_text2,
                   hjust = 1, vjust = 2.4,
                   size = 4.5, fontface = "italic", family = text.family)
        if (!flip) {
          p1 <- p1 +
            annotate("text",
                     x = -Inf, y = Inf,
                     label = annot_text_pval,
                     hjust = -0.1, vjust = 1.3,
                     size = 4.5, fontface = "italic", family = text.family)
        } else {
          p1 <- p1 +
            annotate("text",
                     x = Inf, y = Inf,
                     label = annot_text_pval,
                     hjust = 1.7, vjust = 1.3,
                     size = 4.5, fontface = "italic", family = text.family)
        }

      } else if (show.test == FALSE & show.ph == TRUE) {
        p <- p + annotate("text",
                          x = Inf, y = Inf,
                          label = ph_name,
                          hjust = 1, vjust = 1.2,
                          size = 4.5, fontface = "italic", family = text.family)
      }
    } else if (current_test %in% c(student, wilcox)) {
      if (show.test == TRUE) {
        p <- p + annotate("text",
                          x = Inf, y = Inf,
                          label = annot_text,
                          hjust = 1, vjust = 1.2,
                          size = 4.5, fontface = "italic", family = text.family)
        if (!flip) {
          p1 <- p1 + annotate("text",
                              x = -Inf, y = Inf,
                              label = annot_text_full,
                              hjust = -0.1, vjust = 1.3,
                              size = 4.5, fontface = "italic", family = text.family)
        } else {
          p1 <- p1 + annotate("text",
                              x = Inf, y = Inf,
                              label = annot_text_full,
                              hjust = 1.7, vjust = 1.3,
                              size = 4.5, fontface = "italic", family = text.family)
        }
      }
    }

    if (grid == TRUE && grid.axis == TRUE) {
      p1 <- p1 + labs(tag = var)
    }

    p1 <- p1 + theme(axis.text.y = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.text.x = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.title = element_text(size = axis.title.size, color = "black", face = axis.title.face, family = text.family))

    plots1[[var]] <- p1


    p <- p + theme(axis.text.y = element_text(size = axis.breaks.size, color = "black", family = text.family),
                   axis.text.x = element_text(size = axis.breaks.size, color = "black", family = text.family),
                   axis.title = element_text(size = axis.title.size, color = "black", face = axis.title.face, family = text.family))

    plots[[var]] <- p

    plot.file <- glue(plot.name)
    if (save.plot == TRUE) {
      ggsave(
        filename = file.path(dirs, plot.file),
        plot = p,
        device = "png",
        height = plot.height,
        width = plot.width
      )
    }

    invisible(print(p))
  }

  export_pdf <- function(export_details, filename = "Stats_Report.pdf") {
    pdf(filename, width = 8.5, height = 11)

    vars <- names(export_details)
    for (i in seq_along(vars)) {
      var <- vars[i]
      blocks <- list()

      title <- grid::textGrob(
        bquote(bold(.(var)) ~ "-" ~ italic(.(ifelse(!is.null(export_details[[var]]$Log) && export_details[[var]]$Log,
                                                    "log10 transformed data", "raw data")))),
        gp = grid::gpar(fontsize = 18)
      )

      blocks <- c(blocks, list(title))

      if (!is.null(export_details[[var]]$Test)) {
        test_title <- grid::textGrob(
          export_details[[var]]$TestName,
          gp = grid::gpar(fontsize = 14, fontface = "bold")
        )
        test_table <- gridExtra::tableGrob(
          export_details[[var]]$Test,
          rows = NULL,
          theme = gridExtra::ttheme_default(
            base_size = 9,
            padding = grid::unit(c(2, 2), "mm")
          )
        )
        test_block <- gridExtra::arrangeGrob(test_title, test_table, ncol = 1,
                                             heights = c(1, 10))
        blocks <- c(blocks, list(test_block))
      }

      if (!is.null(export_details[[var]]$Posthoc)) {
        posthoc_title <- grid::textGrob(
          export_details[[var]]$PosthocName,
          gp = grid::gpar(fontsize = 14, fontface = "bold")
        )

        adj_line <- grid::textGrob(
          paste("Adjustment:", ifelse(is.null(export_details[[var]]$Adjustment),
                                      "none",
                                      export_details[[var]]$Adjustment)),
          gp = grid::gpar(fontsize = 10, fontface = "italic")
        )

        posthoc_table <- gridExtra::tableGrob(
          export_details[[var]]$Posthoc,
          rows = NULL,
          theme = gridExtra::ttheme_default(
            base_size = 9,
            padding = grid::unit(c(2, 2), "mm")
          )
        )

        posthoc_block <- gridExtra::arrangeGrob(posthoc_title, adj_line, posthoc_table,
                                                ncol = 1,
                                                heights = c(1, 0.5, 10))
        blocks <- c(blocks, list(posthoc_block))
      }

      if (!is.null(export_details[[var]]$CLD)) {
        cld_title <- grid::textGrob(
          "Compact Letter Display",
          gp = grid::gpar(fontsize = 14, fontface = "bold")
        )
        cld_table <- gridExtra::tableGrob(
          export_details[[var]]$CLD,
          rows = NULL,
          theme = gridExtra::ttheme_default(
            base_size = 9,
            padding = grid::unit(c(2, 2), "mm")
          )
        )
        cld_block <- gridExtra::arrangeGrob(cld_title, cld_table,
                                            ncol = 1, heights = c(1, 10))
        blocks <- c(blocks, list(cld_block))
      }

      gridExtra::grid.arrange(grobs = blocks, ncol = 1)
    }

    dev.off()
  }
  export_pdf(export_details, filename = file.path("elements", "Stats_Report.pdf"))

  if (grid) {
    total_plots <- length(plots1)

    if (grid.axis) {
      plots1 <- lapply(plots1, function(pp) {
        pp + theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = axis.breaks.size, color = "black", family = text.family),
          axis.text.y  = element_text(size = axis.breaks.size, color = "black", family = text.family)
        )
      })

      if (!flip) {
        idx <- seq_len(total_plots)
        last_idx_per_col <- sapply(1:ncol, function(j) {
          if (j > total_plots) return(NA_integer_)
          j + ncol * floor((total_plots - j) / ncol)
        })
        last_idx_per_col <- last_idx_per_col[!is.na(last_idx_per_col)]

        keep_idx <- intersect(idx, last_idx_per_col)
        drop_idx <- setdiff(idx, keep_idx)

        plots1[drop_idx] <- lapply(plots1[drop_idx], function(pp) {
          pp + theme(axis.text.x = element_blank())
        })

      } else {
        col_ids <- ((seq_len(total_plots) - 1) %% ncol) + 1
        non_left_idx <- which(col_ids != 1)

        plots1[non_left_idx] <- lapply(plots1[non_left_idx], function(pp) {
          pp + theme(axis.text.y = element_blank())
        })
      }
    }

    grid_plot <- patchwork::wrap_plots(plots1, ncol = ncol, nrow = nrow) &
      theme(
        plot.tag = element_text(size = var.size, face = var.face, family = text.family,
                                hjust = 1, vjust = 1),
        plot.tag.position = c(0.98, 0.99)
      )

    if (grid.axis) {
      grid_plot <- cowplot::ggdraw() +
        theme(plot.background = element_rect(fill = "white", color = NA)) +
        cowplot::draw_plot(grid_plot) +
        cowplot::draw_label(grid.axis.x, x = 0.5 + x.title.shift, y = 0, vjust = 0.5,
                            size = axis.title.size, fontface = axis.title.face,
                            fontfamily = text.family, hjust = 0.5) +
        cowplot::draw_label(grid.axis.y, x = 0, y = 0.5, angle = 90,
                            vjust = 0.5, size = axis.title.size,
                            fontface = axis.title.face, fontfamily = text.family,
                            hjust = 0.5) +
        theme(plot.margin = margin(t = 10, r = 10, b = 25, l = 17))
    }

    if (save.grid) {
      ggsave(
        filename = file.path("elements", grid.name),
        plot = grid_plot,
        device = "png",
        height = grid.height,
        width = grid.width,
        bg = "white"
      )
    }

    return(grid_plot)
  }
}
