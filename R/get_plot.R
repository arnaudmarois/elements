usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("multcompView")
usethis::use_package("FSA")
usethis::use_package("rlang")
usethis::use_package("rcompanion")
usethis::use_package("tibble")
usethis::use_package("scales")
usethis::use_package("cowplot")
usethis::use_package("grid")
usethis::use_package("gridExtra")
usethis::use_package("glue")
usethis::use_package("biostat")
usethis::use_package("forcats")


#' Generate customizable boxplots or meanplots with statistical tests
#'
#' This function generates boxplots or meanplots for one or multiple response variables
#' with options for ANOVA, Kruskal-Wallis, t-tests, and Wilcoxon tests, plus customizable plot options.
#'
#' @importFrom dplyr group_by mutate arrange desc ungroup
#' @importFrom rlang as_name ensym
#' @importFrom forcats fct_inorder
#' @importFrom tibble tibble
#' @importFrom multcompView multcompLetters2
#' @importFrom FSA dunnTest
#' @importFrom ggplot2 ggplot geom_boxplot geom_jitter scale_y_continuous theme geom_text
#' @importFrom rcompanion cldList
#' @importFrom scales label_wrap
#' @importFrom cowplot plot_grid
#' @importFrom grid textGrob
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom glue glue
#' @importFrom biostat make_cld
#'
#' @param data A data.frame.
#' @param response Character vector of response variables.
#' @param x Character string for x-axis grouping variable.
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
#' @param filter.na Logical. If `TRUE`, rows with `NA` values in the current response variable are removed before analysis. Default is `FALSE`.
#' @param order.x Optional character vector specifying the order of the `x`-axis categories in the plot. Values must exactly match the factor levels in `x`.
#' @param show.test Logical. If `TRUE`, the statistical test name and p-value are displayed in the top-left corner. Default is `FALSE`.
#' @param show.ph Logical. If `TRUE`, the post-hoc test name and p-value are displayed in the top-left corner. Default is `FALSE`.
#' @param show.letters Logical. If `TRUE`, significance letters for all groups are displayed (only for ANOVA and Kruskal-Wallis). Default is `FALSE`.
#' @param show.brackets Logical. If `TRUE` when `show.letters` is `TRUE`, a bracket is displayed for adjacent groups sharing the same letter. Default is `FALSE`.
#' @param plot.type Character string specifying the type of plot to produce. Either `"boxplot"` for box-and-whisker plots or `"meanplot"` for plots with group means and error bars. Default is `"boxplot"`.
#' @param fill Optional character. Specifies which plot element to fill with colors based on the x variable. Default is `FALSE`.
#'   - With `plot.type = "boxplot"`, `"box"` fills the boxes, and `"point"` fills the points.
#'   - With `plot.type = "meanplot"`, `"point"` fills the points.
#' @param fill.box Optional. If `fill = "box"`, a character vector of colors to fill each box individually. The length should match the number of levels in the x variable. Otherwise, a single color applied to all boxes. Default is `"white"`.
#' @param fill.point Optional. Same as `fill.box` but applies when `fill = "point"`. Default is `"white"`.
#' @param fill.mean Optional. A single color used to fill all mean points when `plot.type = "meanplot"`. Default is `"black"`.
#' @param log Logical. If `TRUE`, y-axis breaks and limits are adjusted to properly accommodate data values that may be negative or positive as a result of a logarithmic transformation. Default is `FALSE`.
#' @param label.x Optional character vector specifying the labels to display on the x-axis.
#' @param show.legend Logical. If `TRUE`, the plot legend is displayed. Default is `FALSE`.
#' @param point.shape Optional numeric. Specifies the shape of the points in the plot. Default is `21`.
#' @param mean.shape Optional numeric. Specifies the shape of the mean points in the plot. Default is `21`.
#' @param point.size Optional numeric. Specifies the size of the points in the plot. Default is `2.5`.
#' @param mean.size Optional numeric. Specifies the size of the mean points in the plot. Default is `2.5`.
#' @param jitter.width Optional numeric. Specifies the amount of horizontal jitter applied to points. Default is `0.2`.
#' @param error.width Optional numeric. Specifies the width of the error bars when `plot.type = "meanplot"`. Default is `0.2`.
#' @param axis.title.x Optional character string to rename the x-axis title. If not provided, the default x variable name is used.
#' @param axis.title.y Optional character string to rename the y-axis title. If not provided, the default y variable name is used.
#' @param axis.breaks.size Optional numeric. Specifies the size of all axis breaks text. Default is `12`.
#' @param axis.title.size Optional numeric. Specifies the size of the axis title text. Default is `12`.
#' @param axis.title.face Optional character. Specifies the font face (e.g., `"plain"`, `"bold"`, `"italic"`) for both axis titles. Default is `"plain"`.
#' @param letters.face Optional character. Specifies the font face for all significance letters. Default is `"plain"`.
#' @param letters.size Optional numeric. Specifies the size of all significance letters. Default is `5`.
#' @param text.family Optional character. Specifies the font family for all text elements in the plot. Default is `"sans"`.
#' @param theme.style Character. Specifies the ggplot2 theme style preset to apply to the plot.
#'   Options include `"classic"`, `"bw"`, `"minimal"`, `"light"`, `"dark"`, `"gray"`, and `"void"`. Default is `"classic"`.
#' @param grid Optional logical. If `TRUE`, arranges all response variable plots into a combined grid layout. Default is `FALSE`.
#' @param ncol Integer. Number of columns in the combined grid plot.
#' @param nrow Integer. Number of rows in the combined grid plot.
#' @param grid.axis Optional logical. If `TRUE` and `grid = TRUE`, individual plot axes are hidden and shared x and y axes are displayed for the entire grid. Default is `FALSE`.
#' @param grid.axis.x Optional character string to name the shared x-axis title. Default is `"Groups"`.
#' @param grid.axis.y Optional character string to name the shared y-axis title. Default is `"Concentration"`.
#' @param label.size Optional numeric. Size of the labels displaying the name of each individual plot in the combined grid. Default is `7`.
#' @param label.face Optional character. Specifies the font face of the labels displaying the name of each individual plot in the combined grid. Default is `"bold"`.
#' @param save.plot Optional logical. If `TRUE`, saves each individual plot as a JPEG file in the working directory. Default is `FALSE`.
#' @param by.var Optional logical. Used only if `save.plot = TRUE`. If `TRUE`, saves each plot individually in separate folders named after each response variable. Default is `FALSE`.
#' @param plot.height Optional numeric. Height of the saved plot image in inches when `save.plot = TRUE`. Default is `5`.
#' @param plot.width Optional numeric. Width of the saved plot image in inches when `save.plot = TRUE`. Default is `5`.
#' @param plot.name Optional character. Name of the saved plot file. Defaults to `"plot_{var}.png"`, where `{var}` is replaced by the response variable name when saving multiple plots.
#' @param save.grid Optional logical. If `TRUE`, saves the combined grid as a JPEG file in the working directory. Default is `FALSE`.
#' @param grid.name Optional character. Name of the saved combined grid plot file. Default is `"grid.png"`.
#' @param grid.height Optional numeric. Height of the saved combined grid plot image in inches when `save.grid = TRUE`. Default is `8`.
#' @param grid.width Optional numeric. Width of the saved combined grid plot image in inches when `save.grid = TRUE`. Default is `7`.

#' @return A plot or list of plots.
#' @export

var.adjust <- list()
var.method <- list()

get.plot <- function(
    data,
    response,
    x,
    method = NULL,
    adjust = NULL,
    var.method = NULL,
    var.adjust = NULL,
    filter.na = FALSE,
    order.x = NULL,
    show.test = FALSE,
    show.ph = FALSE,
    show.letters = FALSE,
    show.brackets = FALSE,
    plot.type = "boxplot",
    fill = FALSE,
    fill.box = "white",
    fill.point = "white",
    fill.mean = "black",
    log = FALSE,
    label.x = NULL,
    show.legend = FALSE,
    point.shape = 21,
    mean.shape = 21,
    point.size = 2.5,
    mean.size = 2.5,
    jitter.width = 0.2,
    error.width = 0.2,
    axis.title.x = NULL,
    axis.title.y = NULL,
    axis.breaks.size = 12,
    axis.title.size = 12,
    axis.title.face = "plain",
    letters.face = "plain",
    letters.size = 5,
    text.family = "sans",
    theme.style = "classic",
    grid = FALSE,
    ncol = NULL,
    nrow = NULL,
    grid.axis = FALSE,
    grid.axis.x = "Groups",
    grid.axis.y = "Concentration",
    label.size = 7,
    label.face = "bold",
    save.plot = FALSE,
    by.var = FALSE,
    plot.height = 5,
    plot.width = 5,
    save.grid = FALSE,
    grid.name = "grid.png",
    grid.height = 8,
    grid.width = 7
) {
  anova <- c("anova", "aov")
  kruskal <- c("kruskal", "kruskal-wallis", "kw")
  student <- c("student", "t.test", "t")
  wilcox <- c("wilcox", "wilcox.test", "wilcoxon", "mann-whitney")

  if (!is.character(x)) {
    x <- rlang::as_name(rlang::ensym(x))
  }

  if (length(x) != 1 || !nzchar(x)) {
    stop("😞 The `x` argument must be a single non-empty variable.")
  }

  if (save.plot == TRUE | save.grid == TRUE) {
    main_dir <- file.path("elementstats")
    dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
  }

  plots <- list()
  plots1 <- list()

  axis.range <- function(vec, n_breaks = 5) {
    min_val <- min(vec, na.rm = TRUE)
    max_val <- max(vec, na.rm = TRUE)
    range <- c(
      min_val*0.95,
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
        min_val*1.05
      } else {
        min_val*0.95
      }
      , max_val*1.05
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

  #Stops before Loop
  if (is.null(method) && is.null(var.method)) {
    stop("😞 You must specify at least one of the arguments `method` or `var.method`.")
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
      dirs <- file.path("elementstats", var)
    } else {
      dirs <- file.path("elementstats", "Plots")
    }
    dir.create(dirs, recursive = TRUE, showWarnings = FALSE)


    current_method <- if (!is.null(var.method[[var]])) var.method[[var]] else method
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

    if (current_method %in% kruskal) {
      current_adjust <- map_kw[[adj_norm]]
    } else if (current_method %in% anova) {
      current_adjust <- map_aov[[adj_norm]]
    }

    if (is.null(current_adjust)) {
      current_adjust <- "none"
    }

    if (!is.null(adjust) || !is.null(var.adjust)) {
      message("Adjustment method used for ", var, ": ", current_adjust)
    }

    formula <- as.formula(paste(var, "~", x))
    cld <- NULL

    data2 <- if (filter.na) {
      data %>% filter(!is.na(.data[[var]]))
    } else {
      data
    }

    #Stops & Messages
    if (missing(x)) {
      stop("😞 The `x` argument is required.")
    }
    if (is.null(current_method)) {
      stop(glue::glue("😞 No statistical method is defined for the variable '{var}'."))
    }
    if (!is.null(method) && !is.null(var.method)) {
      stop("😞 Please provide only one of 'method' or 'var.method', not both.")
    }
    if (!is.null(adjust) && !is.null(var.adjust)) {
      stop("😞 Please provide only one of 'adjust' or 'var.adjust', not both.")
    }
    if (current_method %in% student && !is.null(current_adjust)) {
      stop("😞 The `adjust` argument is not supported for the 't.test' method.")
    }
    if (current_method %in% student && (show.ph || show.letters || show.brackets)) {
      stop("😞 The arguments `show.ph`, `show.letters`, and `show.brackets` are not supported for the 't.test' method.")
    }
    if (current_method %in% wilcox && !is.null(current_adjust)) {
      stop("😞 The `adjust` argument is not supported for the 'wilcox.test' method.")
    }
    if (current_method %in% wilcox && (show.ph || show.letters || show.brackets)) {
      stop("😞 The arguments `show.ph`, `show.letters`, and `show.brackets` are not supported for the 'wilcox.test' method.")
    }
    if (plot.type == "meanplot" && !missing(fill.box)) {
      message("⚠️ The fill.box argument is ignored when plot.type = 'meanplot'.")
    }
    if (plot.type == "boxplot" && !missing(fill.mean)) {
      message("⚠️ The fill.mean argument is ignored when plot.type = 'boxplot'.")
    }
    if (plot.type == "boxplot" && !missing(error.width)) {
      message("⚠️ The error.width argument is ignored when plot.type = 'boxplot'.")
    }
    if (plot.type == "boxplot" && !missing(mean.size)) {
      message("⚠️ The mean.size argument is ignored when plot.type = 'boxplot'.")
    }
    if (save.plot == FALSE && !missing(plot.height)) {
      message("⚠️ The plot.height argument is ignored when save.plot = FALSE.")
    }
    if (save.plot == FALSE && !missing(plot.width)) {
      message("⚠️ The plot.width argument is ignored when save.plot = FALSE.")
    }
    if (grid == FALSE && !missing(grid.axis)) {
      message("⚠️ The grid.axis argument is ignored when grid = FALSE.")
    }
    if (grid == FALSE && !missing(grid.axis.y)) {
      message("⚠️ The grid.axis.y argument is ignored when grid = FALSE.")
    }
    if (grid == FALSE && !missing(grid.axis.x)) {
      message("⚠️ The grid.axis.x argument is ignored when grid = FALSE.")
    }
    if (grid == FALSE && !missing(label.size)) {
      message("⚠️ The label.size argument is ignored when grid = FALSE.")
    }
    if (grid == FALSE && !missing(label.face)) {
      message("⚠️ The label.face argument is ignored when grid = FALSE.")
    }
    if (grid == FALSE && !missing(grid.name)) {
      message("⚠️ The grid.name argument is ignored when grid = FALSE.")
    }
    if (save.grid == FALSE && !missing(grid.name)) {
      message("⚠️ The grid.name argument is ignored when save.grid = FALSE.")
    }
    if (grid == FALSE && !missing(grid.height)) {
      message("⚠️ The grid.height argument is ignored when grid = FALSE.")
    }
    if (save.grid == FALSE && !missing(grid.height)) {
      message("⚠️ The grid.height argument is ignored when save.grid = FALSE.")
    }
    if (grid == FALSE && !missing(grid.width)) {
      message("⚠️ The grid.width argument is ignored when grid = FALSE.")
    }
    if (save.grid == FALSE && !missing(grid.width)) {
      message("⚠️ The grid.width argument is ignored when save.grid = FALSE.")
    }
    if (!plot.type %in% c("boxplot", "meanplot")) {
      stop("😞 The plot.type argument must be either 'boxplot' or 'meanplot'.")
    }
    if (plot.type == "boxplot" && !fill %in% c("box", "point", FALSE)) {
      stop("😞 The fill argument must be 'box', 'point', or FALSE when plot.type = 'boxplot'.")
    }
    if (plot.type == "meanplot" && !fill %in% c("point", FALSE)) {
      stop("😞 The fill argument must be 'point' or FALSE when plot.type = 'meanplot'.")
    }
    if (save.plot == FALSE && !missing(by.var)) {
      stop("😞 The by.element argument cannot be used when save.plot == FALSE.")
    }
    if (grid == FALSE && !missing(save.grid)) {
      stop("😞 The save.grid argument cannot be used when grid == FALSE.")
    }
    if (current_method %in% c(anova, kruskal)) {
      if (!missing(order.x) && length(order.x) < 3) {
        stop("😞 `order.x` must contain at least 3 groups")
      }
      if (!missing(label.x) && length(label.x) < 3) {
        stop("😞 `label.x`  must contain at least 3 groups")
      }
    }
    if (current_method %in% c(student, wilcox)) {
      if (!missing(order.x) && length(order.x) != 2) {
        stop("😞 `order.x` must contain exactly 2 groups")
      }
      if (!missing(label.x) && length(label.x) != 2) {
        stop("😞 `label.x` must contain exactly 2 groups")
      }
    }
    if (!current_method %in% c(anova, kruskal, student, wilcox)) {
      stop("Invalid statistical method: ", current_method)
    }

    # Method
    #Anova
    if (current_method %in% anova) {
      test <- aov(formula, data = data2)

      if (is.character(current_adjust) && nzchar(current_adjust)) {
        response <- data2[[var]]
        group_factor <- data2[[x]]

        group_means <- data2 %>%
          group_by(.data[[x]]) %>%
          summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(mean_val))

        data2[[x]] <- factor(data2[[x]], levels = group_means[[x]])
        ordre_groupes <- group_means[[x]]

        posthoc <- pairwise.t.test(
          x = data2[[var]],
          g = data2[[x]],
          p.adjust.method = current_adjust,
          paired = FALSE)

        cld_raw <- make_cld(posthoc)

        cld_raw <- cld_raw %>%
          slice(match(ordre_groupes, cld_raw$group))

        cld <- tibble(
          !!x := cld_raw$group,
          cld = cld_raw$cld
        )
        ph_name <- "Pairwise T-test"

      } else {
        posthoc <- TukeyHSD(test)

        ph_name <- "Tukey Test"

        cld_raw <- multcompLetters2(formula, posthoc[[x]][, "p adj"], as.data.frame(data2))
        cld <- tibble::tibble(!!x := names(cld_raw$Letters), cld = cld_raw$Letters)
      }

      if (!is.null(cld) && length(unique(cld$cld)) == 1) {
        cld <- NULL
      }

      test_name <- "ANOVA"
      pval <- summary(test)[[1]][["Pr(>F)"]][1]

      #Kruskal
    } else if (current_method %in% kruskal) {

      group_means <- data2 %>%
        group_by(.data[[x]]) %>%
        summarise(mean_val = mean(.data[[var]], na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(mean_val))

      data2 <- data2 %>%
        mutate(!!x := factor(.data[[x]], levels = group_means[[x]]))

      test <- kruskal.test(formula, data = data2)
      if (is.null(current_adjust)) {
        posthoc <- dunnTest(formula, data = data2, method = "none")
      } else {
        posthoc <- dunnTest(formula, data = data2, method = current_adjust)
      }

      res <- posthoc$res

      group_levels <- levels(data2[[x]])

      p_mat <- matrix(1, nrow = length(group_levels), ncol = length(group_levels),
                      dimnames = list(group_levels, group_levels))
      for (i in seq_len(nrow(res))) {
        pair <- unlist(strsplit(res$Comparison[i], " - "))
        p_mat[pair[1], pair[2]] <- res$P.adj[i]
        p_mat[pair[2], pair[1]] <- res$P.adj[i]
      }

      cld_raw <- multcompView::multcompLetters(p_mat, threshold = 0.05)
      cld <- tibble::tibble(!!x := names(cld_raw$Letters),
                            cld = cld_raw$Letters)

      if (!is.null(cld) && length(unique(cld$cld)) == 1) {
        cld <- NULL
      }

      test_name <- "Kruskal-Wallis"
      ph_name <- "Dunn's Test"
      pval <- test$p.value

      #T.test
    } else if (current_method %in% student) {

      # Forcer x en facteur
      data2[[x]] <- factor(data2[[x]])

      if (length(levels(data2[[x]])) != 2) {
        stop("The method `'t.test'` requires 2 groups.")
      }

      test <- t.test(data2[[var]] ~ data2[[x]], paired = FALSE)
      test_name <- "T-Test"
      pval <- test$p.value
    }
    #Wilcoxon
    else if(current_method %in% wilcox) {

      data2[[x]] <- factor(data2[[x]])

      if (length(levels(data2[[x]])) != 2) {
        stop("The method `'wilcox.test'` requires 2 groups.")
      }

      test <- wilcox.test(data2[[var]] ~ data2[[x]], paired = FALSE)
      test_name <- "Wilcoxon Rank Sum Test"
      pval <- test$p.value
    }

    else {
      stop("Method: use: `'anova'`, `'kruskal'`, `'t.test'` or `'wilcox.test'`")
    }

    pval_formatted <- if (!is.na(pval) && pval < 0.001) "p < 0.001" else paste0("p = ", format(round(pval, 3), nsmall = 3))

    if (!is.null(order.x)) {
      data2[[x]] <- factor(data2[[x]], levels = order.x)
      if (!is.null(cld)) {
        cld[[x]] <- factor(cld[[x]], levels = order.x)
      }
    }

    ph_label <- paste0("(", ph_name, ")")


    if (show.ph == TRUE & show.test == TRUE) {
      annot_text1 <- paste0(test_name, ", ", pval_formatted)
      annot_text2 <- ph_label
    } else if (show.ph == FALSE & show.test == TRUE) {
      annot_text <- paste0(test_name, ", ", pval_formatted)
      if (current_method %in% wilcox) {
        annot_text <- split.test(annot_text)
      }
    }

    if (is.null(cld)) {
      dt <- data2 %>%
        group_by(.data[[x]]) %>%
        summarise(w = mean(.data[[var]], na.rm = TRUE),
                  sd = sd(.data[[var]], na.rm = TRUE),
                  max = max(.data[[var]], na.rm = TRUE),
                  upper = w + sd,
                  .groups = "drop") %>%
        mutate(y.label = pmax(upper, max, na.rm = TRUE)) %>%
        mutate(cld = NA_character_) }
    else {
      dt <- data2 %>%
        group_by(.data[[x]]) %>%
        summarise(w = mean(.data[[var]], na.rm = TRUE),
                  sd = sd(.data[[var]], na.rm = TRUE),
                  max = max(.data[[var]], na.rm = TRUE),
                  upper = w + sd,
                  .groups = "drop") %>%
        mutate(y.label = pmax(upper, max, na.rm = TRUE)) %>%
        left_join(cld, by = x)
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
      expand_mult <- c(0.06, 0.1)
    } else {
      if(plot.type == "boxplot") { y_breaks <- axis.range(data2[[var]])}
      else if (plot.type == "meanplot") {y_breaks <- axis.range(c(data2[[var]], dt$y.label))}

      label_breaks <- labels.dgt(y_breaks)
      expand_mult <- c(0, 0.10)
    }

    max_data <- max(data2[[var]], na.rm = TRUE)
    min_data <- min(data2[[var]], na.rm = TRUE)
    range_data <- max_data - min_data

    # Add 10% padding on top
    max_y_limit <- max_data + 0.1 * range_data

      p <- ggplot(data2, aes(x = .data[[x]], y = .data[[var]])) +
        scale_x_discrete(
          name = if (!is.null(axis.title.x)) axis.title.x else x,
          labels = if (!is.null(label.x)) label_wrap(10)(label.x) else dt[[x]]) +
        theme.chosen

      if (plot.type == "boxplot") {
        if (fill == "box") {
          p <- p + geom_boxplot(aes(group = .data[[x]], fill = .data[[x]]), show.legend = show.legend, outlier.shape = NA, na.rm = TRUE) +
            scale_fill_manual(values = fill.box) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black", fill = fill.point,
                        stroke = 0.5, size = point.size, na.rm = TRUE)

        } else if (fill == "point") {
          p <- p + geom_boxplot(aes(group = .data[[x]]), show.legend = show.legend, outlier.shape = NA, na.rm = TRUE, fill = fill.box) +
            geom_jitter(aes(fill = .data[[x]]), width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, na.rm = TRUE, show.legend = show.legend) +
            scale_fill_manual(values = fill.point)

        } else if (fill == FALSE) {
          p <- p + geom_boxplot(aes(group = .data[[x]]), show.legend = show.legend, outlier.shape = NA, na.rm = TRUE, fill = fill.box) +
            geom_jitter(width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point)
        }
      } else if (plot.type == "meanplot") {
        if (fill == "point") {
          p <- p +
            geom_jitter(aes(fill = .data[[x]]), width = jitter.width, shape = point.shape, colour = "black",
                        stroke = 0.5, size = point.size, show.legend = show.legend, na.rm = TRUE) +
            geom_errorbar(data = dt, aes(x = .data[[x]], ymin = if (log) w-sd else ifelse(w-sd < 0, 0, w-sd),
                                         ymax = w+sd), width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = .data[[x]], y = w), shape = mean.shape, size = mean.size, fill = fill.mean, color = "black") +
            scale_fill_manual(values = fill.point)

        } else if (fill == FALSE) {
          p <- p +  geom_jitter(width = jitter.width, shape = point.shape, colour = "black",
                                stroke = 0.5, size = point.size, na.rm = TRUE, fill = fill.point) +
            geom_errorbar(data = dt, aes(x = .data[[x]], ymin = if (log) w-sd else ifelse(w-sd < 0, 0, w-sd),
                                         ymax = w+sd), width = error.width, color = "black", inherit.aes = FALSE) +
            geom_point(data = dt, aes(x = .data[[x]], y = w), shape = mean.shape, size = mean.size, fill = fill.mean, color = "black")
        }
      }

    letters_df <- dt %>%
      mutate(x_pos = as.numeric(.data[[x]]))

    y_offset <- diff(range(y_breaks)) * 0.08

    if (!is.null(cld) & show.letters == TRUE) {

      # Calculer ymax_plot pour garder la même échelle y dans les deux cas
      runs <- rle(as.character(letters_df$cld))
      run_lengths <- runs$lengths
      run_letters <- runs$values
      starts <- cumsum(c(1, head(run_lengths, -1)))
      ends <- cumsum(run_lengths)

      all_ys <- c()
      for (i in seq_along(run_letters)) {
        group_indices <- starts[i]:ends[i]
        group_max <- letters_df$max[group_indices]
        bracket_y <- max(group_max, na.rm = TRUE) + y_offset
        all_ys <- c(all_ys, bracket_y)
      }
      ymax_plot <- max(all_ys, na.rm = TRUE) * 1.1

      if (show.brackets) {

        bracket_data <- tibble(
          xstart = numeric(0),
          xend = numeric(0),
          xmid = numeric(0),
          y = numeric(0),
          letter = character(0),
          vertical_height = numeric(0)
        )

        isolated_data <- tibble(
          x = numeric(0),
          y = numeric(0),
          letter = character(0)
        )

        for (i in seq_along(run_letters)) {
          group_indices <- starts[i]:ends[i]
          group_x <- letters_df$x_pos[group_indices]
          group_max <- letters_df$max[group_indices]

          bracket_y <- max(group_max, na.rm = TRUE) + y_offset

          if (run_lengths[i] >= 2) {
            bracket_data <- bracket_data %>%
              bind_rows(
                tibble(
                  xstart = min(group_x),
                  xend = max(group_x),
                  xmid = mean(c(min(group_x), max(group_x))),
                  y = bracket_y,
                  letter = run_letters[i],
                  vertical_height = min(abs(bracket_y - group_max), na.rm = TRUE) * 0.5
                )
              )
          } else {
            isolated_data <- isolated_data %>%
              bind_rows(
                tibble(
                  x = group_x,
                  y = max(group_max, na.rm = TRUE) + y_offset,
                  letter = run_letters[i]
                )
              )
          }
        }

        if (nrow(bracket_data) > 0) {
          p <- p +
            geom_segment(data = bracket_data,
                         aes(x = xstart, xend = xend, y = y, yend = y),
                         inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(x = xstart, xend = xstart, y = y, yend = y - vertical_height),
                         inherit.aes = FALSE) +
            geom_segment(data = bracket_data,
                         aes(x = xend, xend = xend, y = y, yend = y - vertical_height),
                         inherit.aes = FALSE) +
            geom_text(data = bracket_data,
                      aes(x = xmid, y = y, label = letter),
                      vjust = -0.3,
                      inherit.aes = FALSE,
                      fontface = letters.face,
                      family = text.family,
                      size = letters.size)
        }

        if (nrow(isolated_data) > 0) {
          p <- p +
            geom_text(data = isolated_data,
                      aes(x = x, y = y, label = letter),
                      inherit.aes = FALSE,
                      fontface = letters.face,
                      family = text.family,
                      size = letters.size)
        }

      } else {
        # show.brackets == FALSE : on affiche les lettres isolées décalées
        p <- p +
          geom_text(data = dt,
                    aes(label = cld, y = max + y_offset),
                    size = letters.size,
                    family = text.family,
                    fontface = letters.face)
      }

      # Ajouter le scale_y_continuous dans les deux cas, avec la même limite y
      p <- p +
        scale_y_continuous(name = if (!is.null(axis.title.y)) axis.title.y else paste(var),
                           limits = c(min(y_breaks), ymax_plot),
                           breaks = y_breaks,
                           expand = expansion(mult = expand_mult),
                           labels = label_breaks)
    }
    if (is.null(cld) || show.letters == FALSE) {
      p <- p + scale_y_continuous(
        name = if (!is.null(axis.title.y)) axis.title.y else paste(var),
        limits = c(min(y_breaks), max_y_limit),
        breaks = y_breaks,
        expand = expansion(mult = expand_mult),
        labels = label_breaks
      )
    }

    if (current_method %in% c(anova, kruskal)) {
      if (show.test == TRUE & show.ph == FALSE) {
        p <- p +
          annotate("text",
                   x = -Inf, y = Inf,
                   label = annot_text,
                   hjust = -0.05, vjust = 1.2,
                   size = 4.5, fontface = "italic", family = text.family)
      }
      else if (show.test == TRUE & show.ph == TRUE) {
        p <- p +
          annotate("text",
                   x = -Inf, y = Inf,
                   label = annot_text1,
                   hjust = -0.05, vjust = 1.2,
                   size = 4.5, fontface = "italic", family = text.family) +
          annotate("text",
                   x = -Inf, y = Inf,
                   label = annot_text2,
                   hjust = -0.08, vjust = 2.4,
                   size = 4.5, fontface = "italic", family = text.family)
      } else if (show.test == FALSE & show.ph == TRUE) {
        p <- p + annotate("text",
                          x = -Inf, y = Inf,
                          label = ph_name,
                          hjust = -0.08, vjust = 1.2,
                          size = 4.5, fontface = "italic", family = text.family)
      }
    } else if (current_method %in% c(student, wilcox)) {
      if (show.test == TRUE) {
        p <- p + annotate("text",
                          x = -Inf, y = Inf,
                          label = annot_text,
                          hjust = -0.1, vjust = 1.2,
                          size = 4.5, fontface = "italic", family = text.family)
      }}

    p1 <- p
    if (grid == TRUE & grid.axis == TRUE) {
      p1 <- p1 +
        annotate("text", x = Inf, y = Inf, label = paste(var), hjust = 1, vjust = 1,
                 size = label.size, family = text.family, fontface = label.face) +
        theme(axis.text.y = element_text(size = axis.breaks.size, color = "black", family = text.family),
              axis.text.x = element_text(size = axis.breaks.size, color = "black", family = text.family),
              axis.title = element_blank())

      p <- p + theme(axis.text.y = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.text.x = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.title = element_text(size = axis.title.size, color = "black", face = axis.title.face))
      plots1[[var]] <- p1
      plots[[var]] <- p

    } else {
      p <- p + theme(axis.text.y = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.text.x = element_text(size = axis.breaks.size, color = "black", family = text.family),
                     axis.title = element_text(size = axis.title.size, color = "black", face = axis.title.face))
    }
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

  if (grid == TRUE) {
    if (grid.axis == TRUE) {
      grid_plot <- plot_grid(plotlist = plots1, nrow = nrow, ncol = ncol)

      x.grob <- textGrob(if (!is.null(grid.axis.x)) grid.axis.x else x,
                         gp=gpar(fontface=axis.title.face, col="black", fontsize=axis.title.size))

      y.grob <- textGrob(grid.axis.y,
                         gp=gpar(fontface=axis.title.face, col="black", fontsize=axis.title.size), rot=90)


      grid_plot <- grid.arrange(arrangeGrob(grid_plot, left = y.grob, bottom = x.grob))

    } else {
      grid_plot <- plot_grid(plotlist = plots)
    }
    if (save.grid == TRUE) {
      ggsave(
        file.path(main_dir, grid.name),
        plot = grid_plot,
        device = "png",
        height = grid.height,
        width = grid.width
      )
    }
  }
}




