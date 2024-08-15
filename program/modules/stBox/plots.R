library(grDevices)

# Default color scheme to use when custom color scheme is not specified for a sample
cluster_color_func <- colorRampPalette(c("navy", "blue", "cyan", "darkgreen", "green",  "yellow", "orange", "brown", "red", "magenta", "purple", "gray"))


#' create_spatial_plot_cx
#'   Create spatial plot of signal (module score) for selected gene signature or single gene in selected sample
#'
#' @param plot_img_id   - id of tissue background image for selected sample
#' @param seurat        - scored seurat object for selected sample and gene signature or single gene
#' @param sig_gene_name - name of signal variable to plot from scored seurat object
#' @param features      - list of individual genes for selected gene signature or single gene
#' @param title         - plot title
#' @param subtitle      - plot subtitle
#' @param top_genes     - data.frame of top represented genes for each spot in the sample
#' @param scaling       - selected option for color scale range: "native", "trim", "center", or list of min and max values
#' @param file_name     - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_spatial_plot_cx <- function(plot_img_id, seurat, sig_gene_name, features, title,
                                   subtitle, top_genes, scaling, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        buffer.space <- 10   # plot padding around points
        coordinates  <- Seurat::GetTissueCoordinates(seurat)
        spot_cal     <- get_spot_top_represented_genes(top_genes, features)
        signal       <- get_seurat_object_data(seurat = seurat, items = sig_gene_name)
        var.Annot    <- merge(signal, spot_cal, by.x = 0, by.y = "Coord", all.x = TRUE)
        coord        <- var.Annot$Row.names

        var.Annot   <- var.Annot %>%
            select(-c("Row.names"))

        colnames(var.Annot) <- c("signal", "top")
        rownames(var.Annot) <- coord

        events <- htmlwidgets::JS("{'mousemove' : function(o, e, t) {
                                 if (o != null && o != false &&
                                     o.z != null && o.z.signal != null &&
                                     o.z.signal.length > 0) {
                                     if (o.z.top[0] == null) {
                                       top_genes = '';
                                     } else {
                                       top_genes = o.z.top[0];
                                     }
                                     signal = Math.round((o.z.signal[0] + Number.EPSILON) * 10000)/10000;
                                     t.showInfoSpan(e, '<b>Signal</b>: ' + signal +
                                                       '<br/>' +
                                                       '<b> Top Genes</b>: ' + top_genes);
                                 }
                  }}")

        # color spectrum scaling only (no scaling of original data!)
        breaks <- list()
        colors <- list("#5E4FA2", "#3E96B7", "#88CFA4", "#D7EF9B", "#FFFFBF", "#FDD380", "#F88D52", "#DC494C", "#9E0142")
        col.pr <- 2

        if (is.list(scaling)) {
            if (length(scaling) == 2) {
                breaks <- seq(from = scaling[[1]], to = scaling[[2]], length.out = length(colors))
            }
        } else {
            if (scaling == "trim") { # q1-q99
                q      <- quantile(signal, probs = c(0, 0.01, 0.99, 1), na.rm = TRUE, names = FALSE)
                breaks <- seq(from = q[2], to = q[3], length.out = length(colors))
            } else if (scaling == "center") {
                breaks <- seq(from = 0, to = 1, length.out = length(colors))
            }
        }

        dr.continue <- quantile(signal, probs = c(0, 0.01, 0.99, 1), na.rm = TRUE, names = FALSE)
        dr.continue <- abs(dr.continue[3] - dr.continue[2]) >= g_dynamic_range_minimum

        # disallow small DR on single-gene plots only
        if ((length(features) > 1) || (dr.continue)) {

            # check if we can round the breaks to the number of digits
            # and properly define the spectrum without duplicate values
            if (length(breaks) > 0) {
                testround <- round(breaks, digits = col.pr)
                if (length(unique(testround)) == length(testround)) {
                    breaks <- testround
                }
            }

            ### To view the raw background image:
            # grid::grid.raster(Seurat::GetImage(seurat, mode = "raw", image = "slice1"))

            ### To view the ggplot seurat graph (the color scaling will NOT match)
            # Seurat::SpatialFeaturePlot(seurat, sig_gene_name, "slice1")

            plot <- canvasXpress(
                data                      = coordinates,
                varAnnot                  = var.Annot,
                ### For interactive testing replace the backgroundImage javascript reference line
                # backgroundImage        = Seurat::GetImage(seurat, mode = "raw", image = "slice1"),
                backgroundImage           = glue('javascript://{plot_img_id}'),
                backgroundType            = "panel",
                colorBy                   = "signal",
                title                     = title,
                subtitle                  = subtitle,
                yAxis                     = list("imagerow"),
                xAxis                     = list("imagecol"),
                setMinX                   = max(0,   floor(min(coordinates$imagecol) - buffer.space)),
                setMaxX                   = min(600, ceiling(max(coordinates$imagecol) + buffer.space)),
                setMinY                   = max(0,   floor(min(coordinates$imagerow) - buffer.space)),
                setMaxY                   = min(600, ceiling(max(coordinates$imagerow) + buffer.space)),
                dataPointSizeScaleFactor  = 1.4,
                scatterOutlineThreshold   = 100000,
                outlineWidth              = 0.1,
                transparency              = 1,
                transparencyHidden        = 0.5,
                visiumHideWhenFilter      = TRUE,
                visiumFixedAspectRatio    = FALSE,
                colorSpectrum             = colors,
                colorSpectrumBreaks       = breaks,
                filterMode                = "color",
                missingDataColor          = "#F0F0F0",
                showLegendTitle           = FALSE,
                moveable                  = FALSE,
                broadcastGroup            = plot_img_id,
                events                    = events,
                saveFilename              = file_name) %>% add_cx_common_config

            if (is.null(subtitle)) {
                plot <- plot %>%
                    add_cx_small_chart_config()
            } else {
                plot <- plot %>%
                    add_cx_large_chart_config()
            }
        }
    }

    plot
}


#' create_cluster_plot_cx
#'   Create spatial plot of clusters for selected sample
#'
#' @param plot_img_id - id of tissue background image for selected sample
#' @param seurat      - seurat object for selected sample
#' @param file_name   - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_cluster_plot_cx <- function(plot_img_id, seurat, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        buffer.space <- 10 # plot padding around points
        coordinates  <- Seurat::GetTissueCoordinates(seurat)
        clusters     <- get_clusters(seurat = seurat)
        plot         <- NULL

        if (!is.null(clusters) && (NROW(clusters) > 0)) {
            var.Annot <- clusters %>%
                rename(cluster = 1) %>%
                mutate(cluster = as.character(cluster))
            color_orde <- get_color_order(seurat      = seurat,
                                          data_groups = unique(var.Annot$cluster),
                                          color_slot  = "user.Clustering_color_scheme")
            colors     <- color_orde$colors
            var.Annot  <- sort_seurat_data(seurat_data = var.Annot, colors = colors)

            coordinates <- coordinates[rownames(var.Annot), ]

            events <- htmlwidgets::JS("{'mousemove' : function(o, e, t) {
                                 if (o != null && o != false &&
                                     o.z != null && o.z.cluster != null &&
                                     o.z.cluster.length > 0) {
                                     t.showInfoSpan(e, '<b>Cluster</b>: ' + o.z.cluster);
                                 }
                  }}")
            plot <- canvasXpress(
                data                      = coordinates,
                varAnnot                  = var.Annot,
                ### For interactive testing replace the backgroundImage javascript reference line
                # backgroundImage          = Seurat::GetImage(seurat, mode = "raw", image = "slice1"),
                backgroundImage           = glue('javascript://{plot_img_id}'),
                backgroundType            = "panel",
                colorBy                   = "cluster",
                colorKey                  = list("cluster" = colors),
                stringVariableFactors     = list("cluster"),
                title                     = "Clusters",
                yAxis                     = list("imagerow"),
                xAxis                     = list("imagecol"),
                setMinX                   = max(0,   floor(min(coordinates$imagecol) - buffer.space)),
                setMaxX                   = min(600, ceiling(max(coordinates$imagecol) + buffer.space)),
                setMinY                   = max(0,   floor(min(coordinates$imagerow) - buffer.space)),
                setMaxY                   = min(600, ceiling(max(coordinates$imagerow) + buffer.space)),
                dataPointSizeScaleFactor  = 1.4,
                scatterOutlineThreshold   = 100000,
                outlineWidth              = 0.1,
                transparency              = 1,
                transparencyHidden        = 0.5,
                visiumHideWhenFilter      = TRUE,
                visiumFixedAspectRatio    = FALSE,
                filterMode                = "color",
                missingDataColor          = "#F0F0F0",
                showLegendTitle           = FALSE,
                legendTextScaleFontFactor = 1,
                broadcastGroup            = plot_img_id,
                movable                   = FALSE,
                events                    = events,
                saveFilename              = file_name
            ) %>% add_cx_common_config
        }
    }
    plot
}


#' create_tissue_plot_cx
#'   Create spatial plot of tissue background image for selected sample
#'
#' @param plot_img_id - id of tissue background image for selected sample
#' @param seurat      - seurat object for selected sample
#' @param file_name   - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_tissue_plot_cx <- function(plot_img_id, seurat, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        buffer.space <- 10 # plot padding around points
        coordinates  <- Seurat::GetTissueCoordinates(seurat)
        clusters     <- get_clusters(seurat = seurat)

        plot <- canvasXpress(
            data                   = data.frame(imagerow = -1, imagecol = -1),
            ### For interactive testing replace the backgroundImage javascript reference line
            # backgroundImage  = Seurat::GetImage(seurat, mode = "raw", image = "slice1"),
            backgroundImage        = glue('javascript://{plot_img_id}'),
            backgroundType         = "panel",
            title                  = "Tissue",
            setMinX                = max(0,   floor(min(coordinates$imagecol) - buffer.space)),
            setMaxX                = min(600, ceiling(max(coordinates$imagecol) + buffer.space)),
            setMinY                = max(0,   floor(min(coordinates$imagerow) - buffer.space)),
            setMaxY                = min(600, ceiling(max(coordinates$imagerow) + buffer.space)),
            broadcastGroup         = plot_img_id,
            movable                = FALSE,
            visiumFixedAspectRatio = FALSE,
            saveFilename           = file_name) %>% add_cx_common_config
    }
    plot
}


#' create_pathology_plot_cx
#'   Create spatial plot of pathology groups for selected sample
#'
#' @param plot_img_id - id of tissue background image for selected sample
#' @param seurat      - seurat object for selected sample
#' @param file_name   - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_pathology_plot_cx <- function(plot_img_id, seurat, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        buffer.space <- 10 # plot padding around points
        coordinates  <- Seurat::GetTissueCoordinates(seurat)
        pathology    <- NULL

        if (("meta.data" %in% slotNames(seurat)) &&
            ("Pathology.Group" %in% names(seurat@meta.data))) {
            pathology <- seurat@meta.data["Pathology.Group"]
        }

        if (!is.null(pathology) && (NROW(pathology) > 0)) {
            names(pathology) <- "pathology"

            # get the colors and order to use
            colors_order <- get_color_order(seurat     = seurat,
                                           data_groups = unique(pathology$pathology) %>% as.character(),
                                           color_slot  = "user.Pathology_color_scheme")
            colors       <- colors_order$colors
            var.Annot    <- sort_seurat_data(seurat_data = pathology, colors = colors)
            coordinates  <- coordinates[rownames(var.Annot), ]

            events <- htmlwidgets::JS("{'mousemove' : function(o, e, t) {
                                 if (o != null && o != false &&
                                     o.z != null && o.z.pathology != null &&
                                     o.z.pathology.length > 0) {
                                     t.showInfoSpan(e, '<b>Pathology</b>: ' + o.z.pathology);
                                 }}}")

            plot <- canvasXpress(
                data                      = coordinates,
                varAnnot                  = var.Annot,
                ### For interactive testing replace the backgroundImage javascript reference line
                # backgroundImage        = Seurat::GetImage(seurat, mode = "raw", image = "slice1"),
                backgroundImage           = glue('javascript://{plot_img_id}'),
                backgroundType            = "panel",
                colorBy                   = "pathology",
                colorKey                  = list("pathology" = colors),
                legendOrder               = list("pathology" = names(colors)),
                stringVariableFactors     = list("pathology"),
                title                     = "Pathology",
                yAxis                     = list("imagerow"),
                xAxis                     = list("imagecol"),
                setMinX                   = max(0,   floor(min(coordinates$imagecol) - buffer.space)),
                setMaxX                   = min(600, ceiling(max(coordinates$imagecol) + buffer.space)),
                setMinY                   = max(0,   floor(min(coordinates$imagerow) - buffer.space)),
                setMaxY                   = min(600, ceiling(max(coordinates$imagerow) + buffer.space)),
                dataPointSizeScaleFactor  = 1.4,
                scatterOutlineThreshold   = 100000,
                outlineWidth              = 0.1,
                transparency              = 1,
                transparencyHidden        = 0.5,
                visiumHideWhenFilter      = TRUE,
                visiumFixedAspectRatio    = FALSE,
                colors                    = colors,
                filterMode                = "color",
                missingDataColor          = "#F0F0F0",
                showLegendTitle           = FALSE,
                legendTextScaleFontFactor = 1,
                broadcastGroup            = plot_img_id,
                movable                   = FALSE,
                events                    = events,
                saveFilename              = file_name) %>% add_cx_common_config
        }
    }
    plot
}


#' create_dot_plot_cx
#'   Create heatmap dot plot of normalized expression of each gene in each cluster, for selected sample and gene signature.
#'   Genes are on the X-axis, clusters on the Y-axis. Each dot is colored by the scaled mean of expm1(Expression) of the
#'   gene in that cluster, and sized by the percent expressed.
#'
#' @param seurat     - seurat object for selected sample
#' @param features   - list of individual genes for selected gene signature
#' @param gene_title - gene signature name to use in plot title
#' @param file_name  - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_dot_plot_cx <- function(seurat, features, gene_title, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        clusters         <- get_clusters(seurat = seurat)

        if (!is.null(clusters) && (NROW(clusters) > 0)) {
            # for coloring the clusters we need to arrange the same as the cluster spatial plot
            clusters <- clusters %>%
                rename(cluster = 1) %>%
                mutate(cluster = as.character(cluster))

            colors_order     <- get_color_order(seurat      = seurat,
                                                data_groups = unique(clusters$cluster),
                                                color_slot  = "user.Clustering_color_scheme")
            colors           <- colors_order$colors
            samplesClustered <- !colors_order$object_colors
            clusters         <- sort_seurat_data(clusters, colors, convert_to_character = FALSE)
            data             <- get_seurat_object_data(seurat, features)

            if (!is.null(data)) {
                data <- bind_cols(
                    data[rownames(clusters), , drop = F],
                    Cluster = clusters$cluster) %>%
                    gather("Gene", "Expression", any_of(features)) %>%
                    group_by(Cluster, Gene) %>%
                    summarise(mean_exp = mean(expm1(Expression), na.rm = TRUE),
                              pct_exp  = sum(Expression > 0, na.rm = TRUE)/length(Expression) * 100) %>%
                    group_by(Gene) %>%
                    mutate(mean_exp = scale(mean_exp),
                           mean_exp = case_when(mean_exp < 0 ~ 0,
                                                mean_exp > 1 ~ 1,
                                                TRUE         ~ mean_exp))

                cx.mean <- data %>%
                    select(-pct_exp) %>%
                    spread(Cluster, mean_exp) %>%
                    as.data.frame()

                rownames(cx.mean) <- cx.mean$Gene
                cx.mean           <- cx.mean %>% select(-Gene)

                cx.pct <- data %>%
                    select(-mean_exp) %>%
                    spread(Cluster, pct_exp) %>%
                    as.data.frame()

                rownames(cx.pct) <- cx.pct$Gene
                cx.pct           <- cx.pct %>% select(-Gene)

                smp.annot           <- data.frame("Cluster" = colnames(cx.mean), stringsAsFactors = F)
                rownames(smp.annot) <- smp.annot$Cluster

                events <- htmlwidgets::JS("{'mousemove' : function(o, e, t) {
                                                      if ((o != null) && (o.y != null) && !(o.objectType)) {
                                                          t.showInfoSpan(e,
                                                              '<b>' + o.y.vars[0] + '</b><br/>' +
                                                              '<b>Cluster ' + o.y.smps[0] + '</b><br/>' +
                                                              'Avg Expression: ' + o.y.data[0] + '<br/>' +
                                                              'Pct Expressed: &nbsp;' + o.y.data2[0])
                                                      };
                                                  }}")

                plot <- canvasXpress(
                    data                       = list(y = cx.mean, data2 = cx.pct),
                    smpAnnot                   = smp.annot,
                    stringSampleFactors        = list("Cluster"),
                    graphType                  = "Heatmap",
                    objectBorderColor          = "white",
                    sizeBy                     = "Percent\nExpressed",
                    sizes                      = c(3, seq(8, 40, by = 4)),
                    sizeByData                 = "data2",
                    sizeByContinuous           = TRUE,
                    colorSpectrum              = list("lightgray", "darkblue"),
                    colorKey                   = list(Cluster = colors),
                    yAxisTitle                 = "Cluster",
                    xAxisTitle                 = NULL,
                    title                      = glue("{gene_title} Normalized Expression"),
                    titleScaleFontFactor       = 0.45,
                    titleFontStyle             = "bold",
                    smpOverlays                = list("Cluster"),
                    smpOverlayProperties       = list(Cluster = list(thickness = 20,
                                                                     rotate    = 90,
                                                                     showName  = FALSE,
                                                                     showBox   = FALSE)),
                    smpTitleScaleFontFactor    = 0.7,
                    showNameOverlays           = FALSE,
                    overlayTextColor           = "white",
                    overlayTextFontStyle       = "bold",
                    overlaysThickness          = 45,
                    overlayTextScaleFontFactor = 1.3,
                    smpTitle                   = "Cluster",
                    samplesClustered           = samplesClustered,
                    showSmpDendrogram          = FALSE,
                    variablesClustered         = ifelse(NROW(cx.mean) > 1, TRUE, FALSE),
                    showVarDendrogram          = FALSE,
                    varTextRotate              = 45,
                    showSampleNames            = FALSE,
                    showHeatmapIndicator       = TRUE,
                    heatmapIndicatorPosition   = "top",
                    heatmapIndicatorHeight     = 15,
                    heatmapIndicatorWidth      = 600,
                    broadcast                  = FALSE,
                    events                     = events,
                    saveFilename               = file_name) %>% add_cx_common_config
            }
        }
    }

    plot
}


#' create_box_plot_cx
#'   Create box plot of the signal (module score) of selected single gene in selected sample, grouped by cluster.
#'   Clusters are on the Y-axis, signal on the X-axis.
#'
#' @param seurat        - scored seurat object for selected sample and selected single gene
#' @param sig_gene_name - name of signal variable to plot from scored seurat object
#' @param gene_title    - gene name to use in plot title
#' @param file_name     - file name to use when downloading the plot as an image or as json
#'
#' @return canvasXpress plot object or NULL
create_box_plot_cx <- function(seurat, sig_gene_name, gene_title, file_name) {
    plot <- NULL

    if (!is.null(seurat)) {
        clusters <- get_clusters(seurat = seurat)

        if (!is.null(clusters) && (NROW(clusters) > 0)) {
            # for coloring the clusters we need to arrange the same as the cluster spatial plot
            clusters  <- clusters %>%
                rename(cluster = 1) %>%
                mutate(cluster = as.character(cluster))

            colors_order <- get_color_order(seurat, unique(clusters$cluster), "user.Clustering_color_scheme")
            colors       <- colors_order$colors
            clusters     <- sort_seurat_data(seurat_data = clusters, colors = colors)
            data         <- get_seurat_object_data(seurat = seurat, items = sig_gene_name)

            if (!is.null(data)) {
                data <- bind_cols(
                    data[rownames(clusters), , drop = F],
                    Cluster = clusters$cluster) %>%
                    mutate(Cluster = as.character(Cluster))
                cx.data <- data %>%
                    select(all_of(sig_gene_name)) %>%
                    rename(!!gene_title := all_of(sig_gene_name)) %>%
                    as.data.frame()
                rownames(cx.data) <- rownames(data)
                cx.data           <- cx.data %>%
                    t() %>%
                    as.data.frame()

                smp.annot <- data[, "Cluster", drop = FALSE]

                plot <- canvasXpress(
                    data                       = cx.data,
                    smpAnnot                   = smp.annot,
                    stringSampleFactors        = list("Cluster"),
                    groupingFactors            = list("Cluster"),
                    graphType                  = "Boxplot",
                    showLegend                 = FALSE,
                    colorBy                    = "Cluster",
                    colorKey                   = list(Cluster = colors),
                    smpTitle                   = "Cluster",
                    xAxisTitle                 = NULL,
                    xAxis2Show                 = TRUE,
                    title                      = glue("{gene_title} Normalized Expression"),
                    titleScaleFontFactor       = 0.45,
                    titleFontStyle             = "bold",
                    smpOverlays                = list("Cluster"),
                    showSampleNames            = FALSE,
                    smpOverlayProperties       = list(Cluster = list(thickness = 20,
                                                                     rotate    = 90,
                                                                     showBox   = FALSE)),
                    smpTitleScaleFontFactor    = 1.3,
                    showNameOverlays           = FALSE,
                    overlayTextColor           = "white",
                    overlayTextFontStyle       = "bold",
                    overlaysThickness          = 45,
                    overlayTextScaleFontFactor = 10,
                    boxplotTransparency        = 1,
                    boxplotOutliersRatio       = 4,
                    xAxisTicks                 = 10,
                    xAxisTextScaleFontFactor   = 0.7,
                    decorations                = list(line = list(list(color = "gray", x = 0))),
                    broadcast                  = FALSE,
                    saveFilename               = file_name) %>% add_cx_common_config
            }
        }
    }

    plot
}


#' get_download_filename
#'   Construct a file name for downloading a canvasXpress chart
#'
#' @param text - vector of identifiers to use in the file name, such as sample name and gene label
#'
#' @return character
get_download_filename <- function(text) {
    # only take the first part of anything that is multi-line
    lines <- sapply(text, function(x) { str_split(x, '\n', simplify = TRUE)[1] })

    glue(format(Sys.time(),'%Y_%m_%d_%H_%M'), ".", paste(lines, collapse = "."))
}

# --- Helper Functions ---


#' get_spot_top_represented_genes
#'   Calculate the top N represented genes for each spot in the given Seurat object.
#'   * If a gene signature is selected, the results are filtered to the top genes within that signature.
#'   * If a single gene is selected, the overall top genes are used.
#'
#' @param top_genes - data.frame of all top represented genes for each spot in the sample
#' @param features  - list of individual genes for selected gene signature or single gene
#'
#' @return data.frame
get_spot_top_represented_genes <- function(top_genes, features) {
    if (length(features) > 1) {
        #filter by signature genes
        top_genes <- top_genes %>%
            filter(Symbol %in% features)
    }

    top_genes %>%
        group_by(Coord) %>%
        top_n(g_top_n_spot_genes, Value) %>%
        group_by(Coord) %>%
        arrange(desc(Value)) %>%
        summarise(top = paste(Symbol, collapse = ', '))
}


#' get_clusters
#'   Get clusters for selected sample. If clusters are provided in `seurat@misc$user.Clustering`, these values are used.
#'   If not, then the values in `seurat@active.ident` are used, if they exist and have more than one level.
#'
#' @param seurat - seurat object for selected sample
#'
#' @return data.frame or NULL
get_clusters <- function(seurat) {
    result <- NULL

    if (!is.null(seurat)) {
        if (("misc" %in% slotNames(seurat)) &&
            ("user.Clustering" %in% names(seurat@misc))) {
            result <- get_seurat_object_data(seurat = seurat , items = seurat@misc$user.Clustering)
        }

        # backup
        if (all(is.null(result),
                is.factor(seurat@active.ident),
                length(levels(seurat@active.ident)) > 1)) {
            result <- get_seurat_object_data(seurat = seurat, items = "ident")
        }
    }
    result
}


#' get_color_order
#'   Get color scheme for data groups (clusters or pathology) in selected sample.
#'   * If a valid color scheme is specified in the seurat object, this scheme is used to specify custom colors and
#'     group order. A color scheme is valid if it contains all valid hex color codes and contains all the data groups
#'     that occur in the selected sample.
#'   * If color scheme is missing or invalid, default colors are used and no custom order is used for data groups.
#'
#' @param seurat      - seurat object for selected sample
#' @param data_groups - char or factor vector with all the unique groups in the selected sample
#' @param color_slot  - one of "user.Clustering_color_scheme" or "user.Pathology_color_scheme"
#'
#' @return list of two values:
#'           colors - char - hex color codes, optionally with names (ordered)
#'           object_colors - boolean - object_colors indicates the returned colors are from seurat object or not
get_color_order <- function(seurat, data_groups, color_slot) {
    object_colors <- TRUE
    colors        <- NULL

    if (color_scheme_exists(seurat = seurat, color_slot = color_slot)) {

        try({
            if (color_slot == "user.Clustering_color_scheme") {
                color_scheme <- seurat@misc$user.Clustering_color_scheme
            } else {
                color_scheme <- seurat@misc$user.Pathology_color_scheme
            }

            # extract the colors and order
            color_order <- unlist(str_split(color_scheme, ",")) %>%
                sapply(function(x) {unlist(str_split(x, "="))})

            color_scheme_colors        <- color_order[2, ] %>% as.character()
            valid_colors               <- all(grepl("^#([A-Fa-f0-9]{6})$", color_scheme_colors))
            color_scheme_groups        <- color_order[1, ] %>% as.character()

            groups_without_spaces      <- remove_spaces_and_lower_case(group = color_scheme_groups)
            data_groups_without_spaces <- remove_spaces_and_lower_case(group = data_groups)

            if (all(valid_colors,
                    length(data_groups) <= length(color_scheme_groups),
                    data_groups_without_spaces %in% groups_without_spaces)) {
                # construct final groups colors names to be exactly as data groups names in case it exists
                final_groups <- sapply(color_scheme_groups, function(group) {
                    group_index <- match(remove_spaces_and_lower_case(group = group), data_groups_without_spaces)
                    if (is.na(group_index)) {
                        group
                    } else {
                        data_groups[group_index]
                    }
                })
                colors        <- color_scheme_colors
                names(colors) <- final_groups
            }
        })
    }

    if (is.null(colors)) {
        colors        <- cluster_color_func(length(data_groups))
        names(colors) <- unique(data_groups) %>% as.character()
        colors        <- colors[sort(names(colors))]
        object_colors <- FALSE
    }
    list(colors = colors, object_colors = object_colors)
}


#' color_scheme_exists
#'   Return TRUE if a color scheme exists in the specified color slot of selected sample, FALSE otherwise
#'
#' @param seurat     - seurat object for selected sample
#' @param color_slot - one of "user.Clustering_color_scheme" or "user.Pathology_color_scheme"
#'
#' @return logical
color_scheme_exists <- function(seurat, color_slot = "user.Clustering_color_scheme") {
    ("misc" %in% slotNames(seurat)) && (color_slot %in% names(seurat@misc))
}


#' get_seurat_object_data
#'   Get variable(s) from Seurat object
#'
#' @param seurat - seurat object
#' @param items  - variable(s) to fetch
#' @param slot   - slot to pull data from
#'
#' @return data.frame as returned by Seurat::FetchData()
get_seurat_object_data <- function(seurat, items, slot = "data") {
    try({
        Seurat::FetchData(object = seurat, vars = items, slot = slot)
    }, silent = TRUE)
}


#' sort_seurat_data
#'   Re-order data.frame for plotting, to ensure the legend displays according to the specified color order
#'
#' @param seurat_data          - data.frame with 1 column to reorder
#' @param colors               - named list of colors in desired legend order (names must match the values in the
#'                               data.frame column)
#' @param convert_to_character - logical - when TRUE, forces the column to character in the returned data.frame
#'
#' @return data.frame
sort_seurat_data <- function(seurat_data, colors, convert_to_character = TRUE) {
    # we expect that the data frame only has one column, otherwise return the original data frame
    if (NCOL(seurat_data) == 1) {
        col_name <- names(seurat_data)
    } else {
        warning("More than 1 column in the dataframe detected, using original dataframe")
        return(seurat_data)
    }

    seurat_data <- seurat_data %>%
        mutate({{ col_name }} := factor(.data[[col_name]], levels = names(colors), ordered = TRUE)) %>%
        arrange(.data[[col_name]])

    if (convert_to_character) {
        seurat_data <- seurat_data %>%
            mutate({{ col_name }} := as.character(.data[[col_name]]))
    }
    seurat_data
}


#' remove_spaces_and_lower_case
#'   Standardize data group name (clusters or pathology) by removing spaces and converting to lower case
#'
#' @param group - char - group name
#'
#' @return char - standardized name
remove_spaces_and_lower_case <- function(group) {
    gsub("[[:space:]]", "", group) %>% tolower()
}


#' add_cx_common_config
#'   Add different plots common configurations to passed CX plot
#'
#' @param cxObject - CX plot object
#'
#' @return CX Object
add_cx_common_config <- function(cxObject) {
    result <- cxObject

    if (!is.null(result) && is.list(result) && !is.null(result$x) && !is.null(result$x$config)) {

        common_config <- list(
            graphType                = "Scatter2D",
            scatterType              = "visium",
            backgroundType           = "panel",

            xAxisMajorTicks          = FALSE,
            xAxisMinorTicks          = FALSE,
            xAxisShow                = FALSE,
            yAxisMajorTicks          = FALSE,
            yAxisMinorTicks          = FALSE,
            yAxisShow                = FALSE,
            noValidate               = TRUE,
            printMagnification       = 3,
            selectionColor           = "#000000",
            legendBackgroundColor    = "#FFFFFF",
            legendTitleAlign         = "center",
            titleAlign               = "center",
            subtitleAlign            = "center",

            zoomDisable              = TRUE,
            disableWheel             = TRUE
        )

        for (item in names(common_config)) {
            # do not override a value set in the chart configuration
            if (is.null(result$x$config[[item]])) {
                result$x$config[item] <- common_config[[item]]
            }
        }
    }
    result
}


#' add_cx_large_chart_config
#'     - large plot format used in the single overview and pathology tabs
#'
#' @param cx_plot - canvasXpress plot object
#'
#' @return canvasXpress plot object
add_cx_large_chart_config <- function(cx_plot) {
    cx_plot %>%
        canvasXpress(disableToolbar            = FALSE,
                     toolbarItems              = c("Save", "History", "Table", "Explore", "Lasso", "Customize", "Maximize"),
                     showLegend                = TRUE,
                     colorByShowLegend         = TRUE,
                     showLegendTitle           = FALSE,
                     titleScaleFontFactor      = 0.7,
                     subtitleScaleFontFactor   = 0.6,
                     legendTextScaleFontFactor = 1)
}


#' add_cx_small_chart_config
#'     - small plot format used in the single extended tab
#'
#' @param cx_plot - canvasXpress plot object
#'
#' @return canvasXpress plot object
add_cx_small_chart_config <- function(cx_plot) {
    cx_plot %>%
        canvasXpress(toolbarItems              = c("Save", "Lasso", "Customize", "Maximize"),
                     showLegend                = TRUE,
                     colorByShowLegend         = TRUE,
                     showLegendTitle           = FALSE,
                     titleScaleFontFactor      = 0.9,
                     legendTextScaleFontFactor = 0.7)
}
