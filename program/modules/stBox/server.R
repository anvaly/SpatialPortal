#' stBoxServer
#'   Server functionality for spatial transcriptomics visualization box module
#'
#' @param id                  - box id
#' @param sg_genes_choices    - list of genes for each available organism
#' @param signature_choices   - character vector of all gene signature names
#' @param signatures          - data.frame of genes for each gene signature
#' @param samples             - data.frame of metadata for all available samples
#' @param st_objects          - list of st_data_class objects for all available samples
#' @param selected_dataset    - name of initial dataset selection
#' @param selected_signature  - name of initial gene signature selection
#' @param selected_sample     - name of initial sample selection
#' @param selected_singleGene - name of initial single gene selection
#' @param logger              - reactive logger S4 object
#'
#' @return reactive expression containing a logical - TRUE if the UI box is open, FALSE if it has been closed
stBoxServer <- function(id,
                        sg_genes_choices,
                        signature_choices,
                        signatures,
                        samples,
                        st_objects,
                        selected_dataset    = character(0),
                        selected_signature  = character(0),
                        selected_sample     = character(0),
                        selected_singleGene = character(0),
                        logger) {
    moduleServer(id, function(input, output, session) {
        internal <- reactiveValues()
        internal$id            <- id
        internal$valid         <- TRUE
        internal$datasetSel    <- selected_dataset
        internal$signatureSel  <- selected_signature
        internal$sampleSel     <- selected_sample
        internal$singleGeneSel <- selected_singleGene
        internal$plotScaling   <- "native"
        internal$bkg_img_file  <- tempfile(fileext = ".png")

        update_dataset_selectized_input(session, samples, internal$datasetSel)
        update_samples_selectized_input(session, samples, internal$datasetSel, internal$sampleSel)
        update_signatures_selectized_input(session, signatures, signature_choices, internal$signatureSel)
        update_singleGene_selectized_input(session, sg_genes_choices, internal$datasetSel, internal$singleGeneSel)

        output$boxTitle <- renderText({
            "Create a Spatial Plot"
        })

        output$signatureGeneLbl <- renderText({
            "Signature/Gene: "
        })

        output$organismTooltip <- renderUI({
            ui_tooltip(session$ns("organismTooltip"),
                       "Dataset",
                       "Choose from the available Organisms and Tissues")
        })

        output$plotScalingRadioTT <- renderUI({
            tags$span(class = "periscope-input-label-with-tt",
                      "Color Scale",
                      shiny::img(id     = session$ns("plotScalingRadioTT"),
                                 src    = isolate(periscope:::.g_opts$tt_image),
                                 height = isolate(periscope:::.g_opts$tt_height),
                                 width  = isolate(periscope:::.g_opts$tt_width)),
                      bsTooltip(id        = session$ns("plotScalingRadioTT"),
                                placement = "left",
                                options   = list("data-html" = "true"),
                                glue('Plots display pre-calculated normalized expression values (SCT). ',
                                     'The options modify the range covered by the colormap:<br/><br/>',
                                     '<b>Native</b>: Full data range &nbsp;&nbsp;&nbsp;<br/>',
                                     '<b>Trimmed</b>: 1st to 99th percentile<br/>',
                                     '<b>Fixed</b>: Fixed range of [0-1]<br/>',
                                     '<b>Custom</b>: User specified min/max')))
        })

        output$sampleTooltip <- renderUI({
            ui_tooltip(session$ns("sampleTooltip"),
                       "Sample",
                       "Choose from the available samples")
        })

        output$signatureTooltip <- renderUI({
            ui_tooltip(session$ns("signatureTooltip"),
                       "Gene Signature",
                       "Choose from custom gene signatures.<br/><i>Search for signatures containing a gene of interest by typing that gene\\'s name.</i>")
        })

        output$geneTooltip <- renderUI({
            ui_tooltip(session$ns("geneTooltip"),
                       "Single Gene",
                       "Choose from genes available in the sample.<br/><i>Search for genes of interest by typing that gene\\'s name.</i>")
        })


        observeEvent(input$closeBoxBtn, {
            internal$valid <- FALSE
        }, ignoreInit = TRUE)

        observeEvent(input$viewToggleBtn_std, {
            if (!is.null(input$viewToggleBtn_std) && !is.null(input$viewTypeTabs)) {
                switch_view_type(session, standard = input$viewToggleBtn_std)
            }
        }, ignoreInit = TRUE)

        observeEvent(input$viewToggleBtn_adv, {
            if (!is.null(input$viewToggleBtn_adv) && !is.null(input$viewTypeTabs)) {
                switch_view_type(session, standard = !(input$viewToggleBtn_adv))
            }
        }, ignoreInit = TRUE)

        observeEvent(input$datasetSel, {
            sample     <- character(0)
            signature  <- character(0)
            singleGene <- character(0)

            if (all(!is.null(internal$datasetSel),
                    length(internal$datasetSel) != 0,
                    internal$datasetSel != "",
                    input$datasetSel == internal$datasetSel)) {
                # this is a cloned box being created, use the preset values
                sample     <- internal$sampleSel
                signature  <- internal$signatureSel
                singleGene <- internal$singleGeneSel
            }

            hideFeedback("datasetSel")

            # get selected organism samples
            update_samples_selectized_input(session, samples, input$datasetSel, sample)
            update_singleGene_selectized_input(session, sg_genes_choices, input$datasetSel, singleGene)

            # clear gene and signature inputs if they have data
            if (!is_valid_selection(input$datasetSel) && is_valid_selection(input$signatureSel)) {
                update_signatures_selectized_input(session, signatures, signature_choices, signature)
            }
        })

        observeEvent(input$signatureSel,{
            clear_signature_and_gene_feedback()
        })

        observeEvent(input$singleGeneSel, {
            clear_signature_and_gene_feedback()
        })

        observeEvent(input$sampleSel, {
            hideFeedback("sampleSel")
        })

        observeEvent(input$plotScalingRadio, {
            if (is_valid_selection(input$plotScalingRadio) &&
                (internal$plotScaling != input$plotScalingRadio)) {
                if (input$plotScalingRadio == "custom") {
                    shinyjs::show('customScalingLayout')
                }
                else {
                    shinyjs::hide('customScalingLayout')
                    internal$plotScaling <- input$plotScalingRadio
                }
            }
        })

        observeEvent(input$customScalingBtn, {
            if (is_valid_selection(input$plotScalingRadio) &&
                (input$plotScalingRadio == "custom")) {
                feedbackDanger("customScaling1",
                               (!(is.numeric(type.convert(input$customScaling1, as.is = TRUE))) ||
                                    as.double(input$customScaling1) >= as.double(input$customScaling2)),
                               text = NULL)

                feedbackDanger("customScaling2",
                               (!(is.numeric(type.convert(input$customScaling2, as.is = TRUE))) ||
                                    as.double(input$customScaling2) <= as.double(input$customScaling1)),
                               text = NULL)

                if (is.numeric(type.convert(input$customScaling1)) &&
                    is.numeric(type.convert(input$customScaling2)) &&
                    (as.double(input$customScaling2) > as.double(input$customScaling1))) {
                    internal$plotScaling <- list(input$customScaling1, input$customScaling2)
                }
            }
        })

        observeEvent(c(input$getPlotBtn, internal$plotScaling), {
            feedbackDanger("datasetSel",
                           !is_valid_selection(input$datasetSel),
                           "Dataset must be selected")
            req(input$datasetSel)

            feedbackDanger("sampleSel",
                           !is_valid_selection(input$sampleSel),
                           "Sample must be selected")
            req(input$sampleSel)

            is_only_signature_or_gene <- (is_valid_selection(input$signatureSel) &&
                                              !is_valid_selection(input$singleGeneSel)) ||
                (!is_valid_selection(input$signatureSel) &&
                     is_valid_selection(input$singleGeneSel))

            if (!is_only_signature_or_gene) {
                shinyjs::show(id = "selectionFeedbackDiv", anim = TRUE, animType = "fade")
            }

            feedbackDanger("signatureSel",
                           !is_only_signature_or_gene, "")
            feedbackDanger("singleGeneSel",
                           !is_only_signature_or_gene, "")

            req(is_only_signature_or_gene)

            gene_title <- ""

            if (is_valid_selection(input$signatureSel)) {
                internal$signatureSel  <- input$signatureSel
                internal$singleGeneSel <- NULL
                gene_title             <- internal$signatureSel
                selected_signature     <- signatures[signatures$Name == internal$signatureSel, , drop = F]
                signature_gene_lbl     <- "Signature: "
            } else {
                internal$singleGeneSel <- input$singleGeneSel
                internal$signatureSel  <- NULL
                gene_title             <- internal$singleGeneSel
                signature_gene_lbl     <- "Gene: "
            }

            output$signatureGeneLbl <- renderText({
                signature_gene_lbl
            })

            internal$sampleSel      <- input$sampleSel
            selected_sample         <- st_objects[sapply(st_objects, function(x){x@name == internal$sampleSel})][[1]]
            selected_seurat         <- selected_sample@seurat
            spatial_plot_id         <- UUIDgenerate()
            spatial_plot_img_id     <- glue('{spatial_plot_id}_bkg')
            spatial_plot_adv_img_id <- glue('{spatial_plot_id}_adv_bkg')
            seurat.scored           <- NULL
            sig_gene_name           <- NULL
            features                <- list()
            if (is.null(selected_sample@top_genes)) {
                selected_sample@top_genes <- get_top_represented_genes(selected_seurat)
            }

            overall_top_genes <- get_overall_top_genes(internal$singleGeneSel,
                                                       selected_signature,
                                                       selected_sample@top_genes)

            tryCatch({
                # gene and signature cannot both have values at the same time
                if (!is_valid_selection(internal$singleGeneSel)) {
                    features <- list(selected_signature$Genes[[1]])
                    sig_gene_name <- glue("sig.{selected_signature$Name}")
                } else {
                    gene     <- str_trim(internal$singleGeneSel)
                    features <- list(gene)

                    # make.names is used here because Seurat changes non compliant names
                    # when adding on the module scores, however holds the gene names correctly
                    # on the object so in order to retrieve genes for some plots we have to
                    # not munge the gene names throughout the entire application
                    sig_gene_name <- glue("sig.{make.names(gene)}")
                }

                suppressWarnings({
                    seurat.scored  <- Seurat::AddModuleScore(object   = selected_sample@seurat,
                                                             features = features,
                                                             nbin     = 10,
                                                             name     = make.names(sig_gene_name),
                                                             assay    = "SCT")
                })

                png::writePNG(image  = Seurat::GetImage(selected_sample@seurat, mode = "raw"),
                              target = internal$bkg_img_file)
            },
            error = function(e) {
                warning('Unable to create plot for ', internal$sampleSel, ' due to : ', e)
            })

            output$boxTitle <- renderText({
                glue("{internal$sampleSel}: {gene_title}")
            })

            output$infoSample <- renderText({
                internal$sampleSel
            })

            output$infoSignature <- renderText({
                gene_title
            })

            output$infoGenes <- renderUI({
                HTML(overall_top_genes)
            })

            output$spatialPlot <- renderUI({
                spatial_plot <- create_spatial_plot_cx(
                    plot_img_id   = session$ns(spatial_plot_img_id),
                    seurat        = seurat.scored,
                    sig_gene_name = glue("{sig_gene_name}1"),
                    features      = unlist(features),
                    title         = internal$sampleSel,
                    subtitle      = gene_title,
                    top_genes     = selected_sample@top_genes,
                    scaling       = internal$plotScaling,
                    file_name     = get_download_filename(text = c(internal$sampleSel, gene_title)))

                if (is.null(spatial_plot)) {
                    sp_msg <- "This may occur if the selected gene(s) are not well represented in the sample."
                    if (!is.null(seurat.scored)) {
                        # the plot is empty due to dynamic range limitations
                        sp_msg <- "The signal dynamic range is too small for the chosen gene."
                    }

                    loginfo(glue("Spatial Plot Unavailable for sample: ({internal$sampleSel}) and signature/gene: ({gene_title})"),
                            logger = logger)
                    shinyjs::hide('plotScalingRadio')
                    shinyjs::hide('customScalingLayout')

                    tags$div(style = "text-align:center;
                                      white-space:nowrap;
                                      margin-left:150px;
                                      margin-top:150px;",
                             tags$h4(class = "text-danger",
                                     "Unable to create the requested spatial plot"),
                             tags$small(class = "text-muted",
                                        tags$em(sp_msg,
                                                tags$br(),
                                                "Contact the app author or data owner for more information if needed.")))
                } else {
                    loginfo(glue("Spatial Plot Created for sample: ({internal$sampleSel}) and signature/gene: ({gene_title})"),
                            logger = logger)

                    output[[spatial_plot_img_id]] <- renderImage({
                        list(src         = internal$bkg_img_file,
                             contentType = 'image/png',
                             style       = "display:none;")
                    }, deleteFile = FALSE)

                    output[[spatial_plot_id]] <- renderCanvasXpress(spatial_plot)
                    shinyjs::show('plotScalingRadio')
                    if (is_valid_selection(input$plotScalingRadio) &&
                        (input$plotScalingRadio == "custom")) {
                        shinyjs::show('customScalingLayout')
                    }

                    list(
                        imageOutput(session$ns(spatial_plot_img_id), height = "0px"),
                        canvasXpressOutput(session$ns(spatial_plot_id), height = "450px", width = "600px")
                    )
                }
            })

            output$spatialPlotAdvanced <- renderUI({
                cluster_plot   <- NULL
                dot_box_plot   <- NULL
                tissue_plot    <- NULL
                pathology_plot <- NULL

                spatial_plot <- create_spatial_plot_cx(
                    plot_img_id   = session$ns(spatial_plot_adv_img_id),
                    seurat        = seurat.scored,
                    sig_gene_name = glue("{sig_gene_name}1"),
                    features      = unlist(features),
                    title         = "Spatial",
                    subtitle      = NULL,
                    top_genes     = selected_sample@top_genes,
                    scaling       = internal$plotScaling,
                    file_name     = get_download_filename(text = c(internal$sampleSel, gene_title)))

                if (!is.null(spatial_plot)) {
                    cluster_plot <- create_cluster_plot_cx(
                        plot_img_id = session$ns(spatial_plot_adv_img_id),
                        seurat      = seurat.scored,
                        file_name   = get_download_filename(text = c(internal$sampleSel,"Clusters")))

                    tissue_plot  <- create_tissue_plot_cx(
                        plot_img_id = session$ns(spatial_plot_adv_img_id),
                        seurat      = seurat.scored,
                        file_name   = get_download_filename(text = c(internal$sampleSel,"Tissue")))

                    pathology_plot  <- create_pathology_plot_cx(
                        plot_img_id = session$ns(spatial_plot_adv_img_id),
                        seurat      = seurat.scored,
                        file_name   = get_download_filename(text = c(internal$sampleSel,"Pathology")))

                    if (length(unlist(features)) > 1) {
                        dot_box_plot <- create_dot_plot_cx(
                            seurat     = seurat.scored,
                            features   = unlist(features), # list of genes (used individually)
                            gene_title = gene_title,
                            file_name  = get_download_filename(text = c(internal$sampleSel, gene_title)))
                    } else {
                        dot_box_plot <- create_box_plot_cx(
                            seurat        = seurat.scored,
                            sig_gene_name = glue("{sig_gene_name}1"),  #scored gene
                            gene_title    = gene_title,
                            file_name     = get_download_filename(text = c(internal$sampleSel, gene_title)))
                    }
                }

                if (any(is.null(spatial_plot),
                        is.null(cluster_plot),
                        is.null(dot_box_plot),
                        is.null(pathology_plot),
                        is.null(tissue_plot))) {

                    sp_msg <- "This may occur if the selected gene(s) are not well represented, or extended information (clustering, pathology) is not available in the sample."
                    if (is.null(spatial_plot)) {
                        # the plot is empty due to dynamic range limitations
                        sp_msg <- "The signal dynamic range is too small for the chosen gene."
                    }

                    loginfo(glue("Advanced Plots Unavailable for sample: ({internal$sampleSel}) and signature/gene: ({gene_title})"),
                            logger = logger)

                    tags$div(style = "text-align:center;
                                      margin-top:100px;",
                             tags$h4(class = "text-danger",
                                     style = "text-align:center;",
                                     "Unable to create the requested spatial plots"),
                             tags$small(class = "text-muted",
                                        tags$em(sp_msg,
                                                tags$br(),
                                                "Contact the app author or data owner for more information if needed.")))
                } else {
                    loginfo(glue("Advanced Spatial Plot Created for sample: ({internal$sampleSel}) and signature/gene: ({gene_title})"),
                            logger = logger)

                    # update the background image for the spatial plot to be this tab's content
                    adv_spatial_plot <- spatial_plot
                    adv_spatial_plot$x$config$backgroundImage <- glue("javascript://{session$ns(spatial_plot_adv_img_id)}")

                    output[[spatial_plot_adv_img_id]] <- renderImage({
                        list(src         = internal$bkg_img_file,
                             contentType = 'image/png',
                             style       = "display:none;")
                    }, deleteFile = FALSE)

                    adv_spatial_id <- glue("{spatial_plot_id}_adv")
                    output[[adv_spatial_id]] <- renderCanvasXpress(adv_spatial_plot)

                    adv_cluster_id <- glue("{spatial_plot_id}_clust")
                    output[[adv_cluster_id]] <- renderCanvasXpress(cluster_plot)

                    adv_tissue_id <- glue("{spatial_plot_id}_tissue")
                    output[[adv_tissue_id]] <- renderCanvasXpress(tissue_plot)

                    adv_pathology_id  <- glue("{spatial_plot_id}_pathology")
                    output[[adv_pathology_id]] <- renderCanvasXpress(pathology_plot)

                    adv_dotplot_id <- glue("{spatial_plot_id}_dot_box")
                    output[[adv_dotplot_id]] <- renderCanvasXpress(dot_box_plot)

                    tagList(
                        imageOutput(session$ns(spatial_plot_adv_img_id), height = "0px"),
                        tags$div(style = "display:flex; position:relative; height:250px;",
                                 tags$div(style = "display:flex; position:absolute; justify-content:space-between; align-items:flex-start;
                                                   top:0; left:0; right:0; bottom:0; overflow-x:auto; overflow-y:hidden;",
                                          tags$div(style = "flex:none;",
                                                   canvasXpressOutput(session$ns(adv_spatial_id),
                                                                      height = "225px",
                                                                      width  = "300px")),
                                          tags$div(style = "flex:none;",
                                                   canvasXpressOutput(session$ns(adv_cluster_id),
                                                                      height = "225px",
                                                                      width  = "325px")),
                                          tags$div(style = "flex:none;",
                                                   canvasXpressOutput(session$ns(adv_pathology_id),
                                                                      height = "225px",
                                                                      width  = "345px")),
                                          tags$div(style = "flex:none;",
                                                   canvasXpressOutput(session$ns(adv_tissue_id),
                                                                      height = "225px",
                                                                      width  = "300px"))
                                 )
                        ),
                        tags$div(style = "display:flex; position:relative; height:330px; justify-content:space-around; align-items:center;",
                                 tags$div(style = "display:flex; top:0, left:0; right:0, bottom:0, overflow-x:auto; overflow-y:hidden;",
                                          tags$div(style = "flex:none;",
                                                   canvasXpressOutput(session$ns(adv_dotplot_id),
                                                                      height = "300px",
                                                                      width  = "1250px"))
                                 )
                        )
                    )
                }
            })
        }) #plot button

        return(internal$valid)
    }) #moduleServer
}

# ----------------------------
# --- supporting functions ---


#' get_unique_organism_tissue
#'   Construct a data.frame of unique organism-tissue combinations
#'
#' @param samples - data.frame of samples metadata
#'
#' @return data.frame
get_unique_organism_tissue <- function(samples) {
    results <- NULL
    if (!is.null(samples) && NROW(samples) > 0) {
        results <- samples %>%
            select(Organism, Tissue) %>%
            arrange(Tissue) %>%
            unique() %>%
            unite(col = "Organism_Tissue", sep = " > ", remove = FALSE)
    }
    results
}


#' get_top_represented_genes
#'   Calculate the top represented genes for each spot in a given sample
#'
#' @param data          - sample seurat object
#' @param assay         - name of assay variable in seurat object to use for gene expression
#' @param sct_threshold - threshold for determining top represented genes
#'
#' @return data.frame
get_top_represented_genes <- function(data, assay = "SCT", sct_threshold = 1){
    Seurat::GetAssayData(data, assay = assay) %>%
        as_tibble(rownames = "Symbol") %>%
        gather(key = "Coord", val = "Value", -Symbol) %>%
        filter(Value > sct_threshold)
}


#' get_signature_top_represented_genes
#'   Calculate the overall top N represented genes in a given gene signature for a given sample
#'
#' @param top_genes - data.frame of top represented genes for each spot in the sample
#' @param sig.genes - list of individual genes in the selected gene signature
#'
#' @return data.frame
get_signature_top_represented_genes <- function(top_genes, sig.genes) {
    top_genes %>%
        filter(Symbol %in% sig.genes) %>%
        group_by(Symbol) %>%
        summarise(score_mean = mean(Value)) %>%
        arrange(desc(score_mean)) %>%
        select(Symbol) %>%
        top_n(g_top_n_sig_genes) %>%
        pull(Symbol)
}


#' update_dataset_selectized_input
#'   Update dataset input choices with the datasets from the available samples
#'
#' @param session          - module session object
#' @param samples          - data.frame of metadata for all available samples
#' @param selected_dataset - currently selected dataset
update_dataset_selectized_input <- function(session, samples, selected_dataset = character(0)) {
    choices <- get_unique_organism_tissue(samples)
    updateSelectizeInput(
        session  = session,
        inputId  = "datasetSel",
        server   = TRUE,
        selected = selected_dataset,
        choices  = choices,
        options  = list(placeholder   = "Type/Click then Select",
                        optgroupField = "Organism",
                        labelField    = "Organism_Tissue",
                        valueField    = "Organism_Tissue",
                        searchField   = c("Organism", "Tissue"),
                        sortField     = "Organism",
                        render        = I("{ option: function(item, escape) {
                                           return '<div>' + item.Tissue + '</div>'; }}")
        )
    )
}


#' update_singleGene_selectized_input
#'   Update single gene input choices with the applicable genes for the selected dataset
#'
#' @param session       - module session object
#' @param genes         - list of all genes for each available organism
#' @param datasetSel    - currently selected dataset
#' @param selected_gene - currently selected single gene
update_singleGene_selectized_input <- function(session, genes, datasetSel, selected_gene = character(0)) {
    datasetSel <- ifelse(length(datasetSel) == 0, "", datasetSel)
    choices    <- get_gene_choices(genes, datasetSel)

    updateSelectizeInput(session,
                         "singleGeneSel",
                         server   = TRUE,
                         selected = selected_gene,
                         choices  = choices,
                         options  = list(
                             placeholder = "Type/Click then Select",
                             render      = I("{ option: function(item, escape) {
                                           return '<div><b>' + item.value + '</b></div>'; }}"))
    )
}


#' update_signatures_selectized_input
#'   Update gene signature input choices with the applicable genes for the selected dataset
#'
#' @param session            - module session object
#' @param signatures         - data.frame of genes for each gene signature
#' @param signature_choices  - character vector of all gene signature names
#' @param selected_signature - currently selected gene signature
update_signatures_selectized_input <- function(session, signatures, signature_choices, selected_signature = character(0)) {
    updateSelectizeInput(
        session,
        "signatureSel",
        server   = TRUE,
        selected = selected_signature,
        choices  = signatures[match(signature_choices, signatures$Name),],
        options  = list(
            placeholder = "Type/Click then Select",
            labelField  = "Name",
            searchField = c("Name", "Gene_List"),
            valueField  = "Name",
            render      = I("{ option: function(item, escape) {
                                           return '<div><b>' + item.Name +
                                                  '</b><br>' + '<i><span style=\"display:inline-block;font-size:x-small;line-height:1.3\">' +
                                                  item.Gene_List + '</span></i></div>'; }}")
        )
    )
}


#' update_samples_selectized_input
#'   Update sample input choices with the applicable samples for the selected dataset
#'
#' @param session         - module session object
#' @param samples         - data.frame of metadata for all available samples
#' @param datasetSel      - currently selected dataset
#' @param selected_sample - currently selected sample
update_samples_selectized_input <- function(session, samples, datasetSel, selected_sample = character(0)) {
    datasetSel <- ifelse(length(datasetSel) == 0, "", datasetSel)
    choices    <- get_sample_choices(samples, datasetSel)

    updateSelectizeInput(session,
                         "sampleSel",
                         server   = TRUE,
                         selected = selected_sample,
                         choices  = choices,
                         options  = list(
                             placeholder = "Type/Click then Select",
                             render      = I("{ option: function(item, escape) {
                                           return '<div><b>' + item.value + '</b></div>'; }}"))
    )
}


#' get_sample_choices
#'   Get a character vector of sample names for selected dataset
#'
#' @param samples    - data.frame of metadata for all available samples
#' @param datasetSel - currently selected dataset
#'
#' @return character
get_sample_choices <- function(samples, datasetSel){
    sample_choices  <- character(0)
    organism_tissue <- get_organism_tissue(datasetSel)
    samples <- samples %>%
        filter(Organism %in% organism_tissue[1], Tissue %in% organism_tissue[2])
    if (NROW(samples) > 0) {
        sample_choices <- samples$Sample_Name
    }
    sample_choices
}


#' get_gene_choices
#'   Get a character vector of applicable genes for the selected dataset
#'
#' @param genes      - list of all genes for each available organism
#' @param datasetSel - currently selected dataset
#'
#' @return character or NULL
get_gene_choices <- function(genes, datasetSel) {
    organism <- get_organism_tissue(datasetSel)[1]
    genes[[organism]]
}


#' get_organism_tissue
#'   Get a character vector of organism, tissue for the selected dataset
#'
#' @param datasetSel - currently selected dataset
#'
#' @return character
get_organism_tissue <- function(datasetSel) {
    str_split(datasetSel, pattern = " > ") %>% unlist()
}


#' clear_signature_and_gene_feedback
#'   Clear warning feedback on gene signature and single gene inputs
clear_signature_and_gene_feedback <- function() {
    hideFeedback("signatureSel")
    hideFeedback("singleGeneSel")
    shinyjs::hide(id = "selectionFeedbackDiv")
}


#' is_valid_selection
#'   Determine if selected value is valid
#'
#' @param value
#'
#' @return logical
is_valid_selection <- function(value){
    # selection is valid if it has a real value
    !is.null(value) && value != ""
}


#' get_overall_top_genes
#'   Get the overall top genes for the selected sample and selected gene signature or single gene.
#'   * If a gene signature is selected, get the top N genes within this signature.
#'   * If a single gene is selected, this gene is used.
#'
#' @param singleGeneSel         - currently selected single gene
#' @param selected_signature    - currently selected gene signature
#' @param top_represented_genes - data.frame of top represented genes for each spot in the sample
#'
#' @return character
get_overall_top_genes <- function(singleGeneSel, selected_signature, top_represented_genes) {
    if (is_valid_selection(singleGeneSel)) {
        output <- singleGeneSel
    } else if (length(selected_signature$Genes[[1]]) == 1) {
        output <- selected_signature$Genes[[1]]
    } else {
        output <- get_signature_top_represented_genes(top_represented_genes,
                                                      sig.genes = selected_signature$Genes[[1]])
    }
    glue_collapse(output, sep = "<br/>")
}


#' switch_view_type
#'   Switch chart view in the UI box to standard ("Regular") or advanced ("Extended") view
#'
#' @param session  - module session object
#' @param standard - logical - TRUE for standard view, FALSE for advanced view
#'
#' @return
switch_view_type <- function(session, standard = TRUE) {
    if (standard) {
        updateButton(session, session$ns("viewToggleBtn_std"), value = TRUE,  style = "warning")
        updateButton(session, session$ns("viewToggleBtn_adv"), value = FALSE, style = "default")

        updateTabsetPanel(inputId  = "viewTypeTabs",
                          selected = "Regular")
        updateTabsetPanel(inputId  = "bottomExtension",
                          selected = "Regular_bottom")
    } else {
        updateButton(session, session$ns("viewToggleBtn_std"), value = FALSE, style = "default")
        updateButton(session, session$ns("viewToggleBtn_adv"), value = TRUE,  style = "warning")

        updateTabsetPanel(inputId  = "viewTypeTabs",
                          selected = "Advanced")
        updateTabsetPanel(inputId  = "bottomExtension",
                          selected = "Advanced_bottom")
    }
}
