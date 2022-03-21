library(readr)


st_data_class <- setClass("st_data",
                          slots = list(path      = "character",
                                       name      = "character",
                                       seurat    = "ANY",
                                       top_genes = "ANY"))


#' read_samples_data
#'   Read samples metadata file into a data.frame, add column with sample file names
#'   and remove samples with no sample file.
#'
#' @return data.frame
read_samples_data <- function() {
    sample_meta_file <- glue("{g_metadata_directory}{.Platform$file.sep}{g_sample_meta_file}")
    assert_that(file.exists(sample_meta_file),
                msg = glue("Sample file {sample_meta_file} does not exist"))

    samples <- suppressMessages(read_tsv(sample_meta_file))

    # The required columns might change at any time
    required_columns <- c("Sample", "Organism", "Protocol", "Tissue")
    assert_that(all(required_columns %in% names(samples)),
                msg = glue("All sample columns:{glue_collapse(required_columns, ', ')}
                           must be exist in loaded samples data"))

    samples_files <- read_sample_files()

    # create the sample name based on the organism
    samples <- samples %>%
        mutate(Sample_Name = case_when(Organism == "Rat"   ~ glue("Rat{Tissue}_{Protocol}"),
                                       Organism == "Mouse" ~ glue("{Sample}_{Protocol}"),
                                       TRUE                ~ Sample))

    samples$Sample_File <- apply(samples["Sample_Name"], 1, get_sample_path,
                                 samples_files = samples_files)
    samples %>%
        remove_samples_with_no_files() %>%
        arrange(ID)
}


#' build_st_data_objects
#'   Construct a list of st_data_class objects from available samples
#'
#' @param samples - data.frame of samples metadata
#'
#' @return list
build_st_data_objects <- function(samples) {
    if (NROW(samples) == 0) {
        stop("Could not find any data files, cannot run the application")
    }

    # returns st_data_class objects constructed from available samples
    st_data_objects <- apply(samples, 1, function(x) {
        sobj <- NULL
        tryCatch({
            sobj <- readRDS(x[["Sample_File"]])
        },
        error = function(err) {
            message(glue("Could not read rds for sample {x[['Sample_Name']]} at ",
                         "path {x[['Sample_File']]} due to error: {err}"))
        })

        st_data_class(
            path      = x[["Sample_File"]],
            name      = x[["Sample_Name"]],
            seurat    = sobj,
            top_genes = NULL
        )
    })
}


#' read_signatures
#'   Construct a data.frame of genes for each gene signature
#'
#' @return data.frame
read_signatures <- function() {
    signatures_file <- glue("{g_metadata_directory}{.Platform$file.sep}{g_signatures_file}")
    assert_that(file.exists(signatures_file),
                msg = glue("Signatures file {signatures_file} does not exist"))

    data <- suppressMessages(read_tsv(signatures_file, col_types = 'cc', trim_ws = T))
    data %>%
        rename(Name      = Signature_name,
               Gene_List = Gene_list) %>%
        mutate(Name      = make.names(Name, unique = T),
               Genes     = strsplit(Gene_List, ',', fixed = T),
               Gene_List = gsub(',', ', ', Gene_List, fixed = T))
}


#' get_all_signature_names
#'   Construct a character vector of all gene signature names
#'
#' @param signatures - data.frame of genes for each gene signature
#'
#' @return character
get_all_signature_names <- function(signatures) {
    signatures$Name[order(signatures$Name)] %>% as.character()
}


#' get_organism_genes
#'   Construct a list of genes for each available organism
#'
#' @param samples - data.frame of samples metadata
#'
#' @return list
get_organism_genes <- function(samples){
    genes     <- list()
    organisms <- samples$Organism %>% unique()
    for (organism in organisms) {
        gene_file <- glue("{g_metadata_directory}{.Platform$file.sep}{g_genes_file_prefix}{tolower(organism)}.txt")
        assert_that(file.exists(gene_file),
                    msg = glue("Genes file {gene_file} does not exist"))
        genes[organism] <- suppressMessages(read_tsv(gene_file))
    }
    genes
}


#' read_about
#'   Read HTML content for "About This Application" box in the UI
#'
#' @return html
read_about <- function() {
    assert_that(file.exists("program/data/about.html"),
                msg = "About file does not exist")

    HTML(readLines("program/data/about.html"))
}


#' add_title_to_intro
#'   Add titles to the HTML in the "intro" column of walkthrough data.frame
#'
#' @param intro_dt - data.frame of walkthrough data
#'
#' @return data.frame
add_title_to_intro <- function(intro_dt) {
    intro_dt %>%
        group_by(element) %>%
        mutate(intro = ifelse(is.na(title) || is.null(title) || (title == ""), intro, glue("{tags$h4(title)}{intro}"))) %>%
        select(element, intro, position)
}

# --- Helper Functions ---


#' read_sample_files
#'   Construct a vector of file names for all samples available in the samples directory
#'
#' @return char
read_sample_files <- function() {
    assert_that(dir.exists(g_sample_directory),
                msg = glue("Sample location does not exist: {g_sample_directory}"))

    sample_files <- list.files(g_sample_directory,
                               pattern    = glue(".rds$"),
                               ignore.case = T)

    assert_that(length(sample_files) >= 1,
                msg = glue("There are no samples at: {g_sample_directory}"))

    sample_files
}


#' get_sample_path
#'   Construct the full path of the data file for a sample
#'
#' @param sample_name
#' @param samples_files - vector of file names for all available samples
#'
#' @return character
get_sample_path <- function(sample_name, samples_files) {
    file_path <- ""

    if (grepl("slice", sample_name)) {
        slice_number <- substr(sample_name, regexpr("slice", sample_name) + 5,
                               nchar(sample_name))
        file_name <- glue("{sample_name}{slice_number}.rds")
    } else {
        file_name <- glue("{sample_name}.rds")
    }
    if (file_name %in% samples_files) {
        file_path <- glue("{g_sample_directory}{.Platform$file.sep}{file_name}")
    }
    file_path
}


#' remove_samples_with_no_files
#'   Remove samples with no data files from the data.frame of samples metadata
#'
#' @param samples - data.frame of samples metadata
#'
#' @return data.frame
remove_samples_with_no_files <- function(samples) {
    samples %>%
        filter(Sample_File != "")
}
