set.seed(20962)
gene_names <- paste0("gene-", 1:10)
gene_ids <- paste0("ENSTEST00000", 1:10)
sample_names <- paste0("sample-", 1:10)
samples <- paste0('0000', 1:10)
counts <- matrix(runif(100), nrow = 10,
  dimnames = list(genes = gene_names,
  samples = sample_names))

gene_metadata <- tibble::tibble(
  Name = gene_names,
  GeneID = gene_ids
)
sample_info <- tibble::tibble(
  sample = samples,
  sampleName = sample_names
)
# make_heatmap_plot(counts, sample_info, gene_metadata, "turbo")

test_that("get_gene_metadata works", {
  expect_equal(get_gene_labels(gene_metadata), gene_metadata$Name)
  expect_equal(get_gene_labels(dplyr::select(gene_metadata, -Name)), gene_metadata$GeneID)
})

test_that("get_sample_labels works", {
  expect_equal(get_sample_labels(sample_info, sample_info$sample), sample_info$sampleName)
  expect_equal(
    get_sample_labels(dplyr::select(sample_info, -sampleName), sample_info$sample),
    sample_info$sample)
})

#
