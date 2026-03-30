# Render a lab template .qmd file, returning a list with:
#   $yaml: character vector of YAML front matter lines
#   $m:    output of knitr::knit_child() on the body (use with cat(out$m, sep = "\n"))
#
# Arguments:
#   f           path to the lab template .qmd file
#   show_yaml   if TRUE, knit_child runs with include = FALSE (body only renders
#               output, not code); if FALSE, include = TRUE. Matches existing
#               usage: bring-it-together uses show_yaml = TRUE, all others FALSE.

render_lab <- function(f, show_yaml = FALSE) {
    l <- readLines(f)

    # locate yaml block (first pair of `---`)
    yaml_lines <- grep("^---$", l)
    iyaml <- yaml_lines[1]:yaml_lines[2]

    # yaml as character vector
    y <- l[iyaml]

    # remove yaml and "delete this once complete" instructions from body
    idelt <- grep("delete this .* once complete", l, ignore.case = TRUE)
    b <- l[-c(iyaml, idelt)] |>
        gsub("\\*delete this .* once complete\\*", "", x = _,
             ignore.case = TRUE)

    fb <- tempfile(fileext = ".qmd")
    writeLines(b, fb)

    m <- knitr::knit_child(fb,
                           options = list(include = !show_yaml),
                           quiet = TRUE)

    list(yaml = y, m = m)
}
