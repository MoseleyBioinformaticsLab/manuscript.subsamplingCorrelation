options(repos=structure(c(CRAN="https://cloud.r-project.org")),
        devtools.desc.author = 'person(given = c("Robert", "M"), family = "Flight", email = "rflight79@gmail.com", role = c("aut", "cre"))',
        devtools.desc.license = "MIT",
        blogdown.author = "Robert M Flight",
        blogdown.ext = ".Rmd",
        blogdown.subdir = "post",
        servr.daemon = TRUE,
        future.fork.enable = TRUE,
        tibble.width = Inf,
        tibble.print_min = 20)

system2("clear")
cat("\014")
cli::cli_alert(R.version.string)
cli::cli_alert(paste("Running under", utils::osVersion))
cli::cli_alert(paste("System time is", as.character(lubridate::now())))


setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(type='cairo'))
options(device='x11')

q = function (save = 'no', ...) base::q(save = save, ...)

source("renv/activate.R")
source("./packages.R")
