.ess_dbg_getTracedAndDebugged <-
function(){
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    generics <- methods::getGenerics()
    all_traced <- c()
    for(i in seq_along(generics)){
        genf <- methods::getGeneric(generics[[i]], package=generics@package[[i]])
        if(!is.null(genf)){ ## might happen !! v.2.13
            menv <- methods::getMethodsForDispatch(genf)
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                all_traced <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), all_traced)
            if(!is.null(tfn<-getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv))&&is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                all_traced <- c(generics[[i]], all_traced)
        }
    }
    debugged <- apropos('.', mode = 'function')
    ## traced function don't appear here. Not realy needed and would affect performance.
    debugged <- debugged[which(unlist(lapply(debugged, isdebugged) , recursive=FALSE, use.names=FALSE))]
    unique(c(debugged, all_traced))
    }
.ess_dbg_UndebugALL <-
function(funcs){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
    }
.ess_dbg_UntraceOrUndebug <-
function(name){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        ## name is a name of a function to be undebugged or has a form name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }
    }
.ess_get_completions <-
function(string, end){
  if(getRversion() > '2.14.1'){
    comp <- compiler::enableJIT(0L)
    olderr <- getOption('error')
    options(error=NULL)
    on.exit({options(error = olderr)
             compiler::enableJIT(comp)})
  }
  utils:::.assignLinebuffer(string)
  utils:::.assignEnd(end)
  utils:::.guessTokenFromLine()
  utils:::.completeToken()
  c(get('token', envir=utils:::.CompletionEnv),
    utils:::.retrieveCompletions())
  }
.ess_log_eval <-
function(log_name){
    if(!exists(log_name, envir = .GlobalEnv, inherits = FALSE))
        assign(log_name, list(), envir = .GlobalEnv)
    log <- get(log_name, envir = .GlobalEnv, inherits = FALSE)
    .essWEnames <- allNames(.ess_watch_expressions)
    cur_log <- list()
    .parent_frame <- parent.frame()
    for(i in seq_along(.ess_watch_expressions)){
        capture.output({
        cur_log[[i]] <-
            tryCatch(eval(.ess_watch_expressions[[i]]), envir = .parent_frame,
                     error = function(e) paste('Error:', e$message, '
'),
                     warning = function(w) paste('warning: ', w$message, '
'))
        if(is.null(cur_log[i][[1]]))
            cur_log[i] <- list(NULL)
                   })
    }
    names(cur_log) <- .essWEnames
    assign(log_name, c(log, list(cur_log)), envir = .GlobalEnv)
    invisible(NULL)
}
.ess_watch_eval <-
function(){
    if(!exists('.ess_watch_expressions')){
        assign('.ess_watch_expressions', list(), envir = .GlobalEnv)
    }
    if(length(.ess_watch_expressions) == 0L){
        cat('
# Watch list is empty!

# a       append new expression
# i       insert new expression
# k       kill
# e       edit the expression
# r       rename
# n/p     navigate
# u/d,U   move the expression up/down
# q       kill the buffer
')
    }else{
        .parent_frame <- parent.frame()
        .essWEnames <- allNames(.ess_watch_expressions)
        len0p <- !nzchar(.essWEnames)
        .essWEnames[len0p] <- seq_along(len0p)[len0p]
        for(i in seq_along(.ess_watch_expressions)){
            cat('
@---- ', .essWEnames[[i]], ' ', rep.int('-', max(0, 35 - nchar(.essWEnames[[i]]))), '-@
', sep = '')
            cat( paste('@---:', deparse(.ess_watch_expressions[[i]][[1L]])), ' 
', sep = '')
            tryCatch(print(eval(.ess_watch_expressions[[i]], envir = .parent_frame)),
                     error = function(e) cat('Error:', e$message, '
' ),
                     warning = function(w) cat('warning: ', w$message, '
' ))
        }}
}
.ess_watch_expressions <-
list()
.ess.funargs <-
function(object){
  funname <- deparse(substitute(object))
  if(getRversion() > '2.14.1'){
    comp <- compiler::enableJIT(0L)
    olderr <- getOption('error')
    options(error=NULL)
    on.exit({
      compiler::enableJIT(comp)
      options(error = olderr)
    })
  }
  fun <- tryCatch(object, error=function(e) NULL) ## works for special objects also
  if(is.null(fun)) NULL
  else if(is.function(fun)) {
    special <- grepl('[:$@[]', funname)
    args <- if(!special){
      fundef <- paste(funname, '.default',sep='')
      do.call('argsAnywhere', list(fundef))
    }

    if(is.null(args))
      args <- args(fun)
    if(is.null(args))
      args <- do.call('argsAnywhere', list(funname))

    fmls <- formals(args)
    fmls_names <- names(fmls)
    fmls <- gsub('\"', '\\\"', as.character(fmls), fixed=TRUE)
    args_alist <- sprintf("'(%s)", paste("(\"", fmls_names, "\" . \"", fmls, "\")", sep = '', collapse = ' '))
    allargs <-
      if(special) fmls_names
      else tryCatch(gsub('=', '', utils:::functionArgs(funname, ''), fixed = T), error=function(e) NULL)
    allargs <- sprintf("'(\"%s\")", paste(allargs, collapse = '\" \"'))
    envname <- environmentName(environment(fun))
    cat(sprintf('(list \"%s\" %s %s)\n', envname, args_alist, allargs))
  }
}
.help.ESS <-
function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) 
{
    types <- c("text", "html", "pdf")
    if (!missing(package)) 
        if (is.name(y <- substitute(package))) 
            package <- as.character(y)
    if (missing(topic)) {
        if (!missing(package)) {
            help_type <- if (!length(help_type)) 
                "text"
            else match.arg(tolower(help_type), types)
            if (interactive() && help_type == "html") {
                if (tools:::httpdPort == 0L) 
                  tools::startDynamicHelp()
                if (tools:::httpdPort <= 0L) 
                  return(library(help = package, lib.loc = lib.loc, 
                    character.only = TRUE))
                browser <- if (.Platform$GUI == "AQUA") {
                  function(x, ...) {
                    .Internal(aqua.custom.print("help-files", 
                      x))
                    return(invisible(x))
                  }
                }
                else getOption("browser")
                browseURL(paste("http://127.0.0.1:", tools:::httpdPort, 
                  "/library/", package, "/html/00Index.html", 
                  sep = ""), browser)
                return(invisible())
            }
            else return(library(help = package, lib.loc = lib.loc, 
                character.only = TRUE))
        }
        if (!missing(lib.loc)) 
            return(library(lib.loc = lib.loc))
        topic <- "help"
        package <- "utils"
        lib.loc <- .Library
    }
    ischar <- tryCatch(is.character(topic) && length(topic) == 
        1L, error = identity)
    if (inherits(ischar, "error")) 
        ischar <- FALSE
    if (!ischar) {
        reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
            "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if (!is.name(substitute(topic)) && !stopic %in% reserved) 
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }
    help_type <- if (!length(help_type)) 
        "text"
    else match.arg(tolower(help_type), types)
    paths <- index.search(topic, find.package(package, lib.loc, 
        verbose = verbose))
    tried_all_packages <- FALSE
    if (!length(paths) && is.logical(try.all.packages) && !is.na(try.all.packages) && 
        try.all.packages && missing(package) && missing(lib.loc)) {
        for (lib in .libPaths()) {
            packages <- .packages(TRUE, lib)
            packages <- packages[is.na(match(packages, .packages()))]
            paths <- c(paths, index.search(topic, file.path(lib, 
                packages)))
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }
    paths <- unique(paths)
    attributes(paths) <- list(call = match.call(), topic = topic, 
        tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "help_files_with_topic"
    paths
}
