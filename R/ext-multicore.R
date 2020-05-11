
RAVEWorkers <- R6::R6Class(
  classname = 'RAVEWorkers',
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    # worker list
    low = list(),
    median = list(),
    high = list(),
    local = NULL,
    nworkers = 1L,
    init_timeout = 200,
    grace = 5,

    initialize = function(){
    },

    stop_workers = function(conns, signal = TRUE, grace = 5){
      if(inherits(conns, 'wsdb_client')){
        conns = list(conns)
      }
      if(!length(conns)){
        return()
      }
      stopifnot2(is.list(conns), msg = 'stop_workers must be a list of connections')
      ports <- unlist(lapply(conns, '[[', 'port'))
      if(signal){
        lapply(conns, function(conn){
          if(is.null(conn)){return()}
          tryCatch({
            wsdb::send_quit(conn)
            close(conn)
          }, error = function(e){
            ravecore::rave_warn(e$message)
          })
        })
        later::later(function(){
          lapply(ports, function(port){
            try({
              wsdb::wsdb_stop_server(port = port, signal = FALSE)
            })
          })
        }, delay = grace)
      } else {
        lapply(ports, function(port){
          wsdb::wsdb_stop_server(port = port, signal = FALSE)
        })
      }
    },

    clear_workers = function(conns){
      for(con in conns){
        if(inherits(con, 'wsdb_client')){
          close(con, all = TRUE)
        }
      }
    },

    finalize = function(){
      self$stop_workers(self$low, signal = FALSE)
      self$stop_workers(self$median, signal = FALSE)
      self$stop_workers(self$high, signal = FALSE)
    },



    start_local_worker = function(){
      if(!length(self$local)){
        self$local = list(wsdb::wsdb_client(
          port = wsdb::wsdb_active_ports(include_data_server = TRUE, test = FALSE)[[1]]
        ))
      }

    },

    scale_low = function(n){
      # make sure at least one if multi-core
      if(self$nworkers > 1 && n < 1){
        n = 1
      }
      if(n >= 1){
        connections <- wsdb::wsdb_ensure_connection(
          size = n, max_pool = n, strategy = 'free',
          timeout_each = self$init_timeout, worker_group = 'ravecore-workers-internals-low-priority'
        )
        new_ports <- sapply(connections, '[[', 'port')
      }else{
        connections <- list()
        new_ports <- NULL
      }
      self$stop_workers(lapply(self$low, function(conn){
        if(conn$port %in% new_ports){ return() }
        conn
      }), grace = self$grace)

      self$low <- connections

    },
    scale_median = function(n){
      if(n >= 1){
        connections <- wsdb::wsdb_ensure_connection(
          size = n, max_pool = n, strategy = 'free',
          timeout_each = self$init_timeout, worker_group = 'ravecore-workers-internals-median-priority'
        )
        new_ports <- sapply(connections, '[[', 'port')
      }else{
        connections <- list()
        new_ports <- NULL
      }
      self$stop_workers(lapply(self$median, function(conn){
        if(conn$port %in% new_ports){ return() }
        conn
      }), grace = self$grace)

      self$median <- connections
    },
    scale_high = function(n){
      if(n >= 1){
        connections <- wsdb::wsdb_ensure_connection(
          size = n, max_pool = n, strategy = 'free',
          timeout_each = self$init_timeout, worker_group = 'ravecore-workers-internals-high-priority'
        )
        new_ports <- sapply(connections, '[[', 'port')
      }else{
        connections <- list()
        new_ports <- NULL
      }
      self$stop_workers(lapply(self$high, function(conn){
        if(conn$port %in% new_ports){ return() }
        conn
      }), grace = self$grace)

      self$high <- connections
    },

    rescale = function(){
      self$nworkers <- as.integer(rave_options('max_worker'))
      if(self$nworkers == 1L){
        # remove all workers because you don't want multicore
        self$scale_low(0L)
        self$scale_median(0L)
        self$scale_high(0L)
      } else {
        n_low <- ceiling(self$nworkers / 3)
        n_median <- max(0L, floor(self$nworkers / 3 * 2) - n_low)
        n_high <- self$nworkers - n_median - n_low
        self$scale_low(n_low)
        self$scale_median(n_median)
        self$scale_high(n_high)
      }
      self$start_local_worker()

    },

    ensure_scale = function(){
      if(self$need_rescale()){
        self$rescale()
      }
    },

    get_workers = function(type = c('low', 'median', 'high')){
      self$ensure_scale()

      if(self$single_core()){
        return(list(
          idle = self$local,
          busy = NULL
        ))
      }
      type = match.arg(type)

      connections_to_test <- switch (type,
        'low' = { self$low },
        'median' = { c(self$median, self$low) },
        'high' = { c(self$high, self$median, self$low) }
      )

      counts <- sapply(connections_to_test, function(conn){
        wsdb::wsdb_server_status(conn)
      })
      list(
        idle = connections_to_test[counts < 1],
        busy = connections_to_test[counts >= 1]
      )

    },

    single_core = function(){
      self$nworkers == 1
    },
    need_rescale = function(){
      if(!length(self$local)){ return(TRUE) }
      if(self$nworkers != as.integer(rave_options('max_worker'))){
        return(TRUE)
      }
      n_async = self$nworkers
      if(n_async == 1){ n_async = 0L }

      if( (self$n_low() + self$n_median() + self$n_high()) != n_async ){
        return(TRUE)
      }
      return(FALSE)
    },
    n_low = function(){
      length(self$low)
    },
    n_median = function(){
      length(self$median)
    },
    n_high = function(){
      length(self$high)
    }

  )
)

#' @export
.rave_workers <- RAVEWorkers$new()

get_workers <- function(max, priority = 'low', busy_ok = (priority != 'high')){
  stopifnot2(max >= 1, msg = 'worker count requested must be >= 1')
  .rave_workers$nworkers = rave_options('max_worker')
  .rave_workers$ensure_scale()
  workers = .rave_workers$get_workers(priority)
  if(!length(workers$idle) && busy_ok){
    # then busy is ok too,
    conns <- workers$busy[order(sapply(workers$busy, wsdb::wsdb_server_status))]
  } else {
    conns <- workers$idle
  }
  if(length(conns) > max){
    conns <- conns[seq_len(max)]
  }
  conns
}

#' @export
async_lapply_dedicated <- function(x, FUN, ..., ncores = 0, .callback = NULL){
  if(ncores <= 0){
    ncores = min(length(x), rave_options('max_worker'))
  }
  # get dedicated workers and make sure they are all closed
  wsdb::wsdb_stop_server(group = 'ravecore-workers-internals-dedicated', signal = FALSE)
  conns = wsdb::wsdb_ensure_connection(
    size = ncores, max_pool = ncores, strategy = 'free',
    timeout_each = .rave_workers$init_timeout,
    worker_group = 'ravecore-workers-internals-dedicated'
  )
  on.exit({
    try({
      .rave_workers$stop_workers(conns, signal = FALSE)
    })
  }, add = TRUE, after = TRUE)
  # start workers
  wsdb::wsdb_map(x, FUN, ..., .workers = conns, .callback = .callback)
}

#' @export
async_lapply <- function(x, FUN, ..., priority = 'low', max_core = length(x), .callback = NULL){
  if(!length(x)){
    return(list())
  }
  conns <- get_workers(max_core, priority = priority)
  if(!length(conns)){
    raveutils::rave_info('Cannot find idle workers with priority:{priority}, use main thread...')
    lapply(x, FUN, ...)
  } else {
    wsdb::wsdb_map(x, FUN, ..., .workers = conns, .callback = .callback)
  }
}

#' @export
async_job <- function(FUN, ..., priority = 'low', then = NULL, failure = NULL, ..conns){
  if(!missing(..conns)){
    conns = ..conns
  } else {
    conns <- get_workers(1L, priority = priority)
  }

  if(!length(conns)){
    raveutils::rave_info('Cannot find idle workers with priority:{priority}, use main thread...')
    re <- tryCatch({
      res <- FUN(...)
      if(is.function(then)){
        res <- then(res)
      }
    }, error = function(e){
      if(is.function(failure)){
        failure(e)
      }
    })
    return(re)
  } else {
    wsdb::send_exec(conns[[1]], fun = FUN, fun_args = list(...), then = then,
                    failure = failure, wait = FALSE, .clear_data = TRUE)
  }
}


# rave_options(max_worker = 1)
# system.time({async_lapply_dedicated(1:10, function(x){Sys.sleep(0.1); x+1})})
