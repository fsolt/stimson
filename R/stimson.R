library(tidyverse)
library(rstan)
library(beepr)

### Delete these when turning into a function
seed <- 324
iter <- 3000
chains <- 4
cores <- chains
x <- ukmp
robust <- FALSE
constant_alpha <- FALSE
###

rq <- x %>%
  group_by(rcode) %>%
  summarize(rq = first(qcode),
            rcp = max(cutpoint))

dcpo_data <- list(  K    = max(x$ccode),
                    T    = max(x$tcode),
                    Q    = max(x$qcode),
                    R    = max(x$rcode),
                    N    = length(x$y_r),
                    kk   = x$ccode,
                    tt   = x$tcode,
                    kktt = x$ktcode,
                    qq   = x$qcode,
                    rr   = x$rcode,
                    rq   = rq$rq,
                    rcp  = rq$rcp,
                    y_r  = x$y_r,
                    n_r  = x$n,
                    rob  = as.numeric(robust),
                    c_a  = as.numeric(constant_alpha)
)

start <- proc.time()
out1 <- stan(file = "../DCPO/R/dcpo.stan",
             data = dcpo_data,
             seed = seed,
             iter = iter,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20))
runtime <- proc.time() - start
runtime/60

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(out1, file = str_c("data/output_", str_replace(Sys.time(), " ", "_"), ".rda"))

#Chime
beep()
