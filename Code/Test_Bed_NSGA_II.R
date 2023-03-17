# Run Same Problem w/ NSGA-II Algorithm -----------------------------------
nsgaCostFn <- function(x) {
return(CostFunction(sol = x,
                logic = F,
                nsga = T))
}
test_nsga2 <-
mcnsga2(
    fn = nsgaCostFn,
    varNo = nVar,
    objDim = length(optim_type),
    lowerBounds = rep(0, nVar),
    upperBounds = rep(, nVar),
    popSize = 40,
    generations = 100,
    cprob = 0.7,
    mprob = 0.2
)

saveRDS(test_nsga2,
        file = file.path(res_dir, 'NSGA_II_results.rds'))
idx <-
apply(
    X = unique(t(apply(test_nsga2$parameters,1,decode,test_bench = T))),
    MARGIN = 1,
    FUN = function(a) apply(t(apply(test_nsga2$parameters,1,decode,test_bench = T)), 1, function(b) all(a == b))
)
idx <- which(!duplicated(apply(idx,1,which)))
nsga_sols <- test_nsga2$parameters[idx,]
nsga_pareto_front <- lapply(seq_along(idx),function(x) update_sol(i = x,sol_vector = nsga_sols[x,]))
nsga_pareto_front <- gen_candidates(candidate_list = nsga_pareto_front)

saveRDS(
list(
    `full_results` = test_nsga2,
    `pareto_front_plot` = plotParetoFront(inputData = nsga_pareto_front),
    `nsga_pareto_front` = nsga_pareto_front
),
file = file.path(res_dir, 'NSGA_II_results.rds')
)
