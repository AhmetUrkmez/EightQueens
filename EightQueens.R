Population <- function(N) {
    population <- matrix(nrow = N, ncol = 8)

    for(i in 1: N) {
        population[i, ] <- sample(c(1: 8))
    }

    return(population)
}

Objective <- function(population) {
    n <- nrow(population)
    objective <- numeric(n)

    for(i in 1: n) {
        count <- 0
        row_pos <- population[i, ]
        
        for(j in 1: 7) {
            for(k in (j + 1): 8) {
                if ((row_pos[j] - j) == (row_pos[k] - k)) {
                    count <- count + 1
                }
                if ((row_pos[j] + j) == (row_pos[k] + k)) {
                    count <- count + 1
                }
            }
        }
        
        objective[i] <- count
    }
    return(objective)
}

Roulette <- function(objective, population, N) {
    objective <- 1/objective
    probs <- objective/sum(objective) 
    c.probs <- cumsum(probs)
    offspring <- matrix(nrow = N, ncol = 8)

    for(i in 1: N) {
        offspring[i, ] <- population[which(c.probs >= runif(1))[1], ]
    }

    return(offspring)
}

CrossOver <- function(offspring, N, p.cross) {
    pairs <- sample(N)

    for(i in 1: (N/2)) {
        parent1 <- offspring[pairs[2*i - 1], ]
        parent2 <- offspring[pairs[2*i], ]
        if (runif(1) < p.cross) {
            c.point <- sample(4 + 1, 1)
            c.piece <- (c.point): (c.point + 4 - 1)
            dummy <- parent1
            index1 <- which(!parent2 %in% parent1[c.piece])
            index2 <- which(!parent1 %in% parent2[c.piece])
            parent1[which(!1: 8 %in% c.piece)] <- parent2[index1]
            parent2[which(!1: 8 %in% c.piece)] <- dummy[index2]
            offspring[pairs[2*i - 1], ] <- parent1
            offspring[pairs[2*i], ] <- parent2
        }
    }

    return(offspring)
}

Mutation <- function(offspring, N, p.mutation) {
    chromatide <- sample(N)
    
    for(i in 1: N) {
        if(runif(1) < p.mutation) {
            index <- sample(8, 2)
            dummy <- offspring[chromatide[i], ]
            offspring[chromatide[i], ][index[1]] <- offspring[chromatide[i], ][index[2]]
            offspring[chromatide[i], ][index[2]] <- dummy[index[1]]
        }
    }
    
    return(offspring)
}

GA <- function(N, p.cross, p.mutation) {
    iter <- 1
    population <- Population(N)
    best_fitness <- Inf

    while(1) {
        objective <- Objective(population)

        if(min(objective) < best_fitness) {
            best_chromosome <- population[which(objective == min(objective)), ]
            best_fitness <- min(objective)
            if (best_fitness == 0) {
                break
            }
        }

        offspring <- Roulette(objective, population, N)
        offspring <- CrossOver(offspring, N, p.cross)
        population <- Mutation(offspring, N, p.mutation)

        iter <- iter + 1
    }

    board_design <- matrix(0, ncol = 8, nrow = 8)
    
    for(i in 1: 8) {
        board_design[, i][best_chromosome[i]] <- 1
    }

    return(list("best.chromosome" = best_chromosome[c(1: 8)],
                "best.fitness" = best_fitness,
                "iter" = iter,
                "board.design" = board_design))
}

GA(50, .95, .95)

Benchmark <- function(...) {
    iter <- numeric(1000)
    pb <- progress::progress_bar$new(
        format = "[:bar] :percent",
        total = 1000, clear = FALSE, width = 60
        )

    start <- Sys.time()

    for(i in 1: 1000) {
        iter[i] <- GA(50, .95, .95)$iter
        pb$tick()
    }
    
    end <- Sys.time()

    return(list("mean.iter" = mean(iter),
                "var.iter" = var(iter),
                "max.iter" = max(iter),
                "min.iter" = min(iter),
                "time" = c(end - start)))
}

Benchmark()
