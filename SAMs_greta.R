# Very draft code using greta to find archetypes


library(greta)

# data
y <- with(test_df_log, tapply(present, list(card, species_code), sum))
y[y>1] <- 1
y <- y[rowSums(y) > 0, ]

env <- 
  test_df_log %>% 
  filter(card %in% rownames(y)) %>% 
  group_by(card) %>% 
  summarise_at(vars(FRE3, C_constancy, C_contingency), mean)
x <- model.matrix(~ 0 + FRE3 + C_constancy,
                  data = env)
x <- scale(x)

# indices
n <- nrow(y)  # number of sites
p <- ncol(y)  # number of species
m <- ncol(x)  # number of covariates
k <- 3        # number of archetypes

# slopes and intercepts
beta  <- normal(0, 1, dim = c(k, m))

sd_beta0 <- exponential(1)
z_beta0  <- normal(0, 1, dim = p) 
beta0    <- z_beta0 * sd_beta0

sd_alpha <- exponential(1)
z_alpha  <- normal(0, 1, dim = n) 
alpha    <- z_alpha * sd_alpha

# probability of archetype assignment
# not sure about the prior, using an uninformative prior like the ones in 
# stable isotope models
z <- dirichlet(t(rep(1/k, k)), n_realisations = p)

# linear predictor
eta <- x %*% t(beta) %*% t(z)
eta <- sweep(eta, 2, beta0, "+")
eta <- sweep(eta, 1, alpha, "+")
prob <- ilogit(eta)

# likelihood and model
distribution(y) <- bernoulli(prob)

mod <- model(beta, beta0, z, sd_beta0, sd_alpha)

# optimisation / draws
# o <- opt(mod, max_iterations = 1e3)   # try rerunning this a few times to see the "misalignment" shown below
draws <- mcmc(mod, 
              warmup = 3000,
              sampler = hmc(25, 30),
              # initial_values = initials(beta = beta_true),
              chain = 1)



library(bayesplot)
mcmc_trace(draws, regex_pars = "beta")
mcmc_trace(draws, regex_pars = "sd_")
mcmc_trace(draws, regex_pars = "z")
mcmc_trace(draws, regex_pars = "z\\[1,")

archetype <- categorical(z)
archetype_sim <- calculate(archetype, z, values = draws, nsim = 1000)
archetype_mode <- apply(archetype_sim$archetype, 2, mode)
z_mean <- apply(archetype_sim$z, 2:3, mean)
