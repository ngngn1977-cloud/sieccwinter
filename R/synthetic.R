# Fully synthetic data generator. Nothing here samples, shuffles or perturbs real participant records.
# Every value is drawn from an artificial random-number process; the only real-world knowledge used is the list of variable names,
# types, allowed codes and plausible ranges. The synthetic data contain no true weather effect (outcomes depend only on artificial covariates).

#' Generate the synthetic canonical study CSV as a data frame.
#' @param n Number of synthetic participants (default 60, deliberately different from the study size).
#' @param seed Generator seed (fixed so the shipped CSV is reproducible).
#' @param edge_cases Add missing-value edge cases: one missing PSQI, one missing Outdoor_Mobility, one missing daily temperature value,
#'   and one rainfall cell with a missing total and a rainy-day indicator of 0.
#' @export
siw_make_synthetic <- function(n = 60L, seed = 1234L, edge_cases = TRUE, config = siw_load_config()) {
  siw_with_seed(seed, {
    code <- sprintf("SYN%03d", seq_len(n))
    area <- rep(1:3, length.out = n); area <- area[sample.int(n)]
    jew <- rbinom(n, 1, 0.5); nonj <- which(jew == 0); mus <- integer(n); chr <- integer(n)
    if (length(nonj)) { pick <- rbinom(length(nonj), 1, 0.6); mus[nonj] <- pick; chr[nonj] <- 1L - pick }
    mem <- sample(1:5, n, TRUE, prob = c(0.10, 0.20, 0.35, 0.25, 0.10))
    integ <- sample(1:4, n, TRUE, prob = c(0.10, 0.20, 0.30, 0.40))
    ucla <- pmin(9, pmax(3, 3 + rbinom(n, 6, 0.3)))
    iso <- pmin(20, pmax(5, round(5 + rgamma(n, shape = 4, rate = 0.6))))
    day <- sample(0:78, n, TRUE)                       # artificial day index; no calendar dates are stored
    dat <- data.frame(
      participant_code = code, UCLA_Lonliness_SCORE = ucla, Isolation_cont = iso, integration_score = integ, int_dummy = as.integer(integ == 4),
      Sex = rbinom(n, 1, 0.6), Age = sample(65:88, n, TRUE), assessment_day = day, Area = area, Econ_STS = rbinom(n, 1, 0.5),
      Jewish = jew, Muslim = mus, Christian = chr, PSQI = sample(0:15, n, TRUE), Subjective_Health = sample(0:4, n, TRUE), Memory = mem,
      Memory_Changed = sample(1:3, n, TRUE, prob = c(0.2, 0.6, 0.2)), Subjective_Wellbeing = sample(0:5, n, TRUE), Outdoor_Mobility = sample(0:8, n, TRUE),
      PedestLength_m = round(runif(n, 300, 2800)), Memory_assess = as.integer(mem <= 3), chronic_domain_count = sample(0:6, n, TRUE),
      stringsAsFactors = FALSE)
    # artificial daily weather: a smooth seasonal curve in an artificial calendar plus noise shared by (calendar day, area)
    cal_max <- max(day) + 60L; cal <- 0:cal_max
    base <- list(UTCI = c(11, 3), MRT = c(14, 4), T2M = c(13, 3), RH = c(70, 10), IMS_IDW_TD_MEAN = c(13, 3), IMS_IDW_RH_MEAN = c(68, 10))
    for (pf in names(base)) {
      noise <- matrix(rnorm(length(cal) * 3L, 0, 1), length(cal), 3L)
      seasonal <- base[[pf]][2] * sin(2 * pi * (cal + runif(1, 0, 30)) / 60)
      for (l in 1:59) {
        cd <- day - l + 60L
        v <- base[[pf]][1] + seasonal[cd + 1L] + 0.8 * noise[cbind(cd + 1L, area)] + 0.3 * area
        if (pf %in% c("RH", "IMS_IDW_RH_MEAN")) v <- pmin(98, pmax(25, v))
        dat[[sprintf("%s_L%02d", pf, l)]] <- round(v, 3)
      }
    }
    wet <- matrix(rbinom(length(cal) * 3L, 1, 0.35), length(cal), 3L); amt <- matrix(round(rgamma(length(cal) * 3L, 1.2, 0.35), 2), length(cal), 3L)
    for (l in 1:59) {
      cd <- day - l + 60L; tot <- wet[cbind(cd + 1L, area)] * amt[cbind(cd + 1L, area)]
      tot[tot > 0 & tot < 0.1] <- 0.1
      dat[[sprintf("IMS_IDW_RAIN_TOTAL_L%02d", l)]] <- tot
      dat[[sprintf("IMS_IDW_RAIN_DAYS1_L%02d", l)]] <- as.numeric(tot >= 1)
    }
    if (edge_cases && n >= 10L) {
      dat$PSQI[3L] <- NA; dat$Outdoor_Mobility[7L] <- NA
      dat$T2M_L20[5L] <- NA
      dat$IMS_IDW_RAIN_TOTAL_L35[9L] <- NA; dat$IMS_IDW_RAIN_DAYS1_L35[9L] <- 0
    }
    cum <- siw_reconstruct_cumulative(dat, config)
    cbind(dat, cum)
  })
}
