sites <- unique(master_card_clean$card)
species <- unique(master_nzffd_final$species_code)
model_master <- data.frame(matrix(nrow = length(sites)))
model_master$matrix.nrow...length.sites.. <- sites
names(model_master) <- "card"
model_master$present <- NA
model_master$absent <-  NA


