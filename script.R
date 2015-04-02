# analyze that voting data
library(lubridate)
library(pscl)


chamber = "lower"
year = 2014

legs = read.csv(paste0("ny_legislators.csv"),
    stringsAsFactors = F)
bill_votes = read.csv(paste0("ny_bill_votes.csv"),
    stringsAsFactors = F)
leg_votes = read.csv(paste0("ny_bill_legislator_votes.csv"),
    stringsAsFactors = F)

# clarify party status
legs$party[legs$party == ""] = "Unknown"

# I want dates!
bill_votes$date = as.Date(bill_votes$date)

rollcalls = bill_votes[bill_votes[, "vote_chamber"] == chamber &
                         year(bill_votes[, "date"]) == year &
                           bill_votes[, "motion"] == "Floor Vote", ]

# the relevant votes
rel_votes = leg_votes[leg_votes[, "vote_id"] %in%
    rollcalls[, "vote_id"], ]


# fixing the rel_votes data:
# "Lopez" is Peter Lopez
rel_votes[rel_votes[, "name"] == "Lopez", "leg_id"] =
  legs[legs[, "full_name"] == "Peter Lopez", "leg_id"]

# "Mr Spkr" is Sheldon Silver
rel_votes[rel_votes[, "name"] == "Mr Spkr", "leg_id"] =
  legs[legs[, "full_name"] == "Sheldon Silver", "leg_id"]


# putting together the rollcall object
leg_ids = unique(rel_votes[, "leg_id"])
vote_matrix = matrix(nrow = length(leg_ids),
    ncol = length(rollcalls[, 1]),
    dimnames = list("Legislator ID" = leg_ids,
        "Vote ID" = rollcalls[, "vote_id"]))

for (row in 1:nrow(rel_votes)) {
  vote_matrix[rel_votes[row, "leg_id"],
              rel_votes[row, "vote_id"]] = rel_votes[row, "vote"]
}

# get the names
names = vector(length = length(leg_ids))
for (leg in 1:length(leg_ids)) {
  names[leg] = legs[legs[, "leg_id"] == leg_ids[leg], "full_name"]
}

# get the parties
parties = vector(length = length(leg_ids))
for (leg in 1:length(leg_ids)) {
  parties[leg] = legs[legs[, "leg_id"] == leg_ids[leg], "party"]
}
parties = matrix(parties, length(parties), 1)
colnames(parties) = "party"
parties = as.data.frame(parties, stringsAsFactors = F)

# get the bill names
bill_names = rollcalls[, "bill_id"]
bill_names = matrix(rollcalls[, "bill_id"], length(rollcalls[, 1]), 1)
colnames(bill_names) = "Bill ID"
bill_names = as.data.frame(bill_names, stringsAsFactors = F)

# make that rollcall object
rc = rollcall(vote_matrix, yea = "yes", nay = "no",
    missing = c("other", NA),
    legis.names = names, #vote.names = bill_names,
    legis.data = parties, vote.data = bill_names,
    source = "Sunlight Foundation")


# save that data!
save(rc, file = paste0(state, "_", year, ".RData"))

# load that data!
load(paste0(state, "_", year, ".RData"))



# IDEAL

# "ideal" function from "pscl" library

ideal_results = ideal(rc)
plot(ideal_results)



# W-NOMINATE
library(wnominate)

wnom_results = wnominate(rc, polarity = c("Peter Lopez", "Peter Lopez"))
plot(wnom_results)



# alpha-NOMINATE
library(anominate)

# runs wnominate first, then uses MCMC
anom_results = anominate(rc, polarity = 23)
plot(anom_results)

# some data from the MCMC analysis
densplot.anominate(anom_results)
traceplot.anominate(anom_results)


save(ideal_results, wnom_results, anom_results,
     file = paste0(state, "_", year, "_results.RData"))



# Optimal Classification
library(oc)

oc_results = oc(rc, polarity = c("Peter Lopez"),
    dims = 1)
plot(oc_results)
