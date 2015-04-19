# analyzing rollcalls from the New York state legislature

library(lubridate)
library(reshape2)
library(pscl) # Political Science Computation Library


chamber = "lower"
year = 2014

legs = read.csv("ny_legislators.csv", stringsAsFactors = F)
bill_votes = read.csv("ny_bill_votes.csv", stringsAsFactors = F)
leg_votes = read.csv("ny_bill_legislator_votes.csv",
    stringsAsFactors = F)

# clarify party status
legs$party[legs$party == ""] = "Unknown"

# convert text dates to date objects
bill_votes$date = as.Date(bill_votes$date)

# find all the floor votes in the given chamber in the given year
rollcalls = bill_votes[bill_votes$vote_chamber == chamber &
                         year(bill_votes$date) == year &
                           bill_votes$motion == "Floor Vote", ]

# the relevant votes
rel_votes = leg_votes[leg_votes$vote_id %in% rollcalls$vote_id &
                        leg_votes$leg_id != "", ]

# transform rel_votes from long to wide format
vote_matrix = dcast(rel_votes, leg_id ~ vote_id, value.var = "vote")

# get the names
leg_ids = vote_matrix[, 1]
names = legs[match(leg_ids, legs$leg_id), "full_name"]

# get the parties
parties = legs[match(leg_ids, legs$leg_id), "party"]
parties = data.frame("party" = parties, stringsAsFactors = F)

# get the bill names
vote_ids = data.frame("Vote ID" = colnames(vote_matrix[, -1]),
    stringsAsFactors = F)

# create the rollcall object
rc = rollcall(vote_matrix[, -1], yea = "yes", nay = "no",
    missing = "other", notInLegis = NA, legis.names = names,
    legis.data = parties, vote.data = vote_ids,
    source = "Sunlight Foundation")




# IDEAL

# "ideal" function from "pscl" library

# Assumes quadratic utility, uses MCMC (a Bayesian algorithm) to sort
# legislators
ideal_results = ideal(rc)
plot(ideal_results)




# W-NOMINATE
library(wnominate)

# Assumes Gaussian utility, uses a linear algebra algorithm to sort
# legislators
wnom_results = wnominate(rc, polarity = c("Brian M Kolb", "Brian M Kolb"))
plot(wnom_results)
plot.coords(wnom_results) # just the coordinates




# alpha-NOMINATE
library(anominate)

# Gaussian utility, runs wnominate first, then uses MCMC
anom_results = anominate(rc, polarity = 53)

# save the results for later
save(anom_results, file = "anom_results.RData")

# load old results
load("anom_results.RData")

plot(anom_results)

# Are the utilities Gaussian or quadratic?
# 1 = Gaussian, 0 = quadratic
densplot.anominate(anom_results)






# make writing the rollcall objects easier
create_rc = function(legs, rollcalls, leg_votes) {

  # the relevant votes
  rel_votes = leg_votes[leg_votes$vote_id %in% rollcalls$vote_id &
                          leg_votes$leg_id != "", ]

  # transform rel_votes from long to wide format
  vote_matrix = dcast(rel_votes, leg_id ~ vote_id, value.var = "vote")

  # get the names
  leg_ids = vote_matrix[, 1]
  names = legs[match(leg_ids, legs$leg_id), "full_name"]

  # get the parties
  parties = legs[match(leg_ids, legs$leg_id), "party"]
  parties = data.frame("party" = parties, stringsAsFactors = F)

  # get the bill names
  vote_ids = data.frame("Vote ID" = colnames(vote_matrix[, -1]),
      stringsAsFactors = F)

  # create the rollcall object
  rc = rollcall(vote_matrix[, -1], yea = "yes", nay = "no",
      missing = "other", notInLegis = NA, legis.names = names,
      legis.data = parties, vote.data = vote_ids,
      source = "Sunlight Foundation")
}







# Optimal Classification
library(oc)


# all the floor votes in both chambers, in all years
rollcalls = bill_votes[bill_votes[, "motion"] == "Floor Vote", ]

# make the rollcall object
oc_rc = create_rc(legs, rollcalls, leg_votes)


# Optimal Classification uses "nonmetric unfolding", meaning no
# specified utility curve, though we assume it is symmetric and
# single-peaked. Uses another linear algebra algorithm. Works well
# with missing data, i.e., when comparing multiple chambers.
oc_results = oc(oc_rc, polarity = c("Brian M Kolb", "Brian M Kolb"))
plot(oc_results)
plot.OCcoords(oc_results)


# try plotting the senate and assembly separately
assembly_rc_ids = unique(
    bill_votes[bill_votes$vote_chamber == "lower", "vote_id"])
assembly_ids = unique(
    leg_votes[leg_votes$vote_id %in% assembly_rc_ids, "leg_id"])
assemblymen = legs[legs$leg_id %in% assembly_ids, "full_name"]

senate_rc_ids = unique(
    bill_votes[bill_votes$vote_chamber == "upper", "vote_id"])
senate_ids = unique(
    leg_votes[leg_votes$vote_id %in% senate_rc_ids, "leg_id"])
senators = legs[legs$leg_id %in% senate_ids, "full_name"]

plot(oc_results$legislators[assemblymen, "coord1D"],
     oc_results$legislators[assemblymen, "coord2D"])

plot(oc_results$legislators[senators, "coord1D"],
     oc_results$legislators[senators, "coord2D"])



# how many legislators served in both chambers?
sum(senators %in% assemblymen)
senators[senators %in% assemblymen]
