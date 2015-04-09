# analyzing rollcalls from the New York state legislature

library(lubridate)
library(pscl) # Political Science Computation Library


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

# convert text dates to date objects
bill_votes$date = as.Date(bill_votes$date)

# find all the floor votes in the given chamber in the given year
rollcalls = bill_votes[bill_votes$vote_chamber == chamber &
                         year(bill_votes$date) == year &
                           bill_votes$motion == "Floor Vote", ]

# the relevant votes
rel_votes = leg_votes[leg_votes$vote_id %in% rollcalls$vote_id &
                        leg_votes$leg_id != "", ]

# putting together the rollcall object
leg_ids = unique(rel_votes$leg_id)
vote_matrix = matrix(nrow = length(leg_ids),
    ncol = nrow(rollcalls),
    dimnames = list("Legislator ID" = leg_ids,
        "Vote ID" = rollcalls$vote_id))

for (row in 1:nrow(rel_votes)) {
  vote_matrix[rel_votes[row, "leg_id"], rel_votes[row, "vote_id"]] =
    rel_votes[row, "vote"]
}

# get the names
names = legs[match(leg_ids, legs$leg_id), "full_name"]

# get the parties
parties = legs[match(leg_ids, legs$leg_id), "party"]
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
anom_results = anominate(rc, polarity = 23)

# save the results for later
save(anom_results, file = "anom_results.RData")
# load old results
load("anom_results.RData")

plot(anom_results)

# Are the utilities Gaussian or quadratic?
# 1 = Gaussian, 0 = quadratic
densplot.anominate(anom_results)






# make writing the rollcall objects easier
create_rc = function(legs, bill_votes, rel_votes) {

  # putting together the vote matrix, to use for the rollcall object
  leg_ids = unique(rel_votes[, "leg_id"])
  vote_matrix = matrix(nrow = length(leg_ids),
      ncol = length(rollcalls[, 1]),
      dimnames = list("Legislator ID" = leg_ids,
          "Vote ID" = rollcalls[, "vote_id"]))

  # add votes to the vote matrix
  for (row in 1:nrow(rel_votes)) {
    vote_matrix[rel_votes[row, "leg_id"],
                rel_votes[row, "vote_id"]] = rel_votes[row, "vote"]
  }

  # get the names
  names = legs[match(leg_ids, legs$leg_id), "full_name"]

  # get the parties
  parties = legs[match(leg_ids, legs$leg_id), "party"]
  parties = matrix(parties, length(parties), 1)
  colnames(parties) = "party"
  parties = as.data.frame(parties, stringsAsFactors = F)

  # get the bill names
  bill_names = rollcalls[, "bill_id"]
  bill_names = matrix(rollcalls[, "bill_id"], length(rollcalls[, 1]), 1)
  colnames(bill_names) = "Bill ID"
  bill_names = as.data.frame(bill_names, stringsAsFactors = F)

  # return the rollcall object
  rollcall(vote_matrix, yea = "yes", nay = "no",
           missing = c("other", NA),
           legis.names = names, #vote.names = bill_names,
           legis.data = parties, vote.data = bill_names,
           source = "Sunlight Foundation")  
}







# Optimal Classification
library(oc)


# all the floor votes in both chambers, in all years
rollcalls = bill_votes[bill_votes[, "motion"] == "Floor Vote", ]

# the relevant votes
rel_votes = leg_votes[leg_votes$vote_id %in% rollcalls$vote_id &
                      leg_votes$leg_id != "", ]

# make the rollcall object
oc_rc = create_rc(legs, bill_votes, rel_votes)

# save that data!
save(oc_rc, file = "oc_rc.RData")
# load the data
load("oc_rc.RData")


# Optimal Classification uses "nonmetric unfolding", meaning no
# specified utility curve, though we assume it is symmetric and
# single-peaked. Uses another linear algebra algorithm. Works well
# with missing data, i.e., when comparing multiple chambers.
oc_results = oc(oc_rc, polarity = c("Brian M Kolb", "Brian M Kolb"))
plot(oc_results)
plot.OCcoords(oc_results)






## assemblymen = legs[legs$chamber == "lower", "full_name"]
## senators = legs[legs$chamber == "upper", "full_name"]

## plot(oc_results$legislators[assemblymen, "coord1D"],
##      oc_results$legislators[assemblymen, "coord2D"])

## plot(oc_results$legislators[senators, "coord1D"],
##      oc_results$legislators[senators, "coord2D"])
