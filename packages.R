# install the relevant packages

rollcall_packages = c("lubridate", # date utilities
    "pscl", # Political Science Computation Laboratory
    "wnominate", # Weighted-NOMINATE
    "anominate", # alpha-NOMINATE
    "oc", # Optimal Classification
    "devtools" # to install a package from Github
                      )

install.packages(rollcall_packages,
                 repos = "http://cran.us.r-project.org")
