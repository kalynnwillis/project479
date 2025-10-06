# Conference mapping for NCAA Division I teams
# Based on 2018-2024 conference affiliations

get_team_conference <- function(team_name) {
    # Create conference mapping (major conferences)
    # Note: Some teams changed conferences during 2018-2024

    # Power 5 conferences
    acc <- c(
        "Duke", "North Carolina", "Virginia", "Virginia Tech", "Louisville",
        "Syracuse", "Florida St", "Clemson", "NC State", "Miami", "Pittsburgh",
        "Georgia Tech", "Wake Forest", "Boston College", "Notre Dame"
    )

    big_ten <- c(
        "Michigan", "Michigan St", "Ohio State", "Penn State", "Indiana",
        "Purdue", "Illinois", "Wisconsin", "Iowa", "Minnesota", "Northwestern",
        "Maryland", "Rutgers", "Nebraska"
    )

    big_12 <- c(
        "Kansas", "Kansas St", "Baylor", "Texas", "Texas Tech", "Oklahoma",
        "Oklahoma St", "West Virginia", "Iowa St", "TCU"
    )

    sec <- c(
        "Kentucky", "Tennessee", "Arkansas", "Alabama", "Auburn", "Florida",
        "Georgia", "LSU", "Mississippi St", "Missouri", "Ole Miss", "South Carolina",
        "Texas A&M", "Vanderbilt"
    )

    pac_12 <- c(
        "Arizona", "UCLA", "USC", "Oregon", "Washington", "Colorado",
        "Utah", "Stanford", "California", "Arizona St", "Oregon St",
        "Washington St"
    )

    # Major mid-major conferences
    big_east <- c(
        "Villanova", "UConn", "Creighton", "Xavier", "Marquette", "Providence",
        "Seton Hall", "Butler", "Georgetown", "St. John's", "DePaul"
    )

    aac <- c(
        "Houston", "Memphis", "Cincinnati", "SMU", "Temple", "UCF", "Wichita St",
        "Tulsa", "Tulane", "East Carolina", "South Florida"
    )

    a10 <- c(
        "Dayton", "VCU", "Saint Louis", "Rhode Island", "Richmond", "Davidson",
        "St. Bonaventure", "George Mason", "Duquesne", "Saint Joseph's"
    )

    wcc <- c(
        "Gonzaga", "Saint Mary's", "BYU", "San Francisco", "Santa Clara",
        "Loyola Marymount", "Pepperdine", "Pacific", "Portland", "San Diego"
    )

    mvc <- c(
        "Loyola Chicago", "Drake", "Missouri St", "Bradley", "Northern Iowa",
        "Illinois St", "Indiana St", "Southern Illinois", "Valparaiso", "Evansville"
    )

    # Map teams to conferences
    conf_map <- list(
        "ACC" = acc,
        "Big Ten" = big_ten,
        "Big 12" = big_12,
        "SEC" = sec,
        "Pac-12" = pac_12,
        "Big East" = big_east,
        "American" = aac,
        "A-10" = a10,
        "WCC" = wcc,
        "MVC" = mvc
    )

    # Find conference for each team
    sapply(team_name, function(t) {
        for (conf in names(conf_map)) {
            if (t %in% conf_map[[conf]]) {
                return(conf)
            }
        }
        # Default: "Other" for teams not in major conferences
        return("Other")
    })
}
