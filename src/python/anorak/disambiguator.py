"""Tool for processing an RLT file and replacing potentially ambiguous scorer
names with the canonical name from the provided mapping file."""

import sys
from .config import load_player_aliases
from .results import parse_rlt, write_rlt

def show_duplicate_names(results, aliases):
    """Finds duplicate scorer names that aren't already disambiguated in the
    player mapping file."""

    # Determine which names have already been mapped.
    mapped = set([(team, canonical) for (team, name), canonical in aliases.items()])

    # Create a mapping from a player name to a set of teams associated with it.
    players = {}
    for result in results:
        extract_scorer_teams(players, mapped, result, True)
        extract_scorer_teams(players, mapped, result, False)

    # Filter to retain only names that were previously unmapped that occur for
    # more than one team.
    duplicates = [(name, teams) for name, teams in players.items() if len(teams) > 1]
    # Dump the information in a format that we can copy-and-paste into the
    # mapping file.
    for name, teams in duplicates:
        for team in teams:
            print "%s|%s=" % (team, name)


def extract_scorer_teams(players, mapped, result, home):
    """Extract unmapped scorer names from the result and add to the 'players'
    dictionary provided."""
    for_team = result.home_team if home else result.away_team
    against_team = result.away_team if home else result.home_team
    for goal in (result.home_goals if home else result.away_goals):
        teams = players.get(goal.scorer, set())
        # Own goals are scored by the opposition.
        team = against_team if goal.goal_type == "o" else for_team
        if (team, goal.scorer) not in mapped:
            teams.add(team)
        players[goal.scorer] = teams


def disambiguate(file_path, player_aliases):
    """Update an RLT file by replacing scorer names with canonical versions."""
    results, metadata = parse_rlt(file_path, player_aliases)
    show_duplicate_names(results, player_aliases)
    write_rlt(file_path, results, metadata)


# Entry point, expects two arguments, RLT path and mapping path.
disambiguate(sys.argv[1], load_player_aliases(sys.argv[2]))
