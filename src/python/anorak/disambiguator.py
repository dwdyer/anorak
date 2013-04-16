# Process an RLT file, replacing potentially ambiguous scorer names with their full names
# from the provided mapping file

import sys
from .config import load_player_aliases
from .results import parse_rlt, write_rlt

def show_duplicate_names(results, aliases):
    """Finds duplicate scorer names that aren't disambiguated in the player mapping file."""

    # Determine which names have already been mapped.
    mapped = set([(team, canonical) for (team, name), canonical in aliases.items()])

    # Create a mapping from player name to a set of teams that name is associated with.
    players = {}
    for result in results:
        extract_scorer_teams(players, mapped, result, True)
        extract_scorer_teams(players, mapped, result, False)

    # Filter to retain only previously unmapped names that are associated with more than one team.
    duplicates = [(name, teams) for name, teams in players.items() if len(teams) > 1]
    # Dump the information in a format that we can copy-and-paste into the mapping file.
    for name, teams in duplicates:
        for team in teams:
            print "%s|%s=" % (team, name)


def extract_scorer_teams(players, mapped, result, home):
    for_team = result.home_team if home else result.away_team
    against_team = result.away_team if home else result.home_team
    for goal in (result.home_goals if home else result.away_goals):
        teams = players.get(goal.scorer, set())
        team = against_team if goal.goal_type == "o" else for_team # Own goals are scored by the opposition.
        if (team, goal.scorer) not in mapped:
            teams.add(team)
        players[goal.scorer] = teams


def disambiguate(file_path, player_aliases):
    results, metadata = parse_rlt(file_path, player_aliases)
    show_duplicate_names(results, player_aliases)
    write_rlt(file_path, results, metadata)

# Entry point, expects two arguments, RLT path and mapping path.
disambiguate(sys.argv[1], load_player_aliases(sys.argv[2]))
