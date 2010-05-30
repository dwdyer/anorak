from datetime import datetime
import re
import sys
import urllib
from BeautifulSoup import BeautifulSoup, SoupStrainer
from config import get_files_to_update, load_player_aliases, load_team_aliases
from results import Goal, Result, parse_rlt, write_rlt

def load_html(url, strainer=None):
    """Loads the HTML at the specified URL and returns it as a BeautifulSoup object."""
    urlReader = urllib.urlopen(url)
    html = urlReader.read()
    urlReader.close()
    return BeautifulSoup(html, convertEntities=BeautifulSoup.ALL_ENTITIES, parseOnlyThese=strainer)


# Regex to separate a goal scorer's name from the list of minutes in which they scored.
scorer_regex = re.compile(r"(\D+?)\s\(([\w\s,\+]+)\)")

def scrape_bbc_results(results_page_url, team_aliases = {}, player_aliases = {}):
    """Scrapes the specified BBC web page for football results.  Returns a list of Result objects."""
    # Find all tags that contain dates or results ('mvb' indicates a date, 'competitionResults' indicates a result).
    tags = load_html(results_page_url, SoupStrainer(attrs={"class":["mvb", "competitionResults"]}))
    results = []
    for tag in tags:
        if tag["class"] == "mvb":
            date = datetime.strptime(tag.b.string, "%A, %d %B %Y").date()
        else:
            # 'c1' indicates home team, 'c2' is hyphen-separated score, 'c3' is away team.
            (home_team, home_goals) = parse_team(tag.tr.find("td", attrs={"class":"c1"}), team_aliases, player_aliases)
            score = tag.tr.find("td", attrs={"class":"c2"}).b.string.extract().split("-")
            (away_team, away_goals) = parse_team(tag.tr.find("td", attrs={"class":"c3"}), team_aliases, player_aliases)
            home_score = int(score[0])
            away_score = int(score[1])
            assert home_score == len(home_goals), "Number of %s goals (%d) does not match home score (%d)." % (home_team, len(home_goals), home_score)
            assert away_score == len(away_goals), "Number of %s goals (%d) does not match away score (%d)." % (away_team, len(away_goals), away_score)
            results.append(Result(date, home_team, home_score, away_team, away_score, home_goals, away_goals))
    return results


def parse_team(tag, team_aliases = {}, player_aliases = {}):
    """Extracts a team name and list of goal-scorers from an HTML fragment. Returns a 2-item tuple."""
    team = tag.b.string.extract() if tag.b.a == None else tag.b.a.string.extract()
    if team in team_aliases:
        team = team_aliases[team]
    scorer_list = tag.find("p", attrs={"class":"scorer"}).findAll(text=True)
    goals = []
    for record in scorer_list:
        match = scorer_regex.match(record)
        player = match.group(1)
        if (team, player) in player_aliases:
            player = player_aliases[(team, player)]
        else:
            print "Unknown %s player %s" % (team, player)
        goals.extend(parse_goals(player, match.group(2)))
    goals.sort()
    return (team, goals)


# Regex for parsing a single goal time and any "pen"/"og" modifiers.
goal_regex = re.compile(r"([a-z]*)\s?(\d+)(?:\+(\d+))?")

def parse_goals(scorer, text):
    """Parse the goal (or goals) scored by a goal scorer in a particular game.  Returns a list of that player's goals in the game."""
    goals = []
    records = text.split(", ")
    for record in records:
        match = goal_regex.match(record)
        goals.append(Goal(scorer, int(match.group(2)), match.group(1)[0] if match.group(1) else None))
    return goals
    

def update_all(data_files, team_aliases = {}, player_aliases = {}):
    for url, file in data_files.items():
        old_results, metadata = parse_rlt(file)
        new_results = scrape_bbc_results(url, team_aliases, player_aliases)
        combined_results = list(set(old_results) | set(new_results))
        if len(combined_results) > len(old_results):
            print "Writing %d new results to %s." % (len(combined_results) - len(old_results), file)
            combined_results.sort()
            write_rlt(file, combined_results, metadata)
        else:
            print "No new results for %s, skipping." % file

# Program entry point.  Expects two arguments, path to config file and path to mapping file.
update_all(get_files_to_update(sys.argv[1]), load_team_aliases(sys.argv[2]), load_player_aliases(sys.argv[3]))
