# Update RLT files by scraping the latest results from the web.

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
            home_team_tag = tag.tr.find("td", attrs={"class":"c1"})
            home_team = home_team_tag.b.string.extract() if home_team_tag.b.a == None else home_team_tag.b.a.string.extract()
            home_team = team_aliases.get(home_team, home_team)
            away_team_tag = tag.tr.find("td", attrs={"class":"c3"})
            away_team = away_team_tag.b.string.extract() if away_team_tag.b.a == None else away_team_tag.b.a.string.extract()
            away_team = team_aliases.get(away_team, away_team)

            home_goals = parse_goals(home_team_tag.find("p", attrs={"class":"scorer"}).findAll(text=True), home_team, away_team, player_aliases)
            away_goals = parse_goals(away_team_tag.find("p", attrs={"class":"scorer"}).findAll(text=True), away_team, home_team, player_aliases)

            score = tag.tr.find("td", attrs={"class":"c2"}).b.string.extract().split("-")
            home_score = int(score[0])
            away_score = int(score[1])

            if home_score != len(home_goals):
                print "%s - Number of %s goals (%d) does not match home score (%d)." % (date, home_team, len(home_goals), home_score)
            if away_score != len(away_goals):
                print "%s - Number of %s goals (%d) does not match away score (%d)." % (date, away_team, len(away_goals), away_score)
            results.append(Result(date, home_team, home_score, away_team, away_score, home_goals, away_goals))
    return results


def parse_goals(tag, team, opponent, player_aliases = {}):
    """Extracts a list of goal-scorers from an HTML fragment."""
    goals = []
    for record in tag:
        match = scorer_regex.match(record)
        player = match.group(1)
        goals.extend(parse_scorer(player, team, opponent, match.group(2), player_aliases))
    goals.sort()
    return goals


# Regex for parsing a single goal time and any "pen"/"og" modifiers.
goal_regex = re.compile(r"([a-z]*)\s?(\d+)(?:\+(\d+))?")

def parse_scorer(player, player_team, opponent, text, player_aliases = {}):
    """Parse the goals scored by a player in a particular game."""
    goals = []
    records = text.split(", ")
    for record in records:
        match = goal_regex.match(record)
        type = match.group(1)[0] if match.group(1) else None
        team = opponent if type == "o" else player_team # Own-goals are scored by players on the other team.
        if not (team, player) in player_aliases:
            print "Unknown %s player %s" % (team, player)
        goals.append(Goal(player_aliases.get((team, player), player), int(match.group(2)), type))
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
