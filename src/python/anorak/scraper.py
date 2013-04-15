# Update RLT files by scraping the latest results from the web.

from datetime import datetime
import re
import sys
import urllib
from bs4 import BeautifulSoup, SoupStrainer
from bs4.element import Tag
import dateutil.parser as dparser
from config import get_files_to_update, load_player_aliases, load_team_aliases
from results import Goal, Result, parse_rlt, write_rlt

def load_html(url, strainer=None):
    """Loads the HTML at the specified URL and returns it as a BeautifulSoup object."""
    urlReader = urllib.urlopen(url)
    html = urlReader.read()
    urlReader.close()
    return BeautifulSoup(html, "lxml", parse_only=strainer)


# Regex to separate a goal scorer's name from the list of minutes in which they scored.
scorer_regex = re.compile(r"(\D+?)\s\(([\w\s,\+]+)\)")

def scrape_sky_results(results_page_url, team_aliases = {}, player_aliases = {}):
    """Scrapes the specified Sky Sports web page for football results.  Returns a list of Result objects."""
    # Find all tags that contain dates or results ('fix-head-t2' indicates a date, 'fix-bar' indicates a result).
    tags = load_html(results_page_url, SoupStrainer(attrs={"class":["fix-head-t2", "fix-bar"]}))
    results = []
    for tag in tags:
        if not isinstance(tag, Tag): # Could be Doctype object
            continue
        if  "fix-head-t2" in tag["class"]:
            date = dparser.parse(tag.string).date()
        else:
            home_team_tag = tag.find("div", class_="score-side score-side1")
            home_team = home_team_tag.find(text=True).strip()
            home_team = team_aliases.get(home_team, home_team)
            away_team_tag = tag.find("div", class_="score-side score-side2")
            away_team = away_team_tag.find(text=True).strip()
            away_team = team_aliases.get(away_team, away_team)

            home_goals = parse_goals(home_team_tag.findAll("li"), home_team, away_team, player_aliases)
            away_goals = parse_goals(away_team_tag.findAll("li"), away_team, home_team, player_aliases)

            score = tag.find("div", attrs={"class":"score-post"}).string.split(" - ")
            home_score = int(score[0])
            away_score = int(score[1])

            if home_score != len(home_goals):
                print "%s - Number of %s goals (%d) does not match home score (%d)." % (date, home_team, len(home_goals), home_score)
            if away_score != len(away_goals):
                print "%s - Number of %s goals (%d) does not match away score (%d)." % (date, away_team, len(away_goals), away_score)
            results.append(Result(date, home_team, home_score, away_team, away_score, home_goals, away_goals))
    return results


def parse_goals(tags, team, opponent, player_aliases = {}):
    """Extracts a list of goal-scorers from an HTML fragment."""
    goals = []
    for record in tags:
        match = scorer_regex.match(record.string)
        if not match is None:
            player = match.group(1).strip()
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
    

def update_all(data_files, team_aliases = {}):
    for url, file, players in data_files:
        old_results, metadata = parse_rlt(file)
        new_results = scrape_sky_results(url, team_aliases, load_player_aliases(players))
        combined_results = list(set(old_results) | set(new_results))
        if len(combined_results) > len(old_results):
            print "Writing %d new results to %s." % (len(combined_results) - len(old_results), file)
            combined_results.sort()
            write_rlt(file, combined_results, metadata)
        else:
            print "No new results for %s, skipping." % file

# Program entry point.  Expects two arguments - the path to the config file and the path to the team mapping file.
update_all(get_files_to_update(sys.argv[1]), load_team_aliases(sys.argv[2]))
