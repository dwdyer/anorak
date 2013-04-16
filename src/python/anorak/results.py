# Data types for results plus functions for reading and writing RLT files.

from datetime import datetime
import re

class Result:
    def __init__(self, date, home_team, home_score, away_team, away_score, home_goals, away_goals):
        self.date = date
        self.home_team = home_team
        self.home_score = home_score
        self.away_team = away_team
        self.away_score = away_score
        self.home_goals = home_goals
        self.away_goals = away_goals

    def __str__(self):
        """Returns the result formatted as a single RLT (pipe-delimited) record."""
        home_scorers = "[" + ",".join(map(str, self.home_goals)) + "]" if len(self.home_goals) > 0 else ""
        away_scorers = "[" + ",".join(map(str, self.away_goals)) + "]" if len(self.away_goals) > 0 else ""
        return "|".join([self.date.strftime("%d%m%Y"),
                         self.home_team,
                         str(self.home_score) + home_scorers,
                         self.away_team,
                         str(self.away_score) + away_scorers])

    def __cmp__(self, other):
        """Results are ordered primarily by date.  Results from the same day are sorted alphabetically by home team."""
        date_cmp = cmp(self.date, other.date)
        return date_cmp if date_cmp != 0 else cmp(self.home_team, other.home_team)

    def __hash__(self):
        return hash(self.date) + hash(self.home_team)


class Goal:
    def __init__(self, scorer, minute, goal_type = None):
        self.scorer = scorer
        self.minute = minute
        self.goal_type = goal_type

    def __str__(self):
        """Returns the name of the scorer, a space, the minute of the goal, and (optionally) the type ('p' for penalty, 'o' for own goal)."""
        return "".join([self.scorer, str(self.minute), self.goal_type if self.goal_type else ""])

    def __cmp__(self, other):
        return cmp(self.minute, other.minute)


def parse_rlt(rlt_path, player_aliases = {}):
    """Read an RLT file and return a list of results and a list of metadata records."""
    with open(rlt_path) as rlt:
        line = rlt.readline()
        results = []
        metadata = []
        while line:
            if line[0].isdigit():
                fields = line.split("|")
                date = datetime.strptime(fields[0], "%d%m%Y").date()
                (home_goals, home_scorers) = parse_score(fields[2], fields[1], fields[3], player_aliases)
                (away_goals, away_scorers) = parse_score(fields[4], fields[3], fields[1], player_aliases)
                results.append(Result(date, fields[1], home_goals, fields[3], away_goals, home_scorers, away_scorers))
            else:
                metadata.append(line)
            line = rlt.readline()
    return (results, metadata)

# Regex to separate score and scorers.
score_regex = re.compile(r"(\d+)(?:\[(.+?)\])?")
# Regex to match an RLT goal scorer entry.
goal_regex = re.compile(r"(\D+)(\d+)([po]?)")

def parse_score(text, team, opposition, player_aliases = {}):
    """Parse the score field of an RLT record and return a tuple containing the scorer and scorers."""
    match = score_regex.match(text)
    score = int(match.group(1))
    scorers = match.group(2)
    goals = []
    if scorers:
        items = scorers.split(",")
        for goal in items:
            match = goal_regex.match(goal)
            goal_type = match.group(3)
            key = (opposition if goal_type == "o" else team, match.group(1))
            scorer = player_aliases.get(key, match.group(1))
            goals.append(Goal(scorer, match.group(2), goal_type))
    return (score, goals)


def write_rlt(rlt_path, results, metadata):
    """Write a new RLT file with the specified file name containing the specified results and metadata."""
    with open(rlt_path, "w") as rlt:
        rlt.writelines(metadata)
        rlt.writelines([str(result) + "\n" for result in results])
