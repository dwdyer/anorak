from datetime import datetime

class Result:
    def __init__(self, date, home_team, home_score, away_team, away_score, home_goals = [], away_goals = []):
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
    def __init__(self, scorer, minute, type = None):
        self.scorer = scorer
        self.minute = minute
        self.type = type

    def __str__(self):
        """Returns the name of the scorer, a space, the minute of the goal, and (optionally) the type ('p' for penalty, 'o' for own goal)."""
        return "".join([self.scorer, str(self.minute), self.type if self.type else ""])

    def __cmp__(self, other):
        return cmp(self.minute, other.minute)


def parse_rlt(rlt_path):
    """Read an RLT file and return a list of results and a list of metadata records."""
    with open(rlt_path) as rlt:
        line = rlt.readline()
        results = []
        metadata = []
        while line:
            if line[0].isdigit():
                fields = line.split("|")
                date = datetime.strptime(fields[0], "%d%m%Y").date()
                results.append(Result(date, fields[1], int(fields[2]), fields[3], int(fields[4])))
            else:
                metadata.append(line)
            line = rlt.readline()
    return (results, metadata)

def write_rlt(rlt_path, results, metadata):
    """Write a new RLT file with the specified file name containing the specified results and metadata."""
    with open(rlt_path, "w") as rlt:
        rlt.writelines(metadata)
        rlt.writelines([str(result) + "\n" for result in results])
