from datetime import datetime
import urllib
from BeautifulSoup import BeautifulSoup, SoupStrainer
from config import aliases, data

class Result:
    def __init__(self, date, home_team, home_score, away_team, away_score):
        """Sets all result fields, mapping team names to their canonical forms."""
        self.date = date
        self.home_team = home_team if not home_team in aliases else aliases[home_team]
        self.home_score = home_score
        self.away_team = away_team if not away_team in aliases else aliases[away_team]
        self.away_score = away_score

    def __str__(self):
        """Returns the result formatted as a single RLT (pipe-delimited) record."""
        return "|".join([self.date.strftime("%d%m%Y"), self.home_team, str(self.home_score), self.away_team, str(self.away_score)])

    def __cmp__(self, other):
        """Results are ordered primarily by date.  Results from the same day are sorted alphabetically by home team."""
        date_cmp = cmp(self.date, other.date)
        return date_cmp if date_cmp != 0 else cmp(self.home_team, other.home_team)


def load_html(url, strainer=None):
    """Loads the HTML at the specified URL and returns it as a BeautifulSoup object."""
    urlReader = urllib.urlopen(url)
    html = urlReader.read()
    urlReader.close()
    return BeautifulSoup(html, convertEntities=BeautifulSoup.ALL_ENTITIES, parseOnlyThese=strainer)

def scrape_bbc_results(results_page_url):
    """Scrapes the specified BBC web page for football results.  Returns a list of Result objects."""
    tags = load_html(results_page_url, SoupStrainer(attrs={"class":["mvb", "competitionResults"]}))
    # Find all tags that contain dates or results ('mvb' indicates a date, 'competitionResults' indicates a result).
    results = []
    for tag in tags:
        if tag["class"] == "mvb":
            date = datetime.strptime(tag.b.string, "%A, %d %B %Y").date()
        else:
            # 'c1' indicates home team, 'c2' is hyphen-separated score, 'c3' is away team.
            home_team_cell = tag.tr.find("td", attrs={"class":"c1"}).b
            home_team = home_team_cell.string.extract() if home_team_cell.a == None else home_team_cell.a.string.extract()
            score = tag.tr.find("td", attrs={"class":"c2"}).b.string.extract().split("-")
            away_team_cell = tag.tr.find("td", attrs={"class":"c3"}).b
            away_team = away_team_cell.string.extract() if away_team_cell.a == None else away_team_cell.a.string.extract()
            results.append(Result(date, home_team, int(score[0]), away_team, int(score[1])))
    results.sort()
    return results

def update_all(data_files):
    for url, file in data_files.items():
        new_results = scrape_bbc_results(url)
        print "\n".join([str(result) for result in new_results])
        print

update_all(data)
