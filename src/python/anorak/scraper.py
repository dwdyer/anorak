from datetime import datetime
import urllib
from BeautifulSoup import BeautifulSoup
from aliases import aliases

class Result:
    def __init__(self, date, home_team, home_score, away_team, away_score):
        self.date = date
        self.home_team = home_team if not home_team in aliases else aliases[home_team]
        self.home_score = home_score
        self.away_team = away_team if not away_team in aliases else aliases[away_team]
        self.away_score = away_score

    def __str__(self):
        return "|".join([self.date.strftime("%d%m%Y"), self.home_team, str(self.home_score), self.away_team, str(self.away_score)])

    def __cmp__(self, other):
        date_cmp = cmp(self.date, other.date)
        return date_cmp if date_cmp != 0 else cmp(self.home_team, other.home_team)


def load_html(url):
    """Loads the HTML at the specified URL and returns it as a BeautifulSoup object."""
    urlReader = urllib.urlopen(url)
    html = urlReader.read()
    urlReader.close()
    return BeautifulSoup(html)

def scrape_bbc_results(results_page_url):
    """Scrapes the specified BBC web page for football results.  Returns a list of Result objects."""
    soup = load_html(results_page_url)
    # Find all tags that contain dates or results ('mvb' indicates a date, 'competitionResults' indicates a result).
    tags = soup.findAll(attrs={"class":["mvb", "competitionResults"]})
    results = []
    for tag in tags:
        if tag["class"] == "mvb":
            date = datetime.strptime(tag.b.string, "%A, %d %B %Y").date()
        else:
            # 'c1' indicates home team, 'c2' is hyphen-separated score, 'c3' is away team.
            home_team = tag.tr.find("td", attrs={"class":"c1"}).a.string.extract()
            score = tag.tr.find("td", attrs={"class":"c2"}).b.string.extract().split("-")
            away_team = tag.tr.find("td", attrs={"class":"c3"}).a.string.extract()
            results.append(Result(date, home_team, int(score[0]), away_team, int(score[1])))
    results.sort()
    return results


print "\n".join([str(result) for result in scrape_bbc_results("http://news.bbc.co.uk/sport1/hi/football/eng_prem/results/default.stm")])
