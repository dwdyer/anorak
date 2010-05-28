import sys
from datetime import datetime
import urllib
from BeautifulSoup import BeautifulSoup, SoupStrainer
from config import get_files_to_update
from results import Result, parse_rlt, write_rlt

def load_html(url, strainer=None):
    """Loads the HTML at the specified URL and returns it as a BeautifulSoup object."""
    urlReader = urllib.urlopen(url)
    html = urlReader.read()
    urlReader.close()
    return BeautifulSoup(html, convertEntities=BeautifulSoup.ALL_ENTITIES, parseOnlyThese=strainer)

def scrape_bbc_results(results_page_url):
    """Scrapes the specified BBC web page for football results.  Returns a list of Result objects."""
    # Find all tags that contain dates or results ('mvb' indicates a date, 'competitionResults' indicates a result).
    tags = load_html(results_page_url, SoupStrainer(attrs={"class":["mvb", "competitionResults"]}))
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
    return results

def update_all(data_files):
    for url, file in data_files.items():
        old_results, metadata = parse_rlt(file)
        new_results = scrape_bbc_results(url)
        combined_results = list(set(old_results) | set(new_results))
        if len(combined_results) > len(old_results):
            print "Writing %d new results to %s." % (len(combined_results) - len(old_results), file)
            combined_results.sort()
            write_rlt(file, combined_results, metadata)
        else:
            print "No new results for %s, skipping." % file

print "Reading configuration file: %s" % sys.argv[1]
update_all(get_files_to_update(sys.argv[1]))
