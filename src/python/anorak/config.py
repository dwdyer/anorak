from os.path import dirname, join
from BeautifulSoup import BeautifulStoneSoup

def get_files_to_update(config_path):
    """Parse the specified config XML and return a dictionary of URLs to scrape and the data files that should be updated."""
    print "Reading configuration file: %s" % config_path
    with open(config_path) as config_file:
        xml = config_file.read()
    tags = BeautifulStoneSoup(xml, selfClosingTags=["season"])
    # Find all seasons that specify a source URL for scraping.
    tags = tags.findAll("season", src=True)
    files = {}
    basedir = dirname(config_path)
    for tag in tags:
        files[tag["src"]] = join(basedir, tag["input"])
    return files


# Maps common team name variants to the canonical form of that team's name.
def load_team_aliases(mappings_path):
    """Load mappings for team names.  Maps abbreviated forms to canonical names."""
    print "Reading team name mapping file: %s" % mappings_path
    with open(mappings_path) as mappings_file:
        mappings = {}
        line = mappings_file.readline().rstrip()
        while line:
            values = line.split("=")
            mappings[values[0]] = values[1]
            line = mappings_file.readline().rstrip()
    return mappings


# Maps short versions (i.e. just surname) and variations of a player's name to the canonical form.
def load_player_aliases(mappings_path):
    """Load mappings for player names.  Maps abbreviated forms to canonical names."""
    print "Reading player name mapping file: %s" % mappings_path
    with open(mappings_path) as mappings_file:
        mappings = {}
        line = mappings_file.readline().rstrip()
        while line:
            item = line.split("=")
            key = item[0].split("|")
            mappings[(key[0], key[1])] = item[1]
            line = mappings_file.readline().rstrip()
    return mappings
