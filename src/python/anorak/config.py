from os.path import dirname, join
from bs4 import BeautifulSoup

def get_files_to_update(config_path):
    """Parse the specified config XML and return a list of 3-tuples (URL to scrape, data file to update, player mapping file)."""
    print "Reading configuration file: %s" % config_path
    with open(config_path) as config_file:
        xml = config_file.read()
    tags = BeautifulSoup(xml, features=["xml"])
    # Find all seasons that specify a source URL for scraping.
    tags = tags.findAll("season", src=True)
    files = []
    basedir = dirname(config_path)
    for tag in tags:
        files.append((tag["src"], join(basedir, tag["input"]), join(basedir, tag.parent["players"])))
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
