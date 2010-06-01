# Process an RLT file, replacing potentially ambiguous scorer names with their full names
# from the provided mapping file

import sys
from config import load_player_aliases
from results import parse_rlt, write_rlt

def disambiguate(file_path, player_aliases):
    results, metadata = parse_rlt(file_path, player_aliases)
    write_rlt(file_path, results, metadata)

# Entry point, expects two arguments, RLT path and mapping path.
disambiguate(sys.argv[1], load_player_aliases(sys.argv[2]))
