-- | Functions for generating league tables (including form tables and mini-leagues).
module Anorak.Tables (buildRecord,
                      formTable,
                      goalDiff,
                      LeagueRecord(..),
                      leaguePositions,
                      leagueTable,
                      miniLeagueTable,
                      played,
                      points,
                      pointsPerGame) where

import Anorak.Results
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS(unpack)
import Data.List(foldl', sort)
import Data.Map(Map, (!))
import qualified Data.Map as Map(adjust, adjustWithKey, elems, empty, filterWithKey, findMax, findWithDefault, fromList, keysSet, map, mapAccum, mapWithKey, partitionWithKey, toAscList)
import Data.Maybe(fromMaybe)
import Data.Ord(comparing)
import Data.Set(Set)
import qualified Data.Set as Set(member, toAscList, union)
import Data.Time.Calendar(Day)
import Util.List(keep)

-- | A LeagueRecord contains data about the league performance of a single team.  It
--   includes total number of wins, draws, defeats, goals scored and goals conceded.
data LeagueRecord = LeagueRecord {team :: !Team,     -- ^ The team that this record relates to.
                                  won :: !Int,       -- ^ The number of matches won by this team.
                                  drawn :: !Int,     -- ^ The number of matches drawn by this team.
                                  lost :: !Int,      -- ^ The number of matches lost by this team.
                                  for :: !Int,       -- ^ The total number of goals scored by this team.
                                  against :: !Int,   -- ^ The total number of goals conceded by this team.
                                  adjustment :: !Int -- ^ A points adjustment (can be positive or negative but is usually zero) to be applied to this team's total.
                                 }
-- A LeagueRecord can be rendered as a String containing both member fields and
-- derived fields.
instance Show LeagueRecord where
    show record = BS.unpack (team record) ++
                  " P" ++ show (played record) ++
                  " W" ++ show (won record) ++
                  " D" ++ show (drawn record) ++
                  " L" ++ show (lost record) ++
                  " F" ++ show (for record) ++
                  " A" ++ show (against record) ++
                  " GD" ++ show (goalDiff record) ++
                  " Pts" ++ show (points record)
-- A LeagueRecord can be compared with other records to provide an ordering for a
-- list of records.
instance Eq LeagueRecord where
    (==) record1 record2 = points record1 == points record2
                           && goalDiff record1 == goalDiff record2
                           && for record1 == for record2
                           && won record1 == won record2
instance Ord LeagueRecord where
    compare record1 record2
        | points record1 /= points record2     = comparing points record2 record1
        | goalDiff record1 /= goalDiff record2 = comparing goalDiff record2 record1
        | for record1 /= for record2           = comparing for record2 record1
        | won record1 /= won record2           = comparing won record2 record1
        | otherwise                            = EQ

-- | Calculates the total number of matches played (the sum or wins, draws and defeats).
played :: LeagueRecord -> Int
played record = won record + drawn record + lost record

-- | Calculates the total number of points (3 points for each win, 1 for each draw, +/- any adjustment).
points :: LeagueRecord -> Int
points record = won record * 3 + drawn record + adjustment record

-- | Calculates goal difference (total scored minus total conceded)
goalDiff :: LeagueRecord -> Int
goalDiff record = for record - against record

-- | Calculates average number of league points earned per game.
pointsPerGame :: LeagueRecord -> Double
pointsPerGame record = fromIntegral (points record) / fromIntegral (played record)

-- | Builds a LeagueRecord for the specified team, including all of the results (from those provided) in which that
--   team was involved.
buildRecord :: Team -> [Result] -> LeagueRecord
buildRecord t = foldl' (addResultToRecord t) (LeagueRecord t 0 0 0 0 0 0)

-- | Adds a single match result to a particular team's league record.  If the specified team was not involved in that
--   match, the match is ignored.
addResultToRecord :: Team -> LeagueRecord -> Result -> LeagueRecord
addResultToRecord t record result 
    | t == homeTeam result = addScoreToRecord record (homeScore result) (awayScore result)
    | t == awayTeam result = addScoreToRecord record (awayScore result) (homeScore result)
    | otherwise            = record

addScoreToRecord :: LeagueRecord -> Int -> Int -> LeagueRecord
addScoreToRecord (LeagueRecord t w d l f a adj) goalsScored goalsConceded
    | goalsScored > goalsConceded  = LeagueRecord t (w + 1) d l (f + goalsScored) (a + goalsConceded) adj
    | goalsScored == goalsConceded = LeagueRecord t w (d + 1) l (f + goalsScored) (a + goalsConceded) adj
    | otherwise                    = LeagueRecord t w d (l + 1) (f + goalsScored) (a + goalsConceded) adj

-- | Produces a standard league table with teams ordered in descending order of points.  Takes a map of teams to
--   results and a map of points adjustments and returns a sorted list of league records.
leagueTable :: Map Team [Result] -> Map Team Int -> Int -> [LeagueRecord]
leagueTable teamResults adjustments 0     = sort $ map (adjust adjustments) table
                                            where table = Map.elems $ Map.mapWithKey buildRecord teamResults
leagueTable teamResults adjustments split = updateSection top remainingFixtures ++ updateSection bottom remainingFixtures
                                            where -- Create initial table based on early fixtures.
                                                  before = leagueTable (Map.map (take split) teamResults) adjustments 0
                                                  -- Split league in half.
                                                  (top, bottom) = splitAt (length before `div` 2) before
                                                  -- Keep remaining fixtures to be applied to both halves before recombining.
                                                  remainingFixtures = Map.map (drop split) teamResults

-- | Update the specified table by applying remaining results for member teams.
updateSection :: [LeagueRecord] -> Map Team [Result] -> [LeagueRecord]
updateSection table results = sort $ map (updateRecord results) table

-- | Update the specified league record by applying all of the remaining results for that team.
updateRecord :: Map Team [Result] -> LeagueRecord -> LeagueRecord
updateRecord results record = foldl' (addResultToRecord t) record (results ! t)
                              where t = team record

-- | Produces a form table with teams ordered in descending order of points.
formTable :: Map Team [Result] -> Int -> [(LeagueRecord, [TeamResult])]
formTable teamResults n = map (attachForm formResults) $ leagueTable formResults Map.empty 0
                          where formResults = Map.map (keep n) teamResults

attachForm :: Map Team [Result] -> LeagueRecord -> (LeagueRecord, [TeamResult])
attachForm results record = (record, map (convertResult (team record)) formResults)
                            where formResults = Map.findWithDefault [] (team record) results

-- | Looks up the points adjustment for a team (if any) and applies it to their league record.
adjust :: Map Team Int -> LeagueRecord -> LeagueRecord
adjust adjustments (LeagueRecord t w d l f a adj) = LeagueRecord t w d l f a (adj + Map.findWithDefault 0 t adjustments)

-- | Generate a league table that includes only results between the specified teams.
miniLeagueTable :: Set Team -> Map Team [Result] -> Map ByteString Team -> [LeagueRecord]
miniLeagueTable teams results aliases = leagueTable filteredResults Map.empty 0
                                        where teamNames = Set.union teams $ Map.keysSet aliases
                                              filteredResults = Map.map (filter $ bothTeamsInSet teamNames) $ Map.filterWithKey (\k _ -> Set.member k teamNames) results

bothTeamsInSet :: Set Team -> Result -> Bool
bothTeamsInSet teams result = Set.member (homeTeam result) teams && Set.member (awayTeam result) teams

-- | Calculate the league position of every team at the end of every match day.
leaguePositions :: Set Team -> Map Day [Result] -> Map Team Int -> Int -> Map Team [(Day, Int)]
leaguePositions teams results adj sp = foldr addPosition emptyPosMap positions
                                       where teamList = Set.toAscList teams
                                             halfTeamCount = length teamList `div` 2
                                             numMatchesBeforeSplit = sp * halfTeamCount
                                             (resultsBeforeSplit, resultsAfterSplit) = splitResults numMatchesBeforeSplit results
                                             -- Calculate the league table (as a sorted list of LeagueRecords) for each date that games were played.
                                             tablesByDay = tablesByDate [LeagueRecord t 0 0 0 0 0 (Map.findWithDefault 0 t adj) | t <- teamList] resultsBeforeSplit
                                             -- Split the table into top and bottom half (for leagues that do this, such as the SPL)
                                             (topHalf, bottomHalf) = splitAt halfTeamCount . snd $ Map.findMax tablesByDay
                                             -- Convert the various tables to a list of triples (team, date, position).
                                             beforeSplitPositions = tablesToPositions 1 tablesByDay
                                             topPositions = tablesToPositions 1 $ tablesByDate topHalf resultsAfterSplit
                                             bottomPositions = tablesToPositions (halfTeamCount + 1) $ tablesByDate bottomHalf resultsAfterSplit
                                             positions = beforeSplitPositions ++ topPositions ++ bottomPositions
                                             -- An initial map, with one entry for each team, into which the above triples are folded.
                                             emptyPosMap = Map.fromList [(t, []) | t <- teamList]

-- | Calculate the league table (as a sorted list of LeagueRecords) for each date that games were played.
--   The first argument is the initial state of the league (usually blank records, but for post-split tables it
--   will be the table at the split date).
tablesByDate :: [LeagueRecord] -> Map Day [Result] -> Map Day [LeagueRecord]
tablesByDate records results = Map.map (sort.Map.elems) recordsByDate
                               where initialTable = Map.fromList [(team record, record) | record <- records]
                                     recordsByDate = snd $ Map.mapAccum tableAccum initialTable results

-- Convert table for dates to a list of triples (team, date, position).  First argument is the highest position
-- to assign (usually set to one, but will be something else when calculating positions for the bottom half
-- post-split in the SPL and similar leagues).
tablesToPositions :: Int -> Map Day [LeagueRecord] -> [(Team, Day, Int)]
tablesToPositions startPos = concat . Map.elems . Map.mapWithKey (\matchDay records -> zip3 (map team records) (repeat matchDay) [startPos..]) 
                                      
tableAccum :: Map Team LeagueRecord -> [Result] -> (Map Team LeagueRecord, Map Team LeagueRecord)
tableAccum table results = (table', table')
                           where table' = foldl' addResultToTable table results

addResultToTable :: Map Team LeagueRecord -> Result -> Map Team LeagueRecord
addResultToTable table result = Map.adjustWithKey update hTeam $ Map.adjustWithKey update aTeam table
                                where update t record = addResultToRecord t record result
                                      hTeam = homeTeam result
                                      aTeam = awayTeam result
                        
addPosition :: (Team, Day, Int) -> Map Team [(Day, Int)] -> Map Team [(Day, Int)]
addPosition (t, d, p) = Map.adjust ((d, p):) t

-- | Given a number of matches that must be played before the league splits, divide the results into pre- and post-split fixtures.
--   If there is no split, there will be no post-split fixtures.
splitResults :: Int -> Map Day [Result] -> (Map Day [Result], Map Day [Result])
splitResults 0 results          = (results, Map.empty)
splitResults numMatches results = Map.partitionWithKey (\d _ -> d <= splitDate) results
                                  where splitDate = fromMaybe (fst $ Map.findMax results) . findSplitDate numMatches $ Map.toAscList results

-- | Given a number of matches that must be played before the league splits, determine the date for the last of the pre-split
--   fixtures.
findSplitDate :: Int -> [(Day, [Result])] -> Maybe Day
findSplitDate _ []                           = Nothing
findSplitDate numMatches ((d, results):days)
    | numMatches <= length results = Just d
    | otherwise                    = findSplitDate (numMatches - length results) days
