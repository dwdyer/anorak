<div id="toptabs">
  <div class="tab $if(tableSelected)$selectedtab$endif$">
    <a href="$if(tableSelected)$#$else$index.html$endif$" title="League tables (including home and away tables)">Table</a>
  </div>
  <div class="tab $if(resultsSelected)$selectedtab$endif$">
    <a href="$if(resultsSelected)$#$else$results.html$endif$" title="Full results and overall records">Results</a>
  </div>
  <div class="tab $if(formSelected)$selectedtab$endif$">
    <a href="$if(formSelected)$#$else$formtable.html$endif$" title="Form tables (including home and away form)">Form</a>
  </div>
  $if(metaData.scorers)$
  <div class="tab $if(goalsSelected)$selectedtab$endif$">
    <a href="$if(goalsSelected)$#$else$goals.html$endif$" title="Leading goal-scorers, hat-tricks, etc.">Goals</a>
  </div>
  $endif$
  <div class="tab $if(currentSequencesSelected)$selectedtab$endif$">
    <a href="$if(currentSequencesSelected)$#$else$currentsequences.html$endif$" title="Current unbroken sequences (wins, draws, defeats, unbeaten runs, cleansheets, etc)">Current Sequences</a>
  </div>
  <div class="tab $if(longestSequencesSelected)$selectedtab$endif$">
    <a href="$if(longestSequencesSelected)$#$else$longestsequences.html$endif$" title="Longest sequences for each team (wins, draws, defeats, unbeaten runs, cleansheets, etc)">Longest Sequences</a>
  </div>
  <div class="tab $if(aggregatesSelected)$selectedtab$endif$">
    <a href="$if(aggregatesSelected)$#$else$aggregates.html$endif$" title="Total cleansheets, failures to score, etc.">Aggregates</a>
  </div>
  $if(metaData.miniLeaguesLink)$
  <div class="tab $if(miniLeaguesSelected)$selectedtab$endif$">
    <a href="$if(miniLeaguesSelected)$#$else$$metaData.miniLeaguesLink$$endif$" title="Regional tables and selected other groups">Mini-Leagues</a>
  </div>
  $endif$
</div>
