<div id="toptabs">
  <div class="tab $if(tableSelected)$selectedtab$endif$">
    <a href="$if(tableSelected)$#$else$index.html$endif$">Table</a>
  </div>
  <div class="tab $if(resultsSelected)$selectedtab$endif$">
    <a href="$if(resultsSelected)$#$else$results.html$endif$">Results</a>
  </div>
  <div class="tab $if(formSelected)$selectedtab$endif$">
    <a href="$if(formSelected)$#$else$formtable.html$endif$">Form</a>
  </div>
  <div class="tab $if(currentSequencesSelected)$selectedtab$endif$">
    <a href="$if(currentSequencesSelected)$#$else$currentsequences.html$endif$">Current Sequences</a>
  </div>
  <div class="tab $if(longestSequencesSelected)$selectedtab$endif$">
    <a href="$if(longestSequencesSelected)$#$else$longestsequences.html$endif$">Longest Sequences</a>
  </div>
  $if(metaData.miniLeaguesLink)$
  <div class="tab $if(miniLeaguesSelected)$selectedtab$endif$">
    <a href="$if(miniLeaguesSelected)$#$else$$metaData.miniLeaguesLink$$endif$">Mini-Leagues</a>
  </div>
  $endif$
</div>
