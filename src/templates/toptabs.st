<div id="toptabs">
  $tab(label="Table";hint="League tables (including home and away tables)";link="index.html";selected=tableSelected)$
  $tab(label="Results";hint="Full results and overall records";link="results.html";selected=resultsSelected)$
  $if(!metaData.isArchive)$
    $tab(label="Form";hint="Form tables (including home and away form)";link="formtable.html";selected=formSelected)$
  $endif$
  $if(metaData.hasScorers)$
    $tab(label="Goals";hint="Leading goal-scorers, hat-tricks, etc.";link="goals.html";selected=goalsSelected)$
  $endif$
  $if(!metaData.isArchive)$
    $tab(label="Current Sequences";hint="Current unbroken sequences (wins, draws, defeats, unbeaten runs, cleansheets, etc.)";link="currentsequences.html";selected=currentSequencesSelected)$
  $endif$
  $tab(label="Longest Sequences";hint="Longest sequences for each team (wins, draws, defeats, unbeaten runs, cleansheets, etc.)";link="longestsequences.html";selected=longestSequencesSelected)$
  $tab(label="Aggregates";hint="Total cleansheets, failures to score, etc.";link="aggregates.html";selected=aggregatesSelected)$
  $if(metaData.miniLeaguesLink)$
    $tab(label="Mini-Leagues";hint="Regional tables and selected other groups";link=metaData.miniLeaguesLink;selected=miniLeaguesSelected)$
  $endif$
</div>
