<div class="span3">
  <ul class="nav nav-tabs nav-stacked affix">
    $tab(label="Table";hint="League tables (including home and away tables)";link="index.html";selected=tableSelected;chevron="Y")$
    $tab(label="Results";hint="Full results and overall records";link="results.html";selected=resultsSelected;chevron="Y")$
    $if(!metaData.isArchive)$
      $tab(label="Form";hint="Form tables (including home and away form)";link="formtable.html";selected=formSelected;chevron="Y")$
    $endif$
    $if(metaData.hasScorers)$
      $tab(label="Goals";hint="Leading goal-scorers, hat-tricks, etc.";link="goals.html";selected=goalsSelected;chevron="Y")$
    $endif$
    $if(!metaData.isArchive)$
      $tab(label="Current Sequences";hint="Current unbroken sequences (wins, draws, defeats, unbeaten runs, cleansheets, etc.)";link="currentsequences.html";selected=currentSequencesSelected;chevron="Y")$
    $endif$
    $tab(label="Longest Sequences";hint="Longest sequences for each team (wins, draws, defeats, unbeaten runs, cleansheets, etc.)";link="longestsequences.html";selected=longestSequencesSelected;chevron="Y")$
    $tab(label="Cleansheets";hint="Total cleansheets, failures to score, no-score draws.";link="aggregates.html";selected=aggregatesSelected;chevron="Y")$
    $if(metaData.miniLeaguesLink)$
      $tab(label="Mini-Leagues";hint="Regional tables and selected other groups";link=metaData.miniLeaguesLink;selected=miniLeaguesSelected;chevron="Y")$
    $endif$
  </ul>
</div>
