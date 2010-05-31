$header(title=team)$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$team$ - $metaData.division$ $metaData.season$</h2>
    $teamrecords()$
    <h3>Full Results</h3>
    <table id="results">
      $results:teamresult(dateformat="%a %e %b %Y";showOutcome="true";showScorers="true")$
    </table>
    <div class="clearfloat"></div>
  </div>
  <div id="bottomtabs"></div>
</div>
$footer()$
