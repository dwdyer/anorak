$header(title=team)$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9">
    <h2>$team$ - $metaData.division$ $metaData.season$</h2>
    <div class="row">
      $teamrecords()$
    </div>
    <div class="row">
      <div class="span9">
        <h4>Full Results</h4>
        <table class="table table-condensed">
          $results:teamresult(dateformat="%a %e %b %Y";showOutcome="true";showScorers=metaData.hasScorers)$
        </table>
      </div>
    </div>
  </div>
</div>
$footer()$
