$header(title="League Table")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - League Table</h2>
    <p class="description">
      This table includes <strong>all matches</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
    $leaguetabletabs(overallSelected="true")$
  </div>
</div>
$footer()$

