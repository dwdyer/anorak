$header(title="League Table")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - League Table</h2>
    <p class="description">
      This table includes <strong>all matches</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  $leaguetabletabs(overallSelected="true")$
</div>
$footer()$

