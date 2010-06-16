$header(title="Away League Table")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Away League Table</h2>
    <p>
      This table includes <strong>away matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  $leaguetabletabs(awaySelected="true")$
</div>
$footer()$

