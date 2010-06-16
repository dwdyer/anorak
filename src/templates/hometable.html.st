$header(title="Home League Table")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Home League Table</h2>
    <p>
      This table includes <strong>home matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  $leaguetabletabs(homeSelected="true")$
</div>
$footer()$

