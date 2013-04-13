$header(title="Home League Table")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Home League Table</h2>
    <p>
      This table includes <strong>home matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
    $leaguetabletabs(homeSelected="true")$
  </div>
</div>
$footer()$

