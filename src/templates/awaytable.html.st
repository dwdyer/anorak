$header(title="Away League Table")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Away League Table</h2>
    <p>
      This table includes <strong>away matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
    $leaguetabletabs(awaySelected="true")$
  </div>
</div>
$footer()$

