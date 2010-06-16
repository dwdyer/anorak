$header(title="First Half League Table")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - First Half League Table</h2>
    <p>
      This table is based on goals scored in the <strong>first half only</strong> of league matches.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  $leaguetabletabs(firstHalfSelected="true")$
</div>
$footer()$

