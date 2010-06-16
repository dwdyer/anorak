$header(title="Second Half League Table")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Second Half League Table</h2>
    <p>
      This table is based on goals scored in the <strong>second half only</strong> of league matches.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  $leaguetabletabs(secondHalfSelected="true")$
</div>
$footer()$

