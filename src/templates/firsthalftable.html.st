$header(title="First Half League Table")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - First Half League Table</h2>
    <p>
      This table is based on goals scored in the <strong>first half only</strong> of league matches.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    $tab(label="Overall";hint="Full League Table";link="index.html")$
    $tab(label="Home";hint="Home League Table";link="hometable.html")$
    $tab(label="Away";hint="Away League Table";link="awaytable.html")$
    $tab(label="First Half";hint="First Half League Table";link="#";selected="true")$
    $tab(label="Second Half";hint="Second Half League Table";link="secondhalftable.html")$
  </div>
</div>
$footer()$

