$header(title="Home League Table")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Home League Table</h2>
    <p>
      This table includes <strong>home matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    $tab(label="Overall";hint="Full League Table";link="index.html")$
    $tab(label="Home";hint="Home League Table";link="#";selected="true")$
    $tab(label="Away";hint="Away League Table";link="awaytable.html")$
    $if(metaData.hasScorers)$
    $tab(label="First Half";hint="First Half League Table";link="firsthalftable.html")$
    $tab(label="Second Half";hint="Second Half League Table";link="secondhalftable.html")$
    $endif$
  </div>
</div>
$footer()$

