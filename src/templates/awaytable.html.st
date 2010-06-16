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
  <div id="bottomtabs">
    $tab(label="Overall";hint="Full League Table";link="index.html")$
    $tab(label="Home";hint="Home League Table";link="hometable.html")$
    $tab(label="Away";hint="Away League Table";link="awaytable.html";selected="true")$
    $if(metaData.hasScorers)$
      $tab(label="First Half";hint="First Half League Table";link="firsthalftable.html")$
      $tab(label="Second Half";hint="Second Half League Table";link="secondhalftable.html")$
    $endif$
  </div>
</div>
$footer()$

