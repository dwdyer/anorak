  <div id="bottomtabs">
    $tab(label="Overall";hint="Full League Table";link="index.html";selected=overallSelected)$
    $tab(label="Home";hint="Home League Table";link="hometable.html";selected=homeSelected)$
    $tab(label="Away";hint="Away League Table";link="awaytable.html";selected=awaySelected)$
    $if(metaData.hasScorers)$
      $tab(label="First Half";hint="First Half League Table";link="firsthalftable.html";selected=firstHalfSelected)$
      $tab(label="Second Half";hint="Second Half League Table";link="secondhalftable.html";selected=secondHalfSelected)$
    $endif$
  </div>
