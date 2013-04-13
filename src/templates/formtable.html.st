$header(title="Form Table (Last 6 Matches)";bootstrapJS="Y")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Form Table (Last 6 Matches)</h2>
    <p>
      This table includes <strong>the last 6 matches (home and away) for each team</strong>.  
      You can change the sort order by clicking on the column headings.
      Move your mouse over the form column to see recent results.
    </p>
    $formtable(detail="Last 6 Matches")$
    <ul class="nav nav-tabs">
      $if(!metaData.neutral)$
        $tab(label="Overall";hint="Overall Form";link="formtable.html";selected="true")$
        $tab(label="Home";hint="Home Form";link="homeformtable.html")$
        $tab(label="Away";hint="Away Form";link="awayformtable.html")$
      $endif$
    </ul>
  </div>
</div>
$footer()$

