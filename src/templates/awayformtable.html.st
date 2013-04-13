$header(title="Away Form Table (Last 4 Away Matches)";bootstrapJS="Y")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Away Form Table (Last 4 Away Matches)</h2>
    <p>
      This table includes <strong>the last 4 away matches for each team</strong>.  
      You can change the sort order by clicking on the column headings.
      Move your mouse over the form column to see recent results.
    </p>
    $formtable(detail="Last 4 Away Matches")$
    <ul class="nav nav-tabs">
      $if(!metaData.neutral)$
        $tab(label="Overall";hint="Overall Form";link="formtable.html")$
        $tab(label="Home";hint="Home Form";link="homeformtable.html";)$
        $tab(label="Away";hint="Away Form";link="awayformtable.html";selected="true")$
      $endif$
    </ul>
  </div>
</div>
$footer()$

