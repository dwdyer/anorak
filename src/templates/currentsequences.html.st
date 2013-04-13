$header(title="Current Sequences";bootstrapJS="Y")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Current Sequences</h2>
    <p>
      <strong>Current</strong> unbroken sequences for <strong>all matches (home and away)</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
    <ul class="nav nav-tabs">
      $if(!metaData.neutral)$
        $tab(label="Overall";hint="Current Sequences (All Matches)";link="currentsequences.html";selected="true")$
        $tab(label="Home";hint="Current Home Sequences";link="homecurrentsequences.html")$
        $tab(label="Away";hint="Current Away Sequences";link="awaycurrentsequences.html")$
      $endif$
    </ul>
  </div>
</div>
$footer()$

