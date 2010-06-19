$header(title="Current Home Sequences")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Current Home Sequences</h2>
    <p>
      <strong>Current</strong> unbroken sequences for <strong>home matches only</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
  </div>
  <div id="bottomtabs">
    $if(!metaData.neutral)$
      $tab(label="Overall";hint="Current Sequences (All Matches)";link="currentsequences.html")$
      $tab(label="Home";hint="Current Home Sequences";link="homecurrentsequences.html";selected="true")$
      $tab(label="Away";hint="Current Away Sequences";link="awaycurrentsequences.html")$
    $endif$
  </div>
</div>
$footer()$

