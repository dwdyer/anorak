$header(title="Current Sequences")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Current Sequences</h2>
    <p>
      <strong>Current</strong> unbroken sequences for <strong>all matches (home and away)</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
  </div>
  <div id="bottomtabs">
    $tab(label="Overall";hint="Current Sequences (All Matches)";link="#";selected="true")$
    $tab(label="Home";hint="Current Home Sequences";link="homecurrentsequences.html")$
    $tab(label="Away";hint="Current Away Sequences";link="awaycurrentsequences.html")$
  </div>
</div>
$footer()$

