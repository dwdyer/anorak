$header(title="Current Away Sequences")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Current Away Sequences</h2>
    <p>
      <strong>Current</strong> unbroken sequences for <strong>away matches only</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
  </div>
  <div id="bottomtabs">
    $tab(label="Overall";hint="Current Sequences (All Matches)";link="currentsequences.html")$
    $tab(label="Home";hint="Current Home Sequences";link="homecurrentsequences.html")$
    $tab(label="Away";hint="Current Away Sequences";link="#";selected="true")$
  </div>
</div>
$footer()$

