$header(title="Longest Home Sequences")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Longest Home Sequences</h2>
    <p>
      The longest sequences achieved by each team in <strong>home matches only</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
  </div>
  <div id="bottomtabs">
    $if(!metaData.neutral)$
      $tab(label="Overall";hint="Longest Sequences (All Matches)";link="longestsequences.html")$
      $tab(label="Home";hint="Longest Home Sequences";link="homelongestsequences.html";selected="true")$
      $tab(label="Away";hint="Longest Away Sequences";link="awaylongestsequences.html")$
    $endif$
  </div>
</div>
$footer()$

