$header(title="Longest Sequences")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Longest Sequences</h2>
    <p>
      The longest sequences achieved by each team in <strong>all matches (home and away)</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
  </div>
  <div id="bottomtabs">
    $if(!metaData.neutral)$
      $tab(label="Overall";hint="Longest Sequences (All Matches)";link="longestsequences.html";selected="true")$
      $tab(label="Home";hint="Longest Home Sequences";link="homelongestsequences.html")$
      $tab(label="Away";hint="Longest Away Sequences";link="awaylongestsequences.html")$
    $endif$
  </div>
</div>
$footer()$

