$header(title="Longest Away Sequences";bootstrapJS="Y")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - Longest Away Sequences</h2>
    <p>
      The longest sequences achieved by each team in <strong>away matches only</strong>.
      Move your mouse over the numbers to see the sequence details.
    </p>
    $sequences()$
    <ul class="nav nav-tabs">
      $if(!metaData.neutral)$
        $tab(label="Overall";hint="Longest Sequences (All Matches)";link="longestsequences.html")$
        $tab(label="Home";hint="Longest Home Sequences";link="homelongestsequences.html")$
        $tab(label="Away";hint="Longest Away Sequences";link="awaylongestsequences.html";selected="true")$
     $endif$
    </ul>
  </div>
</div>
$footer()$

