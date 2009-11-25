$header(title="Form Table (Last 6 Matches)")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Form Table (Last 6 Matches)</h2>
    <p>
      This table includes <strong>the last 6 matches (home and away) for each team</strong>.  
      You can change the sort order by clicking on the column headings.
      Move your mouse over the form column to see recent results.
    </p>
    $formtable(detail="Last 6 Matches")$
  </div>
  <div id="bottomtabs">
    <div class="tab selectedtab">
      <a href="#">Overall</a>
    </div>
    <div class="tab">
      <a href="homeformtable.html">Home</a>
    </div>
    <div class="tab">
      <a href="awayformtable.html">Away</a>
    </div>
  </div>
</div>
$footer()$

