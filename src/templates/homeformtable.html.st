$header(title="Home Form Table (Last 4 Home Matches)")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Home Form Table (Last 4 Home Matches)</h2>
    <p>
      This table includes <strong>the last 4 home matches for each team</strong>.  
      You can change the sort order by clicking on the column headings.
      Move your mouse over the form column to see recent results.
    </p>
    $formtable(detail="Last 4 Home Matches")$
  </div>
  <div id="bottomtabs">
    <div class="tab">
      <a href="formtable.html">Overall</a>
    </div>
    <div class="tab selectedtab">
      <a href="#">Home</a>
    </div>
    <div class="tab">
      <a href="awayformtable.html">Away</a>
    </div>
  </div>
</div>
$footer()$

