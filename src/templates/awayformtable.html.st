$header(title="Away Form Table (Last 4 Away Matches)")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Away Form Table (Last 4 Away Matches)</h2>
    <p>
      This table includes <strong>the last 4 away matches for each team</strong>.  
      You can change the sort order by clicking on the column headings.
      Move your mouse over the form column to see recent results.
    </p>
    $formtable(detail="Last 4 Away Matches")$
  </div>
  <div id="bottomtabs">
    <div class="tab">
      <a href="formtable.html">Overall</a>
    </div>
    <div class="tab">
      <a href="homeformtable.html">Home</a>
    </div>
    <div class="tab selectedtab">
      <a href="#">Away</a>
    </div>
  </div>
</div>
$footer()$

