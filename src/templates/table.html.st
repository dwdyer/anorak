$header(title="League Table")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - League Table</h2>
    <p class="description">
      This table includes <strong>all matches</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    <div class="tab selectedtab">
      <a href="#">Overall</a>
    </div>
    <div class="tab">
      <a href="hometable.html">Home</a>
    </div>
    <div class="tab">
      <a href="awaytable.html">Away</a>
    </div>
  </div>
</div>
$footer()$

