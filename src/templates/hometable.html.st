$header(title="Home League Table")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Home League Table</h2>
    <p>
      This table includes <strong>home matches only</strong>.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    <div class="tab">
      <a href="index.html">Overall</a>
    </div>
    <div class="tab selectedtab">
      <a href="#">Home</a>
    </div>
    <div class="tab">
      <a href="awaytable.html">Away</a>
    </div>
  </div>
</div>
$footer()$

