$header(title="Second Half League Table")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Second Half League Table</h2>
    <p>
      This table is based on goals scored in the <strong>second half only</strong> of league matches.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    <div class="tab">
      <a href="index.html">Overall</a>
    </div>
    <div class="tab">
      <a href="hometable.html">Home</a>
    </div>
    <div class="tab">
      <a href="awaytable.html">Away</a>
    </div>
    <div class="tab">
      <a href="firsthalftable.html">First Half</a>
    </div>
    <div class="tab selectedtab">
      <a href="#">Second Half</a>
    </div>
  </div>
</div>
$footer()$

