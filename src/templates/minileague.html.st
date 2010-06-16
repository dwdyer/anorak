$header(title=name+" Mini-League")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - $name$ Mini-League</h2>
    <p>
      This table includes <strong>all matches</strong> played between <strong>$name$</strong> teams.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable()$
  </div>
  <div id="bottomtabs">
    $bottomTabs:{
      $tab(label=it.("0");hint=it.("0");link=it.("1");selected=it.("2"))$
    }$
  </div>
</div>
$footer()$

