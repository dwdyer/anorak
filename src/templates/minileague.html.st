$header(title=name+" Mini-League")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9 tabs-below">
    <h2>$metaData.division$ $metaData.season$ - $name$ Mini-League</h2>
    <p>
      This table includes <strong>all matches</strong> played between <strong>$name$</strong> teams.  
      You can change the sort order by clicking on the column headings.
    </p>
    $leaguetable(miniLeague="true")$
    <ul class="nav nav-tabs">
      $bottomTabs:{
        $tab(label=it.("0");hint=it.("0");link=it.("1");selected=it.("2"))$
      }$
    </ul>
  </div>
</div>
$footer()$

