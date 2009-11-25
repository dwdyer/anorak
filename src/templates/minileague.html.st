$header(title=name+" Mini-League")$
<div id="centre">
  $selector()$
  $tabs()$
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
      <div class="tab $if(it.("2"))$selectedtab$endif$">
        <a href="$if(it.("2"))$#$else$$it.("1")$$endif$">$it.("0")$</a>
      </div>
    }$
  </div>
</div>
$footer()$

