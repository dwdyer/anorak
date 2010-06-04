$header(title="Goals")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Goals</h2>
    <p class="description">
      This page shows the top scorers in the league.  See individual team pages for a complete list of scorers for each club.
    </p>
    <table id="topscorers">
    $scorers:{
      <tr>
        <td>$it.("0")$</td>
        <td>$it.("2"):{<a href="$it.("1")$">$it.("0")$</a>};separator=" / "$</td>
        <td class="number">$it.("1")$</td>
      </tr>}$
    </table>
  </div>
  <div id="bottomtabs"></div>
</div>
$footer()$
