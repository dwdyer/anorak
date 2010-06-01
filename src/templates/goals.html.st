$header(title="Goals")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Goal Scorers</h2>
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
