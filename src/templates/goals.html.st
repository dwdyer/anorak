$header(title="Goals")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Goal Scorers</h2>
    <table id="topscorers">
    $scorers:{
      <tr>
        <td>$it.("2").("0")$</td>
        <td><a href="$it.("1")$">$it.("0")$</a></td>
        <td class="number">$it.("2").("1")$</td>
      </tr>}$
    </table>
  </div>
  <div id="bottomtabs"></div>
</div>
$footer()$
