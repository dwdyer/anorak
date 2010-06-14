$header(title="Goals")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Goals</h2>
    <p class="description">
      See individual team pages for a complete list of scorers for each team.
    </p>
    <table id="penalties">
      <tr><td colspan="3"><h3>Most Goals From Penalties</h3></td></tr>
      $penalties:scorer()$
    </table>
    <table id="topscorers">
      <tr><td colspan="3"><h3>Top Goal Scorers</h3></td></tr>
      $scorers:scorer()$
    </table>
  </div>
  <div id="bottomtabs"></div>
</div>
$footer()$
