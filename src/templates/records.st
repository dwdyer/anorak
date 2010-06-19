<div id="records">
  $if(!metaData.neutral)$
    <img src="http://chart.apis.google.com/chart?chs=230x115&amp;chd=t:$homeWinPercent$,$drawPercent$,$awayWinPercent$&amp;cht=p&amp;chdl=Home%20Wins|Draws|Away%20Wins&amp;chf=bg,s,ffffff00&amp;chco=009900,cc9900,cc0000&amp;chp=4.71" width="230" height="115" alt="Results pie chart." id="resultsPie"/>
  $endif$
  <table id="totals">
    <tr><td>Matches:</td><td class="number">$matches$</td></tr>
    <tr><td>Goals:</td><td class="number">$goals$</td></tr>
    $if(!metaData.neutral)$
      <tr><td colspan="2">&nbsp;</td></tr>
      <tr><td>Home Wins:</td><td class="number">$homeWins$</td></tr>
      <tr><td>Draws:</td><td class="number">$draws$</td></tr>
      <tr><td>Away Wins:</td><td class="number">$awayWins$</td></tr>
    $endif$
  </table>
  <table id="keyresults">
    $if(metaData.neutral)$
      <tr><th colspan="4" class="record"><h3>Biggest Wins</h3></th></tr>
      $bigWins:result()$
    $else$
      <tr><th colspan="4" class="record"><h3>Biggest Home Wins</h3></th></tr>
      $bigHomeWins:result()$
      <tr><td colspan="4">&nbsp;</td></tr>
      <tr><th colspan="4" class="record"><h3>Biggest Away Wins</h3></th></tr>
      $bigAwayWins:result()$
    $endif$
    <tr><td colspan="4">&nbsp;</td></tr>
    <tr><th colspan="4" class="record"><h3>Highest Match Aggregates</h3></th></tr>
    $highAggregates:result()$
  </table>
</div>
