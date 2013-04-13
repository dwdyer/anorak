<div class="span4">
  $if(!metaData.neutral)$
    <img src="http://chart.apis.google.com/chart?chs=280x160&amp;chd=t:$homeWinPercent$,$drawPercent$,$awayWinPercent$&amp;cht=p&amp;chdl=Home%20Wins|Draws|Away%20Wins&amp;chf=bg,s,ffffff00&amp;chco=46a546,ffc40d,c3325f&amp;chp=4.71&amp;chma=0,0,0,20" width="280" height="160" alt="Results pie chart."/>
  $endif$
  <table id="totals" class="table table-condensed">
    <tr><td>Matches</td><td class="number">$matches$</td></tr>
    <tr><td>Goals</td><td class="number">$goals$</td></tr>
    $if(!metaData.neutral)$
      <tr><td>Home Wins</td><td class="number">$homeWins$</td></tr>
      <tr><td>Draws</td><td class="number">$draws$</td></tr>
      <tr><td>Away Wins</td><td class="number">$awayWins$</td></tr>
    $endif$
  </table>
  $if(metaData.neutral)$
    <h4>Biggest Wins</h4>
    <table class="table table-condensed">
      $bigWins:result()$
    </table>
  $else$
    <h4>Biggest Home Wins</h4>
    <table class="table table-condensed">
      $bigHomeWins:result()$
    </table>
    <h4>Biggest Away Wins</h4>
    <table class="table table-condensed">
      $bigAwayWins:result()$
    </table>
  $endif$
  <h4>Highest Match Aggregates</h4>
  <table class="table table-condensed">
    $highAggregates:result()$
  </table>
</div>
