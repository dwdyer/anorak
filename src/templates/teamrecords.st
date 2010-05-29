<table id="teamrecords">
  <tr>
    <td>
      <img src="http://chart.apis.google.com/chart?chs=230x115&amp;chd=t:$winPercent$,$drawPercent$,$defeatPercent$&amp;cht=p&amp;chdl=Wins|Draws|Defeats&amp;chf=bg,s,ffffff00&amp;chco=009900,cc9900,cc0000&amp;chp=4.71" width="230" height="115" alt="Results pie chart." id="resultsPie"/>
      <table id="totals">
        <tr><td>Matches:</td><td class="number">$matches$</td></tr>
        <tr><td>Goals For:</td><td class="number">$goalsFor$</td></tr>
        <tr><td>Goals Against:</td><td class="number">$goalsAgainst$</td></tr>
        <tr><td colspan="2">&nbsp;</td></tr>
        <tr><td>Wins:</td><td class="number">$wins$</td></tr>
        <tr><td>Draws:</td><td class="number">$draws$</td></tr>
        <tr><td>Defeats:</td><td class="number">$defeats$</td></tr>
      </table>
    </td>
    <td>
      <table id="keyresults">
        <tr><th colspan="4" class="record"><h3>Biggest Home Wins</h3></th></tr>
        $bigHomeWins:teamresult(dateformat="%e %b %Y")$
        <tr><td colspan="4">&nbsp;</td></tr>
        <tr><th colspan="4" class="record"><h3>Biggest Away Wins</h3></th></tr>
        $bigAwayWins:teamresult(dateformat="%e %b %Y")$
        <tr><td colspan="4">&nbsp;</td></tr>
        <tr><th colspan="4" class="record"><h3>Highest Match Aggregates</h3></th></tr>
        $highAggregates:teamresult(dateformat="%e %b %Y")$
      </table>
    </td>
  </tr>
</table>

