<table id="teamrecords">
  <tr>
    <td id="piecharts">
      <img src="http://chart.apis.google.com/chart?chs=300x150&amp;cht=lc&amp;chd=t:$positions:{$it.("1")$};separator=","$&amp;chds=20,1&amp;chtt=League%20Position&amp;chma=0,0,0,0&amp;chco=0099cc" width="300" height="150" alt="League position."/>
      <img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$record.("1")$,$record.("3")$,$record.("5")$&amp;cht=p&amp;chtt=All%20Matches&amp;chdl=Wins%20($record.("0")$)|Draws%20($record.("2")$)|Defeats%20($record.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=009900,cc9900,cc0000&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="All matches pie chart."
      /><img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$homeRecord.("1")$,$homeRecord.("3")$,$homeRecord.("5")$&amp;cht=p&amp;chtt=Home&amp;chdl=Wins%20($homeRecord.("0")$)|Draws%20($homeRecord.("2")$)|Defeats%20($homeRecord.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=66ff66,ffff66,ff6666&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="Home matches pie chart."
      /><img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$awayRecord.("1")$,$awayRecord.("3")$,$awayRecord.("5")$&amp;cht=p&amp;chtt=Away&amp;chdl=Wins%20($awayRecord.("0")$)|Draws%20($awayRecord.("2")$)|Defeats%20($awayRecord.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=66ff66,ffff66,ff6666&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="Away matches pie chart."/>
    </td>
    <td>
      <table id="keyresults">
        <tr><th colspan="3"><h3>Biggest Home Wins</h3></th></tr>
        $bigHomeWins:teamresult(dateformat="%e %b %Y")$
        <tr><td colspan="3">&nbsp;</td></tr>
        <tr><th colspan="3"><h3>Biggest Away Wins</h3></th></tr>
        $bigAwayWins:teamresult(dateformat="%e %b %Y")$
        <tr><td colspan="3">&nbsp;</td></tr>
        <tr><th colspan="3"><h3>Highest Match Aggregates</h3></th></tr>
        $highAggregates:teamresult(dateformat="%e %b %Y")$
      </table>
    </td>
    <td>
      $if(metaData.hasScorers)$
      <table id="scorers">
        <tr><th colspan="2"><h3>Goal Scorers</h3></th></tr>
        $scorers:{
        <tr>
          <td>$it.("0")$</td>
          <td class="number">$it.("1")$</td>
        </tr>}$
      </table>
      $endif$
    </td>
  </tr>
</table>
