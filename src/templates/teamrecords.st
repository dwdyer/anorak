<table id="teamrecords">
  <tr>
    <td id="charts">
      <div id="piecharts">
        <img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$record.("1")$,$record.("3")$,$record.("5")$&amp;cht=p&amp;chtt=All+Matches&amp;chdl=Wins+($record.("0")$)|Draws+($record.("2")$)|Defeats+($record.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=66cc66,ffee66,ff6666&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="All matches pie chart."
        />$if(!metaData.neutral)$<img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$homeRecord.("1")$,$homeRecord.("3")$,$homeRecord.("5")$&amp;cht=p&amp;chtt=Home&amp;chdl=Wins+($homeRecord.("0")$)|Draws+($homeRecord.("2")$)|Defeats+($homeRecord.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=66cc66,ffee66,ff6666&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="Home matches pie chart."
        /><img src="http://chart.apis.google.com/chart?chs=100x175&amp;chd=t:$awayRecord.("1")$,$awayRecord.("3")$,$awayRecord.("5")$&amp;cht=p&amp;chtt=Away&amp;chdl=Wins+($awayRecord.("0")$)|Draws+($awayRecord.("2")$)|Defeats+($awayRecord.("4")$)&amp;chf=bg,s,ffffff00&amp;chco=66cc66,ffee66,ff6666&amp;chp=4.71&amp;chdlp=bv&amp;chma=0,0,0,0" width="100" height="175" alt="Away matches pie chart."/>$endif$
      </div>
      $if(!metaData.isAggregated)$
      $if(!metaData.isCollated)$
      <img src="http://chart.apis.google.com/chart?chs=300x175&amp;cht=lc&amp;chd=t:$positions:{$it.("1")$};separator=","$&amp;chds=$teamCount$,0&amp;chtt=League+Position&amp;chf=bg,s,ffffff00&amp;chco=0099cc&amp;chxt=y&amp;chxr=0,$teamCount$,0,5" width="300" height="175" alt="League position."/>
      $endif$
      $endif$
      $if(metaData.hasScorers)$
      <img src="http://chart.apis.google.com/chart?chs=300x175&amp;cht=bvs&amp;chbh=28,2&amp;chdl=For|Against&amp;chdlp=b&amp;chd=t:$goalsByInterval.("0"):{$it$};separator=","$|$goalsByInterval.("1"):{-$it$};separator=","$&amp;chds=-$intervalMaxima.("1")$,$intervalMaxima.("0")$&amp;chxt=x,y&amp;chxr=1,-$intervalMaxima.("1")$,$intervalMaxima.("0")$,5&amp;chxl=0:|1-10|11-20|21-30|31-40|41-50|51-60|61-70|71-80|81-90&amp;chco=66cc66,ff6666&amp;chf=bg,s,ffffff00&amp;chtt=Goal+Times&amp;chxs=0,,9" width="300" height="175" alt="Goals by minute."/>
      $endif$
    </td>
    <td>
      $if(metaData.hasScorers)$
      <table id="scorers">
        <tr><th colspan="2"><h3>$if(metaData.isAggregated)$Top $endif$Goal Scorers</h3></th></tr>
        $scorers:{
        <tr>
          <td>$it.("0")$</td>
          <td class="number">$it.("1")$</td>
        </tr>}$
        $if(!metaData.isAggregated)$
        <tr class="owngoals">
          <td>Own Goals</td>
          <td class="number">$ownGoals$</td>
        </tr>
        $endif$
      </table>
      $endif$
    </td>
  </tr>
</table>
