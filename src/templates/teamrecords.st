<div class="span6">
  <div class="row">
    <div class="span2">
      $resultspie(title="All+Matches";wins=record.("1");draws=record.("3");defeats=record.("5");winPercent=record.("0");drawPercent=record.("2");losePercent=record.("4"))$
    </div>
    $if(!metaData.neutral)$
    <div class="span2">
      $resultspie(title="Home";wins=homeRecord.("1");draws=homeRecord.("3");defeats=homeRecord.("5");winPercent=homeRecord.("0");drawPercent=homeRecord.("2");losePercent=homeRecord.("4"))$
    </div>
    <div class="span2">
      $resultspie(title="Away";wins=awayRecord.("1");draws=awayRecord.("3");defeats=awayRecord.("5");winPercent=awayRecord.("0");drawPercent=awayRecord.("2");losePercent=awayRecord.("4"))$
    </div>
    $endif$
  </div>
  $if(metaData.hasScorers)$
  <img src="http://chart.apis.google.com/chart?chs=420x240&amp;cht=bvs&amp;chbh=39,5&amp;chdl=For|Against&amp;chdlp=b&amp;chd=t:$goalsByInterval.("0"):{$it$};separator=","$|$goalsByInterval.("1"):{-$it$};separator=","$&amp;chds=-$intervalMaxima.("1")$,$intervalMaxima.("0")$&amp;chxt=x,y&amp;chxr=1,-$intervalMaxima.("1")$,$intervalMaxima.("0")$,5&amp;chxl=0:|1-10|11-20|21-30|31-40|41-50|51-60|61-70|71-80|81-90&amp;chco=66cc66,ff6666&amp;chf=bg,s,ffffff00&amp;chtt=Goal+Times&amp;chxs=0,,9" width="420" height="240" alt="Goals by minute."/>
  $endif$
</div>
<div class="span3">
  $if(!metaData.isAggregated)$
  $if(!metaData.isCollated)$
  <img src="http://chart.apis.google.com/chart?chs=220x240&amp;cht=lc&amp;chd=t:$positions:{$it.("1")$};separator=","$&amp;chds=$teamCount$,0&amp;chtt=League+Position&amp;chf=bg,s,ffffff00&amp;chco=049cdb&amp;chxt=y&amp;chxr=0,$teamCount$,0,5&amp;chma=0,0,0,20" width="220" height="240" alt="League position."/>
  $endif$
  $endif$

  $if(metaData.hasScorers)$
  <h4>$if(metaData.isAggregated)$Top $endif$Goal Scorers</h4>
  <table id="scorers" class="table table-condensed">
    $scorers:{
    <tr>
      <td>$it.("0")$</td>
      <td class="number">$it.("1")$</td>
    </tr>}$
    $if(!metaData.isAggregated)$
    <tr class="muted">
      <td><em>Own Goals</em></td>
      <td class="number"><em>$ownGoals$</em></td>
    </tr>
    $endif$
  </table>
  $endif$
</div>
