<tr>
  <td class="date"><small>$it.day;format=dateformat$</small></td>
  <td class="opposition"><a href="$metaData.teamLinks.(it.opposition)$">$it.opposition$</a>$if(!metaData.neutral)$ ($it.venue$)$endif$</td>
  <td class="score">$it.scored$ - $it.conceded$</td>
  $if(showOutcome)$
  <td class="outcome">$it.outcome$</td>
  $endif$
  $if(showScorers)$
  <td class="scorers">
    <small class="muted">$it.goals:{<span class="scorer">$it.("0")$ ($it.("1")$)</span>};separator=", "$</small>
  </td>
  $endif$
</tr>
