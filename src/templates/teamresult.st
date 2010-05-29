<tr>
  <td class="date">$it.day;format=dateformat$</td>
  <td class="opposition"><a href="$it.oppositionLink$">$it.opposition$</a> ($it.venue$)</td>
  <td class="score">$it.scored$ - $it.conceded$</td>
  <td class="outcome">$it.outcome$</td>
  $if(scorers)$
  <td class="scorers">
  $it.goals:{<span class="scorer">$it.("0")$ ($it.("1")$</span>)};separator=", "$
  </td>
  $endif$
</tr>
