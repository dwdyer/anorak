<tr><td colspan="3" class="matchday" id="$it.("0");format="%Y%m%d"$">$it.("0");format="%A %e %B %Y"$</td></tr>
$it.("1"):{<tr>
  <td class="hometeam">
    <a href="$metaData.teamLinks.(it.homeTeam)$">$it.homeTeam$</a>
    $if(metaData.hasScorers)$
    $if(first(it.homeGoals))$
      <br/><small class="muted">$it.homeGoals:{$it.("0")$ ($it.("1")$)};separator="<br/>"$</small>
    $endif$
    $endif$
  </td>
  <td class="score">$it.homeScore$&nbsp;-&nbsp;$it.awayScore$</td>
  <td class="awayteam">
    <a href="$metaData.teamLinks.(it.awayTeam)$">$it.awayTeam$</a>
    $if(metaData.hasScorers)$
    $if(first(it.awayGoals))$
      <br/><small class="muted">$it.awayGoals:{$it.("0")$ ($it.("1")$)};separator="<br/>"$</small>
    $endif$
    $endif$
  </td>
</tr>}$
<tr><td colspan="3" style="border: 0;">&nbsp;</td></tr>
