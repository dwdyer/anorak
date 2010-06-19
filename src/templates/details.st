<table>
  $results:{<tr>
    <td class="date">$it.day;format="%a %e %b %Y"$</td>
    <td class="opposition"><a href="$metaData.teamLinks.(it.opposition)$">$it.opposition$</a>$if(!metaData.neutral)$ ($it.venue$)$endif$</td>
    <td class="score">$it.scored$ - $it.conceded$</td>
    <td class="outcome">$it.outcome$</td>
  </tr>}$
</table>

