<table>
  $results:{<tr>
    <td class="date"><small>$it.day;format="%a %e %b %Y"$</small></td>
    <td class="opposition">$it.opposition$$if(!metaData.neutral)$ ($it.venue$)$endif$</td>
    <td class="score">$it.scored$ - $it.conceded$</td>
    <td class="outcome">$it.outcome$</td>
  </tr>}$
</table>

