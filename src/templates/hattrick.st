<tr>
  <td class="date"><small>$it.("3").day;format="%a %e %b %Y"$</small></td>
  <td>$it.("0")$</td>
  <td><a href="$metaData.teamLinks.(it.("2"))$">$it.("2")$</a></td>
  <td><b>$it.("1")$</b> goals</td>
  <td>$it.("3").scored$-$it.("3").conceded$</td>
  <td><small>vs.</small> <a href="$metaData.teamLinks.(it.("3").opposition)$">$it.("3").opposition$</a>$if(!metaData.neutral)$ ($it.("3").venue$)$endif$</td>
</tr>
