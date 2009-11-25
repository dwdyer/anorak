$header(title=team)$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$team$ - $metaData.division$ $metaData.season$</h2>
    <table>
      $results:{<tr>
        <td class="date"><a href="results.html#$it.("day");format="%Y%m%d"$">$it.("day");format="%a %e %b %Y"$</a></td>
        <td class="oposition">$it.("opposition")$ ($it.("venue")$)</td>
        <td class="score">$it.("scored")$ - $it.("conceded")$</td>
        <td class="outcome">$it.("outcome")$</td>
      </tr>}$
    </table>
  </div>
</div>
$footer()$
