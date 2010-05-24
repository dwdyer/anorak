$header(title=team)$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$team$ - $metaData.division$ $metaData.season$</h2>
    <table id="results">
      <tr>
        <td>
          <table>
            $results:{<tr>
              <td class="date">$it.("day");format="%a %e %b %Y"$</td>
              <td class="opposition"><a href="$it.("oppositionLink")$">$it.("opposition")$</a> ($it.("venue")$)</td>
              <td class="score">$it.("scored")$ - $it.("conceded")$</td>
              <td class="outcome">$it.("outcome")$</td>
            </tr>}$
          </table>
        </td>
        <td>
          $teamrecords()$
        </td>
      </tr>
    </table>
    <div class="clearfloat"></div>
  </div>
  <div id="bottomtabs"></div>
</div>
$footer()$
