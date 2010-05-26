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
            $results:teamresult(dateformat="%a %e %b %Y")$
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
