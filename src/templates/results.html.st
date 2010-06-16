$header(title="Results")$
<div id="centre">
  $selector()$
  $toptabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Results</h2>
    <table id="resultswrapper">
      <tr>
        <td>
          <table id="results">
            $results:matchday()$
          </table>
        </td>
        <td>
          $records()$
        </td>
      </tr>
    </table>
  </div>
</div>
$footer()$

