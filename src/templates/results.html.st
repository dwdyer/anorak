$header(title="Results")$
<div id="centre">
  $selector()$
  $tabs()$
  <div id="content">
    <h2>$metaData.division$ $metaData.season$ - Results</h2>
    <table id="results">
      <tr>
        <td>
          <table>
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

