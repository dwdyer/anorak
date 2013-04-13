$header(title="Results")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9">
    <h2>$metaData.division$ $metaData.season$ - Results</h2>
    <div class="row">
      <div class="span5">
        <table class="table table-condensed">
          $results:matchday()$
        </table>
      </div>
      $records()$
    </div>
  </div>
</div>
$footer()$

