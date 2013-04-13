$header(title="Goals")$
$selector()$
<div class="row">
  $toptabs()$
  <div class="span9">
    <h2>$metaData.division$ $metaData.season$ - Goal Scorers</h2>
    <p class="description">
      This page shows the top scorers in the league.  See individual team pages for a complete list of scorers for each team.
    </p>
    <div class="row">
      <div class="span5">
        <h4>Top Goal Scorers</h4>
        <table id="topscorers" class="table table-condensed">
          $scorers:scorer()$
        </table>
      </div>
      <div class="span4">
        <h4>Most Goals From Penalties</h4>
        <table id="penalties" class="table table-condensed">
          $penalties:scorer()$
        </table>
      </div>
    </div>
    <h4>Hat-Tricks</h4>
    <table id="hattricks" class="table table-condensed">
      $hatTricks:hattrick()$
    </table>
  </div>
</div>
$footer()$
