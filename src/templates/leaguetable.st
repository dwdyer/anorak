<table>
  <tr>
    <td>
      <table>
        <tr><th>&nbsp;</th></tr>
        $table:{<tr><td class="position">$i$.</td></tr>}$
      </table>
    </td>
    <td>
      <table class="sortable">
        <thead>
          <tr>
            <th class="sorttable_nosort">&nbsp;</th>
            <th title="Matches Played">P</th>
            <th title="Matches Won">W</th>
            <th title="Matches Drawn">D</th>
            <th title="Matches Lost">L</th>
            <th title="Goals For">F</th>
            <th title="Goals Against">A</th>
            <th title="Goal Difference">GD</th>
            <th title="Points">Pts.</th>
            <th title="Average Points Per Game">Av. Pts.</th></tr>
        </thead>
        <tbody>
          $table:{<tr>
            <td><a href="$it.("teamLink")$">$it.("team")$</a></td>
            <td class="number">$it.("played")$</td>
            <td class="number">$it.("won")$</td>
            <td class="number">$it.("drawn")$</td>
            <td class="number">$it.("lost")$</td>
            <td class="number">$it.("for")$</td>
            <td class="number">$it.("against")$</td>
            $if(it.("positiveGD"))$
            <td class="number positive">+$it.("goalDiff")$</td>
            $elseif(it.("negativeGD"))$
            <td class="number negative">$it.("goalDiff")$</td>
            $else$
            <td class="number">0</td>
            $endif$
            <td class="number points">
              $it.("points")$$if(it.("adjustment"))$<span class="adjustment">($it.("adjustment")$)</span>$endif$
            </td>
            <td class="number">($it.("average");format="2"$)</td>
          </tr>}$
        </tbody>
      </table>
    </td>
  </tr>
</table>

