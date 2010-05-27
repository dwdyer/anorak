<table class="sortable">
  <thead>
    <tr>
      <th class="nosortindicator" title="Position"></th>
      <th class="sorttable_nosort"></th>
      <th title="Matches Played">P</th>
      <th title="Matches Won">W</th>
      <th title="Matches Drawn">D</th>
      <th title="Matches Lost">L</th>
      <th title="Goals For">F</th>
      <th title="Goals Against">A</th>
      <th title="Goal Difference">GD</th>
      <th title="Points">Pts</th>
      <th class="sorttable_nosort" title="$detail$ (point your cursor at an entry for full details)">Form</th></tr>
  </thead>
  <tbody>
    $table:{<tr>
      <td class="position">$i$.</td>
      <td><a href="$it.("0").("teamLink")$">$it.("0").("team")$</a></td>
      <td class="number">$it.("0").("played")$</td>
      <td class="number">$it.("0").("won")$</td>
      <td class="number">$it.("0").("drawn")$</td>
      <td class="number">$it.("0").("lost")$</td>
      <td class="number">$it.("0").("for")$</td>
      <td class="number">$it.("0").("against")$</td>
      $if(it.("0").("positiveGD"))$
      <td class="number positive">+$it.("0").("goalDiff")$</td>
      $elseif(it.("0").("negativeGD"))$
      <td class="number negative">$it.("0").("goalDiff")$</td>
      $else$
      <td class="number">0</td>
      $endif$
      <td class="number points">$it.("0").("points")$</td>
      <td class="form" onmouseover="this.className='form hover';return true;" onmouseout="this.className='form';return true;">
        <span class="hotspot">$it.("1"):{$it.("outcome")$}$</span>
        <div class="details">
          <strong>$it.("0").("team")$ $detail$</strong>
          $details(results=it.("1"))$
        </div>
      </td>
    </tr>}$
  </tbody>
</table>
