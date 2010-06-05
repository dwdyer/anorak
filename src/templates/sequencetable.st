<table>
  $sequence:{<tr>
    <td><a href="$metaData.teamLinks.(it.("0"))$">$it.("0")$</a></td>
    <td class="number" onmouseover="this.className='number hover';return true;" onmouseout="this.className='number';return true;">
      <span class="hotspot">$length(it.("1"))$</span
      ><div class="shadowwrapper">
        <div class="details">
          <div class="shadow"></div>
          <strong>$it.("0")$</strong>
          $details(results=it.("1"))$
        </div>
      </div>
    </td>
  </tr>}$
</table>

