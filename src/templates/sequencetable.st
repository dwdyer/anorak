<table class="table table-condensed">
  $sequence:{<tr>
    <td><a href="$metaData.teamLinks.(it.("0"))$">$it.("0")$</a></td>
    <td>
      <span class="hotspot number" id="seq$n$$i$" title="<b>$it.("0")$</b>">$length(it.("1"))$</span>
      <script type="text/javascript">
        \$('#seq$n$$i$').popover(\{html:true,trigger:'hover',placement:'left',content:function()\{return \$('#details$n$$i$').html();\}\}); 
      </script>
      <div id="details$n$$i$" class="hide">
        $details(results=it.("1"))$
      </div>
    </td>
  </tr>}$
</table>

