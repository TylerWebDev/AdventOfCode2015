<cfscript> 
data = FileRead('input.txt');
firstBelow = -1;
floor = data.listReduce(function (result, item, index) {
  if (item == '(') {
    return result+1;
  }
  else if (item == ')') {
    if (firstBelow == -1 && result == 0) {
      firstBelow = index; 
    }
    return result-1;
  }
  return result;
}, 0, '');
</cfscript>
<cfoutput>
Day 1-1: Final floor is #floor#
Day 1-2: First position to dip below 0 is #firstBelow#
</cfoutput>