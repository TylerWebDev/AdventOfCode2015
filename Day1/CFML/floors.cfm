<cfsetting enablecfoutputonly="true" />
<cffile file="input.txt" action="read" variable="data" />
<cfset floor = 0 />
<cfset position = 0 />
<cfset firstBelow = -1 />
<cfloop list="#data#" index="c" delimiters="">
  <cfset position++ />

  <cfif c EQ '('>
    <cfset floor++ />
  <cfelseif c EQ ')' >
    <cfset floor-- />
  </cfif>

  <cfif firstBelow EQ -1 AND floor LT 0>
    <cfset firstBelow = position />
  </cfif>
</cfloop>
<cfoutput>
Day 1-1: Final floor is #floor#
Day 1-2: First position to dip below 0 is #firstBelow#
</cfoutput>