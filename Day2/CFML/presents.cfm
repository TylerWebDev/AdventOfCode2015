<cfsetting enablecfoutputonly="true" />
<cfset paper = 0 />
<cfset ribbon = 0 />
<cfloop index="line" file="input.txt">
  <cfset d = line.listSort('numeric','asc','x').listToArray('x') />
  <cfset paper += 3*d[1]*d[2] + 2*d[2]*d[3] + 2*d[1]*d[3] />
  <cfset ribbon += 2*d[1] + 2*d[2] + d[1]*d[2]*d[3] />
</cfloop>
<cfoutput>
  Day 2-1: Total sq. ft of paper needed: #paper#
  Day 2-2: Total ft of ribbon needed: #ribbon#
</cfoutput>