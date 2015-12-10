<cfset x = 0 />
<cfset y = 0 />
<cfset houses = {'0,0':true} />
<cfloop file="input.txt" characters="1" index="c">
  <cfswitch expression="#c#">
    <cfcase value="^"><cfset y++ /></cfcase>
    <cfcase value=">"><cfset x++ /></cfcase>
    <cfcase value="v"><cfset y-- /></cfcase>
    <cfcase value="<"><cfset x-- /></cfcase>
  </cfswitch>
  <cfset houses['#x#,#y#'] = true />
</cfloop>
<cfoutput>
Day 3-1: Total houses visited by santa: #houses.count()#
</cfoutput>


<cfset houses = {
  santa: { position:'0,0' }
  ,robo: { position:'0,0' }
  ,visited: {'0,0':true }
} />
<cfset who = 'robo' />
<cfloop file="input.txt" characters="1" index="c">
  <cfset who = IIF(who EQ 'robo',DE('santa'),DE('robo')) />
  <cfset x = houses[who].position.listFirst() />
  <cfset y = houses[who].position.listLast() />
  <cfswitch expression="#c#">
    <cfcase value="^"><cfset y++ /></cfcase>
    <cfcase value=">"><cfset x++ /></cfcase>
    <cfcase value="v"><cfset y-- /></cfcase>
    <cfcase value="<"><cfset x-- /></cfcase>
  </cfswitch>
  <cfset houses.visited['#x#,#y#'] = true />
  <cfset houses[who].position = '#x#,#y#' />
</cfloop>
<cfoutput>
Day 3-2: Total houses visited by santa+robo: #houses.visited.count()#
</cfoutput>