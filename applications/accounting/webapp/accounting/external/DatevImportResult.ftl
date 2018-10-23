<#if stats?has_content>
	<#if fatalMessages?has_content>
		<@alert type="error">
			<ul>
				<#list fatalMessages as fatalMessage>
					<li>${fatalMessage.message!}</li>
				</#list>
			</ul>
		</@alert>
	<#else>			
		<@table type="data-complex" role="grid">
			<@thead>
				<@tr>
					<@th>Record</@th>
					<@th>Level</@th>
					<@th>Message</@th>
				</@tr>
			</@thead>		
			<#list stats as stat>
				<@tr>
					<@td>${stat.position!}</@td>
					<@td>${stat.level!}</@td>
					<@td>${stat.message!}</@td>
				</@tr>
			</#list>
		</@table>
	</#if>
	
</#if>
