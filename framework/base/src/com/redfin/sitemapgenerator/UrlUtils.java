/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.redfin.sitemapgenerator;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class UrlUtils {
	private static Map<String,String> ENTITIES = new HashMap<String,String>();
	static {
		ENTITIES.put("&", "&amp;");
		ENTITIES.put("'", "&apos;");
		ENTITIES.put("\"", "&quot;");
		ENTITIES.put(">", "&gt;");
		ENTITIES.put("<", "&lt;");
	}
	private static Pattern PATTERN = Pattern.compile("(&|'|\"|>|<)");

	static String escapeXml(String string){
		Matcher matcher = PATTERN.matcher(string);
		StringBuffer sb = new StringBuffer();
		while(matcher.find()) {
		    matcher.appendReplacement(sb, ENTITIES.get(matcher.group(1)));
		}
		matcher.appendTail(sb);

		return sb.toString();
	}

	static void checkUrl(URL url, URL baseUrl) {
		// Is there a better test to use here?
		
		if (baseUrl.getHost() == null) {
			throw new RuntimeException("base URL is null");
		}
		
		if (!baseUrl.getHost().equalsIgnoreCase(url.getHost())) {
			throw new RuntimeException("Domain of URL " + url + " doesn't match base URL " + baseUrl);
		}
	}

	static <K,V> HashMap<K,V> newHashMap() {
		return new HashMap<K,V>();
	}
	
}
