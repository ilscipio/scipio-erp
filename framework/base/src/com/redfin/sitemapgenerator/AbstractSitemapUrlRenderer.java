/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/

package com.redfin.sitemapgenerator;

abstract class AbstractSitemapUrlRenderer<T extends WebSitemapUrl> implements ISitemapUrlRenderer<T> {
	
	public void render(WebSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat, String additionalData) {
		sb.append("  <url>\n");
		sb.append("    <loc>");
		sb.append(UrlUtils.escapeXml(url.getUrl().toString()));
		sb.append("</loc>\n");
		if (url.getLastMod() != null) {
			sb.append("    <lastmod>");
			sb.append(dateFormat.format(url.getLastMod()));
			sb.append("</lastmod>\n");
		}
		if (url.getChangeFreq() != null) {
			sb.append("    <changefreq>");
			sb.append(url.getChangeFreq().toString());
			sb.append("</changefreq>\n");
		}
		if (url.getPriority() != null) {
			sb.append("    <priority>");
			sb.append(url.getPriority().toString());
			sb.append("</priority>\n");
		}
		if (additionalData != null) {
			sb.append(additionalData);
		}
		sb.append("  </url>\n");
	}

	public void renderTag(StringBuilder sb, String namespace, String tagName, Object value) {
		if (value == null) return;
		sb.append("      <");
		sb.append(namespace);
		sb.append(':');
		sb.append(tagName);
		sb.append('>');
		sb.append(UrlUtils.escapeXml(value.toString()));
		sb.append("</");
		sb.append(namespace);
		sb.append(':');
		sb.append(tagName);
		sb.append(">\n");
	}

	public void renderSubTag(StringBuilder sb, String namespace, String tagName, Object value) {
		if (value == null) return;
		sb.append("  ");
		renderTag(sb, namespace, tagName, value);
	}

}
