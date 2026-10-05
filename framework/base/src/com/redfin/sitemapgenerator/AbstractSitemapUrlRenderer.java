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
		if (url.getAltLinks() != null && !url.getAltLinks().isEmpty()) { // SCIPIO: 3.0.0: Added
			for (AltLink altLink : url.getAltLinks()) {
				String ns = altLink.namespace;
				if (ns == null || ns.isEmpty()) {
					ns = "xhtml";
				}
				sb.append("    <");
				sb.append(ns);
				sb.append(":link");
				if (altLink.rel != null) {
					sb.append(" rel=\"");
					sb.append(UrlUtils.escapeXml(altLink.rel));
					sb.append("\"");
				}
				if (altLink.lang != null) {
					sb.append(" hreflang=\"");
					sb.append(UrlUtils.escapeXml(altLink.lang));
					sb.append("\"");
				}
				if (altLink.url != null) {
					sb.append(" href=\"");
					sb.append(UrlUtils.escapeXml(altLink.url));
					sb.append("\"");
				}
				sb.append("/>\n");
			}
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
