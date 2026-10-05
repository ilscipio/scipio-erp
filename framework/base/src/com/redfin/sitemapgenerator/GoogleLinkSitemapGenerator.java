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

import java.io.File;
import java.net.*;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Builds a Google Link Sitemap (to indicate alternate language pages).
 *
 * @author Sergio Vico
 * @see <a href="https://support.google.com/webmasters/answer/2620865">Creating alternate language pages Sitemaps</a>
 * @see <a href="https://developers.google.com/search/mobile-sites/mobile-seo/separate-urls?hl=en">Mobile SEO configurations | Separate URLs </a>
 */
public class GoogleLinkSitemapGenerator extends SitemapGenerator<GoogleLinkSitemapUrl, GoogleLinkSitemapGenerator> {

	private static class Renderer extends AbstractSitemapUrlRenderer<GoogleLinkSitemapUrl>
			implements ISitemapUrlRenderer<GoogleLinkSitemapUrl> {

		public Class<GoogleLinkSitemapUrl> getUrlClass() {

			return GoogleLinkSitemapUrl.class;
		}

		public String getXmlNamespaces() {

			return "xmlns:xhtml=\"http://www.w3.org/1999/xhtml\"";
		}

		public void render(final GoogleLinkSitemapUrl url, final StringBuilder sb, final W3CDateFormat dateFormat) {

			final StringBuilder tagSb = new StringBuilder();
			for (final Entry<URI, Map<String, String>> entry : url.getAlternates().entrySet()) {
				tagSb.append("    <xhtml:link\n");
				tagSb.append("      rel=\"alternate\"\n");
				for(final Entry<String, String> innerEntry : entry.getValue().entrySet()){
					tagSb.append("      " + innerEntry.getKey() + "=\"" + innerEntry.getValue() + "\"\n");
				}
				tagSb.append("      href=\"" + UrlUtils.escapeXml(entry.getKey().toString()) + "\"\n");
				tagSb.append("    />\n");
			}
			super.render(url, sb, dateFormat, tagSb.toString());
		}

	}

	/**
	 * Configures a builder so you can specify sitemap generator options
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir
	 *			Sitemap files will be generated in this directory as either "sitemap.xml" or
	 *			"sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleLinkSitemapGenerator> builder(final String baseUrl, final File baseDir)
		throws MalformedURLException {

		return new SitemapGeneratorBuilder<GoogleLinkSitemapGenerator>(baseUrl, baseDir,
				GoogleLinkSitemapGenerator.class);
	}

	/**
	 * Configures a builder so you can specify sitemap generator options
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir
	 *			Sitemap files will be generated in this directory as either "sitemap.xml" or
	 *			"sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<GoogleLinkSitemapGenerator> builder(final URL baseUrl, final File baseDir) {

		return new SitemapGeneratorBuilder<GoogleLinkSitemapGenerator>(baseUrl, baseDir,
				GoogleLinkSitemapGenerator.class);
	}

	/**
	 * Configures the generator with a base URL and a null directory. The object constructed is not
	 * intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleLinkSitemapGenerator(final String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}

	/**
	 * Configures the generator with a base URL and directory to write the sitemap files.
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir
	 *			Sitemap files will be generated in this directory as either "sitemap.xml" or
	 *			"sitemap1.xml" "sitemap2.xml" and so on.
	 * @throws MalformedURLException
	 */
	public GoogleLinkSitemapGenerator(final String baseUrl, final File baseDir) throws MalformedURLException {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/**
	 * Configures the generator with a base URL and a null directory. The object constructed is not
	 * intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public GoogleLinkSitemapGenerator(final URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}

	/**
	 * Configures the generator with a base URL and directory to write the sitemap files.
	 *
	 * @param baseUrl
	 *			All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir
	 *			Sitemap files will be generated in this directory as either "sitemap.xml" or
	 *			"sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public GoogleLinkSitemapGenerator(final URL baseUrl, final File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	GoogleLinkSitemapGenerator(final AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer());
	}
}
