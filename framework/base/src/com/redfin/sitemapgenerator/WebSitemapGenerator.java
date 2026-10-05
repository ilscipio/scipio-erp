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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

/**
 * Generates a regular old sitemap (USE THIS CLASS FIRST).  To configure options, use {@link #builder(URL, File)}
 * @author Dan Fabulich
 *
 */
public class WebSitemapGenerator extends SitemapGenerator<WebSitemapUrl,WebSitemapGenerator> {
	
	WebSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer(options));
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<WebSitemapGenerator> builder(URL baseUrl, File baseDir) {
		return new SitemapGeneratorBuilder<WebSitemapGenerator>(baseUrl, baseDir, WebSitemapGenerator.class);
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<WebSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
		return new SitemapGeneratorBuilder<WebSitemapGenerator>(baseUrl, baseDir, WebSitemapGenerator.class);
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */

	public WebSitemapGenerator(String baseUrl, File baseDir)
			throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl), baseDir));
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public WebSitemapGenerator(URL baseUrl, File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public WebSitemapGenerator(String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public WebSitemapGenerator(URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}
	
	private static class Renderer extends AbstractSitemapUrlRenderer<WebSitemapUrl> implements ISitemapUrlRenderer<WebSitemapUrl> {

		private final String namespaces; // SCIPIO: 3.0.0: Added

		public Renderer(AbstractSitemapGeneratorOptions<?> options) { // SCIPIO: 3.0.0: Added
			String namespaces = null;
			if (options.namespaces != null && !options.namespaces.isEmpty()) {
				String ns = "";
				for (Map.Entry<String, String> entry : options.namespaces.entrySet()) {
					String name = entry.getKey();
					String value = entry.getValue();
					if (!name.contains(":")) {
						name = "xmlns:" + name;
					}
					if (!ns.isEmpty()) {
						ns += " ";
					}
					ns += name + "=\"" + value + "\"";
				}
				namespaces = ns;
			}
			this.namespaces = namespaces;
		}

		public Class<WebSitemapUrl> getUrlClass() {
			return WebSitemapUrl.class;
		}
		
		public String getXmlNamespaces() {
			return namespaces;
		}

		public void render(WebSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
			super.render(url, sb, dateFormat, null);
		}

	}
}
