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

/**
 * Builds a sitemap for Google Image search. To configure options use {@link #builder(URL, File)}
 * @see <a href="https://support.google.com/webmasters/answer/183668">Manage your sitemaps</a>
 * */
public class GoogleImageSitemapGenerator extends SitemapGenerator<GoogleImageSitemapUrl, GoogleImageSitemapGenerator> {

    GoogleImageSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
        super(options, new GoogleImageSitemapGenerator.Renderer());
    }

    /** Configures the generator with a base URL and directory to write the sitemap files.
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
     * @throws MalformedURLException
     */
    public GoogleImageSitemapGenerator(String baseUrl, File baseDir)
            throws MalformedURLException {
        this(new SitemapGeneratorOptions(baseUrl, baseDir));
    }

    /**Configures the generator with a base URL and directory to write the sitemap files.
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
     */
    public GoogleImageSitemapGenerator(URL baseUrl, File baseDir) {
        this(new SitemapGeneratorOptions(baseUrl, baseDir));
    }

    /**Configures the generator with a base URL and a null directory. The object constructed
     * is not intended to be used to write to files. Rather, it is intended to be used to obtain
     * XML-formatted strings that represent sitemaps.
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     */
    public GoogleImageSitemapGenerator(String baseUrl) throws MalformedURLException {
        this(new SitemapGeneratorOptions(new URL(baseUrl)));
    }

    /**Configures the generator with a base URL and a null directory. The object constructed
     * is not intended to be used to write to files. Rather, it is intended to be used to obtain
     * XML-formatted strings that represent sitemaps.
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     */
    public GoogleImageSitemapGenerator(URL baseUrl) {
        this(new SitemapGeneratorOptions(baseUrl));
    }

    /** Configures a builder so you can specify sitemap generator options
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
     * @return a builder; call .build() on it to make a sitemap generator
     */
    public static SitemapGeneratorBuilder<GoogleImageSitemapGenerator> builder(URL baseUrl, File baseDir) {
        return new SitemapGeneratorBuilder<GoogleImageSitemapGenerator>(baseUrl, baseDir, GoogleImageSitemapGenerator.class);
    }

    /** Configures a builder so you can specify sitemap generator options
     *
     * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
     * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
     * @return a builder; call .build() on it to make a sitemap generator
     * @throws MalformedURLException
     */
    public static SitemapGeneratorBuilder<GoogleImageSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
        return new SitemapGeneratorBuilder<GoogleImageSitemapGenerator>(baseUrl, baseDir, GoogleImageSitemapGenerator.class);
    }

    private static class Renderer extends AbstractSitemapUrlRenderer<GoogleImageSitemapUrl> implements ISitemapUrlRenderer<GoogleImageSitemapUrl> {

        public Class<GoogleImageSitemapUrl> getUrlClass() {
            return GoogleImageSitemapUrl.class;
        }

        public String getXmlNamespaces() {
            return "xmlns:image=\"http://www.google.com/schemas/sitemap-image/1.1\"";
        }

        public void render(GoogleImageSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
            StringBuilder tagSb = new StringBuilder();

            for(Image image : url.getImages()) {
                tagSb.append("    <image:image>\n");
                renderTag(tagSb, "image", "loc", image.getUrl());
                renderTag(tagSb, "image", "caption", image.getCaption());
                renderTag(tagSb, "image", "title", image.getTitle());
                renderTag(tagSb, "image", "geo_location", image.getGeoLocation());
                renderTag(tagSb, "image", "license", image.getLicense());
                tagSb.append("    </image:image>\n");
            }
            super.render(url, sb, dateFormat, tagSb.toString());
        }
    }
}
