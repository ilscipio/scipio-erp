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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** One configurable Google Image Search URL.  To configure, use {@link Options}
 *
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=183668">Creating Image Sitemaps</a>
 */
public class GoogleImageSitemapUrl extends WebSitemapUrl {

    private final List<Image> images;

    public GoogleImageSitemapUrl(String url) throws MalformedURLException {
        this(new Options(url));
    }

    public GoogleImageSitemapUrl(URL url) {
        this(new Options(url));
    }

    public GoogleImageSitemapUrl(Options options) {
        super(options);
        this.images = options.images;
    }

    public void addImage(Image image) {
        this.images.add(image);
        if(this.images.size() > 1000) {
            throw new RuntimeException("A URL cannot have more than 1000 image tags");
        }
    }

    /** Options to configure Google Extension URLs */
    public static class Options extends AbstractSitemapUrlOptions<GoogleImageSitemapUrl, GoogleImageSitemapUrl.Options> {
        private List<Image> images;


        public Options(URL url) {
            super(url, GoogleImageSitemapUrl.class);
            images = new ArrayList<Image>();
        }

        public Options(String url) throws MalformedURLException {
            super(url, GoogleImageSitemapUrl.class);
            images = new ArrayList<Image>();
        }

        public Options images(List<Image> images) {
            if(images != null && images.size() > 1000) {
                throw new RuntimeException("A URL cannot have more than 1000 image tags");
            }
            this.images = images;
            return this;
        }

        public Options images(Image...images) {
            if(images.length > 1000) {
                throw new RuntimeException("A URL cannot have more than 1000 image tags");
            }
            return images(Arrays.asList(images));

        }
    }

    /**Retrieves list of images*/
    public List<Image> getImages() {
        return this.images;
    }
}
