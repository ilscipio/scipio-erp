package com.redfin.sitemapgenerator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

/**
 * Represents a single sitemap for inclusion in a sitemap index.
 * @author Dan Fabulich
 *
 */
public class SitemapIndexUrl {
	final URL url;
	final Date lastMod;
	/** Configures the sitemap URL with a specified lastMod */
	public SitemapIndexUrl(URL url, Date lastMod) {
		this.url = url;
		this.lastMod = lastMod;
	}
	
	/** Configures the sitemap URL with a specified lastMod */
	public SitemapIndexUrl(String url, Date lastMod) throws MalformedURLException {
		this(new URL(url), lastMod);
	}
	
	/** Configures the sitemap URL with no specified lastMod; we'll use {@link SitemapIndexGenerator.Options#defaultLastMod(Date)} or leave it blank if no default is specified */
	public SitemapIndexUrl(URL url) {
		this(url, null);
	}
	
	/** Configures the sitemap URL with no specified lastMod; we'll use {@link SitemapIndexGenerator.Options#defaultLastMod(Date)} or leave it blank if no default is specified */
	public SitemapIndexUrl(String url) throws MalformedURLException {
		this(new URL(url));
	}
}
