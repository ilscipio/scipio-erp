package com.redfin.sitemapgenerator;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/** A convenience class to let you configure options straightforwardly; don't instantiate by hand.
 * 
 * <p>Instead, get one statically from a SitemapGenerator class.  For example: <code>WebSitemapGenerator g =<br>
 * WebSitemapGenerator.builder("http://example.com", myDir).gzip(true).autoValidate(true).build()</code></p>
 * 
 * 
 * @author Dan Fabulich
 *
 * @param <G>
 */
//that weird thing with generics is so sub-classed objects will return themselves
//It makes sense, I swear! http://madbean.com/2004/mb2004-3/
public class SitemapGeneratorBuilder<G extends SitemapGenerator<?,?>> extends AbstractSitemapGeneratorOptions<SitemapGeneratorBuilder<G>> {

	Class<G> sitemapGeneratorClass;
	
	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @param sitemapGeneratorClass the class of the generator the builder will create
	 */
	public SitemapGeneratorBuilder(URL baseUrl, File baseDir, Class<G> sitemapGeneratorClass) {
		super(baseUrl, baseDir);
		this.sitemapGeneratorClass = sitemapGeneratorClass;
	}
	
	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @param sitemapGeneratorClass the class of the generator the builder will create
	 */
	public SitemapGeneratorBuilder(String baseUrl, File baseDir, Class<G> sitemapGeneratorClass) throws MalformedURLException {
		this(new URL(baseUrl), baseDir, sitemapGeneratorClass);
	}
	
	/** Constructs a sitemap generator configured with the options you specified */
	public G build() {
		try {
			return sitemapGeneratorClass.getDeclaredConstructor(AbstractSitemapGeneratorOptions.class).newInstance(this);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}
