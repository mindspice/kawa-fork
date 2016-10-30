<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
<!--<xsl:import href="html/chunktoc.xsl"/>-->
<xsl:import href="chunk.xsl"/>
<xsl:import href="docbook-to-html.xsl"/>
<!--<xsl:import href="kawa-synopsis.xsl"/>-->
<xsl:param name="html.namespace">http://www.w3.org/1999/xhtml</xsl:param>
<xsl:param name="html.script">style/utils.js</xsl:param>

<xsl:template name="body.attributes">
  <xsl:attribute name="onload">javascript:onLoadHandler();</xsl:attribute>
  <xsl:attribute name="onunload">javascript:onUnloadHandler();</xsl:attribute>
  </xsl:template>
</xsl:stylesheet>