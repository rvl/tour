<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
     xmlns:gpx='http://www.topografix.com/GPX/1/0'>

<xsl:output method="text"/>

<xsl:template match="/">
  <xsl:text>[</xsl:text>
  <xsl:apply-templates select="gpx:gpx/gpx:bounds"/>
  <xsl:text>]</xsl:text>
</xsl:template>

<xsl:template match="gpx:bounds">
     <xsl:value-of select="@minlat"/>
     <xsl:text>,</xsl:text>
     <xsl:value-of select="@minlon"/>
     <xsl:text>,</xsl:text>
     <xsl:value-of select="@maxlat"/>
     <xsl:text>,</xsl:text>
     <xsl:value-of select="@maxlon"/>
</xsl:template>

</xsl:stylesheet>
