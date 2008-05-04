<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:msxsl="urn:schemas-microsoft-com:xslt" exclude-result-prefixes="msxsl"
>
  <xsl:output method="xml"
              indent="yes"
              encoding="utf-8"
              omit-xml-declaration="yes" />
  <xsl:preserve-space elements="Param" />
  <xsl:template match="/">
    <xsl:apply-templates select="EisConf/Section/Param" />
  </xsl:template>

  <xsl:template match="Param">
    <xsl:value-of select="ParamName" />=<xsl:value-of select="Default" />
    <xsl:text>
</xsl:text>
  </xsl:template>
</xsl:stylesheet>
