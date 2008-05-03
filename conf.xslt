<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:msxsl="urn:schemas-microsoft-com:xslt" exclude-result-prefixes="msxsl"
>
  <xsl:output method="html" indent="yes"/>

  <xsl:template match="/">
    <html>
      <head>
        <title>Eis configuration</title>
        <style type="text/css">
          * {
          font-family: sans-serif;

          }
          li { font-size: 0.8em; }
          p { font-size: 0.7em; }
          h3 { font-size: 0.95em; }
          .section {
          margin-top: 2em;
          }
          .param {
          position: relative;
          width: 80%;
          left: 10%;
          margin-bottom: 3em;
          }
        </style>
      </head>
      <body>
        <div class="title">
          <h1>Eis configuration</h1>
          <h6>Erlang Irc Server doc</h6>
        </div>
        <p>
          <xsl:value-of select="EisConf/Desc"/>
        </p>
        <div class="index">
          <xsl:for-each select="EisConf/Section">
            <p>
              <xsl:value-of select="@name"/>
              <ul>

                <xsl:for-each select="Param">
                  <li>
                    <a href="#{ParamName}">
                      <xsl:value-of select="ParamName"/>
                    </a>
                  </li>
                </xsl:for-each>

              </ul>
            </p>
          </xsl:for-each>
        </div>
        <xsl:apply-templates select="EisConf/Section" />
      </body>
    </html>
  </xsl:template>

  <xsl:template match="Section">
    <div class="section">
      <h2>
        <xsl:value-of select="@name" />
      </h2>
      <p>
        <xsl:value-of select="Description" />
      </p>
      <xsl:apply-templates select="Param" />
    </div>
  </xsl:template>

  <xsl:template match="Param">
    <div class="param">
      <a name="#{ParamName}" />
      <h3>
        <xsl:value-of select="ParamName"/>
      </h3>
      <p>
        <xsl:value-of select="Desc"/>
      </p>
      <p>
        <i>Default value</i> : <xsl:value-of select="Default"/><br />
        <i>Excpected type</i> : <xsl:value-of select="Type"/>
      </p>
    </div>
  </xsl:template>
</xsl:stylesheet>
