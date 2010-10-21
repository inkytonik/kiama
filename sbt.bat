set SCRIPT_DIR=%~dp0
java -Xmx512M -Xss2M -jar "%SCRIPT_DIR%\sbt-launcher.jar" %*
