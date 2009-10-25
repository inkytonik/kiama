set SCRIPT_DIR=%~dp0
java -Xmx512M -jar "%SCRIPT_DIR%lib\sbt-launcher.jar" %*
