scalanizer
=========

Scalanizer is a new frontend for Scalan framework. It is implemented as a Plugin for Scala compiler.

Plugin supports the following option which can be set up in `ScalanPluginConfig` class.

* `save = true` instructs the plugin to save all the generated boilerplate in the files
* `read = true` instructs the plugin to read all the generated boilerplate from files, which was previously generated.

These options allow to debug both the plugin and generated code. If the project is opened in some IDE the generated code is indexed together with the other files of the project and thus all code navidation commands step by step debugging work.

setup
-----
Clone this repository using `git clone` command. Run `sbt` and then execute `assembly` command which will generate fat jar file of the plugin.

Reference the generated jar file in the compiler parameter (for example `-Xplugin:/fullpath/scalanizer_2.11.6-0.0.1-fat.jar`)

If you set `-P:scalan:debug` additional compiler options you also need to specify a directory for debug output by setting `ScalanPluginConfig.home`.

