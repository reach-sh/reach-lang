# {#guide-editor-support} IDE/Text Editor Support

There are a number of text editor plugins available to provide a better development experience for Reach.
These plugins provide syntax highlighting, build commands, key mappings, snippets, and more.
The following plugins are available:

+ Visual Studio Code: [Reach IDE](https://marketplace.visualstudio.com/items?itemName=reachsh.reach-ide)
+ Atom: [Reach Language Support](https://atom.io/packages/language-reach), [Reach IDE](https://atom.io/packages/reach-ide)
+ Sublime Text: [Reach IDE](https://github.com/chrisnevers/reach-ide-sublime)

We highly recommend using Visual Studio Code IDE, it is the most advanced `Reach` extension.

# {#guide-install-VSCode} Install VSCode

Visual Studio Code (VSCode) enables users to both write and compile `Reach` code.
With the `Reach IDE VSCode extension` installed, VSCode compiles automatically in the background and alerts users of coding errors in seconds when the necessary prerequisites, such as Docker, are installed and running.

1. Download the [VSCode](https://code.visualstudio.com/download) IDE.
2. Double-click the file to install VSCode.
3. In VSCode, click extensions in the left panel.

![VSCode extensions`](/guide/editor-support/VS-extensions.png)

4. Search for `Reach`, and then install the most recent version of the `Reach IDE` extension.

![Reach-IDE`](/guide/editor-support/VS-Reach-IDE.png)

# {#guide-install-Atom} Install Atom

1. Download and install [Atom](https://github.com/atom/atom/releases/tag/v1.59.0) matching your system.
2. In Atom, click `File\Settings`.
3. Click `Install`.

![Click Install](/guide/editor-support/Atom-InstallBTN.png)

4. Type `Reach`.

![Search for Reach](/guide/editor-support/Atom-SearchReach.png)

5. Install `Reach IDE`.

![Reach-IDE`](/guide/editor-support/Atom-Reach-IDE.png)

6. Install `Reach Language Support`.

![Reach-language`](/guide/editor-support/Atom-LanguageReach.png)

# {#guide-install-Sublime} Install Sublime

1. Download and install [Sublime](https://www.sublimetext.com/).
2. Download the [Reach-IDE](https://github.com/chrisnevers/reach-ide-sublime/releases).
3. Unzip the `Reach-IDE` into the `\Sublime Text\Packages` folder of your Sublime installation.
4. Open your project in Sublime.
5. Click `Tools\Command Palette`, and then type `Reach`.

![Command Palette`](/guide/editor-support/CommandPalette.png)

6. Click `Set Syntax:Reach`.

![Set Syntax:Reach](/guide/editor-support/ReachSyntax.png)

7. Repeat 5 and then select `Reach - Compile` to build with `Reach`.