### Compiling the project

- Code compilation (and thus, output generation) is done through the *LaTeX workshop*'s `Build LaTeX project` command.
- The output folder is defined through the `latex-workshop.latex.out-dir` setting. Defaults to `%DIR%`, which points to the TeX project root. In my settings, remapped to `%DIR%/Output`.
- In the output folder, you can simply view the .pdf file.

### Useful shortcuts

- **SyncTeX from cursor (forward)**: 
    - *Meaning*: Find the current cursor position in the .pdf file. Defaults to 
    - *Shortcut*: Defaults to `Ctrl+Alt+j`. Remapped to `Ctrl+l Ctrl+k`

- **SyncTeX from pdf (backward)**:
    - *Meaning*: Find a .pdf element in code.
    - *Shortcut*: Hold `Ctrl` and press the element you would like to find in code.
