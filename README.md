# note-interval-calculator

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Requirements](#requirements)
- [Testing](#testing)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Requirements

[stack](https://docs.haskellstack.org/en/stable/README/):

- The Haskell tool stack
- Install with homebrew: `brew install stack`

## Testing

Tests are located in the [test](./test) directory. You may choose to run:

- Directly with `stack`:

    ```bash
    stack test
    ```

- Interactively with `stack`, followed by running `main` in GHCi:

    ```bash
    stack ghci note-interval-calculator:note-interval-calculator-test
    # λ> main
    ```

- Compiling and reloading tests continuously:

    ```bash
    ghcid --command="stack ghci note-interval-calculator:note-interval-calculator-test"
    ```
